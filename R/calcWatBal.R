###############################################################################
###############################################################################
#
#      calcWatBal.R Soil Water Balance
#
#     Copyright (C) 2024 Institute of Plant Sciences, Sant’Anna School of
#     Advanced Studies, Pisa, Italy
#     (https://www.santannapisa.it/en/institute/plant-sciences).
#
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
#
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
###############################################################################
###############################################################################

#' @title Daily Soil Water Balance Estimation
#'
#' @description This function estimates the daily soil water balance based on a
#' simple mass balance budget approach. It calculates the amount of water
#' available in the root zone  of a homogeneous grass canopy growing on a
#' well-drained, homogeneous soil.
#'
#' @param data A data frame containing the required input variables. The data
#' frame must include the following columns:
#' \itemize{
#'   \item \strong{Lat}: Latitude of the site (decimal degrees).
#'   \item \strong{Lon}: Longitude of the site (decimal degrees).
#'   \item \strong{Elev}: Elevation above sea level (meters).
#'   \item \strong{Year}: Year of the record ("YYYY").
#'   \item \strong{Month}: Month of the record ("MM").
#'   \item \strong{Day}: Day of the record ("DD").
#'   \item \strong{Rain}: Daily rainfall (mm).
#'   \item \strong{Eto}: Daily potential evapotranspiration (mm).
#' }
#'
#' @param soilWHC Numeric. Water holding capacity of the soil (mm).
#'
#' @return A list with the following components:
#'   \describe{
#'     \item{\strong{data}}{A data frame containing the results of the water
#'     balance calculations, with the following columns:
#'       \itemize{
#'         \item \strong{DRAIN}: Deep drainage (mm).
#'         \item \strong{TRAN}: Water lost by transpiration (mm).
#'         \item \strong{RUNOFF}: Surface runoff (mm).
#'         \item \strong{AVAIL}: Available soil moisture storage (mm).
#'         \item \strong{R}: Ratio of actual to potential evapotranspiration.
#'       }
#'     }
#'     \item{\strong{warnings}}{A list of warnings related to any unrealistic
#'     or adjusted values in the input data or parameters used during the water
#'     balance calculations.}
#'   }
#'
#' @references
#' Allen, R.G.; Pereira, L.S.; Raes, D.; Smith, M. (1998). \emph{Crop
#' Evapotranspiration: Guidelines for Computing Crop Water Requirements.} FAO
#' Irrigation and Drainage Paper no. 56, FAO: Rome, Italy. ISBN 92-5-104219-5.
#'
#' Doorenbos, J., & Pruitt, W.O. (1975). \emph{Guidelines for Predicting Crop
#' Water Requirements.} Irrigation and Drainage Paper 24, Food and Agriculture
#' Organization of the United Nations, Rome, 179 p.
#'
#' @seealso \code{\link{calcEto}}, \code{\link{calcSeasCal}}
#'
#' @importFrom graphics legend par lines
#' @importFrom terra rast extract
#' @importFrom sp coordinates CRS proj4string
#'
#' @examples
#' \donttest{
#' ## Load sample data
#' data(AgroClimateData)
#'
#' # Estimate daily PET using the Penman-Monteith method
#' PET.PM <- calcEto(AgroClimateData, method = "PM", Zh = 10)
#'
#' # Add the estimated PET to the AgroClimateData frame
#' AgroClimateData$Eto <- PET.PM$ET.Daily
#'
#' # Estimate daily soil water balance for a soil with 100 mm WHC
#' watBal <- calcWatBal(data = AgroClimateData, soilWHC = 100)
#' }
#'
#' @export
###############################################################################
###############################################################################

calcWatBal <- function(data, soilWHC) {
  warnings.list <- list()

  ## ***** Validate parameters:

  if (is.null(data$Rain) || is.null(data$Eto) || is.null(soilWHC)) {
    stop("Required data missing for 'Rain', 'Eto', or 'soilWHC'")
  }

  ## ***** Check for missing data:

  if (any(is.na(data$Rain)) || any(is.na(data$Eto)) || is.na(soilWHC)) {
    stop("Missing values detected in 'Rain', 'Eto', or 'soilWHC'")
  }

  ## ***** Check for realistic values:

  # Check soil water holding capacity (soilWHC) limits
  if (soilWHC < 15) {
    stop("The soil has very low water holding capacity (< 15 mm)")
  }

  if (soilWHC > 300) {
    soilWHC <- 300
    warnings.list[["soilWHC"]] <- "The soil water holding capacity exceeded
    realistic limits and has been set to the upper limit (300 mm)."
    warning(warnings.list[["soilWHC"]])
  }

  ## Define upper thresholds for Rain and Eto:

  rainU.tr <- 2000
  petU.tr <- 20

  ## Check for excessive Rain values:

  if (any(data$Rain > rainU.tr)) {
    data$Rain[data$Rain > rainU.tr] <- rainU.tr
    warnings.list[["Rain"]] <- paste(
      "Some 'Rain' values exceeded", rainU.tr,
      "mm and were set to this limit."
    )
    warning(warnings.list[["Rain"]])
  }

  ## Check for excessive Eto values:

  if (any(data$Eto > petU.tr)) {
    data$Eto[data$Eto > petU.tr] <- petU.tr
    warnings.list[["Eto"]] <- paste(
      "Some 'Eto' values exceeded", petU.tr,
      "mm/day and were set to this limit."
    )
    warning(warnings.list[["Eto"]])
  }

  ###############################################################################
  ###############################################################################
  ## ***** Initialize parameters

  rcn.pts <- NA

  ## ***** Load curve number raster

  rcn <- terra::rast(system.file("extdata/rcn.tif", package = "AquaBEHER"))

  ## ***** Extract curve number

  pts.dF <- data.frame(
    Lat = as.numeric(data$Lat[1]),
    Lon = as.numeric(data$Lon[1])
  )
  coords <- cbind(Lon = pts.dF$Lon, Lat = pts.dF$Lat)
  sp::coordinates(pts.dF) <- coords
  sp::proj4string(pts.dF) <- sp::CRS("+proj=longlat")
  vect_points <- terra::vect(pts.dF)
  rcn.pts <- terra::extract(rcn, vect_points)[, 2]

  CN <- if (!is.null(rcn.pts) && !is.na(rcn.pts)) rcn.pts else 65

  ## ***** Check curve number limits:

  if (CN < 0 || CN > 100) {
    warnings.list[["CN"]] <- "Curve Number (CN) must be between 0 and 100.
    Set to default value: 65."
    warning(warnings.list[["CN"]])
    CN <- 65
  }

  ## ***** ???????

  DC <- 0.55
  MUF <- 0.096
  WATwp <- 0.15 * soilWHC

  ## ***** Maximum abstraction:

  S <- 25400 / CN - 254

  ## ***** Initial Abstraction as 20% of maximum abstraction (S):

  IA <- 0.2 * S

  date.vec <- as.Date(paste0(data$Year, "-", data$Month, "-", data$Day))
  data$RUNOFF <- data$DRAIN <- data$TRAN <- data$AVAIL <- data$R <- NA
  data$Rain[data$Rain < 2] <- 0

  ###############################################################################
  ###############################################################################
  ## ***** loop over each day:

  for (day in seq_along(date.vec)) {
    if (day == 1) {
      WAT0 <- 0 # Initial soil water content (mm)
    } else {
      WAT0 <- data$AVAIL[day - 1]
    }

    ## ***** Calculate runoff:

    if (data$Rain[day] > IA) {
      data$RUNOFF[day] <- ((data$Rain[day] - IA)^2 /
        (data$Rain[day] + 0.8 * S))
    } else {
      data$RUNOFF[day] <- 0
    }

    data$RUNOFF[day] <- max(data$RUNOFF[day], 0)

    ## ***** Calculate deep drainage:

    excessWater <- WAT0 + data$Rain[day] - data$RUNOFF[day]
    data$DRAIN[day] <- if (excessWater > soilWHC) {
      DC * (excessWater - soilWHC)
    } else {
      0
    }
    data$DRAIN[day] <- max(data$DRAIN[day], 0)

    ## ***** Calculate water lost by transpiration:

    TRANavail <- (WAT0 + data$Rain[day] - data$RUNOFF[day] -
      data$DRAIN[day] - WATwp)
    data$TRAN[day] <- min(MUF * TRANavail, data$Eto[day])
    data$TRAN[day] <- max(data$TRAN[day], 0)
    data$TRAN[day] <- min(data$TRAN[day], soilWHC)

    ##  Calculate ratio of actual to potential evapotranspiration (R-index)

    data$R[day] <- ifelse(data$Eto[day] > 0, data$TRAN[day] / data$Eto[day], 0)
    data$TRAN[day] <- min(data$TRAN[day], data$R[day] * data$Eto[day])

    ## ***** Calculate available soil moisture:

    data$AVAIL[day] <- (WAT0 + data$Rain[day] - data$RUNOFF[day] -
      data$DRAIN[day] - data$TRAN[day])
    data$AVAIL[day] <- min(max(data$AVAIL[day], 0), soilWHC)
  }

  data$R <- round(data$R, 3)
  data$AVAIL <- round(data$AVAIL, 3)
  data$TRAN <- round(data$TRAN, 3)
  data$DRAIN <- round(data$DRAIN, 3)
  data$RUNOFF <- round(data$RUNOFF, 3)

  data <- as.data.frame(data)

  return(list(data = data, warnings = warnings.list))
}

###############################################################################
###############################################################################
#                >>>>>>>>>>   End of code   <<<<<<<<<<                        #
###############################################################################
###############################################################################
