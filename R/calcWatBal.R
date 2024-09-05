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
#' @return A data frame with the following components:
#' \itemize{
#'   \item \strong{DRAIN}: Deep drainage (mm).
#'   \item \strong{TRAN}: Water lost by transpiration (mm).
#'   \item \strong{RUNOFF}: Surface runoff (mm).
#'   \item \strong{AVAIL}: Available soil moisture storage (mm).
#'   \item \strong{R}: Ratio of actual-to-potential evapotranspiration.
#' }
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
#'
#' }
#'
#' @export
###############################################################################
# ***** function to estimate Rindex: actual to potential evapotranspiration
# ratio based on Jones (1987)

calcWatBal <- function(data, soilWHC) {
  # ***** initialize parameters
  # CN : Runoff curve number
  # DC : Drainage coefficient (mm3.mm-3)
  # MUF : Water Uptake coefficient (mm^3 mm^-3)
  # WATfc : Maximum Water content at field capacity (mm) >>>>> WHC
  # WATfc = FC*z
  # WATwp : Water content at wilting Point (mm)
  # WATwp = WP*z

  rcn.pts <- NA ## ?????????????

  # data(rcn, envir = environment())
  rcn <- terra::rast(system.file("extdata/rcn.tif", package = "AquaBEHER"))

   pts.dF <- data.frame(Lat = as.numeric(data$Lat[1]),
                        Lon = as.numeric(data$Lon[1]))
   pts.sp <- pts.dF
   sp::coordinates(pts.sp) <- ~Lon+Lat
   sp::proj4string(pts.sp) <- sp::CRS("+proj=longlat")
   rcn.pts <- as.double(terra::extract(rcn, terra::vect(pts.sp))[2])

  if (!is.null(rcn.pts) & !is.na(rcn.pts)) {
    CN <- rcn.pts
  } else {
    CN <- 65 # *** well managed grass
  }

  DC <- 0.55 #

  MUF <- 0.1
  WATwp <- 0.15 * soilWHC
  # Maximum abstraction (for run off)
  S <- 25400 / CN - 254
  # Initial Abstraction (for run off)
  IA <- 0.2 * S

  date.vec <- as.Date(paste0(data$Year, "-", data$Month, "-", data$Day))

  data$RUNOFF <- data$DRAIN <- data$TRAN <- data$AVAIL <- data$R <- NA

  data$Rain[data$Rain < 2] <- 0

  for (day in seq_along(date.vec)) {

    if (day == 1) {
      WAT0 <- 0

      # Change in water before drainage (Precipitation - Runoff)

      if (data$Rain[day] > IA) {
        data$RUNOFF[day] <- (data$Rain[day] - 0.2 * S)^2 /
          (data$Rain[day] + 0.8 * S)
      } else {
        data$RUNOFF[day] <- 0
      }

      data$RUNOFF[day] <- max(data$RUNOFF[day], 0)

      # Calculating the amount of deep drainage

      if ((WAT0 + data$Rain[day] - data$RUNOFF[day]) > soilWHC) {
        data$DRAIN[day] <- DC * (WAT0 + data$Rain[day] - data$RUNOFF[day] -
                                   soilWHC)
      } else {
        data$DRAIN[day] <- 0
      }

      data$DRAIN[day] <- max(data$DRAIN[day], 0)

      # Calculating the amount of water lost by transpiration (after drainage)

      data$TRAN[day] <- min(MUF * (WAT0 + data$Rain[day] - data$RUNOFF[day] -
        data$DRAIN[day] - WATwp), data$Eto[day])
      data$TRAN[day] <- max(data$TRAN[day], 0)
      data$TRAN[day] <- min(data$TRAN[day], soilWHC)

      data$R[day] <- data$TRAN[day] / data$Eto[day]
      data$TRAN[day] <- max(data$TRAN[day], (data$R[day] * data$Eto[day]))

      data$AVAIL[day] <- WAT0 + (data$Rain[day] - data$RUNOFF[day] -
        data$DRAIN[day] - data$TRAN[day])
      data$AVAIL[day] <- min(data$AVAIL[day], soilWHC)
      data$AVAIL[day] <- max(data$AVAIL[day], 0)

    } else {

       WAT0 <- data$AVAIL[day - 1]

      # Change in water before drainage (Precipitation - Runoff)

      if (data$Rain[day] > IA) {
        data$RUNOFF[day] <- (data$Rain[day] - 0.2 * S)^2 /
          (data$Rain[day] + 0.8 * S)
      } else {
        data$RUNOFF[day] <- 0
      }

      data$RUNOFF[day] <- max((data$RUNOFF[day]), 0)

      # Calculating the amount of deep drainage

      if ((WAT0 + data$Rain[day] - data$RUNOFF[day]) > soilWHC) {
        data$DRAIN[day] <- DC * (WAT0 + data$Rain[day] -
                                   data$RUNOFF[day] - soilWHC)
      } else {
        data$DRAIN[day] <- 0
      }

      data$DRAIN[day] <- max(data$DRAIN[day], 0)

      # Calculating the amount of water lost by transpiration (after drainage)

      data$TRAN[day] <- min(MUF * (WAT0 + data$Rain[day] - data$RUNOFF[day] -
        data$DRAIN[day] - WATwp), data$Eto[day])
      data$TRAN[day] <- max(data$TRAN[day], 0)
      data$TRAN[day] <- min(data$TRAN[day], soilWHC)

      data$R[day] <- data$TRAN[day] / data$Eto[day]
      data$TRAN[day] <- max(data$TRAN[day], (data$R[day] * data$Eto[day]))

      data$AVAIL[day] <- WAT0 + (data$Rain[day] - data$RUNOFF[day] -
        data$DRAIN[day] - data$TRAN[day])
      data$AVAIL[day] <- min(data$AVAIL[day], soilWHC)
      data$AVAIL[day] <- max(data$AVAIL[day], 0)
    }
  }

  data$R <- round(data$R, 3)
  data$AVAIL <- round(data$AVAIL, 3)
  data$TRAN <- round(data$TRAN, 3)
  data$DRAIN <- round(data$DRAIN, 3)
  data$RUNOFF <- round(data$RUNOFF, 3)

  return(data)
}


##############################################################################
##############################################################################
##############################################################################
