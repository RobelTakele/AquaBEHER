#     calcWatBal.R Soil Water Balance
#
#     Copyright (C) 2022 Center of Plant Sciences, Scuola Superiore Sant’Anna
#     (http://www.capitalisegenetics.santannapisa.it)
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

#' @title Soil Water Balance
#'
#' @description Function to estimate a budget-based daily soil water balance.
#' It calculates the amount of water present in the root zone of a homogeneous
#' grass canopy growing on a well-drained and homogeneous soil.
#'
#' @param data a dataframe containing the required variables: Columns must
#' contain the following parameters:
#'
#'        Lat: latitude of the site in decimal degrees.
#'        Lon: longitude of the site in decimal degrees.
#'        Elev: elevation above sea level in (meters).
#'        Year: year of record "YYYY".
#'        Month: month of record "MM".
#'        Day: day of record "DD".
#'        Rain: daily rainfall in (mm).
#'        Eto: daily potential evapotranspiration in (mm).
#'
#' @param soilWHC \verb{  }Water holding capacity of the soil in (mm).
#'
#' @return The function generates a data frame containing the following
#' components:
#'
#' \emph{\code{DRAIN: amount of deep drainage in (mm).}}
#'
#' \emph{\code{TRAN: amount of water lost by transpiration in (mm).}}
#'
#' \emph{\code{RUNOFF: surface runoff in (mm).}}
#'
#' \emph{\code{AVAIL: available soil moisture storage in (mm).}}
#'
#' \emph{\code{R: actual-to-potential evapotranspiration ratio.}}
#'
#'
#' @references Allen, R.G.; Pereira, L.S.; Raes, D.; Smith, M. Crop
#' Evapotranspiration: Guidelines for Computing Crop Water Requirements; FAO
#' Irrigation and Drainage Paper no. 56; FAO: Rome, Italy, 1998;
#' ISBN 92-5-104219-5.
#'
#' Doorenbos, J. and Pruitt, W.O. 1975. Guidelines for predicting crop water
#' requirements, Irrigation and Drainage Paper 24, Food and Agriculture
#' Organization of the United Nations, Rome, 179 p.
#'
#' @seealso \code{\link{calcEto}, \link{calcSeasCal}}
#'
#' @importFrom graphics legend par
#' @importFrom graphics lines
#' @importFrom raster raster extract
#' @importFrom sp coordinates CRS proj4string
#'
#' @examples
#'
#' \dontrun{
#' # load example data:
#' data(AgroClimateData)
#'
#' # Estimate daily PET:
#' PET <- calcEto(AgroClimateData, method = "PM", Zh = 10)
#'
#' # Add the estimated PET 'ET.Daily' to a new column in AgroClimateData:
#' AgroClimateData$Eto <- PET$ET.Daily
#'
#' # Estimate daily water balance for the soil having 100mm of WHC:
#' watBal<- calcWatBal(AgroClimateData, soilWHC = 100)
#'
#' # Visualizing water balance parameters for 2019/20 season
#' watBal.19T20 <- watBal[watBal$Year %in% c(2019, 2020),]
#' date.vec <- as.Date.character(paste0(watBal.19T20$Year, "-",
#'                                      watBal.19T20$Month, "-",
#'                                      watBal.19T20$Day))
#'
#' plot(y = watBal.19T20$AVAIL, x = date.vec, ty="l", col="black", xlab="",
#' ylab=" Water (mm)",
#'        main="Daily Water Balance Parameters", lwd = 1, lty = 2)
#'  lines(y = watBal.19T20$Eto, x = date.vec, col="red", lwd = 3, lty = 1)
#'  lines(y = watBal.19T20$Rain, x = date.vec, col="blue", lwd = 1, lty = 1)
#'
#'   legend("bottomright",c("Rain","Eto","Available Moisture"),
#'         horiz=FALSE, bty='n', cex=1.2,lty=c(1,1,2),lwd=c(1,3,1),
#'         inset=c(0,0.7),
#'         xpd=TRUE, col=c("blue","red","black"))
#' }
#'@export

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

 # data(rcn, envir = environment())
 rcn <- raster::raster(system.file("extdata/rcn.tif", package = "AquaBEHER"))

  pts.dF <- data.frame(Lat = as.numeric(data$Lat[1]),
                       Lon = as.numeric(data$Lon[1]))
  pts.sp <- pts.dF
  sp::coordinates(pts.sp) <- ~Lon+Lat
  sp::proj4string(pts.sp) <- sp::CRS("+proj=longlat")
  rcn.pts <- raster::extract(rcn, pts.sp)

  if (!is.null(rcn.pts) & !is.na(rcn.pts)) {

    CN <- rcn.pts

  } else {

    CN <- 65  # *** well managed grass

  }

  DC <- 0.55 #

  MUF <- 0.1
  WATwp <- 0.15 * soilWHC
  # Maximum abstraction (for run off)
  S <- 25400/CN-254
  # Initial Abstraction (for run off)
  IA <- 0.2*S

  date.vec <- as.Date(paste0(data$Year, "-", data$Month, "-", data$Day))

  data$RUNOFF <- data$DRAIN <- data$TRAN <- data$AVAIL <- data$R <- NA

  data$Rain[data$Rain < 2] <- 0

  for (day in seq_along(date.vec)) {

    if (day == 1) {

      WAT0 <- 0

      # Change in water before drainage (Precipitation - Runoff)

      if (data$Rain[day] > IA){
        data$RUNOFF[day] <- (data$Rain[day]-0.2*S)^2/(data$Rain[day]+0.8*S)
      }else{
        data$RUNOFF[day] <- 0
      }

      data$RUNOFF[day] <- max(data$RUNOFF[day], 0)

      # Calculating the amount of deep drainage

      if ((WAT0+data$Rain[day]-data$RUNOFF[day]) > soilWHC){
        data$DRAIN[day] <- DC*(WAT0+data$Rain[day]-data$RUNOFF[day]-soilWHC)
      }else{
        data$DRAIN[day] <- 0
      }

      data$DRAIN[day] <- max(data$DRAIN[day], 0)

      # Calculating the amount of water lost by transpiration (after drainage)

      data$TRAN[day] <- min(MUF*(WAT0+data$Rain[day]-data$RUNOFF[day]-
                                   data$DRAIN[day]- WATwp), data$Eto[day])
      data$TRAN[day] <- max(data$TRAN[day], 0)
      data$TRAN[day] <- min(data$TRAN[day], soilWHC)

      data$R[day] <- data$TRAN[day] / data$Eto[day]
      data$TRAN[day] <- max(data$TRAN[day], (data$R[day] * data$Eto[day]))

      data$AVAIL[day] <- WAT0 + (data$Rain[day]-data$RUNOFF[day]-
                                   data$DRAIN[day] - data$TRAN[day])
      data$AVAIL[day] <- min(data$AVAIL[day], soilWHC)
      data$AVAIL[day] <- max(data$AVAIL[day], 0)

    } else {

      WAT0 <- data$AVAIL[day-1]

      # Change in water before drainage (Precipitation - Runoff)

      if (data$Rain[day] > IA){
        data$RUNOFF[day] <- (data$Rain[day]-0.2*S)^2/(data$Rain[day]+0.8*S)
      }else{
        data$RUNOFF[day] <- 0
      }

      data$RUNOFF[day] <- max(data$RUNOFF[day], 0)

      # Calculating the amount of deep drainage

      if ((WAT0+data$Rain[day]-data$RUNOFF[day]) > soilWHC){
        data$DRAIN[day] <- DC*(WAT0+data$Rain[day]-data$RUNOFF[day]-soilWHC)
      }else{
        data$DRAIN[day] <- 0
      }

      data$DRAIN[day] <- max(data$DRAIN[day], 0)

      # Calculating the amount of water lost by transpiration (after drainage)

      data$TRAN[day] <- min(MUF*(WAT0+data$Rain[day]-data$RUNOFF[day]-
                                   data$DRAIN[day]- WATwp), data$Eto[day])
      data$TRAN[day] <- max(data$TRAN[day], 0)
      data$TRAN[day] <- min(data$TRAN[day], soilWHC)

      data$R[day] <- data$TRAN[day] / data$Eto[day]
      data$TRAN[day] <- max(data$TRAN[day], (data$R[day] * data$Eto[day]))

      data$AVAIL[day] <- WAT0 + (data$Rain[day]-data$RUNOFF[day]-
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

