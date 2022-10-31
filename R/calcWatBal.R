#     calcWatBal.R Soil Water Balance
#
#     Copyright (C) 2022 Center of Plant Sciences, Scuola Superiore Sant’Anna (http://www.capitalisegenetics.santannapisa.it)
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
######################################################################################################################################################

#' @title Soil Water Balance
#'
#' @description Calculates a daily soil water balance computation for the root zone according to methods described in the FAO Irrigation
#' and drainage paper 56 (Doorenbos et al, 1975; Allen et al, 1998)
#'
#' @param data a dataframe containing the required climate variables: Columns must contain the following parameters:
#'
#'        Lat: latitude of the site in decimal degrees.
#'        Lon: longitude of the site in decimal degrees.
#'        Elev: elevation above sea level in (meters).
#'        Year: year of record "YYYY".
#'        Month: month of record "MM".
#'        Day: day of record "DD".
#'        Rain: daily rainfall in (mm).
#'        Tmax: daily maximum temperature at 2-m height in (°C).
#'        Tmin: daily minimum temperature at 2-m height in (°C).
#'        Eto: daily potential evapotranspiration in (mm).
#'
#' @param soilWHC Water holding capacity of the soil in (mm).
#'
#'
#' @return The function generates a data frame containing the following components:
#'
#' \emph{\code{cumRAIN: accumulated rainfall since the begning of the calculation in (mm).}}
#'
#' \emph{\code{DEMAND: aamospheric water demand (total moisture flux to atmospher) in (mm).}}
#'
#' \emph{\code{RUNOFF: surface runoff in (mm).}}
#'
#' \emph{\code{ERATIO: actual-to-potential evapotranspiration ratio.}}
#'
#' \emph{\code{AVAIL: available soil moisture storage in (mm).}}
#'
#' @references Allen, R.G.; Pereira, L.S.; Raes, D.; Smith, M. Crop Evapotranspiration: Guidelines for Computing Crop Water Requirements; FAO
#' Irrigation and Drainage Paper no. 56; FAO: Rome, Italy, 1998; ISBN 92-5-104219-5.
#'
#' Doorenbos, J. and Pruitt, W.O. 1975. Guidelines for predicting crop water requirements, Irrigation and Drainage Paper 24, Food and Agriculture
#' Organization of the United Nations, Rome, 179 p.
#'
#' @seealso \code{\link{calcEto}, \link{calcSeasCal}}
#'
#' @importFrom graphics legend par
#' @importFrom graphics lines
#'
#' @examples
#' # load example data:
#' data(AgroClimateData)
#'
#' # Estimate daily PET:
#' PET <- calcEto(AgroClimateData, method = "PM", crop = "short")
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
#'                                      watBal.19T20$Month, "-", watBal.19T20$Day))
#'
#' plot(y = watBal.19T20$Rain, x = date.vec, ty="l", col="blue", xlab="", ylab=" Water (mm)",
#'       main="Daily Water Balance Parameters")
#'  lines(y = watBal.19T20$Eto, x = date.vec, col="red", lwd = 3)
#'  lines(y = watBal.19T20$AVAIL, x = date.vec, col="black", lwd = 1, lty = 2)
#'
#'   legend("bottomright",c("Rain","Eto","Available Moisture"),
#'         horiz=FALSE, bty='n', cex=1.2,lty=c(1,1,2),lwd=c(2,2,2), inset=c(0,1),
#'         xpd=TRUE, col=c("blue","red","black"))#'
#'
#'@export

######################################################################################################################################################
# ***** function to estimate Rindex: actual to potential evapotranspiration ratio based on Jones (1987)

calcWatBal <- function(data, soilWHC) {

  Rindex <- function(soilWHC, Avail) {

    percwt <- min(c(100, Avail/soilWHC*100))
    percwt <- max(c(1,percwt))
    Eratio <- min(c(percwt/(97-3.868*sqrt(soilWHC)),1))

    return(Eratio)

  }

# ****************************************************************************************************************************************************

  calcRindex <- function(soilWHC, Kc = 1, Avail, Rain, Eto) {

    Avail <- min(c(Avail, soilWHC))
    Eratio <- Rindex(soilWHC, Avail)
    Demand <- Eratio * Kc * Eto
    result <- Avail + Rain - Demand
    Runoff <- result - soilWHC
    Avail <- min(c(soilWHC, result))
    Avail <- max(c(Avail,0))
    Runoff <- max(c(Runoff,0))

    out.dF <- data.frame(AVAIL = Avail,
                         DEMAND = Demand,
                         ERATIO = Eratio,
                         RAIN = Rain,
                         RUNOFF = Runoff)
    return(out.dF)
  }

 # ****************************************************************************************************************************************************


  data$AVAIL <- data$ERATIO <- data$RUNOFF <- data$DEMAND <- data$cumRAIN <- NA

  for (day in 1:nrow(data)) {

    if (day == 1) {

      data$cumRAIN[day] <- data$Rain[day]

      sWat <- calcRindex(soilWHC = soilWHC,
                         Kc = 1,
                         Avail = 0,
                         Rain = data$Rain[day],
                         Eto = data$Eto[day])

      data$AVAIL[day] <- sWat$AVAIL
      data$ERATIO[day] <- sWat$ERATIO
      data$RUNOFF[day] <- sWat$RUNOFF
      data$DEMAND[day] <- sWat$DEMAND

    } else {

      data$cumRAIN[day] <- data$cumRAIN[day-1] + data$Rain[day]

      sWat <- calcRindex(soilWHC = soilWHC,
                         Kc = 1,
                         Avail = data$AVAIL[day-1],
                         Rain = data$Rain[day],
                         Eto = data$Eto[day])

      data$AVAIL[day] <- sWat$AVAIL
      data$ERATIO[day] <- sWat$ERATIO
      data$RUNOFF[day] <- sWat$RUNOFF
      data$DEMAND[day] <- sWat$DEMAND

    }

  }


  return(data)

}


#####################################################################################################################################################
#####################################################################################################################################################
#####################################################################################################################################################
