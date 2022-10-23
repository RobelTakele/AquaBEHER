#     calcSeasCal.R Rainy season calandar
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

#' @title Rainy Season Calendar
#'
#' @description This function estimates the rainy season calendar, i.e onset date, cessation data and duration of the rainy season based on
#' Agroclimatic approach. The agroclimatic approach defines the onset of the rainy season as the optimal date that ensures sufficient soil moisture
#' during planting and early growing periods to avoid crop failure after sowing and requires information on rainfall and temperature as well as
#' the soil water balance at daily time scale.
#'
#' @param data  an object as returned by calcWatBal or a dataframe having similar parameters.
#' @param onsetWind.start Earliest possible start data of the onset window.
#' @param onsetWind.end The late possible date for end of the onset window.
#' @param e_thresh  Threshold value of actual-to-potential evapotranspiration (Ea/Ep); \emph{default value: e_thresh = 0.25}.
#' @param AW_thr  Threshold value of plant available water (PAW); \emph{default value: AW_thr = 10mm}.
#' @param soilWHC Water holding capacity of the soil in (mm).
#'
#' @return The function generates a data frame containing the following components:
#'
#' \emph{\code{Year: year of the rainy season in "YYYY".}}
#'
#' \emph{\code{Onset.DOY: onset date of the rainy season in julian day.}}
#'
#' \emph{\code{Cesation.DOY: cessation date of the rainy season in julian day.}}
#'
#' \emph{\code{SeasDur: durtion of the season in days.}}
#'
#' @details
#'
#' As per agroclimatic approach, a normal rainy season (growing season) is defined as one when there is an excess of precipitation over
#' potential evapotranspiration (PET). Such a period meets the evapotransiration demands of crops and recharge the moisture of the soil profile
#' (FAO 1977; 1978; 1986). Thus, the rainy season calendar defined accordingly:
#'
#' \strong{Onset}
#'
#' The \emph{onset} of the rainy season will start on the first day after \emph{onsetWind.start}, when the actual-to-potential evapotranspiration
#' ratio \emph{(e_thresh)} is greater than 0.25 and is followed by a 20-day period in which plant available water \emph{(AW_thr)} remains above 10mm.
#'
#' \strong{Cesation}
#'
#' The rainy season will end, \emph{cessation}, on the first day after \emph{onsetWind.end}, when the actual-to-potential evapotranspiration ratio
#' \emph{(e_thresh)} is less than or equal to 0.25 and followed 12 consecutive non-growing days \emph{(AW_thr <= 10mm)}.
#'
#'  \strong{Duration}
#'
#' The \emph{duration} of the rainy season is taken as the difference between the Julian day numbers of the determined cessation date and the
#' determined onset date for that season, i.e. the number of days from onset to cessation.
#'
#' @references FAO, 1977. Crop water requirements. FAO Irrigation and Drainage Paper No. 24, by Doorenbos J and W.O. Pruitt. FAO, Rome, Italy.
#'
#' FAO 1978. Forestry for Local Community Development Food and Agriculture Organization of the United Nation (FAO), FAO Forestry paper, No 7, Rome.
#'
#' FAO, 1986. Early Agrometeorological crop yield forecasting. FAO Plant Production and Protection paper No. 73, by M. Frère and G.F. Popov.
#' FAO, Rome, Italy
#'
#' @seealso \code{\link{calcEto}, \link{calcWatBal}}
#'
#' @examples
#' # load example data:
#' data(climateData)
#'
#' # estimate daily PET:
#' PET <- calcEto(climateData)
#'
#' # Add the estimated PET 'ET.Daily' to a new column in climateData:
#' climateData$Eto <- PET$ET.Daily
#'
#' # estimate daily water balance for the soil having 100mm of WHC:
#' watBal<- calcWatBal(climateData, soilWHC = 100)
#'
#' # estimate the rainy season calandar (Onset, Cessation and Duration):
#'
#' onsetWind.start = "1980-09-01"  # earliest possible start data of the onset window
#' onsetWind.end = "1981-01-31"   # the late possible date for end of the onset window
#'
#' seasCal.dF <- calcSeasCal(watBal, onsetWind.start, onsetWind.end,
#'                           e_thresh = 0.25, AW_thr = 10, soilWHC = 100)
#'
#' str(seasCal.dF)
#'
#' @export

######################################################################################################################################################
# ***** function to estimate

calcSeasCal <- function(data, onsetWind.start, onsetWind.end, e_thresh = 0.25, AW_thr = 10, soilWHC) {

 onset.start <- format(as.Date(paste0(onsetWind.start)) ,"%j")
 onset.end <- format(as.Date(paste0(onsetWind.end)) ,"%j")
 cessation.start <- as.numeric(onset.end) + 60
# cessation.end

 data$DOY <- strftime(as.POSIXlt(as.Date.character(paste0(data$Year, "-", data$Month, "-", data$Day))), "%j")

 year.vec <- sort(unique(data$Year))


 seasCal.dF <- data.frame()

 for (yr in 1:(length(year.vec)-1))   {

   data.yr <- data[data$Year %in% c(year.vec[yr], year.vec[yr+1]), ]

 # ***** onset date based on agroClimatic definition
 #season has ended once 12 consecutive nongrowing days (eratio<0.2, here) have occurred
 #calculate 12-day count, if < 5mm then assign as planting date

 onset <- NA
 onset2 <- NA

 for (day in onset.start :(nrow(data.yr)-1)) {

   if (is.na(onset) & !is.na(data.yr$ERATIO[2]) & (data.yr$ERATIO[day] >= e_thresh)) {

     etseq <- data.yr$AVAIL[day:(day+28)]
     etcount <- length(which(etseq > AW_thr))

     if (etcount > 19) {

       onset <- day

     }  else (next)
   }
 }


 # if (!is.na(onset) & onset > onset.end) {onset <- onset.end}

 if (!is.na(onset)) {onset2 <- as.numeric(data.yr$DOY[onset])}

 onset <- ifelse(soilWHC  < 10, NA, onset)

 onset.dF <- c(onset2, onset)

# ***************************************************************************************************************************************************
 #season has ended once 12 consecutive nongrowing days (eratio<0.2, here) have occurred
 #calculate 12-day count, if < 5mm then assign as

 edate <- NA
 edate2 <- NA

 for (day in cessation.start:(nrow(data.yr)- 12)) {

   if (is.na(edate) & !is.na(data.yr$ERATIO[2])  & (data.yr$ERATIO[day] <= e_thresh)) {

     etseq <- data.yr$AVAIL[(day):(day+12)]
     etcount <- length(which(etseq <= AW_thr))

     if (etcount > 11) {

       edate <- day+1

     } else (next)
   }
 }

 # if (!is.na(edate) & edate > maxdate) {edate <- cessation.end}

 if (!is.na(edate)) {edate2 <- as.numeric(data.yr$DOY[edate])}

 edate <- ifelse(soilWHC < 10, NA, edate)

 edate.dF <- c(edate2, edate)

# ***************************************************************************************************************************************************

 seasdur <- NA

 if(!is.na(onset.dF[1]) & !is.na(edate.dF[1])) {

   seasdur <- length(which(data.yr$DOY == sprintf("%03d", onset.dF[1]))[1]: which(data.yr$DOY == sprintf("%03d", edate.dF[1]))[2])

 }

 seasCal.dF.yr <- cbind.data.frame(Year = year.vec[yr],
                                   Onset.DOY = onset.dF[1],
                                #   Onset.index = onset.dF[2],
                                   Cesation.DOY = edate.dF[1],
                                #   Cesation.index = edate.dF[2],
                                   SeasDur = seasdur)

 seasCal.dF <- rbind(seasCal.dF, seasCal.dF.yr)

# ***************************************************************************************************************************************************

 } # for yr

 return(seasCal.dF)

######################################################################################################################################################

}


#####################################################################################################################################################
#####################################################################################################################################################
#####################################################################################################################################################
