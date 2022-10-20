#' @title Rainy Season Calandar
#'
#' @description This function calculates rainy season calander based on Agroclimatic approach
#'
#' @param data = a dataframe. It should bu an object as returned by calcWatBal
#' @param onsetWind.start onset start
#' @param onsetWind.end onset end
#' @param e_thresh  thgreshold
#' @param AW_thr  PAW
#' @param soilWHC Whater holding capacity of the soil
#'
#'
#' @return The function generates a data frame containing the following components:
#'
#' \code{Year:} { year }
#'
#' \code{Onset.DOY:} { onset in DOY }
#'
#' \code{Onset.index:} {onset index  }
#'
#' \code{Cesation.DOY:} {  ce in DOY }
#'
#' \code{Cesation.index:} { ce in index  }
#'
#' \code{SeasDur:} {  durtion of the season }
#'
#' @references Allen, R.G.; Pereira, L.S.; Raes, D.; Smith, M. Crop Evapotranspiration: Guidelines for Computing Crop Water Requirements; FAO Irrigation and Drainage Paper no. 56; FAO: Rome, Italy, 1998; ISBN 92-5-104219-5.
#'
#' Doorenbos, J. and Pruitt, W.O. 1975. Guidelines for predicting crop water requirements, Irrigation and Drainage Paper 24, Food and Agriculture Organization of the United Nations, Rome, 179 p.
#'
#' @examples
#'
#' data(climateData)
#'
#' Eto.daily <- calcEto(climateData)
#'
#' climateData$Eto <- Eto.daily$ET.Daily
#'
#'  soilWHC = 100
#'
#' watBal.daily <- calcWatBal(climateData, soilWHC)
#'
#' onsetWind.start = "1980-09-01"
#' onsetWind.end = "1981-01-31"
#'
#' seasCal.dF <- calcSeasCal(watBal.daily, onsetWind.start, onsetWind.end,
#'                           e_thresh = 0.25, AW_thr = 10, soilWHC)
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
                                   Onset.index = onset.dF[2],
                                   Cesation.DOY = edate.dF[1],
                                   Cesation.index = edate.dF[2],
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
