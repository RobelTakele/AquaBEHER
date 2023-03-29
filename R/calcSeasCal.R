#     calcSeasCal.R Rainy season calandar
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
#' @title Rainy Season Calendar
#'
#' @description This function estimates the rainy season calendar, i.e onset
#' date, cessation date and duration of the rainy season based on Agroclimatic
#' approach. The agroclimatic approach defines the onset of the rainy season as
#' the optimal date that ensures sufficient soil moisture during planting and
#' early growing periods to avoid crop failure after sowing and requires
#' information on rainfall, reference evapotranspiration and accounting of the
#' daily soil water balance parameters.
#'
#' @param data  an R object of dataframe as returned by \code{\link{calcWatBal}}
#' or a dataframe having similar parameters.
#' @param onsetWind.start Earliest possible start date of the onset window.
#' @param onsetWind.end The latest possible date for end of the onset window.
#' @param cessaWind.end The latest possible date for end of the cessation window.
#' @param soilWHC Water holding capacity of the soil at root zone depth in (mm).
#'
#' @return The function generates list of dataframes  with columns of variables:
#'
#' \strong{\code{Onset.dF:}} a data frame with columns of onset variables:
#'
#' \verb{    } \emph{Year:} year of the season under investigation, "YYYY".
#'
#' \verb{    } \emph{onset.Year:} year of the season under investigation, "YYYY".
#'
#' \verb{    } \emph{onset.Month:} month of the onset of the season in "MM".
#'
#' \verb{    } \emph{onset.Day:} day of the onset of the season in "DD".
#'
#' \verb{    } \emph{JD:} onset date of the season in Julian day, "DOY".
#'
#' \verb{    } \emph{YYYYDOY:} onset date of the season in "YYYY-DOY".
#'
#' \verb{    } \emph{Year:} year of the season under investigation, "YYYY".
#'
#' \strong{\code{Cessation.dF:}} a data frame with columns of onset variables:
#'
#' \verb{    } \emph{Year:} year of the season under investigation, "YYYY".
#'
#' \verb{    } \emph{cessation.Year:} year of the season under investigation,
#' "YYYY".
#'
#' \verb{    } \emph{cessation.Month:} month of the cessation of the season in
#' "MM".
#'
#' \verb{    } \emph{cessation.Day:} day of the cessation of the season in "DD".
#'
#' \verb{    } \emph{JD:} cessation date of the season in Julian day, "DOY".
#'
#' \verb{    } \emph{YYYYDOY:} cessation date of the season in "YYYY-DOY".
#'
#' \strong{\code{Duration.dF:}} a data frame with columns of onset variables:
#'
#' \verb{    } \emph{Year:} year of the season under investigation, "YYYY".
#'
#' \verb{    } \emph{onset.YYYYDOY:} onset date of the season in "YYYY-DOY".
#'
#' \verb{    } \emph{cessation.YYYYDOY:} cessation date of the season in
#' "YYYY-DOY".
#'
#' \verb{    } \emph{Duration:} duration of the season in "days".
#'
#' @details
#'
#' As per agroclimatic approach, a normal rainy season (growing season) is
#' defined as one when there is an excess of precipitation over potential
#' evapotranspiration (PET). Such a period met the evapotransiration demands of
#' crops and recharge the moisture of the soil profile (FAO 1977; 1978; 1986).
#' Thus, the rainy season calendar defined accordingly:
#'
#' \strong{Onset}
#'
#' The \emph{onset} of the rainy season will start on the first day after
#' \emph{onsetWind.start}, when the actual-to-potential evapotranspiration ratio
#' is greater than 0.5 for 7 consecutive days, followed by a 20-day period in
#' which plant available water remains above wilting over the root zone of the
#' soil layer.
#'
#' \strong{Cesation}
#'
#' The rainy season will end, \emph{cessation}, on the first day after
#' \emph{onsetWind.end}, when the actual-to-potential evapotranspiration ratio
#' is less than 0.5 for 7 consecutive days, followed by 12 consecutive
#' non-growing days in which plant available water remains below wilting over
#' the root zone of the soil layer.
#'
#'  \strong{Duration}
#'
#' The \emph{duration} of the rainy season is the total number of days from
#' onset to cessation of the season.
#'
#' @references FAO, 1977. Crop water requirements. FAO Irrigation and Drainage
#' Paper No. 24, by Doorenbos J and W.O. Pruitt. FAO, Rome, Italy.
#'
#' FAO 1978. Forestry for Local Community Development Food and Agriculture
#' Organization of the United Nation (FAO), FAO Forestry paper, No 7, Rome.
#'
#' FAO, 1986. Early Agrometeorological crop yield forecasting. FAO Plant
#' Production and Protection paper No. 73, by M. Frère and G.F. Popov. FAO,
#' Rome, Italy
#'
#' @seealso \code{\link{calcEto}, \link{calcWatBal}}
#'
#' @importFrom ggplot2 ggplot geom_line geom_area scale_x_date scale_y_continuous labs theme_linedraw theme
#' @importFrom dplyr group_by summarize
#' @importFrom lubridate as_date
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
#' watBal <- calcWatBal(AgroClimateData, soilWHC = 100)
#'
#' # estimate the rainy season calandar (Onset, Cessation and Duration):
#' onsetWind.start = "1996-09-01"  # earliest possible start data of the onset window
#' onsetWind.end = "1997-01-31"   # the late possible date for end of the onset window
#' cessaWind.end = "1997-06-30"   # the late possible date for end of the cessation window
#'
#' seasCal.lst <- calcSeasCal(watBal, onsetWind.start, onsetWind.end, cessaWind.end, soilWHC = 100)
#'
#' str(seasCal.lst)
#'
#' }
#' @export

###############################################################################
# ***** function to estimate

calcSeasCal <- function(data, onsetWind.start, onsetWind.end,
                        cessaWind.end, soilWHC) {

  # ***** Check of specific data requirement

  if ((is.null(data$Year) & is.null(data$Month) & is.null(data$Day)) |
      ((length(which(is.na(data$Year)))>0)&
      (length(which(is.na(data$Month)))>0)&
      (length(which(is.na(data$Day)))>0))) {

    stop("Required data column for [Year], [Month] and [Day] is missing!")

  }

  if (is.null(soilWHC) | is.na(soilWHC)) {

    stop("Required data for water holding capacity of the soil [soilWHC]
         is missing! ")

  }

  # if (is.null(data$Rain) | length(which(is.na(data$Rain)))> 0) {
  #
  # stop("Required data column for daily rainfall amount [Rain] is missing! ")
  #
  # }

  if (is.null(data$R) | length(which(is.na(data$R)))> 0) {

    stop("Required data column for actual-to-potential evapotranspiration
         amount [R] is missing! ")

  }

  if (is.null(data$AVAIL) | length(which(is.na(data$AVAIL)))> 0) {

    stop("Required data column for available soil water amount [AVAIL]
         is missing! ")

  }

  if (is.null(onsetWind.start) | is.na(onsetWind.start)) {

    stop("Required date for start of the onset window [onsetWind.start]
         is missing! ")

  }

  if (is.null(onsetWind.end) | is.na(onsetWind.end)) {

    stop("Required date for end of the onset window [onsetWind.end]
         is missing! ")

  }

  if (is.null(cessaWind.end) | is.na(cessaWind.end)) {

    stop("Required date for end of the cessation window [cessaWind.end]
         is missing! ")

  }

 # ****************************************************************************

  data$date <- lubridate::as_date(paste0(data$Year, "-", data$Month, "-",
                                         data$Day))
  data$DOY <- strftime(as.POSIXlt(lubridate::as_date(paste0(data$Year, "-",
                                                            data$Month, "-",
                                                            data$Day))), "%j")
  data$YYDOY <- paste0(data$Year, "-", data$DOY)
  year.vec <- as.numeric(sort(unique(data$Year)))

  Rindex.thr = 0.5
  PAW.thr = min(max((0.25 * soilWHC), 15), 30)
#  data$Rain[data$Rain < 1] <- 0

 if (lubridate::as_date(onsetWind.start) < lubridate::as_date(onsetWind.end) |
     lubridate::as_date(onsetWind.start) < lubridate::as_date(cessaWind.end)) {

   year.len <- length(year.vec) -1

 } else {

   year.len <- length(year.vec)

 }

# *****************************************************************************

 Onset.dF <- data.frame(Year = rep(NA, year.len),
                        onset.Year = rep(NA, year.len),
                        onset.Month = rep(NA, year.len),
                        onset.Day = rep(NA, year.len),
                        onset.JD = rep(NA, year.len),
                        onset.Value = rep(NA, year.len))

 Cessation.dF <- data.frame(Year = rep(NA, year.len),
                            cessation.Year = rep(NA, year.len),
                            cessation.Month = rep(NA, year.len),
                            cessation.Day = rep(NA, year.len),
                            cessation.JD  = rep(NA, year.len),
                            cessation.Value = rep(NA, year.len))

 Duration.dF <- data.frame(Year = rep(NA, year.len),
                           onset.YYYYDOY = rep(NA, year.len),
                           cessation.YYYYDOY = rep(NA, year.len),
                           Duration = rep(NA, year.len))

 for (yr in 1:(year.len))   {

   if (!is.null(onsetWind.start) & !is.null(cessaWind.end)) {

     ons = as.numeric(format(lubridate::as_date(onsetWind.start), "%j"))
     one = as.numeric(format(lubridate::as_date(onsetWind.end), "%j"))
     cne = as.numeric(format(lubridate::as_date(cessaWind.end), "%j"))

 onsetWind.start.yr <- lubridate::as_date(paste0(year.vec[yr],
                                                 substr(onsetWind.start,5,10)))

     if (ons < one) {

 onsetWind.end.yr <- lubridate::as_date(paste0(year.vec[yr],
                                               substr(onsetWind.end, 5, 10)))

     } else {

     onsetWind.end.yr <- lubridate::as_date(paste0(year.vec[yr+1],
                                                   substr(onsetWind.end,5,10)))

     }

     data.onset.yr =
       data[which(onsetWind.start.yr ==
                    data$date):which((onsetWind.end.yr + 20) == data$date), ]

     # ***** onset date

     onset <- NA

  for (day in 1:(nrow(data.onset.yr)- 20)) {

    if (is.na(onset) & (length(which(is.na(data.onset.yr$R))) < 1) &
       (data.onset.yr$R[day] >= Rindex.thr) &
       (data.onset.yr$R[day+1] >= Rindex.thr) &
       (data.onset.yr$R[day+2] >= Rindex.thr) &
       (data.onset.yr$R[day+3] >= Rindex.thr) &
       (data.onset.yr$R[day+4] >= Rindex.thr) &
       (data.onset.yr$R[day+5] >= Rindex.thr) &
       (data.onset.yr$R[day+6] >= Rindex.thr) &
       (data.onset.yr$R[day+7] >= Rindex.thr) &
       (data.onset.yr$R[day+8] >= Rindex.thr) &
       (data.onset.yr$R[day+9] >= Rindex.thr) & (
         data.onset.yr$R[day+10] >= Rindex.thr)) {

         avail.vec <- data.onset.yr$AVAIL[day:(day+20)]
         avail.grDay <- length(which(avail.vec  > PAW.thr))

         if (avail.grDay > 15) {

           onset <- day

         }  else (next)
       }
     }


     if (is.na(onset)){onset.index = NA} else
       {onset.index = data.onset.yr$YYDOY[onset]}
     if (is.na(onset)){onset.Year = NA} else
       {onset.Year = as.numeric(substr(onset.index, 1, 4))}
     if (is.na(onset)){onset.Month = NA} else
       {onset.Month = as.numeric(data.onset.yr$Month[onset])}
     if (is.na(onset)){onset.Day = NA} else
       {onset.Day = as.numeric(data.onset.yr$Day[onset])}

     onset.yr.dF  = data.frame(Year = as.numeric(year.vec[yr]),
                               onset.Year = onset.Year,
                               onset.Month = onset.Month,
                               onset.Day = onset.Day,
                               onset.JD = as.numeric(substr(onset.index, 6, 8)),
                               onset.Value = onset)

     Onset.dF[yr,] <- onset.yr.dF

# *****************************************************************************

      if (!is.na(onset)) {

       cessaWind.start.yr <- lubridate::as_date(paste0(onset.Year, "-",
                                                       onset.Month, "-",
                                                       onset.Day)) + 35
     }

     if (!is.na(onset) & (ons < cne)) {

    cessaWind.end.yr <- lubridate::as_date(paste0(year.vec[yr],
                                                  substr(cessaWind.end,5,10)))

     } else {

    cessaWind.end.yr <- lubridate::as_date(paste0(year.vec[yr+1],
                                                  substr(cessaWind.end, 5, 10)))

     }

     if (!is.na(onset)) {

  data.cessation.yr = data[which(cessaWind.start.yr ==
                                   data$date):which((cessaWind.end.yr + 20) ==
                                                      data$date), ]

      }

 cessation <- NA

 if (!is.na(onset)) {

  for (day in 1:(nrow(data.cessation.yr)- 20)) {

    if (is.na(cessation) & (length(which(is.na(data.cessation.yr$R))) < 1) &
        (data.cessation.yr$R[day] < Rindex.thr) &
        (data.cessation.yr$R[day+1] < Rindex.thr) &
        (data.cessation.yr$R[day+2] < Rindex.thr) &
        (data.cessation.yr$R[day+3] < Rindex.thr) &
        (data.cessation.yr$R[day+4] < Rindex.thr) &
        (data.cessation.yr$R[day+5] < Rindex.thr) &
        (data.cessation.yr$R[day+6] < Rindex.thr) &
        (data.cessation.yr$R[day+7] < Rindex.thr) &
        (data.cessation.yr$R[day+8] < Rindex.thr) &
        (data.cessation.yr$R[day+9] < Rindex.thr)&
        (data.cessation.yr$R[day+10] < Rindex.thr)) {

      avail.vec <- data.cessation.yr$AVAIL[(day):(day+20)]
      avail.grDay <- length(which(avail.vec <= (PAW.thr+10)))

      if (avail.grDay > 12) {

        cessation <- day+1

      } else (next)
    }
  }
}

 if (is.na(onset)) {cessation = NA}


 if (is.na(cessation)){cessation.index = NA}else
   {cessation.index = data.cessation.yr$YYDOY[cessation]}
 if (is.na(cessation)){cessation.Year = NA} else
   {cessation.Year = as.numeric(substr(cessation.index, 1, 4))}
 if (is.na(cessation)){cessation.Month = NA} else
   {cessation.Month = as.numeric(data.cessation.yr$Month[cessation])}
 if (is.na(cessation)){cessation.Day = NA} else
   {cessation.Day = as.numeric(data.cessation.yr$Day[cessation])}


 cessation.yr.dF <- data.frame(Year = as.numeric(year.vec[yr]),
                               cessation.Year = cessation.Year,
                               cessation.Month = cessation.Month,
                               cessation.Day = cessation.Day,
                               cessation.JD = as.numeric(substr(
                                 cessation.index, 6, 8)),
                               cessation.Value = (cessation +
                                                    (Onset.dF$onset.Value[yr]
                                                     + 35)))

   Cessation.dF[yr,] <- cessation.yr.dF

# *****************************************************************************
  duration <- NA

  if (!is.na(onset) & !is.na(cessation)) {

 duration <- length(seq(from =
                        lubridate::as_date(paste0(onset.yr.dF$onset.Year, "-",
                                                  onset.yr.dF$onset.Month, "-",
                                                  onset.yr.dF$onset.Day)),
                        to = lubridate::as_date(paste0(cessation.yr.dF$cessation.Year,"-",
                                                       cessation.yr.dF$cessation.Month,"-",
                                                       cessation.yr.dF$cessation.Day)),
                        by = "day"))

  }

  if (is.na(onset)) {duration = NA}

   Duration.yr.dF <- data.frame(Year = as.numeric(year.vec[yr]),
                                onset.YYYYDOY = onset.index,
                                cessation.YYYYDOY = cessation.index,
                                Duration = duration)

   Duration.dF[yr,] <- Duration.yr.dF


   }

# *****************************************************************************
  } # for yr


 seasCal.lst <- list(Onset.dF, Cessation.dF, Duration.dF )

 return(seasCal.lst)

###############################################################################

}

###############################################################################
###############################################################################
###############################################################################
