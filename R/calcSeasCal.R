###############################################################################
###############################################################################
#
#      calcSeasCal.R Wet season calendar
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

#' @title Wet Season Calendar
#'
#' @description Estimates the wet season calendar, including the onset date,
#' cessation date, and duration, based on an agroclimatic approach. The
#' function relies on daily soil water balance parameters.
#'
#' @param data  An R dataframe returned by \code{\link{calcWatBal}} or
#' a dataframe with similar parameters.
#' @param onsetWind.start The earliest possible start date for the onset window
#' in "MM-DD" format.
#' @param onsetWind.end The latest possible end date for the onset window
#' in "MM-DD" format.
#' @param cessaWind.end The latest possible end date for the cessation window
#' in "MM-DD" format.
#' @param soilWHC The available soil water holding capacity at
#' root zone depth (in mm).
#'
#' @return A dataframe containing the following columns:
#' \describe{
#'   \item{Year}{The year of the season (YYYY).}
#'   \item{OnsetDate}{The onset date, formatted as "YYYY-MM-DD".}
#'   \item{OnsetDOY}{The Julian day (DOY) of the onset.}
#'   \item{OnsetValue}{The number of days since \code{onsetWind.start}.}
#'   \item{CessationDate}{The cessation date, formatted as "YYYY-MM-DD".}
#'   \item{CessationDOY}{The Julian day (DOY) of the cessation.}
#'   \item{CessationValue}{The number of days since \code{onsetWind.start}.}
#'   \item{Duration}{The duration of the wet season (in days).}
#' }
#'
#' @details
#' The agroclimatic approach defines the wet season based on the balance
#' between precipitation and potential evapotranspiration (PET). The wet season
#' begins when the moisture available to crops exceeds their
#' evapotranspiration demands, ensuring optimal growth conditions.
#'
#' \strong{Onset:}
#' The wet season onset is defined as the first day after \code{onsetWind.start}
#' when the ratio of actual evapotranspiration (AET) to potential
#' evapotranspiration (PET) exceeds 0.5 for at least 5 consecutive days, and the
#' soil moisture remains above 25% of the available soil water holding capacity
#' (\code{soilWHC}) for a minimum of 20 consecutive days, ensuring sufficient
#' moisture availability for plant growth.
#'
#' \strong{Cessation:}
#' The wet season ends on the first day after \code{onsetWind.end} when the
#' AET/PET ratio falls below 0.5 for at least 5 consecutive days, and the
#' soil moisture remains below 25% of the available soil water holding capacity
#' (\code{soilWHC}) for a minimum of 12 consecutive days.
#'
#' \strong{Duration:}
#' The total duration of the wet season is the number of days between
#' onset and cessation.
#'
#' @references
#' FAO, 1977. Crop water requirements. FAO Irrigation and Drainage Paper No. 24,
#' by Doorenbos J. and W.O. Pruitt. FAO, Rome, Italy.
#'
#' FAO, 1978. Forestry for Local Community Development. FAO Forestry Paper
#' No. 7, FAO, Rome.
#'
#' FAO, 1986. Early Agrometeorological Crop Yield Forecasting. FAO Plant
#' Production and Protection Paper No. 73, by M. Frère and G.F.
#' Popov. FAO, Rome.
#'
#' @seealso \code{\link{calcEto}, \link{calcWatBal}}
#'
#' @importFrom dplyr group_by summarize mutate %>%
#' @importFrom lubridate as_date
#' @importFrom zoo rollapply
#' @importFrom rlang .data
#'
#' @examples
#' \donttest{
#' ## Load example data:
#' data(AgroClimateData)
#'
#' ## Estimate daily PET:
#' PET <- calcEto(AgroClimateData, method = "PM", Zh = 10)
#'
#' ## Add the estimated PET 'ET.Daily' to a new column in AgroClimateData:
#' AgroClimateData$Eto <- PET$ET.Daily
#'
#' ## Estimate daily water balance for the soil having 100mm of WHC:
#' watBal.list <- calcWatBal(data = AgroClimateData, soilWHC = 100)
#' watBal <- watBal.list$data
#'
#' ## seasonal calendar is estimated for the onset window ranges from
#' ## 01 September to 31 January having a soil with 100mm of WHC:
#'
#' soilWHC <- 100
#' onsetWind.start <- "09-01"
#' onsetWind.end <- "01-31"
#' cessaWind.end <- "06-30"
#'
#' seasCal.dF <- calcSeasCal(data = watBal, onsetWind.start, onsetWind.end,
#' cessaWind.end, soilWHC)
#'
#'  str(seasCal.dF)
#'
#' }
#' @export
###############################################################################
###############################################################################
# ***** function to estimate WSC

calcSeasCal <- function(data, onsetWind.start, onsetWind.end,
                        cessaWind.end, soilWHC) {

         ## ***** Validate parameters ***** ##

  requiredColumns <- c("Year", "Month", "Day", "R", "AVAIL")
  missingCols <- setdiff(requiredColumns, colnames(data))

  if (length(missingCols) > 0) {
    stop(paste("The required data columns",
               paste(missingCols, collapse = ", "), "are missing!"))
  }

  ## ***** Validate onset window start ("MM-DD"):

  if (is.null(onsetWind.start) || is.na(onsetWind.start) ||
      !grepl("^\\d{2}-\\d{2}$", onsetWind.start)) {
    stop("The start date for the onset window 'onsetWind.start' is
         missing or not in 'MM-DD' format!")
  }

  ## ***** Validate onset window end ("MM-DD"):

  if (is.null(onsetWind.end) || is.na(onsetWind.end) ||
      !grepl("^\\d{2}-\\d{2}$", onsetWind.end)) {
    stop("The end date for the onset window 'onsetWind.end' is
         missing or not in 'MM-DD' format!")
  }

  ## ***** Validate cessation window end ("MM-DD"):

  if (is.null(cessaWind.end) || is.na(cessaWind.end) ||
      !grepl("^\\d{2}-\\d{2}$", cessaWind.end)) {
    stop("The end date for the cessation window [cessaWind.end] is missing
         or not in 'MM-DD' format!")
  }

  ## ***** Validate R-index:

  if (any(is.na(data$R)) || any(data$R < 0 | data$R > 1)) {
    stop("The actual-to-potential evapotranspiration ratio 'R'
         must be between 0 and 1!")
  }

  ## ***** Validate AVAIL (available soil water):

  if (any(is.na(data$AVAIL)) || any(data$AVAIL < 0)) {
    stop("The available soil water 'AVAIL' column contains invalid or
         negative values!")
  }

  ## ***** Validate soilWHC:

  if (is.null(soilWHC) || !is.numeric(soilWHC) || soilWHC <= 0) {
    stop("The soil water holding capacity 'soilWHC' must be a
         positive number!")
  }

##############################################################################
##############################################################################

  Rindex.thr <- 0.5
  RAW.thr <- max((0.25 * soilWHC), 5)

## ****************************************************************************

  data$date <- lubridate::as_date(paste0(data$Year, "-",
                                         data$Month, "-", data$Day))

  dates <- as.Date(data$date)
  years <- unique(data$Year)

  # WSC.dF <- data.frame(Year =   years, # integer(),
  #                      OnsetDate = as.Date(character()),
  #                      OnsetDOY = integer(),
  #                      OnsetValue = integer(),
  #                      CessationDate = as.Date(character()),
  #                      CessationDOY = integer(),
  #                      CessationValue = integer(),
  #                      Duration = integer())

  WSC.dF <- data.frame(
    Year = years,
    OnsetDate = as.Date(rep(NA, length(years))),
    OnsetDOY = integer(length(years)),
    OnsetValue = integer(length(years)),
    CessationDate = as.Date(rep(NA, length(years))),
    CessationDOY = integer(length(years)),
    CessationValue = integer(length(years)),
    Duration = integer(length(years))
  )

## ****************************************************************************

  for (yearS in seq_along(years)) {

    yearStart <- years[yearS]

     onsetWind.start.date <- as.Date(paste0(yearStart, "-",
                                           onsetWind.start), format="%Y-%m-%d")

    if (as.numeric(substring(onsetWind.end, 1, 2)) <
        as.numeric(substring(onsetWind.start, 1, 2))) {
      onsetWind.end.date <- as.Date(paste0(yearStart + 1, "-",
                                           onsetWind.end), format="%Y-%m-%d")
    } else {
      onsetWind.end.date <- as.Date(paste0(yearStart, "-",
                                           onsetWind.end), format="%Y-%m-%d")
    }

    onset.window <- data.frame(dates, data$R, data$AVAIL) %>%
      dplyr::filter(dates >= onsetWind.start.date & dates <= onsetWind.end.date)

    colnames(onset.window) <- c("dates", "R", "AVAIL")

    onset.window <- onset.window %>%
      dplyr::mutate(R.aboveThreshold = .data$R > Rindex.thr,
                    AVAIL.aboveThreshold = .data$AVAIL > RAW.thr)

    onset.window <- onset.window %>%
      dplyr::mutate(Rstreak = zoo::rollapply(.data$R.aboveThreshold,
                                             width = 7,
                                             FUN = all, fill = NA,
                                             align = "left"))

    onset.date <- NA

    for (i in which(onset.window$Rstreak == TRUE)) {

      if ((i + 15) <= nrow(onset.window)) {

        if (all(onset.window$AVAIL.aboveThreshold[i:(i + 15)],
                na.rm = TRUE)) {
          onset.date <- onset.window$dates[i]
          break
        }
      }
    }

   # if (is.na(onset.date)) next

    cessation.start.date <- onset.date + 25

    if (as.numeric(substring(cessaWind.end, 1, 2)) <
        as.numeric(substring(onsetWind.start, 1, 2))) {
      cessation.end.date <- as.Date(paste0(yearStart + 1, "-",
                                           cessaWind.end), format="%Y-%m-%d")
    } else {
      cessation.end.date <- as.Date(paste0(yearStart, "-",
                                           cessaWind.end), format="%Y-%m-%d")
    }

    cessation.window <- data.frame(dates, data$R, data$AVAIL) %>%
      dplyr::filter(dates >= cessation.start.date & dates <= cessation.end.date)

    colnames(cessation.window) <- c("dates", "R", "AVAIL")

    cessation.window <- cessation.window %>%
      dplyr::mutate(R.belowThreshold = .data$R < Rindex.thr,
                    AVAIL.belowThreshold = .data$AVAIL < (RAW.thr +
                                                            0.1 * soilWHC))

    cessation.window <- cessation.window %>%
      dplyr::mutate(Rstreak = zoo::rollapply(.data$R.belowThreshold,
                                             width = 7,
                                             FUN = all, fill = NA,
                                             align = "left"))

    cessation.date <- NA

    for (i in which(cessation.window$Rstreak == TRUE)) {

      if ((i + 14) <= nrow(cessation.window)) {

        if (all(cessation.window$AVAIL.belowThreshold[i :(i + 14)],
                na.rm = TRUE)) {
          cessation.date <- cessation.window$dates[i]
          break
        }
      }
    }

    if (!is.na(cessation.date) && !is.na(onset.date)) {
      duration <- as.numeric(difftime(cessation.date,
                                      onset.date, units = "days"))
    } else {
      cessation.date <- NA
      duration <- NA
    }

      data.out.yr <- data.frame(Year = yearStart,
                                OnsetDate = onset.date,
                                OnsetDOY = if (!is.na(onset.date))
                                {format(onset.date, "%j")} else {NA},
                                OnsetValue = if (!is.na(onset.date))
                                {length(seq.Date(onsetWind.start.date,
                                                 onset.date,
                                                 by = "day"))} else {NA},
                                CessationDate = cessation.date,
                                CessationDOY = if (!is.na(cessation.date))
                                {format(cessation.date, "%j")} else {NA},
                                CessationValue = if (!is.na(cessation.date))
                                {length(seq.Date(onsetWind.start.date,
                                                 cessation.date,
                                                 by = "day"))} else {NA},
                                Duration = duration)

    WSC.dF[yearS, ] <- data.out.yr

    # WSC.dF <- rbind(WSC.dF,
    #                 data.frame(Year = yearStart,
    #                            OnsetDate = onset.date,
    #                            OnsetDOY = if (!is.na(onset.date))
    #                            {format(onset.date, "%j")} else {NA},
    #                            OnsetValue = if (!is.na(onset.date))
    #                            {length(seq.Date(onsetWind.start.date,
    #                                             onset.date,
    #                                             by = "day"))} else {NA},
    #                            CessationDate = cessation.date,
    #                            CessationDOY = if (!is.na(cessation.date))
    #                              {format(cessation.date, "%j")} else {NA},
    #                            CessationValue = if (!is.na(cessation.date))
    #                              {length(seq.Date(onsetWind.start.date,
    #                                               cessation.date,
    #                                               by = "day"))} else {NA},
    #                             Duration = duration))

    }

  return(WSC.dF)

}

###############################################################################
###############################################################################
#                >>>>>>>>>>   End of code   <<<<<<<<<<                        #
###############################################################################
###############################################################################
