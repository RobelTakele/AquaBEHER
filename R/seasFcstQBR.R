###############################################################################
###############################################################################
#
#      seasFcstQBR.R Seasonal Forecast of WSC
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

#' @title Seasonal Forecast of Wet Season Calendar (WSC) from Tercile Rainfall
#' Probabilities using Quantile Bin Resampling (QBR)
#'
#' @description
#' Generates seasonal forecasts for Wet Season Calendar (WSC)
#' variables (onset or cessation) using tercile seasonal rainfall probabilities
#  as input via Quantile Bin Resampling (QBR).
#
#' @param hisYearStart Starting year (YYYY) for historical resampling.
#' @param hisYearEnd Ending year (YYYY) for historical resampling.
#' @param hisWSCvar A data frame containing historical WSC simulations. This can
#' be the output from the `calcSeasCal` function or a similar data frame with
#' required columns.
#' @param seasRain A data frame containing seasonal rainfall data with columns
# "Year" and "sRain" (seasonal rainfall).
#' @param rainTerc A data frame with tercile probabilities for rainfall.
#' Columns should be named "T1" (below normal), "T2" (normal), and "T3"
#' (above normal).
#' @param fcstVarName A character string indicating the WSC variable to forecast
#' ("Onset" or "Cessation").
#' @param tercileMethod options are "quantiles" or "fixedValues"
#'
#' @return A data frame containing the tercile probabilities for the WSC
#' variable ("BelowNormal", "Normal", and "AboveNormal").
#'
#' @details
#' Uses QBR (Quantile Bin Resampling) to produce forecasts for
#' onset or cessation of the rainy season. It first categorizes historical WSC
#' simulations based on seasonal rainfall terciles and then resamples based on
#' given rainfall probabilities to generate ensemble forecasts.
#'
#' @references
#' * MacLeod D, Quichimbo EA, Michaelides K, Asfaw DT, Rosolem R, Cuthbert MO,
#' et al. (2023) Translating seasonal climate forecasts into water balance
#' forecasts for decision making. PLOS Clim 2(3): e0000138.
#' https://doi.org/10.1371/journal.pclm.0000138
#'
#' * van den Dool HM. A New Look at Weather Forecasting through Analogues.
#' Monthly Weather Review. 1989; 117(10):2230–2247.
#' https://doi.org/10.1175/1520-0493(1989)117%3C2230:ANLAWF%3E2.0.CO;2
#'
#' @seealso \code{\link{calcSeasCal}, \link{calcWatBal}}
#'
#' @importFrom stats quantile na.omit sd
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by summarize filter
#'
#' @examples
#' \donttest{
#' library(dplyr)
#'
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
#' seasCal.dF <- calcSeasCal(
#'   data = watBal, onsetWind.start, onsetWind.end,
#'   cessaWind.end, soilWHC
#' )
#'
#' ## Tercile Rainfall Probabilities of seasonal Forecast for OND, 2023:
#' rainTerc <- data.frame(T1 = 0.15, T2 = 0.10, T3 = 0.75)
#'
#' ## Summarize rainfall data for October to December:
#' seasRain <- AgroClimateData %>%
#'   filter(Month %in% c(10, 11, 12)) %>%
#'   group_by(Year) %>%
#'   summarize(sRain = sum(Rain))
#'
#' ## Start of the historical resampling year
#' hisYearStart <- 1991
#'
#' ## End of the historical resampling year
#' hisYearEnd <- 2022
#'
#' ## Historical WSC Simulations:
#' hisWSCvar <- seasCal.dF
#'
#' ## WSC variable to forecast:
#' fcstVarName <- "Onset"
#' tercileMethod <- "quantiles"
#'
#' SeasFcst.dF <- seasFcstQBR(
#'   hisYearStart, hisYearEnd, rainTerc,
#'   seasRain, hisWSCvar, fcstVarName,
#'   tercileMethod
#' )
#' }
#' @export
###############################################################################
# ***** function to forecast WSC

seasFcstQBR <- function(hisYearStart, hisYearEnd, rainTerc, seasRain, hisWSCvar,
                        fcstVarName, tercileMethod) {
  ensembleSize <- 100

  ## ***** Input validation ***** ##

  ## ***** Check if tercile probabilities sum to 1:
  if (sum(rainTerc) != 1) {
    stop("Tercile probabilities must sum to 1.")
  }

  ## ***** Check that fcstVarName is either 'Onset' or 'Cessation':
  if (!fcstVarName %in% c("Onset", "Cessation")) {
    stop("fcstVarName must be either 'Onset' or 'Cessation'.")
  }

  ## ***** Check that tercileMethod is either 'quantiles' or 'fixedValues':
  if (!tercileMethod %in% c("quantiles", "fixedValues")) {
    stop("tercileMethod must be either 'quantiles' or 'fixedValues'.")
  }


  ## ***** Check hisYearStart and hisYearEnd are numeric and valid:
  if (!is.numeric(hisYearStart) | !is.numeric(hisYearEnd) |
    hisYearStart > hisYearEnd) {
    stop("hisYearStart and hisYearEnd must be numeric, and hisYearStart
         should be less than or equal to hisYearEnd.")
  }

  ## ***** Check seasRain and hisWSCvar for missing/null values and structure:
  if (any(is.na(rainTerc)) | any(is.null(rainTerc))) {
    stop("rainTerc contains missing or NULL values.")
  }
  if (!"Year" %in% names(seasRain) | !"sRain" %in% names(seasRain)) {
    stop("seasRain must contain columns 'Year' and 'sRain'.")
  }
  if (!"Year" %in% names(hisWSCvar)) {
    stop("hisWSCvar must contain a 'Year' column.")
  }

  ## ***** Extract relevant WSC variable (onset or cessation)
  ## based on fcstVarName:
  wscVar <- paste0(fcstVarName, "Value")

  if (!(wscVar %in% names(hisWSCvar))) {
    stop(paste("The column", wscVar, "is not present in the historical
               WSC variable data frame."))
  }

  ## ***** Filter historical data ***** ##

  hisWSC <- hisWSCvar[, c("Year", wscVar)]
  hisWSC <- na.omit(hisWSC[hisWSC$Year >= hisYearStart &
    hisWSC$Year <= hisYearEnd, ])
  seasRain <- na.omit(seasRain[seasRain$Year >= hisYearStart &
    seasRain$Year <= hisYearEnd, ])

  seasRain <- seasRain[seasRain$Year %in% hisWSC$Year, ]
  hisWSC <- hisWSC[hisWSC$Year %in% seasRain$Year, ]

  if (nrow(hisWSC) != nrow(seasRain)) {
    stop("Mismatch between historical WSC and rainfall data after
         filtering by years.")
  }

  ## ***** Check for unrealistic or extreme values in hisWSCvar and seasRain:
  if (any(hisWSC[[wscVar]] < 1 | hisWSC[[wscVar]] > 366)) {
    warning("hisWSCvar contains unrealistic values (out of range 1-366).")
  }
  if (any(seasRain$sRain < 0)) {
    warning("seasRain contains negative rainfall values.")
  }

  ## ***** Determine terciles of seasonal rainfall totals ***** ##

  if (tercileMethod == "quantiles") {
    tercileBreaks <- quantile(seasRain$sRain, probs = c(0.33, 0.67))
  } else if (tercileMethod == "fixedValues") {
    tercileBreaks <- c(
      mean(seasRain$sRain) - 1.5 * sd(seasRain$sRain),
      mean(seasRain$sRain) + 1.5 * sd(seasRain$sRain)
    )
  } else {
    stop("Invalid tercile_method. Choose 'quantiles' or 'fixed_values'.")
  }

  ## ***** Categorize historical rainfall totals into terciles ***** ##

  tercileCategory <- cut(seasRain$sRain,
    breaks = c(-Inf, tercileBreaks, Inf),
    labels = c("T1", "T2", "T3")
  )

  ## ***** Split historical WSC data by rainfall tercile ***** ##

  wscBYtercile <- split(hisWSC[[wscVar]], tercileCategory)

  ## ***** Determine number of samples to draw from each tercile ***** ##

  ensembleCounts <- round(ensembleSize * rainTerc)

  ## ***** Resample from each tercile to create ensemble forecast ***** ##

  ensembleForecast <- unlist(lapply(names(ensembleCounts), function(terc) {
    if (!is.null(wscBYtercile[[terc]])) {
      sample(wscBYtercile[[terc]],
        size = ensembleCounts[[terc]],
        replace = TRUE
      )
    } else {
      warning(paste("No data available for tercile:", terc))
      return(rep(NA, ensembleCounts[[terc]]))
    }
  }))

  ## ***** Calculate tercile probabilities based on ensemble ***** ##

  wscTercileBreaks <- quantile(hisWSC[[wscVar]], probs = c(0.33, 0.67))

  belowNormalProb <- sum(ensembleForecast < wscTercileBreaks[1],
    na.rm = TRUE
  ) / ensembleSize
  aboveNormalProb <- sum(ensembleForecast > wscTercileBreaks[2],
    na.rm = TRUE
  ) / ensembleSize
  normalProb <- 1 - (belowNormalProb + aboveNormalProb)

  SeasFcst.dF <- data.frame(
    BelowNormal = belowNormalProb,
    Normal = normalProb,
    AboveNormal = aboveNormalProb
  )

  return(SeasFcst.dF)
}

###############################################################################
###############################################################################
#                >>>>>>>>>>   End of code   <<<<<<<<<<                        #
###############################################################################
###############################################################################
