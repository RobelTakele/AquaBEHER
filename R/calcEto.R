<<<<<<< HEAD
###############################################################################
###############################################################################
#
#     calcEto.R Potential Evapotranspiration
#
#     Copyright (C) 2024 Institute of Plant Sciences, Sant’Anna School of
#     Advanced Studies, Pisa, Italy
#     (https://www.santannapisa.it/en/institute/plant-sciences).
=======
#     calcEto.R Potential Evapotranspiration
#
#     Copyright (C) 2022 Center of Plant Sciences, Scuola Superiore Sant’Anna (http://www.capitalisegenetics.santannapisa.it)
>>>>>>> 5bf4ef90266654cfdc6ded681017190829d9c65a
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
<<<<<<< HEAD
###############################################################################
###############################################################################

#' @title Potential Evapotranspiration
#'
#' @description This function calculates Penman-Monteith, Priestley Taylor and
#' Hargreaves-Samani Potential Evapotranspiration using the method described by
#' Allen et al, (1998)
#'
#' @param data A dataframe containing the required weather variables with
#' the following columns:
#' \itemize{
#'   \item \code{Lat}: Latitude of the site in decimal degrees.
#'   \item \code{Lon}: Longitude of the site in decimal degrees.
#'   \item \code{Elev}: Elevation above sea level in meters.
#'   \item \code{Year}: Year of record "YYYY".
#'   \item \code{Month}: Month of record "MM".
#'   \item \code{Day}: Day of record "DD".
#'   \item \code{Tmax}: Daily maximum temperature at 2-m height in °C.
#'   \item \code{Tmin}: Daily minimum temperature at 2-m height in °C.
#'   \item \code{Rs}: Daily surface incoming solar radiation in MJ/m^2/day.
#'   \item \code{RH or RHmax and RHmin}: Daily relative humidity at 2-m height.
#'   \item \code{Tdew}: Daily dew point temperature at 2-m height in °C.
#'   \item \code{U2 or Uz}: Daily wind speed at 2-m or custom height (m/s).
#' }
#'
#' @param method The formulation used to compute Eto; default is \code{"PM"}
#' for Penman-Monteith, \code{"PT"} for Priestley-Taylor, and \code{"HS"} for
#' Hargreaves-Samani.
#'
#' @param crop Either \code{"short"} (default) for FAO-56 hypothetical short
#' grass or \code{"tall"} for ASCE-EWRI standard crop.
#' @param Zh Height of wind speed measurement in meters.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{ET.Daily}: Daily estimations of reference crop
#'   evapotranspiration (mm/day).
#'   \item \code{Ra.Daily}: Daily estimations of extraterrestrial radiation
#'   (MJ/m^2/day).
#'   \item \code{Slope.Daily}: Daily estimations of slope of vapor pressure
#'   curve (kPa/°C).
#'   \item \code{ET.type}: Type of the estimation obtained.
#' }
#'
#' @references
#' Allen, R.G., L.S. Pereira, D. Raes, and M. Smith. 1998. Crop
#' evapotranspiration-Guidelines for Computing Crop Water requirements FAO
#' Irrigation and Drainage Paper 56. FAO, Rome 300: 6541.
#'
#' Allen, R. G. 2005. The ASCE standardized reference evapotranspiration
#' equation. Amer Society of Civil Engineers.
#'
#' Guo, D., Westra, S., & Maier, H. (2016). An R package for modelling actual,
#' potential and reference evapotranspiration. Environmental
#' Modelling & Software, 78, 216-224. doi:10.1016/j.envsoft.2015.12.019.
#'
#' Hargreaves, G.H., & Samani, Z.A. (1985). Reference crop evapotranspiration
#' from ambient air temperature. American Society of Agricultural Engineers.
#'
#' Priestley, C., & Taylor, R. (1972). On the assessment of surface heat flux
#' and evaporation using large-scale parameters. Monthly Weather Review,
#' 100(2), 81-92.
#'
#' @details
#' \strong{Penman-Monteith:} If all variables of Tmax, Tmin, Rs, either U2 or
#' Uz, and either RHmax and RHmin or RH or Tdew are available and crop surface
#' (short or tall) is specified, the Penman-Monteith FAO56 formulation is used
#' (Allen et al. 1998).
#'
#' \strong{Priestley-Taylor:} If all variables of Tmax, Tmin, Rs, and either
#' RHmax and RHmin or RH or Tdew are available, the Priestley-Taylor
#' formulation is used (Priestley and Taylor, 1972).
#'
#' \strong{Hargreaves-Samani:} If only Tmax and Tmin are available, the
#' Hargreaves-Samani formulation is used for estimating reference crop
#' evapotranspiration (Hargreaves and Samani, 1985).
#'
#' @seealso \code{\link{climateData}}, \code{\link{calcWatBal}},
#' \code{\link{calcSeasCal}}
#'
#' @examples
#' # Load example data:
#' data(climateData)
#' calcEto(climateData, method = "HS")
#'
#' # Load example data:
#' data(AgroClimateData)
#' calcEto(AgroClimateData, method = "PM", crop = "short")
#'
#' @export
###############################################################################

calcEto <- function(data, method = "PM", crop = "short", Zh = NULL) {

  ## ***** Function to check missing data:

  checkMissing <- function(data.var, var) {
    missCount <- sum(is.na(data.var))
    if (missCount > 0) {
      warning(paste(
        "Warning: There are", missCount,
        "missing values in the data: ", var
      ))
    }
  }

  ## ***** Function to validate input parameters:

  validateParams <- function(method, crop) {
    if (!method %in% c("PM", "PT", "HS")) {
      stop("Invalid method specified. Choose either 'PM' (Penman-Monteith)
              or 'HS' (Hargreaves-Samani).")
    }
    if (!crop %in% c("short", "tall")) {
      stop("Invalid crop type. Choose either 'short' or 'tall'.")
    }
  }

###############################################################################

  ## *****  Validate parameters:

  validateParams(method, crop)

  ## ***** Check for missing data

  checkMissing(data.var = data$Tmax, var = "Tmax")
  checkMissing(data.var = data$Tmin, var = "Tmin")

  if (is.null(data$Tmax) || is.null(data$Tmin)) {
    stop("Required data missing for 'Tmax' and 'Tmin'")
  }

  ## ***** Check for unrealistic temperature values

  unrealTmax <- sum(data$Tmax > 60 | data$Tmax < -50, na.rm = TRUE)
  unrealTmin <- sum(data$Tmin > 50 | data$Tmin < -60, na.rm = TRUE)

  if (unrealTmax > 0) {
    warning(paste(
      "Warning: There are", unrealTmax,
      "unrealistic values in the maximum temperature data."
    ))
  }

  if (unrealTmin > 0) {
    warning(paste(
      "Warning: There are", unrealTmin,
      "unrealistic values in the minimum temperature data."
    ))
  }

  ## ***** Consistency: Check that the maximum temperature is always
  ## greater than to the minimum temperature.

  TempIncons <- sum(data$Tmax <= data$Tmin, na.rm = TRUE)

  if (TempIncons > 0) {
    warning(paste(
      "Warning: There are", TempIncons,
      "instances where the maximum temperature is",
      "less than the minimum temperature."
    ))
  }

  ## ***** Adjust inconsistent temperature data

  Inconsistent.Rows <- which(data$Tmin >= data$Tmax)

  ## >>>>> Adjust Tmin to be slightly less than Tmax

  data$Tmin[Inconsistent.Rows] <- data$Tmax[Inconsistent.Rows] - 0.1

  ## >>>>> Report the number of adjustments made:

  message(paste(
    "Adjusted", length(Inconsistent.Rows),
    "instances where Tmin was equal to or greater than Tmax."
  ))

###############################################################################
###############################################################################

  if (method == "HS") {
###############################################################################
    # ***** Hargreaves-Samani

    ## ***** universal constants *****

    lambda <- 2.45 # latent heat of evaporation (MJ.kg^-1) at 20°C
    Cp <- 1.013e-3 # specific heat at constant pressure (MJ.kg^-1.°C^-1)
    e <- 0.622 # ratio molecular weight of water vapor to dry air
    Gsc <- 0.082 # solar constant (MJ.m^-2.min^-1)
    lat.rad <- data$Lat * (pi / 180) # latitude in radians

    ## ***** Date and elevation data *****

    date.vec <- as.Date(paste0(data$Year, "-", data$Month, "-", data$Day))
    data$J <- as.numeric(format(date.vec, "%j")) ## Julian day of the year
    Elev <- unique(data$Elev)

    if (length(Elev) != 1) {
      stop("Elevation data should have only one unique value.")
    }

    ts <- "daily"
    message <- "yes"

    # ***** Calculating mean temperature (°C)

    Tavg <- (data$Tmax + data$Tmin) / 2

    # ***** Atmospheric pressure (kPa) as a function of altitude

    P <- 101.3 * ((293 - 0.0065 * Elev) / 293)^5.26

  ## Slope of saturation vapor pressure curve at air temperature Tavg (kPa/ °C)

    delta <- 4098 * (0.6108 *
      exp((17.27 * Tavg) / (Tavg + 237.3))) / ((Tavg + 237.3)^2)

    # ***** psychrometric constant (kPa/°C)

    gamma <- (Cp * P) / (lambda * e)

    # ***** Inverse relative distance Earth-Sun

    dr <- 1 + 0.033 * cos(2 * pi / 365 * as.numeric(data$J))

    # ***** Solar dedication (rad)

    SDc <- 0.409 * sin(2 * pi / 365 * as.numeric(data$J) - 1.39)

    # ***** sunset hour angle (rad)

    Ws <- acos(-tan(lat.rad) * tan(SDc))

    # ***** Daylight hours (hour)

    N <- 24 / pi * Ws

    # ***** Extraterrestrial radiation (MJ m-2 day-1)

    Ra <- (1440 / pi) * dr * Gsc * (Ws * sin(lat.rad) * sin(SDc) +
                                      cos(lat.rad) * cos(SDc) * sin(Ws))

    # ***** empirical coefficient by Hargreaves and Samani (1985)

    C.HS <- 0.00185 * (data$Tmax - data$Tmin)^2 - 0.0433 *
      (data$Tmax - data$Tmin) + 0.4023

    # reference crop evapotranspiration by Hargreaves and Samani (1985)

    ET.HS.Daily <- 0.0135 * C.HS * Ra / lambda * (data$Tmax - data$Tmin)^0.5 *
      (Tavg + 17.8)

    ET.Daily <- ET.HS.Daily

    # Generate summary message for results

    ET.formulation <- "Hargreaves-Samani"
    ET.type <- "Reference Crop ET"

    results <- list(
      ET.Daily = ET.Daily,
      Ra.Daily = Ra,
      Slope.Daily = delta,
      ET.formulation = ET.formulation,
      ET.type = ET.type
    )

    class(results) <- "PEToutList"
    return(results)

###############################################################################
###############################################################################

    } else if (method == "PT") {

    # ***** universal constants *****

    lambda <- 2.45 # Latent heat of evaporation (MJ.kg^-1 at 20°C)
    Cp <- 1.013e-3 # Specific heat at constant pressure (MJ.kg^-1.°C^-1)
    e <- 0.622 # Ratio of molecular weight of water vapor/dry air
    Sigma <- 4.903e-09 # Stefan-Boltzmann constant (MJ.K^-4.m^-2.day^-1)
    Gsc <- 0.082 # Solar constant (MJ.m^-2.min^-1)
    G <- 0 # Soil heat flux (negligible for daily time-step)
    alphaPT <- 1.26 # Priestley-Taylor coefficient
    alpha <- 0.23

    ## ***** Latitude in Radians *****

    lat.rad <- data$Lat * (pi / 180)

    ## ***** Date and Elevation Data *****

    date.vec <- as.Date(paste0(data$Year, "-", data$Month, "-", data$Day))
    data$J <- as.numeric(format(date.vec, "%j")) # Julian day of the year
    Elev <- unique(data$Elev)

    if (length(Elev) != 1) {
      stop("Elevation data should have only one unique value.")
    }

    ts <- "daily"
    message <- "yes"

    # ***** Data Validation *****

    reqVars <- c("Tmax", "Tmin", "Rs")
    misVars <- reqVars[!reqVars %in% names(data)]

    if (length(misVars) > 0) {
      stop(paste("Required data missing for:", paste(misVars,
        collapse = ", "
      )))
    }

    checkMissing(data.var = data$Rs, var = "Rs")

    if (is.na(as.numeric(alpha)) || alpha < 0 || alpha > 1) {
      stop("Please use a numeric value between 0 and 1 for the alpha
          (albedo of evaporative surface).")
    }

    ## ***** Calculating mean temperature (°C)

    Tavg <- (data$Tmax + data$Tmin) / 2

    ## ***** Calculate Actual Vapor Pressure (kPa)

    if (!is.null(data$Va) & !is.null(data$Vs)) {
      Ea <- data$Va
      Es <- data$Vs
    } else if (!is.null(data$RHmax) & !is.null(data$RHmin)) {
      EsTmax <- 0.6108 * exp(17.27 * data$Tmax / (data$Tmax + 237.3))
      EsTmin <- 0.6108 * exp(17.27 * data$Tmin / (data$Tmin + 237.3))
      Es <- (EsTmax + EsTmin) / 2
      Ea <- (EsTmin * data$RHmax / 100 + EsTmax * data$RHmin / 100) / 2
    } else if (!is.null(data$RH)) {
      EsTmax <- 0.6108 * exp(17.27 * data$Tmax / (data$Tmax + 237.3))
      EsTmin <- 0.6108 * exp(17.27 * data$Tmin / (data$Tmin + 237.3))
      Es <- (EsTmax + EsTmin) / 2
      Ea <- (data$RH / 100) * Es
    } else if (!is.null(data$Tdew)) {
      EsTmax <- 0.6108 * exp(17.27 * data$Tmax / (data$Tmax + 237.3))
      EsTmin <- 0.6108 * exp(17.27 * data$Tmin / (data$Tmin + 237.3))
      Es <- (EsTmax + EsTmin) / 2
      Ea <- 0.6108 * exp((17.27 * data$Tdew) / (data$Tdew + 237.3))
    } else {
      stop("Required data missing for vapor pressure calculation.")
    }

    ## ***** Atmospheric pressure (kPa) as a function of altitude

    P <- 101.3 * ((293 - 0.0065 * Elev) / 293)^5.26

    ## ***** Slope of Saturation Vapor Pressure Curve (kPa/°C) *****

    delta <- 4098 * (0.6108 * exp((17.27 * Tavg) / (Tavg + 237.3))) /
      ((Tavg + 237.3)^2)

    # ***** psychrometric constant (kPa/°C)

    gamma <- (Cp * P) / (lambda * e)

    # ***** Inverse relative distance Earth-Sun

    dr <- 1 + 0.033 * cos(2 * pi / 365 * data$J)

    # ***** Solar dedication (rad)

    SDc <- 0.409 * sin(2 * pi / 365 * as.numeric(data$J) - 1.39)

    # ***** sunset hour angle (rad)

    Ws <- acos(-tan(lat.rad) * tan(SDc))

    # ***** Daylight hours (hour)

    N <- 24 / pi * Ws

    # ***** Extraterrestrial radiation (MJ m-2 day-1)

    Ra <- (1440 / pi) * dr * Gsc * (Ws * sin(lat.rad) * sin(SDc) +
                                      cos(lat.rad) * cos(SDc) * sin(Ws))

    # ***** Clear-sky solar radiation (MJ m-2 day-1)

    Rso <- (0.75 + (2 * 10^-5) * Elev) * Ra

    Rs <- data$Rs # ***** solar or shortwave radiation (MJ m-2 day-1)

    ## ***** Net Outgoing Longwave Radiation (MJ.m^-2.day^-1)

    Rnl <- Sigma * (0.34 - 0.14 * sqrt(Ea)) *
      ((data$Tmax + 273.2)^4 + (data$Tmin + 273.2)^4) / 2 *
      (1.35 * Rs / Rso - 0.35)

    # ***** Net Incoming Shortwave Radiation (MJ.m^-2.day^-1) *****

    Rnsg <- (1 - alpha) * Rs # ***** For grass or user-defined albedo

    # ***** Net Radiation (MJ.m^-2.day^-1) *****

    Rng <- Rnsg - Rnl

    # ***** Potential Evapotranspiration (mm/day) *****

 E.PT.Daily <- alphaPT * (delta / (delta + gamma) * Rng / lambda - G / lambda)
 ET.Daily <- E.PT.Daily

    ## ***** Generate summary message for results

    ET.formulation <- "Priestley-Taylor"
    ET.type <- "Potential ET"

    Surface <- ifelse(alpha != 0.08, paste("user-defined, albedo =", alpha),
      paste("water, albedo =", alpha)
    )

    if (message == "yes") {
      message(paste(ET.formulation, ET.type))
      message("Evaporative surface: ", Surface)
      message("Timestep: daily")
      message("Units: mm")
      message("Time duration: ", date.vec[1], " to ",
              date.vec[length(date.vec)])
    }

    results <- list(
      ET.Daily = ET.Daily,
      Ra.Daily = Ra,
      Slope.Daily = delta,
      Ea.Daily = Ea,
      Es.Daily = Es,
      ET.formulation = ET.formulation,
      ET.type = ET.type
    )

    class(results) <- "PEToutList"
    return(results)

###############################################################################
###############################################################################

     } else if (method == "PM") {

    # ***** Universal Constants *****

    lambda <- 2.45 # Latent heat of evaporation (MJ.kg^-1 at 20°C)
    Cp <- 1.013e-3 # Specific heat at constant pressure (MJ.kg^-1.°C^-1)
    e <- 0.622 # Ratio of molecular weight of water vapour/dry air
    Sigma <- 4.903e-09 # Stefan-Boltzmann constant (MJ.K^-4.m^-2.day^-1)
    Gsc <- 0.082 # Solar constant (MJ.m^-2.min^-1)
    G <- 0 # Soil heat flux (MJ.m^-2.day^-1, negligible for daily)

    # Convert latitude to radians

    lat.rad <- data$Lat * (pi / 180)

    # Convert date to Julian day

    date.vec <- as.Date(paste(data$Year, data$Month, data$Day, sep = "-"))
    data$J <- as.numeric(format(date.vec, "%j"))

    Elev <- unique(data$Elev)
    if (length(Elev) != 1) {
      stop("Elevation data should have only one unique value.")
    }
    ts <- "daily"
    message <- "yes"

    # ***** Input Validation *****

    if (is.null(data$Rs)) { # solar radiation data is required
      stop("Required data missing for 'Rs' (solar radiation)")
    }

    if (is.null(data$U2) & is.null(data$Uz)) {
      stop("Required data missing for 'Uz' or 'U2' (wind speed)")
    }

    if (is.null(data$Va) | is.null(data$Vs)) {
      if (is.null(data$RHmax) | is.null(data$RHmin)) {
        if (is.null(data$RH)) {
          if (is.null(data$Tdew)) {
            stop("Required data missing: need either 'Tdew', or 'Va' and 'Vs',
            or 'RHmax' and 'RHmin', r 'RH'")
          }
        }
      }
    }

    # ***** check user-input crop type and specify Alberto

    if (!crop %in% c("short", "tall")) {
     stop("Please enter 'short' or 'tall' for the desired reference crop type")
    } else {
      alpha <- 0.23
      z0 <- ifelse(crop == "short", 0.02, 0.1)
    }

    # ***** Calculating mean temperature (°C)

    Tavg <- (data$Tmax + data$Tmin) / 2

    # ***** Calculating Actual Vapor Pressure (kPa) *****

    if (!is.null(data$Va) & !is.null(data$Vs)) {
      Ea <- data$Va
      Es <- data$Vs
    } else if (!is.null(data$RHmax) & !is.null(data$RHmin)) {
      EsTmax <- 0.6108 * exp(17.27 * data$Tmax / (data$Tmax + 237.3))
      EsTmin <- 0.6108 * exp(17.27 * data$Tmin / (data$Tmin + 237.3))
      Es <- (EsTmax + EsTmin) / 2
      Ea <- (EsTmin * data$RHmax / 100 + EsTmax * data$RHmin / 100) / 2
    } else if (!is.null(data$RH)) {
      EsTmax <- 0.6108 * exp(17.27 * data$Tmax / (data$Tmax + 237.3))
      EsTmin <- 0.6108 * exp(17.27 * data$Tmin / (data$Tmin + 237.3))
      Es <- (EsTmax + EsTmin) / 2
      Ea <- (data$RH / 100) * Es
    } else if (!is.null(data$Tdew)) {
      EsTmax <- 0.6108 * exp(17.27 * data$Tmax / (data$Tmax + 237.3))
      EsTmin <- 0.6108 * exp(17.27 * data$Tmin / (data$Tmin + 237.3))
      Es <- (EsTmax + EsTmin) / 2
      Ea <- 0.6108 * exp(17.27 * data$Tdew / (data$Tdew + 237.3))
    }

    # ***** Atmospheric pressure (kPa) as a function of altitude

    P <- 101.3 * ((293 - 0.0065 * Elev) / 293)^5.26

    # ***** Slope of Saturation Vapor Pressure Curve (kPa/°C) *****

    delta <- 4098 * (0.6108 * exp((17.27 * Tavg) / (Tavg + 237.3))) /
      ((Tavg + 237.3)^2)

    # ***** psychrometric constant (kPa/°C)

    gamma <- (Cp * P) / (lambda * e)

    # ***** Inverse relative distance Earth-Sun

    dr <- 1 + 0.033 * cos(2 * pi / 365 * as.numeric(data$J))

    # ***** Solar dedication (rad)

    SDc <- 0.409 * sin(2 * pi / 365 * as.numeric(data$J) - 1.39)

    # ***** sunset hour angle (rad)

    Ws <- acos(-tan(lat.rad) * tan(SDc))

    # ***** Daylight hours (hour)

    N <- 24 / pi * Ws

    # ***** Extraterrestrial radiation (MJ m-2 day-1)

    Ra <- (1440 / pi) * dr * Gsc * (Ws * sin(lat.rad) * sin(SDc) +
                                      cos(lat.rad) * cos(SDc) * sin(Ws))

    # ***** Clear-sky solar radiation (MJ m-2 day-1)

    Rso <- (0.75 + (2 * 10^-5) * Elev) * Ra

    Rs <- data$Rs # ***** solar or shortwave radiation (MJ m-2 day-1)

    # ***** estimated net outgoing longwave radiation (MJ m-2 day-1)

    Rnl <- Sigma * (0.34 - 0.14 * sqrt(Ea)) *
      ((data$Tmax + 273.2)^4 + (data$Tmin + 273.2)^4) / 2 *
      (1.35 * Rs / Rso - 0.35)

    # net incoming shortwave radiation (MJ m-2 day-1)

    Rnsg <- (1 - alpha) * Rs # ***** for grass

    # ***** net radiation

    Rng <- Rnsg - Rnl

    # ***** Wind Speed Adjustment *****

    if (is.null(data$U2)) {
      U2 <- data$Uz * 4.87 / log(67.8 * Zh - 5.42)
    } else {
      U2 <- data$U2
    }

    # ***** Evapotranspiration Calculation *****

    if (crop == "short") {
      ET.RC.Daily <- (0.408 * delta * (Rng - G) + gamma * 900 * U2 *
                        (Es - Ea) / (Tavg + 273)) / (delta + gamma *
                                                       (1 + 0.34 * U2))

      ET.formulation <- "Penman-Monteith FAO56"
      ET.type <- "Reference Crop ET"
      Surface <- paste(
        "FAO-56 hypothetical short grass, albedo =", alpha,
        "; surface resistance = 70 sm^-1; crop height = 0.12 m;",
        "roughness height =", z0, "m"
      )
    } else {
      ET.RC.Daily <- (0.408 * delta * (Rng - G) + gamma * 1600 * U2 *
                        (Es - Ea) / (Tavg + 273)) / (delta + gamma *
                                                       (1 + 0.38 * U2))
      ET.formulation <- "Penman-Monteith ASCE-EWRI Standardised"
      ET.type <- "Reference Crop ET"
      Surface <- paste(
        "ASCE-EWRI hypothetical tall grass, albedo =", alpha,
        "; surface resistance = 45 sm^-1; crop height = 0.50 m;",
        "roughness height =", z0, "m"
      )
    }

    # Generate summary message for results

    if (message == "yes") {
      message(ET.formulation, " ", ET.type)
      message("Evaporative surface: ", Surface)
      message("Timestep: daily")
      message("Units: mm")
      message("Time duration: ", date.vec[1], " to ",
              date.vec[length(date.vec)])
    }

    results <- list(
      ET.Daily = ET.RC.Daily,
      Ra.Daily = Ra,
      Slope.Daily = delta,
      Ea.Daily = Ea,
      Es.Daily = Es,
      ET.formulation = ET.formulation,
      ET.type = ET.type
    )

    class(results) <- "PEToutList"
    return(results)
  }

###############################################################################
}

###############################################################################
###############################################################################
#                >>>>>>>>>>   End of code   <<<<<<<<<<                        #
###############################################################################
###############################################################################
=======
##################################################################################################################################
#' @title Potential Evapotranspiration
#'
#' @description This function calculates Penman-Monteith, Priestley Taylor and Hargreaves-Samani Potential Evapotranspiration
#' using the method described by Allen et al, (1998)
#'
#' @param data a dataframe containing the required climate variables: columns must contain the following parameters:
#'
#' \verb{      }\emph{\strong{\code{Lat:}} latitude of the site in decimal degrees.}
#'
#' \verb{      }\emph{\strong{\code{Lon:}} longitude of the site in decimal degrees.}
#'
#' \verb{      }\emph{\strong{\code{Elev:}} elevation above sea level in (meters).}
#'
#' \verb{      }\emph{\strong{\code{Year:}} year of record "YYYY".}
#'
#' \verb{      }\emph{\strong{\code{Month:}} month of record "MM".}
#'
#' \verb{      }\emph{\strong{\code{Day:}} day of record "DD".}
#'
#' \verb{      }\emph{\strong{\code{Tmax:}} daily maximum temperature at 2-m height in (°C).}
#'
#' \verb{      }\emph{\strong{\code{Tmin:}} daily minimum temperature at 2-m height in (°C).}
#'
#' \verb{      }\emph{\strong{\code{Rs:}} daily surface incoming solar radiation in (MJ/m^2/day).}
#'
#' \verb{      }\emph{\strong{\code{RH or RHmax and RHmin:}} daily relative humidity at 2-m height.}
#'
#' \verb{      }\emph{\strong{\code{Tdew:}} daily dew point temperature at 2-m height in (°C).}
#'
#' \verb{      }\emph{\strong{\code{U2 or Uz:}} daily wind speed at 2-m or Z-m(custom) height (m/s).}
#'
#' \verb{    }
#' @param method \verb{  }the formulation used to compute Eto; default is \emph{method = "PM"} gives the the Penman-Monteith
#' formulation; \emph{method = "PT"} gives \verb{  }the Priestley-Taylor formulation and \emph{method = "HS"} gives the Hargreaves
#' Samani formulation.
#'
#' @param crop \verb{  }either \emph{crop = "short"} (default) or \emph{crop = "tall"}; short indicates that the method for FAO-56
#' hypothetical short grass will be applied \verb{  }(Allen et al.1998); tall indicates that the method for ASCE-EWRI standard
#' crop will be applied (ASCE, 2005).
#'
#' @param Zh \verb{  }height of wind speed measurement in meters,
#'
#' @return The function generates a list containing the following objects:
#'
#' \code{ET.Daily:} {daily estimations of reference crop evapotranspiration (mm/day)}
#'
#' \code{Ra.Daily:} {daily estimations of extraterristrial radiation (MJ/m2/day)}
#'
#' \code{Slope.Daily:} {daily estimations of slope of vapour pressure curve (kPa/°C)}
#'
#' \code{ET.type:} {type of the estimation obtained}
#'
#' @references Allen, R.G., L.S. Pereira, D. Raes, and M. Smith. 1998. ‘Crop evapotranspiration-Guidelines for Computing Crop
#' Water requirements FAO Irrigation and Drainage Paper 56’. FAO, Rome 300: 6541.
#'
#' Allen, R. G. 2005. The ASCE standardized reference evapotranspiration equation. Amer Society of Civil Engineers.
#'
#' Guo, Danlu & Westra, Seth & Maier, Holger. (2016). An R package for modelling actual, potential and reference
#' evapotranspiration. Environmental Modelling & Software. 78. 216-224. 10.1016/j.envsoft.2015.12.019.
#'
#' Hargreaves, G.H.Samani, Z.A. 1985, Reference crop evapotranspiration from ambient air temperature. American Society of
#' Agricultural Engineers.
#'
#' Priestley, C. & Taylor, R. 1972, On the assessment of surface heat flux and evaporation using large-scale parameters'. Monthly
#' Weather Review, vol. 100, no. 2, pp. 81-92.
#'
#' @details
#'
#'  \strong{Penman-Monteith:}
#'  If all variables of Tmax, Tmin, Rs, either U2 or Uz, and either RHmax and RHmin or RH or Tdew are available and crop surface
#'  (short or tall) is specified in argument the Penman-Monteith FAO56 formulation is used (Allen et al.1998).
#'
#'   \strong{Priestley-Taylor:}
#'   If all variables of Tmax, Tmin, Rs and either RHmax and RHmin or RH or Tdew are available the Priestley-Taylor formulation
#'   is used (Priestley and Taylor, 1972).
#'
#'   \strong{Hargreaves-Samani:}
#'   If only Tmax and Tmin are available, the Hargreaves-Samani formulation is used or estimating reference crop
#'   evapotranspiration (Hargreaves and.Samani, 1985).
#'
#' @seealso \code{\link{climateData}, \link{calcWatBal}, \link{calcSeasCal}}
#'
#' @examples
#' # load example data:
#' data(climateData)
#'
#' calcEto(climateData, method = "HS")
#'
#' # load example data:
#' data(AgroClimateData)
#'
#' calcEto(AgroClimateData, method = "PM", crop = "short")
#'
#' @export
##################################################################################################################################

 calcEto  <- function(data, method = "PM", crop = "short", Zh = NULL) {

 if (method == "HS") {

##################################################################################################################################
 # ***** Hargreaves-Samani

        # ***** universal constants *****

   lambda = 2.45       # ***** latent heat of evaporation = 2.45 MJ.kg^-1 at 20 degree Celsius
   Cp =1.013 * 10^-3   # ***** specific heat at constant pressure = MJ kg^-1 °C^-1
   e = 0.622           # ***** ratio molecular weight of water vapour/dry air
   lat.rad = data$Lat * (pi/180)
   Gsc = 0.082  # ***** solar constant = 0.0820 MJ.m^-2.min^-1
    date.vec <- as.Date.character(paste0(data$Year, "-", data$Month, "-", data$Day))
    data$J <- strftime(as.POSIXlt(date.vec), "%j") # *****  julian day of the year
    Elev <- unique(data$Elev)

   ts="daily"
   message = "yes"

   # ***** Check of specific data requirement

   if (is.null(data$Tmax)|is.null(data$Tmin)) {

     stop("Required data missing for 'Tmax' and 'Tmin'")

   }


   # ***** Calculating mean temperature (°C)

   Tavg <- (data$Tmax + data$Tmin) / 2

   # ***** Atmospheric pressure (kPa) as a function of altitude

   P <- 101.3 * ((293 - 0.0065 * Elev) / 293)^5.26

   # ***** Slope of saturation vapor pressure curve at air temperature Tavg (kPa/ °C)

   delta <- 4098 * (0.6108 * exp((17.27 * Tavg)/(Tavg + 237.3))) / ((Tavg + 237.3)^2)

   # ***** psychrometric constant (kPa/°C)

   gamma <- (Cp * P) / (lambda * e)

   # ***** Inverse relative distance Earth-Sun

   dr <- 1 + 0.033*cos(2*pi/365 * as.numeric(data$J))

   # ***** Solar dedication (rad)

   SDc  <- 0.409 * sin(2*pi/365 * as.numeric(data$J) - 1.39)

   # ***** sunset hour angle (rad)

   Ws <- acos(-tan(lat.rad) * tan(SDc))

   # ***** Daylight hours (hour)

   N <- 24/pi * Ws

   # ***** Extraterrestrial radiation (MJ m-2 day-1)

   Ra <- (1440/pi) * dr * Gsc * (Ws * sin(lat.rad) * sin(SDc) + cos(lat.rad) * cos(SDc) * sin(Ws))

   # ***** empirical coefficient by Hargreaves and Samani (1985)

   C.HS <- 0.00185 * (data$Tmax - data$Tmin)^2 - 0.0433 * (data$Tmax - data$Tmin) + 0.4023

   # reference crop evapotranspiration by Hargreaves and Samani (1985)

   ET.HS.Daily <- 0.0135 * C.HS * Ra / lambda * (data$Tmax - data$Tmin)^0.5 * (Tavg + 17.8)

   ET.Daily <- ET.HS.Daily

   # Generate summary message for results

   ET.formulation <- "Hargreaves-Samani"
   ET.type <- "Reference Crop ET"

   # message(ET.formulation, " ", ET.type)
   # message("Evaporative surface: reference crop")

   results <- list(ET.Daily = ET.Daily,
                   Ra.Daily = Ra,
                   Slope.Daily = delta,
                   ET.formulation = ET.formulation,
                   ET.type = ET.type)


   # message("Timestep: ", ts)
   # message("Units: mm")
   # message("Time duration: ", date.vec[1], " to ", date.vec[length(date.vec)])


 return(results)

##################################################################################################################################

 } else if (method == "PT") {


   alpha=0.23


   # ***** universal constants *****

   lambda = 2.45       # ***** latent heat of evaporation = 2.45 MJ.kg^-1 at 20 degree Celsius
   Cp =1.013 * 10^-3   # ***** specific heat at constant pressure = MJ kg^-1 °C^-1
   e = 0.622           # ***** ratio molecular weight of water vapour/dry air
   Sigma = 4.903e-09  # Stefan-Boltzmann constant = 4.903*10^-9 MJ.K^-4.m^-2.day^-1
   lat.rad = data$Lat * (pi/180)
   Gsc = 0.082  # ***** solar constant = 0.0820 MJ.m^-2.min^-1
   G = 0  # soil heat flux negligible for daily time-step = 0 (Allen et al., 1998, page 68)
   alphaPT = 1.26  # Priestley-Taylor coefficient
   date.vec <- as.Date.character(paste0(data$Year, "-", data$Month, "-", data$Day))
   data$J <- strftime(as.POSIXlt(date.vec), "%j") # *****  julian day of the year
   Elev <- unique(data$Elev)

   ts="daily"
   message = "yes"



   # ***** Check of specific data requirement

   if (is.null(data$Tmax)|is.null(data$Tmin)) {
     stop("Required data missing for 'Tmax' and 'Tmin'")
   }

   if (is.null(data$Va)|is.null(data$Vs)) {
     if (is.null(data$RHmax)|is.null(data$RHmin)) {
       if (is.null(data$RH)) {
         if (is.null(data$Tdew)) {
           stop("Required data missing: need either 'Tdew', or 'Va' and 'Vs', or 'RHmax' and 'RHmin', r 'RH'")
         }
       }
     }
   }

   if (is.null(data$Rs)) { # solar radiation data is required
     stop("Required data missing for 'Rs'")
   }

   # check user-input albedo
   if (is.na(as.numeric(alpha))) {
     stop("Please use a numeric value for the alpha (albedo of evaporative surface)")
   }
   if (!is.na(as.numeric(alpha))) {
     if (as.numeric(alpha) < 0 | as.numeric(alpha) > 1) {
       stop("Please use a value between 0 and 1 for the alpha (albedo of evaporative surface)")
     }
   }

   # ***** Calculating mean temperature (°C)

   Tavg <- (data$Tmax + data$Tmin) / 2


   # ***** Calculating mean temperature (°C)

   Tavg <- (data$Tmax + data$Tmin) / 2

   # ***** calculating actual vapor pressure (kPa)

   if (!is.null(data$Va) & !is.null(data$Vs)) {

     Ea <- data$Va
     Es <- data$Vs

   } else if (!is.null(data$RHmax) & !is.null(data$RHmin)) {

     # Saturation vapor pressure from temperature (kPa)

     EsTmax <- 0.6108 * exp(17.27 * data$Tmax / (data$Tmax + 237.3))
     EsTmin <- 0.6108 * exp(17.27 * data$Tmin / (data$Tmin + 237.3))

     Es <- (EsTmax + EsTmin)/2

     # Actual vapor pressure derived from max and min relative humidity data

     Ea  <- (EsTmin * data$RHmax/100 + EsTmax * data$RHmin/100)/2

   }  else if (!is.null(data$RH)) {

     # Saturation vapor pressure from temperature (kPa)

     EsTmax <- 0.6108 * exp(17.27 * data$Tmax / (data$Tmax + 237.3))
     EsTmin <- 0.6108 * exp(17.27 * data$Tmin / (data$Tmin + 237.3))

     Es <- (EsTmax + EsTmin)/2

     # Actual vapor pressure derived from max and min relative humidity data

     Ea  <- (data$RH/100) * Es

   } else if (!is.null(data$Tdew)) {

     # Saturation vapor pressure from temperature (kPa)

     EsTmax <- 0.6108 * exp(17.27 * data$Tmax / (data$Tmax + 237.3))
     EsTmin <- 0.6108 * exp(17.27 * data$Tmin / (data$Tmin + 237.3))

     Es <- (EsTmax + EsTmin)/2

     # Actual vapour pressure derived from dewpoint temperature

     Ea  <- 0.6108 * exp((17.27 * data$Tdew)/(data$Tdew + 237.3))

   }


   # ***** Atmospheric pressure (kPa) as a function of altitude

   P <- 101.3 * ((293 - 0.0065 * Elev) / 293)^5.26

   # ***** Slope of saturation vapor pressure curve at air temperature Tavg (kPa/ °C)

   delta <- 4098 * (0.6108 * exp((17.27 * Tavg)/(Tavg + 237.3))) / ((Tavg + 237.3)^2)

   # ***** psychrometric constant (kPa/°C)

   gamma <- (Cp * P) / (lambda * e)

   # ***** Inverse relative distance Earth-Sun

   dr <- 1 + 0.033*cos(2*pi/365 * as.numeric(data$J))

   # ***** Solar dedication (rad)

   SDc  <- 0.409 * sin(2*pi/365 * as.numeric(data$J) - 1.39)

   # ***** sunset hour angle (rad)

   Ws <- acos(-tan(lat.rad) * tan(SDc))

   # ***** Daylight hours (hour)

   N <- 24/pi * Ws


   # ***** Extraterrestrial radiation (MJ m-2 day-1)

   Ra <- (1440/pi) * dr * Gsc * (Ws * sin(lat.rad) * sin(SDc) + cos(lat.rad) * cos(SDc) * sin(Ws))


   # ***** Clear-sky solar radiation (MJ m-2 day-1)

   Rso <- (0.75 + (2*10^-5)*Elev) * Ra

   Rs <- data$Rs # ***** solar or shortwave radiation (MJ m-2 day-1)

   # ***** estimated net outgoing longwave radiation (MJ m-2 day-1)

   Rnl <- Sigma * (0.34 - 0.14 * sqrt(Ea)) * ((data$Tmax+273.2)^4 + (data$Tmin+273.2)^4)/2  * (1.35 * Rs / Rso - 0.35)

   # net incoming shortwave radiation (MJ m-2 day-1)

   Rnsg <- (1 - alpha) * Rs       # ***** for grass

   # ***** net radiation

   Rng <- Rnsg - Rnl

   # well-watered crop evapotranspiration in a semi-arid and windy location

   E.PT.Daily <- alphaPT * (delta/(delta + gamma) * Rng / lambda - G / lambda)

   ET.Daily <- E.PT.Daily


   # Generate summary message for results

   ET.formulation <- "Priestley-Taylor"
   ET.type <- "Potential ET"

   if (alpha != 0.08) {
     Surface <- paste("user-defined, albedo =", alpha)
   } else if (alpha == 0.08) {
     Surface <- paste("water, albedo =", alpha)
   }

   # message(ET.formulation, " ", ET.type)
   # message("Evaporative surface: ", Surface)
   #
   # message("Timestep: ", ts)
   # message("Units: mm")
   # message("Time duration: ", date.vec[1], " to ", date.vec[length(date.vec)])

   results <- list(ET.Daily = ET.Daily,
                   Ra.Daily = Ra,
                   Slope.Daily = delta,
                   Ea.Daily = Ea,
                   Es.Daily = Es,
                   ET.formulation = ET.formulation,
                   ET.type = ET.type)

   return(results)


##################################################################################################################################

 } else if (method == "PM") {


   # ***** universal constants *****

   lambda = 2.45       # ***** latent heat of evaporation = 2.45 MJ.kg^-1 at 20 degree Celsius
   Cp =1.013 * 10^-3   # ***** specific heat at constant pressure = MJ kg^-1 °C^-1
   e = 0.622           # ***** ratio molecular weight of water vapour/dry air
   Sigma = 4.903e-09  # Stefan-Boltzmann constant = 4.903*10^-9 MJ.K^-4.m^-2.day^-1
   lat.rad = data$Lat * (pi/180)
   Gsc = 0.082  # ***** solar constant = 0.0820 MJ.m^-2.min^-1
   G = 0  # soil heat flux negligible for daily time-step = 0 (Allen et al., 1998, page 68)
   date.vec <- as.Date.character(paste0(data$Year, "-", data$Month, "-", data$Day))
   data$J <- strftime(as.POSIXlt(date.vec), "%j") # *****  julian day of the year
   Elev <- unique(data$Elev)

   ts="daily"
   message = "yes"

   # ***** Check of specific data requirement

   if (is.null(data$Tmax)|is.null(data$Tmin)) {
     stop("Required data missing for 'Tmax' and 'Tmin'")
   }

   if (is.null(data$Rs)) { # solar radiation data is required
     stop("Required data missing for 'Rs'")
   }

   if (is.null(data$U2) & is.null(data$Uz)) {
     stop("Required data missing for 'Uz' or 'U2'")
   }


   if (is.null(data$Va)|is.null(data$Vs)) {
     if (is.null(data$RHmax)|is.null(data$RHmin)) {
       if (is.null(data$RH)) {
         if (is.null(data$Tdew)) {
         stop("Required data missing: need either 'Tdew', or 'Va' and 'Vs', or 'RHmax' and 'RHmin', r 'RH'")
         }
       }
     }
   }

   # ***** check user-input crop type and specify albedo

   if (crop != "short" & crop != "tall") {

     stop("Please enter 'short' or 'tall' for the desired reference crop type")

   } else {

     alpha <- 0.23 # albedo for both short and tall crop

     if (crop == "short") {

       z0 <- 0.02 # roughness height for short grass

     } else {

       z0 <- 0.1 # roughness height for tall grass

     }
   }

   # ***** Calculating mean temperature (°C)

   Tavg <- (data$Tmax + data$Tmin) / 2

   # ***** calculating actual vapor pressure (kPa)

   if (!is.null(data$Va) & !is.null(data$Vs)) {

     Ea <- data$Va
     Es <- data$Vs

   } else if (!is.null(data$RHmax) & !is.null(data$RHmin)) {

     # Saturation vapor pressure from temperature (kPa)

     EsTmax <- 0.6108 * exp(17.27 * data$Tmax / (data$Tmax + 237.3))
     EsTmin <- 0.6108 * exp(17.27 * data$Tmin / (data$Tmin + 237.3))

     Es <- (EsTmax + EsTmin)/2

     # Actual vapor pressure derived from max and min relative humidity data

      Ea  <- (EsTmin * data$RHmax/100 + EsTmax * data$RHmin/100)/2

    }  else if (!is.null(data$RH)) {

     # Saturation vapor pressure from temperature (kPa)

     EsTmax <- 0.6108 * exp(17.27 * data$Tmax / (data$Tmax + 237.3))
     EsTmin <- 0.6108 * exp(17.27 * data$Tmin / (data$Tmin + 237.3))

     Es <- (EsTmax + EsTmin)/2

     # Actual vapor pressure derived from max and min relative humidity data

     Ea  <- (data$RH/100) * Es

    } else if (!is.null(data$Tdew)) {

    # Saturation vapor pressure from temperature (kPa)

    EsTmax <- 0.6108 * exp(17.27 * data$Tmax / (data$Tmax + 237.3))
    EsTmin <- 0.6108 * exp(17.27 * data$Tmin / (data$Tmin + 237.3))

    Es <- (EsTmax + EsTmin)/2

    # Actual vapour pressure derived from dewpoint temperature

    Ea  <- 0.6108 * exp((17.27 * data$Tdew)/(data$Tdew + 237.3))

  }


   # ***** Atmospheric pressure (kPa) as a function of altitude

   P <- 101.3 * ((293 - 0.0065 * Elev) / 293)^5.26

   # ***** Slope of saturation vapor pressure curve at air temperature Tavg (kPa/ °C)

   delta <- 4098 * (0.6108 * exp((17.27 * Tavg)/(Tavg + 237.3))) / ((Tavg + 237.3)^2)

   # ***** psychrometric constant (kPa/°C)

   gamma <- (Cp * P) / (lambda * e)

   # ***** Inverse relative distance Earth-Sun

   dr <- 1 + 0.033*cos(2*pi/365 * as.numeric(data$J))

   # ***** Solar dedication (rad)

   SDc  <- 0.409 * sin(2*pi/365 * as.numeric(data$J) - 1.39)

   # ***** sunset hour angle (rad)

   Ws <- acos(-tan(lat.rad) * tan(SDc))

   # ***** Daylight hours (hour)

   N <- 24/pi * Ws


   # ***** Extraterrestrial radiation (MJ m-2 day-1)

   Ra <- (1440/pi) * dr * Gsc * (Ws * sin(lat.rad) * sin(SDc) + cos(lat.rad) * cos(SDc) * sin(Ws))

   # ***** Clear-sky solar radiation (MJ m-2 day-1)

   Rso <- (0.75 + (2*10^-5)*Elev) * Ra

   Rs <- data$Rs # ***** solar or shortwave radiation (MJ m-2 day-1)

   # ***** estimated net outgoing longwave radiation (MJ m-2 day-1)

   Rnl <- Sigma * (0.34 - 0.14 * sqrt(Ea)) * ((data$Tmax+273.2)^4 + (data$Tmin+273.2)^4)/2  * (1.35 * Rs / Rso - 0.35)

   # net incoming shortwave radiation (MJ m-2 day-1)

   Rnsg <- (1 - alpha) * Rs       # ***** for grass

   # ***** net radiation

   Rng <- Rnsg - Rnl

   # ***** wind speed

   if (is.null(data$U2)) {

     U2 <- data$Uz * 4.87 / log(67.8*Zh - 5.42)
   } else {
     U2 <- data$U2
   }

   if (crop == "short") {

     # FAO-56 reference crop evapotranspiration from short grass

     ET.RC.Daily <- (0.408 * delta * (Rng - G) + gamma * 900 * U2 * (Es - Ea)/(Tavg + 273)) / (delta + gamma * (1 + 0.34*U2))

   } else {

     # ASCE-EWRI standardised Penman-Monteith for long grass

     ET.RC.Daily <- (0.408 * delta * (Rng - G) + gamma * 1600 * U2 * (Es - Ea)/(Tavg + 273)) / (delta + gamma * (1 + 0.38*U2))
   }

   ET.Daily <- ET.RC.Daily



   # Generate summary message for results

   if (crop == "short") {

     r_s <- 70 # will not be used for calculation - just informative
     CH <- 0.12 # will not be used for calculation - just informative

     ET.formulation <- "Penman-Monteith FAO56"
     ET.type <- "Reference Crop ET"
     Surface <- paste("FAO-56 hypothetical short grass, albedo =",
                      alpha, "; surface resistance =", r_s, "sm^-1; crop height =", CH, " m; roughness height =", z0, "m")
   } else {

     r_s <- 45 # will not be used for calculation - just informative
     CH <- 0.50 # will not be used for calculation - just informative

     ET.formulation <- "Penman-Monteith ASCE-EWRI Standardised"
     ET.type <- "Reference Crop ET"
     Surface <- paste("ASCE-EWRI hypothetical tall grass, albedo =",
                      alpha, "; surface resistance =", r_s, "sm^-1; crop height =", CH, " m; roughness height =", z0, "m")
   }

   results <- list(ET.Daily = ET.Daily,
                   Ra.Daily = Ra,
                   Slope.Daily = delta,
                   Ea.Daily = Ea,
                   Es.Daily = Es,
                   ET.formulation = ET.formulation,
                   ET.type = ET.type)

   # message(ET.formulation, " ", ET.type)
   # message("Evaporative surface: ", Surface)
   #
   # message("Timestep: ", ts)
   # message("Units: mm")
   # message("Time duration: ", date.vec[1], " to ", date.vec[length(date.vec)])


   return(results)


   }

##################################################################################################################################

}

##################################################################################################################################
##################################################################################################################################
##################################################################################################################################

>>>>>>> 5bf4ef90266654cfdc6ded681017190829d9c65a
