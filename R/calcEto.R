#' @title Potential Evapotranspiration
#'
#' @description This function calculates Penman-Monteith, Priestley Taylor and Hargreaves-Samani Potential Evapotranspiration
#' using the method described by Allen et al, (1998)
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
#' @return The function generates a list containing the following components:
#'
#' \code{ET.Daily:} {Daily estimations of reference crop evapotranspiration (mm/day)}
#'
#' \code{Ra.Daily:} {Daily estimations of extraterristrial radiation (MJ/m2/day)}
#'
#' \code{Slope.Daily:} {Daily estimations of slope of vapour pressure curve (kPa/°C)}
#'
#' \code{ET.type:} {Type of the estimation obtained}
#'
#' @references Allen, R.G., L.S. Pereira, D. Raes, and M. Smith. 1998. ‘Crop evapotranspiration-Guidelines for Computing Crop Water requirements
# FAO Irrigation and Drainage Paper 56’. FAO, Rome 300: 6541.
#'
#' @seealso \code{\link{climateData}, \link{calcWatBal}, \link{calcSeasCal}}
#'
#' @examples
#'
#' calcEto(climateData)
#'
#'
#' @export

 calcEto <- function(data) {

   # ***** universal constants *****

   lambda = 2.45   # ***** latent heat of evaporation = 2.45 MJ.kg^-1 at 20 degree Celsius
#   lat_rad = udunits2::ud.convert(unique(data$Lat), "degree", "radians")   # ?????????????????????
   lat_rad = data$Lat * (pi/180)
   Gsc = 0.082  # ***** solar constant = 0.0820 MJ.m^-2.min^-1


   date.vec <- as.Date.character(paste0(data$Year, "-", data$Month, "-", data$Day))
   data$J <- strftime(as.POSIXlt(date.vec), "%j") # *****  julian day of the year
   Elev <- unique(data$Elev)
   ts = "daily"


   # ***** Check of specific data requirement

   if (is.null(data$Tmax)|is.null(data$Tmin)) {

     stop("Required data missing for 'Tmax.daily' and 'Tmin.daily'")

   }


   # ***** Calculating mean temperature

   Tavg <- (data$Tmax + data$Tmin) / 2

   P <- 101.3 * ((293 - 0.0065 * Elev) / 293)^5.26   # ***** atmospheric pressure
   delta <- 4098 * (0.6108 * exp((17.27 * Tavg)/(Tavg + 237.3))) / ((Tavg + 237.3)^2)  # ***** slope of vapour pressure curve
   gamma <- 0.00163 * P / lambda   # ***** psychrometric constant
   dr <- 1 + 0.033*cos(2*pi/365 * as.numeric(data$J))   # ***** inverse relative distance Earth-Sun
   delta2 <- 0.409 * sin(2*pi/365 * as.numeric(data$J) - 1.39) # ***** solar dedication
   ws <- acos(-tan(lat_rad) * tan(delta2))  # ***** sunset hour angle
   N <- 24/pi * ws # ***** calculating daily values

   # ***** extraterristrial radiation

   Ra <- (1440/pi) * dr * Gsc * (ws * sin(lat_rad) * sin(delta2) + cos(lat_rad) * cos(delta2) * sin(ws))

   # ***** empirical coefficient by Hargreaves and Samani (1985)

   C.HS <- 0.00185 * (data$Tmax - data$Tmin)^2 - 0.0433 * (data$Tmax - data$Tmin) + 0.4023

   # reference crop evapotranspiration by Hargreaves and Samani (1985)

   ET.HS.Daily <- 0.0135 * C.HS * Ra / lambda * (data$Tmax - data$Tmin)^0.5 * (Tavg + 17.8)

   ET.Daily <- ET.HS.Daily

   # Generate summary message for results

   ET.formulation <- "Hargreaves-Samani"
   ET.type <- "Reference Crop ET"

   message(ET.formulation, " ", ET.type)
   message("Evaporative surface: reference crop")

   results <- list(ET.Daily=ET.Daily,
                   Ra.Daily=Ra,
                   Slope.Daily = delta,
                   ET.formulation=ET.formulation,
                   ET.type=ET.type)


   message("Timestep: ", ts)
   message("Units: mm")
   message("Time duration: ", date.vec[1], " to ", date.vec[length(date.vec)])


 return(results)



  }


#####################################################################################################################################################
#####################################################################################################################################################
#####################################################################################################################################################

