#' @title Raw Climate Data Required for Calculating Evapotranspiration
#'
#' @description A example data set contains the raw climate data including the variables required for calculating evapotranspiration in function calcEto
#' over the period between 1/1/1980 and 12/31/1984 at Nampula station in Mozambique.
#'
#' @docType data
#'
#' @usage data(climateData)
#'
#' @format A data frame with 1827 rows and 11 variables:
#' \describe{
#'   \item{\code{Station_ID}}{weather station ID}
#'   \item{\code{Station_Name}}{weather station name}
#'   \item{\code{Lat}}{latitude of the site in decimal degrees}
#'   \item{\code{Lon}}{longitude of the site in decimal degrees}
#'   \item{\code{Elev}}{elevation above sea level in (m)}
#'   \item{\code{Year}}{year of record "yyyy"}
#'   \item{\code{Month}}{month of record "mm"}
#'   \item{\code{Day}}{day of record "dd"}
#'   \item{\code{Rain}}{daily rainfall in (mm)}
#'   \item{\code{Tmax}}{daily maximum temperature at 2m height in (°C)}
#'   \item{\code{Tmin}}{daily minimum temperature at 2m height in (°C)}
#'}
#' @source INAM - Instituto Nacional de Meteorologia, Mozambique
#'
#' @keywords datasets
#'
#' @examples
#'
#' data(climateData)
#'
#' head(climateData)
#'
"climateData"


