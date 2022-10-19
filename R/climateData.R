#' Raw Climate Data Required for Calculating Evapotranspiration
#'
#' A example data set contains the raw climate data including the variables required for calculating evapotranspiration in function calcEto
#' over the period between 1/1/1980 and 12/31/1984 at Nampula station in Mozambique.
#'
#' @docType data
#'
#' @usage data(climateData)
#'
#' @format An object of class \code{"data.frame"}
#' \describe{
#'               Station_Name: weather station name
#'               Lat: latitude of the site in decimal degrees [°]
#'               Lon: longitude of the site in decimal degrees [°]
#'               Elev: elevation above sea level [m]
#'               Year: year in YYYY format
#'               Month: month in MM format
#'               Day: day in DD format
#'               Tmax: daily maximum temperature at 2m height [°C]
#'               Tmin: daily minimum temperature at 2m height [°C]
#'  }
#'
#' @source INAM - Instituto Nacional de Meteorologia, Mozambique
#'
#' @keywords datasets
#'
#' @examples
#'
#' data(climateData)
#' head(climateData)
#'
#' @export
#'
# "climateData"


