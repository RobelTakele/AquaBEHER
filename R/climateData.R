#     climateData.R Example climate data
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

#' @title A dataframe containing raw climate data
#'
#' @description The R data object was obtained from Instituto Nacional de Meteorologia (INAM). This example data set contains the raw climate data
#'  including the variables required for calculating evapotranspiration and soil water balance over the period between 1/1/1980 and 12/31/1984 at a
#'  weather station located in Nampula province of Mozambique.
#'
#' @docType data
#'
#' @usage data(climateData)
#'
#' @format A data frame with 1827 rows and 11 variables:
#'
#'   \verb{    }\emph{\strong{\code{WMO_Code:}} World Meteorological Organization (WMO) station code.}
#'
#'   \verb{    }\emph{\strong{\code{Station_Name:}} name of the weather station.}
#'
#'   \verb{    }\emph{\strong{\code{Lat:}} latitude of the site in decimal degrees.}
#'
#'   \verb{    }\emph{\strong{\code{Lon:}} longitude of the site in decimal degrees.}
#'
#'   \verb{    }\emph{\strong{\code{Elev:}} elevation above sea level in (meters).}
#'
#'   \verb{    }\emph{\strong{\code{Year:}} year of record "YYYY".}
#'
#'   \verb{    }\emph{\strong{\code{Month:}} month of record "MM".}
#'
#'   \verb{    }\emph{\strong{\code{Day:}} day of record "DD".}
#'
#'   \verb{    }\emph{\strong{\code{Rain:}} daily rainfall in (mm).}
#'
#'   \verb{    }\emph{\strong{\code{Tmax:}} daily maximum temperature at 2-m height in (°C).}
#'
#'   \verb{    }\emph{\strong{\code{Tmin:}} daily minimum temperature at 2-m height in (°C).}
#'
#' @source INAM - Instituto Nacional de Meteorologia, Mozambique \url{https://www.inam.gov.mz/}
#'
#' @keywords datasets
#'
#' @seealso \code{\link{calcEto}} for a daily estimation of potential evapotranspiration.
#'
#' @examples
#' # load example data:
#' data(climateData)
#'
#' # Get the structure of the data frame:
#' str(climateData)
#'
#' # Get the head of the data frame:
#' head(climateData)
#'
"climateData"

# ********** end of code **********
######################################################################################################################################################
