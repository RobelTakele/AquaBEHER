#     AgroClimateData.R Example AgroClimateData data used for Eto and soil water balance calculations
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

#' @title Example AgroClimate data from NASA POWER
#'
#' @description NASA/POWER CERES/MERRA2 Native Resolution Daily Data from 01/01/1996 through 12/31/2020 extracted for a grid located in located in
#' Angochen, Nampula province of Mozambique.
#'
#' @format A data frame containing daily observations of AgroClimate parameters:
#' \describe{
#'   \item{\code{Source}}{Source of the data.}
#'   \item{\code{Lat}}{latitude of the site in decimal degrees.}
#'   \item{\code{Lon}}{longitude of the site in decimal degrees.}
#'   \item{\code{Elev}}{elevation above sea level in (meters).}
#'   \item{\code{Year}}{year of record "YYYY".}
#'   \item{\code{Month}}{month of record "MM".}
#'   \item{\code{Day}}{day of record "DD".}
#'   \item{\code{Rain}}{ MERRA-2 Precipitation Corrected (mm/day).}
#'   \item{\code{Tmax}}{MERRA-2 Temperature at 2 Meters Maximum (°C).}
#'   \item{\code{Tmin}}{ MERRA-2 Temperature at 2 Meters Minimum (°C).}
#'   \item{\code{Rs}}{CERES SYN1deg All Sky Surface Shortwave Downward Irradiance (MJ/m^2/day).}
#'   \item{\code{RH}}{MERRA-2 Relative Humidity at 2 Meters (%).}
#'   \item{\code{Tdew}}{MERRA-2 Dew/Frost Point at 2 Meters (°C).}
#'   \item{\code{U2}}{MERRA-2 Wind Speed at 2 Meters (m/s).}
#'}
#'
#' @source \url{https://power.larc.nasa.gov/data-access-viewer/}
#'
#' @keywords datasets
#'
#' @seealso \code{\link{climateData}, \link{calcEto}}
#'
#' @examples
#' # load example data:
#' data(AgroClimateData)
#'
#' # Get the structure of the data frame:
#' str(AgroClimateData)
#'
#' # Get the head of the data frame:
#' head(AgroClimateData)
#'
"AgroClimateData"

# ********** end of code **********
######################################################################################################################################################
