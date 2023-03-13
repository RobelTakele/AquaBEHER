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
#########################################################################################################################################

#' @title Example AgroClimate data from AgERA5
#'
#' @description The AgERA5 dataset provides daily surface meteorological data for the period from 1979 to present as input for agriculture
#' and agro-ecological studies. This dataset is based on the hourly ECMWF ERA5 data at surface level
#'
#' NASA/POWER CERES/MERRA2 Native Resolution Daily Data from 01/01/1996 through 12/31/2020 extracted for a grid located in located in
#' Angochen, Nampula province of Mozambique.
#'
#' @format A data frame containing daily observations of AgroClimate parameters:
#' \describe{
#'   \item{\code{GridID}}{Grid id of the location.}
#'   \item{\code{Lat}}{latitude of the site in decimal degrees.}
#'   \item{\code{Lon}}{longitude of the site in decimal degrees.}
#'   \item{\code{Elev}}{elevation above sea level in (meters).}
#'   \item{\code{WHC}}{water holding capacity in (mm).}
#'   \item{\code{Year}}{year of record "YYYY".}
#'   \item{\code{Month}}{month of record "MM".}
#'   \item{\code{Day}}{day of record "DD".}
#'   \item{\code{Rain}}{Precipitation (mm/day).}
#'   \item{\code{Tmax}}{Temperature at 2 Meters Maximum (°C).}
#'   \item{\code{Tmin}}{Temperature at 2 Meters Minimum (°C).}
#'   \item{\code{Rs}}{CAll Sky Surface Shortwave Downward Irradiance (MJ/m^2/day).}
#'   \item{\code{Tdew}}{Dew/Frost Point at 2 Meters (°C).}
#'   \item{\code{Uz}}{Wind Speed at 2 Meters (m/s).}
#'}
#'
#' @source \url{https://cds.climate.copernicus.eu/cdsapp#!/dataset/sis-agrometeorological-indicators?tab=overview}
#'
#' @keywords datasets
#'
#' @references AgERA5, 2021, Copernicus Climate Change Service (C3S), Fifth generation of ECMWF atmospheric reanalysis of the global
#' climate for agriculture and ago-ecological studies. Copernicus Climate Change Service Climate Data Store (CDS), July-2021.
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
##########################################################################################################################################
