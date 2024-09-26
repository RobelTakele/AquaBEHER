###############################################################################
###############################################################################
#
#   AgroClimateData.R: Daily Surface Meteorological Data (1982-2022) Extracted
#   from AgERA5
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

#' @title Daily Surface Meteorological Data (1982-2022) Extracted from AgERA5
#'
#' @description The AgERA5 dataset provides daily surface meteorological data
#' for the period from 1979 to present as input for agriculture and
#' agro-ecological studies. This dataset is based on the hourly ECMWF ERA5 data
#' at surface level, with data from January 1, 1982, through December 31, 2022,
#' extracted for a grid located in Angochen, Nampula province of Mozambique.
#'
#' @docType data
#'
#' @usage data(AgroClimateData)
#'
#' @format A data frame containing daily observations of weather variables:
#' - **GridID**: Grid ID of the location.
#' - **Lat**: Latitude of the grid in decimal degrees.
#' - **Lon**: Longitude of the grid in decimal degrees.
#' - **Elev**: Elevation above sea level in meters.
#' - **WHC**: Water holding capacity in millimeters (mm).
#' - **Year**: Year of record ("YYYY").
#' - **Month**: Month of record ("MM").
#' - **Day**: Day of record ("DD").
#' - **Rain**: Daily precipitation in millimeters (mm/day).
#' - **Tmax**: Daily maximum temperature at 2 meters height in degrees Celsius
#' (°C).
#' - **Tmin**: Daily minimum temperature at 2 meters height in degrees Celsius
#' (°C).
#' - **Rs**: Total amount of energy provided by solar radiation at the surface
#' over the period 00-24h local time per unit area and time (MJ/m^2/day).
#' - **Tdew**: Mean dew point temperature at a height of 2 meters above the
#' surface over the period 00h-24h local time (°C).
#' - **Uz**: Mean wind speed at a height of 2 meters above the surface over the
#' period 00h-24h local time (m/s).
#'
#' @source \url{https://cds.climate.copernicus.eu/cdsapp#!/dataset/sis-agrometeorological-indicators?tab=overview}
#'
#' @references AgERA5, 2021, Copernicus Climate Change Service (C3S), Fifth
#' generation of ECMWF atmospheric reanalysis of the global climate for
#' agriculture and agro-ecological studies. Copernicus Climate Change Service
#' Climate Data Store (CDS), July 2021.
#'
#' @seealso \code{\link{climateData}}, \code{\link{calcEto}}
#'
#' @examples
#' ## Load the agroclimate data
#' data(AgroClimateData)
#'
#' ## Get the structure of the data frame
#' str(AgroClimateData)
#'
#' ## Get the head of the data frame
#' head(AgroClimateData)
#'
"AgroClimateData"

###############################################################################
###############################################################################
#                >>>>>>>>>>   End of code   <<<<<<<<<<                        #
###############################################################################
###############################################################################
