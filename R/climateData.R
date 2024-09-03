##################################################################################################
##################################################################################################
#
#     climateData.R: Daily Weather Data (1996-2020) from Angochen Weather Observing Station.
#
#     Copyright (C) 2024 Institute of Plant Sciences, Sant’Anna School of Advanced Studies,
#     Pisa, Italy (https://www.santannapisa.it/en/institute/plant-sciences).
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
##################################################################################################
##################################################################################################

#' @title Daily Weather Data (1996-2020) from Angochen Weather Observing Station, Mozambique
#'
#' @description This R data object contains daily weather data obtained from Instituto Nacional de
#' Meteorologia (INAM). The sample data includes daily raw weather data from January 1, 1996, to
#' December 31, 2020, from a weather station located in Angochen, Nampula province of Mozambique.
#'
#' @docType data
#'
#' @usage data(climateData)
#'
#' @format A data frame containing daily values of weather variables:
#' - **Station_Name**: Name of the weather station.
#' - **Lat**: Latitude of the site in decimal degrees.
#' - **Lon**: Longitude of the site in decimal degrees.
#' - **Elev**: Elevation above sea level in meters.
#' - **Year**: Year of the record (YYYY).
#' - **Month**: Month of the record (MM).
#' - **Day**: Day of the record (DD).
#' - **Rain**: Daily rainfall in millimeters (mm).
#' - **Tmax**: Daily maximum temperature at 2 meters height in degrees Celsius (°C).
#' - **Tmin**: Daily minimum temperature at 2 meters height in degrees Celsius (°C).
#'
#' @source INAM - Instituto Nacional de Meteorologia, Mozambique
#'
#' @keywords datasets, weather, climate, Mozambique, meteorology
#'
#' @seealso \code{\link{AgroClimateData}}, \code{\link{calcEto}}
#'
#' @examples
#' ## Load the climate data
#' data(climateData)
#'
#' ## Get the structure of the data frame
#' str(climateData)
#'
#' ## Get the head of the data frame
#' head(climateData)
#'
"climateData"

###################################################################################################
###################################################################################################
#                          >>>>>>>>>>   End of code   <<<<<<<<<<                                  #
###################################################################################################
###################################################################################################
