<<<<<<< HEAD
###############################################################################
###############################################################################
#
#     climateData.R: Daily Weather Data (1996-2020) from Angochen Weather
#     Observing Station.
#
#     Copyright (C) 2024 Institute of Plant Sciences, Sant’Anna School of
#     Advanced Studies, Pisa, Italy
#     (https://www.santannapisa.it/en/institute/plant-sciences).
=======
#     climateData.R Example climate data
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

#' @title Daily Weather Data (1996-2020) from Angochen Weather Observing
#' Station, Mozambique
#'
#' @description This R data object contains daily weather data obtained from
#' Instituto Nacional de Meteorologia (INAM). The sample data includes daily raw
#' weather data from January 1, 1996, to December 31, 2020, from a weather
#' station located in Angochen, Nampula province of Mozambique.
=======
###################################################################################################################################

#' @title A dataframe containing raw climate data
#'
#' @description The R data object was obtained from Instituto Nacional de Meteorologia (INAM). This example data set contains the daily raw climate
#'  data over the period between 1/1/1996 and 12/31/2020 from a weather station located in Angochen, Nampula province of Mozambique.
>>>>>>> 5bf4ef90266654cfdc6ded681017190829d9c65a
#'
#' @docType data
#'
#' @usage data(climateData)
#'
<<<<<<< HEAD
#' @format A data frame containing daily values of weather variables:
#' - **Station_Name**: Name of the weather station.
#' - **Lat**: Latitude of the site in decimal degrees.
#' - **Lon**: Longitude of the site in decimal degrees.
#' - **Elev**: Elevation above sea level in meters.
#' - **Year**: Year of the record (YYYY).
#' - **Month**: Month of the record (MM).
#' - **Day**: Day of the record (DD).
#' - **Rain**: Daily rainfall in millimeters (mm).
#' - **Tmax**: Daily maximum temperature at 2 meters height in degrees Celsius
#' (°C).
#' - **Tmin**: Daily minimum temperature at 2 meters height in degrees Celsius
#' (°C).
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
=======
#' @format A data frame containing daily observations of climate parameters:
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
#' @seealso \code{\link{AgroClimateData}, \link{calcEto}}
#'
#' @examples
#' # load example data:
#' data(climateData)
#'
#' # Get the structure of the data frame:
#' str(climateData)
#'
#' # Get the head of the data frame:
>>>>>>> 5bf4ef90266654cfdc6ded681017190829d9c65a
#' head(climateData)
#'
"climateData"

<<<<<<< HEAD
###############################################################################
###############################################################################
#                >>>>>>>>>>   End of code   <<<<<<<<<<                        #
###############################################################################
###############################################################################
=======
# ********** end of code **********
######################################################################################################################################################
>>>>>>> 5bf4ef90266654cfdc6ded681017190829d9c65a
