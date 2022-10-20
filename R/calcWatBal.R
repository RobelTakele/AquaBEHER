#' @title Soil Water Balance
#'
#' @description This function calculates a daily water balance computation for the root zone according to algorithms described in the FAO Irrigation
#' and drainage paper 56
#'
#' @param data = a dataframe containing the required climate variables: Columns must contain the following parameters:
#'
#'               Station_Name: weather station name
#'               Lat: latitude of the site in decimal degrees
#'               Lon: longitude of the site in decimal degrees
#'               Elev: elevation above sea level (m)
#'               Year: year in YYYY format
#'               Month: month in MM format
#'               Day: day of record
#'               Rain:
#'               Tmax: daily maximum temperature at 2m height (°C)
#'               Tmin: daily minimum temperature at 2m height (°C)
#'               Eto:
#'
#' @param soilWHC Whater holding capacity of the soil
#'
#'
#' @return The function generates a data frame containing the following components:
#'
#' \code{cumRAIN:} {  }
#'
#' \code{DEMAND:} {  }
#'
#' \code{RUNOFF:} {  }
#'
#' \code{ERATIO:} {  }
#'
#' \code{AVAIL:} {   }
#'
#' @references Allen, R.G.; Pereira, L.S.; Raes, D.; Smith, M. Crop Evapotranspiration: Guidelines for Computing Crop Water Requirements; FAO Irrigation and Drainage Paper no. 56; FAO: Rome, Italy, 1998; ISBN 92-5-104219-5.
#'
#' Doorenbos, J. and Pruitt, W.O. 1975. Guidelines for predicting crop water requirements, Irrigation and Drainage Paper 24, Food and Agriculture Organization of the United Nations, Rome, 179 p.
#'
#' @examples
#'
#' data(climateData)
#'
#' Eto.daily <- calcEto(climateData)
#'
#' climateData$Eto <- Eto.daily$ET.Daily
#'
#'  soilWHC = 100
#'
#' watBal.daily <- calcWatBal(climateData, soilWHC)
#'
#' plot(watBal.daily$ERATIO*100, ty="l")
#' lines(watBal.daily$Eto, col="red")
#' lines(watBal.daily$Rain, col="blue")
#'
#' @export

######################################################################################################################################################
# ***** function to estimate Rindex: actual to potential evapotranspiration ratio based on Jones (1987)

calcWatBal <- function(data, soilWHC) {

  Rindex <- function(soilWHC, Avail) {

    percwt <- min(c(100, Avail/soilWHC*100))
    percwt <- max(c(1,percwt))
    Eratio <- min(c(percwt/(97-3.868*sqrt(soilWHC)),1))

    return(Eratio)

  }

# ****************************************************************************************************************************************************

  calcRindex <- function(soilWHC, Kc = 1, Avail, Rain, Eto) {

    Avail <- min(c(Avail, soilWHC))
    Eratio <- Rindex(soilWHC, Avail)
    Demand <- Eratio * Kc * Eto
    result <- Avail + Rain - Demand
    Runoff <- result - soilWHC
    Avail <- min(c(soilWHC, result))
    Avail <- max(c(Avail,0))
    Runoff <- max(c(Runoff,0))

    out.dF <- data.frame(AVAIL = Avail,
                         DEMAND = Demand,
                         ERATIO = Eratio,
                         RAIN = Rain,
                         RUNOFF = Runoff)
    return(out.dF)
  }

 # ****************************************************************************************************************************************************


  data$AVAIL <- data$ERATIO <- data$RUNOFF <- data$DEMAND <- data$cumRAIN <- NA

  for (day in 1:nrow(data)) {

    if (day == 1) {

      data$cumRAIN[day] <- data$Rain[day]

      sWat <- calcRindex(soilWHC = soilWHC,
                         Kc = 1,
                         Avail = 0,
                         Rain = data$Rain[day],
                         Eto = data$Eto[day])

      data$AVAIL[day] <- sWat$AVAIL
      data$ERATIO[day] <- sWat$ERATIO
      data$RUNOFF[day] <- sWat$RUNOFF
      data$DEMAND[day] <- sWat$DEMAND

    } else {

      data$cumRAIN[day] <- data$cumRAIN[day-1] + data$Rain[day]

      sWat <- calcRindex(soilWHC = soilWHC,
                         Kc = 1,
                         Avail = data$AVAIL[day-1],
                         Rain = data$Rain[day],
                         Eto = data$Eto[day])

      data$AVAIL[day] <- sWat$AVAIL
      data$ERATIO[day] <- sWat$ERATIO
      data$RUNOFF[day] <- sWat$RUNOFF
      data$DEMAND[day] <- sWat$DEMAND

    }

  }


  return(data)

}


#####################################################################################################################################################
#####################################################################################################################################################
#####################################################################################################################################################
