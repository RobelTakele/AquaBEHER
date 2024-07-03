#     fcstWSC.R Seasonal Forecast of WSC
#
#     Copyright (C) 2022 Center of Plant Sciences, Scuola Superiore Sant’Anna
#     (http://www.capitalisegenetics.santannapisa.it)
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

#' @title Seasonal Forecast of WSC from Rainfall Probabilities
#' @description This function forecasts the Wet Season Calendar (WSC) based on seasonal rainfall 
#' totals and seasonal forecast of rainfall terciles.
#' @param sesRain  A data frame containing seasonal rainfall data with columns 'Year' and 'sRain' 
#' (seasonal rainfall).
#' @param var.dF A data frame containing WSC variables including the year and the variable of 
#' interest (e.g., 'onset' or 'cessation').
#' @param rainTerc A data frame with tercile probabilities for rainfall (columns 'T1', 'T2', and 'T3' 
#' representing below normal, normal, and above normal terciles respectively).
#' @param variable A character string indicating the variable of interest ('onset' or 'cessation').
#' @return A data frame containing the probabilities of the variable falling into the below normal,
#'  normal, and above normal terciles (columns 'BN', 'NN', and 'AN' respectively).
#'
#' @importFrom stats quantile
#'
#'
#'
#'
#'
#' @export
###############################################################################
# ***** function to forecast WSC

fcstWSC <- function(sesRain, var.dF, rainTerc, variable) {


  if (variable == "onset") {

###############################################################################

    var.dF$sRain <- sesRain[sesRain$Year %in% var.dF$Year, ]$sRain
    var.dF$tercVal <- NA

    rr.tercCUT <- quantile(var.dF$sRain, probs = c(1/3, 2/3),
                           na.rm = TRUE)

    for (yr in seq_along(var.dF$Year))  {

      Yyr <-  var.dF[yr, ]

      if ( Yyr$sRain <=  rr.tercCUT[1]) {

        var.dF$tercVal[yr] <- 1

      } else if (Yyr$sRain >=  rr.tercCUT[2]) {

        var.dF$tercVal[yr] <- 3

      } else {

        var.dF$tercVal[yr] <- 2

      }
    }


    var.dF.t1 <- var.dF[var.dF$tercVal %in% 1, ]
    idx.t1 <- round(rainTerc$T1 * nrow(var.dF))
    resa.t1 <- sample(x = var.dF.t1$onset.Value[!is.na(var.dF.t1$onset.Value)],
                      size = idx.t1,
                      replace = TRUE)

    var.dF.t2 <- var.dF[var.dF$tercVal %in% 2, ]
    idx.t2 <- round(rainTerc$T2 * nrow(var.dF))
    resa.t2 <- sample(x = var.dF.t2$onset.Value[!is.na(var.dF.t2$onset.Value)],
                      size = idx.t2,
                      replace = TRUE)

    var.dF.t3 <- var.dF[var.dF$tercVal %in% 3, ]
    idx.t3 <- round(rainTerc$T3 * nrow(var.dF))
    resa.t3 <- sample(x = var.dF.t3$onset.Value[!is.na(var.dF.t3$onset.Value)],
                      size = idx.t3,
                      replace = TRUE)

    var.dF.Ens <- data.frame(Members = seq_along(c(resa.t1, resa.t2, resa.t3)),
                             onset.Value = c(resa.t1, resa.t2, resa.t3))


    ons.tercileCUT <- quantile(var.dF$onset.Value,
                               probs = c(1/3, 2/3), na.rm = TRUE)

      fcst.m <- var.dF.Ens$onset.Value
      tercileCUT.dF <- as.data.frame(lapply(ons.tercileCUT, rep, length(fcst.m)))
      dF.m <- cbind.data.frame(fcst.m, tercileCUT.dF)

      tercInd.ons <- sapply(seq_along(dF.m[,1]),

                          FUN = function(x) {
                            y <- NA
                            if (length(which(!is.na(dF.m[x,]))) == 3) {
                              y <- which(order(c(dF.m[x, 1], dF.m[x, 2], dF.m[x, 3])) == 1)
                            }

                            y
                          }, simplify = TRUE)

      terCount <- table(as.numeric(tercInd.ons))
      probTerc.1 <- round(terCount[1]/sum(terCount) * 100, digits = 2)
      probTerc.2 <- round(terCount[2]/sum(terCount) * 100, digits = 2)
      probTerc.3 <- round(terCount[3]/sum(terCount) * 100, digits = 2)

      probTerc.dF <- cbind.data.frame(BN = probTerc.1,
                                      NN = probTerc.2,
                                      AN = probTerc.3)


###############################################################################

  } else if (variable == "cessation") {


    var.dF$sRain <- sesRain[sesRain$Year %in% var.dF$Year, ]$sRain
    var.dF$tercVal <- NA

    rr.tercCUT <- quantile(var.dF$sRain, probs = c(1/3, 2/3),
                           na.rm = TRUE)

    for (yr in seq_along(var.dF$Year))  {

      Yyr <-  var.dF[yr, ]

      if ( Yyr$sRain <=  rr.tercCUT[1]) {

        var.dF$tercVal[yr] <- 1

      } else if (Yyr$sRain >=  rr.tercCUT[2]) {

        var.dF$tercVal[yr] <- 3

      } else {

        var.dF$tercVal[yr] <- 2

      }
    }


    var.dF.t1 <- var.dF[var.dF$tercVal %in% 1, ]
    idx.t1 <- round(rainTerc$T1 * nrow(var.dF))
    resa.t1 <- sample(x = var.dF.t1$cessation.Value[!is.na(var.dF.t1$cessation.Value)],
                      size = idx.t1,
                      replace = TRUE)

    var.dF.t2 <- var.dF[var.dF$tercVal %in% 2, ]
    idx.t2 <- round(rainTerc$T2 * nrow(var.dF))
    resa.t2 <- sample(x = var.dF.t2$cessation.Value[!is.na(var.dF.t2$cessation.Value)],
                      size = idx.t2,
                      replace = TRUE)

    var.dF.t3 <- var.dF[var.dF$tercVal %in% 3, ]
    idx.t3 <- round(rainTerc$T3 * nrow(var.dF))
    resa.t3 <- sample(x = var.dF.t3$cessation.Value[!is.na(var.dF.t3$cessation.Value)],
                      size = idx.t3,
                      replace = TRUE)

    var.dF.Ens <- data.frame(Members = seq_along(c(resa.t1, resa.t2, resa.t3)),
                             cessation.Value = c(resa.t1, resa.t2, resa.t3))


    ons.tercileCUT <- quantile(var.dF$cessation.Value,
                               probs = c(1/3, 2/3), na.rm = TRUE)

    fcst.m <- var.dF.Ens$cessation.Value
    tercileCUT.dF <- as.data.frame(lapply(ons.tercileCUT, rep, length(fcst.m)))
    dF.m <- cbind.data.frame(fcst.m, tercileCUT.dF)

    tercInd.ons <- sapply(seq_along(dF.m[,1]),

                          FUN = function(x) {
                            y <- NA
                            if (length(which(!is.na(dF.m[x,]))) == 3) {
                              y <- which(order(c(dF.m[x, 1], dF.m[x, 2], dF.m[x, 3])) == 1)
                            }

                            y
                          }, simplify = TRUE)

    terCount <- table(as.numeric(tercInd.ons))
    probTerc.1 <- round(terCount[1]/sum(terCount) * 100, digits = 2)
    probTerc.2 <- round(terCount[2]/sum(terCount) * 100, digits = 2)
    probTerc.3 <- round(terCount[3]/sum(terCount) * 100, digits = 2)

    probTerc.dF <- cbind.data.frame(BN = probTerc.1,
                                    NN = probTerc.2,
                                    AN = probTerc.3)


 }  # if variable


 return(probTerc.dF)

###############################################################################

}

###############################################################################
###############################################################################
###############################################################################
