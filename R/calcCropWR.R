#     calcEto.R Crop Water Requirement
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

#' @title Crop Water Requirement
#'
#' @description This function computes crop water requirement/water demand and crop coefficients using the method described in FAO56
#'
#' @param data a dataframe containing the required climate data, see \code{\link{calcEto}}
#' @param Crop character. Name of the crop , Default: NULL
#' @param pheno length of crop growth stages, Default: c(Ph_ini = NULL, Ph_dev = NULL, Ph_mid = NULL, Ph_late = NULL)
#' @param Kc.Type the type of Kc used for estimation, either "single" or "double", Default: 'Single'
#' @param Kc defaoult Kc values in the form of: c(Kc_ini = NULL, Kc.mid = NULL, Kc.end = NULL)
#' @param CropHmax maximum crop height in (m)
#' @param plantDate a dataframe containing columens with 'Year' in YYYY and 'plantDate' in julian date.
#' @param WHC soil water holding capacity
#' @param SoilTex a character specfying the name of soil textural class according to USDA
#'
#' @return The function generates a data frame containing the following components:
#'
#' \emph{\code{Crop: the crop name that water requirement estimated for .}}
#'
#' \emph{\code{Eto: .daily estimations of reference crop evapotranspiration (mm/day).}}
#'
#' \emph{\code{tabKc: daily tabulated Kc values over the growing period.}}
#'
#' \emph{\code{adjKc: daily adjested Kc values over the growing period.}}
#'
#' \emph{\code{CropWR: daily estimates of crop water requirement over the growing period.}}
#'
#' @references Allen, R.G.; Pereira, L.S.; Raes, D.; Smith, M. Crop Evapotranspiration: Guidelines for Computing Crop Water Requirements; FAO
#' Irrigation and Drainage Paper no. 56; FAO: Rome, Italy, 1998; ISBN 92-5-104219-5.
#'
#' @seealso \code{\link{calcEto}, \link{calcSeasCal},  \link{calcWatBal}}
#'
#' @importFrom soiltexture TT.points.in.classes
#' @importFrom graphics lines
#'
#' @examples
#' # load example data:
#' data(AgroClimateData)
#'
#' @export

######################################################################################################################################################

calcCropWR <- function(data, Crop = NULL, pheno = c(Ph_ini = NULL, Ph_dev = NULL, Ph_mid = NULL, Ph_late = NULL), Kc.Type = "Single",
                       Kc = c(Kc_ini = NULL, Kc.mid = NULL, Kc.end = NULL),  CropHmax = NULL, plantDate = NULL, WHC = NULL, SoilTex = NULL)  {

  #  crop.vec <- c("Cowpeas", "Spring Wheat", "Maize", "Sorghum", "Rice")

  # ***** Lengths of crop development stages (Phenology) in days (FAO56)

  pheno.dF <- data.frame(Crop = c("Cowpeas", "Spring Wheat", "Maize", "Sorghum", "Rice"),
                         Ph_ini = c(20, 20, 30, 20, 30),
                         Ph_dev = c(30, 25, 50, 35, 30),
                         Ph_mid = c(30, 60, 60, 45, 60),
                         Ph_late = c(20, 30, 40, 30, 30),
                         Total = c(110, 135, 180, 140, 150),
                         PlantDate = c("March", "March/Apr", "April", "Mar/April" , "Dec/May"),
                         Region = c("Mediterranean", "35-45 Lat", "East Africa", "Arid Region", "Tropics/Mediterranean"))

  # ***** Tabulated Kc values and Maximum Crop Height from FAO-56
  # ***** Cowpeas; Kc_end = 0.6, for harvested fresh; Kc_end = 0.35, for harvested dry.
  # **** Spring Wheat: Kc_end = 0.4, is for hand-harvested crops, else Kc_end = 0.25
  # ***** Maize:  Kc_end = 0.6, harvest at high grain moisture. harvest; Kc_end = 0.35, for harvest after complete field drying of the grain
  #                      (to about 18% moisture, wet mass basis).

  Kc.dF <- data.frame(Crop = c("Cowpeas", "Spring Wheat", "Maize", "Sorghum", "Rice"),
                      Kc.Type = "Single",
                      Kc_ini = c(0.4, 0.3, 0.3, 0.3, 1.05),
                      Kc.mid = c(1.05, 1.15, 1.2, 1.1, 1.2),
                      Kc.end = c(0.35, 0.4, 0.35, 0.55, 0.8),
                      CropHmax = c(0.4, 1, 2, 2, 1))


  ######################################################################################################################################################
  # ***** Checking required arguments

  if (is.null(SoilTex)) {

    stop("Please enter the required information on soil texture")

  } else if (is.character(SoilTex)) {

    SoilTex <- SoilTex
  } else if (is.data.frame(SoilTex)) {

    tex.dF <- as.data.frame(soiltexture::TT.points.in.classes(tri.data = SoilTex, class.sys = "USDA.TT"))
    SoilTex.nm <- names(which.max(tex.dF))

    if (SoilTex.nm == "Cl")     SoilTex = "clay"
    if (SoilTex.nm == "SiCl")   SoilTex = "silty clay"
    if (SoilTex.nm == "SaCl")   SoilTex = "sandy clay"
    if (SoilTex.nm == "ClLo")   SoilTex = "clay loam"
    if (SoilTex.nm == "SiClLo") SoilTex = "silty clay loam"
    if (SoilTex.nm == "SaClLo") SoilTex = "sandy clay loam"
    if (SoilTex.nm == "Lo")     SoilTex = "loam"
    if (SoilTex.nm == "SiLo")   SoilTex = "silty loam"
    if (SoilTex.nm == "SaLo")   SoilTex = "sandy loam"
    if (SoilTex.nm == "Si")     SoilTex = "silt"
    if (SoilTex.nm == "LoSa")   SoilTex = "loamy sand"
    if (SoilTex.nm == "Sa")     SoilTex = "sand"

  }


  if (is.null(Crop)) {

    stop("Please enter the desired crop name")

  }

  if (is.null(plantDate)) {

    stop("Please enter the planting date for each season")

  }

  if (is.null(pheno)) {

    pheno = pheno.dF[pheno.dF$Crop %in% Crop, ][,2:5]

  }

  if (is.null(Kc)) {

    Kc = Kc.dF[Kc.dF$Crop %in% Crop, ][,3:5]

  }

  if (is.null(CropHmax)) {

    CropHmax = Kc.dF[Kc.dF$Crop %in% Crop, ][, 6]

  }

  if (is.null(data$Va)|is.null(data$Vs)) {
    if (is.null(data$RHmax)|is.null(data$RHmin)) {
      if (is.null(data$RH)) {
        if (is.null(data$Tdew)) {
          stop("Required data missing: need either 'Tdew', or 'Va' and 'Vs', or 'RHmax' and 'RHmin', r 'RH'")
        }
      }
    }
  }

  if (is.null(data$RHmin) & !is.null(data$Tdew)) {

    # Saturation vapor pressure from temperature (kPa)

    EsTmax <- 0.6108 * exp(17.27 * data$Tmax / (data$Tmax + 237.3))

    # Actual vapour pressure derived from dewpoint temperature

    Ea  <- 0.6108 * exp((17.27 * data$Tdew)/(data$Tdew + 237.3))

    data$RHmin <- (Ea/EsTmax) * 100

  } else if (is.null(data$RHmin) & is.null(data$Tdew) & !is.null(data$RH)) {

    # Saturation vapor pressure from temperature (kPa)

    EsTmax <- 0.6108 * exp(17.27 * data$Tmax / (data$Tmax + 237.3))
    EsTmin <- 0.6108 * exp(17.27 * data$Tmin / (data$Tmin + 237.3))

    Es <- (EsTmax + EsTmin)/2

    # Actual vapor pressure derived from max and min relative humidity data

    Ea  <- (data$RH/100) * Es

    data$RHmin <- (Ea/EsTmax) * 100

  }



######################################################################################################################################################
  # ***** estimating crop water demand


  data.WR <- data.frame()

  for (yr in (plantDate$Year)) {

    # ***** constructing the Kc curve using tabulated values

    plantDate.yr <- plantDate[plantDate$Year %in% yr,]

    data.yr <- data[data$Year %in% c(yr,yr+1),]
    data.yr$h <- NA
    data.yr$Stage <- NA
    data.yr$tabkc <- NA
    data.yr$adjkc <- NA

    data.yr$Stage[plantDate.yr[1,2]:(plantDate.yr[1,2]+pheno$Ph_ini)] <- "Initial"
    data.yr$tabkc[plantDate.yr[1,2]:(plantDate.yr[1,2]+pheno$Ph_ini)] <-  Kc$Kc_ini

    data.yr$Stage[(plantDate.yr[1,2]+pheno$Ph_ini+1):(plantDate.yr[1,2]+pheno$Ph_ini+pheno$Ph_dev)] <- "Development"
    data.yr$tabkc[(plantDate.yr[1,2]+pheno$Ph_ini+1):
                    (plantDate.yr[1,2]+pheno$Ph_ini+pheno$Ph_dev)] <- seq(from = Kc$Kc_ini, to = Kc$Kc.mid, length.out = pheno$Ph_dev)

    data.yr$Stage[(plantDate.yr[1,2]+pheno$Ph_ini+pheno$Ph_dev+1):(plantDate.yr[1,2]+pheno$Ph_ini+pheno$Ph_dev+pheno$Ph_mid)] <- "Mid-season"
    data.yr$tabkc[(plantDate.yr[1,2]+pheno$Ph_ini+pheno$Ph_dev+1):(plantDate.yr[1,2]+pheno$Ph_ini+pheno$Ph_dev+pheno$Ph_mid)] <- Kc$Kc.mid

    data.yr$Stage[(plantDate.yr[1,2]+pheno$Ph_ini+pheno$Ph_dev+pheno$Ph_mid+1):
                    (plantDate.yr[1,2]+pheno$Ph_ini+pheno$Ph_dev+pheno$Ph_mid+pheno$Ph_late)] <- "Late season"
    data.yr$tabkc[(plantDate.yr[1,2]+pheno$Ph_ini+pheno$Ph_dev+pheno$Ph_mid+1):
                    (plantDate.yr[1,2]+pheno$Ph_ini+pheno$Ph_dev+pheno$Ph_mid+pheno$Ph_late)] <-
      seq(from = Kc$Kc.mid, to = Kc$Kc.end, length.out = pheno$Ph_late)

    # ****  crop growth linearly up to the Mid-season stage ??????????????????????????????/

    data.yr$h[(plantDate.yr[1,2]):(plantDate.yr[1,2]+pheno$Ph_ini+pheno$Ph_dev)] <-
      seq(from = 0.1, to = CropHmax, length.out = (pheno$Ph_dev+pheno$Ph_ini+1))

    data.yr$h[(plantDate.yr[1,2]+pheno$Ph_ini+pheno$Ph_dev+1):(plantDate.yr[1,2]+pheno$Ph_ini+pheno$Ph_dev+pheno$Ph_mid+pheno$Ph_late)] <- CropHmax

    # ***** Adjust Kc_ini to reflect wetting frequency of soil surface [if Kc_ini ≤ 1.15]

    # ***** REW: Readily evaporable water (i.e., maximum depth of water that can be evaporated from the soil surface layer without restriction
    # during stage 1) in [mm] .

    if (SoilTex == "sand") {
      REW = 4
    } else if (SoilTex == "loamy sand") {
      REW = 6
    } else if (SoilTex == "sandy loam") {
      REW = 8
    } else if (SoilTex == "loam") {
      REW = 9
    } else if (SoilTex == "silty loam") {
      REW = 9.5
    } else if (SoilTex == "silt") {
      REW = 9.5
    } else if (SoilTex == "silty clay loam") {
      REW = 9.5
    } else if (SoilTex == "silty clay") {
      REW = 10
    } else if (SoilTex == "clay") {
      REW = 10
    } else {
      REW = 6
    }

    REWcor <- min(REW, WHC)       # ******** ????????????????????????????????????
    TEW = WHC                      # ******** ????????????????????????????????????
    TEWcor <- min(TEW, WHC)

    # ***** average time between wetting events (Tw) in days


    Nw <- length(data.yr$Rain[plantDate.yr[1,2]:(plantDate.yr[1,2]+pheno$Ph_ini)][data.yr$Rain[plantDate.yr[1,2]:(plantDate.yr[1,2]+pheno$Ph_ini)] > 1])
    Pmean <- sum(data.yr$Rain[plantDate.yr[1,2]:(plantDate.yr[1,2]+pheno$Ph_ini)], na.rm = TRUE)/Nw
    Eto.mean <- mean(data.yr$Rain[plantDate.yr[1,2]:(plantDate.yr[1,2]+pheno$Ph_ini)], na.rm = TRUE)

    Tw <- pheno$Ph_ini/(Nw + 0.5)

    Eso <- 1.15 * Eto.mean

    T1 <- REWcor/Eso

    adjKc_ini <- (TEWcor - (TEWcor - REWcor) * exp(-(Tw - T1) * Eso * (1 + (REWcor/(TEWcor - REWcor)))/TEWcor)) /(Tw * Eto.mean)

    adjKc_ini <- min( adjKc_ini, 1.15)


    # ***** Climate Adjustment for Kc_mid

    h.mid <- mean(data.yr$h[(plantDate.yr[1,2]+pheno$Ph_ini+pheno$Ph_dev+1):(plantDate.yr[1,2]+pheno$Ph_ini+pheno$Ph_dev+pheno$Ph_mid)], na.rm = TRUE)
    U2.mid <- mean(data.yr$U2[(plantDate.yr[1,2]+pheno$Ph_ini+pheno$Ph_dev+1):(plantDate.yr[1,2]+pheno$Ph_ini+pheno$Ph_dev+pheno$Ph_mid)], na.rm = TRUE)
    RHmin.mid <- mean(data.yr$RHmin[(plantDate.yr[1,2]+pheno$Ph_ini+pheno$Ph_dev+1):(plantDate.yr[1,2]+pheno$Ph_ini+pheno$Ph_dev+pheno$Ph_mid)],
                      na.rm = TRUE)

    adjKc_mid <- Kc$Kc.mid + ((0.04*( U2.mid  - 2))-(0.004*(RHmin.mid-45))*(h.mid/3)^0.3)


    # ***** Climate Adjustment for Kc_end

    h.end <- mean(data.yr$h[(plantDate.yr[1,2]+pheno$Ph_ini+pheno$Ph_dev+pheno$Ph_mid+1):
                              (plantDate.yr[1,2]+pheno$Ph_ini+pheno$Ph_dev+pheno$Ph_mid+pheno$Ph_late)], na.rm = TRUE)
    U2.end <- mean(data.yr$U2[(plantDate.yr[1,2]+pheno$Ph_ini+pheno$Ph_dev+pheno$Ph_mid+1):
                                (plantDate.yr[1,2]+pheno$Ph_ini+pheno$Ph_dev+pheno$Ph_mid+pheno$Ph_late)], na.rm = TRUE)

    RHmin.end <- mean(data.yr$RHmin[(plantDate.yr[1,2]+pheno$Ph_ini+pheno$Ph_dev+pheno$Ph_mid+1):
                                      (plantDate.yr[1,2]+pheno$Ph_ini+pheno$Ph_dev+pheno$Ph_mid+pheno$Ph_late)], na.rm = TRUE)

    adjKc_end <- Kc$Kc.end + ((0.04*(U2.end - 2))-(0.004*(RHmin.end-45))*(h.end/3)^0.3)

    # ***** constructing the Kc curve using adjested values

    data.yr$adjkc[plantDate.yr[1,2]:(plantDate.yr[1,2]+pheno$Ph_ini)] <-  adjKc_ini

    data.yr$adjkc[(plantDate.yr[1,2]+pheno$Ph_ini+1):
                    (plantDate.yr[1,2]+pheno$Ph_ini+pheno$Ph_dev)] <- seq(from = adjKc_ini, to = adjKc_mid, length.out = pheno$Ph_dev)

    data.yr$adjkc[(plantDate.yr[1,2]+pheno$Ph_ini+pheno$Ph_dev+1):(plantDate.yr[1,2]+pheno$Ph_ini+pheno$Ph_dev+pheno$Ph_mid)] <- adjKc_mid

    data.yr$adjkc[(plantDate.yr[1,2]+pheno$Ph_ini+pheno$Ph_dev+pheno$Ph_mid+1):
                    (plantDate.yr[1,2]+pheno$Ph_ini+pheno$Ph_dev+pheno$Ph_mid+pheno$Ph_late)] <-
      seq(from = adjKc_mid, to = adjKc_end, length.out = pheno$Ph_late)



    plot(data.yr$tabkc, type = "l", lty=1, lwd = 3)
    lines(data.yr$adjkc, lty = 2, lwd = 3, col = "red")

    data.yr$CropWR <- NA
    data.yr$CropWR <- data.yr$adjkc * data.yr$Eto

    data.WR <- rbind(data.WR, data.yr)

  }

######################################################################################################################################################

  return(data.WR)

}

######################################################################################################################################################
######################################################################################################################################################
######################################################################################################################################################
