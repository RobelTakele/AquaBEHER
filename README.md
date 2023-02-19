
<!-- README.md is generated from README.Rmd. Please edit that file -->

# AquaBEHER

<!-- badges: start -->

[![R-CMD-check](https://github.com/RobelTakele/AquaBEHER/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/RobelTakele/AquaBEHER/actions/workflows/R-CMD-check.yaml)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Gitter
chat](https://badges.gitter.im/gitterHQ/gitter.png)](https://gitter.im/r-hub/community)
<!-- badges: end -->

The goal of AquaBEHER is to computes and integrates daily reference
evapotranspiration (Eto) into FAO56 water balance model. The AquaBEHER
package can estimate daily parameters of crop and soil water balances
parameters for agricultural crops. The package can also estimate rainy
season calandar (Onset, Cessation and Duration) based on agroclimatic
approach.

Specifically, the package can perform the following functions:

- Estimation of daily evapotranspiration
- Estimation of daily soil water balance
- Estimation of rainy season calandar:
  - Onset of the rainy season
  - Cessation of the rainy season
  - Duration of the rainy season

## Installation

You can install the development version of AquaBEHER from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("RobelTakele/AquaBEHER")
```

## Example

This is a basic example which shows you how to estimate daily water
balance:

``` r
library(AquaBEHER)
#> Loading required package: soiltexture
#> Loading required package: lubridate
#> 
#> Attaching package: 'lubridate'
#> The following objects are masked from 'package:base':
#> 
#>     date, intersect, setdiff, union
#> Loading required package: raster
#> Loading required package: sp
library(ggplot2)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
data(AgroClimateData)

head(AgroClimateData)
#>       Source      Lat     Lon  Elev Year Month Day  Rain  Tmax  Tmin    Rs
#> 1 NASAPOWER  -16.2163 39.9145 25.19 1996     1   1  4.27 32.76 24.39 27.03
#> 2 NASAPOWER  -16.2163 39.9145 25.19 1996     1   2 26.59 30.76 24.85 25.20
#> 3 NASAPOWER  -16.2163 39.9145 25.19 1996     1   3  9.63 31.33 24.66 24.61
#> 4 NASAPOWER  -16.2163 39.9145 25.19 1996     1   4  4.50 31.37 24.36 22.64
#> 5 NASAPOWER  -16.2163 39.9145 25.19 1996     1   5  2.85 29.88 23.66 23.92
#> 6 NASAPOWER  -16.2163 39.9145 25.19 1996     1   6  5.88 29.81 23.16 22.64
#>      RH  Tdew   U2
#> 1 69.81 21.26 1.32
#> 2 79.75 23.01 1.63
#> 3 77.81 22.78 1.82
#> 4 75.31 22.00 1.95
#> 5 74.44 21.00 1.73
#> 6 74.75 21.15 1.48
```

``` r

Eto.daily <- calcEto(AgroClimateData, method = "PM", crop = "short")
AgroClimateData$Eto <- Eto.daily$ET.Daily
soilWHC = 100
watBal <- calcWatBal(AgroClimateData, soilWHC)
```

The output of daily soil water balance can be ploted:

``` r

watBal <- watBal[watBal$Year %in% c(2010, 2020),]
date.vec <- as.Date.character(paste0(watBal$Year, "-", watBal$Month, "-", watBal$Day))

plot(watBal$AVAIL, ty="l", xlab="Days since 2010", ylab="Water (mm)", col="black", lwd = 1, lty = 2)
lines(watBal$Eto, col="red", lwd = 3)
lines(watBal$Rain, col="blue", lwd = 1)

   legend("bottom",c("Rain","Eto","Available Moisture"),
         horiz=TRUE, bty='n', cex=1,lty=c(1,1,2),lwd=c(2,2,2), inset=c(1,1),
         xpd=TRUE, col=c("blue","red","black"))
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

<img align="right" width="300" src="http://www.capitalisegenetics.santannapisa.it/sites/default/files/u65/Logo%20plant%20sciences.png">

The **Center of Plant Sciences Group** is a geographically and
culturally diverse research team working on climate and crop genetics at
**Scuola Superiore Sant’Anna**, Pisa, Italy.

You can contact us sending an email to Matteo Dell’Acqua
(<a href="mailto:m.dellacqua@santannapisa.it"
class="uri">mailto:m.dellacqua@santannapisa.it</a>) or Mario Enrico Pè
(<a href="mailto:m.pe@santannapisa.it"
class="uri">mailto:m.pe@santannapisa.it</a>). You can also visit the
crop genetics (<http://www.capitalisegenetics.santannapisa.it/>) web
page.

We are committed to the [free
software](https://www.fsf.org/about/what-is-free-software) and
[FAIR](https://www.go-fair.org/fair-principles/) principles. This set of
repositories collects our latest developments and provide reusable code.
