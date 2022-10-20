
<!-- README.md is generated from README.Rmd. Please edit that file -->

# AquaBEHER

<!-- badges: start -->
<!-- badges: end -->

The goal of AquaBEHER is to computes and integrates daily reference
evapotranspiration (Eto) into FAO56 water balance model. The AquaBEHER
package can estimate daily parameters of crop and soil water balances
parameters for agricultural crops. The package can also estimate rainy
season calandar (Onset, Cessation and Duration) based on agroclimatic
approach.

Specifically, the package can perform the following functions:

-   Estimation of daily evapotranspiration
-   Estimation of daily soil water balance
-   Estimation of rainy season calandar:
    -   Onset of the rainy season
    -   Cessation of the rainy season
    -   Duration of the rainy season

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
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
data(climateData)

head(climateData)
#>    Station_ID Station_Name   Lat    Lon Elev Year Month Day Rain Tmax Tmin
#> 1 MZ000067237      NAMPULA -15.1 39.283  441 1980     1   1  0.8 30.7 21.6
#> 2 MZ000067237      NAMPULA -15.1 39.283  441 1980     1   2  0.0 31.8 22.1
#> 3 MZ000067237      NAMPULA -15.1 39.283  441 1980     1   3  0.0 30.3 22.7
#> 4 MZ000067237      NAMPULA -15.1 39.283  441 1980     1   4 30.3 28.7 22.2
#> 5 MZ000067237      NAMPULA -15.1 39.283  441 1980     1   5 11.3 24.8 22.0
#> 6 MZ000067237      NAMPULA -15.1 39.283  441 1980     1   6 10.6 30.1 22.7
```

``` r

Eto.daily <- calcEto(climateData)
#> Hargreaves-Samani Reference Crop ET
#> Evaporative surface: reference crop
#> Timestep: daily
#> Units: mm
#> Time duration: 1980-01-01 to 1984-12-31
climateData$Eto <- Eto.daily$ET.Daily
soilWHC = 100
watBal.daily <- calcWatBal(climateData, soilWHC)
```

The output of daily soil water balance can be ploted:

<img src="man/figures/README-plot water balance-1.png" width="100%" />

<img align="right" width="300" src="http://www.capitalisegenetics.santannapisa.it/sites/default/files/u65/Logo%20plant%20sciences.png">

[![projects](https://img.shields.io/badge/projects-eu-003399)](profile/projects.md#International)

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
