## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----AquaBEHER setup----------------------------------------------------------
# Install packages if not already installed
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(knitr, rmarkdown, prettydoc, dplyr, ggplot2, lubridate, raster, sp, devtools)

# devtools::install_local("/path/to/AquaBEHER_0.1.0.tar.gz")

library(AquaBEHER)
library(ggplot2)

## ----climateData--------------------------------------------------------------
data(AgroClimateData)
str(AgroClimateData)
head(AgroClimateData)

## -----------------------------------------------------------------------------
PET <- calcEto(AgroClimateData, method = "PM", crop = "short")

str(PET)

## -----------------------------------------------------------------------------
# Compute Eto using hargreves-samani formulation using the example data from 'AgroClimateData':

data(AgroClimateData)

Eto.HS <- calcEto(AgroClimateData, method = "HS")

# Now compute Eto using Penman-Monteith formulation for hypothetical grass (short crop):

Eto.PM <- calcEto(AgroClimateData, method = "PM", Zh = 10)

plot(Eto.PM$ET.Daily[1:1000], ty = "l", xlab = "Days since 1996", ylab = "Eto (mm/day)", col = "black", lwd = 1, lty = 2)
lines(Eto.HS$ET.Daily[1:1000], col = "blue", lwd = 2, lty = 1)

legend("bottom", c("Eto: Penman–Monteith ", "Eto: Hargreaves-Samani"),
  horiz = TRUE, bty = "n", cex = 1, lty = c(2, 1), lwd = c(2, 2), inset = c(1, 1),
  xpd = TRUE, col = c("black", "blue")
)

## -----------------------------------------------------------------------------
PET <- calcEto(AgroClimateData, method = "PM", Zh = 10)

# Add the estimated PET 'ET.Daily' to a new column in AgroClimateData:
AgroClimateData$Eto <- PET$ET.Daily

# Estimate daily water balance for the soil having 100mm of WHC:
soilWHC <- 100

watBal <- calcWatBal(data = AgroClimateData, soilWHC)

str(watBal)

# Plotting the water balance output for the climatological year from 2019 to 2020 using ggplot2:

watBal.19T20 <- watBal[watBal$Year %in% c(2019, 2020), ]
date.vec <- as.Date.character(paste0(watBal.19T20$Year, "-", watBal.19T20$Month, "-", watBal.19T20$Day))

ggplot(data = watBal.19T20) +
  geom_line(aes(y = AVAIL, x = date.vec, fill = "AVAIL"), size = 0.8, color = "red") +
  geom_col(aes(y = Rain, x = date.vec, fill = "Rain"), size = 1) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y") +
  scale_fill_manual(name = " ", values = c("AVAIL" = "red", "Rain" = "blue")) +
  scale_y_continuous(expand = c(0, 2)) +
  labs(y = "Moisture (mm)", x = NULL) +
  theme_linedraw() +
  theme(
    axis.title = element_text(size = 14, colour = "black", family = "Times New Roman"),
    axis.text = element_text(size = 10, colour = "black", family = "Times New Roman"),
    axis.text.x = element_text(size = 10, colour = "black", family = "Times New Roman", angle = 45, vjust = 0.5)
  )

## -----------------------------------------------------------------------------
# seasonal calndar is estimated for the onset window ranges from 01-September to 31-January having a soil with 100mm of WHC
soilWHC <- 100
onsetWind.start <- "1996-09-01" # earliest possible start date of the onset window
onsetWind.end <- "1997-01-31" # the latest possible date for end of the onset window
cessaWind.end <- "1997-06-30" # the latest possible date for end of the cessation window

seasCal.lst <- calcSeasCal(watBal, onsetWind.start, onsetWind.end, cessaWind.end, soilWHC = 100)

str(seasCal.lst)

# plotting year to year variation of onset cessation and seasonal duration

seasCal.dF <- data.frame(
  Year = seasCal.lst[[1]][, c("Year")],
  Onset = seasCal.lst[[1]][, c("onset.JD")],
  Cessation = seasCal.lst[[2]][, c("cessation.JD")],
  Duration = seasCal.lst[[3]][, c("Duration")]
)

ggplot(data = seasCal.dF) +
  geom_line(aes(y = Onset, x = Year, color = "Onset"), size = 1) +
  geom_line(aes(y = Cessation, x = Year, color = "Cessation"), size = 1) +
  geom_area(aes(y = Duration, x = Year, color = "Duration⁠"), size = 0.8, alpha = 0.4) +
  scale_color_manual(name = "Calendar", values = c("Onset" = "blue", "Cessation" = "red", "Duration" = "grey")) +
  labs(y = "Day of a year (DOY)", x = NULL) +
  theme_bw()

