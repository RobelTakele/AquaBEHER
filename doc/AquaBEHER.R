## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo = TRUE,
  message = FALSE,
  warning = FALSE
)

## ----`AquaBEHER` setup--------------------------------------------------------
## Install required packages:
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(knitr, rmarkdown, prettydoc, dplyr, ggplot2, lubridate,
# terra, devtools, ggrepel, zoo)

## Install AquaBEHER from CRAN:
# install.packages("AquaBEHER")

## Install AquaBEHER from GitHub:
# devtools::install_github("RobelTakele/AquaBEHER")

library(AquaBEHER)
library(ggplot2)
library(ggrepel)
library(dplyr)

## ----climateData--------------------------------------------------------------
data(AgroClimateData)
str(AgroClimateData)
head(AgroClimateData)

## -----------------------------------------------------------------------------
PET <- calcEto(AgroClimateData, method = "PM", crop = "short")

str(PET)

## ----PETplot, fig.height = 4, fig.width = 6, fig.dpi = 300, fig.align = 'center'----
## Compute PET using Hargreaves-Samani formulation using the sample data f
## rom 'AgroClimateData':
Eto.HS <- calcEto(AgroClimateData, method = "HS")

## Now compute PET using Penman-Monteith formulation:
Eto.PM <- calcEto(AgroClimateData, method = "PM", Zh = 10)

plot(Eto.PM$ET.Daily[1:1000],
  type = "l", xlab = "Days since 1996",
  ylab = "Eto (mm/day)", col = "black", lwd = 1, lty = 2
)
lines(Eto.HS$ET.Daily[1:1000], col = "blue", lwd = 2, lty = 1)

legend("bottom", c("Eto: Penman–Monteith", "Eto: Hargreaves-Samani"),
  horiz = TRUE, bty = "n", cex = 1, lty = c(2, 1),
  lwd = c(2, 2), inset = c(1, 1),
  xpd = TRUE, col = c("black", "blue")
)

## ----WATBALplot, fig.height = 6, fig.width = 10, fig.dpi = 300, fig.align = 'center'----
PET <- calcEto(AgroClimateData, method = "PM", Zh = 10)

## Add the estimated PET 'ET.Daily' to a new column in AgroClimateData:
AgroClimateData$Eto <- PET$ET.Daily

## Estimate daily water balance for the soil having 100mm of soilWHC:
soilWHC <- 100

watBal.list <- calcWatBal(data = AgroClimateData, soilWHC)
watBal <- watBal.list$data

str(watBal)

## Filter the data for the years 2019 and 2020:
watBal.19T20 <- watBal[watBal$Year %in% c(2019, 2020), ]

## Create a date vector:
date.vec <- as.Date(paste0(
  watBal.19T20$Year, "-",
  watBal.19T20$Month, "-",
  watBal.19T20$Day
), format = "%Y-%m-%d")

## Add the date vector to the data frame:
watBal.19T20$date <- date.vec

## Plotting the water balance output for the climatological year
## from 2019 to 2020 using ggplot2:

library(ggplot2)
library(scales)

ggplot(data = watBal.19T20, aes(x = date)) +
  geom_bar(aes(y = Rain),
    stat = "identity", fill = "#1f78b4",
    alpha = 0.6, width = 0.8
  ) +
  geom_line(aes(y = AVAIL), color = "#33a02c", size = 1.5) +
  geom_line(aes(y = Eto),
    color = "#ff7f00", size = 1.2,
    linetype = "dashed"
  ) +
  scale_x_date(
    date_labels = "%b %Y", date_breaks = "1 month",
    expand = c(0.01, 0)
  ) +
  scale_y_continuous(
    name = "Available Soil Water (mm)",
    sec.axis = sec_axis(~., name = "Rainfall (mm)")
  ) +
  labs(
    title = "Rainfall, Available Soil Water and
       Potential Evapotranspiration",
    subtitle = "Data from 2019 to 2020",
    x = " ",
    y = " "
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "grey40"),
    axis.title.y = element_text(color = "#33a02c"),
    axis.title.y.right = element_text(color = "#1f78b4"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linetype = "dotted", color = "grey80")
  )

## ----WSC, fig.height = 6, fig.width = 10, fig.dpi = 300, fig.align = 'center'----
## The wet season calendar is estimated for the onset window ranges from
## 01-September to 31-January having a soil with 80mm of soilWHC:

data(AgroClimateData)

PET <- calcEto(AgroClimateData, method = "HS")
AgroClimateData$Eto <- PET$ET.Daily

soilWHC <- 80

watBal.list <- calcWatBal(data = AgroClimateData, soilWHC)
watBal <- watBal.list$data

onsetWind.start <- "10-01" ## earliest possible start date of the onset window
onsetWind.end <- "01-31" ## the latest possible date for end of the onset window
cessaWind.end <- "06-30" ## the latest possible date for end of the cessation window

seasCal.dF <- calcSeasCal(
  data = watBal, onsetWind.start,
  onsetWind.end, cessaWind.end, soilWHC
)

str(seasCal.dF)

seasCal.dF$OnsetDate <- as.Date(seasCal.dF$OnsetDate)
seasCal.dF$CessationDate <- as.Date(seasCal.dF$CessationDate)

max_onset <- max(seasCal.dF$OnsetValue, na.rm = TRUE)
max_cessation <- max(seasCal.dF$CessationValue, na.rm = TRUE)
max_value <- max(max_onset, max_cessation)



ggplot(seasCal.dF, aes(x = Year)) +
  geom_line(aes(y = OnsetValue, color = "Onset"),
    size = 1.5, linetype = "solid"
  ) +
  geom_line(aes(y = CessationValue, color = "Cessation"),
    size = 1.5, linetype = "dashed"
  ) +
  geom_point(aes(y = OnsetValue, color = "Onset"),
    size = 3,
    shape = 21, fill = "white"
  ) +
  geom_point(aes(y = CessationValue, color = "Cessation"),
    size = 3, shape = 21, fill = "white"
  ) +
  geom_text_repel(
    aes(
      y = OnsetValue,
      label = ifelse(!is.na(OnsetDate),
        format(OnsetDate, "%Y-%m-%d"), ""
      ),
      color = "Onset"
    ),
    size = 3,
    box.padding = 0.5, point.padding = 0.5
  ) +
  geom_text_repel(
    aes(
      y = CessationValue,
      label = ifelse(!is.na(CessationDate),
        format(CessationDate, "%Y-%m-%d"), ""
      ),
      color = "Cessation"
    ),
    size = 3,
    box.padding = 0.5, point.padding = 0.5
  ) +
  scale_y_continuous(
    name = paste0(
      "Days Since: ",
      format(
        as.Date(paste0(
          "2023-",
          onsetWind.start
        )),
        "%d %b"
      )
    ),
    breaks = seq(0, max_value, by = 10)
  ) +
  labs(
    title = "Onset and Cessation Dates of the Wet Season",
    x = " ", color = "Legend"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    legend.position = "top",
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 12),
    panel.grid.major = element_line(color = "#e0e0e0"),
    panel.grid.minor = element_line(color = "#f0f0f0"),
    panel.background = element_rect(fill = "#f7f7f7"),
    plot.background = element_rect(fill = "#f7f7f7")
  ) +
  scale_color_manual(
    name = "Legend",
    values = c(
      "Onset" = "#1f77b4",
      "Cessation" = "red"
    )
  )

## ----fcstWSC, fig.width = 8, fig.height = 5, fig.dpi = 300, fig.align = 'center'----
## Load example data:
data(AgroClimateData)

## Estimate daily PET:
PET <- calcEto(AgroClimateData, method = "PM", Zh = 10)

## Add the estimated PET 'ET.Daily' to a new column in AgroClimateData:
AgroClimateData$Eto <- PET$ET.Daily

## Estimate daily water balance for the soil having 100mm of WHC:
watBal.list <- calcWatBal(data = AgroClimateData, soilWHC = 100)
watBal <- watBal.list$data

## seasonal calendar is estimated for the onset window ranges from
## 01 September to 31 January having a soil with 100mm of WHC:

soilWHC <- 100
onsetWind.start <- "09-01"
onsetWind.end <- "01-31"
cessaWind.end <- "06-30"

seasCal.dF <- calcSeasCal(
  data = watBal, onsetWind.start, onsetWind.end,
  cessaWind.end, soilWHC
)

## Tercile Rainfall Probabilities of seasonal Forecast for OND, 2023:
rainTerc <- data.frame(T1 = 0.15, T2 = 0.10, T3 = 0.75)

## Summarize rainfall data for October to December:
seasRain <- AgroClimateData %>%
  filter(Month %in% c(10, 11, 12)) %>%
  group_by(Year) %>%
  summarize(sRain = sum(Rain))

## Start of the historical resampling year
hisYearStart <- 1991

## End of the historical resampling year
hisYearEnd <- 2022

## Historical WSC Simulations:
hisWSCvar <- seasCal.dF

## WSC variable to forecast:
fcstVarName <- "Onset"
tercileMethod <- "quantiles"

SeasFcst.dF <- seasFcstQBR(
  hisYearStart, hisYearEnd, rainTerc,
  seasRain, hisWSCvar, fcstVarName,
  tercileMethod
)


## Resafel the dataframe for ggplot:
SeasFcst.dFgg <- data.frame(
  Category = factor(
    c(
      "BelowNormal", "Normal",
      "AboveNormal"
    ),
    levels = c(
      "BelowNormal",
      "Normal",
      "AboveNormal"
    )
  ),
  Probability = c(
    (SeasFcst.dF$BelowNormal * 100),
    (SeasFcst.dF$Normal * 100),
    (SeasFcst.dF$AboveNormal * 100)
  )
)

## Create the bar plot:
ggplot(SeasFcst.dFgg, aes(x = Category, y = Probability, fill = Category)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_manual(values = c(
    "BelowNormal" = "#1f77b4",
    "Normal" = "#ff7f0e",
    "AboveNormal" = "#2ca02c"
  )) +
  geom_text(aes(label = paste0(Probability, "%")),
    vjust = -0.5,
    size = 4, fontface = "bold"
  ) +
  labs(
    title = "Seasonal Forecast of the Onset of the Wet Season",
    x = " ",
    y = "Probability (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.position = "none"
  )

