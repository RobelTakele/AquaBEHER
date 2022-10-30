## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----AquaBEHER setup----------------------------------------------------------

# install.packages("devtools")
# devtools::install_github("RobelTakele/AquaBEHER")

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

# Add the estimated PET 'ET.Daily' to a new column in AgroClimateData:
 AgroClimateData$Eto <- PET$ET.Daily

# Estimate daily water balance for the soil having 100mm of WHC:
soilWHC = 100
 
watBal<- calcWatBal(AgroClimateData, soilWHC)

str(watBal )

# Plotting the water balance output for the climatological year from 2019 to 2020 using ggplot2:

 watBal.19T20 <- watBal[watBal$Year %in% c(2019, 2020),]
 date.vec <- as.Date.character(paste0(watBal.19T20$Year, "-", watBal.19T20$Month, "-", watBal.19T20$Day))

 ggplot() +
         geom_line(aes(y = watBal.19T20$AVAIL, x = date.vec), size = 0.8, color = "grey30") +
         geom_area(aes(y = watBal.19T20$Rain, x = date.vec), fill = "blue", size = 0.8, alpha = 0.7) +

         scale_x_date(date_breaks = "1 month", date_labels =  "%b-%Y") +
   
         scale_y_continuous(expand = c(0, 2))  +
         labs(y="Moisture (mm)", x=NULL) +
   
         theme_linedraw()  +
   
   theme(axis.title = element_text(size = 14, colour = "black", family = "Times New Roman"),
         axis.text = element_text(size = 10, colour = "black", family = "Times New Roman"),
         axis.text.x = element_text(size = 10, colour = "black", family = "Times New Roman", angle = 45, vjust = 0.5))


## -----------------------------------------------------------------------------
# seasonal calndar is estimated for the onset window ranges from 01-September to 31-January having a soil with 100mm of WHC
 soilWHC = 100
 onsetWind.start = "2019-09-01"
 onsetWind.end = "2020-01-31"

seasCal.dF <- calcSeasCal(watBal, onsetWind.start, onsetWind.end,
                          e_thresh = 0.25, AW_thr = 10, soilWHC)

str(seasCal.dF)

# plotting year to year variation of onset cessation and seasonal duration

 ggplot(data = seasCal.dF) +
   geom_line(aes(y = Onset.DOY, x = Year, color = "Onset.DOY"), size = 2) +
   geom_line(aes(y = Cesation.DOY, x = Year, color = "Cesation.DOY"), size = 2) +
   geom_col(aes(y = SeasDur, x = Year, color = "SeasDur"), size = 0.8, alpha = 0.4) +

   scale_color_manual(name = "Calendar", values = c('Onset.DOY' = "blue",
                                                    'Cesation.DOY' = "red",
                                                    'SeasDur' = "grey"))  +

 labs(y="Day of a year (DOY)", x=NULL) +

   theme_bw()



