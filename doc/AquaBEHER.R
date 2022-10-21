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

 data(climateData)
 str(climateData)
 head(climateData)
 

## -----------------------------------------------------------------------------

PET.daily <- calcEto(data = climateData)

str(PET.daily)


## -----------------------------------------------------------------------------

climateData$Eto <- PET.daily$ET.Daily

soilWHC = 100
 
watBal.daily <- calcWatBal(climateData, soilWHC)

str(watBal.daily )


## -----------------------------------------------------------------------------


soilWHC = 100
 onsetWind.start = "1980-09-01"
 onsetWind.end = "1981-01-31"

seasCal.dF <- calcSeasCal(watBal.daily, onsetWind.start, onsetWind.end,
                          e_thresh = 0.25, AW_thr = 10, soilWHC)

str(seasCal.dF)


 watBal.daily.81 <- watBal.daily[watBal.daily$Year %in% c(1980, 1981),]
 date.vec <- as.Date.character(paste0(watBal.daily.81$Year, "-", watBal.daily.81$Month, "-", watBal.daily.81$Day))

 ggplot() +
         geom_line(aes(y = watBal.daily.81$AVAIL, x = date.vec), size = 0.8, color = "grey30") +
         geom_area(aes(y = watBal.daily.81$Rain, x = date.vec), fill = "blue", size = 0.8, alpha = 0.7) +

         scale_x_date(date_breaks = "1 month", date_labels =  "%b-%Y") +
   
         scale_y_continuous(expand = c(0, 2))  +
         labs(y="Moisture (mm)", x=NULL) +
   
         theme_linedraw()  +
   
   theme(axis.title = element_text(size = 14, colour = "black", family = "Times New Roman"),
         axis.text = element_text(size = 10, colour = "black", family = "Times New Roman"),
         axis.text.x = element_text(size = 10, colour = "black", family = "Times New Roman", angle = 45, vjust = 0.5))


