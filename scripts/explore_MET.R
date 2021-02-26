rm(list = ls())

library(dplyr)
library(ggplot2)
library(zoo)

file <- "~/Documents/projects/Hackaton/LidarED/data/COSMOS-UK_Hydrometeorological&Soil_2013-2017.csv"
data <- read.csv(file,stringsAsFactors = FALSE) %>% filter(SITE_ID == "WYTH1")

data.format <- data %>% mutate(DATE_TIME = as.POSIXct(DATE_TIME))
data.format[data.format==-9999] <- NA

vars <- c("LWIN","SWIN","PRECIP","RH","TA","WS","WD")
L <- c("Incoming Longwave radiation","Incoming shortwave radiation","Precipitations","Relative humidity","Air temp","Wind speed","Wind direction")

for (ivar in seq(1,length(vars))){
  ggplot(data = data.format,aes(x=DATE_TIME,y = get(vars[ivar]))) +
    geom_point(shape=1) +
    geom_line(aes(y=rollmean(get(vars[ivar]), 24*7*2, na.pad=TRUE,na.rm=TRUE)),color = "red") +
    labs(x = "",y = L[ivar]) +
    theme_bw()

  ggsave(filename = paste0("~/Documents/projects/Hackaton/LidarED/Figures/",vars[ivar],".png"),
         plot = last_plot(),dpi=300,height = 10,width=20,units = "cm")
}



