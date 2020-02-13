rm(list = ls())

library(dplyr)

data.file <- "/home/carya/data/Wytham/WythamWoodsFluxMicromet_v1.0.csv"
flux.data <- read.csv(data.file)

plot(flux.data %>% filter(Tau > -9990) %>% pull(Tau),type='l')
plot(flux.data %>% filter(H > -9990) %>% pull(H),type='l')
