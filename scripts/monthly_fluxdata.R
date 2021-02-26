rm(list = ls())

library(lubridate)


Fluxdata <- readRDS("/home/femeunier/Downloads/Fluxtower.RDS")
Fluxdata <- Fluxdata %>% mutate(date = as.Date(as.yearmon(time, format = '%Y%m')),
                                year = year(date),
                                month = month(date)) %>% group_by(year,month) %>% mutate(GPP.month = mean(GPP),
                                                                                         Reco.month = mean(Reco),
                                                                                         NEP.month = mean(NEP))

Fluxdata.month <- Fluxdata %>% group_by(year,month) %>% slice_head(n = 1) %>% select(-c(GPP,Reco,NEP)) %>%
  rename(GPP = GPP.month,
         NEP = NEP.month,
         Reco = Reco.month)

saveRDS(object = Fluxdata.month,file = "/home/femeunier/Documents/projects/Hackaton/LidarED/data/Fluxdata.month.RDS")

ggplot(data = Fluxdata) +
  geom_line(aes(x = time, y = GPP)) +
  geom_line(aes(x = time, y = GPP.month)) +
  theme_bw()

