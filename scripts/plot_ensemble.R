rm(list = ls())

library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)

fluxtower.data <- readRDS("/home/femeunier/Downloads/Fluxtower.RDS") %>% mutate(date = as.Date(as.yearmon(time, format = '%Y%m')),
                                                                                year = year(date),
                                                                                month = month(date))

fluxtower.data.month <- fluxtower.data %>% group_by(month) %>% summarise(GPPm = mean(GPP,na.rm = TRUE),
                                                                         GPPsd = sd(GPP,na.rm = TRUE))

unit.conversion <- 1000 / 12 / 86400 * 1e6 / 365
system2("rsync",c("-avz","hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/df.Wytham.ensemble.RDS",
                file.path("/home/femeunier/Documents/projects/Hackaton/LidarED/","outputs","df.Wytham.ensemble.RDS")))

df.Wytham.ensemble <- readRDS(file.path("/home/femeunier/Documents/projects/Hackaton/LidarED/","outputs","df.Wytham.ensemble.RDS")) %>%
  filter(year <= 2012, year >= 2007) %>%
  mutate(time = year + (month - 1)/12,
         GPP = unit.conversion*GPP)

ggplot(data = df.Wytham.ensemble) +
  geom_line(aes(x = time,y = LAI, group = simu.num, color = as.factor(IC), linetype = as.factor(Npft))) +
  facet_grid(is_TRYparam~is_TLSallom) +
  theme_bw()

ggplot(data = df.Wytham.ensemble) +
  geom_line(aes(x = time,y = AGB, group = simu.num, color = as.factor(IC), linetype = as.factor(Npft))) +
  facet_grid(is_TRYparam~is_TLSallom) +
  theme_bw()

df.Wytham.ensemble.year <- df.Wytham.ensemble %>% group_by(simu.num,month) %>%
  summarise(gpp.m = mean(GPP,na.rm = TRUE),
            gpp.sd = sd(GPP,na.rm = TRUE),
            IC = IC[1],
            is_TRYparam = is_TRYparam[1],
            is_TLSallom = is_TLSallom[1],
            Npft = Npft[1],
            .groups = "keep")

ggplot(data = df.Wytham.ensemble.year) +
  geom_line(aes(x = month,y = gpp.m, group = simu.num, color = as.factor(IC), linetype = as.factor(Npft))) +
  geom_point(data = fluxtower.data.month,
             aes(x = month, y = GPPm)) +
  geom_errorbar(data = fluxtower.data.month,
             aes(x = month, ymin = GPPm - GPPsd, ymax = GPPm + GPPsd),width = 0) +
  facet_grid(is_TRYparam~is_TLSallom) +
  theme_bw()


#################################################################################################################

df.Wytham.ensemble.year %>% left_join(fluxtower.data.month %>% dplyr::select(month,GPPm),
                                      by = "month") %>%
  group_by(simu.num) %>% summarise(SSE = sum((gpp.m - GPPm)**2),
                                   IC = IC[1],
                                   is_TRYparam = is_TRYparam[1],
                                   is_TLSallom = is_TLSallom[1],
                                   Npft = Npft[1]) %>% arrange((SSE))

