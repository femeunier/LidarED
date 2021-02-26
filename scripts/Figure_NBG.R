rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(zoo)

# system2("rsync",paste("-avz",
#                       "hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/Wytham/growth_storage_resp/out/near_bare_ground/analy/analysis.RData",
#                       "/home/femeunier/Documents/projects/Hackaton/LidarED/data/NBG.RData"))
#
# system2("rsync",paste("-avz",
#                       "hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/Wytham/growth_storage_resp/out/near_bare_ground_CA/analy/analysis.RData",
#                       "/home/femeunier/Documents/projects/Hackaton/LidarED/data/NBG_FC.RData"))
#
# system2("rsync",paste("-avz",
#                       "hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/Wytham/growth_storage_resp/out/near_bare_ground_Lidar/analy/analysis.RData",
#                       "/home/femeunier/Documents/projects/Hackaton/LidarED/data/NBG_TLS.RData"))


load("/home/femeunier/Documents/projects/Hackaton/LidarED/data/NBG_TLS.RData")
NBG_TLS <- datum
load("/home/femeunier/Documents/projects/Hackaton/LidarED/data/NBG_FC.RData")
NBG_FC <- datum
load("/home/femeunier/Documents/projects/Hackaton/LidarED/data/NBG.RData")
NBG <- datum

fac <- 1000 / 12 / 86400 * 1e6 / 365

df <- bind_rows(list(
  data.frame(year = NBG_TLS$year,
             month = NBG_TLS$month,
             gpp = NBG_TLS$emean$gpp*fac,
             lai = NBG_TLS$emean$lai,
             npp = NBG_TLS$emean$npp*fac,
             agb = NBG_TLS$emean$agb,
             Reco = NBG_TLS$emean$reco*fac,
             nep = NBG_TLS$emean$nep*fac,
             par.gnd = NBG_TLS$emean$par.gnd,
             type = "NBG_TLS"),
  data.frame(year = NBG_FC$year,
             month = NBG_FC$month,
             gpp = NBG_FC$emean$gpp*fac,
             lai = NBG_FC$emean$lai,
             npp = NBG_FC$emean$npp*fac,
             agb = NBG_FC$emean$agb,
             Reco = NBG_FC$emean$reco*fac,
             nep = NBG_FC$emean$nep*fac,
             par.gnd = NBG_FC$emean$par.gnd,
             type = "NBG_FC"),
  data.frame(year = NBG$year,
             month = NBG$month,
             gpp = NBG$emean$gpp*fac,
             lai = NBG$emean$lai,
             npp = NBG$emean$npp*fac,
             agb = NBG$emean$agb,
             Reco = NBG$emean$reco*fac,
             nep = NBG$emean$nep*fac,
             par.gnd = NBG$emean$par.gnd,
             type = "NBG")))

df_select <- df %>% group_by(type) %>% filter(year > (max(year) - 10))
df.month <- df_select %>% group_by(type,month) %>% summarise(gpp = mean(gpp),
                                                             npp = mean(npp),
                                                             lai = mean(lai),
                                                             agb = mean(agb),
                                                             Reco = mean(Reco),
                                                             par.gnd = mean(par.gnd),
                                                             nep = mean(nep))
df.long <- df.month %>% pivot_longer(cols = -c("type","month"),
                                     names_to = "var",
                                     values_to = "value")


# Leaf-on only
df_select %>% filter(month %in% seq(5,10)) %>% group_by(year,type) %>%summarise(lai = mean(lai),
                                                                                gpp = mean(gpp),
                                                                                par.gnd = mean(par.gnd)) %>%
  group_by(type) %>% summarise(lai_m = mean(lai),
                               lai_sd = sd(lai),
                               gpp_m = mean(gpp),
                               gpp_sd = sd(gpp),
                               par.gnd_m = mean(par.gnd),
                               par.gnd_sd = sd(par.gnd))

# All times
df_select  %>% group_by(year,type) %>%summarise(nep = mean(nep),
                                                Reco = mean(Reco),
                                                agb = mean(agb)) %>%
  group_by(type) %>% summarise(agb_m = mean(agb),
                               agb_sd = sd(agb),
                               Reco_m = mean(Reco),
                               Reco_sd = sd(Reco),
                               nep_m = mean(nep),
                               nep_sd = sd(nep))


ggplot(data = df.long) +
  geom_line(aes(x = month,y = value, color = as.factor(type))) +
  facet_wrap(~var,scales = "free") +
  theme_bw()

OP <- as.data.frame(bind_rows(list(
  readRDS(
    "/home/femeunier/Documents/projects/Hackaton/LidarED/data/Wytham.OP.RDS"
  ) %>% mutate(type = "IC-Census"),
  readRDS(
    "/home/femeunier/Documents/projects/Hackaton/LidarED/data/Wytham.OP2.RDS"
  ) %>% mutate(type = "IC-TLS"),
  readRDS(
    "/home/femeunier/Documents/projects/Hackaton/LidarED/data/Wytham.OP3.RDS"
  ) %>% mutate(type = "IC-FC")
)))

OP %>% filter((month %in% seq(5,10))) %>% group_by(year,type) %>%summarise(lai = mean(lai),
                                                                         gpp = mean(gpp)*fac,
                                                                         par.gnd = mean(par.gnd)) %>%
  group_by(type) %>% summarise(lai_m = mean(lai),
                               lai_sd = sd(lai),
                               gpp_m = mean(gpp),
                               gpp_sd = sd(gpp),
                               par.gnd_m = mean(par.gnd),
                               par.gnd_sd = sd(par.gnd))

OP  %>% group_by(year,type) %>%summarise(gpp = mean(gpp)*fac,
                                                nep = mean(nep)*fac,
                                                Reco = mean(reco)*fac,
                                                agb = mean(agb)) %>%
  group_by(type) %>% summarise(agb_m = mean(agb),
                               agb_sd = sd(agb),
                               Reco_m = mean(Reco),
                               Reco_sd = sd(Reco),
                               nep_m = mean(nep),
                               nep_sd = sd(nep))

data <- readRDS("/home/femeunier/Documents/projects/Hackaton/LidarED/data/Fluxdata.month.RDS") %>% mutate(date = as.Date(as.yearmon(time, format = '%Y%m')),
                                                                                                          year = year(date),
                                                                                                          month = month(date))

data %>% filter((month %in% seq(5,10))) %>% group_by(year,month) %>%summarise(gpp = mean(GPP)) %>% ungroup() %>%
  summarise(gpp_m = mean(gpp),
            gpp_sd = sd(gpp))

data %>% group_by(year,month) %>%summarise(nep = mean(NEP),
                                           Reco = mean(Reco)) %>% ungroup() %>%
  summarise(nep_m = mean(nep),
            nep_sd = sd(nep),
            Reco_m = mean(Reco),
            Reco_sd = sd(Reco))

