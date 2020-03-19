rm(list = ls())

library(ncdf4)
library(dplyr)
library(lubridate)
library(purrr)
library(ggplot2)
library(RColorBrewer)
library(LidarED)
library(pracma)
library(gridExtra)

years <- 2007:2009
local.dir <- c("~/data/Wytham/Fluxes/reference/",
               "~/data/Wytham/Fluxes/reference_noconfig/",
               "~/data/Wytham/Fluxes/Hmean/",
               "~/data/Wytham/Fluxes/CA/",
               "~/data/Wytham/Fluxes/Hmax/",
               "~/data/Wytham/Fluxes/AGB/",
               "~/data/Wytham/Fluxes/Bl/",
               "~/data/Wytham/Fluxes/Hmean_CA_AGB_Bl/",
               "~/data/Wytham/Fluxes/Hmax_CA_AGB_Bl/")

flux.model.all <- data.frame()

flux.data <- readRDS(file = file.path(getwd(),"data","Fluxtower.RDS"))

for (idir in seq(1,length(local.dir))){
  flux.model <- read.fluxes(local.dir[idir],years)

  flux.model_interp <- data.frame(time = flux.data$time,
                                  GPP.mod = interp1(flux.model$t,flux.model$GPP,flux.data$time),
                                  NEP.mod = interp1(flux.model$t,flux.model$NEP,flux.data$time),
                                  Reco.mod = interp1(flux.model$t,flux.model$Reco,flux.data$time)) %>% mutate(simulation = basename(local.dir[idir]))

  flux.model.all <- rbind(flux.model.all,
                          as.data.frame(flux.model_interp))
}


# Interpolation

reference <- flux.model.all %>% filter(simulation == "reference")
reference_noconfig <- flux.model.all %>% filter(simulation == "reference_noconfig")
Lidar <- flux.model.all %>% filter(simulation == "Hmax_CA_AGB_Bl")

all_simus <- flux.model.all %>% filter(!(simulation %in% c("reference","reference_noconfig"))) %>% group_by(time) %>%
  summarise(GPP_min = min(GPP.mod),
            GPP_max = max(GPP.mod),
            NEP_min = min(NEP.mod),
            NEP_max = max(NEP.mod),
            Reco_min = min(Reco.mod),
            Reco_max = max(Reco.mod))

A <- ggplot() +
  geom_ribbon(data = all_simus,aes(x = time,ymin = GPP_min, ymax = GPP_max), fill = "grey70",alpha = 0.5) +
  geom_line(data = flux.data,aes(x = time,y = GPP),col='black') +
  geom_line(data = reference,aes(x = time,y = GPP.mod),col='blue',linetype = 2) +
  geom_line(data = Lidar,aes(x = time,y = GPP.mod),col='red',linetype = 2) +
  geom_line(data = reference_noconfig,aes(x = time,y = GPP.mod),col='black',linetype = 3) +
  scale_x_continuous(expand = c(0,0)) +
  theme_bw()

B <-  ggplot() +
  geom_ribbon(data = all_simus,aes(x = time,ymin = Reco_min, ymax = Reco_max), fill = "grey70",alpha = 0.5) +
  geom_line(data = flux.data,aes(x = time,y = Reco),col='black') +
  geom_line(data = reference,aes(x = time,y = Reco.mod),col='blue',linetype = 2) +
  geom_line(data = Lidar,aes(x = time,y = Reco.mod),col='red',linetype = 2) +
  geom_line(data = reference_noconfig,aes(x = time,y = Reco.mod),col='black',linetype = 3) +
  scale_x_continuous(expand = c(0,0)) +
  theme_bw()

C <-  ggplot() +
  geom_ribbon(data = all_simus,aes(x = time,ymin = NEP_min, ymax = NEP_max), fill = "grey70",alpha = 0.5) +
  geom_line(data = flux.data,aes(x = time,y = NEP),col='black') +
  geom_line(data = reference,aes(x = time,y = NEP.mod),col='blue',linetype = 2) +
  geom_line(data = Lidar,aes(x = time,y = NEP.mod),col='red',linetype = 2) +
  geom_line(data = reference_noconfig,aes(x = time,y = NEP.mod),col='black',linetype = 3) +
  scale_x_continuous(expand = c(0,0)) +
  theme_bw()

grid.arrange(A, B, C, nrow = 3)

data.subplotA <- cbind(
  rbind(flux.data %>% mutate(mod = GPP,var = "GPP") %>% select(time,mod,var),
        flux.data %>% mutate(mod = Reco,var = "Reco") %>% select(time,mod,var),
        flux.data %>% mutate(mod = NEP,var = "NEP") %>% select(time,mod,var)),
  rbind(reference_noconfig %>% mutate(obs = GPP.mod) %>% select(obs),
        reference_noconfig %>% mutate(obs = Reco.mod) %>% select(obs),
        reference_noconfig %>% mutate(obs = NEP.mod) %>% select(obs)))


A <- ggplot(data = data.subplotA) +
  geom_point(aes(x = obs,y = mod,color = as.factor(var),fill = as.factor(var)),shape = 1) +
  scale_x_continuous(limits = c(-10,15),name = "Observed") +
  scale_y_continuous(limits = c(-10,15),name = "Simulated") +
  geom_abline(slope = 1, intercept = 0,linetype=2,colour = 'black') +
  scale_color_brewer(palette = "Set2") +
  theme_bw() + theme(legend.position = "none")

summary(lm(data = left_join(flux.data,reference_noconfig,by = "time"),GPP.mod ~ GPP))
summary(lm(data = left_join(flux.data,reference_noconfig,by = "time"),NEP.mod ~ NEP))
summary(lm(data = left_join(flux.data,reference_noconfig,by = "time"),Reco.mod ~ Reco))


data.subplotB <- cbind(
  rbind(flux.data %>% mutate(mod = GPP,var = "GPP") %>% select(time,mod,var),
        flux.data %>% mutate(mod = Reco,var = "Reco") %>% select(time,mod,var),
        flux.data %>% mutate(mod = NEP,var = "NEP") %>% select(time,mod,var)),
  rbind(reference %>% mutate(obs = GPP.mod) %>% select(obs),
        reference %>% mutate(obs = Reco.mod) %>% select(obs),
        reference %>% mutate(obs = NEP.mod) %>% select(obs)))

B <- ggplot(data = data.subplotB) +
  geom_point(aes(x = obs,y = mod,color = as.factor(var),fill = as.factor(var)),shape = 1) +
  scale_x_continuous(limits = c(-10,15)) +
  scale_y_continuous(limits = c(-10,15)) +
  geom_abline(slope = 1, intercept = 0,linetype=2,colour = 'black') +
  scale_color_brewer(palette = "Set2") +
  theme_bw() + theme(legend.position = "none")

summary(lm(data = left_join(flux.data,reference,by = "time"),GPP.mod ~ GPP))
summary(lm(data = left_join(flux.data,reference,by = "time"),NEP.mod ~ NEP))
summary(lm(data = left_join(flux.data,reference,by = "time"),Reco.mod ~ Reco))

data.subplotC <- cbind(
  rbind(flux.data %>% mutate(mod = GPP,var = "GPP") %>% select(time,mod,var),
        flux.data %>% mutate(mod = Reco,var = "Reco") %>% select(time,mod,var),
        flux.data %>% mutate(mod = NEP,var = "NEP") %>% select(time,mod,var)),
  rbind(Lidar %>% mutate(obs = GPP.mod) %>% select(obs),
        Lidar %>% mutate(obs = Reco.mod) %>% select(obs),
        Lidar %>% mutate(obs = NEP.mod) %>% select(obs)))

C <- ggplot(data = data.subplotC) +
  geom_point(aes(x = obs,y = mod,color = as.factor(var),fill = as.factor(var)),shape = 1) +
  scale_x_continuous(limits = c(-10,15)) +
  scale_y_continuous(limits = c(-10,15)) +
  geom_abline(slope = 1, intercept = 0,linetype=2,colour = 'black') +
  scale_color_brewer(palette = "Set2") +
  theme_bw() +  theme(legend.position = c(.12, .9))

summary(lm(data = left_join(flux.data,Lidar,by = "time"),GPP.mod ~ GPP))
summary(lm(data = left_join(flux.data,Lidar,by = "time"),NEP.mod ~ NEP))
summary(lm(data = left_join(flux.data,Lidar,by = "time"),Reco.mod ~ Reco))

grid.arrange(A, B, C, nrow = 1)


