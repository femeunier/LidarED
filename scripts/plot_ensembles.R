rm(list = ls())

library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)
library(tidyr)

fluxtower.data <- readRDS("/home/femeunier/Downloads/Fluxtower.RDS") %>% mutate(date = as.Date(as.yearmon(time, format = '%Y%m')),
                                                                                year = year(date),
                                                                                month = month(date))

fluxtower.data.month <- fluxtower.data %>% group_by(year,month) %>% summarise(GPPm = mean(GPP,na.rm = TRUE),
                                                                              GPPsd = sd(GPP,na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(month) %>%
  summarise(GPPmean = mean(GPPm,na.rm = TRUE),
            GPPsd = sd(GPPm,na.rm = TRUE),
            GPPmin = GPPmean - 1.96*GPPsd/sqrt(length(GPPm)),
            GPPmax = GPPmean + 1.96*GPPsd/sqrt(length(GPPm)))

fluxtower.data.month <- fluxtower.data %>%
  group_by(month) %>%
  summarise(GPPmean = mean(GPP,na.rm = TRUE),
            GPPmin = quantile(GPP,probs = 0.025,na.rm = TRUE),
            GPPmax = quantile(GPP,probs = 0.975,na.rm = TRUE))

unit.conversion <- 1000 / 12 / 86400 * 1e6 / 365

system2("rsync",c("-avz","hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/df.Wytham.ensembles.RDS",
                  file.path("/home/femeunier/Documents/projects/Hackaton/LidarED/","outputs","df.Wytham.ensembles.RDS")))

system2("rsync",c("-avz","hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/Wytham/ensembles/run/Census_noTRY_1/inputs.ensembles.RDS",
                  file.path("/home/femeunier/Documents/projects/Hackaton/LidarED/","outputs","inputs.ensembles.RDS")))

df.inputs <- readRDS(file.path("/home/femeunier/Documents/projects/Hackaton/LidarED/","outputs","inputs.ensembles.RDS"))

df.inputs %>% filter(trait == "water_conductance",
                     pft == "LH") %>% group_by(type) %>% summarise(N = length(trait))

df.inputs %>% filter(trait == "water_conductance",
                     pft == "LH") %>% group_by(type,isTRY,Npft) %>% summarise(N = length(trait))

df.inputs %>% filter(trait == "water_conductance",
                     pft == "LH",
                     isTLS == FALSE) %>% group_by(crownmod,canrad,plasticity) %>% summarise(N = length(trait))

df.Wytham.ensemble <- readRDS(file.path("/home/femeunier/Documents/projects/Hackaton/LidarED/","outputs","df.Wytham.ensembles.RDS")) %>%
  filter(!is.na(month)) %>%
  mutate(time = year + (month - 1)/12,
         GPP = unit.conversion*GPP)


df.Wytham.ensemble.inputs <- df.Wytham.ensemble %>% left_join(df.inputs,
                                                              by = c("name","type","num")) %>%
  filter(!(crownmod == 0 & type == "TLS"))

weird.runs <- df.Wytham.ensemble.inputs %>% filter(type == "TLS",isTRY==1,year ==2011, month == 1, AGB < 15) %>% pull(num) %>% unique()
# weird.runs <- c()

df.Wytham.ensemble.inputs <- df.Wytham.ensemble.inputs %>% filter(!(type == "TLS" & isTRY==1 & num %in% weird.runs))

df.Wytham.ensemble.inputs %>% filter(trait == "water_conductance",
                                     pft == "LH",
                                     year == 2007,
                                     month == 1) %>% group_by(type) %>% summarise(N = length(trait))

df.Wytham.ensemble.inputs %>% filter(trait == "water_conductance",
                                     pft == "LH",
                                     year == 2007, month == 1) %>% group_by(type,isTRY,Npft) %>% summarise(N = length(trait))

df.Wytham.ensemble.inputs.yearly.num <- df.Wytham.ensemble.inputs %>% filter(trait == "q",pft == "LH") %>%
  group_by(type,month,isTRY,num) %>% summarise(gpp.m = mean(GPP),
                                                    lai.m = mean(LAI),
                                               par.m = mean(PAR),
                                                    agb = AGB[length(AGB)],
                                                    gpp.sd = sd(GPP),
                                                    gpp.min = quantile(GPP,0.025),
                                                    gpp.min.se = mean(GPP) - 1.96*sd(GPP)/sqrt(length(unique(num))),
                                                    gpp.max = quantile(GPP,0.975),
                                                    gpp.max.se = mean(GPP) + 1.96*sd(GPP)/sqrt(length(unique(num))),
                                                    .groups = "keep")

df.Wytham.ensemble.inputs.yearly <- df.Wytham.ensemble.inputs.yearly.num %>%
  group_by(type,month,isTRY) %>% summarise(gpp.mean = mean(gpp.m),
                                                gpp.sd = sd(gpp.m),
                                                gpp.min = quantile(gpp.m,0.025),
                                                gpp.min.se = mean(gpp.m) - 1.96*sd(gpp.m)/sqrt(length(unique(num))),
                                                gpp.max = quantile(gpp.m,0.975),
                                                gpp.max.se = mean(gpp.m) + 1.96*sd(gpp.m)/sqrt(length(unique(num))),

                                           lai.mean = mean(lai.m),
                                           lai.sd = sd(lai.m),
                                           lai.min = quantile(lai.m,0.025),
                                           lai.min.se = mean(lai.m) - 1.96*sd(lai.m)/sqrt(length(unique(num))),
                                           lai.max = quantile(lai.m,0.975),
                                           lai.max.se = mean(lai.m) + 1.96*sd(lai.m)/sqrt(length(unique(num))),

                                           par.mean = mean(par.m),
                                           par.sd = sd(par.m),
                                           par.min = quantile(par.m,0.025),
                                           par.min.se = mean(par.m) - 1.96*sd(par.m)/sqrt(length(unique(num))),
                                           par.max = quantile(par.m,0.975),
                                           par.max.se = mean(par.m) + 1.96*sd(par.m)/sqrt(length(unique(num))),
                                                .groups = "keep")




df.Wytham.ensemble.inputs.sum <- df.Wytham.ensemble.inputs %>% filter(trait == "q",pft == "LH") %>%
  group_by(type,time,isTRY) %>% summarise(gpp.m = mean(GPP),
                                               gpp.min = quantile(GPP,0.025),
                                               gpp.max = quantile(GPP,0.975),
                                               gpp.sd = sd(GPP),
                                               lai.m = mean(LAI),
                                               lai.sd = sd(LAI),
                                               agb.m = mean(AGB),
                                               agb.sd = sd(AGB),
                                               .groups = "keep")


df.Wytham.ensemble.inputs.yearly$type <- factor(df.Wytham.ensemble.inputs.yearly$type ,levels = c("NBG","Census","TLS"))
df.Wytham.ensemble.inputs.sum$type <- factor(df.Wytham.ensemble.inputs.sum$type ,levels = c("NBG","Census","TLS"))

# ggplot(data = df.Wytham.ensemble.inputs.sum) +
#   geom_line(data = df.Wytham.ensemble.inputs,
#             aes(x = time, y = AGB, color = as.factor(Npft), group = interaction(as.factor(Npft),num)),alpha = 0.2) +
#   geom_ribbon(aes(x = time,y = agb.m, ymin = agb.m - agb.sd, ymax = agb.m + agb.sd,
#                   group = as.factor(Npft), fill = as.factor(Npft)),color = NA,alpha = 0.5) +
#   geom_line(aes(x = time,y = agb.m,group = as.factor(Npft), color = as.factor(Npft)),alpha = 0.5) +
#   facet_wrap(isTRY~type,scales = "free_x") +
#   scale_y_continuous(limits = c(0,40)) +
#   theme_bw()

# ggplot(data = df.Wytham.ensemble.inputs.sum %>% filter(type != "NBG")) +
#   geom_line(data = df.Wytham.ensemble.inputs %>% filter(type != "NBG"),
#             aes(x = time, y = LAI, color = as.factor(Npft), group = interaction(as.factor(Npft),num)),alpha = 0.2) +
#   geom_ribbon(aes(x = time,y = lai.m, ymin = lai.m - lai.sd, ymax = lai.m + lai.sd,
#                   group = as.factor(Npft), fill = as.factor(Npft)),color = NA,alpha = 0.5) +
#   geom_line(aes(x = time,y = lai.m,group = as.factor(Npft), color = as.factor(Npft)),alpha = 0.5) +
#   geom_hline(yintercept = 3.6, color = 'red') +
#   geom_hline(yintercept = 4.1, color = 'red') +
#   facet_wrap(isTRY~type,scales = "free_x") +
#   theme_bw()


# df.Wytham.ensemble.inputs.sum %>%
#   mutate(month = round(1 + 12*(time - floor(time))),
#          year = floor(time)) %>%
#   group_by(isTRY,type,Npft) %>%
#   summarise(LAI = mean(lai.m),
#             .groups = "keep")

df.Wytham.ensemble.inputs.yearly$isTRY <- as.factor(df.Wytham.ensemble.inputs.yearly$isTRY)
levels(df.Wytham.ensemble.inputs.yearly$isTRY) <- c("False","True")
df.Wytham.ensemble.inputs.yearly.num$isTRY <- as.factor(df.Wytham.ensemble.inputs.yearly.num$isTRY)
levels(df.Wytham.ensemble.inputs.yearly.num$isTRY) <- c("False","True")


df.Wytham.ensemble.inputs.yearly %>% group_by(type,isTRY) %>%
  filter(month %in% seq(6,6)) %>%
  summarise(mean(gpp.sd))

fluxtower.data.month %>% filter(month %in% seq(5,10)) %>%
  summarise(mean(GPPmean))

df.Wytham.ensemble.inputs.yearly %>% filter(month == 1) %>%
  group_by(type) %>%
  summarise(mean(gpp.mean))

fluxtower.data.month %>% filter(month == 1)

df.Wytham.ensemble.inputs.yearly %>%
  group_by(type,isTRY,month) %>%
  summarise(m = mean(gpp.mean)) %>%
  left_join(fluxtower.data.month,
            by = "month") %>%
  group_by(type) %>%
  summarise(r = summary(lm(formula = m ~ GPPmean))[["adj.r.squared"]],
            b = coef(lm(formula = m ~ GPPmean))[2])

df.Wytham.ensemble.inputs.yearly %>%
  group_by(type,month) %>%
  summarise(m = mean(gpp.mean)) %>%
  left_join(fluxtower.data.month,
            by = "month") %>%
  group_by(type) %>%
  mutate(res = GPPmean - m) %>%
  summarise(RMSE = sqrt(sum(res**2)/12))


LAI <- df.Wytham.ensemble.inputs.yearly.num %>% filter(type == "NBG",
                                                isTRY == "False",
                                                month == 6) %>% pull(lai.m) %>% sort()

length(which(LAI > 7))

LAI <- df.Wytham.ensemble.inputs.yearly.num %>% filter(type == "TLS",
                                                       isTRY == "True",
                                                       month == 6) %>% pull(lai.m) %>% sort()

LAI <- df.Wytham.ensemble.inputs.yearly.num %>% filter(type == "TLS",
                                                       isTRY == "True",
                                                       month == 6) %>% pull(gpp.m) %>% sort()


ggplot(data = df.Wytham.ensemble.inputs.yearly) +
  geom_line(data = df.Wytham.ensemble.inputs.yearly.num,
            aes(x = month, y = gpp.m, group = num),
            size = 0.3,
            color = "darkgrey",
            alpha = 0.3) +
  geom_ribbon(aes(x = month,y = gpp.mean, ymin = gpp.min, ymax = gpp.max),
              color = NA,
              fill = "#1b9e77",
              alpha = 0.25) +
  geom_line(aes(x = month, y = gpp.mean),
            color = "#1b9e77",
            size = 1) +
  geom_point(data = fluxtower.data.month,
             aes(x = month, y = GPPmean)) +
  geom_errorbar(data = fluxtower.data.month,
                aes(x = month, ymin = GPPmin, ymax = GPPmax),width = 0) +
  scale_x_continuous(breaks = seq(1,12),
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  # scale_color_manual(values = c("#44CC29",'#1E64C8')) +
  # scale_fill_manual(values = c("#44CC29",'#1E64C8')) +
  labs(x = "", y = "", color = expression(N[PFT])) +
  facet_grid(isTRY~type,scales = "free_x") +
  theme_bw() +
  guides(fill = "none") +
  theme(legend.position = c(0.05,0.92),
        text = element_text(size = 24),
        strip.background = element_blank())

max(fluxtower.data.month$GPPmean) - min(fluxtower.data.month$GPPmean)

max(df.Wytham.ensemble.inputs.yearly.num %>% filter(type == "TLS",
                                                    month == 6,
                                                    isTRY == "True") %>% pull(gpp.m))

min(df.Wytham.ensemble.inputs.yearly.num %>% filter(type == "TLS",
                                                    month == 6,
                                                    isTRY == "True") %>% pull(gpp.m))

ggsave(plot = last_plot(),"/home/femeunier/Documents/projects/Hackaton/LidarED/Figures/Figure_ensemble.png",
       dpi = 300,width = 30,height = 18,units = "cm")


LAI.data <- data.frame(month = 6,
                       LAImin = 3.6,
                       LAImax = 4.1)



ggplot(data = df.Wytham.ensemble.inputs.yearly) +
  geom_line(data = df.Wytham.ensemble.inputs.yearly.num,
            aes(x = month, y = lai.m, group = num),
            size = 0.3,
            color = "darkgrey",
            alpha = 0.3) +
  geom_ribbon(aes(x = month,y = lai.mean, ymin = lai.min, ymax = lai.max),
              color = NA,
              fill = "#1b9e77",
              alpha = 0.25) +
  geom_line(aes(x = month, y = lai.mean),
            color = "#1b9e77",
            size = 1) +
  # geom_point(data = fluxtower.data.month,
  #            aes(x = month, y = GPPmean)) +
  geom_errorbar(data = LAI.data,
                aes(x = month, ymin = LAImin, ymax = LAImax),width = 0.2) +
  scale_x_continuous(breaks = seq(1,12),
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  # scale_color_manual(values = c("#44CC29",'#1E64C8')) +
  # scale_fill_manual(values = c("#44CC29",'#1E64C8')) +
  labs(x = "", y = "", color = expression(N[PFT])) +
  facet_grid(isTRY~type,scales = "free_x") +
  theme_bw() +
  guides(fill = "none") +
  theme(legend.position = c(0.05,0.92),
        text = element_text(size = 24),
        strip.background = element_blank())


ggsave(plot = last_plot(),"/home/femeunier/Documents/projects/Hackaton/LidarED/Figures/Figure_ensemble_LAI.png",
       dpi = 300,width = 30,height = 18,units = "cm")

ggplot(data = df.Wytham.ensemble.inputs.yearly) +
  geom_line(data = df.Wytham.ensemble.inputs.yearly.num,
            aes(x = month, y = par.m, group = num),
            size = 0.3,
            color = "darkgrey",
            alpha = 0.3) +
  geom_ribbon(aes(x = month,y = par.mean, ymin = par.min, ymax = par.max),
              color = NA,
              fill = "#1b9e77",
              alpha = 0.25) +
  geom_line(aes(x = month, y = par.mean),
            color = "#1b9e77",
            size = 1) +
  scale_x_continuous(breaks = seq(1,12),
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  labs(x = "", y = "", color = expression(N[PFT])) +
  facet_grid(isTRY~type,scales = "free_x") +
  theme_bw() +
  guides(fill = "none") +
  theme(legend.position = c(0.05,0.92),
        text = element_text(size = 24),
        strip.background = element_blank())


#######################################################################################################################
# Data vs Simulations

data.vs.simu <- df.Wytham.ensemble.inputs.yearly %>% left_join(fluxtower.data.month,
                                                               by = "month")

data.vs.simu$type <- factor(data.vs.simu$type,levels = c("NBG","Census","TLS"))

ggplot(data = data.vs.simu %>% filter(month %in% c(1:3,5:12)),
       aes(x = gpp.m, y = GPPm,
           color = as.factor(Npft),
           ymin = GPPm - GPPsd, ymax = GPPm + GPPsd,
           xmin = gpp.m - gpp.sd, xmax = gpp.m + gpp.sd)) +
  geom_point() +
  geom_errorbar() +
  geom_errorbarh() +
  stat_smooth(method = "lm",se = FALSE) +
  geom_abline(slope = 1, intercept = 0, color = "black",linetype = 2) +
  facet_wrap(isTRY~type) +
  theme_bw()

data.vs.simu %>% group_by(type,isTRY,Npft) %>% filter(month %in% c(1:3,5:12)) %>%
  summarise(summary(lm(formula = GPPm ~ gpp.m))[["r.squared"]],
            .groups = "keep")


#######################################################################################################################

df.Wytham.ensemble.inputs.yearly.num %>% filter(month == 6,type == "TLS",gpp.m < 2.5)


df.inputs <- readRDS(file.path("/home/femeunier/Documents/projects/Hackaton/LidarED/","outputs","inputs.ensembles.RDS"))

df.inputs.mut <- df.inputs %>%
  mutate(trait = as.character(trait)) %>%
  mutate(type.trait = case_when(trait %in% c("b1Bl_large","b1Bl_small","b2Bl_large","b2Bl_small",
                                             "b1Bs_large","b1Bs_small","b2Bs_large","b2Bs_small",
                                             "b1Ca","b2Ca",
                                             "b1Ht","b2Ht","hgt_ref","hgt_max") ~ "Allom",
                                TRUE ~ "Other"))

df.inputs.mut.wide <- df.inputs.mut %>% dplyr::select(-c(type.trait)) %>%
  pivot_wider(names_from = trait,
              values_from = value) %>%
  mutate(SLA = case_when(is.na(SLA) ~ 60/2,
                         TRUE ~ SLA)) %>%
  mutate(LA = b1Bl_small*SLA)


IO <- df.inputs.mut.wide %>% left_join(df.Wytham.ensemble.inputs.yearly.num %>% filter(month == 6) %>% ungroup() %>% dplyr::select(isTRY,type,agb,gpp.m,lai.m,num),
                                       by = c("type","num","isTRY"))



ggplot(data = IO %>% filter(pft == "LH")) +
  geom_point(aes(x =  interaction(crownmod),y = lai.m, color = as.factor(Npft))) +
  facet_wrap(isTRY~type,scales = "free") +
  theme_bw()



df2plot <- df.inputs %>% filter(type == "TLS",isTRY == 1)

ggplot(data = df2plot) +
  geom_density(aes(x = value)) +
  geom_point(data = df2plot,
             aes(x = value, y = 0), color = "black") +
  # geom_point(data = df2plot %>% filter(num %in% weird.runs,pft == "LH"),
  #            aes(x = value, y = 0), color = "red") +
  facet_wrap(~ trait,scales = "free") +
  theme_bw()

# View(df.inputs.mut.wide %>% filter(type == "Census",isTRY == 1,num %in% weird.runs,pft == "LH"))

crownmod.runs <- IO %>% filter(isTRY == 1,type == "TLS",crownmod == 1) %>% pull(num)

df.Wytham.ensemble.inputs.yearly <- df.Wytham.ensemble.inputs %>%
  filter((type == "TLS" & num %in% crownmod.runs) | (type %in% c("Census","NBG"))) %>%
  group_by(type,month,isTRY,Npft) %>%
  summarise(gpp.m = mean(GPP),
            gpp.sd = sd(GPP),
            lai.m = mean(LAI),
            lai.sd = sd(LAI),
            gpp.min = quantile(GPP,0.025),
            gpp.min.se = mean(GPP) - 1.96*sd(GPP)/sqrt(length(GPP)),
            gpp.max = quantile(GPP,0.975),
            gpp.max.se = mean(GPP) + 1.96*sd(GPP)/sqrt(length(GPP)),
            .groups = "keep")


df.Wytham.ensemble.inputs.yearly$type <- factor(df.Wytham.ensemble.inputs.yearly$type,
                                                levels = c("NBG","Census","TLS"))

ggplot(data = df.Wytham.ensemble.inputs.yearly ) +
  # geom_line(data = df.Wytham.ensemble.inputs.yearly.num %>% filter((type == "TLS" & num %in% crownmod.runs) | (type %in% c("Census","NBG"))),
  #           aes(x = month, y = gpp.m, color = as.factor(Npft), group = interaction(as.factor(Npft),num)),alpha = 0.2) +
  geom_ribbon(aes(x = month,y = gpp.m, ymin = gpp.min, ymax = gpp.max,
                  group = as.factor(Npft), fill = as.factor(Npft)),color = NA,alpha = 0.1) +
  geom_point(data = fluxtower.data.month,
             aes(x = month, y = GPPm)) +
  geom_errorbar(data = fluxtower.data.month,
                aes(x = month, ymin = GPPm - GPPsd, ymax = GPPm + GPPsd),width = 0) +
  facet_grid(isTRY~type,scales = "free_x") +
  theme_bw()


ggplot(data = df.Wytham.ensemble.inputs.yearly ) +
  geom_line(data = df.Wytham.ensemble.inputs.yearly.num %>% filter((type == "TLS" & num %in% crownmod.runs) | (type %in% c("Census","NBG"))),
            aes(x = month, y = lai.m, color = as.factor(Npft), group = interaction(as.factor(Npft),num)),alpha = 0.2) +
  geom_ribbon(aes(x = month,y = lai.m, ymin = lai.m - lai.sd, ymax = lai.m + lai.sd,
                  group = as.factor(Npft), fill = as.factor(Npft)),color = NA,alpha = 0.5) +
  facet_wrap(isTRY~type,scales = "free_x") +
  geom_hline(yintercept = 3.9, color = 'red') +
  theme_bw()

df.Wytham.ensemble.inputs %>% group_by(type,isTRY) %>%
  filter(month > 5 & month < 10,
         year < 2011) %>%
  summarise(LAI = mean(LAI),
            .groups = "keep")
