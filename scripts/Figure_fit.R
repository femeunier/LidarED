rm(list = ls())

library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)
library(cowplot)

# system2("rsync",paste("-avz",
#                       "hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/Wytham.OP.RDS",
#                       "/home/femeunier/Documents/projects/Hackaton/LidarED/data/Wytham.OP.RDS"))
# #
# system2("rsync",paste("-avz",
#                       "hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/Wytham.OP3.RDS",
#                       "/home/femeunier/Documents/projects/Hackaton/LidarED/data/Wytham.OP3.RDS"))

fac <- 1000 / 12 / 86400 * 1e6 / 365

OP <- readRDS("/home/femeunier/Documents/projects/Hackaton/LidarED/data/Wytham.OP.RDS") %>% mutate(gpp = fac*gpp)  %>% group_by(month) %>%
  summarise(GPPm = mean(gpp,na.rm = TRUE),
            GPPsd = sd(gpp,na.rm = TRUE))

OP2 <- readRDS("/home/femeunier/Documents/projects/Hackaton/LidarED/data/Wytham.OP2.RDS") %>% mutate(gpp = fac*gpp)  %>% group_by(month) %>%
  summarise(GPPm = mean(gpp,na.rm = TRUE),
            GPPsd = sd(gpp,na.rm = TRUE))

OP3 <- readRDS("/home/femeunier/Documents/projects/Hackaton/LidarED/data/Wytham.OP3.RDS") %>% mutate(gpp = fac*gpp)  %>% group_by(month) %>%
  summarise(GPPm = mean(gpp,na.rm = TRUE),
            GPPsd = sd(gpp,na.rm = TRUE))



data <- readRDS("/home/femeunier/Downloads/Fluxtower.RDS") %>% mutate(date = as.Date(as.yearmon(time, format = '%Y%m')),
                                                                      year = year(date),
                                                                      month = month(date))

data.month <- data %>% group_by(month) %>% summarise(GPPm = mean(GPP,na.rm = TRUE),
                                                     GPPsd = sd(GPP,na.rm = TRUE))



# Add default runs
# load("/home/femeunier/Downloads/Default.RData")
# default <-   data.frame(time = datum$year + (datum$month-1)/12,
#                         year = datum$year,
#                         month = datum$month,
#                         agb = datum$emean$agb,
#                         gpp = datum$emean$gpp,
#                         lai = datum$emean$lai) %>% filter(year >= (max(year)-10)) %>% group_by(month) %>% summarise(GPPm = mean(fac*gpp,na.rm = TRUE),
#                                                                                                                     GPPsd = sd(fac*gpp,na.rm = TRUE))



modvsobs <- bind_rows(list(OP %>% rename(modm = GPPm,
                                         modsd = GPPsd) %>% left_join(data.month,by = "month") %>% mutate(type = "IC-Inventory"),
                           OP2 %>% rename(modm = GPPm,
                                          modsd = GPPsd) %>% left_join(data.month,by = "month") %>% mutate(type = "IC-TLS"),
                           OP3 %>% rename(modm = GPPm,
                                          modsd = GPPsd) %>% left_join(data.month,by = "month") %>% mutate(type = "IC-FC")
                           #, default %>% rename(modm = GPPm,
                           #                    modsd = GPPsd) %>% left_join(data.month,by = "month") %>% mutate(type = "NBG")
                           ))


modvsobs <-
  modvsobs %>% mutate(
    modsd = case_when(type == "IC-TLS" ~ modsd,
                      TRUE ~ 0),
    GPPsd = case_when(type == "IC-TLS" ~ GPPsd,
                      TRUE ~ 0)
  )

modvsobs <-
  modvsobs %>% mutate(
    type = case_when(
      type == "IC-Inventory" ~ "IC-Default",
      TRUE ~ type
    )
  )

modvsobs$type <- factor(modvsobs$type,levels = c("IC-Default","IC-FC","IC-TLS"))

subplotA <- ggplot(data = modvsobs) +
  geom_point(aes(x = modm,y = GPPm,color = as.factor(type))) +
  geom_errorbar(aes(x = modm,y = GPPm,ymin = GPPm-GPPsd, ymax = GPPm + GPPsd,color = as.factor(type))) +
  geom_errorbarh(aes(y = GPPm,xmin = modm-modsd, xmax = modm + modsd,color = as.factor(type))) +
  geom_abline(slope = 1,linetype = 3) +
  geom_smooth(aes(x = modm,y = GPPm,color = as.factor(type)),method='lm', formula= y~x,se = FALSE) +
  scale_color_manual(values = c("black","#4f8abb","grey")) +
  labs(x = expression(atop("Simulated GPP",paste("(µmol CO" ["2"] ," m" ^"-2"," s" ^"-1",")"))),
       y = expression(atop("Observed GPP",paste("(µmol CO" ["2"] ," m" ^"-2"," s" ^"-1",")"))),
       color = "Configuration") +
  theme_bw() +
  theme(text = element_text(size = 24),
        legend.position = c(0.25,0.89),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))

modvsobs %>% group_by(type) %>% summarise(intercept = coef(lm(formula = GPPm ~ modm)) [1],
                                          slope = coef(lm(formula = GPPm ~ modm)) [2],
                                          r2 = summary(lm(formula = GPPm ~ modm))$r.squared,
                                          rmse = sqrt(sum(GPPm-modm)**2)/length(modm))


subplotB <-
  ggplot() +
  geom_ribbon(data = OP2,
              aes(x = month,ymin = GPPm - GPPsd,
                  ymax = GPPm + GPPsd),fill = "grey",color = NA,alpha = 0.4) +
  # geom_ribbon(data = OP2,
  #             aes(x = month,ymin = GPPm - GPPsd,
  #                 ymax = GPPm + GPPsd),fill = "darkgrey",color = NA,alpha = 0.8) +
  # geom_ribbon(data = OP3,
  #             aes(x = month,ymin = GPPm - GPPsd,
  #                 ymax = GPPm + GPPsd),fill = "red",color = NA,alpha = 0.8) +
  geom_point(data = data.month,
             aes(x = month,y = GPPm)) +
  geom_errorbar(data = data.month,
                aes(x = month,y = GPPm,ymin = GPPm-GPPsd,ymax = GPPm+GPPsd),
                width = 0.1) +
  # geom_line(data = default,aes(x = month,GPPm),linetype = 2,color = "darkred",size = 1.) +
  scale_x_continuous(breaks = 1:12,
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  labs(x = "Month") +
  ylab(expression(atop("GPP",paste("(µmol CO" ["2"] ," m" ^"-2"," s" ^"-1",")")))) +
  theme_bw() +
  theme(text = element_text(size = 24),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))

plot_grid(subplotA,subplotB,align = "hv")

ggsave(plot = last_plot(),"./Figures/Figure_fit.png",
       dpi = 300,width = 12,height = 8)
