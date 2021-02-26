rm(list = ls())

library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)
library(tidyr)


# system2("rsync",paste("-avz",
#                       "hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/Wytham.OP.RDS",
#                       "/home/femeunier/Documents/projects/Hackaton/LidarED/data/Wytham.OP.RDS"))
#
# system2("rsync",paste("-avz",
#                       "hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/Wytham.OP2.RDS",
#                       "/home/femeunier/Documents/projects/Hackaton/LidarED/data/Wytham.OP2.RDS"))


OP <- as.data.frame(bind_rows(list(
  readRDS(
    "/home/femeunier/Documents/projects/Hackaton/LidarED/data/Wytham.OP.RDS"
  ) %>% mutate(type = "Census"),
  readRDS(
    "/home/femeunier/Documents/projects/Hackaton/LidarED/data/Wytham.OP2.RDS"
  ) %>% mutate(type = "TLS")
)))



# Add default runs
load("/home/femeunier/Downloads/default.RData")
default <-   data.frame(time = datum$year + (datum$month-1)/12,
                        year = datum$year,
                        month = datum$month,
                        agb = datum$emean$agb,
                        lai = datum$emean$lai) %>% mutate(agb = agb) %>% filter(year >= (max(year)-10)) %>% dplyr::select(month,agb,lai) %>% pivot_longer(cols = c("agb","lai"),
                                                                                                                              names_to = "param",
                                                                                                                              values_to = "value") %>% mutate(type = "Default") %>%
  filter(param == "agb" | (param == "lai" & month %in% seq(5,9))) %>% dplyr::select(-c("month"))


default <- default %>% group_by(param) %>% summarise(value = mean(value),
                                                     type = type[1])


all.OP <-
  bind_rows(
    list(
      OP %>% dplyr::filter(month %in% seq(5, 9)) %>% rename(value = lai) %>% mutate(param = "lai") %>% dplyr::select(value, param, type),
      OP %>% dplyr::filter(year %in% seq(2007, 2008)) %>% rename(value = agb) %>% mutate(param = "agb") %>% dplyr::select(value, param, type) %>% mutate(value = case_when(type == "TLS" ~ value /0.7,
                                                                                                                                                                           TRUE ~ value)),
      default %>% mutate(value = NA)
    )
  )

all.OP$type <- factor(all.OP$type)
all.OP$type <- factor(all.OP$type, levels(all.OP$type)[c(2,1,3)])


subplotA <- ggplot(data = all.OP %>% filter(param == "agb")) +
  geom_boxplot(aes(x = as.factor(type),y = value,
                   fill = as.factor(type)),alpha = 0.5) +
  labs(x = "",y = "AGB [kgC/m²]",fill = "Configuration",color = "Configuration") +
  geom_point(aes(x = 1,y = default %>% filter(param == "agb") %>% pull(value),color = "Default")) +
  scale_fill_manual(values = c("darkgrey","black")) +
  scale_color_manual(values = c("darkred","darkgrey","black")) +
  # facet_wrap(~ param,scales = "free") +
  theme_bw() +
  theme(text = element_text(size = 24),
        legend.position = "none",
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))

subplotB<- ggplot(data = all.OP %>% filter(param == "lai")) +
  geom_boxplot(aes(x = as.factor(type),y = value,
                   fill = as.factor(type)),alpha = 0.5) +
  labs(x = "",y = "LAI [m²/m²]",fill = "Configuration",color = "Configuration") +
  geom_point(aes(x = 1,y = default %>% filter(param == "lai") %>% pull(value),color = "Default")) +
  scale_fill_manual(values = c("darkgrey","black")) +
  scale_color_manual(values = c("darkred","darkgrey","black")) +
  # facet_wrap(~ param,scales = "free") +
  theme_bw() +
  theme(text = element_text(size = 24),
        legend.position = "none",
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))

plot_grid(subplotB,subplotA,align = "hv")

ggsave(plot = last_plot(),"./Figures/Figure_config.png",
       dpi = 300,width = 10,height = 6)

ggplot(data = OP) +
  geom_boxplot(aes(x = as.factor(type),y = lai)) +
  theme_bw()


OP <- readRDS("/home/femeunier/Documents/projects/Hackaton/LidarED/data/Wytham.OP2.RDS") %>% group_by(time) %>%
  summarise(agb.m = mean(agb),
            agb.sd = sd(agb),
            agb.min = min(agb),
            agb.max = max(agb),
            lai.m = mean(lai),
            lai.sd = sd(lai),
            lai.min = min(lai),
            lai.max = max(lai))

ggplot(data = OP,
       aes(x = time,y = agb.m)) +
  geom_line() +
  geom_ribbon(aes(ymin = agb.m - agb.sd,ymax = agb.m + agb.sd),alpha = 0.4) +
  theme_bw()

ggplot(data = OP,
       aes(x = time,y = lai.m)) +
  geom_line() +
  geom_ribbon(aes(ymin = lai.m - lai.sd,ymax = lai.m + lai.sd),alpha = 0.4) +
  theme_bw()

