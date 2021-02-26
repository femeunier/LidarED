rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)

system2("rsync",paste("-avz",
                      "hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/params.errors.RDS",
                      "/home/femeunier/Documents/projects/Hackaton/LidarED/data/params.errors.RDS"))

params.errors <- readRDS("/home/femeunier/Documents/projects/Hackaton/LidarED/data/params.errors.RDS")

params.errors.filt <-
  params.errors %>% dplyr::select(-index) %>% group_by(simulation) %>% slice_head(n = 1) %>% mutate(simulation = as.numeric(simulation)) %>% filter(!is.na(simulation)) %>%
  mutate(type = case_when(simulation < 99000018300 ~ "SLA (m²/kgC)",
                          simulation >= 99000018300  ~ "Vcmax (µmol/m²/s)"))

params.errors.filt.long <- params.errors.filt %>% pivot_longer(cols = c("SLA","Vm0"),
                                                               values_to = "value",
                                                               names_to = "param") %>% filter((type == "SLA (m²/kgC)" & param == "SLA") | (type == "Vcmax (µmol/m²/s)" & param == "Vm0"))

ggplot(data = params.errors.filt.long) +
  geom_point(aes(x = value,y = SSE),size = 1.) +
  geom_line(aes(x = value,y = SSE),color = "red",size = 1.) +
  facet_wrap(~ as.factor(type),scales = "free") +
  theme_bw() +
  labs(x = "",y = "SSE (kgC/m²/s)²") +
  theme(text = element_text(size = 24),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))

ggsave(plot = last_plot(),"./Figures/Figure_SAerror.png",
       dpi = 300,width = 12,height = 8)

