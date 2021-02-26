rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(cowplot)

# system2("rsync",paste("-avz",
#                       "hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/df_params_Wytham.RDS",
#                       "/home/femeunier/Documents/projects/Hackaton/LidarED/data/"))

load("/home/femeunier/Documents/projects/Hackaton/data/history.pda99000000913.Rdata")
Lidar <- as.data.frame(m) %>% mutate(Vcmax = Vcmax/1.417925,
                                     SLA = SLA/0.48)

load("/home/femeunier/Documents/projects/Hackaton/data/history.pda99000000934.Rdata")
FC <- as.data.frame(m) %>% mutate(Vcmax = Vcmax/1.417925,
                                     SLA = SLA/0.48)

load("/home/femeunier/Documents/projects/Hackaton/data/history.pda99000000907.Rdata")
Default <- as.data.frame(m) %>% mutate(Vcmax = Vcmax/1.417925,
                                       SLA = SLA/0.48)

distributions <- bind_rows(list(Default %>% mutate(type = "IC-Census"),
                                Lidar %>% mutate(type = "IC-TLS"),
                                FC %>% mutate(type = "IC-FC"))) %>%
  pivot_longer(cols = c("SLA","Vcmax"),
               names_to = "param",
               values_to = "value")

alpha = 0.05
distributions.filtered <- distributions %>% group_by(type,param) %>% filter(value > quantile(value,alpha/2) & value < quantile(value,1-alpha/2))

data <- data.frame(SLA = c(NA,34.7,62.8,NA,22.9,25.1),
                   SLA_sd = c(NA,NA,NA,NA,NA,1.5),
                   Vcmax = c(31.9,NA,NA,39.7,31.1,32.6),
                   Vcmax_sd = c(NA,NA,NA,NA,NA,0.9),
                   BA =c(31.59,0.48,0.24,5.96,11.87,sum(c(31.59,0.48,0.24,5.96,11.87))),
                   species = as.factor(c("Ap","Ca","Cm","Fe","Qr","CWM"))) %>% pivot_longer(cols = c("SLA","Vcmax"),
                                                                                 names_to = "param",
                                                                                 values_to = "value")

data$species <- factor(data$species,levels = levels(data$species)[c(1,2,3,5,6,4)])

subplotA <-
  ggplot(data = distributions.filtered %>% filter(param == "SLA")) +
  geom_hline(yintercept = 0,linetype = 1) +
  geom_density(aes(x = value,fill = as.factor(type)),
               alpha=0.7,color = NA) +
  geom_point(data = data %>% filter(param == "SLA"),alpha = 0.8,
             aes(x = value, y = 0,size = BA,color = as.factor(species))) +
  geom_errorbarh(data = data %>% filter(param == "SLA"),alpha = 0.8,
                 aes(x = value, y = 0,xmin = value - SLA_sd, xmax = value + SLA_sd),color = "black",height = 0.01) +
  geom_vline(xintercept = 24.2,linetype = 3,color = "darkred") +
  theme_bw() +
  labs(x = "SLA (m²/kgC)",y = "Probability density", fill = "Configuration") +
  guides(fill = FALSE,
         size = FALSE,
         color = FALSE) +
  scale_color_manual(values = c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','black',"black")) +
  scale_fill_manual(values = c("black","#1E3F5A","grey")) +
  theme(text = element_text(size = 16),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))

distributions.filtered %>% group_by(param,type) %>% summarise(m = median(value))

subplotB <-
  ggplot(data = distributions.filtered %>% filter(param == "Vcmax")) +
  geom_hline(yintercept = 0,linetype = 1) +
  geom_density(aes(x = value,fill = as.factor(type)),
               alpha=0.7,color = NA) +
  geom_point(data = data %>% filter(param == "Vcmax"),alpha = 0.8,
             aes(x = value, y = 0,size = BA,color = as.factor(species))) +
  geom_errorbarh(data = data %>% filter(param == "Vcmax"),alpha = 0.8,
                 aes(x = value, y = 0,xmin = value - Vcmax_sd, xmax = value + Vcmax_sd),color = "black",height = 0.01) +
  geom_vline(xintercept = 17.5,linetype = 3,color = "darkred") +
  theme_bw() +
  labs(x = "Vcmax (µmolC/m²/s)",y = "Probability density",fill = "Configuration") +
  guides(fill = FALSE,
         size = FALSE,
         color = FALSE) +
  scale_color_manual(values = c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','black')) +
  scale_fill_manual(values = c("black","#1E3F5A","grey")) +
  theme(text = element_text(size = 16),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))

plot_grid(subplotA,subplotB)

ggsave(plot = last_plot(),"./Figures/Figure_distributions.png",
       dpi = 300,width = 8,height = 4)
