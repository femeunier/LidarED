rm(list = ls())

library(ggplot2)
library(dplyr)
library(minpack.lm)
library(propagate)
library(investr)
library(LidarED)
library(stringr)
library(purrr)
library(reshape2)
library(cowplot)

# Load data
data.file <- "./data/Wytham_trees_summary_ED2_nodead_add.csv"
data.wytham <- read.csv(data.file,header = TRUE) %>% mutate(TLS_ID = as.character(TLS_ID))

data.file2 <- "./data/Wytham_trees_summary_ED2.csv"
data2.wytham <- read.csv(data.file2,header = TRUE) %>% dplyr::select(TLS_ID,VerticalCrownProjectedArea_pts_.m2.,species) %>% mutate(TLS_ID = as.character(TLS_ID),
                                                                                                                                    species = as.character(species))
data.wytham <- data.wytham %>% left_join(data2.wytham,by = "TLS_ID") %>%
  rename(x =  stemlocx_.m._x,
         y =  stemlocy_.m._x,
         dbh_tls = DBH_TLS_.m._x,
         h = Hgt_pts_.m._x,
         AGV_m = Vol_QSM_avg_.m3._x,
         dbh_census = DBH_census_.m._x,
         CA = VerticalCrownProjectedArea_pts_.m2.,
         LA = leaf_area) %>% mutate(dbh_tls = 100*dbh_tls,
                                    dbh_census = 100*dbh_census)

data.wytham.filled <- data.wytham %>% mutate(species = as.character(species)) %>%
  mutate(species = case_when(species == "" ~ "Others",
                             species == "ACERCA" ~ "Others",
                             species == "ACERPS" ~ "Acer pseudoplatanus",
                             species == "CORYAV" ~ "Corylus avellana",
                             species == "CRATMO" ~ "Crataegus monogyna",
                             species == "FRAXEX" ~ "Fraxinus excelsior",
                             species == "QUERRO" ~ "Quercus robur",
                             TRUE ~ species),
         BA = pi/4*dbh_tls^2) %>% mutate(species = as.factor(species))

data.wytham.filled$species <- factor(data.wytham.filled$species, levels(data.wytham.filled$species)[c(1,2,3,4,6,5)])

ggplot(data = data.wytham.filled,
       aes(x = dbh_tls,y = dbh_census,color = species)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  geom_abline(slope = 1,linetype = 3,color = "black") +
  labs(x = "TLS DBH (cm)",
       y = "Census DBH (cm)",
       color = "Species") +
  geom_smooth(color = "black",method='lm', formula= y~x,se = FALSE) +
  theme_bw() +
  scale_color_manual(values = c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02')) +
  theme(text = element_text(size = 24),
        legend.position = c(0.2,0.79))

summary(lm(data = data.wytham.filled, formula = dbh_census ~ dbh_tls))$r.squared
coef(lm(data = data.wytham.filled, formula = dbh_census ~ dbh_tls))

sqrt(mean(lm(data = data.wytham.filled, formula = dbh_census ~ dbh_tls)$residuals^2))

df.diff <- data.wytham.filled %>% mutate(diff_DBH = dbh_tls - dbh_census,
                              diff_DBH.rel = (dbh_tls - dbh_census)/dbh_census) %>% dplyr::select(TLS_ID,dbh_tls,
                                                                                                  dbh_census,
                                                                                                  diff_DBH,diff_DBH.rel)

ggplot(data = df.diff) +
  geom_histogram(aes(x = 100*diff_DBH.rel),alpha = 0.5) +
  theme_bw() +
  labs(x = "Relative difference in DBH (%)",
       y = "") +
  theme(text = element_text(size = 16),
        legend.position = c(0.2,0.79),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))

ggplot(data = df.diff) +
  geom_histogram(aes(x = diff_DBH),alpha = 0.5) +
  labs(x = "TLS DBH - Census DBH (cm)",
       y = "") +
  theme_bw() +
  theme(text = element_text(size = 16),
        legend.position = c(0.2,0.79),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))

ggsave(plot = last_plot(),"./Figures/Figure_dbh.png",
       dpi = 300,width = 10,height = 8)
