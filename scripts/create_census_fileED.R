rm(list = ls())

library(ggplot2)
library(dplyr)
library(minpack.lm)
library(propagate)
library(investr)
library(LidarED)

setwd("/home/femeunier/Documents/projects/Hackaton/LidarED")

# Load data
data.file <- "/home/femeunier/Documents/projects/Hackaton/LidarED/data/Wytham_trees_summary_ED2_nodead_add.csv"
data.wytham <- read.csv(data.file,header = TRUE) %>% mutate(TLS_ID = as.character(TLS_ID))

data.file2 <- "/home/femeunier/Documents/projects/Hackaton/LidarED/data/Wytham_trees_summary_ED2.csv"
data2.wytham <- read.csv(data.file2,header = TRUE) %>% dplyr::select(TLS_ID,VerticalCrownProjectedArea_pts_.m2.,species) %>% mutate(TLS_ID = as.character(TLS_ID),
                                                                                                                                    species = as.character(species))

data.wytham <- data.wytham %>% left_join(data2.wytham,by = "TLS_ID")  %>% rename(x =  stemlocx_.m._x,
                                      y =  stemlocy_.m._x,
                                      dbh_tls = DBH_TLS_.m._x,
                                      h = Hgt_pts_.m._x,
                                      AGV_m = Vol_QSM_avg_.m3._x,
                                      scientific = species,
                                      dbh_census = DBH_census_.m._x) %>% mutate(dbh_tls = 100*dbh_tls,
                                                                              dbh_census = 100*dbh_census) %>%
  dplyr::select(c(x,y,dbh_tls,h,AGV_m,dbh_census,scientific)) %>% mutate(PFT = case_when(scientific %in% c("ACERPS") ~ 1,
                                                                                         TRUE ~ 0),
                                                                         PFT.name = case_when(scientific %in% c("ACERPS") ~ "LH",
                                                                                              TRUE ~ "MH")) %>%
  mutate(x = pmin(139.5,x))

extr_x <- extremum(data.wytham[["x"]])
extr_y <- extremum(data.wytham[["y"]])
patch_X <- 20
patch_Y <- 20

data.wytham[["plots"]] <- patchnumber_from_position(data.wytham[["x"]],data.wytham[["y"]],patch_X,patch_Y)
data.wytham[["tag"]]  <- 1:nrow(data.wytham)

rho.default <- c(0.74,0.70)
data.wytham[["wood.dens"]]  <-  rho.default[1 + data.wytham$PFT]


ggplot(data.wytham,
       aes(x = x-extr_x[1], y = y-extr_y[1], color = as.factor(plots),size = dbh_census/10,shape = as.factor(PFT))) +
  geom_point(alpha = 0.5) +
  xlab("x (m)") +
  ylab("y (m)") +
  scale_x_continuous(breaks = seq(extr_x[1], extr_x[2], patch_X)-extr_x[1]) +
  scale_y_continuous(breaks = seq(extr_y[1], extr_y[2], patch_Y)-extr_y[1]) +
  # facet_wrap(~ as.factor(PFT)) +
  theme_bw() +
  theme(legend.position = "none")

ggsave(plot = last_plot(),
       file = "./Figures/pos.png")

data.wytham_fin <- data.wytham %>% dplyr::select(c(plots,tag,scientific,wood.dens,dbh_tls)) %>% rename(dbh = dbh_tls)
write.csv(data.wytham_fin,file = "./data/Wytham_census_formattedPFT.csv")
