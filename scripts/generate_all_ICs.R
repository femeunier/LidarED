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
library(qpcR)
library(sjstats)

# Load data
data.file <- "/home/femeunier/Documents/projects/Hackaton/LidarED//data/Wytham_trees_summary_ED2_nodead_add.csv"
data.wytham <- read.csv(data.file,header = TRUE) %>% mutate(TLS_ID = as.character(TLS_ID))

data.file2 <- "/home/femeunier/Documents/projects/Hackaton/LidarED//data/Wytham_trees_summary_ED2.csv"
data2.wytham <- read.csv(data.file2,header = TRUE) %>% dplyr::select(TLS_ID,VerticalCrownProjectedArea_pts_.m2.,species) %>% mutate(TLS_ID = as.character(TLS_ID),
                                                                                                                                    species = as.character(species))
data.wytham <- data.wytham %>% left_join(data2.wytham,by = "TLS_ID")  %>% rename(x =  stemlocx_.m._x,
                                                                                 y =  stemlocy_.m._x,
                                                                                 dbh_tls = DBH_TLS_.m._x,
                                                                                 h = Hgt_pts_.m._x,
                                                                                 AGV_m = Vol_QSM_avg_.m3._x,
                                                                                 scientific = species,
                                                                                 dbh_census = DBH_census_.m._x) %>%
  mutate(dbh_tls = 100*dbh_tls,
         dbh_census = 100*dbh_census) %>%
  mutate(x = pmin(139.5,x))

# data.wytham %>% group_by(species) %>% summarise(N = length(h)) %>% arrange(desc(N))

extr_x <- extremum(data.wytham[["x"]])
extr_y <- extremum(data.wytham[["y"]])
patch_X <- 20
patch_Y <- 20

data.wytham[["plots"]] <- patchnumber_from_position(data.wytham[["x"]],data.wytham[["y"]],patch_X,patch_Y)
data.wytham[["tag"]]  <- 1:nrow(data.wytham)

# ggsave(plot = last_plot(),
#        file = "./Figures/pos.png")

# Correct missing dbh_census
data.wytham <- data.wytham %>% mutate(dbh_census = case_when(is.na(dbh_census) ~ dbh_tls,
                                                             TRUE ~ dbh_census))

# PFT classification (if needed)
species.select <- c("ACERPS")
data.wytham %>% group_by(scientific) %>% summarise(N = length(h))
ratio <- data.wytham %>% group_by(scientific) %>% summarise(N = length(h)) %>% filter(scientific %in% species.select) %>% pull(N)/
  nrow(data.wytham)
Ndefault <- round(data.wytham %>% group_by(scientific) %>% summarise(N = length(h)) %>% filter(scientific == '') %>% pull(N)*ratio)
IDs.default <- data.wytham %>% filter(scientific == '') %>% pull(TLS_ID)
IDs.default.select <- sample(IDs.default,size = Ndefault,replace = FALSE)

rho.default <- c(0.74,0.70)

data.wytham.PFT <- data.wytham  %>% mutate(PFT = case_when(scientific %in% species.select ~ 1,
                                                           TLS_ID %in% IDs.default.select ~ 1,
                                                           TRUE ~ 0),
                                           PFT.name = case_when(scientific %in% species.select ~ "LH",
                                                                TLS_ID %in% IDs.default.select ~ "LH",
                                                                TRUE ~ "MH"))

data.wytham.PFT[["wood.dens"]]  <-  rho.default[1 + data.wytham.PFT$PFT]

data.wytham.onePFT <- data.wytham  %>% mutate(PFT = case_when(TRUE ~ 1),
                                              PFT.name = case_when(TRUE ~ "LH"))
data.wytham.onePFT[["wood.dens"]]  <-  rho.default[1 + data.wytham.onePFT$PFT]

# DBH decision

data.wytham.PFT.TLS <- data.wytham.PFT %>% dplyr::select(c(plots,tag,scientific,wood.dens,dbh_tls)) %>% rename(dbh = dbh_tls)
data.wytham.PFT.Census <- data.wytham.PFT %>% dplyr::select(c(plots,tag,scientific,wood.dens,dbh_census)) %>% rename(dbh = dbh_census)
write.csv(data.wytham.PFT.TLS,file = "/home/femeunier/Documents/projects/Hackaton/LidarED/data/Wytham_PFT_TLS.csv")
write.csv(data.wytham.PFT.Census,file = "/home/femeunier/Documents/projects/Hackaton/LidarED/data/Wytham_PFT_Census.csv")

make_IC_ED2("Wytham_PFT_TLS.csv","Wytham_PFT_TLS")
make_IC_ED2("Wytham_PFT_Census.csv","Wytham_PFT_Census")

ggplot(data.wytham.PFT,
       aes(x = x-extr_x[1], y = y-extr_y[1], color = as.factor(wood.dens),size = dbh_tls/10)) +
  geom_point(alpha = 0.5) +
  xlab("x (m)") +
  ylab("y (m)") +
  scale_x_continuous(breaks = seq(extr_x[1], extr_x[2], patch_X)-extr_x[1]) +
  scale_y_continuous(breaks = seq(extr_y[1], extr_y[2], patch_Y)-extr_y[1]) +
  # facet_wrap(~ as.factor(PFT)) +
  theme_bw() +
  theme(legend.position = "none")

data.wytham.onePFT.TLS <- data.wytham.onePFT %>% dplyr::select(c(plots,tag,scientific,wood.dens,dbh_tls)) %>% rename(dbh = dbh_tls)
data.wytham.onePFT.Census <- data.wytham.onePFT %>% dplyr::select(c(plots,tag,scientific,wood.dens,dbh_census)) %>% rename(dbh = dbh_census)
write.csv(data.wytham.onePFT.TLS,file = "/home/femeunier/Documents/projects/Hackaton/LidarED/data/Wytham_onePFT_TLS.csv")
write.csv(data.wytham.onePFT.Census,file = "/home/femeunier/Documents/projects/Hackaton/LidarED/data/Wytham_onePFT_Census.csv")

make_IC_ED2("Wytham_onePFT_TLS.csv","Wytham_onePFT_TLS")
make_IC_ED2("Wytham_onePFT_Census.csv","Wytham_onePFT_Census")

ggplot(data.wytham.onePFT,
       aes(x = x-extr_x[1], y = y-extr_y[1], color = as.factor(wood.dens),size = dbh_tls/10)) +
  geom_point(alpha = 0.5) +
  xlab("x (m)") +
  ylab("y (m)") +
  scale_x_continuous(breaks = seq(extr_x[1], extr_x[2], patch_X)-extr_x[1]) +
  scale_y_continuous(breaks = seq(extr_y[1], extr_y[2], patch_Y)-extr_y[1]) +
  # facet_wrap(~ as.factor(PFT)) +
  theme_bw() +
  theme(legend.position = "none")


