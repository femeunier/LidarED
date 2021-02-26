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
library(rhdf5)

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

##########################################################################################################
## Figure 1
# census plot

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

data.wytham.filled$species <- factor(as.character(data.wytham.filled$species),c("Acer pseudoplatanus",
                                                                                "Corylus avellana",
                                                                                "Crataegus monogyna",
                                                                                "Fraxinus excelsior",
                                                                                "Quercus robur",
                                                                                "Others"))

names <- data.wytham.filled %>% group_by(species) %>% summarise(N = n()) %>% mutate(full.name = paste0(species," (N = ",N,")"))
data.wytham.filled <- data.wytham.filled %>% left_join(names,by = "species") %>% mutate(full.name = as.factor(full.name))
data.wytham.filled$full.name <- factor(data.wytham.filled$full.name, levels(data.wytham.filled$full.name)[c(1,2,3,4,6,5)])


data.wytham.bin <- data.wytham.filled %>% mutate(dbh.bin = case_when(dbh_tls < 10 ~ 0,
                                                                     dbh_tls < 20 ~ 1,
                                                                     dbh_tls < 30 ~ 2,
                                                                     dbh_tls < 40 ~ 3,
                                                                     dbh_tls < 50 ~ 4,
                                                                     dbh_tls < 60 ~ 5,
                                                                     dbh_tls < 70 ~ 6,
                                                                     dbh_tls < 80 ~ 7,
                                                                     dbh_tls < 90 ~ 8,
                                                                     dbh_tls < 100 ~ 9,
                                                                     dbh_tls >= 100 ~ 10))
data.wytham.bin.sum <- data.wytham.bin %>% group_by(dbh.bin,species) %>% summarise(N = length(dbh_tls)) %>%
  left_join(names %>% dplyr::select(-N),by = "species") %>% mutate(full.name = as.factor(full.name))

data.wytham.bin.sum$full.name <- factor(data.wytham.bin.sum$full.name, levels(data.wytham.bin.sum$full.name)[c(1,2,3,4,6,5)])

source("/home/femeunier/Documents/ED2/R-utils/h5read_opt.r")


file <- "/home/femeunier/Documents/projects/Hackaton/LidarED/data/history-S-1812-06-01-000000-g01.h5"
mymont    = lapply(h5read_opt(file),FUN=aperm)
names(mymont) <- gsub(x = names(mymont), pattern = "\\_", replacement = ".")

df_OP <- data.frame(pft = mymont$PFT,
                    dbh = mymont$DBH,
                    patch = rep(1:length(mymont$PACO.N),mymont$PACO.N),
                    nplant = mymont$NPLANT,
                    area = mymont$AREA[rep(1:length(mymont$PACO.N),mymont$PACO.N)]) %>% mutate(dbh.bin = case_when(dbh < 10 ~ 0,
                                                                                                                    dbh < 20 ~ 1,
                                                                                                                    dbh < 30 ~ 2,
                                                                                                                    dbh < 40 ~ 3,
                                                                                                                    dbh < 50 ~ 4,
                                                                                                                    dbh < 60 ~ 5,
                                                                                                                    dbh < 70 ~ 6,
                                                                                                                    dbh < 80 ~ 7,
                                                                                                                    dbh < 90 ~ 8,
                                                                                                                    dbh < 100 ~ 9,
                                                                                                                    dbh >= 100 ~ 10)) %>% group_by(patch,dbh.bin) %>%
  summarise(nplant = sum(nplant),
            area = area[1]) %>% group_by(dbh.bin) %>% summarise(n = weighted.mean(x = nplant,w = area)) %>% ungroup()


data.sum <- data.wytham.bin.sum %>% group_by(dbh.bin) %>% summarise(N = sum(N)/(100*140)) %>% ungroup() %>% left_join(df_OP,by = "dbh.bin")

ggplot(data = data.sum) +
  geom_point(aes(x = n,y = N)) +
  scale_y_log10() +
  scale_x_log10() +
  theme_bw() +
  geom_abline(slope = 1,linetype = 3)
