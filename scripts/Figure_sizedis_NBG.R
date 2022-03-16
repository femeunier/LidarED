rm(list = ls())

library(rhdf5)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

system2("rsync",c("-avz","hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/df.OP.NBG.RDS",
                  file.path("/home/femeunier/Documents/projects/Hackaton/LidarED/","outputs","df.OP.NBG.RDS")))

df.OP.NBG <- readRDS(file.path("/home/femeunier/Documents/projects/Hackaton/LidarED/","outputs","df.OP.NBG.RDS"))

runs2keep <- df.OP.NBG %>%
  group_by(isTRY) %>%
  filter(dbh.cat == 2,
         nplant > 0)

df.OP.NBG.sum <- df.OP.NBG %>%
  filter(((run %in% (runs2keep %>% filter(isTRY == "TRY") %>% pull(run))) & (isTRY == "TRY"))  |
           ((run %in% (runs2keep %>% filter(isTRY == "noTRY") %>% pull(run))) & (isTRY == "noTRY"))) %>%
  group_by(crown_mod,isTRY,dbh.cat) %>%
  summarise(nplant.m = mean(nplant,na.rm = TRUE),
            nplant.min = 0.9*mean(nplant,na.rm = TRUE),
            nplant.max = mean(nplant,na.rm = TRUE) + sd(nplant,na.rm = TRUE),
            .groups = "keep") %>%
  rename(nplant = nplant.m) %>%
  mutate(run = paste0(isTRY,crown_mod))

# Data

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


data.wytham.bin <- data.wytham %>% mutate(dbh.bin = 1+case_when(dbh_census < 10 ~ 0,
                                                                dbh_census < 20 ~ 1,
                                                                dbh_census < 30 ~ 2,
                                                                dbh_census < 40 ~ 3,
                                                                dbh_census < 50 ~ 4,
                                                                dbh_census < 60 ~ 5,
                                                                dbh_census < 70 ~ 6,
                                                                dbh_census < 80 ~ 7,
                                                                dbh_census < 90 ~ 8,
                                                                dbh_census < 100 ~ 9,
                                                                dbh_census >= 100 ~ 10))
data.wytham.bin.sum <- data.wytham.bin %>% group_by(dbh.bin) %>% summarise(N = length(dbh_tls))


mod.and.obs <- bind_rows(list(df.OP.NBG.sum,
                              data.wytham.bin.sum %>% rename(dbh.cat = dbh.bin,
                                                             nplant = N) %>% mutate(run = 'Inventory'))) %>% filter(dbh.cat > 1)

# Max
data.wytham.bin %>% pull(dbh_tls) %>% max(na.rm = T)

# mod.and.obs <-
#   mod.and.obs %>% mutate(run = factor(
#     case_when(run == "NBG" ~ "NBG-Default",
#               TRUE ~ run),
#     levels = c("Inventory", "NBG-Default",
#                "NBG-FC", "NBG-TLS")
#   ))


mod.and.obs <- mod.and.obs %>% mutate(run = case_when(run == "TRY0" ~ "TRY, Closed canopy",
                                                      run == "TRY1" ~ "TRY, Finite crown",
                                                      run == "noTRY0" ~ "no TRY, Closed canopy",
                                                      run == "noTRY1" ~ "no TRY, Finite crown",
                                                      TRUE ~ run))


a <- mod.and.obs %>% filter(dbh.cat <= 5) %>%
  filter(crown_mod == 1) %>% pull(nplant)
b <- mod.and.obs %>% filter(dbh.cat <= 5) %>%
  filter(crown_mod == 0) %>% pull(nplant)

mean((a-b)/b*100)

a <- mod.and.obs %>% filter(dbh.cat <= 5) %>%
  filter(isTRY == "noTRY") %>% pull(nplant)
b <- mod.and.obs %>% filter(dbh.cat <= 5) %>%
  filter(isTRY == "TRY") %>% pull(nplant)

mean((b - a)/a*100)


ggplot(data = mod.and.obs) +
  geom_errorbar(aes(x = as.numeric(dbh.cat),
                    ymin = nplant.min, ymax = nplant.max,
                    group = as.factor(run)),width = 0.2,
                position=position_dodge(.9)) +
  geom_bar(aes(x = as.numeric(dbh.cat),y = nplant,fill = as.factor(run)),
           stat = "identity", position = "dodge") +
  theme_bw() +
  labs(x = "DBH (cm)", y = "Stem density (#/ha)",fill = "") +
  scale_x_continuous(breaks = 2:11,
                     labels = c("10-20","20-30","30-40",
                                "40-50","50-60","60-70","70-80","80-90","90-100",">100") ) +
  scale_fill_manual(values = c("Darkgrey",'#1b9e77','#d95f02','#e6ab02',"#66a61e")) +
  scale_y_continuous(expand = c(0,0,0.05,0)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        text = element_text(size = 24),
        legend.position = c(0.8,0.75),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))

ggsave(plot = last_plot(),"./Figures/Figure_sizedis_NBG.png",
       dpi = 300,width = 15,height = 7)


df.Wytham.ensemble.BA <- readRDS(file.path("/home/femeunier/Documents/projects/Hackaton/LidarED/","outputs","df.Wytham.ensembles.RDS")) %>%
  filter(!is.na(month),
         year == 2006,
         month == 6) %>%
  mutate(time = year + (month - 1)/12,
         isTRY = case_when(grepl("noTRY",name) ~ "noTRY",
                           TRUE ~ "TRY"))


df.inputs <- readRDS(file.path("/home/femeunier/Documents/projects/Hackaton/LidarED/","outputs","inputs.ensembles.RDS")) %>%
  filter(pft == "LH",
         trait == "q") %>%
  dplyr::select(type,num,crownmod)

df.Wytham.ensemble.BA.inputs <- df.Wytham.ensemble.BA %>%
  left_join(df.inputs,
            by = c("type","num"))

df.Wytham.ensemble.BA.inputs %>% group_by(type,isTRY,crownmod) %>%
  filter(LAI > 0.1) %>%
  summarise(BA.m = mean(N)) %>%
  filter(type == "NBG")



ggplot(data = mod.and.obs) +
  geom_errorbar(aes(x = as.numeric(dbh.cat),
                    ymin = nplant.min, ymax = nplant.max,
                    group = as.factor(run)),width = 0.2,
                position=position_dodge(.9)) +
  geom_bar(aes(x = as.numeric(dbh.cat),y = nplant,fill = as.factor(run)),
           stat = "identity", position = "dodge") +
  theme_bw() +
  labs(x = "DBH (cm)", y = "Stem density (#/ha)",fill = "") +
  scale_x_continuous(breaks = 2:11,
                     labels = c("10-20","20-30","30-40",
                                "40-50","50-60","60-70","70-80","80-90","90-100",">100") ) +
  scale_fill_manual(values = c("Darkgrey",'#1b9e77','#d95f02','#e6ab02',"#66a61e")) +
  scale_y_continuous(expand = c(0,0,0.05,0)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        text = element_text(size = 24),
        legend.position = c(0.8,0.75),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))

ggsave(plot = last_plot(),"./Figures/Figure_sizedis_NBG2.png",
       dpi = 300,width = 15,height = 7)

