rm(list = ls())

library(rhdf5)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

source("/home/femeunier/Documents/ED2/R-utils/h5read_opt.r")
treatment.name <- c("NBG","NBG-TLS","NBG-FC")

# system2("rsync",c("-avz",
#                   paste0("hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/Wytham/growth_storage_resp/out/near_bare_ground/histo/","history-S-1824-01-01-000000-g01.h5"),
#                   paste0("./data/",treatment.name[1],".h5")))
#
# system2("rsync",c("-avz",
#                   paste0("hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/Wytham/growth_storage_resp/out/near_bare_ground_Lidar/histo/","history-S-1824-01-01-000000-g01.h5"),
#                   paste0("./data/",treatment.name[2],".h5")))
#
# system2("rsync",c("-avz",
#                   paste0("hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/Wytham/growth_storage_resp/out/near_bare_ground_CA/histo/","history-S-1824-01-01-000000-g01.h5"),
#                   paste0("./data/",treatment.name[3],".h5")))

files <- file.path("/home/femeunier/Documents/projects/Hackaton/LidarED/data",paste0(treatment.name,".h5"))
df_OP <- data.frame()
m.dbh <- c()

for (ifile in seq(1,length(files))){
  file <- files[ifile]
  mymont    = lapply(h5read_opt(file),FUN=aperm)
  names(mymont) <- gsub(x = names(mymont), pattern = "\\_", replacement = ".")

  m.dbh <- c(m.dbh,max(mymont$DBH))

  df_OP <- bind_rows(list(df_OP,
                          data.frame(dbh.cat = seq(1,11),
                                     nplant = mymont$NPLANT.PY[1,,10]*100*100,
                                     run = treatment.name[ifile])))
}


##########################################################################################################################
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


mod.and.obs <- bind_rows(list(df_OP,
                        data.wytham.bin.sum %>% rename(dbh.cat = dbh.bin,
                                                       nplant = N) %>% mutate(run = 'Inventory'))) %>% filter(dbh.cat > 1)

# Max
data.wytham.bin %>% pull(dbh_tls) %>% max(na.rm = T)

mod.and.obs <-
  mod.and.obs %>% mutate(run = factor(
    case_when(run == "NBG" ~ "NBG-Default",
              TRUE ~ run),
    levels = c("Inventory", "NBG-Default",
               "NBG-FC", "NBG-TLS")
  ))

ggplot(data = mod.and.obs) +
  geom_bar(aes(x = as.numeric(dbh.cat),y = nplant,fill = as.factor(run)),stat = "identity", position = "dodge") +
  theme_bw() +
  labs(x = "DBH (cm)", y = "Stem density (#/m²)",fill = "Source") +
  scale_x_continuous(breaks = 2:11,
                   labels = c("10-20","20-30","30-40",
                              "40-50","50-60","60-70","70-80","80-90","90-100",">100") ) +
  scale_fill_manual(values = c("Darkgrey",'#1b9e77','#d95f02','#e6ab02')) +
  scale_y_continuous(expand = c(0,0,0.05,0)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        text = element_text(size = 24),
        legend.position = c(0.8,0.8),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))

# rmse.df <- mod.and.obs %>% mutate(run = case_when(run == "NBG-TLS" ~ "NBG_TLS",
#                                        run == "NBG-FC" ~ "NBG_FC",
#                                        TRUE ~ run)) %>% pivot_wider(names_from = run, values_from = nplant)
#
mean((mod.and.obs %>% filter(dbh.cat <6,run == "Inventory") %>% pull(nplant) - mod.and.obs %>% filter(dbh.cat <6,run == "NBG-Default") %>% pull(nplant))/
  mod.and.obs %>% filter(dbh.cat <6,run == "Inventory") %>% pull(nplant))
#
# mean((mod.and.obs %>% filter(dbh.cat <9,run == "NBG-FC") %>% pull(nplant) - mod.and.obs %>% filter(dbh.cat <9,run == "NBG") %>% pull(nplant))/
#        mod.and.obs %>% filter(dbh.cat <9,run == "NBG") %>% pull(nplant),na.rm = TRUE)
#
# mean((mod.and.obs %>% filter(dbh.cat < 8,run == "NBG-TLS") %>% pull(nplant) - mod.and.obs %>% filter(dbh.cat <8,run == "NBG-FC") %>% pull(nplant))/
#        mod.and.obs %>% filter(dbh.cat < 8,run == "NBG-FC") %>% pull(nplant),na.rm = TRUE)
#
# rmse.df %>% ungroup() %>% summarise(RMSE = sqrt(sum((Inventory - NBG)**2)/10))
# rmse.df %>% ungroup() %>% summarise(RMSE = sqrt(sum((Inventory - NBG_TLS)**2)/10))
# rmse.df %>% ungroup() %>% summarise(RMSE = sqrt(sum((Inventory - NBG_FC)**2)/10))
#
ggsave(plot = last_plot(),"./Figures/Figure_sizedis.png",
       dpi = 300,width = 12,height = 7)
