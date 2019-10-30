rm(list = ls())

library(ggplot2)
library(dplyr)
library(minpack.lm)
library(propagate)
library(investr)
library(LidarED)

# Load data
data.file <- "./data/Wytham_trees_summary_ED2.csv"
data.wytham <- read.csv(data.file,header = TRUE)

data.wytham <- data.wytham %>% rename(x =  stemlocx_.m.,
                                      y =  stemlocy_.m.,
                                      dbh_tls = DBH_TLS_.m.,
                                      h = Hgt_pts_.m.,
                                      CA = VerticalCrownProjectedArea_pts_.m2.,
                                      AGV_m = Vol_QSM_avg_.m3.,
                                      AGV_sd = Vol_QSM_sd_.m3.,
                                      dbh_census = DBH_census_.m.) %>% mutate(dbh_tls = 100*dbh_tls,
                                                                              dbh_census = 100*dbh_census) %>%
  dplyr::select(c(x,y,dbh_tls,h,CA,AGV_m,dbh_census))

patch_X <- 20
patch_Y <- 20

data.wytham[["patch"]] <- patchnumber_from_position(data.wytham[["x"]],data.wytham[["y"]],patch_X,patch_Y)
