rm(list = ls())

library(ggplot2)
library(dplyr)
library(minpack.lm)
library(propagate)
library(investr)

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
                                                                              dbh_census = 100*dbh_census)

##########################################################################################################
# census plot

ggplot(data.wytham,
       aes(x = x, y = y, size = dbh_census)) +
  geom_point(alpha = 0.5, col = "black") +
  xlab("x (m)") +
  ylab("y (m)") +
  theme_bw()

##########################################################################################################
# census vs lidar dbh plot

ggplot(data.wytham,
       aes(x = dbh_census, y =dbh_tls)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm",level = 0.99,color = 'blue',alpha = 0.5,fill = 'blue') +
  xlab("DBH census (cm)") +
  ylab("DBH TLS (cm)") +
  theme_bw()

##########################################################################################################
  # Tropical tree pft, might be better to use temperate:
  # !---------------------------------------------------------------------------------------!
  # !  PFT | Name                                       | Grass   | Tropical | agriculture? !
  # !------+--------------------------------------------+---------+----------+--------------!
  # !    1 | C4 grass                                   |     yes |      yes |          yes !
  # !    2 | Early tropical                             |      no |      yes |           no !
  # !    3 | Mid tropical                               |      no |      yes |           no !
  # !    4 | Late tropical                              |      no |      yes |           no !
  # !    5 | Temperate C3 grass                         |     yes |       no |          yes !
  # !    6 | Northern pines                             |      no |       no |           no !
  # !    7 | Southern pines                             |      no |       no |           no !
  # !    8 | Late conifers                              |      no |       no |           no !
  # !    9 | Early temperate deciduous                  |      no |       no |           no !
  # !   10 | Mid temperate deciduous                    |      no |       no |           no !
  # !   11 | Late temperate deciduous                   |      no |       no |           no !
  # !   12 | C3 pasture                                 |     yes |       no |          yes !
  # !   13 | C3 crop (e.g.,wheat, rice, soybean)        |     yes |       no |          yes !
  # !   14 | C4 pasture                                 |     yes |      yes |          yes !
  # !   15 | C4 crop (e.g.,corn/maize)                  |     yes |      yes |          yes !
  # !   16 | Tropical C3 grass                          |     yes |      yes |          yes !
  # !   17 | Liana                                      |      no |      yes |           no !
  # !---------------------------------------------------------------------------------------!
##########################################################################################################
# ED2 default allometric equations vs data

df <- data.wytham %>% filter(!is.na(dbh_tls) & !is.na(h))

href <- 61.7
b1Ht <- 0.035
b2Ht <- 0.69

m0 <- nlsLM(data = df,
            h ~ href*(1 -exp(-b1Ht*(dbh_tls**b2Ht))),
            start=list(href=href, b1Ht=b1Ht, b2Ht = b2Ht), control = nls.control(maxiter = 500, tol = 1e-05, minFactor = 1/1024/10,
                                                                        printEval = TRUE, warnOnly = TRUE))

dbh_extr <- extremum(data.wytham$dbh_census)
dbh <- seq(dbh_extr[1],dbh_extr[2],length.out = 1000)

plotFit(m0,interval="prediction",pch=1,col.pred=adjustcolor("blue", 0.5),shade=T,ylim=c(0,35),col.fit = "blue",
        xlab = "DBH TLS (cm)", ylab = 'h (m)')
lines(dbh,pmin(35,dbh2h(href,b1Ht,b2Ht,dbh)),col="red")


##########################################################################################################
# Crown area allometries

df <- data.wytham %>% filter(!is.na(dbh_tls) & !is.na(CA))

b1Ca <- exp(0.1184295)
b2Ca <- 1.0521197319

m0 <- nlsLM(data = df,
            CA ~ b1Ca*(dbh_tls^b2Ca),
            start=list(b1Ca=b1Ca, b2Ca=b2Ca), control = nls.control(maxiter = 500, tol = 1e-05, minFactor = 1/1024/10,
                                                                    printEval = TRUE, warnOnly = TRUE))

ggplot() +
  geom_point(data = df,aes(x = dbh_tls,y = CA)) +
  theme_bw() +
  scale_x_log10() +
  scale_y_log10() +
  xlab("DBH TLS (cm)") +
  ylab("Crown area (m²)") +
  geom_smooth(data = df,aes(x = dbh_tls,y = CA),
              method = "lm",level = 0.99,color = 'blue',fill = 'blue',alpha = 0.5) +
  stat_function(data = data.frame(x=dbh), aes(x),
                fun=function(x) dbh2ca(b1Ca,b2Ca,x),
                linetype='solid', color = 'red',size = 1)

##########################################################################################################
# AGB
wood.dens  <- 0.71 # g/cm3
b1Bs_small <- 0.2226659060
b2Bs_small <- 2.4323608875
b1Bs_large <- 0.2297666520
b2Bs_large <- 2.4255735874
dbh_crit   <- 96.2577896118

AGB <- 2*dbh2bd(b1Bs_small,b2Bs_small,b1Bs_large,b2Bs_large,dbh_crit,dbh) # kg

df <- data.wytham %>% mutate(AGB = AGV_m*wood.dens*1000) # kg

ggplot() +
  geom_point(data = df,aes(x = dbh_tls,y = AGB)) +
  theme_bw() +
  scale_x_log10() +
  scale_y_log10() +
  xlab("DBH TLS (cm)") +
  ylab("AGB (kg biomass)") +
  geom_smooth(data = df,aes(x = dbh_tls,y = AGB),
                               method = "lm",level = 0.99,color = 'blue',fill = 'blue',alpha = 0.5) +
  stat_function(data = data.frame(x=dbh), aes(x),
                fun=function(x) 2*dbh2bd(b1Bs_small,b2Bs_small,b1Bs_large,b2Bs_large,dbh_crit,x),
                linetype='solid', color = 'red',size = 1)
