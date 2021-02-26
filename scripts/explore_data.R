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

# Load data
data.file <- "./data/Wytham_trees_summary_ED2.csv"
data.wytham <- read.csv(data.file,header = TRUE)

data.wytham <- data.wytham %>% rename(id = TLS_ID,  x =  stemlocx_.m.,
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

ggsave(plot = last_plot(),
       file = "./Figures/dbh.png")

##########################################################################################################
  # Mid temperate deciduous, better choice?
pftnum = 10
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

history.file <- "./data/Wytham.xml"

href  <- get_ED_default_pft(history.file,"hgt_ref",pftnum)
hmax  <- get_ED_default_pft(history.file,"hgt_max",pftnum)
isTropi  <- get_ED_default_pft(history.file,"is_tropical",pftnum)
b1Ht  <- get_ED_default_pft(history.file,"b1Ht",pftnum)
b2Ht  <- get_ED_default_pft(history.file,"b2Ht",pftnum)

m0 <- nlsLM(data = df,
            h ~ href + b1Ht*(1 -exp(dbh_tls*b2Ht)),
            start=list(href=href, b1Ht=b1Ht, b2Ht = b2Ht),
            lower=c(1.3,24,-10),
            control = nls.control(maxiter = 500, tol = 1e-05, minFactor = 1/1024/10,
                                                                        printEval = TRUE, warnOnly = TRUE))
dbh_extr <- extremum(data.wytham$dbh_census)
dbh <- seq(dbh_extr[1],dbh_extr[2],length.out = 1000)

plotFit(m0,interval="prediction",pch=1,col.pred=adjustcolor("blue", 0.5),shade=T,ylim=c(0,35),col.fit = "blue",
        xlab = "DBH TLS (cm)", ylab = 'h (m)')
lines(dbh,pmin(hmax,dbh2h(href,b1Ht,b2Ht,dbh,isTropi)),col="red")

##########################################################################################################
# Crown area allometries

df <- data.wytham %>% filter(!is.na(dbh_tls) & !is.na(CA))

b1Ca <- get_ED_default_pft(history.file,"b1Ca",pftnum)
b2Ca <- get_ED_default_pft(history.file,"b2Ca",pftnum)

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

ggsave(plot = last_plot(),
       file = "./Figures/CA.png")


##########################################################################################################
# AGB
wood.dens  <- get_ED_default_pft(history.file,"rho",pftnum)

b1Bs_small <- get_ED_default_pft(history.file,"b1Bs_small",pftnum)
b2Bs_small <- get_ED_default_pft(history.file,"b2Bs_small",pftnum)
b1Bs_large <- get_ED_default_pft(history.file,"b1Bs_large",pftnum)
b2Bs_large <- get_ED_default_pft(history.file,"b2Bs_large",pftnum)

dbh_crit   <- get_ED_default_pft(history.file,"dbh_crit",pftnum)

AGB <- 2*dbh2bd(b1Bs_small,b2Bs_small,b1Bs_large,b2Bs_large,dbh_crit,dbh) # kg


a = c(-5.308133,-5.644074,-5.248602,-5.34768)
b = c(2.488218,2.5189,2.468257,2.47536)
species = c( "FRAXEX","ACERPS","QUERRO","ELSE")
AGB_spp <- purrr::map2(a,b,dbh2agb_exp,dbh)
names(AGB_spp) <- species
AGB_spp <- as.data.frame(AGB_spp)
AGB_df <- data.frame()
for (i in seq(1,length(a))){
  AGB_df <- rbind(AGB_df,
                  data.frame(dbh = dbh,
                             AGB = AGB_spp[,i],
                             sp = species[i]))
}


df <- data.wytham %>% mutate(AGB = AGV_m*wood.dens*1000) # --> kg

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
                linetype='solid', color = 'red',size = 1) +
  geom_line(data = AGB_df, mapping = aes(x = dbh, y = AGB, color = as.factor(sp)))

m0 <- nlsLM(data = df,
            AGB ~ b1Bs*(dbh_tls^b2Bs),
            start=list(b1Bs=b1Bs_large, b2Bs=b2Bs_large), control = nls.control(maxiter = 500, tol = 1e-05, minFactor = 1/1024/10,
                                                                    printEval = TRUE, warnOnly = TRUE))

LM<- lm(data = df,formula=log(AGB) ~ log(dbh_tls))
c(exp(coef(LM)[1]),coef(LM)[2])

ggsave(plot = last_plot(),
       file = "./Figures/AGB.png")


###############################################################################################################
data.file <- "./data/Wytham_nodead.csv"
data.wytham <- read.csv(data.file,header = TRUE)

data.wytham2 <- data.wytham %>% rename(id = TLS_ID,  x =  stemlocx_.m._x,
                                      y =  stemlocy_.m._x,
                                      dbh_tls = DBH_TLS_.m._x,
                                      h = Hgt_pts_.m._x,
                                      dbh_census = DBH_census_.m._x,
                                      leaf_area = leaf_area) %>% mutate(dbh_tls = 100*dbh_tls,
                                                                              dbh_census = 100*dbh_census)



b1Bl_small <- get_ED_default_pft(history.file,"b1Bl_small",pftnum)
b2Bl_small <- get_ED_default_pft(history.file,"b2Bl_small",pftnum)
b1Bl_large <- get_ED_default_pft(history.file,"b1Bl_large",pftnum)
b2Bl_large <- get_ED_default_pft(history.file,"b2Bl_large",pftnum)
SLA <- get_ED_default_pft(history.file,"SLA",pftnum)

Bl <- dbh2bl(b1Bl_small,b2Bl_small,b1Bl_large,b2Bl_large,dbh_crit,dbh) # kgC
LA <- Bl*SLA

ggplot() +
  geom_point(data = data.wytham2,aes(x = dbh_tls,y = leaf_area)) +
  theme_bw() +
  scale_x_log10() +
  scale_y_log10() +
  xlab("DBH TLS (cm)") +
  ylab("Leaf area (m²/plant)") +
  geom_smooth(data = data.wytham2,aes(x = dbh_tls,y = leaf_area),
              method = "lm",level = 0.99,color = 'blue',fill = 'blue',alpha = 0.5) +
  stat_function(data = data.frame(x=dbh), aes(x),
                fun=function(x) dbh2bl(b1Bl_small,b2Bl_small,b1Bl_large,b2Bl_large,dbh_crit,x)*SLA,
                linetype='solid', color = 'red',size = 1)

