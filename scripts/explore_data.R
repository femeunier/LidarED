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

############################################################################
# Final outputs
ED2_config_params <- list()

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
# census plot

ggplot(data.wytham,
       aes(x = x, y = y, size = dbh_census)) +
  geom_point(alpha = 0.5, col = "black") +
  xlab("x (m)") +
  ylab("y (m)") +
  theme_bw()

ggsave(plot = last_plot(),
       file = "./Figures/census.png")


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
# Height

df <- data.wytham %>% filter(!is.na(dbh_tls) & !is.na(h))

history.file <- "./data/Wytham.xml"

href  <- get_ED_default_pft(history.file,"hgt_ref",pftnum)
hmax  <- get_ED_default_pft(history.file,"hgt_max",pftnum)
isTropi  <- get_ED_default_pft(history.file,"is_tropical",pftnum)
b1Ht  <- get_ED_default_pft(history.file,"b1Ht",pftnum)
b2Ht  <- get_ED_default_pft(history.file,"b2Ht",pftnum)

m0 <- nlsLM(data = df,
            h ~ href + b1Ht*(1 -exp(dbh_tls*b2Ht)),
            start=list(href=href, b1Ht=b1Ht, b2Ht = b2Ht), control = nls.control(maxiter = 500, tol = 1e-05, minFactor = 1/1024/10,
                                                                        printEval = TRUE, warnOnly = TRUE))

m1 <- nlsLM(data = df,
            hmean ~ href + b1Ht*(1 -exp(dbh_tls*b2Ht)),
            start=list(href=href, b1Ht=b1Ht, b2Ht = b2Ht), control = nls.control(maxiter = 500, tol = 1e-05, minFactor = 1/1024/10,
                                                                                 printEval = TRUE, warnOnly = TRUE))



dbh_extr <- extremum(data.wytham$dbh_census)
dbh <- seq(dbh_extr[1],dbh_extr[2],length.out = 1000)

png(file = "./Figures/H.png",width = 3200, height = 1600, units = "px", res = 300)
par(mfrow = c(1,2),mar=c(5,4,4,2))
plotFit(m0,interval="prediction",pch=1,col.pred=adjustcolor("blue", 0.5),shade=TRUE,ylim=c(0,35),col.fit = "blue",
        xlab = "DBH TLS (cm)", ylab = 'hmax (m)')
lines(dbh,pmin(hmax,dbh2h(href,b1Ht,b2Ht,dbh,isTropi)),col="red",lty=1,type='l')

plotFit(m1,interval="prediction",pch=1,col.pred=adjustcolor("blue", 0.5),shade=TRUE,ylim=c(0,35),col.fit = "blue",
        xlab = "DBH TLS (cm)", ylab = 'hmean (m)')
lines(dbh,pmin(hmax,dbh2h(href,b1Ht,b2Ht,dbh,isTropi)),col="red",lty=1,type='l')

dev.off()

ED2_config_params[["dbh_h"]] <- split(unname(coef(m0)),c("hgt_ref", "b1Ht", "b2Ht"))
ED2_config_params[["dbh_hmean"]] <- split(unname(coef(m1)),c("hgt_ref", "b1Ht", "b2Ht"))

##########################################################################################################
# Hmean vs Hmax
par(mfrow = c(1,1))

ggplot(data.wytham,
       aes(x = hmean, y = h,col=species)) +
  geom_abline(slope = 1,intercept = 0,col='black',linetype=2) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm",level = 0.99,color = 'blue',alpha = 0.5,fill = 'blue') +
  xlab("averaged H (m)") +
  ylab("max H (m)") +
  scale_x_continuous(limits = c(0,max(data.wytham$h))) +
  scale_y_continuous(limits = c(0,max(data.wytham$h))) +
  theme_bw()

LM <- lm(data = data.wytham,formula = h ~ hmean)
ggsave(plot = last_plot(),
       file = "./Figures/comp_H.png")

##########################################################################################################
# Crown area allometries

df <- data.wytham %>% filter(!is.na(dbh_tls) & !is.na(CA))

b1Ca <- get_ED_default_pft(history.file,"b1Ca",pftnum)
b2Ca <- get_ED_default_pft(history.file,"b2Ca",pftnum)

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

LM <- lm(data = df,formula =  log(CA) ~ log(dbh_tls))
b1Ca <- exp(coef(LM)[1])
b2Ca <- coef(LM)[2]

ED2_config_params[["dbh_CA"]] <- list(b1Ca = b1Ca, b2Ca = b2Ca)
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
  scale_x_log10(limits = c(1,max(df$dbh_tls))) +
  scale_y_log10() +
  xlab("DBH TLS (cm)") +
  ylab("AGB (kg biomass)") +
  geom_smooth(data = df,aes(x = dbh_tls,y = AGB),
                               method = "lm",level = 0.99,color = 'blue',fill = 'blue',alpha = 0.5) +
  stat_function(data = data.frame(x=dbh), aes(x),
                fun=function(x) 2*dbh2bd(b1Bs_small,b2Bs_small,b1Bs_large,b2Bs_large,dbh_crit,x),
                linetype='solid', color = 'red',size = 1) +
  geom_line(data = AGB_df, mapping = aes(x = dbh, y = AGB, color = as.factor(sp)))


LM <- lm(data = df,formula =  log(AGB) ~ log(dbh_tls))
b1Bs <- exp(coef(LM)[1])
b2Bs <- coef(LM)[2]

ggsave(plot = last_plot(),
       file = "./Figures/AGB.png")

ED2_config_params[["dbh_AGB"]] <- list(b1Bs_small = b1Bs, b2Bs_small = b2Bs,
                                       b1Bs_large = b1Bs, b2Bs_large = b2Bs)

saveRDS(object = ED2_config_params,file = './data/all_configs.RDS')


##################################################################################
# Leaf area allometries

b1Bl <- get_ED_default_pft(history.file,"b1Bl_large",pftnum)
b2Bl <- get_ED_default_pft(history.file,"b2Bl_large",pftnum)
SLA_ED <- get_ED_default_pft(history.file,"SLA",pftnum) # m2/kgC
SLA <- 36.7 # From TRY m²/kgC
SLA_biomass <- SLA/2

df <- data.wytham %>% filter(!is.na(dbh_tls) & !is.na(LA)) %>% mutate(Bl = LA/SLA_biomass) # kg

ggplot() +
  geom_point(data = df,aes(x = dbh_tls,y = LA)) +
  theme_bw() +
  scale_x_log10() +
  scale_y_log10() +
  xlab("DBH TLS (cm)") +
  ylab("Leaf area (m²)") +
  geom_smooth(data = df,aes(x = dbh_tls,y = LA),
              method = "lm",level = 0.99,color = 'blue',fill = 'blue',alpha = 0.5) +
  stat_function(data = data.frame(x=dbh), aes(x),
                fun=function(x) SLA_ED/2*b1Bl*(x**b2Bl),
                linetype='solid', color = 'red',size = 1)

ggsave(plot = last_plot(),
       file = "./Figures/Bl.png")

LM <- lm(data = df,formula =  log(Bl) ~ log(dbh_tls))
b1Bl <- exp(coef(LM)[1])
b2Bl <- coef(LM)[2]

ED2_config_params[["dbh_Bl"]] <- list(b1Bl_small = b1Bl, b2Bl_small = b2Bl,b1Bl_large = b1Bl,b2Bl_large = b2Bl)

#################################################################
# Save

saveRDS(ED2_config_params,file = "./data/all_configs.RDS")
