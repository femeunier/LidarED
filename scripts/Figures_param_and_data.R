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

subplotB <- ggplot(data.wytham.filled,
       aes(x = x, y = y - 100, size = BA/10000,
          color = as.factor(full.name))) +
  geom_point(alpha = 0.5) +
  xlab("x (m)") +
  ylab("y (m)") +
  labs(fill = "Species",color = "Species") +
  scale_y_continuous(breaks = seq(0,100,50)) +
  scale_x_continuous(breaks = seq(0,155,50),limits = c(0,150)) +
  theme_bw()+
  scale_color_manual(values = c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02')) +
  theme(text = element_text(size = 24)) +
  coord_fixed(ratio = 1) +
  guides(size = FALSE,color = FALSE,fill = FALSE)


ggsave(plot = subplotB,"./Figures/Figure1b.png",
       dpi = 300,width = 6,height = 4.5)


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



subplotA <- ggplot(data = data.wytham.bin.sum) +
  geom_bar(aes(x = as.factor(dbh.bin),y = N,fill = as.factor(full.name)), stat = "identity",
           alpha = 0.7) +
  scale_fill_manual(values = c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02')) +
  labs(x = "DBH (cm)",y = "Number of stems (-)") +
  labs(fill = "Species") +
  scale_x_discrete(breaks = seq(0,(-1 + length(unique(data.wytham.bin.sum$dbh.bin)))),
                     labels = c("1-10","10-20","20-30","30-40",
                                "40-50","50-60","60-70","70-80","80-90","90-100",">100") ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        text = element_text(size = 24),
        legend.position = c(0.67,0.7))

ggsave(plot = subplotA,"./Figures/Figure1a.png",
      dpi = 300,width = 8,height = 5)


##########################################################################################################
## Figure 2
# Height

pftnum = 10
df <- data.wytham %>% filter(!is.na(dbh_tls) & !is.na(h))

history.file <- "./data/Wytham.xml"

href  <- get_ED_default_pft(history.file,"hgt_ref",pftnum)
hmax  <- get_ED_default_pft(history.file,"hgt_max",pftnum)
isTropi  <- get_ED_default_pft(history.file,"is_tropical",pftnum)
b1Ht  <- get_ED_default_pft(history.file,"b1Ht",pftnum)
b2Ht  <- get_ED_default_pft(history.file,"b2Ht",pftnum)

m0 <- nlsLM(data = df,
            h ~ pmin(0.99*b1Ht+href,href + b1Ht*(1 -exp(dbh_tls*b2Ht))),
            start=list(href=href, b1Ht=b1Ht, b2Ht = b2Ht),
            lower = c(-Inf,-Inf,-Inf), control = nls.control(maxiter = 500, tol = 1e-05, minFactor = 1/1024/10,
                                                                                 printEval = TRUE, warnOnly = TRUE))
RMSE(m0)
1- sum(m0$m$resid()**2)/sum((df$h - mean(df$h))**2)

residuals0 <- (href + b1Ht*(1 -exp(df$dbh_tls*b2Ht)) - df$h)

100*mean(m0$m$resid()/df$h,na.rm = TRUE)
100*mean(residuals0/df$h)

dbh_extr <- extremum(data.wytham$dbh_census)
dbhs <- seq(dbh_extr[1],dbh_extr[2],length.out = 1000)

coefs.m0 <- coef(m0)
df_best <- data.frame(dbh = dbhs,
                      hbest = pmin(0.99*coef(m0)[2] + coef(m0)[1],coefs.m0[1] + coefs.m0[2]*(1 -exp(dbhs*coefs.m0[3]))),
                      hdefault = pmin(hmax,href + b1Ht*(1 -exp(dbhs*b2Ht))))

(df_best$hbest-df_best$hdefault)

subplotA <- ggplot(data = data.wytham.filled) +
  geom_point(aes(x = dbh_tls,y = h,color = as.factor(species))) +
  geom_line(data = df_best,
            aes(x = dbh, y = hdefault),color = "darkgrey",linetype = 1,size = 1.5) +
  geom_line(data = df_best,
            aes(x = dbh, y = hbest),color = "black",linetype = 1,size = 1.5) +
  labs(x = "DBH (cm)",y = "Height (m)",color = "Species") +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw() +
  guides(color = FALSE) +
  scale_color_manual(values = c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02')) +
  theme(text = element_text(size = 24),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))


# Crown area


df <- data.wytham %>% filter(!is.na(dbh_tls) & !is.na(CA))

b1Ca <- get_ED_default_pft(history.file,"b1Ca",pftnum)
b2Ca <- get_ED_default_pft(history.file,"b2Ca",pftnum)

m0 <- LM <- lm(data = df,formula =  log(CA) ~ log(dbh_tls))

RSE <- sqrt(sum(m0$residuals**2)/(nrow(df) - 2))
CF <- exp(RSE**2/2)
predicted <- exp(predict(m0))*CF

residuals <- df$CA-predicted
residuals0 <- log(b1Ca*(df$dbh_tls**b2Ca)) - log(df$CA)

RMSE(m0)
sqrt(mean(residuals0^2))

summary(m0)$r.squared
1-sum(residuals0^2)/sum((log(df$CA)-mean(log(df$CA)))^2)

100*mean(exp(m0$residuals)/(df$CA))
100*mean(exp(residuals0)/(df$CA))


coefs.CA <- coef(m0)
df_best_CA <- data.frame(dbh = dbhs,
                         CAbest = exp(coefs.CA[1])*(dbhs^coefs.CA[2]),
                         CAdefault = b1Ca*(dbhs^b2Ca))

tp <- (df_best_CA$CAbest-df_best_CA$CAdefault)/df_best_CA$CAdefault

subplotB <- ggplot() +
  geom_point(data = data.wytham.filled,
             aes(x = dbh_tls,y = CA,color = as.factor(species))) +
  geom_line(data = df_best_CA,
            aes(x = dbh, y = CAbest),color = "black",linetype = 1,size = 1.5) +
  geom_line(data = df_best_CA,
            aes(x = dbh, y = CAdefault),color = "darkgrey",linetype = 1,size = 1.5) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("DBH (cm)") +
  ylab("Crown area (m²)") +
  theme_bw() +
  guides(color = FALSE) +
  scale_color_manual(values = c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02')) +
  theme(text = element_text(size = 24),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))

# AGB
wood.dens  <- get_ED_default_pft(history.file,"rho",pftnum)

b1Bs_small <- get_ED_default_pft(history.file,"b1Bs_small",pftnum)*0.7
b2Bs_small <- get_ED_default_pft(history.file,"b2Bs_small",pftnum)
b1Bs_large <- get_ED_default_pft(history.file,"b1Bs_large",pftnum)*0.7
b2Bs_large <- get_ED_default_pft(history.file,"b2Bs_large",pftnum)

dbh_crit   <- get_ED_default_pft(history.file,"dbh_crit",pftnum)

data.wytham.filled <- data.wytham.filled %>% mutate(AGB = AGV_m*wood.dens*1000)

m0 <- lm(data = data.wytham.filled,formula = log(AGB) ~ log(dbh_tls))
residuals1 <- (b1Bs_small*(data.wytham.filled$dbh_tls**b2Bs_large)) - (data.wytham.filled$AGB)
residuals0 <- log(b1Bs_small*(data.wytham.filled$dbh_tls**b2Bs_large)) - log(data.wytham.filled$AGB)

RMSE(m0)
sqrt(mean(residuals0^2))

100*mean(exp(m0$residuals)/(data.wytham.filled$AGB))
100*mean((residuals1)/(data.wytham.filled$AGB))

summary(m0)$r.squared
1-sum(residuals0^2)/sum((log(data.wytham.filled$AGB)-mean(log(data.wytham.filled$AGB)))^2)

coefs.AGB <- coef(m0)
df_best_AGB <- data.frame(dbh = dbhs,
                          AGBbest = exp(coefs.AGB[1])*(dbhs^coefs.AGB[2]),
                          AGBdefault = b1Bs_large*(dbhs^b2Bs_large))

mean((df_best_AGB$AGBbest-df_best_AGB$AGBdefault)/df_best_AGB$AGBdefault)

subplotC <- ggplot() +
  geom_point(data = data.wytham.filled,
             aes(x = dbh_tls,y = AGB,color = as.factor(species))) +
  scale_x_log10() +
  scale_y_log10() +
  geom_line(data = df_best_AGB,
            aes(x = dbh, y = AGBdefault),color = "darkgrey",linetype = 1,size = 1.5) +
  geom_line(data = df_best_AGB,
            aes(x = dbh, y = AGBbest),color = "black",linetype = 1,size = 1.5) +
  xlab("DBH (cm)") +
  ylab("Aboveground Woody \n Biomass (kg)") +
  theme_bw() +
  guides(color = FALSE) +
  scale_color_manual(values = c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02')) +
  theme(text = element_text(size = 24),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))


# Leaf area

b1Bl_small <- get_ED_default_pft(history.file,"b1Bl_small",pftnum) #kg biomass
b2Bl_small <- get_ED_default_pft(history.file,"b2Bl_small",pftnum)
b1Bl_large <- get_ED_default_pft(history.file,"b1Bl_large",pftnum)
b2Bl_large <- get_ED_default_pft(history.file,"b2Bl_large",pftnum)
SLA_default <- get_ED_default_pft(history.file,"SLA",pftnum) # m²/kgC

Bl <- dbh2bl(b1Bl_small,b2Bl_small,b1Bl_large,b2Bl_large,dbh_crit,dbhs) # kgC
LA <- Bl*SLA_default
SLA_best = 25.1 # before 29.6

m0 <- lm(data = data.wytham.filled,formula = log(LA) ~ log(dbh_tls))
m1 <- lm(data = data.wytham.filled,formula = log(LA/SLA_best*2) ~ log(dbh_tls))

residuals0 <- (b1Bl_small*(data.wytham.filled$dbh_tls**b2Bl_large)) - (df$LA)/SLA_best*2

RMSE(m1)
sqrt(mean(residuals0^2))

100*mean((exp(m0$residuals)/SLA_best*2)/((data.wytham.filled$LA/SLA_best*2)))
100*mean(residuals0/((data.wytham.filled$LA/SLA_best*2)))

summary(m0)$r.squared
1-sum(residuals0^2)/sum((log(df$LA)-mean(log(df$LA)))^2)


coefs.LA <- coef(m0)
df_best_LA <- data.frame(dbh = dbhs,
                         LAbest = exp(coefs.LA[1])*(dbhs^coefs.LA[2]),
                         LAdefault = LA)

((df_best_LA$LAbest/SLA_best)-(df_best_LA$LAdefault/SLA_default))/(df_best_LA$LAdefault/SLA_default)

exp(coefs.LA[1])/SLA_best*2

subplotD <- ggplot() +
  geom_point(data = data.wytham.filled,
             aes(x = dbh_tls,y = LA/SLA_best*2,color = as.factor(species))) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("DBH (cm)") +
  ylab("Leaf biomass (kg)") +
  geom_line(data = df_best_LA,
            aes(x = dbh, y = LAbest/SLA_best*2),color = "black",linetype = 1,size = 1.5) +
  geom_line(data = df_best_LA,
            aes(x = dbh, y = LAdefault/SLA_default*2),color = "darkgrey",linetype = 1,size = 1.5) +
  # geom_smooth(data = data.wytham2,aes(x = dbh_tls,y = leaf_area),
  #             method = "lm",level = 0.99,color = 'blue',fill = 'blue',alpha = 0.5) +
  # stat_function(data = data.frame(x=dbh), aes(x),
  #               fun=function(x) dbh2bl(b1Bl_small,b2Bl_small,b1Bl_large,b2Bl_large,dbh_crit,x)*SLA,
  #               linetype='solid', color = 'red',size = 1) +
  theme_bw() +
  guides(color = FALSE) +
  scale_color_manual(values = c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02',"#e6ab02")) +
  theme(text = element_text(size = 24),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))

subplotD

plot_grid(subplotC,subplotA,subplotB,subplotD,nrow = 2,align = "hv")

ggsave(plot = last_plot(),"./Figures/Figure2.png",
       dpi = 300,width = 12,height = 8)
