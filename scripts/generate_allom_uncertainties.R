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
library(lme4)
library(lattice)
library(tidyr)
library(ggridges)

# Load data
data.file <- "/home/femeunier/Documents/projects/Hackaton/LidarED/data/Wytham_trees_summary_ED2_nodead_add.csv"
data.wytham <- read.csv(data.file,header = TRUE) %>% mutate(TLS_ID = as.character(TLS_ID))

data.file2 <- "/home/femeunier/Documents/projects/Hackaton/LidarED//data/Wytham_trees_summary_ED2.csv"
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
                                    dbh_census = 100*dbh_census) %>% mutate(PFT = case_when(species %in% c("ACERPS") ~ 1,
                                                                                            TRUE ~ 0),
                                                                            PFT.name = case_when(species %in% c("ACERPS") ~ "LH",
                                                                                                 TRUE ~ "MH")) %>%
  mutate(dbh_census = case_when(is.na(dbh_census) ~ dbh_tls,
                                TRUE ~ dbh_census)) %>% filter(species != "")

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

names <- data.wytham.filled %>% group_by(PFT) %>% summarise(N = n(),
                                                            PFT.name = PFT.name[1]) %>% mutate(full.name = paste0(PFT.name," (N = ",N,")"))
data.wytham.filled <- data.wytham.filled %>% left_join(names,by = "PFT") %>% mutate(full.name = as.factor(full.name))
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
data.wytham.bin.sum <- data.wytham.bin %>% group_by(dbh.bin,PFT) %>% summarise(N = length(dbh_tls)) %>%
  left_join(names %>% dplyr::select(-N),by = "PFT") %>% mutate(full.name = as.factor(full.name))

data.wytham.bin.sum$full.name <- factor(data.wytham.bin.sum$full.name, levels(data.wytham.bin.sum$full.name)[c(1,2,3,4,6,5)])



##########################################################################################################
## Figure 2
# Height

N = 100
df.allom.param <- data.frame()
pftnum = c(10,11)
history.file <- "/home/femeunier/Documents/projects/Hackaton/LidarED//data/Wytham_all.xml"

data.wytham.ref <- data.wytham.filled

for (i in seq(1,N)){

  print(i/N)

  tobereplaced <- sample(nrow(data.wytham.ref),round(nrow(data.wytham.ref))/50,replace=TRUE)
  toreplace <- sample(nrow(data.wytham.ref),round(nrow(data.wytham.ref))/50,replace=FALSE)

  data.wytham[tobereplaced,] <- data.wytham.ref[toreplace,]

  df <- data.wytham %>% filter(!is.na(dbh_tls) & !is.na(h))

  href <- hmax <- b1Ht <- b2Ht <- c()
  for (ipft in seq(1,length(pftnum))){
    href[ipft]  <- get_ED_default_pft(history.file,"hgt_ref",pftnum[ipft])
    hmax[ipft]  <- get_ED_default_pft(history.file,"hgt_max",pftnum[ipft])
    b1Ht[ipft]  <- get_ED_default_pft(history.file,"b1Ht",pftnum[ipft])
    b2Ht[ipft]  <- get_ED_default_pft(history.file,"b2Ht",pftnum[ipft])
  }


  m0 <- nlsLM(data = df,
              h ~ pmin(0.99*b1Ht+hgt_ref,hgt_ref + b1Ht*(1 -exp(dbh_tls*b2Ht))),
              start=list(hgt_ref=href[1], b1Ht=b1Ht[1], b2Ht = b2Ht[1]),
              lower = c(-Inf,-Inf,-Inf), control = nls.control(maxiter = 500, tol = 1e-05, minFactor = 1/1024/10,
                                                               printEval = TRUE, warnOnly = TRUE))
  m1 <- nlsLM(data = df,
              h ~ pmin(0.99*(b1Ht + alpha*PFT) + (href + beta*PFT),(hgt_ref + beta*PFT) + (b1Ht + alpha*PFT) *(1 -exp(dbh_tls*(b2Ht + gamma*PFT)))),
              start=list(b1Ht=b1Ht[1], hgt_ref=href[1], b2Ht = b2Ht[1], alpha = 0, beta = 0, gamma = 0),
              lower = c(-Inf,-Inf,-Inf,-Inf,-Inf,-Inf), control = nls.control(maxiter = 500, tol = 1e-05, minFactor = 1/1024/10,
                                                                              printEval = TRUE, warnOnly = TRUE))

  SSF.m0 <- sum((summary(m0)[["residuals"]])**2)
  SSF.m1 <- sum((summary(m1)[["residuals"]])**2)


  dbh_extr <- extremum(data.wytham$dbh_census)
  dbhs <- seq(dbh_extr[1],dbh_extr[2],length.out = 1000)

  coefs.m0 <- coef(m0)
  coefs.m1 <- coef(m1)

  df.allom.param <- bind_rows(list(df.allom.param,
                                   data.frame(num = i,
                                              param = c(names(coefs.m0),"hgt_max"),
                                              value = c(as.numeric(c(coefs.m0,0.99*coefs.m0[2] + coefs.m0[1]))),
                                              model = "onePFT",
                                              allom = "Height",
                                              pft = 11),
                                   data.frame(num = i,
                                              param = rep(c(names(coefs.m1)[1:3],"hgt_max"),2),
                                              value = c(as.numeric(c(coefs.m1[1:3],0.99*coefs.m1[1] + coefs.m1[2])),
                                                        as.numeric(c(coefs.m1[1:3] + coefs.m1[4:6],0.99*(coefs.m1[1] + coefs.m1[4]) + coefs.m1[2] + coefs.m1[5]))),
                                              model = "PFT",
                                              allom = "Height",
                                              pft = sort(rep(c(10,11),4)))
  ))

  df_best0 <- data.frame(dbh = dbhs,
                         hbest = pmin(0.99*coef(m0)[2] + coef(m0)[1],coefs.m0[1] + coefs.m0[2]*(1 -exp(dbhs*coefs.m0[3]))),
                         hdefault = pmin(hmax,href + b1Ht*(1 -exp(dbhs*b2Ht))))

  df_best1 <- data.frame(dbh = c(dbhs,dbhs),
                         PFT = c(rep(0,length(dbhs)),rep(1,length(dbhs)))) %>%
    mutate(hbest = pmin(0.99*(coef(m1)[1] + coef(m1)[4]*PFT) + (coef(m1)[2] + coef(m1)[5]*PFT),
                        (coef(m1)[2] + coef(m1)[5]*PFT) + (coef(m1)[1] + coef(m1)[4]*PFT)*(1 -exp(dbhs*(coef(m1)[3] + coef(m1)[6])))),
           hdefault = pmin(hmax[PFT+1],href[PFT+1] + b1Ht[PFT+1]*(1 -exp(dbhs*b2Ht[PFT+1]))))




  ##################################################################################################"
  # Crown area

  df <- data.wytham %>% filter(!is.na(dbh_tls) & !is.na(CA))

  b1Ca <- b2Ca <- c()
  for (ipft in seq(1,length(pftnum))){
    b1Ca[ipft]  <- get_ED_default_pft(history.file,"b1Ca",pftnum[ipft])
    b2Ca[ipft]  <- get_ED_default_pft(history.file,"b2Ca",pftnum[ipft])
  }

  # xyplot(log(CA) ~ log(dbh_tls), groups=PFT, data=df, type='p')

  m0 <- LM <- lm(data = df,formula =  log(CA) ~ log(dbh_tls))
  m1 <- lmList(log(CA) ~ log(dbh_tls) | PFT, data=df)

  SSF.m0 <- sum((summary(m0)[["residuals"]])**2)
  SSF.m1 <- sum(unlist((summary(m1)[["residuals"]]))**2)


  coefs.CA0 <- coef(m0)
  coefs.CA1 <- coef(m1)

  df.allom.param <- bind_rows(list(df.allom.param,
                                   data.frame(num = i,
                                              param = c("b1Ca","b2Ca"),
                                              value = as.numeric(c(exp(coefs.CA0[1]),coefs.CA0[2])),
                                              model = "onePFT",
                                              allom = "CA",
                                              pft = 11),
                                   data.frame(num = i,
                                              param = rep(c("b1Ca","b2Ca"),2),
                                              value = as.numeric(c(exp(coefs.CA1[1,1]),coefs.CA1[1,2],
                                                                   exp(coefs.CA1[2,1]),coefs.CA1[2,2])),
                                              model = "PFT",
                                              allom = "CA",
                                              pft = sort(rep(c(10,11),2)))
  ))

  df_best_CA0 <- data.frame(dbh = c(dbhs),
                            CAbest = c(exp(coefs.CA0[1])*(dbhs^coefs.CA0[2])),
                            CAdefault = c(b1Ca[1]*(dbhs^b2Ca[1]),b1Ca[2]*(dbhs^b2Ca[2])))

  df_best_CA <- data.frame(dbh = c(dbhs,dbhs),
                           PFT = c(rep(0,length(dbhs)),rep(1,length(dbhs))),
                           CAbest = c(exp(coefs.CA1[1,1])*(dbhs^coefs.CA1[1,2]),
                                      exp(coefs.CA1[2,1])*(dbhs^coefs.CA1[2,2])),
                           CAdefault = c(b1Ca[1]*(dbhs^b2Ca[1]),b1Ca[2]*(dbhs^b2Ca[2])))

  ##################################################################################
  # AGB

  wood.dens <- b1Bs_small <- b2Bs_small <- b1Bs_large <- b2Bs_large <- dbh_crit <- c()

  for (ipft in seq(1,length(pftnum))){
    wood.dens[ipft]  <- get_ED_default_pft(history.file,"rho",pftnum[ipft])

    b1Bs_small[ipft]  <- get_ED_default_pft(history.file,"b1Bs_small",pftnum[ipft])
    b2Bs_small[ipft]  <- get_ED_default_pft(history.file,"b2Bs_small",pftnum[ipft])
    b1Bs_large[ipft]  <- get_ED_default_pft(history.file,"b1Bs_large",pftnum[ipft])
    b2Bs_large[ipft]  <- get_ED_default_pft(history.file,"b2Bs_large",pftnum[ipft])

    dbh_crit[ipft]   <- get_ED_default_pft(history.file,"dbh_crit",pftnum[ipft])
  }

  data.wytham <- data.wytham %>% mutate(AGB = AGV_m*wood.dens[PFT+1]*1000)

  # plot(dbhs,b1Bs_small[1]*(dbhs)**b2Bs_small[1],log = "xy",type = "l")

  df <- data.wytham %>% filter(!is.na(dbh_tls) & !is.na(AGB))

  m0 <- lm(data = data.wytham,formula = log(AGB) ~ log(dbh_tls))
  m1 <- lmList(log(AGB) ~ log(dbh_tls) | PFT, data=df)

  SSF.m0 <- sum((summary(m0)[["residuals"]])**2)
  SSF.m1 <- sum(unlist((summary(m1)[["residuals"]]))**2)


  coefs.AGB0 <- coef(m0)
  coefs.AGB1 <- coef(m1)

  df.allom.param <- bind_rows(list(df.allom.param,
                                   data.frame(num = i,
                                              param = c("b1Bs_small","b2Bs_small","b1Bs_large","b2Bs_large"),
                                              value = as.numeric(c(exp(coefs.AGB0[1])/0.7,coefs.AGB0[2]),
                                                                 c(exp(coefs.AGB0[1])/0.7,coefs.AGB0[2])),
                                              model = "onePFT",
                                              allom = "Bs",
                                              pft = 11),
                                   data.frame(num = i,
                                              param = rep(c("b1Bs_small","b2Bs_small","b1Bs_large","b2Bs_large"),2),
                                              value = as.numeric(c(exp(coefs.AGB1[1,1])/0.7,coefs.AGB1[1,2],
                                                                   exp(coefs.AGB1[1,1])/0.7,coefs.AGB1[1,2],
                                                                   exp(coefs.AGB1[2,1])/0.7,coefs.AGB1[2,2],
                                                                   exp(coefs.AGB1[2,1])/0.7,coefs.AGB1[2,2])),
                                              model = "PFT",
                                              allom = "Bs",
                                              pft = sort(rep(c(10,11),4)))
  ))





  ####################################################################################################################
  # Leaf area

  b1Bl_small <- b2Bl_small <- b1Bl_large <- b2Bl_large <- SLA_default <- c()

  for (ipft in seq(1,length(pftnum))){

    b1Bl_small[ipft]  <- get_ED_default_pft(history.file,"b1Bl_small",pftnum[ipft])
    b2Bl_small[ipft]  <- get_ED_default_pft(history.file,"b2Bl_small",pftnum[ipft])
    b1Bl_large[ipft]  <- get_ED_default_pft(history.file,"b1Bl_large",pftnum[ipft])
    b2Bl_large[ipft]  <- get_ED_default_pft(history.file,"b2Bl_large",pftnum[ipft])

    SLA_default[ipft]   <- get_ED_default_pft(history.file,"SLA",pftnum[ipft])
  }

  SLA_best = c(25.8,31.36889) # before 29.6
  SLA_single_pft <- 29.30672

  Bl <- c(dbh2bl(b1Bl_small[1],b2Bl_small[1],b1Bl_large[1],b2Bl_large[1],dbh_crit[1],dbhs),
          dbh2bl(b1Bl_small[2],b2Bl_small[2],b1Bl_large[2],b2Bl_large[2],dbh_crit[2],dbhs)) # kgC
  LA <- c(dbh2bl(b1Bl_small[1],b2Bl_small[1],b1Bl_large[1],b2Bl_large[1],dbh_crit[1],dbhs)*SLA_best[1],
          dbh2bl(b1Bl_small[2],b2Bl_small[2],b1Bl_large[2],b2Bl_large[2],dbh_crit[2],dbhs)*SLA_best[2])

  # plot(dbhs,dbh2bl(b1Bl_small[2],b2Bl_small[2],b1Bl_large[2],b2Bl_large[2],dbh_crit[2],dbhs)*SLA_best[2],log = "xy")

  m0 <- lm(data = data.wytham,formula = log(LA) ~ log(dbh_tls))
  m1 <- lmList(log(LA) ~ log(dbh_tls) | PFT, data=df)

  SSF.m0 <- sum((summary(m0)[["residuals"]])**2)
  SSF.m1 <- sum(unlist((summary(m1)[["residuals"]]))**2)


  coefs.LA0 <- coef(m0)
  coefs.LA1 <- coef(m1)

  df.allom.param <- bind_rows(list(df.allom.param,
                                   data.frame(num = i,
                                              param = c("b1Bl_small","b2Bl_small","b1Bl_large","b2Bl_large"),
                                              value = as.numeric(c(exp(coefs.LA0[1])*2,coefs.LA0[2],
                                                                   exp(coefs.LA0[1])*2,coefs.LA0[2])),
                                              model = "onePFT",
                                              allom = "Bl",
                                              pft = 11),
                                   data.frame(num = i,
                                              param = rep(c("b1Bl_small","b2Bl_small","b1Bl_large","b2Bl_large"),2),
                                              value = as.numeric(c(exp(coefs.LA1[1,1])*2,coefs.LA1[1,2],
                                                                   exp(coefs.LA1[1,1])*2,coefs.LA1[1,2],
                                                                   exp(coefs.LA1[2,1])*2,coefs.LA1[2,2],
                                                                   exp(coefs.LA1[2,1])*2,coefs.LA1[2,2])),
                                              model = "PFT",
                                              allom = "Bl",
                                              pft = sort(rep(c(10,11),4)))))





}


ggplot(data = df.allom.param %>% filter(allom == "Bl",
                                        param %in% c("b1Bl_small","b2Bl_small"))) +
  geom_boxplot(aes(x = interaction(model,as.factor(pft)),y = value)) +
  facet_wrap(~as.factor(param)) +
  theme_bw()


saveRDS(df.allom.param,"/home/femeunier/Documents/projects/Hackaton/LidarED/data/df.allom.param.all.RDS")
system2("scp",c("/home/femeunier/Documents/projects/Hackaton/LidarED/data/df.allom.param.all.RDS",
                "hpc:/scratch/gent/vo/000/gvo00074/hackaton/run_Wytham/ref/data/"))
