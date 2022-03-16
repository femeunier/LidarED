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

subplotB <- ggplot(data.wytham.filled,
                   aes(x = x, y = y - 100, size = BA/10000,
                       color = as.factor(PFT))) +
  geom_point(alpha = 0.5) +
  xlab("x (m)") +
  ylab("y (m)") +
  labs(fill = "Species",color = "Species") +
  scale_y_continuous(breaks = seq(0,100,50)) +
  scale_x_continuous(breaks = seq(0,155,50),limits = c(0,150)) +
  theme_bw()+
  scale_color_manual(values = c('#44CC29','#137300')) +
  theme(text = element_text(size = 24)) +
  coord_fixed(ratio = 1) +
  guides(size = FALSE,color = FALSE,fill = FALSE)

subplotB

ggsave(plot = subplotB,"/home/femeunier/Documents/projects/Hackaton/LidarED/Figures/Figure1bPFT.png",
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
data.wytham.bin.sum <- data.wytham.bin %>% group_by(dbh.bin,PFT) %>% summarise(N = length(dbh_tls)) %>%
  left_join(names %>% dplyr::select(-N),by = "PFT") %>% mutate(full.name = as.factor(full.name))

data.wytham.bin.sum$full.name <- factor(data.wytham.bin.sum$full.name, levels(data.wytham.bin.sum$full.name)[c(1,2,3,4,6,5)])


# levels(data.wytham.bin.sum$full.name)[2] <- "MH (N = 283)"


subplotA <- ggplot(data = data.wytham.bin.sum) +
  geom_bar(aes(x = as.factor(dbh.bin),y = N,fill = as.factor(full.name)), stat = "identity",
           alpha = 0.7) +
  scale_fill_manual(values = c('#137300','#44CC29')) +
  labs(x = "DBH (cm)",y = "Number of stems (-)") +
  labs(fill = "PFT") +
  scale_x_discrete(breaks = seq(0,(-1 + length(unique(data.wytham.bin.sum$dbh.bin)))),
                   labels = c("1-10","10-20","20-30","30-40",
                              "40-50","50-60","60-70","70-80","80-90","90-100",">100") ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        text = element_text(size = 24),
        legend.position = c(0.8,0.8))

subplotA

ggsave(plot = subplotA,"/home/femeunier/Documents/projects/Hackaton/LidarED/Figures/Figure1aPFT.png",
       dpi = 300,width = 8,height = 5)


##########################################################################################################
## Figure 2
# Height

pftnum = c(10,11)
df <- data.wytham %>% filter(!is.na(dbh_tls) & !is.na(h))

history.file <- "/home/femeunier/Documents/projects/Hackaton/LidarED//data/Wytham_all2.xml"

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

A <- lm(data.frame(x = predict(m1),y = df$h),formula = y ~ x)
summary(A)$adj.r.squared

SSF.m0 <- sum((summary(m0)[["residuals"]])**2)
SSF.m1 <- sum((summary(m1)[["residuals"]])**2)

df.residuals <- df.allom.param <- data.frame()
df.residuals <- bind_rows(list(df.residuals,
                               data.frame(SSQ = c(SSF.m0,SSF.m1),
                                          allom = "Height",
                                          model = c("onePFT","PFTs"))))

dbh_extr <- extremum(data.wytham$dbh_census)
dbhs <- seq(dbh_extr[1],dbh_extr[2],length.out = 1000)

coefs.m0 <- coef(m0)
coefs.m1 <- coef(m1)

df.allom.param <- bind_rows(list(df.allom.param,
                                 data.frame(param = c(names(coefs.m0),"hgt_max"),
                                            value = c(as.numeric(c(coefs.m0,0.99*coefs.m0[2] + coefs.m0[1]))),
                                            model = "onePFT",
                                            allom = "Height",
                                            pft = 11),
                                 data.frame(param = rep(c(names(coefs.m1)[1:3],"hgt_max"),2),
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

df.residual <- df %>% mutate(hbest = pmin(0.99*(coef(m1)[1] + coef(m1)[4]*PFT) + (coef(m0)[2] + coef(m1)[5]*PFT),
                                          (coef(m1)[2] + coef(m1)[5]*PFT) + (coef(m1)[1] + coef(m1)[4]*PFT)*(1 -exp(dbh_tls*(coef(m1)[3] + coef(m1)[6])))),
                             hdefault = pmin(hmax[PFT+1],href[PFT+1] + b1Ht[PFT+1]*(1 -exp(dbh_tls*b2Ht[PFT+1]))),
                             h0 = pmin(0.99*coef(m0)[2] + coef(m0)[1],coefs.m0[1] + coefs.m0[2]*(1 -exp(dbh_tls*coefs.m0[3])))) %>%
  mutate(residual_best = h - hbest,
         residual_default = h - hdefault,
         residual0 = h - h0) %>%
  dplyr::select(dbh_tls,PFT,h,residual_best,residual_default,residual0)

df.residual %>% ungroup() %>%
  summarise(RMSE1 = sqrt(sum(residual_best**2)/nrow(df.residual))/mean(df$h),
            RMSE0 = sqrt(sum(residual0**2)/nrow(df.residual))/mean(df$h))

df.residual %>% group_by(PFT) %>%
  summarise(RMSE1 = sqrt(sum(residual_best**2)/length(residual_best))/mean(h))

df.residual.long <- df.residual %>% pivot_longer(cols = c("residual_best","residual_default","residual0"),
                                                 names_to = "type",
                                                 values_to = "residual") %>%
  mutate(hfit = h - residual)

df.residual.long %>% mutate(PFT = case_when(type %in% c("residual_default","residual0") ~ 0,
                                            TRUE ~ PFT)) %>%
  group_by(type,PFT) %>% summarise(RMSE = sqrt(sum(residual**2)/length(residual)),
                                   m = mean(h),
                                   RMSD = RMSE/m)


subplotAbis <- ggplot(data = data.wytham.filled) +
  geom_point(aes(x = dbh_tls,y = h,color = as.factor(PFT)),alpha = 0.4) +
  # geom_line(data = df_best1 %>% filter(PFT == 0),
  #           aes(x = dbh, y = hdefault), color = 'black',linetype = 1,size = 1) +
  geom_line(data = df_best1,
            aes(x = dbh, y = hbest, color = as.factor(PFT)),linetype = 1,size = 1.5) +
  geom_line(data = df_best0,
            aes(x = dbh, y = hbest), color = 'black',linetype = 1,size = 1.5) +
  labs(x = "DBH (cm)",y = "Height (m)",color = "Species") +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw() +
  guides(color = FALSE) +
  scale_color_manual(values = c("#44CC29",'#137300')) +
  theme(text = element_text(size = 24),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))

subplotAbis

ggsave(plot = subplotAbis,"/home/femeunier/Documents/projects/Hackaton/LidarED/Figures/Figure2aPFT.png",
       dpi = 300,width = 8,height = 5.5)

HeightA <- ggplot() +
  geom_point(data = df.residual.long %>% filter(type %in% c("residual0")),
             aes(x = hfit, y = residual, color = type)) +
  geom_point(data = df.residual.long %>% filter(!(type %in% c("residual_default","residual0"))),
             aes(x = hfit, y = residual, color = as.factor(PFT))) +
  geom_hline(yintercept = 0,linetype = 2, color = "black") +
  scale_color_manual(values = c("#44CC29",'#137300',"black")) +
  labs(x = "Fitted height (m)",y = "Residual (m)") +
  theme_bw() +
  theme(text = element_text(size = 24),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  guides(color = FALSE)

HeightB <- ggplot() +
  geom_density(data = df.residual.long %>% filter(type != "residual_default") %>%
                 mutate(PFT = case_when(type == "residual0" ~ 0,
                                        TRUE ~ PFT)),
             aes(x = residual, color = interaction(type,PFT)), fill = NA, alpha = 0.4, adjust = 1, outline.type = "upper",size = 1.5) +
  geom_vline(xintercept = 0,linetype = 2, color = "black") +
  scale_fill_manual(values = c('#44CC29',"darkgrey",'#137300')) +
  scale_color_manual(values = c('#44CC29',"darkgrey",'#137300')) +
  scale_x_continuous(labels = c("","","",""),
                     breaks = c(-10,-5,0,5)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "",y = "Density") +
  coord_flip() +
  theme_bw() +
  theme(text = element_text(size = 24),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  guides(color = FALSE)

plot_grid(HeightA,HeightB,nrow = 1,align = "hv",rel_widths = c(2,1))

ggsave(plot = last_plot(),"/home/femeunier/Documents/projects/Hackaton/LidarED/Figures/Figure3aPFT.png",
       dpi = 300,width = 10,height = 4.5)

# ggplot() +
#   geom_density_ridges(data = df.residual.long %>% filter(type != "residual_default") %>%
#                         mutate(PFT = case_when(type == "residual0" ~ 0,
#                                                TRUE ~ PFT)),
#                       aes(x = residual, y = interaction(type,PFT), fill = interaction(type,PFT)), color = NA, alpha = 0.4) +
#   geom_vline(xintercept = 0,linetype = 2, color = "black") +
#   # geom_hline(yintercept = 0,linetype = 2, color = "black") +
#   scale_fill_manual(values = c('#44CC29',"darkgrey",'#137300')) +
#   scale_color_manual(values = c('#44CC29',"darkgrey",'#137300')) +
#   # scale_y_continuous(expand = c(0,0)) +
#   # coord_flip() +
#   theme_bw() +
#   guides(color = FALSE)


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

df.residuals <- bind_rows(list(df.residuals,
                               data.frame(SSQ = c(SSF.m0,SSF.m1),
                                          allom = "Crown area",
                                          model = c("onePFT","PFTs"))))

coefs.CA0 <- coef(m0)
coefs.CA1 <- coef(m1)

df.allom.param <- bind_rows(list(df.allom.param,
                                 data.frame(param = c("b1Ca","b2Ca"),
                                            value = as.numeric(c(exp(coefs.CA0[1]),coefs.CA0[2])),
                                            model = "onePFT",
                                            allom = "CA",
                                            pft = 11),
                                 data.frame(param = rep(c("b1Ca","b2Ca"),2),
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

subplotBbis <- ggplot() +
  geom_point(data = data.wytham.filled,
             aes(x = dbh_tls,y = CA,color = as.factor(PFT)),alpha = 0.4) +
  geom_line(data = df_best_CA,
            aes(x = dbh, y = CAbest,color = as.factor(PFT)),linetype = 1,size = 1.5) +
  # geom_line(data = df_best_CA,
  #           aes(x = dbh, y = CAdefault,color = as.factor(PFT)),linetype = 2,size = 1.5) +
  geom_line(data = df_best_CA0,
            aes(x = dbh, y = CAbest),linetype = 1,size = 1.5, color = 'black') +
  scale_x_log10() +
  scale_y_log10() +
  xlab("DBH (cm)") +
  ylab("Crown area (m²)") +
  theme_bw() +
  guides(color = FALSE) +
  scale_color_manual(values = c("#44CC29",'#137300')) +
  theme(text = element_text(size = 24),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))

subplotBbis

ggsave(plot = subplotBbis,"/home/femeunier/Documents/projects/Hackaton/LidarED/Figures/Figure2bPFT.png",
       dpi = 300,width = 8,height = 5.5)

df.residual <- df %>% mutate(CAbest = exp(coefs.CA1[PFT + 1,1])*(dbh_tls^coefs.CA1[PFT + 1,2]),
                             CAdefault = b1Ca[PFT + 1]*(dbh_tls^b2Ca[PFT + 1]),
                             CA0 = exp(coefs.CA0[1])*(dbh_tls^coefs.CA0[2])) %>%
  mutate(residual_best = CA - CAbest,
         residual_default = CA - CAdefault,
         residual0 = CA - CA0) %>%
  dplyr::select(dbh_tls,PFT,CA,residual_best,residual_default,residual0)

df.residual %>% ungroup() %>%
  summarise(RMSE1 = sqrt(sum(residual_best**2)/nrow(df.residual))/mean(CA),
            RMSE0 = sqrt(sum(residual0**2)/nrow(df.residual))/mean(CA))

df.residual %>% group_by(PFT) %>%
  summarise(RMSE1 = sqrt(sum(residual_best**2)/length(residual_best))/mean(CA))


df.residual.long <- df.residual %>% pivot_longer(cols = c("residual_best","residual_default","residual0"),
                                                 names_to = "type",
                                                 values_to = "residual") %>%
  mutate(CAfit = CA - residual)

df.residual.long %>% mutate(PFT = case_when(type %in% c("residual_default","residual0") ~ 0,
                                             TRUE ~ PFT)) %>%
  group_by(type,PFT) %>% summarise(RMSE = sqrt(mean(residual**2)),
                                                  m = mean(CA),
                                                  RMSD = RMSE/m)

CAA <- ggplot() +
  geom_point(data = df.residual.long %>% filter(type %in% c("residual0")),
             aes(x = CAfit, y = residual, color = type)) +
  geom_point(data = df.residual.long %>% filter(!(type %in% c("residual_default","residual0"))),
             aes(x = CAfit, y = residual, color = as.factor(PFT))) +
  geom_hline(yintercept = 0,linetype = 2, color = "black") +
  scale_color_manual(values = c("#44CC29",'#137300',"black")) +
  labs(x = "Fitted crown area (m)",y = "Residual (m²)") +
  theme_bw() +
  theme(text = element_text(size = 24),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  guides(color = FALSE)

CAB <- ggplot() +
  geom_density(data = df.residual.long %>% filter(type != "residual_default") %>%
                 mutate(PFT = case_when(type == "residual0" ~ 0,
                                        TRUE ~ PFT)),
               aes(x = residual, color = interaction(type,PFT)), fill = NA, alpha = 0.4, adjust = 10, outline.type = "upper",size = 1.5) +
  geom_vline(xintercept = 0,linetype = 2, color = "black") +
  scale_fill_manual(values = c('#44CC29',"darkgrey",'#137300')) +
  scale_color_manual(values = c('#44CC29',"darkgrey",'#137300')) +
  scale_x_continuous(labels = c("","","",""),
                     breaks = c(0,100,200,300)) +
  scale_y_continuous(expand = c(0,0),breaks = c(0,0.015)) +
  labs(x = "",y = "Density") +
  coord_flip() +
  theme_bw() +
  theme(text = element_text(size = 24),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  guides(color = FALSE)

plot_grid(CAA,CAB,nrow = 1,align = "hv",rel_widths = c(2,1))

ggsave(plot = last_plot(),"/home/femeunier/Documents/projects/Hackaton/LidarED/Figures/Figure3bPFT.png",
       dpi = 300,width = 10,height = 4.5)


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

data.wytham.filled <- data.wytham.filled %>% mutate(AGB = AGV_m*wood.dens[PFT+1]*1000)

# plot(dbhs,b1Bs_small[1]*(dbhs)**b2Bs_small[1],log = "xy",type = "l")

df <- data.wytham.filled %>% filter(!is.na(dbh_tls) & !is.na(AGB))

m0 <- lm(data = data.wytham.filled,formula = log(AGB) ~ log(dbh_tls))
m1 <- lmList(log(AGB) ~ log(dbh_tls) | PFT, data=df)

SSF.m0 <- sum((summary(m0)[["residuals"]])**2)
SSF.m1 <- sum(unlist((summary(m1)[["residuals"]]))**2)

df.residuals <- bind_rows(list(df.residuals,
                               data.frame(SSQ = c(SSF.m0,SSF.m1),
                                          allom = "AGB",
                                          model = c("onePFT","PFTs"))))

coefs.AGB0 <- coef(m0)
coefs.AGB1 <- coef(m1)

df.allom.param <- bind_rows(list(df.allom.param,
                                 data.frame(param = c("b1Bs_small","b2Bs_small","b1Bs_large","b2Bs_large"),
                                            value = as.numeric(c(exp(coefs.AGB0[1])/0.7,coefs.AGB0[2]),
                                                               c(exp(coefs.AGB0[1])/0.7,coefs.AGB0[2])),
                                            model = "onePFT",
                                            allom = "Bs",
                                            pft = 11),
                                 data.frame(param = rep(c("b1Bs_small","b2Bs_small","b1Bs_large","b2Bs_large"),2),
                                            value = as.numeric(c(exp(coefs.AGB1[1,1])/0.7,coefs.AGB1[1,2],
                                                                 exp(coefs.AGB1[1,1])/0.7,coefs.AGB1[1,2],
                                                                 exp(coefs.AGB1[2,1])/0.7,coefs.AGB1[2,2],
                                                                 exp(coefs.AGB1[2,1])/0.7,coefs.AGB1[2,2])),
                                            model = "PFT",
                                            allom = "Bs",
                                            pft = sort(rep(c(10,11),4)))
                                 ))


df_best_AGB0 <- data.frame(dbh = c(dbhs)) %>%
  mutate(AGBbest = c(exp(coefs.AGB0[1])*(dbhs^coefs.AGB0[2])))

df_best_AGB <- data.frame(dbh = c(dbhs,dbhs),
                          PFT = c(rep(0,length(dbhs)),rep(1,length(dbhs)))) %>%
  mutate(
    AGBbest = c(exp(coefs.AGB1[1,1])*(dbhs^coefs.AGB1[1,2]),
                exp(coefs.AGB1[2,1])*(dbhs^coefs.AGB1[2,2])),
    AGBdefault = c(b1Bs_large[1]*(dbhs^b2Bs_large[1]),
                   b1Bs_large[2]*(dbhs^b2Bs_large[2])))

subplotCbis <- ggplot() +
  geom_point(data = data.wytham.filled,
             aes(x = dbh_tls,y = AGB,color = as.factor(PFT)),alpha = 0.4) +
  scale_x_log10() +
  scale_y_log10() +
  # geom_line(data = df_best_AGB,
  #           aes(x = dbh, y = AGBdefault,color = as.factor(PFT)),linetype = 2,size = 1.5) +
  geom_line(data = df_best_AGB,
            aes(x = dbh, y = AGBbest,color = as.factor(PFT)),linetype = 1,size = 1.5) +
  geom_line(data = df_best_AGB0,
            aes(x = dbh, y = AGBbest),linetype = 1,size = 1.5, color = "black") +
  xlab("DBH (cm)") +
  ylab("Aboveground Woody \n Biomass (kg)") +
  theme_bw() +
  guides(color = FALSE) +
  scale_color_manual(values = c("#44CC29",'#137300')) +
  theme(text = element_text(size = 24),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))

subplotCbis

ggsave(plot = subplotCbis,"/home/femeunier/Documents/projects/Hackaton/LidarED/Figures/Figure2CPFT.png",
       dpi = 300,width = 8,height = 5.5)

df.residual <- df %>% mutate(AGBbest = exp(coefs.AGB1[PFT + 1,1])*(dbh_tls^coefs.AGB1[PFT + 1,2]),
                             AGBdefault = b1Bs_large[PFT + 1]*(dbh_tls^b2Bs_large[PFT + 1]),
                             AGB0 = exp(coefs.AGB0[1])*(dbh_tls^coefs.AGB0[2])) %>%
  mutate(residual_best = AGB - AGBbest,
         residual_default = AGB - AGBdefault,
         residual0 = AGB - AGB0) %>%
  dplyr::select(dbh_tls,PFT,AGB,residual_best,residual_default,residual0)


df.residual %>% ungroup() %>%
  summarise(RMSE1 = sqrt(sum(residual_best**2)/nrow(df.residual))/mean(AGB),
            RMSE0 = sqrt(sum(residual0**2)/nrow(df.residual))/mean(AGB))

df.residual %>% group_by(PFT) %>%
  summarise(RMSE1 = sqrt(sum(residual_best**2)/length(residual_best))/mean(AGB))

df.residual.long <- df.residual %>% pivot_longer(cols = c("residual_best","residual_default","residual0"),
                                                 names_to = "type",
                                                 values_to = "residual") %>%
  mutate(AGBfit = AGB - residual)

df.residual.long %>% mutate(PFT = case_when(type %in% c("residual_default","residual0") ~ 0,
                                            TRUE ~ PFT)) %>%
  group_by(type,PFT) %>% summarise(RMSE = sqrt(mean(residual**2)),
                                                  m = mean(AGB),
                                                  RMSD = RMSE/m)


AGBA <- ggplot() +
  geom_point(data = df.residual.long %>% filter(type %in% c("residual0")),
             aes(x = AGBfit, y = residual, color = type)) +
  geom_point(data = df.residual.long %>% filter(!(type %in% c("residual_default","residual0"))),
             aes(x = AGBfit, y = residual, color = as.factor(PFT))) +
  geom_hline(yintercept = 0,linetype = 2, color = "black") +
  scale_color_manual(values = c("#44CC29",'#137300',"black")) +
  labs(x = "Fitted AGB (m)",y = "Residual (kg)") +
  theme_bw() +
  theme(text = element_text(size = 24),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  guides(color = FALSE)

AGBB <- ggplot() +
  geom_density(data = df.residual.long %>% filter(type != "residual_default") %>%
                 mutate(PFT = case_when(type == "residual0" ~ 0,
                                        TRUE ~ PFT)),
               aes(x = residual, color = interaction(type,PFT)), fill = NA, alpha = 0.4, adjust = 20, outline.type = "upper",size = 1.5) +
  geom_vline(xintercept = 0,linetype = 2, color = "black") +
  scale_fill_manual(values = c('#44CC29',"black",'#137300')) +
  scale_color_manual(values = c('#44CC29',"black",'#137300')) +
  scale_x_continuous(labels = c("","",""),
                     breaks = c(0,10000,20000)) +
  scale_y_continuous(expand = c(0,0),breaks = c(0,0.001)) +
  labs(x = "",y = "Density") +
  coord_flip() +
  theme_bw() +
  theme(text = element_text(size = 24),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  guides(color = FALSE)

plot_grid(AGBA,AGBB,nrow = 1,align = "hv",rel_widths = c(2,1))

ggsave(plot = last_plot(),"/home/femeunier/Documents/projects/Hackaton/LidarED/Figures/Figure3CPFT.png",
       dpi = 300,width = 10.5,height = 4.5)

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

m0 <- lm(data = data.wytham.filled,formula = log(LA) ~ log(dbh_tls))
m1 <- lmList(log(LA) ~ log(dbh_tls) | PFT, data=df)

SSF.m0 <- sum((summary(m0)[["residuals"]])**2)
SSF.m1 <- sum(unlist((summary(m1)[["residuals"]]))**2)

df.residuals <- bind_rows(list(df.residuals,
                               data.frame(SSQ = c(SSF.m0,SSF.m1),
                                          allom = "Leaf biomass",
                                          model = c("onePFT","PFTs"))))

coefs.LA0 <- coef(m0)
coefs.LA1 <- coef(m1)

df.allom.param <- bind_rows(list(df.allom.param,
                                 data.frame(param = c("b1Bl_small","b2Bl_small","b1Bl_large","b2Bl_large"),
                                            value = as.numeric(c(exp(coefs.LA0[1])*2,coefs.LA0[2],
                                                                 exp(coefs.LA0[1])*2,coefs.LA0[2])),
                                            model = "onePFT",
                                            allom = "Bl",
                                            pft = 11),
                                 data.frame(param = rep(c("b1Bl_small","b2Bl_small","b1Bl_large","b2Bl_large"),2),
                                            value = as.numeric(c(exp(coefs.LA1[1,1])*2,coefs.LA1[1,2],
                                                                 exp(coefs.LA1[1,1])*2,coefs.LA1[1,2],
                                                                 exp(coefs.LA1[2,1])*2,coefs.LA1[2,2],
                                                                 exp(coefs.LA1[2,1])*2,coefs.LA1[2,2])),
                                            model = "PFT",
                                            allom = "Bl",
                                            pft = sort(rep(c(10,11),4)))
                                 ))

df_best_LA0 <- data.frame(dbh = c(dbhs),
                         LAbest = c(exp(coefs.LA0[1])/SLA_single_pft*2*(dbhs^coefs.LA0[2])))

df_best_LA <- data.frame(dbh = c(dbhs,dbhs),
                         PFT = c(rep(0,length(dbhs)),rep(1,length(dbhs))),
                         LAbest = c(exp(coefs.LA1[1,1])/25.8*2*(dbhs^coefs.LA1[1,2]),
                                    exp(coefs.LA1[2,1])/31.36889*2*(dbhs^coefs.LA1[2,2])),
                         LAdefault = Bl)

subplotDbis <- ggplot() +
  geom_point(data = data.wytham.filled %>% mutate(Bl = case_when(PFT == 0 ~ LA/25.8*2,
                                                                 PFT == 1 ~ LA/31.36889*2)),
             aes(x = dbh_tls,y = Bl,color = as.factor(PFT)),alpha = 0.4) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("DBH (cm)") +
  ylab("Leaf biomass (kg)") +
  geom_line(data = df_best_LA,
            aes(x = dbh, y = LAbest,color = as.factor(PFT)),linetype = 1,size = 1.5) +
  geom_line(data = df_best_LA0,
            aes(x = dbh, y = LAbest),linetype = 1,size = 1.5, color = 'black') +
  # geom_line(data = df_best_LA,
  #           aes(x = dbh, y = LAdefault,color = as.factor(PFT)),linetype = 2,size = 1.5) +
  theme_bw() +
  guides(color = FALSE) +
  scale_color_manual(values = c("#44CC29",'#137300')) +
  theme(text = element_text(size = 24),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))

subplotDbis

# plot_grid(subplotC,subplotA,subplotB,subplotD,nrow = 2,align = "hv")
plot_grid(subplotCbis,subplotAbis,
          subplotBbis,subplotDbis,nrow = 2,align = "hv")

ggsave(plot = last_plot(),"/home/femeunier/Documents/projects/Hackaton/LidarED/Figures/Figure2PFT.png",
       dpi = 300,width = 12,height = 8)

df.residual <- df %>% mutate(LAbest = exp(coefs.LA1[PFT + 1,1])*(dbh_tls^coefs.LA1[PFT + 1,2]),
                             LAdefault = SLA_default[PFT + 1] *b1Bl_large[PFT + 1]*(dbh_tls^b2Bl_large[PFT + 1]),
                             LA0 = exp(coefs.LA0[1])*(dbh_tls^coefs.LA0[2])) %>%
  mutate(residual_best = LA - LAbest,
         residual_default = LA - LAdefault,
         residual0 = LA - LA0) %>%
  dplyr::select(dbh_tls,PFT,LA,residual_best,residual_default,residual0)

df.residual %>% ungroup() %>%
  summarise(RMSE1 = sqrt(sum(residual_best**2)/nrow(df.residual))/mean(LA),
            RMSE0 = sqrt(sum(residual0**2)/nrow(df.residual))/mean(LA))

df.residual %>% group_by(PFT) %>%
  summarise(RMSE1 = sqrt(sum(residual_best**2)/length(residual_best))/mean(LA))


df.residual.long <- df.residual %>% pivot_longer(cols = c("residual_best","residual_default","residual0"),
                                                 names_to = "type",
                                                 values_to = "residual") %>%
  mutate(LAfit = LA - residual)

df.residual.long %>% mutate(PFT = case_when(type %in% c("residual_default","residual0") ~ 0,
                                            TRUE ~ PFT)) %>%
  group_by(type,PFT) %>% summarise(RMSE = sqrt(mean(residual**2)),
                                                  m = mean(LA),
                                                  RMSD = RMSE/m)




LAA <- ggplot() +
  geom_point(data = df.residual.long %>% filter(type %in% c("residual0")),
             aes(x = LAfit, y = residual/29.30672, color = type)) +
  geom_point(data = df.residual.long %>% filter(!(type %in% c("residual_default","residual0"))),
             aes(x = LAfit, y = residual/30, color = as.factor(PFT))) +
  geom_hline(yintercept = 0,linetype = 2, color = "black") +
  scale_color_manual(values = c("#44CC29",'#137300',"black")) +
  labs(x = "Fitted leaf biomass (kg)",y = "Residual (m²)") +
  theme_bw() +
  theme(text = element_text(size = 24),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  guides(color = FALSE)

LAB <- ggplot() +
  geom_density(data = df.residual.long %>% filter(type != "residual_default") %>%
                 mutate(PFT = case_when(type == "residual0" ~ 0,
                                        TRUE ~ PFT)),
               aes(x = residual/30, color = interaction(type,PFT)), fill = NA, alpha = 0.4, adjust = 10, outline.type = "upper",size = 1.5) +
  geom_vline(xintercept = 0,linetype = 2, color = "black") +
  scale_fill_manual(values = c('#44CC29',"darkgrey",'#137300')) +
  scale_color_manual(values = c('#44CC29',"darkgrey",'#137300')) +
  scale_x_continuous(labels = c("","","","","",""),
                     breaks = 500*seq(-1,4)) +
  scale_y_continuous(breaks = c(0,0.004*25)) +
  labs(x = "",y = "Density") +
  coord_flip() +
  theme_bw() +
  theme(text = element_text(size = 24),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  guides(color = FALSE)

plot_grid(LAA,LAB,nrow = 1,align = "hv",rel_widths = c(2,1))

ggsave(plot = last_plot(),"/home/femeunier/Documents/projects/Hackaton/LidarED/Figures/Figure3DPFT.png",
       dpi = 300,width = 10,height = 4.5)

df.residuals

saveRDS(df.allom.param,"/home/femeunier/Documents/projects/Hackaton/LidarED/data/df.allom.param.RDS")
system2("scp",c("/home/femeunier/Documents/projects/Hackaton/LidarED/data/df.allom.param.RDS",
                "hpc:/scratch/gent/vo/000/gvo00074/hackaton/run_Wytham/ref/data/"))
