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
  guides(size = FALSE,color = FALSE,fill = FALSE)

ggsave(plot = subplotB,"./Figures/Figure1b.png",
       dpi = 300,width = 4.5,height = 4.5)


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
            start=list(href=href, b1Ht=b1Ht, b2Ht = b2Ht), control = nls.control(maxiter = 500, tol = 1e-05, minFactor = 1/1024/10,
                                                                                 printEval = TRUE, warnOnly = TRUE))
RMSE(m0)
plot(predict(m0),df$h)
abline(a = 0,b = 1,col = "red")
LM <- lm(data = data.frame(x = predict(m0),y = df$h),y ~ x)
summary(LM)$r.squared
sqrt(mean(LM$residuals^2))

dbh_extr <- extremum(data.wytham$dbh_census)
dbhs <- seq(dbh_extr[1],dbh_extr[2],length.out = 1000)

coefs.m0 <- coef(m0)
df_best <- data.frame(dbh = dbhs,
                      hbest = pmin(0.99*coef(m0)[2] + coef(m0)[1],coefs.m0[1] + coefs.m0[2]*(1 -exp(dbhs*coefs.m0[3]))),
                      hdefault = pmin(hmax,href + b1Ht*(1 -exp(dbhs*b2Ht))))

ggplot(data = data.wytham.filled) +
  geom_point(aes(x = dbh_tls,y = h,color = as.factor(species)),size = 1) +
  geom_line(data = df_best,
            aes(x = dbh, y = hdefault),color = "darkgrey",linetype = 1,size = 2) +
  geom_line(data = df_best,
            aes(x = dbh, y = hbest),color = "black",linetype = 1,size = 2) +
  labs(x = "",y = "",color = "Species") +
  scale_x_continuous(breaks = c(),labels = c("","","")) +
  scale_y_continuous(breaks = c(),labels = c("","","")) +
  theme_bw() +
  guides(color = FALSE) +
  scale_color_manual(values = c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02')) +
  theme(text = element_text(size = 40),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))

ggsave(plot = last_plot(),"./Figures/Scheme.png",
       dpi = 300,width = 4.5,height = 3)
