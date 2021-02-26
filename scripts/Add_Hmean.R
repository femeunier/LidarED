rm(list = ls())

library(ggplot2)
library(dplyr)
library(LidarED)
library(purrr)

# Load data
data.file <- "./data/Wytham_trees_summary_ED2_noDead.csv"
data.file_added <- "./data/Wytham_trees_summary_ED2_nodead_add.csv"

data.wytham <- read.csv(data.file,header = TRUE)

dir_profiles <- "/home/carya/data/Wytham"

H_mean <- map_dbl(1:nrow(data.wytham),function(i){
  name <- as.character(data.wytham$TLS_ID[i])
  file = file.path(dir_profiles,paste0("lm_",name,".profile"))

  if (file.exists(file)){
    data_temp <- read.csv(file,header=TRUE)
    H_mean <- weighted.mean(data_temp[,1],data_temp[,2])
    # H_max <- max(data_temp[which(data_temp[,2]>0),1])
  } else {
    H_mean = NA
  }
  return(H_mean)
})

data.wytham$hmean <- H_mean

df <- data.wytham %>% filter(!is.na(DBH_TLS_.m._x))

history.file <- "./data/Wytham.xml"
pftnum = 10

href  <- get_ED_default_pft(history.file,"hgt_ref",pftnum)
hmax  <- get_ED_default_pft(history.file,"hgt_max",pftnum)
isTropi  <- get_ED_default_pft(history.file,"is_tropical",pftnum)
b1Ht  <- get_ED_default_pft(history.file,"b1Ht",pftnum)
b2Ht  <- get_ED_default_pft(history.file,"b2Ht",pftnum)

m0 <- nlsLM(data = df,
            hmean ~ href + b1Ht*(1 -exp(100*DBH_TLS_.m._x*b2Ht)),
            start=list(href=href, b1Ht=b1Ht, b2Ht = b2Ht), control = nls.control(maxiter = 500, tol = 1e-05, minFactor = 1/1024/10,
                                                                                 printEval = TRUE, warnOnly = TRUE))

dbh_extr <- extremum(data.wytham$DBH_census_.m.)*100
dbh <- seq(dbh_extr[1],dbh_extr[2],length.out = 1000)

data.wytham$hmean[is.na(data.wytham$hmean)] <- pmin(data.wytham$Hgt_pts_.m._x[is.na(data.wytham$hmean)],
                                                    predict(m0,data.frame(DBH_TLS_.m._x=data.wytham$DBH_TLS_.m._x[is.na(data.wytham$hmean)])))

par(mfrow=c(1,1))
plotFit(m0,interval="prediction",pch=1,col.pred=adjustcolor("blue", 0.5),shade=TRUE,ylim=c(0,35),col.fit = "blue",
        xlab = "DBH TLS (cm)", ylab = 'h (m)')
lines(dbh/100,pmin(hmax,dbh2h(href,b1Ht,b2Ht,dbh,isTropi)),col="red",lty=1,type='l')


write.csv(x = data.wytham,data.file_added)
