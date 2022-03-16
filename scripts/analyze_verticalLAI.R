rm(list = ls())

rm(list = ls())

library(rhdf5)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(PEcAn.ED2)

source("/data/gent/vo/000/gvo00074/felicien/R/h5read_opt.r")

OP.dir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/Wytham/ensembles"
N = 250
pre <- c("TRY","noTRY")
configs <- c("NBG","Census","TLS")

df_OP <- data.frame()

for (iconfig in seq(1,length(configs))){
  print(paste0("- ",iconfig))
  for (i in seq(1,N)){
    print(i/N)
    for (j in seq(1,length(pre))){
      dir.name <- paste0(configs[iconfig],"_",pre[j],"_",i)
      run.dir <- file.path(OP.dir,"run",dir.name)
      out.dir <- file.path(OP.dir,"out",dir.name)
      histo.dir <- file.path(out.dir,"histo")
      histo.file <- file.path(histo.dir,"history-S-2006-06-01-000000-g01.h5")

      if (file.exists(histo.file)){
        ED2INfile <- file.path(run.dir,"ED2IN")
        ed2in <- read_ed2in(ED2INfile)
        I_crown_mod <- ed2in$CROWN_MOD

        file <- histo.file
        mymont    = lapply(h5read_opt(file),FUN=aperm)
        names(mymont) <- gsub(x = names(mymont), pattern = "\\_", replacement = ".")

        if (length(mymont$HITE)>0){

          df_OP <- bind_rows(list(df_OP,
                                  data.frame(pft = mymont$PFT,
                                             Hite = mymont$HITE,
                                             dbh = mymont$DBH,
                                             patch = rep(1:length(mymont$PACO.N),mymont$PACO.N),
                                             nplant = mymont$NPLANT,
                                             LAI = mymont$LAI.CO,
                                             conf = configs[iconfig],
                                             run = i,
                                             crown_mod = I_crown_mod,
                                             isTRY = pre[j]) %>% group_by(patch) %>% mutate(cumLAI = cumsum(LAI))))
        }
      }
    }
  }
}


saveRDS(df_OP,"df.OP.verticalLAI.RDS")
# scp /home/femeunier/Documents/projects/Hackaton/LidarED/scripts/analyze_verticalLAI.R hpc:/data/gent/vo/000/gvo00074/felicien/R
