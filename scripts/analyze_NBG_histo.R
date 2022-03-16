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

df_OP <- data.frame()

for (i in seq(1,N)){
  print(i/N)
  for (j in seq(1,length(pre))){
    dir.name <- paste0("NBG_",pre[j],"_",i)
    run.dir <- file.path(OP.dir,"run",dir.name)
    out.dir <- file.path(OP.dir,"out",dir.name)
    histo.dir <- file.path(out.dir,"histo")
    histo.file <- file.path(histo.dir,"history-S-2011-06-01-000000-g01.h5")

    if (file.exists(histo.file)){
      ED2INfile <- file.path(run.dir,"ED2IN")
      ed2in <- read_ed2in(ED2INfile)
      I_crown_mod <- ed2in$CROWN_MOD

      file <- histo.file
      mymont    = lapply(h5read_opt(file),FUN=aperm)
      names(mymont) <- gsub(x = names(mymont), pattern = "\\_", replacement = ".")

      df_OP <- bind_rows(list(df_OP,
                              data.frame(dbh.cat = seq(1,11),
                                         nplant = mymont$NPLANT.PY[1,,10]*100*100,
                                         run = i,
                                         crown_mod = I_crown_mod,
                                         isTRY = pre[j])))

    }
  }
}


saveRDS(df_OP,"df.OP.NBG.RDS")
# scp /home/femeunier/Documents/projects/Hackaton/LidarED/scripts/analyze_NBG_histo.R hpc:/data/gent/vo/000/gvo00074/felicien/R
