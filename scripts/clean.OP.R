rm(list = ls())

library(purrr)
library(ED2scenarios)
library(LidarED)
library(PEcAn.ED2)
library(stringr)
library(dplyr)

rundir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/Wytham/ensembles/run/"
outdir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/Wytham/ensembles/out/"

ensembles <- c("Census","NBG","TLS")
Nensemble <- 50

df.simu <- data.frame()

write_joblauncher

for (iensemble in seq(1,length(ensembles))){
  print(iensemble)
  for (i in seq(1,Nensemble)){

    run.name <- paste0(ensembles[iensemble],"_",i)

    run_ref <- file.path(rundir,run.name)
    out_ref <- file.path(outdir,run.name)

    ED2IN <- file.path(run_ref,"ED2IN")
    datum.file <- file.path(out_ref,"analy","analysis.RData")

    if(file.exists(datum.file)){
      ed2in <- read_ed2in(ED2IN)
      OPfiles <- ed2in$FFILOUT
      CMD <- paste0("$(find ",dirname(OPfiles)," -name '*' ! -name '",paste0(basename(OPfiles),".RData"),"')")

      system2("rm",CMD)
    }
  }
}

# scp /home/femeunier/Documents/projects/Hackaton/LidarED/scripts/clean.OP.R hpc:/data/gent/vo/000/gvo00074/felicien/R

