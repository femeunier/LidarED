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
Nensemble <- 250

df.simu <- data.frame()

for (iensemble in seq(1,length(ensembles))){
  print(iensemble)
  for (i in seq(1,Nensemble)){
    print(paste("-",i/Nensemble))

    for (isTRY in seq(0,1)){

      run.name <- paste0(ensembles[iensemble],"_",ifelse(isTRY == 1,"TRY","noTRY"),"_",i)

      ED2INfile <- file.path(rundir,run.name,"ED2IN")
      config.file <- file.path(rundir,run.name,"config.xml")
      datum.file <- file.path(outdir,run.name,"analy","analysis.RData")

      if(file.exists(datum.file) & (file.info(datum.file)$ctime > "2022-01-20 00:00:00 CEST")){
        load(datum.file)
        years <- datum$year
        months <- datum$month
        gpp <- datum$emean$gpp
        lai <- datum$emean$lai
        agb <- datum$emean$agb
        par.gnd <- datum$emean$par.gnd
        reco <- datum$emean$reco
        nep <- datum$emean$nep
        nplant <- apply(datum$szpft$nplant[,12,],1,sum)
        ba <- datum$szpft$ba[,12,18]

      } else{
        years <- months <- gpp <- lai <- agb <- par.gnd <- reco <- nep <- nplant <-  ba <- NA
      }

      df.simu <- bind_rows(list(df.simu,
                                data.frame(name = run.name,
                                           type = ensembles[iensemble],
                                           num = i,
                                           year = years,
                                           month = months,
                                           GPP = gpp,
                                           LAI = lai,
                                           AGB = agb,
                                           PAR = par.gnd,
                                           Reco = reco,
                                           N = nplant,
                                           BA = ba,
                                           NEP = nep)))
    }
  }
}



saveRDS(df.simu,"df.Wytham.ensembles.RDS")

# scp /home/femeunier/Documents/projects/Hackaton/LidarED/scripts/analyze_outputs_ensembles.R hpc:/data/gent/vo/000/gvo00074/felicien/R
