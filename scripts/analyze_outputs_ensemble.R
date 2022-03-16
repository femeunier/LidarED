rm(list = ls())

library(purrr)
library(ED2scenarios)
library(LidarED)
library(PEcAn.ED2)
library(stringr)
library(dplyr)

rundir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/Wytham/ensembles/run/"
outdir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/Wytham/ensembles/out/"

simus <- list.dirs(rundir)
simus <- simus[2:length(simus)]
Nsimus <- length(simus)

df.simu <- data.frame()

for (i in seq(1,Nsimus)){

  ED2INfile <- file.path(rundir,basename(simus[i]),"ED2IN")
  config.file <- file.path(rundir,basename(simus[i]),"config.xml")
  datum.file <- file.path(outdir,basename(simus[i]),"analy","analysis.RData")

  ed2in <- read_ed2in(ED2INfile)

  is_NBG <- (ed2in$IED_INIT_MODE == 0)
  is_census <- grepl("Census", ed2in$SFILIN, fixed = TRUE)
  is_TLS <- grepl("TLS", ed2in$SFILIN, fixed = TRUE)
  crownmod <- ed2in$CROWN_MOD

  pfts <- ed2in$INCLUDE_THESE_PFT
  Npft <- length(pfts)

  is_TLSallom <- length(get_ED_default_pft(config.file,"b1Ht",11))>0
  is_TRYparam <- length(get_ED_default_pft(config.file,"SLA",11))>0
  if(file.exists(datum.file)){
    load(datum.file)
    years <- datum$year
    months <- datum$month
    gpp <- datum$emean$gpp
    lai <- datum$emean$lai
    agb <- datum$emean$agb

  } else{
    years <- months <- gpp <- lai <- agb <- NA
  }

  df.simu <- bind_rows(list(df.simu,
                            data.frame(IC = case_when(is_NBG ~ "NBG",
                                                      is_census ~ "Census",
                                                      is_TLS ~ "TLS",
                                                      TRUE ~ "Unknown"),
                                       crownmod = crownmod,
                                       is_TRYparam = is_TRYparam,
                                       is_TLSallom = is_TLSallom,
                                       simu.num = i,
                                       Npft = Npft,
                                       year = years,
                                       month = months,
                                       GPP = gpp,
                                       LAI = lai,
                                       AGB = agb)))
}

saveRDS(df.simu,"df.Wytham.ensemble.RDS")

# scp /home/femeunier/Documents/projects/Hackaton/LidarED/scripts/analyze_outputs_ensemble.R hpc:/data/gent/vo/000/gvo00074/felicien/R
