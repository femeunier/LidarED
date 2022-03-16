rm(list = ls())

library(purrr)
library(ED2scenarios)
library(LidarED)
library(PEcAn.ED2)

ref_dir <- "/scratch/gent/vo/000/gvo00074/hackaton/run_Wytham/ref"
ed2in <- read_ed2in(file.path(ref_dir,"ED2IN"))
ed2in$ITOUTPUT <- 0
ed2in$IMETAVG <- 3

rundir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/Wytham/ensembles/run"
outdir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/Wytham/ensembles/out"

if(!dir.exists(rundir)) dir.create(rundir)
if(!dir.exists(outdir)) dir.create(outdir)

SLA_default <- c(24.2,60.0)

add2config_min <- list(growth_resp_factor = 0.45,
                       storage_turnover_rate = 0.166)

add2config <- list(onePFT = readRDS("/scratch/gent/vo/000/gvo00074/hackaton/run_Wytham/ref/data/TRY_CWM.RDS"),
                   PFT = readRDS("/scratch/gent/vo/000/gvo00074/hackaton/run_Wytham/ref/data/TRY_CWM_PFT.RDS"))

# add2config_TLS <- readRDS("/home/femeunier/Documents/projects/Hackaton/LidarED/data/df.allom.param.RDS")
add2config_TLS <- readRDS("/scratch/gent/vo/000/gvo00074/hackaton/run_Wytham/ref/data/df.allom.param.RDS")

defaults <- list()
settings <- list(model = list(revision = "git",
                              config.header = NULL))
PREFIX_XML <- "<?xml version=\"1.0\"?>\n<!DOCTYPE config SYSTEM \"ed.dtd\">\n"
pft_name <- c("temperate.North_Mid_Hardwood","temperate.Late_Hardwood")
pft_nums <- c('10','11')


list_dir <- list()

#############################################################################################

ICscenario <- c("Census","TLS","NBG")
PFTscenario <- c("onePFT","PFT")
delta_PFT <- c(1,0)

param.TRY <- c(FALSE,TRUE)   # TRY parameters
param.allom <- c(FALSE,TRUE) # TLS allometries

for (iIC in seq(1,length(ICscenario))){
  for (iPFTscenario in seq(1,length(PFTscenario))){
    for (iparam.TRY in seq(1,length(param.TRY))){
      for (iparam.allom in seq(1,length(param.allom))){

        run.name <- paste0(ICscenario[iIC],"_",PFTscenario[iPFTscenario])

        if (param.TRY[iparam.TRY]){
          run.name <- paste0(run.name,"_paramTRY")
        }

        if (param.allom[iparam.allom]){
          run.name <- paste0(run.name,"_paramAllom")
        }

        run_ref <- file.path(rundir,run.name)
        out_ref <- file.path(outdir,run.name)

        if(!dir.exists(run_ref)) dir.create(run_ref)
        if(!dir.exists(out_ref)) dir.create(out_ref)
        if(!dir.exists(file.path(out_ref,"analy"))) dir.create(file.path(out_ref,"analy"))
        if(!dir.exists(file.path(out_ref,"histo"))) dir.create(file.path(out_ref,"histo"))

        Npft <- ifelse(PFTscenario[iPFTscenario] == "onePFT",1,2)
        isNBG <- (ICscenario[iIC] == "NBG")

        # ED2IN
        ed2in_scenar <- ed2in
        ed2in_scenar$IEDCNFGF <- file.path(run_ref,"config.xml")
        ed2in_scenar$FFILOUT <- file.path(out_ref,"analy","analysis")
        ed2in_scenar$SFILOUT <- file.path(out_ref,"histo","history")

        #IC
        if (isNBG){
          ed2in_scenar$IYEARA <- 1912
          ed2in_scenar$IED_INIT_MODE <- 0
        } else {
          ed2in_scenar$SFILIN <- file.path("/data/gent/vo/000/gvo00074/felicien/R/LidarED/data/Wytham",paste0("Wytham_",PFTscenario[iPFTscenario],"_",ICscenario[iIC]))
        }

        # Process

        if (param.allom[iparam.allom]){
          ed2in_scenar$CROWN_MOD <- 1
        } else {
          ed2in_scenar$CROWN_MOD <- 0
        }

        ed2in_scenar$INCLUDE_THESE_PFT <- sort(11 - ((1:Npft)-1))

        write_ed2in(ed2in_scenar,filename = file.path(run_ref,"ED2IN"))

        # Config multiple PFTs
        config_TRY <- list()

        if (param.allom[iparam.allom]){
          if (param.TRY[iparam.TRY]){
            for (ipft in seq(1,Npft)){
              if (PFTscenario[iPFTscenario] == "onePFT"){

                df.select <- add2config_TLS %>% dplyr::filter(model == "onePFT") %>% dplyr::select(param,value)
                temp.param <- as.numeric(df.select$value)
                names(temp.param) <- df.select$param
                temp.param.list <- as.list(temp.param)

                config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]] <- unlist(c(add2config_min,
                                                                                   add2config[[PFTscenario[iPFTscenario]]],
                                                                                   temp.param.list))

                config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]][c("b1Bl_small","b1Bl_large")] <-
                  config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]][c("b1Bl_small","b1Bl_large")]/ (config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]][c("SLA")]*2)

              } else{

                df.select <- add2config_TLS %>% dplyr::filter(model == "PFT",pft == as.numeric(pft_nums[ipft])) %>% dplyr::select(param,value)
                temp.param <- as.numeric(df.select$value)
                names(temp.param) <- df.select$param
                temp.param.list <- as.list(temp.param)

                config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]] <- unlist(c(add2config_min,
                                                                                   add2config[[PFTscenario[iPFTscenario]]][[pft_nums[ipft]]],
                                                                                   temp.param.list))

                config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]][c("b1Bl_small","b1Bl_large")] <-
                  config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]][c("b1Bl_small","b1Bl_large")]/ (config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]][c("SLA")]*2)

              }
            }
          } else {
            for (ipft in seq(1,Npft)){

              if (PFTscenario[iPFTscenario] == "onePFT"){

                df.select <- add2config_TLS %>% dplyr::filter(model == "onePFT") %>% dplyr::select(param,value)
                temp.param <- as.numeric(df.select$value)
                names(temp.param) <- df.select$param
                temp.param.list <- as.list(temp.param)

                config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]] <- unlist(c(add2config_min,
                                                                                  temp.param.list))

                config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]][c("b1Bl_small","b1Bl_large")] <-
                  config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]][c("b1Bl_small","b1Bl_large")]/ 60


              } else{

                df.select <- add2config_TLS %>% dplyr::filter(model == "PFT",pft == as.numeric(pft_nums[ipft])) %>% dplyr::select(param,value)
                temp.param <- as.numeric(df.select$value)
                names(temp.param) <- df.select$param
                temp.param.list <- as.list(temp.param)

                config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]] <- unlist(c(add2config_min,
                                                                                   temp.param.list))

                config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]][c("b1Bl_small","b1Bl_large")] <-
                  config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]][c("b1Bl_small","b1Bl_large")]/ SLA_default[ipft]
              }
            }
          }
        } else {
          if (param.TRY[iparam.TRY]){
            for (ipft in seq(1,Npft)){

              if (PFTscenario[iPFTscenario] == "onePFT"){
                config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]] <- unlist(c(add2config_min,
                                                                                   add2config[[PFTscenario[iPFTscenario]]]))
              } else{
                config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]] <- unlist(c(add2config_min,
                                                                                   add2config[[PFTscenario[iPFTscenario]]][[pft_nums[ipft]]]))
              }
            }
          } else {
            for (ipft in seq(1,Npft)){
              config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]] <- unlist(add2config_min)
            }
          }
        }


        xml <- write.config.xml.ED2(defaults = defaults,
                                    settings = settings,
                                    trait.values = config_TRY)

        XML::saveXML(xml, file = file.path(run_ref,"config.xml"), indent = TRUE,
                     prefix = PREFIX_XML)

        # job.sh
        write_job(file =  file.path(run_ref,"job.sh"),
                  nodes = 1,ppn = 18,mem = 16,walltime = 24,
                  prerun = "ml UDUNITS/2.2.26-intel-2018a R/3.4.4-intel-2018a-X11-20180131 HDF5/1.10.1-intel-2018a; ulimit -s unlimited",
                  CD = run_ref,
                  ed_exec = "/user/scratchkyukon/gent/gvo000/gvo00074/felicien/ED2/ED/build/ed_2.1-opt",
                  ED2IN = "ED2IN")

        list_dir[[run.name]] = run_ref
      }
    }
  }
}

dumb <- write_bash_submission(file = file.path(rundir,"all_jobs.sh"),
                              list_files = list_dir,
                              job_name = "job.sh")

# scp /home/femeunier/Documents/projects/Hackaton/LidarED/scripts/generate_scenario_IC_params.R hpc:/data/gent/vo/000/gvo00074/felicien/R

