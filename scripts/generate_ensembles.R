rm(list = ls())

library(purrr)
library(ED2scenarios)
library(LidarED)
library(PEcAn.ED2)
library(dplyr)
library(stringr)
library(ggplot2)

ref_dir <- "/scratch/gent/vo/000/gvo00074/hackaton/run_Wytham/ref"
ed2in <- read_ed2in(file.path(ref_dir,"ED2IN"))

ed2in$ITOUTPUT <- 0
ed2in$IMETAVG <- 3

ed2in$IMONTHA <- 06
ed2in$IYEARA <- 2006

ed2in$IMONTHZ <- 06
ed2in$IYEARZ <- 2011

rundir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/Wytham/ensembles/run"
outdir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/Wytham/ensembles/out"

if(!dir.exists(rundir)) dir.create(rundir)
if(!dir.exists(outdir)) dir.create(outdir)

SLA_default <- c(24.2,60.0)
SLA_best = c(25.8,31.36889) # before 29.6
SLA_best_single_pft <- 29.30672

add2config_min <- list(growth_resp_factor = 0.45,
                       storage_turnover_rate = 0.166)

# add2config <- list(onePFT = readRDS("/home/femeunier/Documents/projects/Hackaton/LidarED/data/TRY_CWM.RDS"),PFT = readRDS("/home/femeunier/Documents/projects/Hackaton/LidarED/data/TRY_CWM_PFT.RDS"))

add2config <- list(onePFT = readRDS("/scratch/gent/vo/000/gvo00074/hackaton/run_Wytham/ref/data/TRY_CWM.RDS"),
                   PFT = readRDS("/scratch/gent/vo/000/gvo00074/hackaton/run_Wytham/ref/data/TRY_CWM_PFT.RDS"))

# add2config_TLS <- readRDS("/home/femeunier/Documents/projects/Hackaton/LidarED/data/df.allom.param.all.RDS")
add2config_TLS <- readRDS("/scratch/gent/vo/000/gvo00074/hackaton/run_Wytham/ref/data/df.allom.param.all.RDS")

ggplot(data = add2config_TLS %>% filter(allom == "Bl",
                                        param == "b1Bl_small")) +
  geom_boxplot(aes(x = interaction(as.factor(pft),param),y = value)) +
  facet_wrap(~ model) +
  theme_bw()

Nsample <- max(add2config_TLS$num)

defaults <- list()
settings <- list(model = list(revision = "git",
                              config.header = NULL))
PREFIX_XML <- "<?xml version=\"1.0\"?>\n<!DOCTYPE config SYSTEM \"ed.dtd\">\n"
pft_name <- c("temperate.North_Mid_Hardwood","temperate.Late_Hardwood")
pft_name_short <- c("MH","LH")
pft_nums <- c('10','11')


list_dir <- list()

#############################################################################################
ensembles <- c("Census","NBG","TLS")
Nensemble <- 250
delta_N <- 0

PFTscenario <- c("onePFT","PFT")
delta_PFT <- c(1,0)

param.TRY <- c(FALSE,TRUE)   # TRY parameters

# Process
typeRTM <- c(0,1) # c(1,2)
plasticity <- c(0,1) # c(0,2)

df.inputs <- data.frame()

for (i in (delta_N+seq(1,Nensemble))){
  print(paste("-",(i-delta_N)/(Nensemble)))

  test.pft <- sample(PFTscenario,1)
  iPFTscenario <- which(PFTscenario == test.pft)
  Npft <- ifelse(test.pft == "onePFT",1,2)

  # Parameter

  # sample <- sample.params(Npft = Npft,
  #                         prior.file = "/data/gent/vo/000/gvo00074/pecan/output/other_runs/Wytham/data/pft-priors_mod.csv",
  #                         c("f_labile"))

  sample <- sample.params(Npft = Npft,
                          prior.file = "/data/gent/vo/000/gvo00074/pecan/output/other_runs/Wytham/data/pft-priors_mod.csv",
                          c("water_conductance","growth_resp_factor","mort2","Vm0","Rd0","q","SLA",
                            "clumping_factor","quantum_efficiency","leaf_reflect_nir","stomatal_slope","leaf_reflect_vis","repro_min_h"))

  sample.allom <- sample.allom(Npft = Npft,
                               history.file = "/data/gent/vo/000/gvo00074/pecan/output/other_runs/Wytham/data/Wytham_all.xml",
                               list.sample = sample,
                               list.allom = c("h","CA","Bs","Bl"),
                               alpha = 0.1)


  for (isTRY in seq(0,1)){

    if (isTRY){
      if (test.pft == "onePFT"){
        add2config.sample.onePFT <- sample.try(add2config[[PFTscenario[iPFTscenario]]])
      } else {
        add2config.sample.PFT <- list()
        for (ipft in seq(1,Npft)){
          add2config.sample.PFT[[ipft]] <- sample.try(add2config[[PFTscenario[iPFTscenario]]][[pft_nums[ipft]]])
        }
      }
    }

    for (iensemble in seq(1,length(ensembles))){

      isNBG <- (ensembles[iensemble] == "NBG")
      isTLS <- (ensembles[iensemble] ==  "TLS")

      run.name <- paste0(ensembles[iensemble],"_",ifelse(isTRY == 1,"TRY","noTRY"),"_",i)

      run_ref <- file.path(rundir,run.name)
      out_ref <- file.path(outdir,run.name)

      if(!dir.exists(run_ref)) dir.create(run_ref)
      if(!dir.exists(out_ref)) dir.create(out_ref)
      if(!dir.exists(file.path(out_ref,"analy"))) dir.create(file.path(out_ref,"analy"))
      if(!dir.exists(file.path(out_ref,"histo"))) dir.create(file.path(out_ref,"histo"))

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
        ed2in_scenar$SFILIN <- file.path("/data/gent/vo/000/gvo00074/felicien/R/LidarED/data/Wytham",paste0("Wytham_",test.pft,"_",ensembles[iensemble]))
      }

      ed2in_scenar$INCLUDE_THESE_PFT <- sort(11 - ((1:Npft)-1))

      # Process
      # if (isTLS){
      #   sample.crownmod <- 1
      # } else if (iensemble == 1) {
      #   sample.crownmod <- sample(c(0,1),1)
      # }

      sample.crownmod <- sample(c(0,1),1)

      canrad <- sample(typeRTM,1)
      plasticity.type <- sample(plasticity,1)

      ed2in_scenar$CROWN_MOD <- sample.crownmod
      ed2in_scenar$ICANRAD <- canrad
      ed2in_scenar$TRAIT_PLASTICITY_SCHEME <- plasticity.type

      write_ed2in(ed2in_scenar,filename = file.path(run_ref,"ED2IN"))

      # Config multiple PFTs
      config_TRY <- list()

      df.param <- data.frame()

      if (isTRY){
        for (ipft in seq(1,Npft)){
          sample.allom[[ipft]][["Vm0"]] <- NULL
          sample[[ipft]][["Vm0"]] <- NULL
        }
      }


      if (isTLS){
        if (isTRY){
          for (ipft in seq(1,Npft)){
            if (test.pft == "onePFT"){

              df.select <- add2config_TLS %>% dplyr::filter(model == "onePFT",
                                                            num == sample(Nsample,1)) %>% dplyr::select(param,value)
              temp.param <- as.numeric(df.select$value)
              names(temp.param) <- df.select$param
              temp.param.list <- as.list(temp.param)

              all.params <- unlist(c(add2config.sample.onePFT,
                                     sample[[pft_name[delta_PFT[iPFTscenario] + ipft]]],
                                     temp.param.list,
                                     add2config_min))

              config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]] <- all.params[!duplicated(names(all.params))]

              config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]][c("b1Bl_small","b1Bl_large")] <-
                config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]][c("b1Bl_small","b1Bl_large")]/ (config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]][c("SLA")]*2)/1.55

              df.param <- data.frame(pft = pft_name_short[delta_PFT[iPFTscenario] + ipft],
                                     trait = names(config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]]),
                                     value = as.numeric(config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]]))


            } else{

              df.select <- add2config_TLS %>% dplyr::filter(model == "PFT",pft == as.numeric(pft_nums[ipft]),
                                                            num == sample(Nsample,1)) %>% dplyr::select(param,value)
              temp.param <- as.numeric(df.select$value)
              names(temp.param) <- df.select$param
              temp.param.list <- as.list(temp.param)

              all.params <- unlist(c(add2config.sample.PFT[[ipft]],
                                     sample[[pft_name[delta_PFT[iPFTscenario] + ipft]]],
                                     temp.param.list,
                                     add2config_min))

              config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]] <- all.params[!duplicated(names(all.params))]

              config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]][c("b1Bl_small","b1Bl_large")] <-
                config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]][c("b1Bl_small","b1Bl_large")]/ (config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]][c("SLA")]*2)/1.55

              df.param <- bind_rows(list(df.param,
                                         data.frame(pft = pft_name_short[delta_PFT[iPFTscenario] + ipft],
                                                    trait = names(config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]]),
                                                    value = as.numeric(config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]]))))


            }
          }
        } else {
          for (ipft in seq(1,Npft)){

            if (test.pft == "onePFT"){

              df.select <- add2config_TLS %>% dplyr::filter(model == "onePFT",
                                                            num == sample(Nsample,1)) %>% dplyr::select(param,value)
              temp.param <- as.numeric(df.select$value)
              names(temp.param) <- df.select$param
              temp.param.list <- as.list(temp.param)

              all.params <- unlist(c(sample[[pft_name[delta_PFT[iPFTscenario] + ipft]]],
                                     temp.param.list,
                                     add2config_min))

              config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]] <- all.params[!duplicated(names(all.params))]

              config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]][c("b1Bl_small","b1Bl_large")] <-
                config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]][c("b1Bl_small","b1Bl_large")]/ (config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]][c("SLA")]*2)/1.55

              df.param <- data.frame(pft = pft_name_short[delta_PFT[iPFTscenario] + ipft],
                                     trait = names(config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]]),
                                     value = as.numeric(config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]]))


            } else{

              df.select <- add2config_TLS %>% dplyr::filter(model == "PFT",
                                                            pft == as.numeric(pft_nums[ipft]),
                                                            num == sample(Nsample,1)) %>% dplyr::select(param,value)
              temp.param <- as.numeric(df.select$value)
              names(temp.param) <- df.select$param
              temp.param.list <- as.list(temp.param)

              all.params <- unlist(c(sample[[pft_name[delta_PFT[iPFTscenario] + ipft]]],
                                     temp.param.list,
                                     add2config_min))

              config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]] <- all.params[!duplicated(names(all.params))]

              config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]][c("b1Bl_small","b1Bl_large")] <-
                config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]][c("b1Bl_small","b1Bl_large")]/ (config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]][c("SLA")]*2)/1.55

              df.param <- bind_rows(list(df.param,
                                         data.frame(pft = pft_name_short[delta_PFT[iPFTscenario] + ipft],
                                                    trait = names(config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]]),
                                                    value = as.numeric(config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]]))))

            }
          }
        }
      } else {
        if (isTRY){
          for (ipft in seq(1,Npft)){

            if (test.pft == "onePFT"){


              all.params <- unlist(c(add2config.sample.onePFT,
                                     sample.allom[[pft_name[delta_PFT[iPFTscenario] + ipft]]],
                                     add2config_min))

              config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]] <- all.params[!duplicated(names(all.params))]

              df.param <- data.frame(pft = pft_name_short[delta_PFT[iPFTscenario] + ipft],
                                     trait = names(config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]]),
                                     value = as.numeric(config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]]))



            } else{

              all.params <- unlist(c(add2config.sample.PFT[[ipft]],
                                     sample.allom[[pft_name[delta_PFT[iPFTscenario] + ipft]]],
                                     add2config_min))

              config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]] <- all.params[!duplicated(names(all.params))]


              df.param <- bind_rows(list(df.param,
                                         data.frame(pft = pft_name_short[delta_PFT[iPFTscenario] + ipft],
                                                    trait = names(config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]]),
                                                    value = as.numeric(config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]]))))
            }
          }
        } else {
          for (ipft in seq(1,Npft)){

            all.params <- unlist(c(sample.allom[[pft_name[delta_PFT[iPFTscenario] + ipft]]],
                                   add2config_min))

            config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]] <- all.params[!duplicated(names(all.params))]

            df.param <- bind_rows(list(df.param,
                                       data.frame(pft = pft_name_short[delta_PFT[iPFTscenario] + ipft],
                                                  trait = names(config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]]),
                                                  value = as.numeric(config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]]))))
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
                nodes = 1,ppn = 18,mem = 16,walltime = ifelse(isNBG,24,2),
                prerun = "ml UDUNITS/2.2.26-intel-2018a R/3.4.4-intel-2018a-X11-20180131 HDF5/1.10.1-intel-2018a; ulimit -s unlimited",
                CD = run_ref,
                ed_exec = "/user/scratchkyukon/gent/gvo000/gvo00074/felicien/ED2/ED/build/ed_2.1-opt-master-2bb6872",
                ED2IN = "ED2IN",
                clean = TRUE)

      list_dir[[run.name]] = run_ref

      df.inputs <- bind_rows(list(df.inputs,
                                  bind_cols(data.frame(name = basename(run_ref),
                                                       type = ensembles[iensemble],
                                                       num = i,
                                                       isTRY = isTRY,
                                                       Npft = Npft,
                                                       isNBG = isNBG,
                                                       isTLS = isTLS,
                                                       crownmod = ed2in_scenar$CROWN_MOD,
                                                       canrad = ed2in_scenar$ICANRAD,
                                                       plasticity = ed2in_scenar$TRAIT_PLASTICITY_SCHEME),
                                            df.param)))


    }
  }
}

saveRDS(df.inputs,"inputs.ensembles.RDS")
saveRDS(df.inputs,file.path(file.path(rundir,paste0(ensembles[1],"_","noTRY_",delta_N+1),"inputs.ensembles.RDS")))

dumb <- write_bash_submission(file = file.path(rundir,"all_jobs.sh"),
                              list_files = list_dir,
                              job_name = "job.sh")

# scp /home/femeunier/Documents/projects/Hackaton/LidarED/scripts/generate_ensembles.R hpc:/data/gent/vo/000/gvo00074/felicien/R

