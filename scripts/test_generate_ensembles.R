rm(list = ls())

library(purrr)
library(ED2scenarios)
library(LidarED)
library(dplyr)
library(stringr)

SLA_default <- c(24.2,60.0)

add2config_min <- list(growth_resp_factor = 0.45,
                       storage_turnover_rate = 0.166)


add2config <- list(onePFT = readRDS("/home/femeunier/Documents/projects/Hackaton/LidarED/data/TRY_CWM.RDS"),
                   PFT = readRDS("/home/femeunier/Documents/projects/Hackaton/LidarED/data/TRY_CWM_PFT.RDS"))

add2config_TLS <- readRDS("/home/femeunier/Documents/projects/Hackaton/LidarED/data/df.allom.param.all.RDS")
Nsample <- max(add2config_TLS$num)


pft_name <- c("temperate.North_Mid_Hardwood","temperate.Late_Hardwood")
pft_name_short <- c("MH","LH")
pft_nums <- c('10','11')


#############################################################################################
ensembles <- c("Census","TLS")
Nensemble <- 10

PFTscenario <- c("onePFT","PFT")
delta_PFT <- c(1,0)

param.TRY <- c(FALSE,TRUE)   # TRY parameters

# Process
typeRTM <- c(1) # c(1,2)
plasticity <- c(0) # c(0,2)

df.inputs <- data.frame()

for (i in seq(1,Nensemble)){
  print(paste0("-",i/Nensemble))

  isTRY <- sample(param.TRY,1)

  test.pft <- sample(PFTscenario,1)
  iPFTscenario <- which(PFTscenario == test.pft)
  Npft <- ifelse(test.pft == "onePFT",1,2)

  canrad <- sample(typeRTM,1)
  plasticity.type <- sample(plasticity,1)

  # Parameter
  sample <- sample.params(Npft = Npft,
                          prior.file = "/home/femeunier/Documents/projects/Hackaton/LidarED/data/pft-priors_mod.csv",
                          c("water_conductance","growth_resp_factor","mort2","Vm0","Rd0","q",
                            "clumping_factor","quantum_efficiency","leaf_reflect_nir","stomatal_slope","leaf_reflect_vis","repro_min_h"))

  # sample.allom <- sample
  sample.allom <- sample.allom(Npft = Npft,
                               history.file = "/home/femeunier/Documents/projects/Hackaton/LidarED/data/Wytham_all.xml",
                               list.sample = sample,
                               list.allom = c("h","CA","Bs","Bl"))


  for (iensemble in seq(1,length(ensembles))){

    isNBG <- (ensembles[iensemble] == "NBG")
    isTLS <- (ensembles[iensemble] ==  "TLS")

    run.name <- paste0(ensembles[iensemble],"_",i)

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

            config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]] <- unlist(c(sample[[pft_name[delta_PFT[iPFTscenario] + ipft]]],
                                                                               sample.try(add2config[[PFTscenario[iPFTscenario]]]),
                                                                               temp.param.list,
                                                                               add2config_min))

            config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]][c("b1Bl_small","b1Bl_large")] <-
              config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]][c("b1Bl_small","b1Bl_large")]/ (config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]][c("SLA")]*2)

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

            config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]] <- unlist(c(sample[[pft_name[delta_PFT[iPFTscenario] + ipft]]],
                                                                               sample.try(add2config[[PFTscenario[iPFTscenario]]][[pft_nums[ipft]]]),
                                                                               temp.param.list,
                                                                               add2config_min))

            config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]][c("b1Bl_small","b1Bl_large")] <-
              config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]][c("b1Bl_small","b1Bl_large")]/ (config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]][c("SLA")]*2)

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

            config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]] <- unlist(c(sample[[pft_name[delta_PFT[iPFTscenario] + ipft]]],
                                                                               temp.param.list,
                                                                               add2config_min))

            config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]][c("b1Bl_small","b1Bl_large")] <-
              config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]][c("b1Bl_small","b1Bl_large")]/ SLA_default[ipft+1]

            df.param <- data.frame(pft = pft_name_short[delta_PFT[iPFTscenario] + ipft],
                                   trait = names(config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]]),
                                   value = as.numeric(config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]]))


          } else{

            df.select <- add2config_TLS %>% dplyr::filter(model == "PFT",pft == as.numeric(pft_nums[ipft]),
                                                          num == sample(Nsample,1)) %>% dplyr::select(param,value)
            temp.param <- as.numeric(df.select$value)
            names(temp.param) <- df.select$param
            temp.param.list <- as.list(temp.param)

            config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]] <- unlist(c(sample[[pft_name[delta_PFT[iPFTscenario] + ipft]]],
                                                                               temp.param.list,
                                                                               add2config_min))

            config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]][c("b1Bl_small","b1Bl_large")] <-
              config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]][c("b1Bl_small","b1Bl_large")]/ SLA_default[ipft]

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
            config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]] <- unlist(c(sample.allom[[pft_name[delta_PFT[iPFTscenario] + ipft]]],
                                                                               sample.try(add2config[[PFTscenario[iPFTscenario]]]),
                                                                               add2config_min))

            df.param <- data.frame(pft = pft_name_short[delta_PFT[iPFTscenario] + ipft],
                                   trait = names(config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]]),
                                   value = as.numeric(config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]]))
          } else{
            config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]] <- unlist(c(sample.allom[[pft_name[delta_PFT[iPFTscenario] + ipft]]],
                                                                               sample.try(add2config[[PFTscenario[iPFTscenario]]][[pft_nums[ipft]]]),
                                                                               add2config_min))

            df.param <- bind_rows(list(df.param,
                                       data.frame(pft = pft_name_short[delta_PFT[iPFTscenario] + ipft],
                                                  trait = names(config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]]),
                                                  value = as.numeric(config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]]))))
          }
        }
      } else {
        for (ipft in seq(1,Npft)){
          config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]] <- unlist(sample.allom[[pft_name[delta_PFT[iPFTscenario] + ipft]]],
                                                                           add2config_min)

          df.param <- bind_rows(list(df.param,
                                     data.frame(pft = pft_name_short[delta_PFT[iPFTscenario] + ipft],
                                                trait = names(config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]]),
                                                value = as.numeric(config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]]))))
        }
      }
    }


    df.inputs <- bind_rows(list(df.inputs,
                                bind_cols(data.frame(type = ensembles[iensemble],
                                                     num = i,
                                                     isTRY = isTRY,
                                                     Npft = Npft,
                                                     isNBG = isNBG,
                                                     isTLS = isTLS,
                                          df.param))))


  }
}

saveRDS(df.inputs,"/home/femeunier/Documents/projects/Hackaton/LidarED/data/inputs.ensembles_test.RDS")


