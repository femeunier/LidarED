sample.params <- function(Npft = 1,
                          prior.file = "/data/gent/vo/000/gvo00074/pecan/output/other_runs/Wytham/data/pft-priors.csv",
                          params.select = c("water_conductance","growth_resp_factor","mort2","Vm0","Rd0","q",
                                            "clumping_factor","quantum_efficiency","leaf_reflect_nir","stomatal_slope","leaf_reflect_vis","repro_min_h")
                          ){

  priors <- read.csv(prior.file) %>%
    filter(pft != "umbs.northern_pine") %>%
    mutate(pft.num = case_when(pft == "umbs.early_hardwood" ~ 9,
                               pft == "umbs.mid_hardwood" ~ 10,
                               pft == "umbs.late_hardwood" ~ 11))

  priors <- priors %>%
    mutate(trait = as.character(trait)) %>%
    mutate(trait = case_when(trait == "Vcmax" ~ "Vm0",
                             trait == "leaf_respiration_rate_m2" ~ "Rd0",
                             trait == "fineroot2leaf" ~ "q",
                             TRUE ~ trait)) %>%
    filter(trait %in% params.select)

  list.sample <- list()
  delta_pft = ifelse(Npft == 1,10,9)

  pft_name <- data.frame(name = c("temperate.North_Mid_Hardwood","temperate.Late_Hardwood"),
                         num = c(10,11))

  for (ipft in seq(1,Npft)){

    priors.select <- priors %>% filter(pft.num == (delta_pft + ipft))
    pft.name <- as.character((pft_name %>% filter(num == (delta_pft + ipft)) %>% pull(name)))
    list.sample[[pft.name]] <- list()

    for (iparam in seq(1,length(params.select))){

      param.name <- params.select[iparam]
      priors.select.param <- priors.select %>% filter(trait == param.name)

      distn <- as.character(priors.select.param$distn)
      parama <- priors.select.param$parama
      paramb <- priors.select.param$paramb
      param.sample <- do.call(paste0("r", distn), list(1, parama, paramb))

      list.sample[[pft.name]][[param.name]] <- ifelse(param.name == "growth_resp_factor",min(0.6,param.sample),param.sample)

    }
  }

  return(list.sample)

}
