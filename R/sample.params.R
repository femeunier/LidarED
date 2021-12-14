sample.params <- function(Npft = 1,
                          prior.file = "/home/femeunier/Downloads/pft-priors.csv",
                          params.select){

  priors <- read.csv(prior.file) %>%
    filter(pft != "umbs.northern_pine") %>%
    mutate(pft.num = case_when(pft == "umbs.early_hardwood" ~ 9,
                               pft == "umbs.mid_hardwood" ~ 10,
                               pft == "umbs.late_hardwood" ~ 11))

  priors <- priors %>%
    filter(trait %in% params.select) %>%
    mutate(trait = as.character(trait)) %>%
    mutate(trait = case_when(trait == "Vcmax" ~ "Vm0",
                             trait == "leaf_respiration_rate_m2" ~ "Rd0",
                             trait == "fineroot2leaf" ~ "q",
                             TRUE ~ trait))

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

      list.sample[[pft.name]][[param.name]] <- param.sample

    }
  }

  return(list.sample)

}
