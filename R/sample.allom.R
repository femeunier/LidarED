sample.allom <- function(Npft = 1,
                         history.file = "/home/femeunier/Documents/projects/Hackaton/LidarED/data/Wytham_all.xml",
                         list.sample){

  pftnum <- c(9,10,11)
  Nsamples <- 1

  delta_pft = ifelse(Npft == 1,10,9)

  pft_name <- data.frame(name = c("temperate.North_Mid_Hardwood","temperate.Late_Hardwood"),
                         num = c(10,11))


  dbhs <- seq(1,130,length.out = 1000)
  alpha = 0.05
  ##########################################################################################

  for (ipft in seq(1,Npft)){

    pft.name <- as.character((pft_name %>% filter(num == (delta_pft + ipft)) %>% pull(name)))

    # Height

    h <- matrix(NA,nrow = length(pftnum),ncol = length(dbhs))
    href <- hmax <- b1Ht <- b2Ht <- c()

    for (ipft in seq(1,length(pftnum))){
      href[ipft] <- get_ED_default_pft(history.file,"hgt_ref",pftnum[ipft])
      b1Ht[ipft] <- get_ED_default_pft(history.file,"b1Ht",pftnum[ipft])
      b2Ht[ipft] <- get_ED_default_pft(history.file,"b2Ht",pftnum[ipft])

      h[ipft,] <- pmin(0.99*b1Ht[ipft]+href[ipft],
                       href[ipft] + b1Ht[ipft]*(1 -exp(dbhs*b2Ht[ipft])))
    }

    href.all <-  b1Ht.all <- b2Ht.all <- c()
    h.all <- matrix(NA,nrow = Nsamples,ncol = length(dbhs))

    for (isample in seq(1,Nsamples)){
      href.sample <- runif(n = 1,min = (1-alpha)*min(href), (1+alpha)*max(href))
      b1Ht.sample <- runif(n = 1,min = (1-alpha)*min(b1Ht), (1+alpha)*max(b1Ht))
      b2Ht.sample <- runif(n = 1,min = (1-alpha)*min(b2Ht), (1+alpha)*max(b2Ht))

      h.all[isample,] <- pmin(0.99*b1Ht.sample+href.sample,
                              href.sample + b1Ht.sample*(1 -exp(dbhs*b2Ht.sample)))

      list.sample[[pft.name]][["hgt_ref"]] <- href.sample
      list.sample[[pft.name]][["b1Ht"]] <- b1Ht.sample
      list.sample[[pft.name]][["b2Ht"]] <- b2Ht.sample
      list.sample[[pft.name]][["hgt_max"]] <- 0.99*b1Ht.sample+href.sample

    }



    ##########################################################################################
    # Crown area

    b1Ca <- b2Ca <- c()
    CA <- matrix(NA,nrow = length(pftnum),ncol = length(dbhs))

    for (ipft in seq(1,length(pftnum))){
      b1Ca[ipft] <- get_ED_default_pft(history.file,"b1Ca",pftnum[ipft])
      b2Ca[ipft] <- get_ED_default_pft(history.file,"b2Ca",pftnum[ipft])

      CA[ipft,] <- b1Ca[ipft]*(dbhs**b2Ca[ipft])
    }

    b1Ca.all <- b2Ca.all <- c()
    CA.all <- matrix(NA,nrow = Nsamples,ncol = length(dbhs))
    isample <- 1
    while (isample <= Nsamples){
      b1Ca.sample <- runif(n = 1,min = (1-alpha)*min(b1Ca), (1+alpha)*max(b1Ca))
      b2Ca.sample <- runif(n = 1,min = (1-alpha)*min(b2Ca), (1+alpha)*max(b2Ca))
      CA.sample <- b1Ca.sample*(dbhs**b2Ca.sample)

      pfts <- c()

      for (ipft in seq(1,length(pftnum))){
        if (any(CA.sample >= CA[ipft,]) & any(!(CA.sample >= CA[ipft,]))){
          pfts[ipft] <- TRUE
        } else{
          pfts[ipft] <- FALSE
        }
      }

      if (all(pfts)){
        b1Ca.all <- c(b1Ca.all,b1Ca.sample)
        b2Ca.all <- c(b2Ca.all,b2Ca.sample)
        CA.all[isample,] <- CA.sample
        isample <- isample + 1

        list.sample[[pft.name]][["b1Ca"]] <- b1Ca.all
        list.sample[[pft.name]][["b2Ca"]] <- b2Ca.all

      }
    }

    ##########################################################################################
    # Bs

    wood.dens <- b1Bs_small <- b2Bs_small <- dbh_crit <- c()
    Bs <- matrix(NA,nrow = length(pftnum),ncol = length(dbhs))

    for (ipft in seq(1,length(pftnum))){
      wood.dens[ipft]  <- get_ED_default_pft(history.file,"rho",pftnum[ipft])

      b1Bs_small[ipft]  <- get_ED_default_pft(history.file,"b1Bs_small",pftnum[ipft])
      b2Bs_small[ipft]  <- get_ED_default_pft(history.file,"b2Bs_small",pftnum[ipft])

      Bs[ipft,] <- b1Bs_small[ipft]*(dbhs**b2Bs_small[ipft])
    }


    b1Bs.all <- b2Bs.all <- c()
    Bs.all <- matrix(NA,nrow = Nsamples,ncol = length(dbhs))
    isample <- 1
    while (isample <= Nsamples){
      b1Bs.sample <- runif(n = 1,min = (1-alpha)*min(b1Bs_small), (1+alpha)*max(b1Bs_small))
      b2Bs.sample <- runif(n = 1,min = (1-alpha)*min(b2Bs_small), (1+alpha)*max(b2Bs_small))
      Bs.sample <- b1Bs.sample*(dbhs**b2Bs.sample)

      pfts <- c()

      for (ipft in seq(1,length(pftnum))){
        if (any(Bs.sample >= Bs[ipft,]) & any(!(Bs.sample >= Bs[ipft,]))){
          pfts[ipft] <- TRUE
        } else{
          pfts[ipft] <- FALSE
        }
      }

      if (all(pfts)){
        b1Bs.all <- c(b1Bs.all,b1Bs.sample)
        b2Bs.all <- c(b2Bs.all,b2Bs.sample)
        Bs.all[isample,] <- Bs.sample
        isample <- isample + 1

        list.sample[[pft.name]][["b1Bs_small"]] <- b1Bs.all
        list.sample[[pft.name]][["b2Bs_small"]] <- b2Bs.all
        list.sample[[pft.name]][["b1Bs_large"]] <- b1Bs.all
        list.sample[[pft.name]][["b2Bs_large"]] <- b2Bs.all

      }
    }

    ##########################################################################################
    # Bl

    b1Bl_small <- b2Bl_small <- SLA_default <- c()
    Bl <- LA <- matrix(NA,nrow = length(pftnum),ncol = length(dbhs))

    for (ipft in seq(1,length(pftnum))){

      b1Bl_small[ipft]  <- get_ED_default_pft(history.file,"b1Bl_small",pftnum[ipft])
      b2Bl_small[ipft]  <- get_ED_default_pft(history.file,"b2Bl_small",pftnum[ipft])
      Bl[ipft,] <- b1Bl_small[ipft]*(dbhs**b2Bl_small[ipft])

      SLA_default[ipft]   <- get_ED_default_pft(history.file,"SLA",pftnum[ipft])
      LA[ipft,] <- SLA_default[ipft]*Bl[ipft,]

    }

    b1Bl.all <- b2Bl.all <- c()
    Bl.all <- matrix(NA,nrow = Nsamples,ncol = length(dbhs))
    isample <- 1
    while (isample <= Nsamples){
      b1Bl.sample <- runif(n = 1,min = (1-alpha)*min(b1Bl_small), (1+alpha)*max(b1Bl_small))
      b2Bl.sample <- runif(n = 1,min = (1-alpha)*min(b2Bl_small), (1+alpha)*max(b2Bl_small))
      Bl.sample <- b1Bl.sample*(dbhs**b2Bl.sample)

      pfts <- c()

      for (ipft in seq(1,length(pftnum))){
        if (any(Bl.sample >= Bl[ipft,]) & any(!(Bl.sample >= Bl[ipft,]))){
          pfts[ipft] <- TRUE
        } else{
          pfts[ipft] <- FALSE
        }
      }

      if (all(pfts)){
        b1Bl.all <- c(b1Bl.all,b1Bl.sample)
        b2Bl.all <- c(b2Bl.all,b2Bl.sample)
        Bl.all[isample,] <- Bl.sample
        isample <- isample + 1

        list.sample[[pft.name]][["b1Bl_small"]] <- b1Bl.all
        list.sample[[pft.name]][["b2Bl_small"]] <- b2Bl.all
        list.sample[[pft.name]][["b1Bl_large"]] <- b1Bl.all
        list.sample[[pft.name]][["b2Bl_large"]] <- b2Bl.all

      }
    }
  }

  return(list.sample)

}

