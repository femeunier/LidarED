rm(list = ls())

library(purrr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)

OP_dir <- "~/R/LidarED/runs/growth_storage_resp/out"

all_scenarios <- c("Hmax","Hmean","CA","AGB","Bl")

x2 <- combn(1:5,2)
x3 <- combn(1:5,3)
x4 <- combn(1:5,4)
x5 <- combn(1:5,5)

temp_combn <- c(as.list(combn(1:5,1)),
                lapply(seq_len(ncol(x2)), function(i) x2[,i]),
                lapply(seq_len(ncol(x3)), function(i) x3[,i]),
                lapply(seq_len(ncol(x4)), function(i) x4[,i]),
                lapply(seq_len(ncol(x5)), function(i) x5[,i]))
cmbs <-
  lapply(seq_len(length(temp_combn)),function(i){
    if (all(c(1,2) %in% temp_combn[[i]])){
      out <- NULL
    } else {
      out <-temp_combn[[i]]
    }
    return(out)
  })

cmbs[sapply(cmbs, is.null)] <- NULL

Nyears = 5

df_all <- data.frame()


names_scenar_all <- c()
for (i in c(seq(1,5),22,23)){ #length(cmbs)
  names_scenar <- paste(unlist(map(seq_len(length(cmbs[[i]])),function(j){all_scenarios[cmbs[[i]][j]]})),collapse = '_')
  names_scenar_all <- c(names_scenar_all,names_scenar)

  dir_output <- file.path(OP_dir,names_scenar)

  load(file = file.path(dir_output,"analysis.RData"))

  gpp <- datum$emean$gpp*1000/12/86400*1e6/365
  sm.stress <- datum$emean$sm.stress
  bseeds <- apply(datum$szpft$bseeds,1,sum)
  bsapwood <- apply(datum$szpft$bsapwood,1,sum)
  lai <- datum$emean$lai
  agb <- datum$emean$agb
  nep <- datum$emean$nep*1000/12/86400*1e6/365
  reco <- datum$emean$reco*1000/12/86400*1e6/365
  rauto <- datum$emean$plant.resp*1000/12/86400*1e6/365
  rhetero <- datum$emean$het.resp*1000/12/86400*1e6/365

  month <- datum$month
  year <- datum$year
  year_end <- year[length(year)]

  df_temp <- data.frame(scenar = names_scenar,
                        AGBt = grepl('AGB',names_scenar),
                        Blt = grepl('Bl',names_scenar),
                        CAt = grepl('CA',names_scenar),
                        Ht = ifelse(grepl('Hmax',names_scenar),"Hmax",ifelse(grepl('Hmean',names_scenar),"Hmean","regular")),
                        config = TRUE,
                        year = year,
                        month = month,
                        GPP = gpp,
                        LAI = lai,
                        AGB = agb,
                        Reco = reco,
                        Rauto = rauto,
                        bseeds = bseeds,
                        bsapwood = bsapwood,
                        Rhetero = rhetero,
                        NEP = nep,
                        sm.stress = sm.stress)

  df_all <- rbind(df_all,
                  df_temp)
}

###############################################################################
# Add references

# names_scenar <- c("reference","reference_noconfig","near_bare_ground","near_bare_ground_noconfig")
names_scenar <- c("reference","reference_noconfig")

for (iscenar in seq(1,length(names_scenar))){

  dir_output <- file.path(OP_dir,names_scenar[iscenar])

  if (grepl("near_bare_ground",names_scenar[iscenar])){
    names_scenar_all <- c(names_scenar_all,paste0(c(names_scenar[iscenar]),"_short"))
  }
  names_scenar_all <- c(names_scenar_all,c(names_scenar[iscenar]))
}

for (iscenar in seq(1,length(names_scenar))){

  dir_output <- file.path(OP_dir,names_scenar[iscenar])

  if (grepl("near_bare_ground",names_scenar[iscenar])){

    load(file = file.path(dir_output,"analysis_short.RData"))

    gpp <- datum$emean$gpp*1000/12/86400*1e6/365
    sm.stress <- datum$emean$sm.stress
    bseeds <- apply(datum$szpft$bseeds,1,sum)
    bsapwood <- apply(datum$szpft$bsapwood,1,sum)
    lai <- datum$emean$lai
    agb <- datum$emean$agb
    nep <- datum$emean$nep*1000/12/86400*1e6/365
    reco <- datum$emean$reco*1000/12/86400*1e6/365
    rauto <- datum$emean$plant.resp*1000/12/86400*1e6/365
    rhetero <- datum$emean$het.resp*1000/12/86400*1e6/365

    month <- datum$month
    year <- datum$year

    df_temp <- data.frame(scenar = paste0(names_scenar[iscenar],"_short"),
                          year = year,
                          month = month,
                          AGBt = grepl('AGB',names_scenar[iscenar]),
                          Blt = grepl('Bl',names_scenar[iscenar]),
                          CAt = grepl('CA',names_scenar[iscenar]),
                          Ht = ifelse(grepl('Hmax',names_scenar[iscenar]),"Hmax",ifelse(grepl('Hmean',names_scenar[iscenar]),"Hmean","regular")),
                          config = !grepl('noconfig',names_scenar[iscenar]),
                          GPP = gpp,
                          NEP = nep,
                          LAI = lai,
                          Reco = reco,
                          Rauto = rauto,
                          bseeds = bseeds,
                          bsapwood = bsapwood,
                          Rhetero = rhetero,
                          AGB = agb,
                          sm.stress = sm.stress)

    df_all <- rbind(df_all,
                    df_temp)

    load(file = file.path(dir_output,"analysis_long.RData"))

  } else {
    load(file = file.path(dir_output,"analysis.RData"))
  }

  gpp <- datum$emean$gpp*1000/12/86400*1e6/365
  sm.stress <- datum$emean$sm.stress
  bseeds <- apply(datum$szpft$bseeds,1,sum)
  bsapwood <- apply(datum$szpft$bsapwood,1,sum)
  lai <- datum$emean$lai
  agb <- datum$emean$agb
  nep <- datum$emean$nep*1000/12/86400*1e6/365
  reco <- datum$emean$reco*1000/12/86400*1e6/365
  rauto <- datum$emean$plant.resp*1000/12/86400*1e6/365
  rhetero <- datum$emean$het.resp*1000/12/86400*1e6/365

  month <- datum$month
  year <- datum$year

  df_temp <- data.frame(scenar = names_scenar[iscenar],
                        year = year,
                        month = month,
                        AGBt = grepl('AGB',names_scenar[iscenar]),
                        Blt = grepl('Bl',names_scenar[iscenar]),
                        CAt = grepl('CA',names_scenar[iscenar]),
                        Ht = ifelse(grepl('Hmax',names_scenar[iscenar]),"Hmax",ifelse(grepl('Hmean',names_scenar[iscenar]),"Hmean","regular")),
                        config = !grepl('noconfig',names_scenar[iscenar]),
                        GPP = gpp,
                        NEP = nep,
                        LAI = lai,
                        Reco = reco,
                        Rauto = rauto,
                        bseeds = bseeds,
                        bsapwood = bsapwood,
                        Rhetero = rhetero,
                        AGB = agb,
                        sm.stress = sm.stress)

  df_all <- rbind(df_all,
                  df_temp)
}

###############################################################################

df_formatted <- df_all %>% mutate(time = as.Date(paste(as.character(year),sprintf(fmt = "%02d",month),sprintf(fmt = "%02d",1),sep = '/'),format = "%Y/%m/%d")) %>%
  group_by(scenar) %>% mutate(time_elapsed = as.numeric(time - time[1])) %>%  ungroup()

df_formatted_last <-
  df_formatted %>% group_by(scenar) %>% mutate(time_elapsed2 = time_elapsed - (time_elapsed[row_number()==n()]) + Nyears *365) %>%
  filter(time_elapsed2 >= 0) %>% mutate(time_yr = time_elapsed2/365)

ggplot(data = df_formatted_last,aes(x = time_yr,
                               y = NEP,color = scenar)) +
  geom_line() +
  theme_bw()

ggplot(data = df_formatted_last %>% filter(grepl("near_bare_ground|Hmean_CA_AGB_Bl",scenar)),aes(x = time_yr,
                                    y = NEP,color = scenar)) +
  geom_line() +
  scale_color_brewer(palette = "Spectral") +
  theme_bw()

###############################################################################

yr_fl <-
  df_formatted_last %>% ungroup() %>% group_by(scenar, month) %>% summarise(
    GPP_m = mean(GPP),
    bseeds = mean(bseeds),
    bsapwood = mean(bsapwood),
    GPP_sd = sd(GPP),
    sm.stress = mean(sm.stress),
    NEP_m = mean(NEP),
    Reco_m = mean(Reco),
    LAI_m = mean(LAI),
    AGB_m = mean(AGB),
    AGBt = AGBt[1],
    Blt = Blt[1],
    CAt = CAt[1],
    Ht = (Ht[1]),
    test = case_when(
      ifelse(Ht[1] == "regular", TRUE, FALSE) & CAt[1] ~ "Hregular",
      ifelse(Ht[1] == "regular", TRUE, FALSE) &
        !CAt[1] ~ "default",
      ifelse(Ht[1] == "Hmean", TRUE, FALSE) &
        !CAt[1] ~ "Hmean",
      ifelse(Ht[1] == "Hmax", TRUE, FALSE) &
        !CAt[1] ~ "Hmax",
      ifelse(Ht[1] == "Hmax", TRUE, FALSE) &
        CAt[1] ~ "Hmax_CA",
      ifelse(Ht[1] == "Hmean", TRUE, FALSE) &
        CAt[1] ~ "Hmean_CA"
    ),

    config = config[1]
  ) %>% ungroup()

yr_diff <- yr_fl %>%
  mutate(
    GPP_diff = GPP_m - GPP_m[scenar == "reference"],
    sm_diff = sm.stress - sm.stress[scenar == "reference"],
    NEP_diff = NEP_m - NEP_m[scenar == "reference"],
    AGB_diff = AGB_m - AGB_m[scenar == "reference"],
    LAI_diff = LAI_m - LAI_m[scenar == "reference"],
    Reco_diff = Reco_m - Reco_m[scenar == "reference"]
  )


A <- ggplot(data = yr_diff %>% filter(! (grepl("near_bare_ground",scenar) | grepl("reference",scenar))),
       aes(x = month,
           y = GPP_diff,group = scenar,color = scenar)) +
  geom_line() +
  scale_color_brewer(palette = "Spectral") +
  theme_bw() + theme(legend.position = "none")

B <- ggplot(data = yr_diff %>% filter(! (grepl("near_bare_ground",scenar) | grepl("reference",scenar))),
            aes(x = month,
                y = Reco_diff,group = scenar,color = scenar)) +
  geom_line() +
  scale_color_brewer(palette = "Spectral") +
  theme_bw() + theme(legend.position = "none")


C <- ggplot(data = yr_diff %>% filter(! (grepl("near_bare_ground",scenar) | grepl("reference",scenar))),
            aes(x = month,
                y = NEP_diff,group = scenar,color = scenar)) +
  geom_line() +
  scale_color_brewer(palette = "Spectral") +
  theme_bw()

grid.arrange(A, B, C, nrow = 1)
