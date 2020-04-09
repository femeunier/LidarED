rm(list = ls())

library(purrr)
library(dplyr)
library(ggplot2)
require(Hmisc)

OP_dir <- "~/R/LidarED/runs/growth_storage_resp/"

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
for (i in c(1:5,22,23)){ #length(cmbs)
  names_scenar <- paste(unlist(map(seq_len(length(cmbs[[i]])),function(j){all_scenarios[cmbs[[i]][j]]})),collapse = '_')
  names_scenar_all <- c(names_scenar_all,names_scenar)

  dir_output <- file.path(OP_dir,names_scenar)

  if (file.exists(file.path(dir_output,"analysis.RData"))){
    load(file = file.path(dir_output,"analysis.RData"))

    month <- datum$month
    year <- datum$year
    year_end <- year[length(year)]

    Days_month <- monthDays(as.Date(paste0(year,'/',month,'/01')))
    Days_year <- yearDays(as.Date(paste0(year,'/',month,'/01')))

    gpp <- datum$emean$gpp*1000/Days_year*Days_month
    nep <- datum$emean$nep*1000/Days_year*Days_month
    reco <- datum$emean$reco*1000/Days_year*Days_month

    rauto <- datum$emean$plant.resp*1000/Days_year*Days_month
    rhetero <- datum$emean$het.resp*1000/Days_year*Days_month

    df_temp <- data.frame(scenar = names_scenar,
                          AGBt = grepl('AGB',names_scenar),
                          Blt = grepl('Bl',names_scenar),
                          CAt = grepl('CA',names_scenar),
                          Ht = ifelse(grepl('Hmax',names_scenar),"Hmax",ifelse(grepl('Hmean',names_scenar),"Hmean","regular")),
                          config = TRUE,
                          year = year,
                          month = month,
                          GPP = gpp,
                          Reco = reco,
                          Rauto = rauto,
                          NEP = nep)

    df_all <- rbind(df_all,
                    df_temp)
  }
}

###############################################################################
# Add references

names_scenar <- c("reference","reference_config","near_bare_ground_config")

for (iscenar in seq(1,length(names_scenar))){

  dir_output <- file.path(OP_dir,names_scenar[iscenar])

  names_scenar_all <- c(names_scenar_all,c(names_scenar[iscenar]))
}

for (iscenar in seq(1,length(names_scenar))){

  dir_output <- file.path(OP_dir,names_scenar[iscenar])

  load(file = file.path(dir_output,"analysis.RData"))

  month <- datum$month
  year <- datum$year
  year_end <- year[length(year)]

  Days_month <- monthDays(as.Date(paste0(year,'/',month,'/01')))
  Days_year <- yearDays(as.Date(paste0(year,'/',month,'/01')))

  gpp <- datum$emean$gpp*1000/Days_year*Days_month
  nep <- datum$emean$nep*1000/Days_year*Days_month
  reco <- datum$emean$reco*1000/Days_year*Days_month

  rauto <- datum$emean$plant.resp*1000/Days_year*Days_month
  rhetero <- datum$emean$het.resp*1000/Days_year*Days_month

  df_temp <- data.frame(scenar = names_scenar[iscenar],
                        AGBt = grepl('AGB',names_scenar[iscenar]),
                        Blt = grepl('Bl',names_scenar[iscenar]),
                        CAt = grepl('CA',names_scenar[iscenar]),
                        Ht = ifelse(grepl('Hmax',names_scenar[iscenar]),"Hmax",ifelse(grepl('Hmean',names_scenar[iscenar]),"Hmean","regular")),
                        config = TRUE,
                        year = year,
                        month = month,
                        GPP = gpp,
                        Reco = reco,
                        Rauto = rauto,
                        NEP = nep)

  df_all <- rbind(df_all,
                  df_temp)
}

###############################################################################

data.month <- readRDS(file = file.path(getwd(),"data","Fluxtower_monthly.RDS")) %>% mutate(date = as.Date(paste0(year,'/',month,'/01')))
df_all_filter <- df_all %>% mutate(date = as.Date(paste0(year,'/',month,'/01'))) %>% filter(date >= min(data.month$date),
                                                                                            date <= max(data.month$date))


data.plot <- rbind(df_all_filter %>% filter(scenar %in% c("reference_config","Hmax_CA_AGB_Bl")) %>% select(month,NEP,GPP,Reco,year,scenar),
                   as.data.frame(data.month %>% mutate(scenar = "data") %>% select(year,month,GPP,Reco,NEP,scenar)))
C <- ggplot(data = data.plot,
            aes(x=month, y=NEP, fill=as.factor(year))) +
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_brewer(palette = "YlOrBr") +
  facet_grid(~scenar) +
  theme_bw()

A <- ggplot(data = data.plot,
       aes(x=month, y=GPP, fill=as.factor(year))) +
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_brewer(palette = "Greens") +
  facet_grid(~scenar) +
  theme_bw()


B <- ggplot(data = data.plot,
       aes(x=month, y=Reco, fill=as.factor(year))) +
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_brewer(palette = "Reds") +
  facet_grid(~scenar) +
  theme_bw()

grid.arrange(A, B, C, nrow = 3)

ggsave(plot = grid.arrange(A, B, C, nrow = 3),filename = "./Figures/monthlyFluxes.png",dpi=300,height = 15,width=15)

