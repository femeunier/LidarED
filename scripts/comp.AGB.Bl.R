rm(list = ls())

library(purrr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

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

names_scenar <- c("reference_config","reference")
names_scenar_all <- c(names_scenar)

for (i in seq(1,length(cmbs))){
  names_scenar <- paste(unlist(map(seq_len(length(cmbs[[i]])),function(j){all_scenarios[cmbs[[i]][j]]})),collapse = '_')
  names_scenar_all <- c(names_scenar_all,names_scenar)
}

delta_AGB <- LAI_max <- LAI_max2 <- LAI_peak <- AGB_m <- LAI_m <- c()

for (iscenar in seq(1,length(names_scenar_all))){
  dir_output <- file.path(OP_dir,names_scenar_all[iscenar])

  load(file = file.path(dir_output,"analysis.RData"))

  time <- names(unlist(datum$patch$lai))
  area <- unlist(datum$patch$area)
  LAI <- unlist(datum$patch$lai)
  maxLAIpatch <- unlist(lapply(datum$patch$lai,max))
  pos2 <- which(grepl(paste(c("m07","m08"),collapse='|'),names(maxLAIpatch)))
  LAI_max2[iscenar] <- mean(maxLAIpatch[pos2])

  pos <- which(grepl(paste(c("m07","m08"),collapse='|'),time))

  LAI_m[iscenar] <- mean(datum$emean$lai)
  LAI_max[iscenar] <- max(LAI)
  LAI_peak[iscenar] <- weighted.mean(LAI[pos],area[pos])
  AGB_m[iscenar] <- mean(datum$emean$agb[60])
  delta_AGB[iscenar] <- (datum$emean$agb[37] - datum$emean$agb[13])*10/2 # per year
}

names(LAI_max2) <- names(AGB_m) <- names(LAI_peak) <- names(LAI_max) <- names(LAI_peak) <- names(delta_AGB) <- names_scenar_all

df <- data.frame(scenario = names_scenar_all,AGB_m = AGB_m,LAI_peak = LAI_peak,LAI_m = LAI_m) %>% mutate(
  Blt = case_when(
    grepl("Bl", scenario) ~ "Bl",
    scenario == "reference_config" ~ "reference_config",
    scenario == "reference" ~ "reference",
    TRUE ~ "Other"
  ),
  AGBt = case_when(
    grepl("AGB", scenario) ~ "AGB",
    scenario == "reference_config" ~ "reference_config",
    scenario == "reference" ~ "reference",
    TRUE ~ "Other"
  ))


ggplot(data = df %>% group_by(Blt) %>% summarise(LAI_m = mean(LAI_m)),
       aes(x = Blt,y = LAI_m,fill = Blt,color = Blt)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Greens") +
  scale_color_brewer(palette = "Greens") +
  theme_bw() + theme(legend.position = "none") + labs(x="")

ggsave(plot = last_plot(),filename = "./Figures/LAI.png",dpi=300,height = 10,width=10)

ggplot(data = df %>% group_by(AGBt) %>% summarise(AGB_m = mean(AGB_m)),
       aes(x = AGBt,y = AGB_m,fill = AGBt,color = AGBt)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "YlOrBr") +
  scale_color_brewer(palette = "YlOrBr") +
  theme_bw() +  theme(legend.position = "none") + labs(x="")

ggsave(plot = last_plot(),filename = "./Figures/AGB.png",dpi=300,height = 10,width=10)

