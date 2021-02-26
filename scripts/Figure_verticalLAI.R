rm(list = ls())

library(dplyr)
library(rhdf5)
library(pracma)

source("/home/femeunier/Documents/ED2/R-utils/h5read_opt.r")

Nruns <- 100
Treatments <-3
treatment.name <- c("Default","Lidar","FC")
init = c(99000015531,99000016031,99000018034)

df_OP <- data.frame()

for (i in seq(1,Treatments)){
  print(i/Treatments)
  for (irun in seq(1,Nruns)){
    # system2("rsync",c("-avz",
    #                   paste0("hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/Wytham/growth_storage_resp/out/",init[i]-1+irun,"/Wytham-S-2007-01-01-000000-g01.h5"),
    #                   paste0("./data/","history_",i,irun,"h5")))

    file <- paste0("./data/","history_",i,irun,"h5")

    if (file.exists(file)){
      mymont    = lapply(h5read_opt(file),FUN=aperm)
      names(mymont) <- gsub(x = names(mymont), pattern = "\\_", replacement = ".")


      df_OP <- bind_rows(list(df_OP,
        data.frame(pft = mymont$PFT,
                   Hite = mymont$HITE,
                   dbh = mymont$DBH,
                   patch = rep(1:length(mymont$PACO.N),mymont$PACO.N),
                   nplant = mymont$NPLANT,
                   LAI = mymont$LAI.CO,
                   simulation = treatment.name[i],
                   run = irun) %>% group_by(patch) %>% mutate(cumLAI = cumsum(LAI))))
    }

  }
}

# add default
# file <- "/home/femeunier/Documents/projects/Hackaton/LidarED/data/history-S-1812-06-01-000000-g01.h5"
# mymont    = lapply(h5read_opt(file),FUN=aperm)
# names(mymont) <- gsub(x = names(mymont), pattern = "\\_", replacement = ".")
#
# df_OP <- bind_rows(list(df_OP,
#                         data.frame(pft = mymont$PFT,
#                                    Hite = mymont$HITE,
#                                    dbh = mymont$DBH,
#                                    patch = rep(1:length(mymont$PACO.N),mymont$PACO.N),
#                                    nplant = mymont$NPLANT,
#                                    LAI = mymont$LAI.CO,
#                                    simulation = "NBG",
#                                    run = 1) %>% group_by(patch) %>% mutate(cumLAI = cumsum(LAI))))
# treatment.name <- as.character(unique(df_OP$simulation))
# Treatments <- length(treatment.name)

z = seq(0,35,0.5)

df_interp <- data.frame()
for (i in seq(1,Treatments)){

  df_temp <- df_OP %>% filter(simulation == treatment.name[i])
  Nruns <- length(unique(df_temp$run))

  for (irun in seq(1,Nruns)){
    df_temp <- df_OP %>% filter(run == irun,
                                simulation == treatment.name[i])
    patches <- df_temp  %>% pull(patch) %>% unique()
    for (ipatch in seq(1,length(patches))){
      df_now <- df_temp %>% filter(patch == patches[ipatch])
      x <- df_now$Hite
      y <-  df_now$LAI
      y2 <- df_now$cumLAI
      yinterp <- interp1(c(50,max(x)+0.01,x,min(x)-0.01,0),c(0,0,y,0,0),z)
      yinterp2 <- interp1(c(50,max(x)+0.01,x,min(x)-0.01,0),c(0,0,y2,0,0),z)
      df_interp <- bind_rows(list(df_interp,
                                  data.frame(hite = z,
                                             LAI = yinterp,
                                             cumLAI = yinterp2,
                                             simulation = treatment.name[i],
                                             run = irun,
                                             patch = ipatch)))
    }
  }
}

zmax <- df_interp %>% group_by(simulation) %>% filter(LAI> 0) %>% summarise(zmax = max(hite))

df_OP.sum <-
  df_interp %>% group_by(hite,simulation,run) %>% summarise(LAI = mean(LAI),cumLAI = mean(cumLAI)) %>% group_by(hite,simulation) %>%
  summarise(LAIm = mean(LAI),
            LAIsd = sd(LAI),
            LAIse = LAIsd/sqrt(length(LAI)),
            cumLAIm = mean(cumLAI),
            cumLAIsd = sd(cumLAI),
            cumLAIse = cumLAIsd/sqrt(length(cumLAI))) %>% filter((simulation == "Default" & hite <= 25.5) |
                                                                   (simulation == "Lidar" & hite <= 22.5) |
                                                                   (simulation == "FC" & hite <= 25.))

# df_OP.sum <-
#   df_OP.sum %>% mutate(
#     simulation = case_when(
#       simulation == "IC-Inventory" ~ "IC-Default",
#       TRUE ~ simulation
#     )
#   )

# df_OP.sum$simulation <- factor(df_OP.sum$simulation, levels(df_OP.sum$simulation)[c(3,1,2)])
levels(df_OP.sum$simulation) <- c('IC-Default','IC-TLS','IC-FC')

ggplot(data = df_OP.sum) +
  geom_line(aes(y = LAIm,x = hite, color = as.factor(simulation))) +
  geom_ribbon(aes(y = LAIm,x = hite, ymin = LAIm - LAIsd,ymax = LAIm + LAIsd,
                  fill = as.factor(simulation)),alpha = 0.5) +
  theme_bw() +
  scale_color_manual(values = c("black","grey","#1E3F5A")) +
  scale_fill_manual(values = c("black","grey","#1E3F5A")) +
  labs(y = "LAI (m²/m²)",x = "Height (m)",color = "Configuration",fill = "Configuration") +
  coord_flip() +
  theme(text = element_text(size = 24),
        legend.position = c(0.85,0.2),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))

# ggplot(data = df_OP.sum) +
#   geom_line(aes(y = cumLAIm,x = hite, color = as.factor(simulation))) +
#   geom_ribbon(aes(y = cumLAIm,x = hite, ymin = cumLAIm - cumLAIsd,ymax = cumLAIm + cumLAIsd,
#                   fill = as.factor(simulation)),alpha = 0.5) +
#   theme_bw() +
#   scale_color_manual(values = c("darkgrey","black")) +
#   scale_fill_manual(values = c("darkgrey","black")) +
#   labs(y = "Mean LAI (m²/m²)",x = "Height (m)",color = "Configuration",fill = "Configuration") +
#   coord_flip() +
#   theme(text = element_text(30),
#         legend.position = c(0.85,0.2),
#         axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
#         axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))


ggsave(plot = last_plot(),"./Figures/Figure_vertical.png",
       dpi = 300,width = 10,height = 7)

# ggplot(data = df_default %>% filter(patch == 1)) +
#   geom_line(aes(x = cumLAI,Hite)) +
#   theme_bw()
#
#
# ggplot(data = df_default %>% filter(patch == 1)) +
#   geom_point(aes(x = LAI,y = Hite, color = as.factor(simulation))) +
#   theme_bw()
