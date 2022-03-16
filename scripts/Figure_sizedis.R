rm(list = ls())

library(rhdf5)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

system2("rsync",c("-avz","hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/df.OP.verticalLAI.RDS",
                  file.path("/home/femeunier/Documents/projects/Hackaton/LidarED/","outputs","df.OP.verticalLAI.RDS")))

df.OP.verticalLAI <- readRDS(file.path("/home/femeunier/Documents/projects/Hackaton/LidarED/","outputs","df.OP.verticalLAI.RDS"))

Treatments <- as.character(unique(df.OP.verticalLAI$conf))

z = seq(0,35,0.5)

df_interp <- data.frame()

# i = 1 ; cisTRY = "noTRY"
for (i in seq(1,length(Treatments))){
  for (cisTRY in c("TRY","noTRY")){
    df_temp <- df.OP.verticalLAI %>% filter(isTRY == cisTRY,
                                            conf == Treatments[i])
    runs <- (unique(df_temp$run))

    for (irun in seq(1,min(100,length(runs)))){
      print(irun/length(runs))
      df_temp_run <- df_temp %>% filter(run == runs[irun])

      patches <- df_temp_run  %>% pull(patch) %>% unique()
      for (ipatch in seq(1,length(patches))){
        df_now <- df_temp_run %>% filter(patch == patches[ipatch])
        x <- df_now$Hite
        y <-  df_now$LAI
        y2 <- df_now$cumLAI
        yinterp <- interp1(c(50,max(x)+0.01,x,min(x)-0.01,0),c(0,0,y,0,0),z)
        yinterp2 <- interp1(c(50,max(x)+0.01,x,min(x)-0.01,0),c(0,0,y2,0,0),z)

        if (max(y2)> 0){
          df_interp <- bind_rows(list(df_interp,
                                      data.frame(hite = z,
                                                 LAI = yinterp,
                                                 cumLAI = yinterp2,
                                                 conf = Treatments[i],
                                                 try = cisTRY,
                                                 run = irun,
                                                 patch = ipatch)))
        }
      }
    }
  }
}


df_OP.sum <-
  df_interp %>% group_by(hite,conf,try,run) %>%
  summarise(LAI = mean(LAI),cumLAI = mean(cumLAI),
            .groups = "keep") %>%
  group_by(hite,conf,try) %>%
  summarise(LAIm = mean(LAI),
            LAIsd = sd(LAI),
            LAIse = LAIsd/sqrt(length(LAI)),
            cumLAIm = mean(cumLAI),
            cumLAIsd = sd(cumLAI),
            cumLAIse = cumLAIsd/sqrt(length(cumLAI)),
            .groups = "keep")


df_OP.sum$try <- factor(df_OP.sum$try, levels = c("noTRY","TRY"))
levels(df_OP.sum$try) <- c("False","True")

ggplot(data = df_OP.sum %>% filter(hite <= 30)) +
  geom_line(aes(y = LAIm,x = hite, color = as.factor(conf))) +
  geom_ribbon(aes(y = LAIm,x = hite, ymin = pmax(0,LAIm - LAIsd),ymax = LAIm + LAIsd,
                  fill = as.factor(conf)),alpha = 0.5) +
  theme_bw() +
  coord_flip() +
  facet_wrap(~ try) +
  scale_fill_manual(values = c('#1b9e77','#d95f02','#e6ab02',"#66a61e")) +
  scale_color_manual(values = c('#1b9e77','#d95f02','#e6ab02',"#66a61e")) +
  labs(y = "Leaf area density (m²/m³)", x = "z (m)",
       fill = "Configuration", color = "Configuration") +
  theme(text = element_text(size = 24),
        legend.position = c(0.89,0.19),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        strip.background.x = element_blank()) +
  guides(fill = "none",
        color = "none")

df_OP.sum %>% group_by(conf,try) %>%
  summarise(sum(LAIm)*0.5*1.2)

ggsave(plot = last_plot(),"./Figures/Figure_zdis.png",
       dpi = 300,width = 30,height = 15,units = "cm")
