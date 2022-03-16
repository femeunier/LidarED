rm(list = ls())

library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)
library(rlang)
library(tidyr)

# system2("rsync",c("-avz","hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/Wytham/ensembles/run/Census_1/inputs.ensembles.RDS",
#                   file.path("/home/femeunier/Documents/projects/Hackaton/LidarED/","outputs","inputs.ensembles.RDS")))

system2("rsync",c("-avz","hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/inputs.ensembles.RDS",
                  file.path("/home/femeunier/Documents/projects/Hackaton/LidarED/","outputs","inputs.ensembles.RDS")))

df.inputs <- readRDS(file.path("/home/femeunier/Documents/projects/Hackaton/LidarED/","outputs","inputs.ensembles.RDS"))
# df.inputs <- readRDS(file.path("/home/femeunier/Documents/projects/Hackaton/LidarED/","data","inputs.ensembles_test.RDS"))

df.inputs.mut <- df.inputs %>%
  mutate(trait = as.character(trait)) %>%
  mutate(type.trait = case_when(trait %in% c("b1Bl_large","b1Bl_small","b2Bl_large","b2Bl_small",
                                             "b1Bs_large","b1Bs_small","b2Bs_large","b2Bs_small",
                                             "b1Ca","b2Ca",
                                             "b1Ht","b2Ht","hgt_ref","hgt_max") ~ "Allom",
                                TRUE ~ "Other"))

df.inputs.mut.wide <- df.inputs.mut %>% dplyr::select(-c(type.trait)) %>%
  filter(Npft == 1) %>%
  pivot_wider(names_from = trait,
              values_from = value) %>%
  mutate(SLA = case_when(is.na(SLA) ~ 60/2,
                         TRUE ~ SLA)) %>%
  mutate(LA = b1Bl_small*SLA)


ggplot(data = df.inputs.mut %>% filter(type.trait == "Other")) +
  geom_boxplot(aes(x = as.factor(pft),y = value, fill = pft)) +
  facet_wrap(type ~ as.factor(trait),scales = "free",nrow = 3) +
  theme_bw()

ggplot(data = df.inputs.mut %>% filter(trait == "growth_resp_factor")) +
  geom_boxplot(aes(x = as.factor(pft),y = value, fill = pft)) +
  facet_wrap(as.factor(Npft) ~ as.factor(isTRY),scales = "free",nrow = 3) +
  theme_bw()

df.inputs.allom <- df.inputs.mut %>% filter(type.trait == "Allom")

ensembles <- as.character(unique(df.inputs.allom$type))
df.alloms <- data.frame()
dbhs <- seq(1,130,length.out = 250)

pfts <- c("LH","MH")
SLAs <- c(60,24.2)

for (i in seq(1,length(ensembles))){

  censemble <- df.inputs.allom %>% filter(type == ensembles[i])

  runs <- unique(censemble$num)
  Nensemble <- length(runs)

  for (iensemble in seq(1,min(50,Nensemble))){

    for (TRY in c(0,1)){

      cparams <- censemble %>% filter(num == runs[iensemble],isTRY == TRY)
      traits <- as.character(cparams$trait)
      Npft <- cparams$Npft[1]

      is_try <- (cparams %>% pull(isTRY))[1]

      for (ipft in seq(1,Npft)){

        cparam.pft <- cparams %>% filter(pft == pfts[ipft])

        if (all(c("hgt_ref","hgt_max","b1Ht","b2Ht") %in% traits)){

          hgt_max <- cparam.pft %>% filter(trait == "hgt_max") %>% pull(value)
          href <- cparam.pft %>% filter(trait == "hgt_ref") %>% pull(value)
          b1Ht <- cparam.pft %>% filter(trait == "b1Ht") %>% pull(value)
          b2Ht <- cparam.pft %>% filter(trait == "b2Ht") %>% pull(value)

          h = pmin(hgt_max,href + b1Ht*(1 -exp(dbhs*b2Ht)))

          df.alloms <- bind_rows(list(df.alloms,
                                      data.frame(type = ensembles[i],
                                                 Npft = Npft,
                                                 pft = pfts[ipft],
                                                 allom.type = "height",
                                                 num = runs[iensemble],
                                                 dbh = dbhs,
                                                 value = h,
                                                 TRY = is_try)))

        }


        if (all(c("b1Ca","b2Ca") %in% traits)){

          b1Ca <- cparam.pft %>% filter(trait == "b1Ca") %>% pull(value)
          b2Ca <- cparam.pft %>% filter(trait == "b2Ca") %>% pull(value)

          CA = b1Ca*(dbhs**b2Ca)

          df.alloms <- bind_rows(list(df.alloms,
                                      data.frame(type = ensembles[i],
                                                 Npft = Npft,
                                                 pft = pfts[ipft],
                                                 allom.type = "CA",
                                                 num = runs[iensemble],
                                                 dbh = dbhs,
                                                 value = CA,
                                                 TRY = is_try)))

        }

        if (all(c("b1Bs_small","b2Bs_small") %in% traits)){

          b1Bs_small <- cparam.pft %>% filter(trait == "b1Bs_small") %>% pull(value)
          b2Bs_small <- cparam.pft %>% filter(trait == "b2Bs_small") %>% pull(value)

          Bs = b1Bs_small*(dbhs**b2Bs_small)

          df.alloms <- bind_rows(list(df.alloms,
                                      data.frame(type = ensembles[i],
                                                 Npft = Npft,
                                                 pft = pfts[ipft],
                                                 allom.type = "Bs",
                                                 num = runs[iensemble],
                                                 dbh = dbhs,
                                                 value = Bs,
                                                 TRY = is_try)))

        }


        if (all(c("b1Bl_small","b2Bl_small") %in% traits)){


          b1Bl_small <- cparam.pft %>% filter(trait == "b1Bl_small") %>% pull(value)
          b2Bl_small <- cparam.pft %>% filter(trait == "b2Bl_small") %>% pull(value)


          Bl = b1Bl_small*(dbhs**b2Bl_small)
          SLA = df.inputs.mut %>% filter(trait == "SLA",num == runs[iensemble],type == ensembles[i],pft == pfts[ipft]) %>% pull(value)*2
          if (is_empty(SLA)){
            SLA = SLAs[ipft]
          } else{
          }

          LA = Bl*SLA

          df.alloms <- bind_rows(list(df.alloms,
                                      data.frame(type = ensembles[i],
                                                 Npft = Npft,
                                                 pft = pfts[ipft],
                                                 allom.type = "LA",
                                                 num = runs[iensemble],
                                                 dbh = dbhs,
                                                 value = LA,
                                                 TRY = is_try)))

          df.alloms <- bind_rows(list(df.alloms,
                                      data.frame(type = ensembles[i],
                                                 Npft = Npft,
                                                 pft = pfts[ipft],
                                                 allom.type = "Bl",
                                                 num = runs[iensemble],
                                                 dbh = dbhs,
                                                 value = Bl,
                                                 TRY = is_try)))

        }
      }
    }
  }
}

ggplot(data = df.alloms %>% filter(allom.type == "Bl",Npft == 1)) +
  geom_line(aes(x = dbh,y = value,color = interaction(Npft,pft),group = interaction(Npft,pft,as.factor(num)))) +
  facet_wrap(type ~ TRY,nrow = 3,scales = "free") +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()

ggplot(data = df.alloms %>% filter(TRY == 1,allom.type == "Bs")) +
  geom_line(aes(x = dbh,y = value,color = interaction(Npft,pft),group = interaction(Npft,pft,as.factor(num)))) +
  facet_wrap(type ~ allom.type,nrow = 3,scales = "free") +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()


ggplot(data = df.alloms %>% filter(allom.type == "CA",Npft == 1)) +
  geom_line(aes(x = dbh,y = value,color = interaction(Npft,pft),group = interaction(Npft,pft,as.factor(num)))) +
  facet_wrap(type ~ TRY,nrow = 3,scales = "free") +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()

# df.alloms %>% filter(allom.type == "LA",dbh == 1)
# df.inputs.allom %>% filter(type == "TLS",trait == "b1Bl_small")

ggplot(data = df.alloms %>% filter(TRY == 0,allom.type == "Bl")) +
  geom_line(aes(x = dbh,y = value,color = interaction(Npft,pft),group = interaction(Npft,pft,as.factor(num)))) +
  facet_wrap(type ~ TRY,nrow = 3,scales = "free") +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()

ggplot(data = df.alloms %>% filter(allom.type == "Bl",Npft == 2)) +
  geom_line(aes(x = dbh,y = value,color = interaction(Npft,pft),group = interaction(Npft,pft,as.factor(num)))) +
  facet_wrap(type ~ TRY,nrow = 3,scales = "free") +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()

ggplot(data = df.alloms %>% filter(TRY == 0,allom.type == "LA")) +
  geom_line(aes(x = dbh,y = value,color = interaction(Npft,pft),group = interaction(Npft,pft,as.factor(num)))) +
  facet_wrap(type ~ allom.type,nrow = 3,scales = "free") +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()


ggplot(data = df.alloms %>% filter(allom.type == "LA",Npft == 1)) +
  geom_line(aes(x = dbh,y = value,color = interaction(Npft,pft),group = interaction(Npft,pft,as.factor(num)))) +
  facet_wrap(type ~ TRY,nrow = 3,scales = "free") +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()


ggplot(data = df.inputs.mut %>% filter(type.trait != "Other",
                                       trait %in% c("b1Bl_small","b2Bl_small"))) +
  geom_boxplot(aes(x = interaction(as.factor(Npft),as.factor(pft)),y = value, fill = pft)) +
  facet_wrap(type ~ as.factor(trait),scales = "free",nrow = 3) +
  theme_bw()


ggplot(data = df.inputs.mut %>% filter(type.trait != "Other",
                                       trait %in% c("b1Bl_small","b2Bl_small"),
                                       Npft == 1)) +
  geom_boxplot(aes(x = (as.factor(type)),y = value, fill = pft)) +
  facet_wrap(as.factor(trait) ~ isTRY,scales = "free",nrow = 3) +
  theme_bw()

View(df.inputs.mut.wide %>% filter(!(num %in% c(6,19,20)) &
                              type == "TLS"))

ggplot(data = df.inputs.mut.wide) +
  geom_boxplot(aes(x = (as.factor(type)),y = LA, fill = pft)) +
  facet_wrap(~ isTRY,scales = "free",nrow = 3) +
  theme_bw()




