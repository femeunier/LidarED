rm(list = ls())

system2("rsync",c("-avz","hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/df.Wytham.ensembles.RDS",
                  file.path("/home/femeunier/Documents/projects/Hackaton/LidarED/","outputs","df.Wytham.ensembles.RDS")))

unit.conversion <- 1000 / 12 / 86400 * 1e6 / 365

df.Wytham.ensemble <- readRDS(file.path("/home/femeunier/Documents/projects/Hackaton/LidarED/","outputs","df.Wytham.ensembles.RDS")) %>%
  filter(!is.na(month),
         year < 2011,
         year >= 2006) %>%
  mutate(time = year + (month - 1)/12,
         GPP = unit.conversion*GPP,
         Reco = unit.conversion*Reco,
         NEP = unit.conversion*NEP,
         isTRY = case_when(grepl("noTRY",name) ~ "noTRY",
                           TRUE ~ "TRY"))

df.inputs <- readRDS(file.path("/home/femeunier/Documents/projects/Hackaton/LidarED/","outputs","inputs.ensembles.RDS")) %>%
  filter(pft == "LH") %>%
  dplyr::select(type,num,crownmod)

A <- df.Wytham.ensemble %>% dplyr::select(type,num,year,month,Reco,time,isTRY) %>%
  ungroup() %>%
  left_join(df.inputs %>% ungroup(),
            by = c("type","num")) %>%
  group_by(type,crownmod) %>%
  summarise(Reco = mean(Reco)) %>%
  pivot_wider(names_from = c(type),
              values_from = Reco)
as.vector((A[2,2:4] - A[1,2:4])/(A[1,2:4]))

df.Wytham.ensemble %>% dplyr::select(type,num,year,month,PAR,time) %>%
  pivot_wider(names_from = type,
              values_from = PAR)


runs2remove <-  df.inputs %>% filter((crownmod ==0 & type == "TLS")) %>% pull(num) %>% unique()


(df.Wytham.ensemble %>% filter(month %in% seq(5,10)) %>%
  group_by(name) %>%
  summarise(LAI = mean(LAI),
            PAR = mean(PAR),
            GPP = mean(GPP),
            type = type[1],
            num = num[1],
            .groups = "keep") %>%
  filter(type %in% c("Census","NBG") | (type == "TLS" & !(num%in% c(runs2remove)))) %>%
  group_by(type) %>%
  summarise(LAI.m = mean(LAI),
            GPP.m = mean(GPP),
            PAR.m = mean(PAR),
            LAI.sd = sd(LAI),
            GPP.sd= sd(GPP),
            PAR.sd = sd(PAR)))[c(2,1,3),c(1,1+c(1,4,2,5,3,6))]

(df.Wytham.ensemble %>%
    group_by(name) %>%
    summarise(AGB = mean(AGB),
              NEP = mean(NEP),
              Reco = mean(Reco),
              type = type[1],
              num = num[1],
              .groups = "keep") %>%
    filter(type %in% c("Census","NBG") | (type == "TLS" & !(num%in% c(runs2remove)))) %>%
    group_by(type) %>%
    summarise(AGB.m = mean(AGB),
              NEP.m = mean(NEP),
              Reco.m = mean(Reco),
              AGB.sd = sd(AGB),
              NEP.sd= sd(NEP),
              Reco.sd = sd(Reco)))[c(2,1,3),c(1,1+c(1,4,2,5,3,6))]


#######################################################################################################################

df.inputs <- readRDS(file.path("/home/femeunier/Documents/projects/Hackaton/LidarED/","outputs","inputs.ensembles.RDS")) %>%
  filter(pft == "LH",
         trait == "q") %>%
  dplyr::select(type,num,crownmod)

df.Wytham.ensemble.all <- left_join(df.Wytham.ensemble,
                                    df.inputs,
                                    by = c("type","num"))

(df.Wytham.ensemble.all %>% filter(month %in% seq(5,10)) %>%
   group_by(name) %>%
   summarise(LAI = mean(LAI),
             PAR = mean(PAR),
             GPP = mean(GPP),
             type = type[1],
             num = num[1],
             crownmod = crownmod[1],
             .groups = "keep") %>%
   filter(type %in% c("Census","NBG") | (type == "TLS" & !(num%in% c(runs2remove)))) %>%
   group_by(type,crownmod) %>%
   summarise(LAI.m = mean(LAI),
             GPP.m = mean(GPP),
             PAR.m = mean(PAR),
             LAI.sd = sd(LAI),
             GPP.sd= sd(GPP),
             PAR.sd = sd(PAR)))[c(3,4,1,2,5),c(1,2,2+c(1,4,2,5,3,6))]

(df.Wytham.ensemble.all %>%
    group_by(name) %>%
    summarise(AGB = mean(AGB),
              NEP = mean(NEP),
              Reco = mean(Reco),
              type = type[1],
              num = num[1],
              crownmod = crownmod[1],
              .groups = "keep") %>%
    filter(type %in% c("Census","NBG") | (type == "TLS" & !(num%in% c(runs2remove)))) %>%
    group_by(type,crownmod) %>%
    summarise(AGB.m = mean(AGB),
              NEP.m = mean(NEP),
              Reco.m = mean(Reco),
              AGB.sd = sd(AGB),
              NEP.sd= sd(NEP),
              Reco.sd = sd(Reco)))[c(3,4,1,2,5),c(1,2,2+c(1,4,2,5,3,6))]



