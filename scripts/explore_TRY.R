rm(list = ls())

library(dplyr)
library(stringr)
library(Hmisc)

# file.TRY <- "/home/carya/data/Wytham/Traits_Wytham.rds"
file.TRY <- "/home/femeunier/Documents/projects/Hackaton/LidarED/data/Traits_Wytham.rds"
data.TRY <- readRDS(file.TRY)

# Species
# Ac = Acer campestre
# Ap = Acer pseudoplatanus
# Ca = Corylus avellana
# Cm = Crataegus monogyra
# Fe = Fraxinus excelsior
# Qr = Quercus robur

all_species <- c('Ac','Ap','Ca','Cm','Fe','Qr')
pfts <- c(10,11,10,10,10,10)
traits <- c("SLA","Vcmax")
traits.name <- c("SLA","Vm0")
factors <- c(2,1/2.4) # m²/kgC, @ 15°C, @ 15°C
factors <- c(1,1/2.4) # m²/kgC, @ 15°C, @ 15°C
names(factors) <- traits

trait_mean <- trait_unit <- trait_values <- list()

for (trait in traits){
  trait_mean[[trait]] <- c()
  trait_unit[[trait]] <- c()
  trait_values[[trait]] <- list()

  for (ispecies in all_species){
    species_trait <- paste(ispecies,trait,sep = '_')
    temp <- data.TRY[[species_trait]]
    mean <- as.numeric(temp$StdValue)
    units <- unique(temp$UnitName)[1]

    trait_values[[trait]][[ispecies]] <- mean*factors[trait]
    trait_mean[[trait]][ispecies] <- mean(trait_values[[trait]][[ispecies]],na.rm = TRUE)
    trait_unit[[trait]][ispecies] <- units
  }
}



# Community means
data.file <- "/home/femeunier/Documents/projects/Hackaton/LidarED/data/Wytham_trees_summary_ED2_nodead_add.csv"
data.wytham <- read.csv(data.file,header = TRUE) %>% mutate(TLS_ID = as.character(TLS_ID))

data.file2 <- "/home/femeunier/Documents/projects/Hackaton/LidarED/data/Wytham_trees_summary_ED2.csv"
data2.wytham <- read.csv(data.file2,header = TRUE) %>% dplyr::select(TLS_ID,VerticalCrownProjectedArea_pts_.m2.,species) %>% mutate(TLS_ID = as.character(TLS_ID),
                                                                                                                                    species = as.character(species))
data.wytham_sel <- data.wytham %>% left_join(data2.wytham,by = "TLS_ID") %>%
  mutate(dbh = DBH_TLS_.m._x) %>%
  dplyr::select(X,species,dbh) %>% filter(str_length(species)>0) %>% mutate(species = case_when(
  species == "QUERRO" ~ "Qr",
  species == "ACERPS" ~ "Ap",
  species == "FRAXEX" ~ "Fe",
  species == "CORYAV" ~ "Ca",
  species == "CRATMO" ~ "Cm",
  species == "ACERCA" ~ "Ac"))

data.wytham_count <- data.wytham_sel %>% group_by(species) %>% summarise(N = n(),
                                                                         BA = sum(pi/4*dbh**2)) %>% ungroup() %>% mutate(rel_n = N/sum(N),
                                                                                                                         rel_BA = BA/sum(BA))
# w <- data.wytham_count$rel_n
w <- data.wytham_count$rel_BA
names(w) <- data.wytham_count$species


all.obs <- list()
all.weight <- list()
for (trait in traits){
  all.obs[[trait]] <- list()
  for (ipft in seq(1,length(unique(pfts)))){
    cpft = sort(unique(pfts))[ipft]
    for (ispecies in seq(1,length(all_species))){
      species = all_species[ispecies]
      if (pfts[ispecies] == cpft){
        all.obs[[trait]][[as.character(sort(unique(pfts))[ipft])]] <- c(all.obs[[trait]][[as.character(sort(unique(pfts))[ipft])]],
                                                                        trait_values[[trait]][[species]])
        all.weight[[trait]][[as.character(sort(unique(pfts))[ipft])]] <- c(all.weight[[trait]][[as.character(sort(unique(pfts))[ipft])]],
                                                                           as.numeric(rep(w[species],length(trait_values[[trait]][[species]]))))
      }
    }
  }
}



trait_CM <- c()
trait_CM_list <- list()
for (itrait in seq(1,length(traits))){
  trait = traits[itrait]
  trait.name = traits.name[itrait]

  m.na <- trait_mean[[trait]][!is.na(trait_mean[[trait]])]
  w.na <- w[!is.na(trait_mean[[trait]])]
  w.mean <- weighted.mean(m.na,w.na,na.rm = TRUE)
  w.sd <- sqrt(wtd.var(as.numeric(m.na),as.numeric(w.na)/min(w.na)))

  trait_CM[trait] <- w.mean
  trait_CM_list[[trait.name]] <- rnorm(w.mean,w.sd,n = 1000)
}

saveRDS(trait_CM_list,"/home/femeunier/Documents/projects/Hackaton/LidarED/data/TRY_CWM.RDS")
system2("scp",c("/home/femeunier/Documents/projects/Hackaton/LidarED/data/TRY_CWM.RDS",
                "hpc:/scratch/gent/vo/000/gvo00074/hackaton/run_Wytham/ref/data/"))

trait_CM_PFT <- matrix(nrow = length(unique(pfts)),ncol = length(traits))
trait_CM_PFT_list <- list()

for (ipft in seq(1,length(unique(pfts)))){
  trait_CM_PFT_list[[as.character(sort(unique(pfts))[ipft])]] <- list()
  for (itrait in seq(1,length(traits))){
    trait = traits[itrait]
    trait.name = traits.name[itrait]
    X = trait_mean[[trait]][all_species[pfts %in% sort(unique(pfts))[ipft]]]
    W = w[all_species[pfts %in% sort(unique(pfts))[ipft]]]

    w.mean <- weighted.mean(X,W/sum(W),na.rm = TRUE)
    w.sd <- sqrt(wtd.var(all.obs[[traits[itrait]]][[as.character(pfts[ipft])]],
                         all.weight[[traits[itrait]]][[as.character(pfts[ipft])]],na.rm = TRUE))/
      sqrt(length(all.obs[[traits[itrait]]][[as.character(pfts[ipft])]]))
    trait_CM_PFT[ipft,itrait] <- w.mean
    trait_CM_PFT_list[[as.character(sort(unique(pfts))[ipft])]][[trait.name]] <- rnorm(w.mean,w.sd,n = 1000)
  }
}

colnames(trait_CM_PFT) <- names(trait_mean)
rownames(trait_CM_PFT) <- sort(unique(pfts))

saveRDS(trait_CM_PFT_list,"/home/femeunier/Documents/projects/Hackaton/LidarED/data/TRY_CWM_PFT.RDS")
system2("scp",c("/home/femeunier/Documents/projects/Hackaton/LidarED/data/TRY_CWM_PFT.RDS",
                "hpc:/scratch/gent/vo/000/gvo00074/hackaton/run_Wytham/ref/data/"))
