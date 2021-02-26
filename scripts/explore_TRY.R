rm(list = ls())

library(dplyr)
library(stringr)

file.TRY <- "/home/carya/data/Wytham/Traits_Wytham.rds"
data.TRY <- readRDS(file.TRY)

# Species
# Ac = Acer campestre
# Ap = Acer pseudoplatanus
# Ca = Corylus avellana
# Cm = Crataegus monogyra
# Fe = Fraxinus excelsior
# Qr = Quercus robur

all_species <- c('Ac','Ap','Ca','Cm','Fe','Qr')
traits <- c("SLA","Vcmax","Jmax")
factors <- c(2,1/2.4,1/2.4) # m²/kgC, @ 15°C, @ 15°C
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
    trait_mean[[trait]][ispecies] <- mean(trait_values[[trait]][[ispecies]])
    trait_unit[[trait]][ispecies] <- units
  }
}

# Community means
data.file <- "./data/Wytham_trees_summary_ED2_nodead_add.csv"
data.wytham <- read.csv(data.file,header = TRUE) %>% mutate(TLS_ID = as.character(TLS_ID))

data.file2 <- "./data/Wytham_trees_summary_ED2.csv"
data2.wytham <- read.csv(data.file2,header = TRUE) %>% dplyr::select(TLS_ID,VerticalCrownProjectedArea_pts_.m2.,species) %>% mutate(TLS_ID = as.character(TLS_ID),
                                                                                                                                    species = as.character(species))
data.wytham_sel <- data.wytham %>% left_join(data2.wytham,by = "TLS_ID") %>% dplyr::select(X,species) %>% filter(str_length(species)>0) %>% mutate(species = case_when(
  species == "QUERRO" ~ "Qr",
  species == "ACERPS" ~ "Ap",
  species == "FRAXEX" ~ "Fe",
  species == "CORYAV" ~ "Ca",
  species == "CRATMO" ~ "Cm",
  species == "ACERCA" ~ "Ac"))

data.wytham_count <- data.wytham_sel %>% group_by(species) %>% summarise(N = n()) %>% ungroup() %>% mutate(rel_n = N/sum(N))
w <- data.wytham_count$rel_n
names(w) <- data.wytham_count$species

trait_CM <- c()
for (trait in traits){
  trait_CM[trait] <- weighted.mean(trait_mean[[trait]],w,na.rm = TRUE)
}
