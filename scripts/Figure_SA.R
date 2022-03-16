rm(list = ls())

library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)
library(tidyr)
library(tidyverse)

# Fluxtower data
fluxtower.data <- readRDS("/home/femeunier/Downloads/Fluxtower.RDS") %>% mutate(date = as.Date(as.yearmon(time, format = '%Y%m')),
                                                                                year = year(date),
                                                                                month = month(date))

fluxtower.data.month <- fluxtower.data %>% group_by(month) %>% summarise(GPPm = mean(GPP,na.rm = TRUE),
                                                                         GPPsd = sd(GPP,na.rm = TRUE))

fluxtower.data.month <- fluxtower.data %>%
  group_by(month) %>%
  summarise(GPPmean = mean(GPP,na.rm = TRUE),
            GPPmin = quantile(GPP,probs = 0.025,na.rm = TRUE),
            GPPmax = quantile(GPP,probs = 0.975,na.rm = TRUE))

unit.conversion <- 1000 / 12 / 86400 * 1e6 / 365

# Outputs
system2("rsync",c("-avz","hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/df.Wytham.ensembles.RDS",
                  file.path("/home/femeunier/Documents/projects/Hackaton/LidarED/","outputs","df.Wytham.ensembles.RDS")))

# system2("rsync",c("-avz","hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/Wytham/ensembles/run/Census_noTRY_1/inputs.ensembles.RDS",
#                   file.path("/home/femeunier/Documents/projects/Hackaton/LidarED/","outputs","inputs.ensembles.RDS")))

# Params
df.inputs <- readRDS(file.path("/home/femeunier/Documents/projects/Hackaton/LidarED/","outputs","inputs.ensembles.RDS")) %>%
  filter(pft == "LH")

df.Wytham.ensemble <- readRDS(file.path("/home/femeunier/Documents/projects/Hackaton/LidarED/","outputs","df.Wytham.ensembles.RDS")) %>%
  filter(!is.na(month),
         year < 2011,
         year >= 2006) %>%
  mutate(time = year + (month - 1)/12,
         GPP = unit.conversion*GPP)

df.Wytham.ensemble %>% mutate(isTRY = case_when(grepl("noTRY",name) ~ 0,
                                                TRUE ~ 1)) %>%
  group_by(type,isTRY) %>%
  filter(month == 6) %>%
  summarise(LAI = mean(LAI,na.rm = TRUE))

df.inputs.all <- df.inputs %>%
  filter(crownmod == 1) %>%
  mutate(trait.pft = paste0(trait,".",pft)) %>%
  dplyr::select(-c(pft,trait.pft)) %>%
  pivot_wider(names_from = trait,
              values_from = value)


# Test with GPP in june
df.Wytham.ensemble.long <- df.Wytham.ensemble %>%
  pivot_longer(cols = c(GPP,LAI,AGB,PAR),
               names_to = "variable",
               values_to = "value")

vars <- c("GPP","LAI","AGB","PAR")
types <- c("NBG","Census","TLS")
isTRY <- c(0,1)
df.VarDec.try <- data.frame()

for (var in vars){

  df.op.tests <- df.Wytham.ensemble.long  %>%
    filter(variable == var) %>%
    filter(month == 6) %>%
    group_by(name) %>%
    summarise(value.m = mean(value,na.rm = TRUE))


  df.Wytham.ensemble.inputs <- df.op.tests %>% left_join(df.inputs.all,
                                                         by = c("name")) %>%
    dplyr::select(-c("Npft","crownmod","canrad","plasticity"))

  for (ctype in types){
    for (cisTRY in isTRY){

      df2test <- df.Wytham.ensemble.inputs %>%
        filter(type == ctype,
               isTRY == cisTRY) %>%
        dplyr::select(-c(name,num,isNBG,isTLS,type,isTRY))

      anovobj <- aov(lm(formula = as.formula(paste0("value.m ~ .")),
                        data = df2test,
                        na.action=na.exclude))
      allssq <- summary(anovobj)[[1]][,2]


      df <- data.frame(name = c(rownames(summary(anovobj)[[1]]),"Total"),
                       ssq = c(allssq,sum(allssq))) %>%
        mutate(name = trimws(name, which = "right")) %>%
        mutate(type = case_when(name %in% c("hgt_ref","b1Ht","b2Ht") ~ "Height",
                                name %in% c("b1Ca","b2Ca") ~ "Crown area",
                                name %in% c("b1Bs_small","b2Bs_small") ~ "Bd",
                                name %in% c("b1Bl_small","b2Bl_small") ~ "Bl",
                                TRUE ~ name)) %>%
        ungroup() %>%
        mutate(p.var = c(allssq/sum(allssq),1))

      df.group <- df %>% group_by(type) %>% summarise(ssq.tot = sum(ssq),
                                                      p.var= sum(p.var),
                                                      .groups = "keep")

      df.group.miss <- df.group %>% filter(type != "Total") %>%
        mutate(prop = ssq.tot/ df.group %>%
                 filter(type == "Total") %>%
                 pull(ssq.tot)) %>%
        arrange(desc(type)) %>%
        mutate(lab.ypos = cumsum(prop) - 0.5*prop)

      df.VarDec.try <- bind_rows(list(df.VarDec.try,
                                      df.group.miss %>% mutate(IC = ctype,
                                                               Var = sqrt(var(df2test %>% pull(value.m))),
                                                               isTRY = cisTRY,
                                                               cvar = var)))
    }
  }
}


types.ord <- df.VarDec.try %>% group_by(type) %>% summarise(p.var = mean(p.var),
                                                            .groups = "keep") %>% arrange((p.var)) %>% pull(type)

df.VarDec.try$type <- factor(df.VarDec.try$type,
                             levels = types.ord)

df.VarDec.try$IC <- factor(df.VarDec.try$IC,
                           levels = c("NBG","Census","TLS"))
df.VarDec.try$isTRY <- as.factor(df.VarDec.try$isTRY)
levels(df.VarDec.try$isTRY) <- c("False","True")

# df.VarDec.try$cvar <- factor(df.VarDec.try$cvar,
#                              levels = c("PAR","LAI","GPP","AGB"))

levels(df.VarDec.try$type) <- c("Refl. (VIS)","Refl. (NIR)",
                                "Min. height","Mort. C bal.",
                                "Water cond.",
                                "Stomatal slope",
                                "Clumping",
                                "CA",
                                "Bl",
                                "Leaf resp.",
                                "h",
                                "Root:leaf",
                                "Bd",
                                "Quant. eff.",
                                "Growth resp.",
                                "Vc,max",
                                "SLA",
                                "Residuals")

ggplot(data = df.VarDec.try %>%
         ungroup() %>%
         filter(type %in% (df.VarDec.try %>% filter(p.var > 0.05) %>% pull(type) %>% unique()),
                type != "Residuals")) +
  geom_bar(aes(x = type, y = p.var, fill = cvar),
           stat = "identity",
           width = 0.9,
           position = position_dodge(width = 0.9)) +
  facet_grid(isTRY ~ IC) +
  scale_fill_manual(values = c('#1b9e77','#66a61e','#d95f02','#7570b3','#e7298a','#e6ab02'),
                    breaks = c("PAR","LAI","GPP","AGB")) +
  coord_flip() +
  labs(y = "Partial variance (-)",x = "", fill = "Model output") +
  theme_bw() +
  theme(text = element_text(size = 20),
        legend.position = c(0.9,0.15),
        strip.background = element_blank(),
        strip.text.y = element_text(angle = 270))

ggsave(plot = last_plot(),"/home/femeunier/Documents/projects/Hackaton/LidarED/Figures/Figure_SA.png",
       dpi = 300,width = 30,height = 18,units = "cm")





