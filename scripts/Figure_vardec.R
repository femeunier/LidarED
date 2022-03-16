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

system2("rsync",c("-avz","hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/Wytham/ensembles/run/Census_noTRY_1/inputs.ensembles.RDS",
                  file.path("/home/femeunier/Documents/projects/Hackaton/LidarED/","outputs","inputs.ensembles.RDS")))

# Params
df.inputs <- readRDS(file.path("/home/femeunier/Documents/projects/Hackaton/LidarED/","outputs","inputs.ensembles.RDS")) %>%
  filter(pft == "LH")

df.Wytham.ensemble <- readRDS(file.path("/home/femeunier/Documents/projects/Hackaton/LidarED/","outputs","df.Wytham.ensembles.RDS")) %>%
  filter(!is.na(month),
         year < 2011,
         year >= 2006) %>%
  mutate(time = year + (month - 1)/12,
         GPP = unit.conversion*GPP)

df.inputs.all <- df.inputs %>%
  mutate(trait.pft = paste0(trait,".",pft)) %>%
  dplyr::select(-c(pft,trait.pft)) %>%
  pivot_wider(names_from = trait,
              values_from = value)

runs2remove <-  df.inputs.all %>% filter((crownmod ==0 & type == "TLS")) %>% pull(num) %>% unique()

# Test with GPP in june

df.op.tests <- df.Wytham.ensemble %>%
  ungroup() %>%
  filter(time == max(time)) %>%
  filter(type == "NBG") %>%
  group_by(name) %>%
  summarise(GPP.m = mean(N,na.rm = TRUE))


df.Wytham.ensemble.inputs <- df.op.tests %>% left_join(df.inputs.all,
                                                       by = c("name"))

df.Wytham.ensemble.inputs %>%
  group_by(type,isTRY) %>%
  summarise(N = length(Npft),
            .groups = "keep")

########################################################################################################
# Per type

# df2test <- df.Wytham.ensemble.inputs %>%
#   filter(type == "TLS",
#          isTRY == 1) %>%
#   dplyr::select(-c(name,num,isNBG,isTLS,type,isTRY))

types <- c("NBG")
df.VarDec <- data.frame()


for (ctype in types){

  df2test <- df.Wytham.ensemble.inputs %>%
    filter(type == ctype) %>%
    dplyr::select(-c(name,num,isNBG,isTLS,type))


  anovobj <- aov(lm(formula = as.formula(paste0("GPP.m ~ .")),
                    data = df2test,
                    na.action=na.exclude))
  allssq <- summary(anovobj)[[1]][,2]

  print(c(sum(allssq),
          var(df2test %>% pull(GPP.m)),
          var(df2test %>% filter(isTRY == 0) %>% pull(GPP.m)),
          var(df2test %>% filter(isTRY == 1) %>% pull(GPP.m))))

  df <- data.frame(name = c(rownames(summary(anovobj)[[1]]),"Total"),
                   ssq = c(allssq,sum(allssq))) %>%
    mutate(name = trimws(name, which = "right")) %>%
    mutate(type = case_when(name %in% c("Npft","crownmod","canrad","plasticity") ~ "process",
                            name == "isTRY" ~ "Meta-analysis",
                            name %in% c("Residuals","Total") ~ name,
                            name %in% c("hgt_ref","b1Ht","b2Ht","b1Ca","b2Ca","b1Bs_small","b2Bs_small","b1Bl_small","b2Bl_small") ~ "Allometric parameters",
                            TRUE ~ "Unconstrained parameters"))

  df.group <- df %>% group_by(type) %>% summarise(ssq.tot = sum(ssq),
                                                  .groups = "keep")

  df.group.miss <- df.group %>% filter(type != "Total") %>% mutate(prop = ssq.tot/ df.group %>% filter(type == "Total") %>% pull(ssq.tot)) %>%
    arrange(desc(type)) %>%
    mutate(lab.ypos = cumsum(prop) - 0.5*prop)


  df.VarDec <- bind_rows(list(df.VarDec,
                              df.group.miss %>% mutate(IC = ctype,
                                                       Var = sqrt(var(df2test %>% pull(GPP.m))))))

}


df.VarDec$IC <- factor(df.VarDec$IC,levels = c("NBG","Census","TLS"))

ggplot(df.VarDec,
       aes(x = (Var/2), y = prop, fill = type, width = (Var))) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  facet_wrap(~ IC) +
  theme_void()


# mydf <- tibble(group = rep(c("group a", "group b"), each = 3),
#                cond = rep(c("x", "y", "z"), times = 2),
#                value = c(1, 2, 3, 2, 4, 6)) %>%
#   group_by(group) %>%
#   add_tally(value, name = "total") %>%
#   ungroup() %>%
#   mutate(label = sprintf("total = %d", total)) %>%
#   print()
#
# mydf %>% ggplot(aes(x = total/2, y = value, fill = cond, width = total)) +
#   geom_bar(stat = "identity", position = "fill", color = "white") +
#   facet_wrap(~ group + label, strip.position = "bottom") +
#   coord_polar("y", start = 0, direction = -1) +
#   theme_bw(base_size = 12) +
#   theme(axis.title = element_blank(),
#         axis.ticks = element_blank(),
#         axis.text = element_blank(),
#         panel.grid = element_blank(),
#         panel.border = element_blank(),
#         legend.title = element_text(size = 14),
#         strip.background = element_rect(fill = NA, colour = NA),
#         strip.text = element_text(size = 16))

##############################################################################################

########################################################################################################
# Per type

# df2test <- df.Wytham.ensemble.inputs %>%
#   filter(type == "TLS",
#          isTRY == 1) %>%
#   dplyr::select(-c(name,num,isNBG,isTLS,type,isTRY))

types <- c("NBG")
isTRY <- c(0)
df.VarDec.try <- data.frame()

ctype = "NBG" ; cisTRY = 0

for (ctype in types){
  for (cisTRY in isTRY){

    df2test <- df.Wytham.ensemble.inputs %>%
      filter(type == ctype,
             isTRY == cisTRY) %>%
      dplyr::select(-c(name,num,isNBG,isTLS,type,isTRY))

    anovobj <- aov(lm(formula = as.formula(paste0("GPP.m ~ .")),
                      data = df2test,
                      na.action=na.exclude))
    allssq <- summary(anovobj)[[1]][,2]

    print(c(sum(allssq),
            var(df2test %>% pull(GPP.m))))

    df <- data.frame(name = c(rownames(summary(anovobj)[[1]]),"Total"),
                     ssq = c(allssq,sum(allssq))) %>%
      mutate(name = trimws(name, which = "right")) %>%
      mutate(type = case_when(name %in% c("Npft","crownmod","canrad","plasticity") ~ "process",
                              name %in% c("isTRY","Vm0","SLA") ~ "Meta-analysis",
                              name %in% c("Residuals","Total") ~ name,
                              name %in% c("hgt_ref","b1Ht","b2Ht","b1Ca","b2Ca","b1Bs_small","b2Bs_small","b1Bl_small","b2Bl_small") ~ "Allometric parameters",
                              TRUE ~ "Unconstrained parameters"))

    df.group <- df %>% group_by(type) %>% summarise(ssq.tot = sum(ssq),
                                                    .groups = "keep")

    df.group.miss <- df.group %>% filter(type != "Total") %>%
      mutate(prop = ssq.tot/ df.group %>%
               filter(type == "Total") %>%
               pull(ssq.tot)) %>%
      arrange(desc(type)) %>%
      mutate(lab.ypos = cumsum(prop) - 0.5*prop)

    df.VarDec.try <- bind_rows(list(df.VarDec.try,
                                    df.group.miss %>% mutate(IC = ctype,
                                                             Var = sqrt(var(df2test %>% pull(GPP.m))),
                                                             isTRY = cisTRY)))
  }
}


df.VarDec.try$IC <- factor(df.VarDec.try$IC,levels = c("NBG","Census","TLS"))
df.VarDec.try$type <- factor(df.VarDec.try$type,
                             levels = c("Allometric parameters","Meta-analysis","Unconstrained parameters",
                                        "process","Residuals"))

df.VarDec.try$isTRY <- as.factor(df.VarDec.try$isTRY)
levels(df.VarDec.try$isTRY) <- c("False","True")

ggplot(df.VarDec.try,
       aes(x = (Var/2), y = prop, fill = type, color = type, width = (Var))) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c('#137300','#1b9e77','#66a61e','#d95f02','#7570b3','#e7298a','#e6ab02')) +
  scale_color_manual(values = c('#137300','#1b9e77','#66a61e','#d95f02','#7570b3','#e7298a','#e6ab02')) +
  facet_grid(isTRY ~ IC) +
  theme_void() +
  theme(text = element_text(size = 24),
        strip.background = element_blank(),
        strip.text.y = element_text(angle = 270)) +
  guides(color = "none", fill = "none")

ggsave(plot = last_plot(),"/home/femeunier/Documents/projects/Hackaton/LidarED/Figures/Figure_VarDec.png",
       dpi = 300,width = 30,height = 18,units = "cm")
