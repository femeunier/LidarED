rm(list = ls())

library(pracma)
library(bigleaf)
library(lubridate)
library(bigleaf)
library(cowplot)
library(grid)
library(gridExtra)

file.fluwtower.data <- file.path(getwd(),"data","Fluxtower.csv")
fluwtower.data <- read.csv(file.fluwtower.data)
tinit <- min(c(fluwtower.data[1,1],fluwtower.data[1,3],fluwtower.data[1,5]))
fluwtower.data[1,c(1,3,5)] <- tinit

tend <- max(c(fluwtower.data[,1],fluwtower.data[,3],fluwtower.data[,5]),na.rm=TRUE)
fluwtower.data[which.max(fluwtower.data[,1]),1] <- tend
fluwtower.data[which.max(fluwtower.data[,3]),3] <- tend
fluwtower.data[which.max(fluwtower.data[,5]),5] <- tend

t <- seq(tinit,tend,1/365)
delta_t <- 86400
NEP <- interp1(fluwtower.data[,1],fluwtower.data[,2],t)
GPP <- interp1(fluwtower.data[!is.na(fluwtower.data[,3]),3],fluwtower.data[!is.na(fluwtower.data[,3]),4],t)# All fluxes in µmol C02/m2/s
R <- interp1(fluwtower.data[!is.na(fluwtower.data[,5]),5],fluwtower.data[!is.na(fluwtower.data[,5]),6],t)

par(mfrow = c(3,1))
plot(t,GPP,col = 'darkgreen',type='l',ylim = c(0,22))
plot(t,R,col = 'red',type='l',ylim = c(0,22))
plot(t,NEP,col = 'green',type='l',ylim = c(-10,12))


df.data <-
  data.frame(time = t,
             GPP = GPP,
             Reco = R,
             NEP = NEP)

saveRDS(object = df.data, file = file.path(getwd(),"data","Fluxtower.RDS"))

##############################################################################################
# Monthly averages

df.month <-
  df.data %>% mutate(Date = date_decimal(time),
                     month = month(Date),
                     year = year(Date)) %>% group_by(year,month) %>% summarise(GPP = sum(bigleaf::umolCO2.to.gC(GPP),na.rm=TRUE),
                                                                               Reco = sum(bigleaf::umolCO2.to.gC(Reco),na.rm=TRUE),
                                                                               NEP = sum(bigleaf::umolCO2.to.gC(NEP),na.rm=TRUE)) %>% filter(!(month == 5 & year == 2009))
A <- ggplot(data = df.month,
       aes(x=month, y=NEP, fill=as.factor(year))) +
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_brewer(palette = "YlOrBr") +
  theme_bw()

B <- ggplot(data = df.month,
       aes(x=month, y=GPP, fill=as.factor(year))) +
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_brewer(palette = "Greens") +
  theme_bw()

C <- ggplot(data = df.month,
       aes(x=month, y=Reco, fill=as.factor(year))) +
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_brewer(palette = "Reds") +
  theme_bw()

grid.arrange(A, B, C, nrow = 3)

df.month.flux <-
  df.data %>% mutate(Date = date_decimal(time),
                     month = month(Date),
                     year = year(Date)) %>% group_by(year,month) %>% summarise(GPP = mean(GPP,na.rm=TRUE)*1e-6*12/1000*86400*365,
                                                                               Reco = mean(Reco,na.rm=TRUE)*1e-6*12/1000*86400*365,
                                                                               NEP = mean(NEP,na.rm=TRUE)*1e-6*12/1000*86400*365)

saveRDS(object = df.month.flux, file = file.path(getwd(),"data","Fluxtower_monthly.RDS"))

##############################################################################################
#2007-2012 rep

df.fluxes <-
  df.data %>% mutate(Date = date_decimal(time),
                     day = yday(Date)) %>% group_by(day) %>% summarise(GPP = mean(GPP),
                                                                     Reco = mean(Reco),
                                                                     NEP = mean(NEP))

Time_all <- seq(as.Date("2007/01/01"),as.Date("2012/12/31"),"day")

df.fluxes.Xtrap <-
  data.frame(time = Time_all,
             day = yday(Time_all),
             year = year(Time_all)) %>% mutate(n = year - year[1],
                                               GPP = interp1(df.fluxes$day,
                                                             df.fluxes$GPP,
                                                             day),
                                               Reco = interp1(df.fluxes$day,
                                                              df.fluxes$Reco,
                                                              day),
                                               NEP = interp1(df.fluxes$day,
                                                             df.fluxes$NEP,
                                                             day))

saveRDS(object = df.fluxes.Xtrap, file = file.path(getwd(),"data","Fluxtower_extrap.RDS"))
