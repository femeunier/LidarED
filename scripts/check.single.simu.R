rm(list = ls())

system2("scp",c("hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/Wytham/IC/out/NBG_PFT/analy/analysis.RData",
                file.path("/home/femeunier/Documents/projects/Hackaton/LidarED/","outputs","analysis.RData")))

load(file.path("/home/femeunier/Documents/projects/Hackaton/LidarED/","outputs","analysis.RData"))

plot(datum$emean$gpp,type = "l")

matplot(datum$szpft$lai[,12,c(10,11,18)],type = "l",col = c("red","green","black"))
