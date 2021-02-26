rm(list = ls())

library(ncdf4)
library(dplyr)
library(lubridate)
library(purrr)
library(ggplot2)
library(RColorBrewer)

remote.dir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/Wytham/out/archives/ICANRAD2/reference/analy"
years <- 2007:2009
# local.dir <- tempdir()
local.dir <- "~/data/Wytham/Fluxes/Hmean_CA_AGB_Bl/"

NEE <-
  unlist(map(1:length(years), function(iyear) {
    file.name <-
      paste0("analysis-T-", years[iyear], "-00-00-000000-g01.h5")

    # system2("scp",c(file.path(paste0("hpc:",remote.dir),file.name),
    #                 local.dir))

    local.file <- file.path(local.dir, file.name)

    nc <- nc_open(local.file)

    GPP_flux <- ncvar_get(nc, "FMEAN_GPP_PY") * 1000 / 12 / 86400 * 1e6 / 365
    NPP_flux <- ncvar_get(nc, "FMEAN_NPP_PY") * 1000 / 12 / 86400 * 1e6 / 365
    Rauto_flux <- ncvar_get(nc, "FMEAN_PLRESP_PY") * 1000 / 12 / 86400 * 1e6 / 365
    Rhetero_flux <- ncvar_get(nc, "FMEAN_RH_PY") * 1000 / 12 / 86400 * 1e6 / 365
    LAI <- apply(ncvar_get(nc,"LAI_PY"),3,sum)


    nc_close(nc)

    NEE <- -(NPP_flux - Rhetero_flux)
    return(NEE)
  }))


time <- seq(as.POSIXct(paste(paste(years[1],'01','01',sep='/'),'06:30:00'),format = "%Y/%m/%d %H:%M:%S",tz = "UTC"),
            as.POSIXct(paste(paste(years[length(years)]+1,'01','01',sep='/'),'06:00:00'),format = "%Y/%m/%d %H:%M:%S",tz = "UTC"),
            length.out = length(NEE))
time_tz <- as.POSIXct(format(time, tz="Europe/London",usetz=TRUE))

df <- data.frame(t = time,
                 NEE = NEE) %>% mutate(year = year(t),
                                       month = month(t),
                                       day = day(t),
                                       h = hour(t),
                                       tnum = decimal_date(t),
                                       doy = as.numeric(strftime(t, format = "%j"))) %>% group_by(year) %>% mutate(doy_rel = (doy-1)/max(doy)) %>% ungroup() %>%
  mutate(ydoy = year + doy_rel,
         t2 = as.Date(t,"%Y/%m/%d")) %>%
  filter(year >= years[1] & year <= years[length(years)]) %>%
  filter(t >= as.Date("2007/05/01") &
        t <=  as.Date("2009/05/01"))

df_f <- df %>% group_by(year,month,h) %>% summarise(NEE_m = mean(NEE))
df_t <- df %>% group_by(t2,h) %>% summarise(NEE_m = mean(NEE))

ggplot(data = df_t,
       aes(x = h,y = t2, fill = NEE_m)) +
  geom_tile() +
  scale_fill_distiller(palette = "Spectral")+
  # scale_fill_gradientn(colours = rev(c("firebrick",rainbow(10)))) +
  scale_x_continuous(expand = c(0,0.5),breaks = seq(0,24,6)) +
  scale_y_date(expand = c(0,0)) +
  theme_bw()
