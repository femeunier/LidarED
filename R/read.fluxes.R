read.fluxes <- function(local.dir,years,filename = "analysis"){

  NEP <-
    -unlist(map(1:length(years), function(iyear) {
      file.name <-
        paste0(filename,"-T-", years[iyear], "-00-00-000000-g01.h5")

      local.file <- file.path(local.dir, file.name)

      nc <- nc_open(local.file)

      GPP_flux <- ncvar_get(nc, "FMEAN_GPP_PY") * 1000 / 12 / 86400 * 1e6 / 365
      NPP_flux <- ncvar_get(nc, "FMEAN_NPP_PY") * 1000 / 12 / 86400 * 1e6 / 365
      Rauto_flux <- ncvar_get(nc, "FMEAN_PLRESP_PY") * 1000 / 12 / 86400 * 1e6 / 365
      Rhetero_flux <- ncvar_get(nc, "FMEAN_RH_PY") * 1000 / 12 / 86400 * 1e6 / 365

      nc_close(nc)

      NEE <- -(NPP_flux - Rhetero_flux)
      return(NEE)
    }))


  GPP <-
    unlist(map(1:length(years), function(iyear) {
      file.name <-
        paste0(filename,"-T-", years[iyear], "-00-00-000000-g01.h5")

      local.file <- file.path(local.dir, file.name)

      nc <- nc_open(local.file)

      GPP_flux <- ncvar_get(nc, "FMEAN_GPP_PY") * 1000 / 12 / 86400 * 1e6 / 365

      nc_close(nc)

      return(GPP_flux)
    }))


  R <-
    unlist(map(1:length(years), function(iyear) {
      file.name <-
        paste0(filename,"-T-", years[iyear], "-00-00-000000-g01.h5")

      local.file <- file.path(local.dir, file.name)

      nc <- nc_open(local.file)

      Rauto_flux <- ncvar_get(nc, "FMEAN_PLRESP_PY") * 1000 / 12 / 86400 * 1e6 / 365
      Rhetero_flux <- ncvar_get(nc, "FMEAN_RH_PY") * 1000 / 12 / 86400 * 1e6 / 365

      nc_close(nc)

      R <- (Rauto_flux + Rhetero_flux)
      return(R)
    }))


  time <- seq(as.POSIXct(paste(paste(years[1],'01','01',sep='/'),'06:30:00'),format = "%Y/%m/%d %H:%M:%S",tz = "UTC"),
              as.POSIXct(paste(paste(years[length(years)]+1,'01','01',sep='/'),'06:00:00'),format = "%Y/%m/%d %H:%M:%S",tz = "UTC"),
              length.out = length(NEP))

  flux.model <- data.frame(time = time,
                           Reco = R,
                           NEP =  NEP,
                           GPP = GPP) %>% mutate(week = as.numeric(strftime(time,format="%W")),
                                                 year = year(time),
                                                 t = decimal_date(time)) %>% group_by(year,week) %>% summarise(GPP = mean(GPP),
                                                                                                               NEP = mean(NEP),
                                                                                                               Reco = mean(Reco),
                                                                                                               t = mean(t))
  return(flux.model)
}
