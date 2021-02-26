read.fluxes.Q <- function(local.dir,years,months = 1:12,filename = "analysis"){

  all.years.months <- expand.grid(months,years)
  all.months <- all.years.months[,1]
  all.years <- all.years.months[,2]

  NEP <-
    -unlist(map2(1:length(all.years),1:length(all.months),function(iyear,imonth) {
      file.name <-
        paste0(filename,"-Q-", all.years[iyear], "-",sprintf("%02d",all.months[imonth]),"-00-000000-g01.h5")

      local.file <- file.path(local.dir, file.name)

      nc <- nc_open(local.file)

      GPP_flux <- ncvar_get(nc, "MMEAN_GPP_PY") * 1000 / 12 / 86400 * 1e6 / 365
      NPP_flux <- ncvar_get(nc, "MMEAN_NPP_PY") * 1000 / 12 / 86400 * 1e6 / 365
      Rauto_flux <- ncvar_get(nc, "MMEAN_PLRESP_PY") * 1000 / 12 / 86400 * 1e6 / 365
      Rhetero_flux <- ncvar_get(nc, "MMEAN_RH_PY") * 1000 / 12 / 86400 * 1e6 / 365

      nc_close(nc)

      NEE <- -(NPP_flux - Rhetero_flux)
      return(NEE)
    }))


  GPP <-
    unlist(map2(1:length(all.years),1:length(all.months),function(iyear,imonth) {
      file.name <-
        paste0(filename,"-Q-", all.years[iyear], "-",sprintf("%02d",all.months[imonth]),"-00-000000-g01.h5")

      local.file <- file.path(local.dir, file.name)

      nc <- nc_open(local.file)

      GPP_flux <- ncvar_get(nc, "MMEAN_GPP_PY") * 1000 / 12 / 86400 * 1e6 / 365

      nc_close(nc)

      return(GPP_flux)
    }))


  R <-
    unlist(map2(1:length(all.years),1:length(all.months),function(iyear,imonth) {
      file.name <-
        paste0(filename,"-Q-", all.years[iyear], "-",sprintf("%02d",all.months[imonth]),"-00-000000-g01.h5")

      local.file <- file.path(local.dir, file.name)

      nc <- nc_open(local.file)

      Rauto_flux <- ncvar_get(nc, "MMEAN_PLRESP_PY") * 1000 / 12 / 86400 * 1e6 / 365
      Rhetero_flux <- ncvar_get(nc, "MMEAN_RH_PY") * 1000 / 12 / 86400 * 1e6 / 365

      nc_close(nc)

      R <- (Rauto_flux + Rhetero_flux)
      return(R)
    }))


  time <- all.years + (all.months-1)/12

  flux.model <- data.frame(time = time,
                           year = all.years,
                           month = all.months,
                           Reco = R,
                           NEP =  NEP,
                           GPP = GPP)

  return(flux.model)
}
