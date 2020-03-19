rm(list = ls())

library(ncdf4)
library(purrr)

# OP.dir <- "~/R/LidarED/runs/growth_storage_resp/out/reference_noconfig/"
OP.dir <- "~/R/LidarED/runs/growth_storage_resp/out/Hmean_CA_AGB_Bl/"

datum.file <- file.path(OP.dir,"analysis.RData")
load(datum.file)

year_select <- 2008

######################################################################
years_all <- datum$year
select <- years_all %in% year_select
pos <- which(select)

split_growth_resp <- 0
split_storage_resp <- 0
######################################################################

years <- sort(rep(year_select,12))
months <- rep(1:12,length(year_select))

temp <- map_dfr(seq(1,length(years)),function(i){
  file <- file.path(OP.dir,paste0("analysis-Q-",years[i],"-",sprintf("%02d",months[i]),"-00-000000-g01.h5"))
  nc <- nc_open(file)

  gpp <- ncvar_get(nc,"MMEAN_GPP_PY")
  npp <- ncvar_get(nc,"MMEAN_NPP_PY")
  Rauto <- ncvar_get(nc,"MMEAN_PLRESP_PY")
  Rhet <- ncvar_get(nc,"MMEAN_RH_PY")
  Reco <- Rhet + Rauto

  #################################################################################
  # Some av utils

  ipaconow    <- rep(sequence(ncvar_get(nc,"NPATCHES_GLOBAL")),times=ncvar_get(nc,"PACO_N"))
  nplantconow <- ncvar_get(nc,"NPLANT")
  area <- ncvar_get(nc,"AREA")

  #################################################################################
  # NPP components are fucked up as well, needs to recalculate
  #

  NPPtemp <- (datum$szpft$bleaf[pos[i]+1,12,18]-datum$szpft$bleaf[pos[i],12,18])*12
  NPPleaf <-  ifelse(NPPtemp>0,NPPtemp,0)

  bfroots <- datum$szpft$balive - datum$szpft$bleaf - datum$szpft$bsapwood
  NPProot <- (bfroots[pos[i]+1,12,18] - bfroots[pos[i],12,18])*12

  # NPPfineroot <-  ncvar_get(nc,"MMEAN_NPPFROOT_PY")
  # NPPsw <-  ncvar_get(nc,"MMEAN_NPPSAPWOOD_PY")
  #
  # NPPdaily <- ncvar_get(nc,"MMEAN_NPPDAILY_PY")
  #
  # NPPw <-  ncvar_get(nc,"MMEAN_NPPWOOD_PY")
  # NPPseeds <-  ncvar_get(nc,"MMEAN_NPPSEEDS_PY")
  # NPPcoarseroot <-  ncvar_get(nc,"MMEAN_NPPCROOT_PY")
  #
  # NPPstorage <- NPPdaily - (NPPleaf + NPPfineroot + NPPsw + NPPw + NPPseeds  + NPPcoarseroot)

  # NPPstorageroot <- NPPstorage*0.5
  # NPPstorageleaf <- NPPstorage*0.5
  #
  # NPP_ag <- 0.7*(NPPsw + NPPw) + NPPleaf + NPPseeds + (NPPstorageleaf)
  # NPP_bg <- 0.3*(NPPsw + NPPw) + NPPfineroot + NPPcoarseroot + (NPPstorageroot)
  #
  # NPP_stem <- 0.7*(NPPsw + NPPw)

  # NPPleaf <-  ncvar_get(nc,"MMEAN_NPPLEAF_PY") + NPPstorageleaf
  # NPPsw <-  ncvar_get(nc,"MMEAN_NPPSAPWOOD_PY")
  # NPPfineroot <-  ncvar_get(nc,"MMEAN_NPPFROOT_PY") + NPPstorageroot
  # NPPcoarseroot <-  ncvar_get(nc,"MMEAN_NPPCROOT_PY")

  #################################################################################

  RCWD <-  ncvar_get(nc,"MMEAN_CWD_RH_PY")

  # Respiration is fucked up, needs to recalculate

  plrespconow <- ncvar_get(nc,"MMEAN_PLRESP_CO")

  leaf.resp <- weighted.mean(tapply( X= ncvar_get(nc,"MMEAN_LEAF_RESP_CO") * nplantconow, INDEX = ipaconow, FUN = sum),area)
  root.resp <- weighted.mean(tapply( X= ncvar_get(nc,"MMEAN_ROOT_RESP_CO") * nplantconow, INDEX = ipaconow, FUN = sum),area)

  sapa.storage.resp <- weighted.mean(tapply( X= ncvar_get(nc,"MMEAN_SAPA_STORAGE_RESP_CO") * nplantconow, INDEX = ipaconow, FUN = sum),area)
  root.storage.resp <- weighted.mean(tapply( X= ncvar_get(nc,"MMEAN_ROOT_STORAGE_RESP_CO") * nplantconow, INDEX = ipaconow, FUN = sum),area)
  leaf.storage.resp <- weighted.mean(tapply( X= ncvar_get(nc,"MMEAN_LEAF_STORAGE_RESP_CO") * nplantconow, INDEX = ipaconow, FUN = sum),area)
  sapb.storage.resp <- weighted.mean(tapply( X= ncvar_get(nc,"MMEAN_SAPB_STORAGE_RESP_CO") * nplantconow, INDEX = ipaconow, FUN = sum),area)

  sapa.growth.resp <- weighted.mean(tapply( X= ncvar_get(nc,"MMEAN_SAPA_GROWTH_RESP_CO") * nplantconow, INDEX = ipaconow, FUN = sum),area)

  if (split_storage_resp == 1){

    broot <- weighted.mean(tapply( X= ncvar_get(nc,"BROOT") * nplantconow, INDEX = ipaconow, FUN = sum),area)
    bsapa <- weighted.mean(tapply( X= ncvar_get(nc,"BSAPWOODA") * nplantconow, INDEX = ipaconow, FUN = sum),area)
    bsapb <- weighted.mean(tapply( X= ncvar_get(nc,"BSAPWOODB") * nplantconow, INDEX = ipaconow, FUN = sum),area)
    bleaf <- weighted.mean(tapply( X= ncvar_get(nc,"BLEAF") * nplantconow, INDEX = ipaconow, FUN = sum),area)

    balive <- broot + bsapa + bsapb + bleaf

    growth_resp_int <- sapa.growth.resp/balive

    sapa.growth.resp <- growth_resp_int*bsapa
    root.growth.resp <- growth_resp_int*broot
    leaf.growth.resp <- growth_resp_int*bleaf
    sapb.growth.resp <- growth_resp_int*bsapb

  } else {
    root.growth.resp <- weighted.mean(tapply( X= ncvar_get(nc,"MMEAN_ROOT_GROWTH_RESP_CO") * nplantconow, INDEX = ipaconow, FUN = sum),area)
    leaf.growth.resp <- weighted.mean(tapply( X= ncvar_get(nc,"MMEAN_LEAF_GROWTH_RESP_CO") * nplantconow, INDEX = ipaconow, FUN = sum),area)
    sapb.growth.resp <- weighted.mean(tapply( X= ncvar_get(nc,"MMEAN_SAPB_GROWTH_RESP_CO") * nplantconow, INDEX = ipaconow, FUN = sum),area)
  }


  growth.resp <- root.growth.resp + leaf.growth.resp + sapa.growth.resp + sapb.growth.resp
  storage.resp <- leaf.storage.resp + root.storage.resp + sapa.storage.resp + sapb.storage.resp

  Rleaf  <- leaf.resp    + leaf.storage.resp + leaf.growth.resp
  Rstem  <- sapa.storage.resp + sapa.growth.resp
  Rfineroot <- root.resp + root.growth.resp + root.storage.resp
  Rcoarseroot <- sapb.storage.resp + sapb.growth.resp
  Rroot <- Rfineroot + Rcoarseroot

  litterfall <- 365.25*weighted.mean(tapply( X= ncvar_get(nc,"MMEAN_LEAF_DROP_CO") * nplantconow, INDEX = ipaconow, FUN = sum),area)

  nc_close(nc)

  data.frame(year = years[i],month = months[i],
             gpp = gpp, npp = npp, Reco = Reco,Rauto = Rauto, Rhet = Rhet,
             Rleaf = Rleaf, Rstem = Rstem, Rroot = Rroot, RCWD = RCWD,
             litterfall = litterfall,
             NPPleafseeds = NPPleaf,
             NPProot = NPProot)
             # npp_ag = NPP_ag,npp_bg = NPP_bg,
             # NPPstem = NPP_stem, NPPleafseeds = NPPleaf + NPPseeds,NPPcoarseroot = NPPcoarseroot, NPPfineroot = NPPfineroot,NPPstorage = NPPstorage)
})

colMeans(temp[,-c(1,2)]*10)
