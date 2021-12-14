make_IC_ED2 <- function(census.csv,iata){
  # detach("package:LidarED", unload=TRUE)

  IED_INIT_MODE = 6 # ED2 input file 3 or 6

  #==========================================================================================#
  #==========================================================================================#
  #==========================================================================================#
  #==========================================================================================#
  #        THE FOLLOWING BLOCK ALLOWS YOU TO CONTROL MOST SETTINGS FOR DATA ANALYSIS.        #
  #==========================================================================================#
  #==========================================================================================#
  #==========================================================================================#
  #==========================================================================================#


  setwd("/home/femeunier/Documents/projects/Hackaton/LidarED")

  #------------------------------------------------------------------------------------------#
  #     Set paths.                                                                           #
  #------------------------------------------------------------------------------------------#
  here    = file.path(getwd(),"scripts")             # Current path
  there   = file.path(here,"../data") # Census path
  outpath = file.path(here,"../data") # Path where the output should be written
  #------------------------------------------------------------------------------------------#

  setwd(here)

  #------------------------------------------------------------------------------------------#
  #     Location of interest.                                                                #
  #------------------------------------------------------------------------------------------#
  place      = "Wytham"   # Name of the site, for output directory
  # census.csv = "Wytham_census_formattedPFT.csv"  # Name of the csv file with the forest inventory data
  # iata       = "WythamPFT"             # Tag for site (short name for output files)
  identity   = ""         # Unique identification (in case you have multiple tests).
  lon        = -1.34      # Longitude of the site
  lat        =  51.78     # Latitude of the site
  year.out   = 2012       # Year of measurements
  #------------------------------------------------------------------------------------------#



  #------------------------------------------------------------------------------------------#
  #     Plot information.                                                                    #
  #------------------------------------------------------------------------------------------#
  subplot.area    = 20*20   # Area of each subplot (all trees) [m2]
  allplot.area    = 20*20   # Area of each plot    (all large trees) [m2]
  nplots          = 40     # Number of plots
  min.dbh.subplot = 2.     # Minimum DBH in the sub-sample
  min.dbh.allplot = 2.     # Minimum DBH for all plot

  #------------------------------------------------------------------------------------------#


  #------------------------------------------------------------------------------------------#
  #     Soil carbon information.  These numbers came from a previous simulation, but you can #
  # use site level data instead.                                                             #
  #------------------------------------------------------------------------------------------#
  fast.soil.c   = 0.1495   #  Litter layer              [kgC/m2]
  struct.soil.c = 6.126    #  Structural soil carbon    [kgC/m2]
  struct.soil.l = 6.126    #  Structural soil lignin    [kgC/m2]
  slow.soil.c   = 4.546    #  Slow soil carbon          [kgC/m2]
  min.soil.n    = 0.639    #  Mineralised soil nitrogen [kgN/m2]
  fast.soil.n   = 0.00348  #  Fast soil nitrogen        [kgN/m2]
  #------------------------------------------------------------------------------------------#


  #------------------------------------------------------------------------------------------#
  #     Set allometric option (not necessary, this is just to make the dummy columns in the  #
  # pss and css files to be consistent).  This has the same meaning as the ED2IN option.     #
  #------------------------------------------------------------------------------------------#
  iallom = 3
  #------------------------------------------------------------------------------------------#

  #------------------------------------------------------------------------------------------#
  #     PFTs that will be used.  Currently this script supports only PFTs 2, 3, 4.  Feel     #
  # free to adapt it for other PFTs.                                                         #
  #------------------------------------------------------------------------------------------#
  pft.idx     = c(2,3,4)
  #------------------------------------------------------------------------------------------#


  #------------------------------------------------------------------------------------------#
  #------------------------------------------------------------------------------------------#
  #------------------------------------------------------------------------------------------#
  #------------------------------------------------------------------------------------------#
  #------------------------------------------------------------------------------------------#
  #------------------------------------------------------------------------------------------#
  #------------------------------------------------------------------------------------------#
  #------------------------------------------------------------------------------------------#
  #            CHANGES BEYOND THIS POINT ARE FOR ADJUSTING THE INPUT FILE ONLY.              #
  #------------------------------------------------------------------------------------------#
  #------------------------------------------------------------------------------------------#
  #------------------------------------------------------------------------------------------#
  #------------------------------------------------------------------------------------------#
  #------------------------------------------------------------------------------------------#
  #------------------------------------------------------------------------------------------#
  #------------------------------------------------------------------------------------------#
  #------------------------------------------------------------------------------------------#




  #------------------------------------------------------------------------------------------#
  #     Load some useful functions.                                                          #
  #------------------------------------------------------------------------------------------#
  #==========================================================================================#
  #==========================================================================================#
  #       Function that converts DBH to height.                                              #
  #------------------------------------------------------------------------------------------#
  dbh2h <- function(dbh,ipft,use.crit=TRUE){
    #----- Make sure that the PFT variable has the same length as dbh. ---------------------#
    if (length(ipft) == 1){
      zpft = rep(ipft,times=length(dbh))
    }else{
      zpft = ipft
    }#end if
    #---------------------------------------------------------------------------------------#



    #----- Limit dbh to dbh.crit. ----------------------------------------------------------#
    if (use.crit){
      dbhuse = pmin(pft$dbh.crit[zpft],dbh) + 0. * dbh
    }else{
      dbhuse = dbh
    }#end if (use.crit)
    #---------------------------------------------------------------------------------------#


    #----- Find out which functional form to use. ------------------------------------------#
    tropo         = pft$tropical[zpft] & iallom %in% c(0,1,3)
    tropn         = pft$tropical[zpft] & iallom %in% c(2)
    tempe         = ! pft$tropical[zpft]
    #---------------------------------------------------------------------------------------#


    #----- Handy aliases. ------------------------------------------------------------------#
    hgt.ref = pft$hgt.ref[zpft]
    b1Ht    = pft$b1Ht   [zpft]
    b2Ht    = pft$b2Ht   [zpft]
    #---------------------------------------------------------------------------------------#



    #----- Find height. --------------------------------------------------------------------#
    h = ifelse( test = tempe
                , yes  = hgt.ref + b1Ht * (1. - exp(b2Ht * dbhuse) )
                , no   = ifelse( test = tropn
                                 , yes  = hgt.ref * (1.-exp(-b1Ht * dbhuse^b2Ht ) )
                                 , no   = exp(b1Ht + b2Ht * log(dbhuse) )
                )#end ifelse
    )#end ifelse
    #---------------------------------------------------------------------------------------#

    return(h)
  }#end function dbh2h
  #==========================================================================================!
  #==========================================================================================!






  #==========================================================================================#
  #==========================================================================================#
  #       Function that converts size (DBH and Height) to biomass of living tissues.         #
  #------------------------------------------------------------------------------------------#
  size2ba <- function(dbh,hgt,ipft,use.crit=TRUE){
    #----- Make sure that the PFT variable has the same length as dbh. ---------------------#
    if (length(ipft) == 1){
      zpft = rep(ipft,times=length(dbh))
    }else{
      zpft = ipft
    }#end if
    #---------------------------------------------------------------------------------------#


    #----- Limit dbh to dbh.crit. ----------------------------------------------------------#
    if (use.crit){
      dbhuse  = pmin(dbh,pft$dbh.crit[zpft]) + 0. * dbh
    }else{
      dbhuse  = dbh
    }#end if (use.crit)
    #---------------------------------------------------------------------------------------#


    #----- Decide which variable to use as dependent variable (DBH or DBH^2*Hgt). ----------#
    size     = ifelse( test = pft$tropical[zpft] & (! pft$liana[zpft]) & (iallom %in% 3)
                       , yes  = dbhuse * dbhuse * hgt
                       , no   = dbhuse
    )#end ifelse
    #---------------------------------------------------------------------------------------#



    #----- Find leaf biomass. --------------------------------------------------------------#
    bleaf  = pft$b1Bl[zpft] / C2B * size ^ pft$b2Bl[zpft]
    broot  = pft$qroot[zpft] * bleaf
    bsapw  = pft$qsw  [zpft] * hgt * bleaf
    bbark  = pft$qbark[zpft] * hgt * bleaf
    balive = bleaf + broot + bsapw + bbark
    #---------------------------------------------------------------------------------------#


    return(balive)
  }# end function size2ba
  #==========================================================================================#
  #==========================================================================================#






  #==========================================================================================#
  #==========================================================================================#
  #       Function that converts size (DBH and Height) to biomass of structural tissues.     #
  #------------------------------------------------------------------------------------------#
  size2bd <- function(dbh,hgt,ipft){
    #----- Make sure that the PFT variable has the same length as dbh. ---------------------#
    if (length(ipft) == 1){
      zpft = rep(ipft,times=length(dbh))
    }else{
      zpft = ipft
    }#end if
    #---------------------------------------------------------------------------------------#


    #----- Decide which variable to use as dependent variable (DBH or DBH^2*Hgt). ----------#
    size     = ifelse( test = pft$tropical[zpft] & (! pft$liana[zpft]) & (iallom %in% 3)
                       , yes  = dbh * dbh * dbh2h(dbh,ipft=zpft)
                       , no   = dbh
    )#end ifelse
    #---------------------------------------------------------------------------------------#



    #----- Select allometric parameters based on the size. ---------------------------------#
    bdead = ifelse( test = dbh < pft$dbh.crit[zpft]
                    , yes  = pft$b1Bs.small[zpft] / C2B * size ^ pft$b2Bs.small[zpft]
                    , no   = pft$b1Bs.large[zpft] / C2B * size ^ pft$b2Bs.large[zpft]
    )#end ifelse
    #---------------------------------------------------------------------------------------#

    return(bdead)
  }# end function size2bd
  #==========================================================================================#
  #==========================================================================================#






  #==========================================================================================#
  #==========================================================================================#
  #       Function that converts size (DBH and Height) to individual leaf area index.        #
  #------------------------------------------------------------------------------------------#
  size2lai <- function(dbh,hgt,nplant,ipft,use.crit=TRUE){
    #----- Make sure that the PFT variable has the same length as dbh. ---------------------#
    if (length(ipft) == 1){
      zpft = rep(ipft,times=length(dbh))
    }else{
      zpft = ipft
    }#end if
    #---------------------------------------------------------------------------------------#


    #----- Limit dbh to dbh.crit. ----------------------------------------------------------#
    if (use.crit){
      dbhuse  = pmin(dbh,pft$dbh.crit[zpft]) + 0. * dbh
    }else{
      dbhuse  = dbh
    }#end if (use.crit)
    #---------------------------------------------------------------------------------------#


    #----- Decide which variable to use as dependent variable (DBH or DBH^2*Hgt). ----------#
    size     = ifelse( test = pft$tropical[zpft] & (! pft$liana[zpft]) & (iallom %in% 3)
                       , yes  = dbhuse * dbhuse * hgt
                       , no   = dbhuse
    )#end ifelse
    #---------------------------------------------------------------------------------------#



    #----- Find leaf biomass. --------------------------------------------------------------#
    bleaf  = pft$b1Bl[zpft] / C2B * size ^ pft$b2Bl[zpft]
    lai    = nplant * pft$SLA[zpft] * bleaf
    #---------------------------------------------------------------------------------------#


    return(lai)
  }# end function size2lai
  #==========================================================================================#
  #==========================================================================================#

  #------------------------------------------------------------------------------------------#


  #------------------------------------------------------------------------------------------#
  #     Set some constants.                                                                  #
  #------------------------------------------------------------------------------------------#
  C2B <<- 2.0  # Biomass:Carbon ratio [kg_Bio / kg_C]
  #------------------------------------------------------------------------------------------#


  #----- Create output directory if it doesn't exist. ---------------------------------------#
  outplace = file.path(outpath,place)
  dummy    = dir.create(outplace,recursive=TRUE,showWarnings=FALSE)
  #------------------------------------------------------------------------------------------#


  #------------------------------------------------------------------------------------------#
  #      Read the table with PFT parameters.                                                 #
  #------------------------------------------------------------------------------------------#
  pft.table =   file.path(here,"pft_allom_table.csv")
  cat(" + Read in the PFT table (",basename(pft.table),").","\n",sep="")
  pft       =   read.csv(pft.table,header=TRUE,stringsAsFactors=FALSE)
  pft       =   pft[pft$iallom == iallom,,drop=FALSE]
  pft       =   pft[order(pft$ipft),,drop=FALSE]
  pft       <<- pft


  #------------------------------------------------------------------------------------------#


  #----- Read forest inventory data. --------------------------------------------------------#
  census.input = file.path(there,census.csv)
  cat(" + Read in the data set (",basename(census.input),").","\n",sep="")
  census    = read.csv(census.input,header=TRUE,stringsAsFactors=FALSE)
  ncohorts  = nrow(census)
  #------------------------------------------------------------------------------------------#



  #------------------------------------------------------------------------------------------#
  #     Find the plant functional type.  We use wood density to determine the break points.  #
  #------------------------------------------------------------------------------------------#
  pft.mid.rho = pft$rho[pft.idx]
  npft        = length(pft.mid.rho)
  pft.brks    = c(-Inf,0.5*(pft.mid.rho[-1]+pft.mid.rho[-npft]),Inf)
  # pft.brks    = c(-Inf,(pft.mid.rho[-npft]),Inf)
  pft.cut     = as.numeric(cut(census$wood.dens,pft.brks))
  census$pft  = pft.idx[pft.cut]

  #------------------------------------------------------------------------------------------#


  #------------------------------------------------------------------------------------------#
  #     Find the demographic density of the individuals.                                     #
  #------------------------------------------------------------------------------------------#
  census$n = with(census, ifelse(dbh < min.dbh.allplot,1/subplot.area,1/allplot.area))

  #------------------------------------------------------------------------------------------#



  #------------------------------------------------------------------------------------------#
  #     Estimate biomass and leaf area index.                                                #
  #------------------------------------------------------------------------------------------#
  census$height = with(census,dbh2h   (dbh=dbh,ipft=pft))
  census$balive = with(census,size2ba (dbh=dbh,hgt=height,ipft=pft))
  census$bdead  = with(census,size2bd (dbh=dbh,hgt=height,ipft=pft))
  census$lai    = with(census,size2lai(dbh=dbh,hgt=height,nplant=n,ipft=pft))
  #------------------------------------------------------------------------------------------#



  #------------------------------------------------------------------------------------------#
  #     Organise the dat using the tags, blocks, and coordinates.                            #
  #------------------------------------------------------------------------------------------#
  o             = order(census$plots,-census$dbh,census$tag)
  census        = census[o,,drop=FALSE]
  #------------------------------------------------------------------------------------------#



  #------------------------------------------------------------------------------------------#
  #    File names for output.                                                                #
  #------------------------------------------------------------------------------------------#
  outprefix = paste0(iata,identity,".lat",sprintf("%.3f",lat),"lon",sprintf("%.3f",lon))
  pssfile   = file.path(outplace,paste0(outprefix,".pss"))
  cssfile   = file.path(outplace,paste0(outprefix,".css"))
  #------------------------------------------------------------------------------------------#





  #------------------------------------------------------------------------------------------#
  #     Format the PSS/CSS files.                                                            #
  #------------------------------------------------------------------------------------------#
  cat (" + Create PSS/CSS file.","\n")
  #---------------------------------------------------------------------------------------#
  #      Output data frame.                                                               #
  #---------------------------------------------------------------------------------------#

  #########################################################################################
  # Hack for pft number
  census$pft <- 10
  census$pft[census$wood.dens < 0.72] <- 11
  #########################################################################################

  outcohorts  = data.frame( time   = sprintf("%4.4i"  , rep(year.out,times=ncohorts))
                            , patch  = sprintf("0x%3.3X", census$plot   )
                            , cohort = sprintf("0x%3.3X", census$tag    )
                            , dbh    = sprintf("%9.3f"  , census$dbh    )
                            , hite   = sprintf("%9.3f"  , census$height )
                            , pft    = sprintf("%5i"    , census$pft    )
                            , n      = sprintf("%15.8f" , census$n      )
                            , bdead  = sprintf("%9.3f"  , rep(0.,times=ncohorts)  )
                            , balive = sprintf("%9.3f"  , census$balive )
                            , lai    = sprintf("%10.4f" , census$lai    )
  )#end data.frame
  #---------------------------------------------------------------------------------------#


  #----- Write the cohort file. ----------------------------------------------------------#
  dummy   = write.table( x         = outcohorts
                         , file      = cssfile
                         , append    = FALSE
                         , quote     = FALSE
                         , sep       = " "
                         , row.names = FALSE
                         , col.names = TRUE
  )#end write.table
  #---------------------------------------------------------------------------------------#


  #---------------------------------------------------------------------------------------#
  #    Format the output so the table is legible.                                         #
  #---------------------------------------------------------------------------------------#


  npatches   = length(unique(census$plots))




  if (IED_INIT_MODE == 6){
    outpatches = list( time  = sprintf("%4.4i"  ,rep(year.out     ,times=npatches))
                       , patch = sprintf("0x%3.3X",unique(census$plots))
                       , trk   = sprintf("%5i"    ,rep(2            ,times=npatches))
                       , age   = sprintf("%6.1f"  ,rep(0            ,times=npatches))
                       , area  = sprintf("%9.7f"  ,rep(1/npatches   ,times=npatches))
                       , water = sprintf("%5i"    ,rep(0            ,times=npatches))
                       , fsc   = sprintf("%10.5f" ,rep(fast.soil.c  ,times=npatches))
                       , stsc  = sprintf("%10.5f" ,rep(struct.soil.c,times=npatches))
                       , stsl  = sprintf("%10.5f" ,rep(struct.soil.l,times=npatches))
                       , ssc   = sprintf("%10.5f" ,rep(slow.soil.c  ,times=npatches))
                       , lai   = sprintf("%10.5f" ,with(census,tapply(lai,plots,sum)))
                       , msn   = sprintf("%10.5f" ,rep(min.soil.n   ,times=npatches))
                       , fsn   = sprintf("%10.5f" ,rep(fast.soil.n  ,times=npatches))
                       , nep   = sprintf("%10.5f" ,rep(0            ,times=npatches))
                       , gpp   = sprintf("%10.5f" ,rep(0            ,times=npatches))
                       , rh    = sprintf("%10.5f" ,rep(0            ,times=npatches))
    )#end data.frame
  } else if (IED_INIT_MODE == 3){

    outpatches = list( Site  = sprintf("%5i"      ,rep(1            ,times=npatches))
                       , time  = sprintf("%4.4i"  ,rep(year.out     ,times=npatches))
                       , patch = sprintf("0x%3.3X",unique(census$plots))
                       , trk   = sprintf("%5i"    ,rep(2            ,times=npatches))
                       , age   = sprintf("%6.1f"  ,rep(0            ,times=npatches))
                       , area  = sprintf("%9.7f"  ,rep(1/npatches   ,times=npatches))
                       , water = sprintf("%5i"    ,rep(0            ,times=npatches))
                       , fsc   = sprintf("%10.5f" ,rep(fast.soil.c  ,times=npatches))
                       , stsc  = sprintf("%10.5f" ,rep(struct.soil.c,times=npatches))
                       , stsl  = sprintf("%10.5f" ,rep(struct.soil.l,times=npatches))
                       , ssc   = sprintf("%10.5f" ,rep(slow.soil.c  ,times=npatches))
                       , psc   = sprintf("%10.5f" ,rep(0.           ,times=npatches))
                       , msn   = sprintf("%10.5f" ,rep(min.soil.n   ,times=npatches))
                       , fsn   = sprintf("%10.5f" ,rep(fast.soil.n  ,times=npatches))
    )#end data.frame
  }
  #---------------------------------------------------------------------------------------#
  # Missing psc

  #----- Write the patch file. -----------------------------------------------------------#
  dummy   = write.table( x         = outpatches
                         , file      = pssfile
                         , append    = FALSE
                         , quote     = FALSE
                         , sep       = " "
                         , row.names = FALSE
                         , col.names = TRUE
  )#end write.table
  #---------------------------------------------------------------------------------------#
  if (IED_INIT_MODE == 3){
    # write site
  }
  #---------------------------------------------------------------------------------------#

  # plot.new()
  # hist(census$pft,xlab="PFT",ylab="Frequency",main='')
  # box()
  #
  # plot.new()
  # boxplot(census$dbh)
  # boxplot(census$height)

  system2("scp",paste(cssfile,"hpc:/data/gent/vo/000/gvo00074/felicien/R/LidarED/data/Wytham/"))
  system2("scp",paste(pssfile,"hpc:/data/gent/vo/000/gvo00074/felicien/R/LidarED/data/Wytham/"))
}
