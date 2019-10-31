#==========================================================================================#
#==========================================================================================#
#       This code creates a PSS/CSS file for a tree survey conducted at the Tapajos        #
# National Forest near Santarem in Brazil, in 2000.  The original data set can be found at #
#                                                                                          #
# Menton, M., M. Figueira, C.A.D. de Sousa, S.D. Miller, H.R. da Rocha, and M.L. Goulden.  #
#     2011. LBA-ECO CD-04 Biomass Survey, km 83 Tower Site, Tapajos National Forest,       #
#     Brazil. Data set. Available on-line [http://daac.ornl.gov] from Oak Ridge National   #
#     Laboratory Distributed Active Archive Center, Oak Ridge, Tennessee, U.S.A.           #
#     doi:10.3334/ORNLDAAC/990                                                             #
#                                                                                          #
#     Do not distribute this data.  Also, anyone intending to use this data other than for #
# this training MUST comply with LBA data policy. Please visit the ORNL site to learn      #
# more:  http://dx.doi.org/10.3334/ORNLDAAC/990                                            #
#------------------------------------------------------------------------------------------#

#----- Leave this as the first command, this will reset your R session. -------------------#
rm(list=ls())
options(warn=0)
gc()
graphics.off()
#------------------------------------------------------------------------------------------#

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


#------------------------------------------------------------------------------------------#
#     Set paths.                                                                           #
#------------------------------------------------------------------------------------------#
here    =  "/home/femeunier/Documents/R/ED2_Support_Files/pss+css_processing"              # Current path
there   = '/home/femeunier/Documents/data/paracou/' # Census path
outpath = file.path(here,"sites") # Path where the output should be written
#------------------------------------------------------------------------------------------#

setwd(here)

#------------------------------------------------------------------------------------------#
#     Location of interest.                                                                #
#------------------------------------------------------------------------------------------#
place      = "paracou"   # Name of the site, for output directory
census.csv = "census_Paracou_all_extrapolated.csv"  # Name of the csv file with the forest inventory data
iata       = "paracou"             # Tag for site (short name for output files)
identity   = "PFT_mid"         # Unique identification (in case you have multiple tests).
lon        = -53           # Longitude of the site
lat        =  5           # Latitude of the site
year.out   = 2000              # Year of measurements
#------------------------------------------------------------------------------------------#



#------------------------------------------------------------------------------------------#
#     Plot information.                                                                    #
#------------------------------------------------------------------------------------------#
subplot.area    = 70*70   # Area of each subplot (all trees) [m2]
allplot.area    = 70*70   # Area of each plot    (all large trees) [m2]
nplots          = 10     # Number of plots
min.dbh.subplot = 10.     # Minimum DBH in the sub-sample
min.dbh.allplot = 10.     # Minimum DBH for all plot
min.dbh.subplot_L = 1.   # Liana minimum DBH in the sub-sample
min.dbh.allplot_L = 1.   # Liana minimum DBH in the sub-sample
consider_liana = TRUE
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
source(file.path(here,"allometry.r"))
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
#pft.brks    = c(-Inf,0.5*(pft.mid.rho[-1]+pft.mid.rho[-npft]),Inf)
#pft.brks    = c(-Inf,(pft.mid.rho[-npft]),Inf)
pft.brks    = c(-Inf,c(0),Inf)
pft.cut     = as.numeric(cut(census$wood.dens,pft.brks))
census$pft  = pft.idx[pft.cut]

if ("is_liana" %in% colnames(census)){
  if (consider_liana){
    census$pft[census$is_liana]=17
    census$keep <- TRUE
    plots_uni <- unique(census$plots)
    nplots_tot <- length(plots_uni)
    for (i in seq(nplots_tot)){
      if (all(census$pft[census$plots==plots_uni[i]] == 17)){
        census$keep[census$plots==plots_uni[i]] <- FALSE
      }
    }
  } else {
    census$keep <- TRUE
    census$keep[census$is_liana]=FALSE
    plots_uni <- unique(census$plots)
    nplots_tot <- length(plots_uni)
    for (i in seq(nplots_tot)){
      if (all(census$pft[census$plots==plots_uni[i]] == 17)){
        census$keep[census$plots==plots_uni[i]] <- FALSE
      }
    }

  }
  census <- census[census$keep,]
  ncohorts <- nrow(census)
}

#------------------------------------------------------------------------------------------#


#------------------------------------------------------------------------------------------#
#     Find the demographic density of the individuals.                                     #
#------------------------------------------------------------------------------------------#
census$n = with(census, ifelse(dbh < min.dbh.allplot,1/subplot.area,1/allplot.area))

if ("is_liana" %in% colnames(census)){
  census$n[census$is_liana] =  with(census, ifelse(dbh[census$is_liana] < min.dbh.allplot_L,1/subplot.area,1/allplot.area))
}
#------------------------------------------------------------------------------------------#



#------------------------------------------------------------------------------------------#
#     Estimate biomass and leaf area index.                                                #
#------------------------------------------------------------------------------------------#
census$height = with(census,dbh2h   (dbh=dbh,ipft=pft))
census$balive = with(census,size2ba (dbh=dbh,hgt=height,ipft=pft))
census$bdead  = with(census,size2bd (dbh=dbh,hgt=height,ipft=pft))
census$lai    = with(census,size2lai(dbh=dbh,hgt=height,nplant=n,ipft=pft))
#------------------------------------------------------------------------------------------#

if ("is_liana" %in% colnames(census)){
  allom=iallom
  source('~/Documents/R/review_paper/allometry/allom_ed.r')
  source('~/Documents/R/review_paper/allometry/allom_ed_param.r')
  is_liana=census$is_liana


  census$height[is_liana] = sapply(census$dbh[is_liana],function(dbh) dbh2h(dbh,pft,allom,17))

  if (consider_liana){
    plot_uni=sort(unique(census$plots))
    dbh_ths_L=2. # cm
    delta_z_L=0.2 # m
    for (i in seq(1,length(plot_uni))){
      pos = census$plots == plot_uni[i]

      if (any(pos & !census$is_liana)){
        tree_max_height = max(census$height[pos & !census$is_liana]) # Tree maximal height in this patch
        census$height[census$is_liana & pos & census$dbh > dbh_ths_L] = min(tree_max_height+delta_z_L,pft$hgt.max[17])
      }
    }
  }
}





#------------------------------------------------------------------------------------------#
#     Organise the dat using the tags, blocks, and coordinates.                            #
#------------------------------------------------------------------------------------------#
o             = order(census$plots,-census$dbh,census$tag)
census        = census[o,,drop=FALSE]
#------------------------------------------------------------------------------------------#



#------------------------------------------------------------------------------------------#
#    File names for output.                                                                #
#------------------------------------------------------------------------------------------#
outprefix = paste0(iata,"_",identity,".lat",sprintf("%.3f",lat),"lon",sprintf("%.3f",lon))
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

   plot.new()
   hist(census$pft,xlab="PFT",ylab="Frequency",main='')
   box()

   plot.new()
   hist(census$wood.dens,xlab=TeX("$Wood \\, density \\, \\[g.cm^{-3}\\]$"),ylab="Frequency",main='')
   box()
   abline(v=pft.brks,lty=2,lwd=2,col='red')
   abline(v=c(0.53,0.71),lty=2,lwd=2,col='black')
   arrows(c(0.53,0.71),c(80000,80000),c(0.45,0.6),c(80000,80000),lwd=2,lty=1,length=0.1)


   delta_dbh=2
   dbhs <- c(seq(1,28,delta_dbh),Inf)
   density <- rep(0,length(dbhs)-1)

   for (idbh in seq(1,length(dbhs)-1)){
     sel = (census$pft ==17)  & (census$dbh>=dbhs[idbh]) & (census$dbh<dbhs[idbh+1])
     density[idbh] = sum(census$n[sel]/nplots)
   }


   # for (ipft in c(2,3,4)){
   #  census$height[census$pft==ipft] = sapply(census$dbh[census$pft==ipft],function(dbh) dbh2h(dbh,pft,allom,ipft))
   # }
   # Npatches=npatches
   # maxH_L=maxH_T=minH_L=minH_T=matrix(NA,Npatches)
   # for (i in seq(1,Npatches)){
   #   h=census$height[census$plots==i]
   #   pft=census$pft[census$plots==i]
   #   if (any(pft==17)){
   #     maxH_L[i]=max(h[pft==17])
   #     minH_L[i]=min(h[pft==17])
   #   }
   #   if (any(pft!=17)){
   #     maxH_T[i]=max(h[pft!=17])
   #     minH_T[i]=min(h[pft!=17])
   #   }
   # }
   #
   # plot.new()
   # par(mar=c(2,2,2,2),mfrow=c(1,2))
   # plot(seq(1,Npatches),maxH_T,type='p',col='green')
   # lines(seq(1,Npatches),maxH_L,type='p',col='blue')
   #
   # plot(seq(1,Npatches),minH_L,type='p',col='blue',ylim=c(0,1.1*max(minH_L)))
   # lines(seq(1,Npatches),minH_T,type='p',col='green')
   #

   #plot(census$dbh[census$pft==17],census$height[census$pft==17]) #,xlim=c(2,10))
