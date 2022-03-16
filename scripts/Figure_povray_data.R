rm(list = ls())
library(dplyr)

#----- Paths. -----------------------------------------------------------------------------#
here           = "/home/femeunier/Documents/ED2/R-utils"   # Current directory.
there          = "/home/femeunier/Documents/projects/Hackaton/LidarED/outputs/"    # Directory where analyses/history are
srcdir         = "/home/femeunier/Documents/ED2/R-utils" # Source  directory.
outroot        = "/home/femeunier/Documents/projects/Hackaton/LidarED/Figures/allom/" # Directory for figures
#------------------------------------------------------------------------------------------#

place       = c("WythamPFT")
outmain = paste(outroot,place,sep="/")
outpref = paste(outmain,"povray",sep="/")

inpref  = file.path(there,place)
bnpref  = basename(inpref)
pcout = ""

size = list(height = 6.8,
            width = 8.8,
            ratio = 1.294118,
            paper = "special")
depth = 300

povscript = file.path(here,place,"polygon.pov")
dummy     = file.copy(from="/home/femeunier/Documents/projects/Hackaton/LidarED/Figures/povray_template.pov",
                      to=povscript)
#------------------------------------------------------------------------------------#


pigment = "       pigment  { color rgb <0. ,0. ,0. >}   "

lieu    = place
lesim    = unlist(strsplit(lieu,split="\n"))
povtitle = NULL
for (n in sequence(length(lesim))){
  yt       = sprintf("%7.1f",200 - 20 * (n-1))
  povtitle = rbind( povtitle
                    ,       "//----- The header. --------------------------------//"
                    , paste("text { ttf \"cyrvetic.ttf\" \"","","\" 5,0",sep="")
                    ,               pigment
                    ,       "       finish{ ambient 1.0 diffuse 0.0}"
                    ,       "       scale     <   12.0,   12.0,    0.1>"
                    ,       "       rotate    <   28.8,    0.0,    0.0>"
                    ,       "       rotate    <    0.0,   45.0,    0.0>"
                    , paste("       translate < -150.0,", yt,",  200.0>",sep="")
                    ,       "     }// end text"
                    ,       "//--------------------------------------------------//"
                    ,       " "
                    ,       " "
  )#end rbind
}#end for (n in 1:lesim)
povstamp = rbind(       "//----- The time stamp. ----------------------------//"
                        , paste("text { ttf \"cyrvetic.ttf\" \"","","\" 5,0",sep="")
                        ,        pigment
                        ,       "       finish{ ambient 1.0 diffuse 0.0}"
                        ,       "       scale     <   12.0,   12.0,    0.1>"
                        ,       "       rotate    <   28.8,    0.0,    0.0>"
                        ,       "       rotate    <    0.0,   45.0,    0.0>"
                        ,       "       translate <  100.0,  180.0, -150.0>"
                        ,       "     }// end text"
                        ,       "//--------------------------------------------------//"
                        ,       " "
                        ,       " "
)#end rbind
#------------------------------------------------------------------------------------#
########################################################################################
# Data

# Load data
data.file <- "/home/femeunier/Documents/projects/Hackaton/LidarED/data/Wytham_trees_summary_ED2_nodead_add.csv"
data.wytham <- read.csv(data.file,header = TRUE) %>% mutate(TLS_ID = as.character(TLS_ID))

data.file2 <- "/home/femeunier/Documents/projects/Hackaton/LidarED//data/Wytham_trees_summary_ED2.csv"
data2.wytham <- read.csv(data.file2,header = TRUE) %>% dplyr::select(TLS_ID,VerticalCrownProjectedArea_pts_.m2.,species) %>% mutate(TLS_ID = as.character(TLS_ID),
                                                                                                                                    species = as.character(species))
data.wytham <- data.wytham %>% left_join(data2.wytham,by = "TLS_ID") %>%
  rename(x =  stemlocx_.m._x,
         y =  stemlocy_.m._x,
         dbh_tls = DBH_TLS_.m._x,
         h = Hgt_pts_.m._x,
         AGV_m = Vol_QSM_avg_.m3._x,
         dbh_census = DBH_census_.m._x,
         CA = VerticalCrownProjectedArea_pts_.m2.,
         LA = leaf_area) %>% mutate(dbh_tls = 100*dbh_tls,
                                    dbh_census = 100*dbh_census) %>% mutate(PFT = case_when(species %in% c("ACERPS") ~ 1,
                                                                                            TRUE ~ 0),
                                                                            PFT.name = case_when(species %in% c("ACERPS") ~ "LH",
                                                                                                 TRUE ~ "MH")) %>%
  mutate(dbh_census = case_when(is.na(dbh_census) ~ dbh_tls,
                                TRUE ~ dbh_census)) %>% filter(species != "") %>%
  filter(!is.na(dbh_tls))

dbhco <- data.wytham$dbh_tls
pftco <- data.wytham$PFT+ 10
xco <- data.wytham$x - 70
yco <- data.wytham$y - 150
h_ref <- -3.2;h1 <- 26.2;h2 <- - 0.074
hiteco <- h_ref + h1*(1 - exp(data.wytham$dbh_tls*h2))

#########################################################################################

povplant = rbind(       "//----- The plants. ---------------------------------//"
                        , rbind( paste("plant(", unlist( mapply( FUN = paste
                                                                 , sprintf("%7.2f",dbhco  )
                                                                 , sprintf("%2i"  ,pftco  )
                                                                 , sprintf("%7.2f",xco    )
                                                                 , sprintf("%7.2f",yco    )
                                                                 , sprintf("%7.2f",hiteco )
                                                                 , MoreArgs = list(sep=",")
                        )#end mapply
                        )#end unlist
                        ,")"
                        ,sep=""
                        )#end paste
                        )#rbind
                        ,       "//---------------------------------------------------//"
                        )#end rbind


#------------------------------------------------------------------------------------#
#      Append the title, stamp, and plants...                                        #
#------------------------------------------------------------------------------------#
write(x = povtitle, file = povscript, ncolumns = 1, append = TRUE)
write(x = povstamp, file = povscript, ncolumns = 1, append = TRUE)
write(x = povplant, file = povscript, ncolumns = 1, append = TRUE)
#------------------------------------------------------------------------------------#

#------------------------------------------------------------------------------------#
#     Call POV-Ray (must use system, though...)                                      #
#------------------------------------------------------------------------------------#
povray  = Sys.which("povray")
povray = "/usr/bin/povray"

outtemp = file.path(tempdir(),paste(place,".png",sep=""))
outfile = file.path(outpref,paste(bnpref,"-",pcout,".png",sep=""))

povopts = paste("-D"
                ,"-V"
                ,"+UA"
                ,paste("+W",round(size$width*depth ),sep="")
                ,paste("+H",round(size$height*depth),sep="")
                ,paste("+O",outtemp,sep="")
                ,sep = " "
)#end paste
dummy   = system( command       = paste(povray,povopts,povscript,sep=" ")
                  , intern        = TRUE
                  , ignore.stdout = TRUE
                  , ignore.stderr = TRUE
)#end system
dummy   = file.copy(from=outtemp,to=outfile,overwrite=TRUE)
dummy   = file.remove(outtemp,povscript)
