rm(list = ls())

library(LidarED)
library(PEcAn.ED2)


main.dir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/Wytham/growth_storage_resp/run"
simulations <- c("near_bare_ground","near_bare_ground_resp")
year.init <- rep(2007,2)

for (isimu in seq(1,length(simulations))){
  ed2in.file <- file.path(main.dir,"run",simulations[isimu],"ED2IN")
  ed2in.file.dest <- ed2in.file
  ed2in.file <- "/home/carya/ED2/ED/run/ED2IN"
  ed2in.file.dest <- "/home/carya/ED2/ED/run/ED2IN_mod"
  ed2in <- read_ed2in(ed2in.file)
  
  # History run
  ed2in$RUNTYPE <- "HISTORY"
  
  # Activate T-files
  
  ed2in$ITOUTPUT <- 3
  ed2in$SFILIN <- ed2in$SFILOUT
  
  # IC
  ed2in$IDATEH <- 1
  ed2in$IMONTHH <- 1
  ed2in$IYEARH <- year.init[isimu]
  
}