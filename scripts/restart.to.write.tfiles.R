rm(list = ls())

library(LidarED)
library(PEcAn.ED2)
library(ED2scenarios)


main.dir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/Wytham/growth_storage_resp"
simulations <- c("near_bare_ground","near_bare_ground_resp")
year.init <- rep(2007,2)

for (isimu in seq(1,length(simulations))){
  ed2in.file <- file.path(main.dir,"run",simulations[isimu],"ED2IN")
  ed2in.file.dest <- ed2in.file
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

  write_job_noR(file =  file.path(run_scenar,"job.sh"),
                nodes = 1,ppn = 18,mem = 16,walltime = 24,
                prerun = "ml UDUNITS/2.2.26-intel-2018a R/3.4.4-intel-2018a-X11-20180131 HDF5/1.10.1-intel-2018a; ulimit -s unlimited",
                CD = run_scenar,
                ed_exec = "/user/scratchkyukon/gent/gvo000/gvo00074/felicien/ED2/ED/build/ed_2.1-opt",
                ED2IN = "ED2IN")

  setwd(file.path(main.dir,"run"))

  system2("bash","job.sh")
}
