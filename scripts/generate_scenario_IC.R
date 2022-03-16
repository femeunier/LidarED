rm(list = ls())

library(purrr)
library(ED2scenarios)
library(LidarED)
library(PEcAn.ED2)

ref_dir <- "/scratch/gent/vo/000/gvo00074/hackaton/run_Wytham/ref"
ed2in <- read_ed2in(file.path(ref_dir,"ED2IN"))
ed2in$ITOUTPUT <- 0

rundir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/Wytham/IC/run"
outdir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/Wytham/IC/out"

if(!dir.exists(rundir)) dir.create(rundir)
if(!dir.exists(outdir)) dir.create(outdir)

# add2config <- list(SLA = 36.7*0.48,
#                    Vm0 = 32.85,
#                    growth_resp_factor = 0.45,
#                    storage_turnover_rate = 0.166) # From TRY

add2config_min <- list(growth_resp_factor = 0.45,
                       storage_turnover_rate = 0.166)

defaults <- list()
settings <- list(model = list(revision = "git",
                              config.header = NULL))
PREFIX_XML <- "<?xml version=\"1.0\"?>\n<!DOCTYPE config SYSTEM \"ed.dtd\">\n"
pft_name <- c("temperate.North_Mid_Hardwood","temperate.Late_Hardwood")


list_dir <- list()

#############################################################################################

ICscenario <- c("Census","TLS","NBG")
PFTscenario <- c("onePFT","PFT")
delta_PFT <- c(1,0)

for (iIC in seq(1,length(ICscenario))){
  for (iPFTscenario in seq(1,length(PFTscenario))){

    run.name <- paste0(ICscenario[iIC],"_",PFTscenario[iPFTscenario])
    run_ref <- file.path(rundir,run.name)
    out_ref <- file.path(outdir,run.name)

    if(!dir.exists(run_ref)) dir.create(run_ref)
    if(!dir.exists(out_ref)) dir.create(out_ref)
    if(!dir.exists(file.path(out_ref,"analy"))) dir.create(file.path(out_ref,"analy"))
    if(!dir.exists(file.path(out_ref,"histo"))) dir.create(file.path(out_ref,"histo"))

    Npft <- ifelse(PFTscenario[iPFTscenario] == "onePFT",1,2)
    isNBG <- (ICscenario[iIC] == "NBG")

    # ED2IN
    ed2in_scenar <- ed2in
    ed2in_scenar$IEDCNFGF <- file.path(run_ref,"config.xml")
    ed2in_scenar$FFILOUT <- file.path(out_ref,"analy","analysis")
    ed2in_scenar$SFILOUT <- file.path(out_ref,"histo","history")

    #IC
    if (isNBG){
      ed2in_scenar$IYEARA <- 1912
      ed2in_scenar$IED_INIT_MODE <- 0
    } else {
      ed2in_scenar$SFILIN <- file.path("/data/gent/vo/000/gvo00074/felicien/R/LidarED/data/Wytham",paste0("Wytham_",PFTscenario[iPFTscenario],"_",ICscenario[iIC]))
    }

    ed2in_scenar$INCLUDE_THESE_PFT <- sort(11 - ((1:Npft)-1))

    write_ed2in(ed2in_scenar,filename = file.path(run_ref,"ED2IN"))

    # Config multiple PFTs
    config_TRY <- list()

    for (ipft in seq(1,Npft)){
      config_TRY[[pft_name[delta_PFT[iPFTscenario] + ipft]]] <- unlist(add2config_min)
    }
    xml <- write.config.xml.ED2(defaults = defaults,
                                settings = settings, trait.values = config_TRY)

    XML::saveXML(xml, file = file.path(run_ref,"config.xml"), indent = TRUE,
                 prefix = PREFIX_XML)

    # job.sh
    write_job(file =  file.path(run_ref,"job.sh"),
              nodes = 1,ppn = 18,mem = 16,walltime = 24,
              prerun = "ml UDUNITS/2.2.26-intel-2018a R/3.4.4-intel-2018a-X11-20180131 HDF5/1.10.1-intel-2018a; ulimit -s unlimited",
              CD = run_ref,
              ed_exec = "/user/scratchkyukon/gent/gvo000/gvo00074/felicien/ED2/ED/build/ed_2.1-opt",
              ED2IN = "ED2IN")

    list_dir[[run.name]] = run_ref



  }
}

dumb <- write_bash_submission(file = file.path(rundir,"all_jobs.sh"),
                              list_files = list_dir,
                              job_name = "job.sh")

# scp /home/femeunier/Documents/projects/Hackaton/LidarED/scripts/generate_scenario_IC.R hpc:/data/gent/vo/000/gvo00074/felicien/R

