rm(list = ls())

library(purrr)
library(ED2scenarios)
library(LidarED)
library(PEcAn.ED2)

ref_dir <- "/home/carya/R/ED2scenarios/reference"
ed2in <- read_ed2in(file.path(ref_dir,"ED2IN"))

rundir <- "/home/carya/R/LidarED/runs/run"
outdir <- "/home/carya/R/LidarED/runs/out"

if(!dir.exists(rundir)) dir.create(rundir)
if(!dir.exists(outdir)) dir.create(outdir)

all_configs <- readRDS(file = './data/all_configs.RDS')
all_scenarios <- c("Hmax","Hmean","CA","AGB","Bl")
add2config <- list(SLA = 36.7*0.48,
                   Vm0 = 32.85) # From TRY

x2 <- combn(1:5,2)
x3 <- combn(1:5,3)
x4 <- combn(1:5,4)
x5 <- combn(1:5,5)

temp_combn <- c(as.list(combn(1:5,1)),
                lapply(seq_len(ncol(x2)), function(i) x2[,i]),
                lapply(seq_len(ncol(x3)), function(i) x3[,i]),
                lapply(seq_len(ncol(x4)), function(i) x4[,i]),
                lapply(seq_len(ncol(x5)), function(i) x5[,i]))
cmbs <-
  lapply(seq_len(length(temp_combn)),function(i){
  if (all(c(1,2) %in% temp_combn[[i]])){
    out <- NULL
  } else {
    out <-temp_combn[[i]]
  }
  return(out)
})

cmbs[sapply(cmbs, is.null)] <- NULL

defaults <- list()
settings <- list(model = list(revision = "git",
                              config.header = NULL))
PREFIX_XML <- "<?xml version=\"1.0\"?>\n<!DOCTYPE config SYSTEM \"ed.dtd\">\n"
pft_name <- "temperate.North_Mid_Hardwood"


list_dir <- list()

#############################################################################################
# Reference

run_ref <- file.path(rundir,"reference")
out_ref <- file.path(outdir,"reference")

if(!dir.exists(run_ref)) dir.create(run_ref)
if(!dir.exists(out_ref)) dir.create(out_ref)

# ED2IN
ed2in_scenar <- ed2in
ed2in_scenar$IEDCNFGF <- file.path(run_ref,"")
ed2in_scenar$FFILOUT = file.path(out_ref,"analy","analysis")
ed2in_scenar$SFILOUT = file.path(out_ref,"histo","history")

write_ed2in(ed2in_scenar,filename = file.path(run_ref,"ED2IN"))

# Config
config_TRY <- list()
config_TRY[[pft_name]] <- unlist(add2config)
xml <- write.config.xml.ED2(defaults = defaults,
                            settings = settings, trait.values = config_TRY)

XML::saveXML(xml, file = file.path(run_ref,"config.xml"), indent = TRUE,
             prefix = PREFIX_XML)

# job.sh
write_job(file =  file.path(run_ref,"job.sh"),
          nodes = 1,ppn = 18,mem = 16,walltime = 24,
          prerun = "ml UDUNITS/2.2.26-intel-2018a R/3.4.4-intel-2018a-X11-20180131 HDF5/1.10.1-intel-2018a; ulimit -s unlimited",
          CD = run_ref,
          ed_exec = "/user/scratchkyukon/gent/gvo000/gvo00074/felicien/ED2/ED/run/ed_2.1-opt",
          ED2IN = "ED2IN")

list_dir[["reference"]] = run_ref

###############################################################################################
# Near-bare ground

run_ref2 <- file.path(rundir,"near_bare_ground")
out_ref2 <- file.path(outdir,"near_bare_ground")

if(!dir.exists(run_ref2)) dir.create(run_ref2)
if(!dir.exists(out_ref2)) dir.create(out_ref2)

# ED2IN
ed2in_scenar <- ed2in
ed2in_scenar$IED_INIT_MODE <- 0
ed2in_scenar$IEDCNFGF <- file.path(run_ref2,"")
ed2in_scenar$FFILOUT = file.path(out_ref2,"analy","analysis")
ed2in_scenar$SFILOUT = file.path(out_ref2,"histo","history")

write_ed2in(ed2in_scenar,filename = file.path(run_ref2,"ED2IN"))

# Config
XML::saveXML(xml, file = file.path(run_ref2,"config.xml"), indent = TRUE,
             prefix = PREFIX_XML)

# job.sh
write_job(file =  file.path(run_ref2,"job.sh"),
          nodes = 1,ppn = 18,mem = 16,walltime = 24,
          prerun = "ml UDUNITS/2.2.26-intel-2018a R/3.4.4-intel-2018a-X11-20180131 HDF5/1.10.1-intel-2018a; ulimit -s unlimited",
          CD = run_ref2,
          ed_exec = "/user/scratchkyukon/gent/gvo000/gvo00074/felicien/ED2/ED/run/ed_2.1-opt",
          ED2IN = "ED2IN")

list_dir[["near_bare_ground"]] = run_ref2

####################################################################################################################
# All combinations

for (i in seq_len(length(cmbs))){

  names_scenar <- paste(unlist(map(seq_len(length(cmbs[[i]])),function(j){all_scenarios[cmbs[[i]][j]]})),collapse = '_')
  run_scenar <- file.path(rundir,names_scenar)
  out_scenar <- file.path(outdir,names_scenar)

  if(!dir.exists(run_scenar)) dir.create(run_scenar)
  if(!dir.exists(out_scenar)) dir.create(out_scenar)

  # config file
  trait_values <- list()
  trait_values[[pft_name]] <- unlist(all_configs[cmbs[[i]]],use.names = FALSE)
  names(trait_values[[pft_name]]) <- unlist(map(seq_len(length(cmbs[[i]])),function(j){names(all_configs[[cmbs[[i]][j]]])}))
  trait_values[[pft_name]] <- c(trait_values[[pft_name]],unlist(add2config))

  xml <- write.config.xml.ED2(defaults = defaults,
                              settings = settings, trait.values = trait_values)

  XML::saveXML(xml, file = file.path(run_scenar,"config.xml"), indent = TRUE,
               prefix = PREFIX_XML)

  # ED2IN
  ed2in_scenar <- ed2in
  ed2in_scenar$IEDCNFGF <- file.path(run_scenar,"config.xml")
  ed2in_scenar$FFILOUT = file.path(out_scenar,"analy","analysis")
  ed2in_scenar$SFILOUT = file.path(out_scenar,"histo","history")

  write_ed2in(ed2in_scenar,filename = file.path(run_scenar,"ED2IN"))

  # job.sh
  write_job(file =  file.path(run_scenar,"job.sh"),
            nodes = 1,ppn = 18,mem = 16,walltime = 24,
            prerun = "ml UDUNITS/2.2.26-intel-2018a R/3.4.4-intel-2018a-X11-20180131 HDF5/1.10.1-intel-2018a; ulimit -s unlimited",
            CD = run_scenar,
            ed_exec = "/user/scratchkyukon/gent/gvo000/gvo00074/felicien/ED2/ED/run/ed_2.1-opt",
            ED2IN = "ED2IN")

  list_dir[[names_scenar]]=run_scenar

}

dumb <- write_bash_submission(file = file.path(rundir,"all_jobs.sh"),
                              list_files = list_dir,
                              job_name = "job.sh")
