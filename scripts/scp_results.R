rm(list = ls())

library(stringr)

local.dir <- "~/R/LidarED/runs/growth_storage_resp"
remote.dir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/Wytham/growth_storage_resp/run"

file <- file.path(remote.dir,"all_jobs.sh")
system2("scp",c(paste0("hpc:",file),local.dir))

local.file <- file.path(local.dir,"all_jobs.sh")
all_job.file <- readLines(local.file)[seq(2,length(readLines(local.file)),2)]

Ndir <- length(all_job.file)

for (idir in seq_len(Ndir)){#seq_len(Ndir)
  dir_temp <- substr(all_job.file[idir],4,nchar(all_job.file[idir]))
  dir_name <- file.path(local.dir,"out",basename(dir_temp))
  if(!dir.exists(dir_name)) dir.create(dir_name)

  print(dir_name)

  dir_out <- file.path(dirname(dirname(dir_temp)),"out",basename(dir_temp),"analy")

  system2("scp",c(paste0("hpc:",file.path(dir_out,"*.RData")),file.path(dir_name)))
  # system2("scp",c("-r",paste0("hpc:",file.path(dir_out,"Figures")),file.path(dir_name,"Figures")))
}

# rename

system2("mv",c(file.path(local.dir,"out","reference"),file.path(local.dir,"out","reference_config")))
system2("mv",c(file.path(local.dir,"out","reference_resp"),file.path(local.dir,"out","reference")))

system2("mv",c(file.path(local.dir,"out","near_bare_ground"),file.path(local.dir,"out","near_bare_ground_config")))
system2("mv",c(file.path(local.dir,"out","near_bare_ground_resp"),file.path(local.dir,"out","near_bare_ground")))

system2("rm",c("-rf",file.path(local.dir,"out","reference_noconfig")))
system2("rm",c("-rf",file.path(local.dir,"out","near_bare_ground_noconfig")))
