rm(list = ls())

local.dir <- "~/data/Wytham/Fluxes/"
remote.dir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/Wytham/run"

file <- file.path(remote.dir,"all_jobs.sh")
system2("scp",c(paste0("hpc:",file),local.dir))

local.file <- file.path(local.dir,"all_jobs.sh")
all_job.file <- readLines(local.file)[seq(2,length(readLines(local.file)),2)]

Ndir <- length(all_job.file)

for (idir in seq(8,Ndir)){

  dir_temp <- substr(all_job.file[idir],4,nchar(all_job.file[idir]))
  dir_name <- file.path(local.dir,basename(dir_temp))

  print(dir_name)

  if(!dir.exists(dir_name)) dir.create(dir_name)
  dir_out <- file.path(dirname(dirname(dir_temp)),"out",basename(dir_temp),"analy")

  system2("scp",c(paste0("hpc:",file.path(dir_out,"*T*")),file.path(dir_name)))

}
