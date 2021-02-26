rm(list = ls())

Nruns <- 100
Treatments <-3
init = c(99000015531,99000016031,99000018034)

mainDir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/Wytham/growth_storage_resp/out"

for (i in seq(1,Treatments)){
  print(i/Treatments)
  for (irun in seq(1,Nruns)){
    directory <- file.path(mainDir,init[i]-1+irun)

    if(!dir.exists(directory)){
      system2("mkdir",directory)
    }
  }
}
