rm(list = ls())

for (i in seq(13047183,13047372)){
  system2("qdel",i)
}
