#==========================================================================================#
#==========================================================================================#
#     Leave these commands at the beginning.  They will refresh the session.               #
#------------------------------------------------------------------------------------------#
rm(list=ls())
graphics.off()
#==========================================================================================#
#==========================================================================================#

# Inputs
pecanxmlfile="/home/carya/R/LidarED/data/pecan.xml"        # Paracou without MA ensemble runs

########################################################################################################

# pecanxmlfile="/home/carya/R/inputs/BCI_liana_Kmax_only.xml"

# ----------------------------------------------------------------------
# Load required libraries
# ----------------------------------------------------------------------

library(PEcAn.all)
library(PEcAn.utils)
library(RCurl)
library(purrr)
library(rlist)

# make sure always to call status.end
options(warn=1)

options(error=quote({
  PEcAn.utils::status.end("ERROR")
  PEcAn.remote::kill.tunnel(settings)
  if (!interactive()) {
    q(status = 1)
  }
}))

#options(warning.expression=status.end("ERROR"))

# ----------------------------------------------------------------------
# PEcAn Workflow
# ----------------------------------------------------------------------
# Open and read in settings file for PEcAn run.
args <- commandArgs(trailingOnly = TRUE)
if (is.na(args[1])){
  settings <- PEcAn.settings::read.settings(pecanxmlfile)
} else {
  settings.file = args[1]
  settings <- PEcAn.settings::read.settings(settings.file)
}

# Update/fix/check settings. Will only run the first time it's called, unless force=TRUE
settings <- PEcAn.settings::prepare.settings(settings, force=FALSE)

# Write pecan.CHECKED.xml
PEcAn.settings::write.settings(settings, outputfile = "pecan.CHECKED.xml")

settings <- PEcAn.workflow::do_conversions(settings,overwrite.met = TRUE)

# Query the trait database for data and priors
if (PEcAn.utils::status.check("TRAIT") == 0){
  PEcAn.utils::status.start("TRAIT")
  settings <- PEcAn.workflow::runModule.get.trait.data(settings)
  PEcAn.settings::write.settings(settings, outputfile='pecan.TRAIT.xml')
  PEcAn.utils::status.end()
} else if (file.exists(file.path(settings$outdir, 'pecan.TRAIT.xml'))) {
  settings <- PEcAn.settings::read.settings(file.path(settings$outdir, 'pecan.TRAIT.xml'))
}

# Run the PEcAn meta.analysis
if(!is.null(settings$meta.analysis)) {
  if (PEcAn.utils::status.check("META") == 0){
    PEcAn.utils::status.start("META")
    PEcAn.MA::runModule.run.meta.analysis(settings)
    PEcAn.utils::status.end()
  }
}


# Write model specific configs
if (PEcAn.utils::status.check("CONFIG") == 0){
  PEcAn.utils::status.start("CONFIG")
  settings <- PEcAn.workflow::runModule.run.write.configs(settings)
  PEcAn.settings::write.settings(settings, outputfile='pecan.CONFIGS.xml')
  PEcAn.utils::status.end()
} else if (file.exists(file.path(settings$outdir, 'pecan.CONFIGS.xml'))) {
  settings <- PEcAn.settings::read.settings(file.path(settings$outdir, 'pecan.CONFIGS.xml'))
}


