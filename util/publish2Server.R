
########################################################################################
# setwd("/data/BiCS_RDE_Development/shiny-server_development/pharmacometrics/shinyPMx")
# setwd("/efs/shiny-server_testing/pharmacometrics")
# https://confluence.regeneron.com/x/QLZaAg             # how to setup version control
########################################################################################

# 0) create a project on bitbucket, and get url by click "clone", must be in "ssh://......"
# 1) in Rstudio, create a project using version control with the url in step 0). 
# 2) run in shell "cat ~/.ssh/id_rsa.pub"  to get the key
# 3) add the key into the bitbucket
# 4) done

# # Check current origin URL. Initially should look like https://feng.yang@ritscm.regeneron.com/scm/~feng.yang/shinypmx.git/
# git remote -v
# 
# # Remove current origin.
# git remote rm origin
# 
# # Check that current origin URL has been removed.
# git remote -v
# 
# # Add new origin URL.
# git remote add origin ssh://git@ritscm.regeneron.com:7999/~feng.yang/shinypmx.git/

# git remote add origin  ssh://git@ritscm.regeneron.com:7999/phar/shinypmx.git
# git remote set-url origin ssh://git@ritscm.regeneron.com:7999/phar/shinypmx.git


#   
# # Check that current origin URL has been updated.
# git remote -v

# cat ~/.ssh/id_rsa.pub
 
 
#### Instructions and shortcuts. ####

# Can also run this entire publishing script as:
#   source(file="/data/BiCS_RDE_Development/shiny-server_development/pharmacometrics/dynamicReport/publish2Server.R")

# Published app will be available on the BiCS RDE TEST server at the shareable URL below:
#   https:// xxx/pharmacometrics/dynamic_report_dev

# Submit a ticket at the Bioinformatics Support Center to request that app access be restricted to only a subset of all Regeneron users:
#   https://regeneron.service-now.com/gsc?id=sc_cat_item&sys_id=73bc69eb4ff032003743d49f0310c709



############################################ 
#### Load the necessary functions. ####
############################################
# Set the working directory. Can only be run from within the DEV server.
#setwd("/data/BiCS_RDE_Development/shiny-server_development/pharmacometrics/shinyPMx")
# Run the global file, which will load all of the necessary functions.
#source(file="global.R")
 
if (1==2) { 

globalConfigurationDirectory <- "/data/BiCS_RDE_Development/shiny-server_development/global_configuration/"
source(file=paste(globalConfigurationDirectory,"load_server_configuration_objects.R",sep=""),local=TRUE)

#### Publish the app to test. ####
# Publish shinyPMx repository.
publishToTEST(
  appDirOnTest="/efs/shiny-server_testing/pharmacometrics/regnR",
  #repositoryURL="ssh://git@ritscm.regeneron.com:7999/phar/handbook.git", 
  repositoryURL="ssh://git@ritscm.regeneron.com/~feng.yang/regnR.git", 
  owningGroup="pharmacometrics",
  executeCommands=TRUE
) 
 
# Publish data repository.
publishToTEST(
  appDirOnTest="/efs/data_and_analysis_testing/pharmacometrics/pharmacometrics_data",
  repositoryURL="ssh://git@ritscm.regeneron.com:7999/bics/pharmacometrics_data.git",
  owningGroup="pharmacometrics",
  executeCommands=TRUE
)

}