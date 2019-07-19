#### Instructions and shortcuts. ####

# Can also run this entire publishing script as:
#   source(file="/data/BiCS_RDE_Development/shiny-server_development/pharmacometrics/dynamicReport/publish_latest_version_to_test.R")

# Published app will be available on the BiCS RDE TEST server at the shareable URL below:
#   https://ritabdauva30004.rit.aws.regeneron.com/pharmacometrics/dynamic_report_dev

# Submit a ticket at the Bioinformatics Support Center to request that app access be restricted to only a subset of all Regeneron users:
#   https://regeneron.service-now.com/gsc?id=sc_cat_item&sys_id=73bc69eb4ff032003743d49f0310c709



################################################################################
#"/data/BiCS_RDE_Development/shiny-server_development/pharmacometrics/shinyPMx"
# setwd("/efs/shiny-server_testing/pharmacometrics")
################################################################################


# 0)git config --global user.name "Feng Yang"
# git config --global user.email "feng.yang@regeneron.com"
# 
# 1) I just want to clone this repository
# env GIT_SSL_NO_VERIFY=true   git clone https://feng.yang@ritscm.regeneron.com/scm/~feng.yang/shinypmx.git

# 2) My code is ready to be pushed
# cd /efs/shiny-server_testing/pharmacometrics
# git init
# git add --all
# git commit -m "Initial Commit"
# git remote add origin https://feng.yang@ritscm.regeneron.com/scm/~feng.yang/bla_pd1.git
# env GIT_SSL_NO_VERIFY=true git push -u origin master

# 3) My code is already tracked by Git
# cd /efs/shiny-server_testing/pharmacometrics
# git remote set-url origin https://feng.yang@ritscm.regeneron.com/scm/~feng.yang/bla_pd1.git 
# env GIT_SSL_NO_VERIFY=true git push -u origin master


# mytest  11/30/2017

################################################

#### Load the necessary functions. ####
# Set the working directory. Can only be run from within the DEV server.
setwd("/data/BiCS_RDE_Development/shiny-server_development/pharmacometrics/shinyPMx")
# Run the global file, which will load all of the necessary functions.
source(file="global.R")


#### Publish the app to test. ####
# Publish app repository.
publishToTEST(
    appDirOnTest="/efs/shiny-server_testing/pharmacometrics/shinyPMx",
    #repositoryURL="ssh://git@ritscm.regeneron.com:7999/bics/pharmacometrics-shiny-app----dynamicreport.git",
    repositoryURL="https://feng.yang@ritscm.regeneron.com/scm/~feng.yang/shinypmx.git", 
    owningGroup="pharmacometrics",
    executeCommands=TRUE
) 


