

# -----------------------------------------------
setwd("~/handbook/")
source("~/handbook/global.R")
# -----------------------------------------------
  
dataFile_name <- "~/handbook/data/nmdatPKPD.csv"
nmdat <- read_datafile(inFile=dataFile_name) 
 
FIGURE = NULL
TABLE = NULL

#-------------------------------------------------
tt = nmdat %>% 
  filter(TESTCAT == "RESP1") %>% select(-DVOR) %>% 
  descriptive_time_profile_mean(params=NULL)
FIGURE = c(FIGURE, tt$figure)
TABLE = c(TABLE, tt$table)

tt = nmdat %>% 
  filter(TESTCAT == "PK") %>% 
  descriptive_time_profile_indiv(params=NULL)
FIGURE = c(FIGURE, tt$figure)
TABLE = c(TABLE, tt$table)

tt = nmdat %>% 
  mutate(TESTCAT=ifelse(TESTCAT=="RESP1", "RESP", TESTCAT)) %>% 
  descriptive_PKPD_sigmoid(params=NULL)
FIGURE = c(FIGURE, tt$figure)
TABLE = c(TABLE, tt$table)

tt = nmdat %>% 
  mutate(TESTCAT=ifelse(TESTCAT=="RESP1", "RESP", TESTCAT)) %>% 
  descriptive_PKPD_hysterisis(params=NULL)
FIGURE = c(FIGURE, tt$figure)
TABLE = c(TABLE, tt$table)




