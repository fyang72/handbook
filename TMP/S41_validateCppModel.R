

##########################################################################
# Need 
# 1) model file
# 2) adpx:  nonmem-ready dataset; 
# 3) xpdb:  nonmem output results with individual parameters
##########################################################################


# load mrgsolve cpp model
#----------------------------------------
library(dplyr)
library("mrgsolve") 

source("./cpp/mod_R3918_human_MM_final.cpp")

mod <- mread("R2810", tempdir(),mymodel)     # ?mread , ?mcode
 

# adpx = read_csv("./PKreport/data/nmdat_PK_CH50_cohort5_25mg_IV_removeOutliers.csv") %>% filter(TESTN %in% c(0, 1))
# patab = read.table("./PKreport/ctl/PKPD_MM002/patab001", skip=1, header=TRUE) %>% distinct(ID, .keep_all=TRUE)

#adpx = read_csv("./data/nmdat_1024_2018.csv") #%>% filter(TEST =="REGN3918")
adpx = read_csv("./data/nmdat_PKPD_1024_2018.csv") 
adex = adpx %>%  filter(EVID==1) 

tdata = adex %>% 
  mutate(AMT = EXDOSE) %>% 
  select(USUBJID, ID, ARMA, TIME, AMT, RATE, EVID, CMT, MDV, TESTN, CH50HBL, C5BL, WGTBL) %>% 
  mutate(ARMA = gsub("-", " ", ARMA, fixed=TRUE))

# add individual parameters
tdata = tdata %>% left_join(xpdb %>% 
                              select(ID, CL,  V2, Q, V3, F1, KA, VMAX,  KSS, EC50, GAMMA, EMAX) %>%  # no need of WGT_ON_CLQ WGT_ON_VSS
                              distinct(ID, .keep_all=TRUE), 
                            by="ID")  

# assign individual parameters as poulation parameters
tdata = tdata %>% mutate(
              MDV = 1, 
              TESTN=1, 
              ID = as.integer(ID), 
              TIME = as_numeric(TIME), 
              AMT = as_numeric(AMT),  ################
              RATE = as_numeric(RATE), 
              EVID = as.integer(EVID), 
              CMT = as.integer(CMT), 
              MDV = as.integer(MDV), 
              CH50HBL = as_numeric(CH50HBL), 
              C5BL = as_numeric(C5BL)
)  %>% rename(TVCL = CL, 
              TVV2 = V2, 
              TVQ = Q, 
              TVV3 = V3, 
              TVF1 = F1, 
              TVKA = KA, 
              TVVMAX = VMAX, 
              TVKSS = KSS, 
              TVEC50 = EC50, 
              TVGAMMA = GAMMA, 
              TVEMAX = EMAX)


seed = 1234
# set seed to make reproducible simulation (if IIV=1)
set.seed(seed)

infhr.lst = 1
delta = 0.1
followUpPeriod = 77  #days

# library("PKPDmisc")
treat_end = 35 #l24*7    
sim_end = treat_end + followUpPeriod   # default 112 days                      # note dose by week only
tgrid = sim_timept(start=0, end=treat_end, delta=delta, dtime=seq(0,treat_end, by=7), infhr.lst=infhr.lst)

#---------------------------------------- 
# run simulation
#---------------------------------------- 
tdata = tdata %>% filter(as.integer(EVID)==1)    #%>%mutate(WGTBL = 70)
out = mod  %>% zero_re %>% param( COVARIATE=0, SCALING=0) %>%     # change parameters
   data_set(tdata) %>%   
   mrgsim(end=sim_end, delta=delta,  tad=TRUE) %>%  as.data.frame() %>% capitalize_names()# add=tgrid, , carry.out=c("ii", "evid")


# only add ID, STUDYID, USUBJID, ARMA
out = out %>% left_join(adpx %>% as.data.frame() %>% 
                          distinct(ID, .keep_all=TRUE) %>% 
                          select(ID,  USUBJID, ARMA),
                        by=c("ID"))  

# nominal time (NTIM)
out = out %>% mutate(NTIM = TIME)

# fill up II by  Last Observation Carried Forward
# na.locf https://stackoverflow.com/questions/48392015/r-carry-forward-last-observation-n-times-by-group?rq=1
#out = out %>% mutate(II=ifelse(II==0, NA, II))  %>% group_by(ID) %>% fill(II)


# EVID, DVOR and EXSEQ    
# -----------------------------------
simData = out #%>% mutate(EVID=ifelse(TAD==0, 1, 0))   %>% slice(2:n()) %>%
#mutate(DVOR = IPRED) %>% 
#group_by(ID)%>%  mutate(EXSEQ=cumsum(EVID)) %>%   # for calculat the last dose interval later
#as.data.frame()



#---------------------------------------- 
# priminary PK plot     
# -----------------------------------
#adpx = read_csv("./PKreport/data/nmdat_PK_CH50.csv") %>%  
rawData = adpx %>% mutate(DVOR =as_numeric(DVOR), 
         ARMA = gsub("_", " ", ARMA, fix=TRUE)) %>% filter(ARMA == "3 mg/kg IV")

tdata = simData %>% 
           mutate(ARMA = gsub("_", " ", ARMA, fix=TRUE),
                  IPRED = CENTRAL/V2) %>% 
           filter(ARMA == "3 mg/kg IV")

fig = ggplot(data=tdata, aes(x=TIME, y=IPRED, group=ID, col=ARMA)) + geom_line() + 
  facet_wrap(~ARMA) + scale_y_log10() + 
  geom_point(data = rawData%>%filter(TEST=="REGN3918"), aes(x=as_numeric(TIME),y=as_numeric(DVOR) ))  +
  geom_line(data = xpdb%>%filter(TEST=="REGN3918", ARMA == "3 mg/kg IV"), aes(x=TIME, y=exp(IPRED)), col="blue" )   # from S03_modelDiagnostic
fig
  


#---------------------------------------- 
# priminary PD plot     
# -----------------------------------
#adpx = read_csv("./PKreport/data/nmdat_PK_CH50.csv") %>%  
rawData = adpx %>% mutate(DVOR =as_numeric(DVOR), 
                          ARMA = gsub("_", " ", ARMA, fix=TRUE)) %>% filter(ARMA == "3 mg/kg IV")

tdata = simData %>% 
  mutate(ARMA = gsub("_", " ", ARMA, fix=TRUE),
         IPRED = CH50) %>% 
  filter(ARMA == "3 mg/kg IV")

fig = ggplot(data=tdata, aes(x=TIME, y=IPRED, group=ID, col=ARMA)) + geom_line() + 
  facet_wrap(~ARMA) + #scale_y_log10() + 
  geom_point(data = rawData%>%filter(TEST=="CH50H"), aes(x=as_numeric(TIME),y=as_numeric(DVOR) ))  +
  geom_line(data = xpdb%>%filter(TEST=="CH50H", ARMA == "3 mg/kg IV"), aes(x=TIME, y=IPRED), col="blue" )   # from S03_modelDiagnostic
fig

