# Version 0.1   Created on 11/08/2018, Feng Yang
# 
#----------------------------------------------------------------------------
# What are needed 
# 1) a model file (.cpp)
# 2) nmdat:  nonmem dataset; 
# 3) xpdb:  nonmem output results with individual parameters 

# Goals: 
# 0) validate the cpp model against xpdb (NONMEM output file)
# 1) use actual dose regimens, calculate post-hoc exposure  
# 2) use ideal dose regimens, to assess covariate Effects on Exposure  
#----------------------------------------------------------------------------
# 
# Load model file
#---------------------
source("./cpp/mod_R3918_human_MM_final.cpp")
mod <- mread("R2810", tempdir(),mymodel)     # ?mread , ?mcode


# Load nmdat 
#---------------------
nmdat = read_csv("./data/nmdat_PKPD_1024_2018.csv")   %>% 
  mutate(ARMA = gsub("_", " ", ARMA, fix=TRUE), 
         TIME=as_numeric(TIME), 
         DVOR = as_numeric(DVOR) 
  )

# Load xpdb 
#---------------------
# see "modelDiagnostic.R"



fig = empty_canvas(label="Cpp Model\n Validation")
attr(fig, 'title') <-  ""
FIGURE_ALL[["cppModel_validation"]] = fig 



#############################################################################
#############################################################################
# Goal 1: use actual dose regimens, calculate post-hoc exposure  
#############################################################################
#############################################################################
library("mrgsolve") 

adex = nmdat %>%  filter(EVID==1) %>% 
  select(STUDYID, USUBJID, ID, ARMA, TIME, AMT, EXROUTE, RATE, EVID, CMT, MDV, TESTN, CH50HBL, C5BL, WGTBL) %>% 
  mutate(ARMA = gsub("-", " ", ARMA, fixed=TRUE))

# add individual parameters
adex = adex %>% left_join(xpdb %>% 
                            select(ID, one_of(setdiff(colnames(xpdb), colnames(adex)))) %>%  # no need of WGT_ON_CLQ WGT_ON_VSS
                            distinct(ID, .keep_all=TRUE), 
                          by="ID")  

# assign individual parameters as poulation parameters
adex=adex %>% mutate(POP="POSTHOC",
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
)  


#---------------------------------------- 
# 0) validate the cpp model against xpdb (NONMEM output file)  
# ---------------------------------------

# order ARMA for plotting purpose
arma.lst = c("1 mg/kg IV",   "3 mg/kg IV",   "10 mg/kg IV",  "30 mg/kg IV",  
             "300 mg SC",    "600 mg SC",   "400 mg SC QW")

# run on individual model parameters
simData_IPRED <- 
  runSim_by_adpx(mod %>% zero_re %>% param(COVARIATE=0, SCALING=0),
                 adex %>% select(-starts_with("TV")) %>% 
                   rename(TVCL = CL, 
                          TVV2 = V2, 
                          TVQ = Q, 
                          TVV3 = V3, 
                          TVF1 = F1, 
                          TVKA = KA, 
                          TVVMAX = VMAX, 
                          TVKSS = KSS, 
                          TVEC50 = EC50, 
                          TVGAMMA = GAMMA, 
                          TVEMAX = EMAX
                   )) %>% 
  mutate(NTIM = TIME,
         IPRED = CENTRAL/V2, 
         IPRED = ifelse(IPRED<0.078, 0.078, IPRED), 
         DVOR = IPRED, 
         ARMA = ordered(ARMA, levels = arma.lst)
  )

# run on population model parameters
simData_PRED <- runSim_by_adpx(mod %>% zero_re %>% param(COVARIATE=0, SCALING=0),
                               adex) %>%  
  mutate(NTIM = TIME,
         IPRED = CENTRAL/V2, 
         IPRED = ifelse(IPRED<0.078, 0.078, IPRED), 
         DVOR = IPRED, 
         ARMA = ordered(ARMA, levels = arma.lst)
  )






#---------------------------------------- 
# priminary PK plot     
# ---------------------------------------
tdata = nmdat %>% filter(TEST=="REGN3918") %>% filter(ARMA=="30 mg/kg IV", USUBJID=="R3918-HV-1659-826-001-147")

fig = ggplot() + 
  geom_point(data = tdata, aes(x=as_numeric(TIME),y=as_numeric(DVOR)))  + 
  
  geom_line(data = xpdb%>%filter(TEST=="REGN3918") %>% filter(ARMA=="30 mg/kg IV", USUBJID=="R3918-HV-1659-826-001-147") %>%
              mutate(DVOR = exp(DV), PRED = exp(PRED), IPRED = exp(IPRED)),  aes(x=TIME, y=IPRED, group=USUBJID),  col="blue" ) +   # from S03_modelDiagnostic
  geom_line(data = xpdb%>%filter(TEST=="REGN3918") %>% filter(ARMA=="30 mg/kg IV", USUBJID=="R3918-HV-1659-826-001-147")  %>%
              mutate(DVOR = exp(DV), PRED = exp(PRED), IPRED = exp(IPRED)),  aes(x=TIME, y=PRED, group=USUBJID),  col="green" ) + 
  # 
  
  geom_line(data=simData_IPRED %>% filter(ARMA=="30 mg/kg IV", USUBJID=="R3918-HV-1659-826-001-147"), 
            aes(x=TIME, y=IPRED, group=USUBJID, col="IPRED")  ) + 
  
  geom_line(data=simData_PRED %>% filter(ARMA=="30 mg/kg IV", USUBJID=="R3918-HV-1659-826-001-147"), 
            aes(x=TIME, y=IPRED, group=USUBJID, col="PRED") ) + 
  
  xlab("Time (day)") + 
  ylab("Population Predicted/observed Concentration (mg/L)") + 
  
  theme_bw() + base_theme(font.size = as.integer(12)) + 
  guides(col=guide_legend(ncol=4,byrow=TRUE))    
fig

fig = fig + facet_wrap(~ARMA)
attr(fig, 'title') <- "Predicted/observed Time Profiles of REGN1500 in Linear Scale"
attr(fig, 'width') <- 8
attr(fig, 'height') <- 6
FIGURE_ALL[["sim_CppModel_validation_PK_LN"]] = fig  

# log scale
fig = fig + facet_wrap(~USUBJID) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^round(x, digits=0)), labels=trans_format("log10", math_format(10^.x))) + 
  annotation_logticks(sides ="l")
attr(fig, 'title') <- "Population Predicted Time Profiles of REGN1500 in Log Scale"
attr(fig, 'width') <- 8
attr(fig, 'height') <- 6
FIGURE_ALL[["sim_CppModel_validation_PK_LOG"]] = fig   





#---------------------------------------- 
# priminary PD plot, similar for PD    
# --------------------------------------- 
# omitted
