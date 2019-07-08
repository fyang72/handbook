

setwd("~/handbook/")
source("~/handbook/global.R")

ioutput = 1

#install.packages("remotes")
#remotes::install_github("rmflight/fakeDataWithError")

#----------------------------
# adsl
#----------------------------
library(dmutate)
nsubject = 60

race_lst <- c("WHITE",
              "BLACK OR AFRICAN AMERICAN",
              "ASIAN",
              "AMERICAN INDIAN OR ALASKA NATIVE",
              "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER",
              "OTHER",
              "UNKNOWN",
              "NOT REPORTED"
)
ethnic_lst = c("NOT HISPANIC OR LATINO",  "HISPANIC OR LATINO")

adsl <- data.frame(STUDYID="FY001-001-001", USUBJID=add_prefix(1:nsubject)) %>%
  mutate(STUDYID = as.character(STUDYID)) %>% 
  mutate(USUBJID = paste0(STUDYID, "-", USUBJID)) %>% 
  
  mutate_random(WGTBL[50,110] ~ rnorm(75,30)) %>%   # [lower,upper] ~ rnorm(mu,sd))
  mutate(WGTBL = signif(WGTBL, digits=3) %>% as_numeric()) %>%
  
  mutate_random(HGTBL[150,200] ~ rnorm(175,30)) %>%  
  mutate(HGTBL = signif(HGTBL, digits=3) %>% as_numeric()) %>%
  
  mutate_random(AGE[18,60] ~ rnorm(25,30)) %>%  
  mutate(AGE = signif(AGE, digits=2) %>% as_numeric()) %>%
  
  mutate(AGEU = "YEARS") %>% 
  mutate(BMIBL = signif(WGTBL/(HGTBL/100)^2, digits=3) %>% as_numeric()) %>%   
  
  mutate(BSABL = signif(0.007184 *
                          as_numeric(WGTBL)^0.425 * 
                          as_numeric(HGTBL)^0.725, digits=3) %>% as_numeric()) %>%  # in M^2
 
  # c( "MALE", "FEMALE", "UNKNOWN")
  mutate_random(SEXN ~ rbinomial(0.7)) %>%  # '1'=70%, '0'=30%
  mutate(SEX = ifelse(SEXN==1, "MALE", 
                      ifelse(SEXN==0, "FEMALE", 
                             ifelse(SEXN==3, "UNKNOW", NA)))) %>% 
  
  # RACE
  mutate(RACE = ordered(sample(race_lst, nsubject,replace=TRUE), levels=race_lst)) %>% 
  mutate(RACEN = as.integer(RACE)) %>% 
  
  # ETHNIC
  mutate(ETHNIC = ordered(sample(ethnic_lst, nsubject,replace=TRUE), levels=ethnic_lst)) %>% 
  mutate(ETHNICN = as.integer(ETHNIC))  
  
# STUDYID
adsl <- adsl %>% mutate(STUDYID = as.character(STUDYID)) %>% as_tibble()
  

#----------------------------
# adex 
#----------------------------
#STUDYID	USUBJID	EXSTDTC	EXENDTC	EXDOSE	EXDOSU	EXROUTE	EXROUTN	ARMA	ARMAN	
# VISIT	VISITNUM	TIME	TRTSDTM	EXTRT	EXDUR	EXSEQ
# 1 mg/kg, 3 mg/kg, 10 mg/kg 
# 75 mg SC, 150 mg SC, 300 mg SC

library(dmutate)

adex <- data.frame(STUDYID="FY001-001-001", USUBJID=add_prefix(1:nsubject)) %>%
  mutate(USUBJID = paste0(STUDYID, "-", USUBJID)) %>% 
  mutate(STUDYID = as.character(STUDYID)) %>%  
  mutate(ARMA = c(rep(c("1 mg/kg IV", "3 mg/kg IV", "10 mg/kg IV"), each=10), 
                  rep(c("75 mg SC", "150 mg SC", "300 mg SC"), each=10))
  ) %>% 
  mutate(ARMA = ordered(ARMA, levels=unique(ARMA))) %>% 
  mutate(ARMAN = as.integer(ARMA)) %>% 
  
  mutate(EXDOSE = c(rep(c(1, 3, 10), each=10), 
                    rep(c(75, 150, 300), each=10))
  ) %>% 
  
  # admin.route.lst = c("SUBCUTANEOUS", "INTRAVENOUS", "INTRAMUSCULAR", "IVT")
  mutate(EXDOSU = rep(c("mg/kg", "mg"), each=30)) %>%
  mutate(EXROUTE = rep(c("INTRAVENOUS", "SUBCUTANEOUS"), each=30)) %>%
  mutate(EXROUTN = rep(c(2, 1), each=30)) %>%
  
  mutate(EXTRT = "FY001", 
         EXTRTN = 1, 
         NTIM = 0, 
         VISIT = "VISIT 3", 
         VISITNUM = 3 
         )  
   

# EXSTDTC	EXENDTC
reference_time <- as.POSIXct("1972-01-01 12:12:12", tz = "America/New_York",
                             tryFormats = c("%Y-%m-%d %H:%M:%OS",
                                            "%Y/%m/%d %H:%M:%OS",
                                            "%Y-%m-%d %H:%M",
                                            "%Y/%m/%d %H:%M",
                                            "%Y-%m-%d",
                                            "%Y/%m/%d")
)
adex <- adex %>% as_tibble() %>% 
  mutate(EXSTDTC = reference_time + sample(0:10000, nsubject)) %>% 
  mutate(EXENDTC = EXSTDTC + sample(seq(12*60, 18*60, by=1), nsubject, replace=TRUE)) %>% 
  
  mutate(EXSTDTC = as.character(EXSTDTC), 
         EXENDTC = as.character(EXENDTC)) %>% 
  
  mutate(EXENDTC =  ifelse(EXROUTE %in% "SUBCUTANEOUS", EXSTDTC, EXENDTC))  
    
# TRTSDTM
adex <- adex %>% mutate(TRTSDTM = EXSTDTC)







#----------------------------
# adpc
#----------------------------
#STUDYID	USUBJID	SAMDTTM	DVOR	DVORU	ARMA	ARMAN	VISIT	VISITNUM	TIMEPT	
# TIME	NTIM	TEST	TESTN	TESTCD	TESTCAT	BLQ	LLOQ	METHOD


adpc <- data.frame(STUDYID="FY001-001-001", USUBJID=add_prefix(1:nsubject)) %>%
  mutate(USUBJID = paste0(STUDYID, "-", USUBJID)) %>% 
  mutate(STUDYID = as.character(STUDYID)) %>% 
  mutate(ARMA = c(rep(c("1 mg/kg IV", "3 mg/kg IV", "10 mg/kg IV"), each=10), 
                  rep(c("75 mg SC", "150 mg SC", "300 mg SC"), each=10))
  ) %>%
  mutate(ARMA = ordered(ARMA, levels=unique(ARMA))) %>% 
  mutate(ARMAN = as.integer(ARMA)) %>% 
  
  # admin.route.lst = c("SUBCUTANEOUS", "INTRAVENOUS", "INTRAMUSCULAR", "IVT")
  mutate(TEST = "Concentration of FY001", TESTN=1, TESTCD="FY001", TESTCAT="PK") %>%
  mutate(BLQ = 0, LLOQ = 0.001, METHOD="YZ001") 
  
subj_lst <- unique(adpc$USUBJID) 
adpc_IV <- expand.grid(USUBJID=subj_lst[1:30], 
                       NTIM=c(15/60/24, 1/24, 4/24, 8/24, 24/24, 
                              3, 7, 14, 21, 28, 56, 84, 112)) %>% arrange(USUBJID) %>% 
  mutate(USUBJID = as.character(USUBJID))

adpc_SC <- expand.grid(USUBJID=subj_lst[31:60], 
                       NTIM=c(1,3,7, 14, 21, 28, 56, 84, 112)) %>% arrange(USUBJID)%>% 
  mutate(USUBJID = as.character(USUBJID))

adpc = bind_rows(adpc_IV, adpc_SC) %>% arrange(USUBJID, NTIM) %>% 
  left_join(adpc %>% distinct(USUBJID, .keep_all=TRUE) %>% 
              select(STUDYID, ARMA, ARMAN,USUBJID, TEST, TESTN, TESTCD, TESTCAT, 
                     BLQ, LLOQ, METHOD), 
            by = "USUBJID")


# TIMEPT
adpc <- adpc %>% mutate(TIMEPT = ifelse(NTIM==15/60/24, "15 MINS", 
                                        ifelse(NTIM==1/24, "1 HR", 
                                               ifelse(NTIM==4/24, "4 HR", 
                                                      ifelse(NTIM==8/24, "8 HR", paste0("DAY ", NTIM+1))))))
# VISIT
adpc <- adpc %>% mutate(VISITNUM = round(adpc$NTIM/7, digits=0) %>% as.factor() %>% as.integer() + 2) %>% 
  mutate(VISIT = paste0("VISIT ", VISITNUM))


# SAMDTTM
adpc <- adpc %>% as_tibble() %>%  
  left_join(adex %>% distinct(USUBJID, .keep_all=TRUE) %>% select(USUBJID, EXSTDTC), by="USUBJID") %>% 
  mutate(EXSTDTC = as.POSIXct(EXSTDTC, tz = "America/New_York")) %>% 
  mutate(SAMDTTM = EXSTDTC + 
           NTIM*24*60*60 + 
           sample(seq(5*60, 10*60, by=1), 1, replace=TRUE)
         )  


# DVORU
adpc <- adpc %>% mutate(
  DVOR = NA, 
  DVORU = "mg/L")


if(ioutput == 1) {
  write_csv(adsl, path="~/handbook/data/adsl.csv")
  write_csv(adex, path="~/handbook/data/adex.csv")
}


adsl0 = adsl
adex0 = adex
adpc0 = adpc


#----------------------------
# nmdat
#----------------------------
adsl <- build_adsl(adsl0)
adex <- build_adex(adex0)
adpc <- build_adpc(adpc0) 

nmdat <- build_adpx(adsl, adex, adpc, other=NULL)
nmdat <- nmdat %>% build_nmdat() 

 
cppModel_file = "~/handbook/cpp/LN001.cpp"
cppModel=mread(model='cppModel', 
               project=paste0(HOME, '/cpp/'), 
               quiet=TRUE, 
               file=basename(cppModel_file))

adsl <- adsl; #data.frame(USUBJID="001", WGTBL=75) %>% mutate(USUBID=as.character(USUBJID))
adex <- parseARMA(c("1 mg/kg IV Q1W*1", "3 mg/kg IV Q1W*1", "10 mg/kg IV Q1W*1", 
                    "75 mg SC Q1W*1", "150 mg SC Q1W*1", "300 mg SC Q1W*1")
                  )

tgrid <- c(15/60/24, 1/24, 4/24, 8/24, 24/24, 3)
simData <- runSim_by_nmdat(
  cppModel = cppModel,    # model file 
  adex = nmdat %>% mutate(RATE=ifelse(is.na(RATE), 0, RATE)),   # dose regimen
  simulation_delta = 1, #simulation_delta,  # integration step                  
  tgrid = tgrid,     # extra timepoint (other than delta)
  infusion_hrs_lst = 1,  # hour,  infusion hours
  treat_end = 112, 
  sim_end = 112,   # how long of the followup period after treatment
  seed=1234  
)


figure =NULL
tdata = simData  #%>% 
#mutate(ordered(ARMA, levels=unique(ARMA))

x=setup_scale(myscale='1_4', mylimit=c(0, max(tdata$TIME/7, na.rm=TRUE)))
tdata = tdata %>% 
  mutate(xvar = TIME/7, 
         yvar = IPRED
  )

figLn = ggplot(data=tdata, #%>%mutate(USUBJID=paste0(ARMA,USUBJID)), 
               aes(x=xvar, y=yvar, group=ID, col=ARMA)) + 
  geom_line() + 
  
  xlab("Time (Week)") + 
  ylab("Predicted Concentration (mg/L)") + 
  
  #coord_cartesian(ylim = c(1E-2, 300)) + 
  theme_bw() + base_theme(font.size = as.integer(12)) + 
  
  scale_color_manual(values = colScheme()) +  #c("black", "blue",   "black",  "red")) + #"cyan",
  
  scale_x_continuous(breaks=x$breaks, label=x$labels) +
  #scale_x_continuous(breaks=seq(0, 112, by=7), label=seq(0, 112, by=7)) +
  
  guides(col=guide_legend(ncol=3, byrow=TRUE))   + 
  
  ggplot2::theme(panel.grid.minor = element_line(colour = "gray98",size=0.75))   

#-----------------------
# fig1: Linear profile
#-----------------------
loc_x <- 0  # location of line label 
BLOQ = 0.078
fig = figLn + 
  geom_hline(yintercept=c(BLOQ), lty="dashed") + 
  geom_text(y=BLOQ*100, x=loc_x, aes(label=paste0("BLQ=", BLOQ, " mg/L"), hjust=0), size=3, color='black')   

attr(fig, 'title') <-   "Predicted Individual Concentration-time Profiles for Proposed Dosing Regimens"
attr(fig, 'width') <- 8
attr(fig, 'height') <- 6
figure[["SIM_INDIV_PK_LN"]]  = fig

fig + scale_y_log10() + facet_wrap(~ARMA)


##################################################
nmdat <- nmdat %>% mutate(TIME=round(TIME, digits=3)) %>%
  left_join(simData %>% select(USUBJID, TIME, IPRED) %>% 
              rename(DVOR2=IPRED) %>% 
              mutate(TIME=round(TIME, digits=3)), 
            by = c("USUBJID", "TIME")
  ) %>% 
  mutate(DVOR=DVOR2) %>% select(-DVOR2) %>% 
  mutate(DV = log(DVOR))

head(nmdat)

if(ioutput == 1) {
  adpc <- nmdat %>% filter(EVID==0, TIME!=0) %>% 
    select(one_of(colnames(adpc)))
  
write_csv(adpc, path="~/handbook/data/adpc.csv")


write_csv(nmdat, path="~/handbook/data/nmdatPK.csv")

}


################
nmdat0 = nmdat
#################
# add adpd



adpc <- nmdat0 %>% filter(EVID==0, TIME!=0) %>% 
  select(one_of(colnames(adpc)))

EMAX = -100
EC50 = 50.8

library(fakeDataWithError)
adpd1 <- adpc %>% mutate(RESP1 = EMAX*DVOR/(DVOR + EC50)) %>% 
  mutate(RESP1 = -1*add_prop_uniform(1, abs(RESP1), u_sd=10, p_sd=0.1 )) %>% 
  mutate(DVOR=RESP1) %>% select(-RESP1) %>% 
  mutate(TEST = "RESP1", TESTN = 2, TESTCD = "RESP1",TESTCAT= "RESP1", METHOD="YZ002", DVORU="unitless", 
         CMT = 4, LLOQ=-100)
#ggplot(adpd, aes(x=DVOR, y=RESP1)) + geom_point()

adpd2 <- adpc %>% mutate(RESP2 = EMAX*DVOR/(DVOR + EC50)) %>% 
  mutate(RESP2 = +1* add_prop_uniform(1, abs(RESP2), u_sd=20, p_sd=0.2 ))%>% 
  mutate(DVOR=RESP2) %>% select(-RESP2) %>% 
  mutate(TEST = "RESP2", TESTN = 3, TESTCD = "RESP2",TESTCAT= "RESP2", METHOD="YZ003", DVORU="unitless", 
         CMT = 5, LLOQ=-100)
#ggplot(adpd, aes(x=DVOR, y=RESP2)) + geom_point()
 
adpd <- bind_rows(adpd1, adpd2) %>% arrange(USUBJID, TIME)

if(ioutput == 1) {
  write_csv(adpd, path="~/handbook/data/other.csv")
}

adsl <- build_adsl(adsl0)
adex <- build_adex(adex0)
adpc <- adpc
other <- build_adpc(adpd) 

nmdat <- build_adpx(adsl, adex, adpc, other=other)
nmdat <- nmdat %>% build_nmdat() 

nmdat <- nmdat %>% mutate(
  DV = ifelse(TESTCAT %in% "PK", log(DVOR), DVOR), 
  CMT = ifelse(TESTCAT %in% "RESP1", 4, 
               ifelse(TESTCAT %in% "RESP2", 5, CMT)
  )
)


if(ioutput == 1) {
  
  write_csv(nmdat, path="~/handbook/data/nmdatPKPD.csv")
}

