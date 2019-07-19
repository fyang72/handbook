 

library(dplyr)
library("mrgsolve")
source("./cpp/mod.R2810.LN900.cpp")
mod <- mread("R2810", tempdir(),mymodel)     # ?mread , ?mcode

#mod_MM = mod %>% param(THETA_MM)
#param(mod_MM)  


seed =1234
set.seed(seed);    

#----------------------------------------  
# adsl
#----------------------------------------
#MODEL_HOME <- "H:\\FYANG\\R2810_PD1\\MODEL\\"
#tt = read_runno(MODEL_HOME, subdir="ctl/PsN/", runno="LN014", postfix="")  

#adsl = create_adsl(POP="STD", nsubject=10, meanWt=80, seed=1234)    
adsl =  adpx00 %>% dplyr::filter(C==".") %>% distinct(USUBJID, .keep_all=TRUE)     #, ID %in% tt$xpdb$ID)

adsl$POP = "SIM"
#adsl$WEIGHTBL = adsl$WGTBL  
adsl = adsl %>% select(POP, USUBJID, WGTBL, ALBBL, IGGBL,  BMIBL, ALTBL, RACEN)  ########
adsl = adsl %>% mutate(WGTBL=as_numeric(WGTBL), 
                       ALBBL=as_numeric(ALBBL), 
                       IGGBL=as_numeric(IGGBL), 
                       BMIBL=as_numeric(BMIBL), 
                       ALTBL=as_numeric(ALTBL),
                       RACEN=as_numeric(RACEN))
# WGTBL=75, ALBBL=38, IGGBL=9.7, BMIBL=26.5, ALTBL=21, RACEN=1,
# adsl$ALB_ON_CLQ=-1.00381
# adsl$ALT_ON_CLQ=-0.0817805
# adsl$IGG_ON_CLQ=0.182303    
# adsl$BMI_ON_VSS=-0.552805
# adsl$BLK_ON_T50=0.946409

#adsl = sample_n(adsl, size=2000, replace=TRUE)
#adsl$USUBJID = as.integer(as.factor(adsl$USUBJID))


#----------------------------------------
# adex
#----------------------------------------  
#library('gdata')
library('xlsx')    # do you have to open the file at least once?
ADEX = data.frame(read.xlsx("./cpp/run_Rsim_input.xlsx", "Sheet1"), stringsAsFactors=FALSE)
ADEX0 = ADEX[which(!is.na(as.numeric(ADEX$STUDYID))),  ]

ADEX0$ARMA = trim(ADEX0$ARMA)

adsl = data.frame(POP="SIM", USUBJID="001", WGTBL=75)
adex = create_adex(adsl, ADEX0%>% filter(STUDYID==5))   #, GROUP %in% c(2,3)


#----------------------------------------
# run simulation
#---------------------------------------- 

# setup simulation timept 
treat_end = 3*56  
sim_end = 3*56 

#treat_end = 24*7;  sim_end = 32*7
tgrid = sim_timept(start=0, end=sim_end, delta=1, dtime=seq(0,treat_end, by=7))

OUT = NULL
n.replicate = 10
for (ireplicate in 1:n.replicate)  {
  
  print(ireplicate)
  
  # run simulation
  out = mod %>%  param( COVARIATE=1, SCALING=0) %>%    zero_re %>% 
   
    ev(as.ev(adex)) %>% mrgsim(end=sim_end, delta=1, add=tgrid) %>% as.data.frame() #%>% capitalize_names()
  
  out = out %>% left_join(adex %>% as.data.frame() %>% select(ID, POP, STUDYID, USUBJID, ARMA), by="ID") 
  colnames(out) = toupper(colnames(out))
  
  OUT= rbind(OUT, cbind(replicate=ireplicate, out))
}



tdata = OUT %>% mutate(DVOR=IPRED)

#######################################################################
#######################################################################
#######################################################################
#######################################################################

load("./data/Predict350.Rdata")
tdata = Predict350


# setup simulation timept 
treat_end = 3*56  
sim_end = 3*56 


tdata = tdata %>% filter(TIME>=(treat_end-6*7), TIME<=treat_end) %>% 
           mutate(NTIM=TIME) %>% 
          mutate(USUBJID = paste0(USUBJID, "-", replicate))


#ggplot(tdata, aes(x=NTIM, y=DVOR, group=ARMA)) + geom_point()


tdata = tdata %>% 
  group_by_(.dots =c("STUDYID", "ARMA",  "USUBJID")) %>%   
  dplyr::summarise(
    Cmin.ss = DVOR[n()],  # the last conc at this time interval
    Cmax.ss = max(DVOR),
    AUCtau.ss = auc_partial(NTIM, DVOR),
    Cavg.ss = AUCtau.ss/(6*7)   # 6 wks
     
  )

# melt down
tdata  = tdata %>% gather(TEST, DVOR, 4:7)



group_by = c("STUDYID", "ARMA", "TEST")
tdata = tdata %>% calc_stats(id="USUBJID", group_by=group_by, value="DVOR")  %>%
  select(STUDYID, ARMA, TEST, N, Mean_SD)  

tdata = tdata %>% spread(TEST, Mean_SD) 

tdata = tdata %>% ungroup() %>% mutate(N = 2000) %>% select(-Cavg.ss, -STUDYID)

write_csv(tdata, path= "./adhoc_0405_2018/output.csv")


