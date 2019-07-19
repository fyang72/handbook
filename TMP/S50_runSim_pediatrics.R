
###################################
# setup
###################################
# some variables used in dose regimen

idebug = 1
if (idebug ==1) {
  FIGURE_ALL = NULL
  TABLE_ALL = NULL
}


mg = 1; mkg = 2
SC = 1;  IV = 2


# load mrgsolve cpp model
#-------------------------------------
library(dplyr)
library("mrgsolve")
source("./PKreport/cpp/mod_R3918_human_MM.cpp")

#source("./PKreport/cpp/mod_R3918_human_TMDD.cpp")
#source("./PKreport/cpp/mod_R3918_MM002.cpp")

mod <- mread("R2810", tempdir(),mymodel)     # ?mread , ?mcode



###################################
# runCurrentSim
###################################
runCurrentSim <- function(mod, adsl, Dosing.Regimen, seed) {
  
  adex = parseARMA(Dosing.Regimen) %>% rename(DOSEID = ID)
  
  tdata = expand.grid(DOSEID = unique(adex$DOSEID), USUBJID = unique(adsl$USUBJID), POP=unique(adsl$POP))
  tdata = tdata %>% left_join(adex, by="DOSEID")
  tdata = tdata %>% left_join(adsl, by=c("USUBJID", "POP" ))
  
  adex = tdata %>% mutate(ID = as.integer(as.factor(paste0(USUBJID,"_", DOSEID, "_", POP))),
                          amt = ifelse(unit=="mg/kg", amt * WGTBL, amt), 
                          cmt = ifelse(route=="IV", 2, 
                                       ifelse(route=="SC", 1, 0)), 
                          rate = ifelse(route=="IV", amt/(1/24), 0))
  
  adex = adex %>% rename(ARMA0=ARMA) %>% 
    group_by(ID) %>% mutate(ARMA = paste0(ARMA0, collapse="+")) %>% ungroup() %>% as.data.frame()
  
  adex$ARMA = ordered(adex$ARMA, levels=unique(adex$ARMA))
  
  # set seed to make reproducible simulation (if IIV=1)
  set.seed(seed)
  
  infhr.lst = 1
  delta = 1
  followUpPeriod = 84  #days
  
  # library("PKPDmisc")
  treat_end = max((adex$addl+1)*adex$ii)  #l24*7    
  sim_end = treat_end + followUpPeriod   # default 112 days                      # note dose by week only
  tgrid = sim_timept(start=0, end=treat_end, delta=delta, dtime=seq(0,treat_end, by=7), infhr.lst=infhr.lst)
  
  #---------------------------------------- 
  # run simulation
  #---------------------------------------- 
  out = mod  %>% data_set(as.ev(adex)) %>% 
    mrgsim(end=sim_end, delta=delta, add=tgrid, tad=TRUE, carry.out=c("ii", "evid")) %>%  as.data.frame()
  colnames(out) = toupper(colnames(out))
  
  # only add ID, STUDYID, USUBJID, ARMA
  out = out %>% left_join(adex %>% as.data.frame() %>% 
                            distinct(ID, .keep_all=TRUE) %>% 
                            select(ID,  USUBJID, ARMA, POP),
                          by=c("ID"))  
  
  # nominal time (NTIM)
  out = out %>% mutate(NTIM = TIME)
  
  simData = out %>% mutate(EVID=ifelse(TAD==0, 1, 0))   %>% slice(2:n()) 
  
  
  # fill up II by  Last Observation Carried Forward
  # na.locf https://stackoverflow.com/questions/48392015/r-carry-forward-last-observation-n-times-by-group?rq=1
  #out = out %>% mutate(II=ifelse(II==0, NA, II))  %>% group_by(ID) %>% fill(II)
  
  
  # EVID, DVOR and EXSEQ    
  # -----------------------------------
  #simData = out %>% mutate(EVID=ifelse(TAD==0, 1, 0))   %>% slice(2:n()) %>%
  #  mutate(DVOR = CP) %>% 
  #  group_by(ID)%>%  mutate(EXSEQ=cumsum(EVID)) %>%   # for calculat the last dose interval later
  #  as.data.frame()
  
  return(simData)
}



##########################################################################################
# canvas_simulation_results
##########################################################################################
fig = empty_canvas(label="Simulation Results")
attr(fig, 'title') <-  ""

FIGURE_ALL[["canvas_simulation_results"]] = fig  



################################################################################################
# simulate 
#   1) 15 mg/kg IV + 300 mg SC QW*4
#   2) 20 mg/kg IV + 400 mg SC QW*4
#   3) 25 mg/kg IV + 600 mg SC QW*4
#   4) 30 mg/kg IV + 800 mg SC QW*4
################################################################################################

seed = 1234

################################################################################################
# priminary plots for a single typical subject with body weight 70 kg
################################################################################################
Dosing.Regimen = c("12 mg/kg Q1W*1 IV + 300 mg SC QW*16" , 
                   "18 mg/kg Q1W*1 IV + 400 mg SC QW*16" ,   
                   "28 mg/kg Q1W*1 IV + 600 mg SC QW*16",
                   "30 mg/kg Q1W*1 IV + 800 mg SC QW*16",
                   "42 mg/kg Q1W*1 IV + 800 mg SC QW*16"
)

Dosing.Regimen = c("30 mg/kg Q1W*1 IV + 300 mg SC QW*16" , 
                   "30 mg/kg Q1W*1 IV + 400 mg SC QW*16" ,   
                   "30 mg/kg Q1W*1 IV + 600 mg SC QW*16",
                   "30 mg/kg Q1W*1 IV + 800 mg SC QW*16" 
)


# define a population
nsubject = 1 
adsl = create_adsl(POP="A typical patient with 70kg", nsubject, meanWt=70, seed =seed) #%>% filter(WGTBL<160)
adsl$WGTBL = 70
simData_70kg_N1 = runCurrentSim(mod  %>% zero_re %>% param( COVARIATE=0, SCALING=0), 
                                adsl, Dosing.Regimen, seed)

tdata = simData_70kg_N1  %>% mutate(DVOR=CP)
tdata = tdata %>% mutate(ARMA = gsub("Q1W*1 ", "", ARMA, fix=TRUE ))

x=setup_axis(xscale='7_14', xlimits=c(0, max(tdata$TIME)))

#---------------------------------------- 
# priminary PK plot     
# -----------------------------------
fig = ggplot(data=tdata, aes(x=TIME, y=DVOR, group=ID, col=ARMA)) + geom_line() + 
  facet_wrap(~POP) + 
  xlab("Time (day)") + 
  ylab("Predicted Conc. of Total REGN3918 (mg/L)") + 
  
  #coord_cartesian(xlim = c(0, 85)) + 
  theme_bw() + base_theme(font.size = as.integer(12)) + 
  
  scale_x_continuous(breaks=x$xbreaks, label=x$xlabels) +
  #scale_x_continuous(breaks=seq(0, 112, by=7), label=seq(0, 112, by=7)) +
  
  guides(col=guide_legend(ncol=2,byrow=TRUE))   + 
  
  ggplot2::theme(panel.grid.minor = element_line(colour = "gray98",size=0.75))   

attr(fig, 'title') <-   "Predicted Mean Concentration-time Profiles of REGN3918 for Potential Dosing Regimens"
FIGURE_ALL[["priminary_PK_plot"]]  = fig 


fig


# priminary PD plot     
# -----------------------------------

tdata = simData_70kg_N1  %>% mutate(CH50_PCHG = (CH50-CH50_BL)/CH50_BL*100) %>% mutate(DVOR=CH50_PCHG)
x=setup_axis(xscale='7_14', xlimits=c(0, max(tdata$TIME)))

fig = ggplot(data=tdata, aes(x=TIME, y=DVOR, group=ID, col=ARMA)) + geom_line() + 
  facet_wrap(~POP) + 
  xlab("Time (day)") + 
  ylab("Predicted Percent Change from Baseline (CH50)") + 
  
  #coord_cartesian(xlim = c(0, 85)) + 
  theme_bw() + base_theme(font.size = as.integer(12)) + 
  
  scale_x_continuous(breaks=x$xbreaks, label=x$xlabels) +
  #scale_x_continuous(breaks=seq(0, 112, by=7), label=seq(0, 112, by=7)) +
  
  guides(col=guide_legend(ncol=2,byrow=TRUE))   

fig

attr(fig, 'title') <-   "Predicted Mean Percent Change from Baseline (CH50) for Potential Dosing Regimens"
FIGURE_ALL[["priminary_PD_plot"]]  = fig 




################################################################################################
# priminary plots for pediatric patients
################################################################################################

seed = 1234

Dosing.Regimen = c("12 mg/kg Q1W*1 IV + 300 mg SC QW*16" , 
                   "18 mg/kg Q1W*1 IV + 400 mg SC QW*16" ,   
                   "28 mg/kg Q1W*1 IV + 600 mg SC QW*16",
                   "30 mg/kg Q1W*1 IV + 800 mg SC QW*16",
                   "42 mg/kg Q1W*1 IV + 800 mg SC QW*16"
)


Dosing.Regimen = c("60 mg/kg Q1W*1 IV + 250 mg SC QW*26" , 
                   "60 mg/kg Q1W*1 IV + 300 mg SC QW*26" ,   
                   "60 mg/kg Q1W*1 IV + 400 mg SC QW*26",
                   "60 mg/kg Q1W*1 IV + 500 mg SC QW*26" 
)

 

# define a population
nsubject = 1000

# 1 Population: BODY WEIGHT RANGE from 5-10 kg
adsl_7p5 = create_adsl(POP="A typical patient with 7.5kg [5-10]", nsubject, meanWt=7.5, lowerWt=5, upperWt=10, seed =seed)  
summary(adsl_7p5$WGTBL); length(adsl_7p5$USUBJID)

# 2 Population: BODY WEIGHT RANGE from 10-20 kg
adsl_15 = create_adsl(POP="A typical patient with 15kg [10-20]", nsubject, meanWt=15, lowerWt=10, upperWt=20,seed =seed)  
 

# 3 Population: BODY WEIGHT RANGE from 10-30 kg
#adsl_20 = create_adsl(POP="A typical patient with 20kg", nsubject, meanWt=20, lowerWt=10, upperWt=30,seed =seed)  
# summary(adsl_20$WGTBL)
#

# 3 Population: BODY WEIGHT RANGE from 20-40 kg
adsl_30 = create_adsl(POP="A typical patient with 30kg [20-40]", nsubject, meanWt=30, lowerWt=20, upperWt=40,seed =seed)  
adsl = rbind(adsl_7p5, adsl_15, adsl_30)


# %>% zero_re
simData = runCurrentSim(mod %>% param( COVARIATE=0, SCALING=1), # , WGT_ON_CLQ=0, WGT_ON_VSS=0 ), 
                                adsl, Dosing.Regimen, seed)

tdata = simData  %>% mutate(DVOR=CP)
tdata = tdata %>% mutate(ARMA = gsub("Q1W*1 ", "", ARMA, fix=TRUE ))
tdata = tdata %>% mutate(POP = gsub("A typical patient with", "WT=", POP, fix=TRUE ), 
                         POP = ordered(POP, levels=unique(POP)))

tdata = tdata %>% mutate(ARMA = ordered(ARMA, levels=unique(ARMA)))

tdata$POP %>% unique()

tdata$ARMA %>% unique()

x=setup_axis(xscale='7_28', xlimits=c(0, max(tdata$TIME)))




#---------------------------------------- 
# priminary PK plot     
# -----------------------------------
time =7+26*7
fig = ggplot(data=tdata%>% filter(TIME==time), aes(x=POP, y=DVOR, col=POP)) + 
  geom_boxplot(notch=TRUE, outlier.colour="black") + #, outlier.shape=8,outlier.size=4)  + 
  #xlab("Time (day)") + 
  xlab("") +
  ylab("Predicted Conc. of Total REGN3918 (mg/L)") + 
  
  #coord_cartesian(xlim = c(0, 85)) + 
  theme_bw() + base_theme(font.size = as.integer(12)) + 
  
  #scale_x_continuous(breaks=x$xbreaks, label=x$xlabels) +
  #scale_x_continuous(breaks=seq(0, 112, by=7), label=seq(0, 112, by=7)) +
  
  geom_hline(yintercept = 400, lty="dashed")  +
  
  guides(col=guide_legend(ncol=3,byrow=TRUE))   + 
  
  ggplot2::theme(panel.grid.minor = element_line(colour = "gray98",size=0.75)) +    
  ggplot2::theme(legend.position="none")  + 
  facet_wrap(~ARMA) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

attr(fig, 'title') <-   "Predicted Mean Concentration-time Profiles of REGN3918 for Potential Dosing Regimens"
attr(fig, 'width') <- 8
attr(fig, 'height') <- 6
FIGURE_ALL[["priminary_PK_plot_pediatric"]]  = fig 


fig 
  
  
  
#---------------------------------------- 
# priminary PK plot     
# -----------------------------------
fig = ggplot(data=tdata, aes(x=TIME, y=DVOR, group=ID, col=ARMA)) + geom_line() + 
  facet_wrap(~POP, scale="free") + 
  xlab("Time (day)") + 
  ylab("Predicted Conc. of Total REGN3918 (mg/L)") + 
  
  #coord_cartesian(xlim = c(0, 85)) + 
  theme_bw() + base_theme(font.size = as.integer(12)) + 
  
  scale_x_continuous(breaks=x$xbreaks, label=x$xlabels) +
  #scale_x_continuous(breaks=seq(0, 112, by=7), label=seq(0, 112, by=7)) +
  
  geom_hline(yintercept = 400, lty="dashed")  +
  
  guides(col=guide_legend(ncol=2,byrow=TRUE))   + 
  
  ggplot2::theme(panel.grid.minor = element_line(colour = "gray98",size=0.75))   

attr(fig, 'title') <-   "Predicted Mean Concentration-time Profiles of REGN3918 for Potential Dosing Regimens"
attr(fig, 'width') <- 8
attr(fig, 'height') <- 6
FIGURE_ALL[["priminary_PK_plot_pediatric"]]  = fig 


fig


# priminary PD plot     
# -----------------------------------

tdata = simData_70kg_N1  %>% mutate(CH50_PCHG = (CH50-CH50_BL)/CH50_BL*100) %>% mutate(DVOR=CH50_PCHG)
x=setup_axis(xscale='7_28', xlimits=c(0, max(tdata$TIME)))

fig = ggplot(data=tdata, aes(x=TIME, y=DVOR, group=ID, col=ARMA)) + geom_line() + 
  facet_wrap(~POP) + 
  xlab("Time (day)") + 
  ylab("Predicted Percent Change from Baseline (CH50)") + 
  
  #coord_cartesian(xlim = c(0, 85)) + 
  theme_bw() + base_theme(font.size = as.integer(12)) + 
  
  scale_x_continuous(breaks=x$xbreaks, label=x$xlabels) +
  #scale_x_continuous(breaks=seq(0, 112, by=7), label=seq(0, 112, by=7)) +
  
  guides(col=guide_legend(ncol=2,byrow=TRUE))   

fig

attr(fig, 'title') <-   "Predicted Mean Percent Change from Baseline (CH50) for Potential Dosing Regimens"
attr(fig, 'width') <- 8
attr(fig, 'height') <- 6

FIGURE_ALL[["priminary_PD_plot_pediatric"]]  = fig 




################################################################################################
# final output to doc and ppt
################################################################################################

if (idebug ==1) { 
  # use customized version
  myppt <- pptx(title = "title", template = './PKreport/docs/pptTemplate.pptx')
  mydoc <- docx(template = "./PKreport/docs/memoTemplate.docx", empty_template = FALSE)
  
  tt = print2_word_ppt(FIGURE_ALL, TABLE_ALL,  mydoc, myppt) 
  
  writeDoc(tt$mydoc, file = './PKreport/docs/memo_results.docx')
  
  writeDoc(tt$myppt, file = './PKreport/docs/ppt_results.pptx')
  
}

