
idebug = 1

if (idebug ==1) {
  FIGURE_ALL = NULL
  TABLE_ALL = NULL
}


library(xpose4)
debugit = FALSE
source(paste0(dirname(HOME), "/global.R"))
library(scales) 



mg = 1; mkg = 2
SC = 1;  IV = 2


#--------------------------------
# load mrgsolve cpp model
#-------------------------------------
library(dplyr)
library("mrgsolve")
source("./PKreport/cpp/mod_R3918_human_MM.cpp")

#source("./PKreport/cpp/mod_R3918_human_TMDD.cpp")
#source("./PKreport/cpp/mod_R3918_MM002.cpp")

mod <- mread("R2810", tempdir(),mymodel)     # ?mread , ?mcode


#--------------------------------
# no loading dose
#--------------------------------

addl = 28*5
amt = c(60, 70, 80, 90, 100)
adex = ev(time=0, ii=1, addl=addl, amt=amt)  %>% as.data.frame()
adex = adex %>% mutate(ID = 1:nrow(adex), 
                       ARMA = paste0(amt, " mg", " SC*QD")) 
                       #ARMA = ordered(ARMA, levels=unique(ARMA)))

adex_no_loading_dose = adex


#--------------------------------
# with loading dose
#--------------------------------
WGTBL = 70

addl = 0
amt0 = rep(30, times=5)  #c(20, 22, 25, 27.5, 30)  
adex0 = ev(time=0, ii=1, addl=addl, amt=amt0*WGTBL, cmt=2, rate=amt0*WGTBL/(1/24), wt=WGTBL)  %>% as.data.frame()
adex0 = adex0 %>% mutate(ID = 1:nrow(adex0), 
                       ARMA = paste0(amt0, " mg/kg", " IV*QD")) 
                       #ARMA = ordered(ARMA, levels=unique(ARMA)))
adex0


addl = 28*5
amt = c(60, 70, 80, 90, 100)
adex1 = ev(time=1, ii=1, addl=addl, amt=amt, rate=0, wt=WGTBL)  %>% as.data.frame()
adex1 = adex1 %>% mutate(ID = 1:nrow(adex1), 
                       ARMA = paste0(amt, " mg", " SC*QD"))

adex = rbind(adex0, adex1) %>% arrange(ID, time) %>% 
       group_by(ID) %>% mutate(ARMA = paste0(ARMA, collapse="+")) #%>% 
adex$ARMA = adex$ARMA %>% ordered(., levels=unique(adex$ARMA))

                       # mutate( ARMA = ordered(ARMA, levels=unique(ARMA)))

adex_with_loading_dose = adex

adex




#--------------------------------
# run simulation
#--------------------------------
adex = adex_no_loading_dose
adex = adex_with_loading_dose


sim_end = 140
delta = 0.1
tgrid = 0
simData = mod  %>% zero_re %>% param( COVARIATE=0, SCALING=0)  %>% 
  data_set(as.ev(adex)) %>% 
  mrgsim(end=sim_end, delta=delta, add=tgrid, tad=TRUE, carry.out=c("ii", "evid")) %>%  as.data.frame()
colnames(simData) = toupper(colnames(simData))

simData = simData  %>% mutate(DVOR = CENTRAL/V2) %>% 
  left_join(adex %>% select(ID, ARMA), by="ID")  %>% 
  filter(EVID==0)

simData$ARMA %>% unique()

 

#---------------------------------------- 
# priminary PK plot     
# ---------------------------------------
tdata = simData 

x=setup_axis(xscale='7_14', xlimits=c(0, max(tdata$TIME)))
fig = ggplot(data=tdata, aes(x=TIME, y=DVOR, group=ID, col=ARMA)) + geom_line() + 
  #facet_wrap(~POP) + 
  xlab("Time (day)") + 
  ylab("Predicted Conc. of Total REGN3918 (mg/L)") + 
  scale_color_manual(values=palette()) + 
  
  #coord_cartesian(xlim = c(0, 85)) + 
  theme_bw() + base_theme(font.size = as.integer(12)) + 
  
  scale_x_continuous(breaks=x$xbreaks, label=x$xlabels) +
  #scale_x_continuous(breaks=seq(0, 112, by=7), label=seq(0, 112, by=7)) +
  
  guides(col=guide_legend(ncol=2,byrow=TRUE))   + 
  
  ggplot2::theme(panel.grid.minor = element_line(colour = "gray95",size=0.75))   

attr(fig, 'title') <-   "Predicted Mean Concentration-time Profiles of REGN3918 for Potential Dosing Regimens"
attr(fig, 'width') <- 8
attr(fig, 'height') <- 6
FIGURE_ALL[["priminary_PK_plot"]]  = fig 


fig
 


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
    group_by(ID) %>% mutate(ARMA = paste0(ARMA0, collapse="+"))    %>% ungroup() %>% as.data.frame()
  
  adex$ARMA =  ordered(adex$ARMA, levels=unique(adex$ARMA))
  
  # set seed to make reproducible simulation (if IIV=1)
  set.seed(seed)
  
  infhr.lst = 1
  delta = 0.1
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



#c(60, 70, 80, 90, 100)

Dosing.Regimen = c("30 mg/kg Q1D*1 IV + 60 mg SC QD*140" , 
                   "30 mg/kg Q1D*1 IV + 70 mg SC QD*140" ,   
                   "30 mg/kg Q1D*1 IV + 80 mg SC QD*140",
                   "30 mg/kg Q1D*1 IV + 90 mg SC QD*140" , 
                   "30 mg/kg Q1D*1 IV + 100 mg SC QD*140" 
)


# define a population
nsubject = 1 
adsl = create_adsl(POP="A typical patient with 70kg", nsubject, meanWt=70, seed =seed) #%>% filter(WGTBL<160)
adsl$WGTBL = 70
simData_70kg_N1 = runCurrentSim(mod  %>% zero_re %>% param( COVARIATE=0, SCALING=0), 
                                adsl, Dosing.Regimen, seed)

tdata = simData_70kg_N1  %>% mutate(DVOR=CP)
#tdata = tdata %>% mutate(ARMA = gsub("Q1W*1 ", "", ARMA, fix=TRUE ))

x=setup_axis(xscale='7_14', xlimits=c(0, max(tdata$TIME)))

#---------------------------------------- 
# priminary PK plot     
# -----------------------------------
fig = ggplot(data=tdata, aes(x=TIME, y=DVOR, group=ID, col=ARMA)) + geom_line() + 
  facet_wrap(~POP) + 
  xlab("Time (day)") + 
  ylab("Predicted Conc. of Total REGN3918 (mg/L)") + 
  
  #coord_cartesian(xlim = c(0, 85)) + 
  scale_color_manual(values=palette()) + 
  theme_bw() + base_theme(font.size = as.integer(12)) + 
  
  scale_x_continuous(breaks=x$xbreaks, label=x$xlabels) +
  #scale_x_continuous(breaks=seq(0, 112, by=7), label=seq(0, 112, by=7)) +
  
  guides(col=guide_legend(ncol=2,byrow=TRUE))   + 
  
  ggplot2::theme(panel.grid.minor = element_line(colour = "gray98",size=0.75))   

attr(fig, 'title') <-   "Predicted Mean Concentration-time Profiles of REGN3918 for Potential Dosing Regimens"
FIGURE_ALL[["priminary_PK_plot"]]  = fig 


fig








