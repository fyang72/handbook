
###################################
# setup
###################################
# some variables used in dose regimen

if (idebug ==1) {
  FIGURE_ALL = NULL
  TABLE_ALL = NULL
}
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

mg = 1; mkg = 2
SC = 1;  IV = 2


# load mrgsolve cpp model
#-------------------------------------
library(dplyr)
library("mrgsolve")
source("./PKreport/cpp/mod_ALNY_human.cpp")

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
  
  # set seed to make reproducible simulation (if IIV=1)
  set.seed(seed)
  
  infhr.lst = 1
  delta = 1
  followUpPeriod = 2880  #days
  
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

Dosing.Regimen = c("300 mg SC QW*16" , 
                   "400 mg SC QW*16" ,   
                   "600 mg SC QW*16",
                   "800 mg SC QW*16"
)


Dosing.Regimen = c("50 mg SC QW*1" , 
                   "100 mg SC QW*1" ,   
                   "500 mg SC QW*1",
                   "900 mg SC QW*1"
)




# define a population
nsubject = 1 
adsl = create_adsl(POP="", nsubject, meanWt=70, seed =seed) #%>% filter(WGTBL<160)
adsl$WGTBL = 5
simData_70kg_N1 = runCurrentSim(mod  %>% zero_re %>% param( COVARIATE=0, SCALING=0), 
                                adsl, Dosing.Regimen, seed)

tdata = simData_70kg_N1
#  
# tdata = tdata %>% mutate(ARMA = gsub("Q1W*1 ", "", ARMA, fix=TRUE ))
# 
# 
# tdata = tdata %>% select(USUBJID, ID, TIME, ARMA, POP, C_total, C_free, R_free, RC, R2C, R_total, CH50) %>%  
#   gather(key= TEST, value=DVOR, -ID, -USUBJID, -TIME, -ARMA, -POP)  %>% 
#   arrange(USUBJID, TEST, TIME)


x=setup_axis(xscale='7_28', xlimits=c(0, 100))

#tdata = tdata %>% filter(ARMA == "42 mg/kg IV+800 mg SC QW*16" )

#tdata = tdata %>% filter(ARMA == "18 mg/kg IV+400 mg SC QW*16"  )


#tdata = tdata %>% filter(ARMA == "800 mg SC QW*16"  )
 

 
#tdata = tdata %>% mutate(ID_TEST = paste0(ID, "_", TEST), 
#                         TEST = ordered(TEST, levels= c("C_free", "C_total", "R_free", "R_total", "RC", "R2C", "CH50")))


tdata = tdata %>% mutate(DVOR = E, 
                         TIME = TIME/24,
                         ARMA = ordered(ARMA, levels=Dosing.Regimen)
) %>% filter(DVOR!=0)


#---------------------------------------- 
# priminary PK plot     
# -----------------------------------

fig = ggplot(data=tdata, aes(x=TIME, y=DVOR, group=ID, col=ARMA)) + geom_line() + 
  facet_wrap(~POP) + 
  xlab("Time (days)") + 
  ylab("Predicted Concentration (ug/mL)") + 
  
  #coord_cartesian(xlim = c(0, 85)) + 
 
  theme_bw() + base_theme(font.size = as.integer(12)) + 
   scale_color_manual(values = palette() ) + 
  
  #scale_x_continuous(breaks=x$xbreaks, label=x$xlabels) +
  #scale_x_continuous(breaks=seq(0, 112, by=7), label=seq(0, 112, by=7)) +
  
  guides(col=guide_legend(ncol=4,byrow=TRUE))   + 
  
  ggplot2::theme(panel.grid.minor = element_line(colour = "gray98",size=0.75))   

 fig_LN = fig

 fig = fig_LN +  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^round(x, digits=0)),
                            labels =   trans_format("log10", math_format(10^.x))) 
 fig = fig + annotation_logticks(sides ="l") #+  facet_wrap(~ARMA) + # "trbl", for top, right, bottom, and left.
   theme(panel.grid.minor = element_blank())  #+ 
  # ggplot2::theme(legend.position = "none")

fig = fig +  coord_cartesian(ylim = c(1, 120)) 

attr(fig, 'title') <-   "Predicted Mean Concentration-time Profiles of REGN3918 for Potential Dosing Regimens"
attr(fig, 'width') <- 8
attr(fig, 'height') <- 6
FIGURE_ALL[["priminary_PK_plot"]]  = fig  


fig


fig_LN




 

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





