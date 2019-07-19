
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
source("./PKreport/cpp/mod_R3918_PKPD_TMDD.cpp")
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



# define a population
nsubject = 1 
adsl = create_adsl(POP="A typical patient with 70kg", nsubject, meanWt=70, seed =seed) #%>% filter(WGTBL<160)
adsl$WGTBL = 70
simData_70kg_N1 = runCurrentSim(mod  %>% zero_re %>% param( COVARIATE=1, SCALING=0), 
                                adsl, Dosing.Regimen, seed)




tdata = simData_70kg_N1  %>% mutate(C_total = CENTRAL/V2, 
                                    C_free = CONC1,  
                                    MASS_BALANCE_C = C_total - (C_free + RC + R2C), 
                                    
                                    R_total = TARGET, 
                                    R_free = R, 
                                    MASS_BALANCE_TARGET = R_total - (R_free + RC + 2*R2C), 
                                    
                                    DVOR=C_total)
tdata = tdata %>% mutate(ARMA = gsub("Q1W*1 ", "", ARMA, fix=TRUE ))


tdata = tdata %>% select(USUBJID, ID, TIME, ARMA, POP, C_total, C_free, R_free, RC, R2C, R_total, CH50) %>%  
  gather(key= TEST, value=DVOR, -ID, -USUBJID, -TIME, -ARMA, -POP)  %>% 
  arrange(USUBJID, TEST, TIME)


x=setup_axis(xscale='7_14', xlimits=c(0, 250))

#tdata = tdata %>% filter(ARMA == "42 mg/kg IV+800 mg SC QW*16" )

#tdata = tdata %>% filter(ARMA == "18 mg/kg IV+400 mg SC QW*16"  )


tdata = tdata %>% filter(ARMA == "800 mg SC QW*16"  )
 

 
tdata = tdata %>% mutate(ID_TEST = paste0(ID, "_", TEST), 
                         TEST = ordered(TEST, levels= c("C_free", "C_total", "R_free", "R_total", "RC", "R2C", "CH50")))
#---------------------------------------- 
# priminary PK plot     
# -----------------------------------

fig = ggplot(data=tdata, aes(x=TIME, y=DVOR, group=ID_TEST, col=TEST)) + geom_line() + 
  facet_wrap(~POP) + 
  xlab("Time (day)") + 
  ylab("Predicted Concentration") + 
  
  #coord_cartesian(xlim = c(0, 85)) + 
  theme_bw() + base_theme(font.size = as.integer(12)) + 
   scale_color_manual(values = gg_color_hue(n=10) ) + 
  
  scale_x_continuous(breaks=x$xbreaks, label=x$xlabels) +
  #scale_x_continuous(breaks=seq(0, 112, by=7), label=seq(0, 112, by=7)) +
  
  guides(col=guide_legend(ncol=4,byrow=TRUE))   + 
  
  ggplot2::theme(panel.grid.minor = element_line(colour = "gray98",size=0.75))   



fig = fig +  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^round(x, digits=0)),
                           labels =   trans_format("log10", math_format(10^.x))) 
fig = fig + annotation_logticks(sides ="l") +  facet_wrap(~ARMA) + # "trbl", for top, right, bottom, and left.
  theme(panel.grid.minor = element_blank())  #+ 
 # ggplot2::theme(legend.position = "none")

attr(fig, 'title') <-   "Predicted Mean Concentration-time Profiles of REGN3918 for Potential Dosing Regimens"
attr(fig, 'width') <- 8
attr(fig, 'height') <- 6
FIGURE_ALL[["priminary_PK_plot"]]  = fig  




fig


# priminary PD plot     
# -----------------------------------

tdata = simData_70kg_N1  %>% mutate(CH50_PCHG = (CH50-CH50_BL)/CH50_BL*100) %>% mutate(DVOR=CH50_PCHG)
x=setup_axis(xscale='7_14', xlimits=c(0, 200))

fig = ggplot(data=tdata, aes(x=TIME, y=DVOR, group=USUBJID, col=ARMA)) + geom_line() + 
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
nsubject = 1 
adsl_5 = create_adsl(POP="A typical patient with 5kg",   nsubject, meanWt=5, seed =seed) %>% mutate(WGTBL=5)
adsl_10 = create_adsl(POP="A typical patient with 10kg", nsubject, meanWt=10, seed =seed) %>% mutate(WGTBL=10)
adsl_20 = create_adsl(POP="A typical patient with 20kg", nsubject, meanWt=10, seed =seed) %>% mutate(WGTBL=20)
adsl_30 = create_adsl(POP="A typical patient with 30kg", nsubject, meanWt=10, seed =seed) %>% mutate(WGTBL=30)
adsl = rbind(adsl_5, adsl_10, adsl_20, adsl_30)


simData_70kg_N1 = runCurrentSim(mod  %>% zero_re %>% param( COVARIATE=0, SCALING=1), # , WGT_ON_CLQ=0, WGT_ON_VSS=0 ), 
                                adsl, Dosing.Regimen, seed)

tdata = simData_70kg_N1  %>% mutate(DVOR=CP)
tdata = tdata %>% mutate(ARMA = gsub("Q1W*1 ", "", ARMA, fix=TRUE ))

x=setup_axis(xscale='7_28', xlimits=c(0, 250))

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
x=setup_axis(xscale='7_28', xlimits=c(0, 250))

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
# Plot 95% CI time profiles  of both PK and PD
################################################################################################
Dosing.Regimen = c("10 mg/kg Q1W*1 IV + 300 mg SC QW*4" , 
                   "15 mg/kg Q1W*1 IV + 400 mg SC QW*4" ,   
                   "25 mg/kg Q1W*1 IV + 600 mg SC QW*4",
                   "30 mg/kg Q1W*1 IV + 800 mg SC QW*4",
                   "35 mg/kg Q1W*1 IV + 800 mg SC QW*4"
)

nsubject = 1000
adsl = create_adsl(POP="A typical patient with 70kg", nsubject, meanWt=70, seed =seed) #%>% filter(WGTBL<160)
simData_70kg_N1000 = runCurrentSim(mod  %>% param( COVARIATE=1, SCALING=0),      #%>% zero_re
                                   adsl, Dosing.Regimen, seed)


simData_70kg_N1000 = simData_70kg_N1000 %>% mutate(ARMA = gsub("Q1W*1 ", "", ARMA, fix=TRUE ))




# PK 
tdata = simData_70kg_N1000   %>% mutate(DVOR = CP, NTIM=TIME)

tdata = tdata %>% calc_stats(id="USUBJID", group_by=c("ARMA", "NTIM"), value="DVOR") 


#---------------------------------------------------------------------------------------------------
x=setup_axis(xscale='7_14', xlimits=c(0, 112))

fig = ggplot(tdata , aes(x=NTIM, y=Mean, group=ARMA, col=ARMA)) + 
  # geom_point() + 
  geom_line() + 
  #geom_errorbar(aes(ymin = meanMinusSE, ymax = meanPlusSE), width=2)  + 
  geom_ribbon(data=tdata,aes(ymin=PCT2P5,ymax=PCT97P5),alpha=0.3) + 
  
  #facet_wrap(~TEST, nrow=3, scales="free" ) + 
  scale_x_continuous(breaks=x$xbreaks, label=x$xlabels) +
  #ggtitle("Concentration Time Profile") + 
  xlab("Time (day)") + 
  ylab("Mean Concentration of Total REGN3918 (mg/L)") + 
  #scale_y_log10() + 
  #coord_cartesian(xlim = c(0, 85)) + 
  theme_bw() + 
  #scale_colour_manual(values = c("black", "blue", "green")) + 
  
  ggplot2::theme(legend.position = "bottom", legend.title = element_blank()) + 
  base_theme(font.size = as.integer(12)) + 
  guides(col=guide_legend(ncol=4,byrow=TRUE)) +  
  
  ggplot2::theme(  #panel.grid.minor = element_line(colour = "gray95",size=0.75), 
    panel.grid.major = element_line(colour = "gray97",size=0.75)  )  



fig = fig +    facet_wrap(~ARMA)  + ggplot2::theme(legend.position = "none")
attr(fig, 'title') <- "Predicted Mean Concentration-Time Proifiles (95% CI, Linear-scale) for Potential Dosing Regimens" 
attr(fig, 'width') <- 8
attr(fig, 'height') <- 6
FIGURE_ALL[["PK_TIME_PROFILE_CI95_LN"]]  = fig 


fig = fig +  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^round(x, digits=0)),
                           labels =   trans_format("log10", math_format(10^.x))) 
fig = fig + annotation_logticks(sides ="l") +  facet_wrap(~ARMA) + # "trbl", for top, right, bottom, and left.
  theme(panel.grid.minor = element_blank()) + ggplot2::theme(legend.position = "none")

attr(fig, 'title') <-  "Predicted Mean Concentration-Time Proifiles (95% CI, Log-scale) for Potential Dosing Regimens"
attr(fig, 'width') <- 8
attr(fig, 'height') <- 6
FIGURE_ALL[["PK_TIME_PROFILE_CI95_LOG"]]  = fig 

#------------------------------
# PD
#------------------------------

tdata = simData_70kg_N1000   %>% mutate(DVOR = (CH50-CH50_BL)/CH50_BL*100, NTIM=TIME)

tdata = tdata %>% calc_stats(id="USUBJID", group_by=c("ARMA", "NTIM"), value="DVOR") 

x=setup_axis(xscale='7_14', xlimits=c(0, 112))

fig = ggplot(tdata , aes(x=NTIM, y=Mean, group=ARMA, col=ARMA)) + 
  # geom_point() + 
  geom_line() + 
  #geom_errorbar(aes(ymin = meanMinusSE, ymax = meanPlusSE), width=2)  + 
  geom_ribbon(data=tdata,aes(ymin=PCT2P5,ymax=PCT97P5),alpha=0.3) + 
  
  #facet_wrap(~TEST, nrow=3, scales="free" ) + 
  scale_x_continuous(breaks=x$xbreaks, label=x$xlabels) +
  #ggtitle("Concentration Time Profile") + 
  xlab("Time (day)") + 
  ylab("Mean Percent Change from Baseline in CH50") + 
  #scale_y_log10() + 
  #coord_cartesian(xlim = c(0, 85)) + 
  theme_bw() + 
  #scale_colour_manual(values = c("black", "blue", "green")) + 
  
  ggplot2::theme(legend.position = "bottom", legend.title = element_blank()) + 
  base_theme(font.size = as.integer(12)) + 
  guides(col=guide_legend(ncol=4,byrow=TRUE)) +  
  
  ggplot2::theme(  #panel.grid.minor = element_line(colour = "gray95",size=0.75), 
    panel.grid.major = element_line(colour = "gray97",size=0.75)  )   

#scale_y_log10(breaks = trans_breaks("log10", function(x) 10^round(x, digits=0)),
#              labels =   trans_format("log10", math_format(10^.x))
#) 

#fig = fig + annotation_logticks(sides ="l") +  facet_wrap(~ARMA) + # "trbl", for top, right, bottom, and left.
#  theme(panel.grid.minor = element_blank())
fig = fig  +facet_wrap(~ARMA) + ggplot2::theme(legend.position = "none")
attr(fig, 'title') <-  "Predicted Mean Concentration-Time Proifiles (95% CI) for Potential Dosing Regimens"
attr(fig, 'width') <- 8
attr(fig, 'height') <- 6
FIGURE_ALL[["CH50PCT_TIME_PROFILE_CI95"]]  = fig 






################################################################################################
# calcualte individual exposure metrics
################################################################################################

# PK 
tdata = simData_70kg_N1000   %>% filter(TIME==35)  %>% mutate(DVOR = CP)
tabl = tdata  %>%  calc_stats(id="USUBJID", group_by=c("POP", "ARMA"), value="DVOR") %>% 
  select(group_by, N, Mean_SE, Mean_SD, Median_Range)

attr(tabl, 'title') <- "Summary of Concentration for Different Dosing Regimens at Day 35"
TABLE_ALL[["Summary_PK_Day35"]]  = tabl


# CH50_PCHG
tdata = simData_70kg_N1000   %>% filter(TIME==35)  %>% mutate( CH50_PCHG = (CH50-CH50_BL)/CH50_BL*100, 
                                                               DVOR = CH50_PCHG)
tabl = tdata  %>%  calc_stats(id="USUBJID", group_by=c("POP", "ARMA"), value="DVOR") %>% 
  select(group_by, N, Mean_SE, Mean_SD, Median_Range)

attr(tabl, 'title') <- "Summary of Percentage of Change from Baseline (CH50) for Different Dosing Regimens at Day 35" 
TABLE_ALL[["Summary_CH50_PCHG_Day35"]]  = tabl



# filter out the last dosing interval
#tdata = tdata  %>%  
# group_by_(.dots =c(group_by )) %>%  
# top_n(n=1,wt=EXSEQ) %>% filter(TAD<=II)
#

# calculate the PK parameter for each subject and each time interval
# out = tdata  %>% 
#   group_by_(.dots =c(group_by )) %>%   
#   dplyr::summarise(
#     #CMIN = round(min(DVOR),digits=3),    # Note the infusion hours, could be 5 min to 2 hr, typically.
#     CMIN = round(DVOR[n()],digits=3),   # the last conc at this time interval
#     CMAX = round(max(DVOR),digits=3),       
#     AUCtau = round(auc_partial(NTIM, DVOR),digits=3)    # , data%>%pull(input$yvar))   #, range=data%>%range_(input$xvar)
#   )  






################################################################################################
# calculate the percentage of patient above conc threshold at treatmetn period
################################################################################################
tdata = simData_70kg_N1000 

tdata = tdata %>% mutate(CH50_PCHG = (CH50-CH50_BL)/CH50_BL*100)

tdata = tdata %>% filter(TIME==35)  %>% 
  mutate(CONC_GT_100 = ifelse(CP>=100, 1, 0), 
         CONC_GT_150 = ifelse(CP>=150, 1, 0), 
         CONC_GT_200 = ifelse(CP>=200, 1, 0),
         
         CH50_EC99 = ifelse(CH50_PCHG<(-99), 1, 0),
         CH50_EC95 = ifelse(CH50_PCHG<(-95), 1, 0), 
         CH50_EC90 = ifelse(CH50_PCHG<(-90), 1, 0), 
         CH50_EC80 = ifelse(CH50_PCHG<(-80), 1, 0)
  )

# percentage of population above theshold concentration  
tabl = tdata %>% group_by(ARMA) %>% summarise(N=length(unique(USUBJID)), 
                                              PCT_CONC_GT_100 = paste0(round(sum(CONC_GT_100)/N*100,digits=2),"%"),
                                              PCT_CONC_GT_150 = paste0(round(sum(CONC_GT_150)/N*100,digits=2),"%"),
                                              PCT_CONC_GT_200 = paste0(round(sum(CONC_GT_200)/N*100,digits=2),"%")
)


attr(tabl, 'title') <- "Percentage of Subject Achieving threshold Concentration for Different Dosing Regimens at Day 35" 
TABLE_ALL[["CONC_PCT_Day35"]] = tabl

tabl 

# percentage of population achieving EC99
tabl = tdata %>% group_by(ARMA) %>% summarise(N=length(unique(USUBJID)), 
                                              PCT_CH50_EC99 = paste0(round(sum(CH50_EC99)/N*100,digits=2),"%"),
                                              PCT_CH50_EC95 = paste0(round(sum(CH50_EC95)/N*100,digits=2),"%"),
                                              PCT_CH50_EC90 = paste0(round(sum(CH50_EC90)/N*100,digits=2),"%"), 
                                              PCT_CH50_EC80 = paste0(round(sum(CH50_EC80)/N*100,digits=2),"%")
)

attr(tabl, 'title') <-"Percentage of Subject Achieving EC80 to EC99 for Different Dosing Regimens at Day 35" 

TABLE_ALL[["CH50_PCT_Day35"]] = tabl



tdata %>% group_by(ARMA, CH50_EC99) %>% summarise(N=length(unique(USUBJID)), 
                                                  Mean=mean(CP)) %>% filter(CH50_EC99==1) %>% as.data.frame()


FIGURE_ALL[["tab_pk_parameters"]]  = fig


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





