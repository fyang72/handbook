



##########################################################################################
# canvas_simulation_results
##########################################################################################
fig = empty_canvas(label="Loading Dose \nfor Cohort 5")
attr(fig, 'title') <-  ""

FIGURE_ALL[["canvas_loading_dose_issue"]] = fig 





#########################################################
# Cohort 5, loading dose 15 mg/kg or 25 mg/kg? 
#########################################################
# 
adpx = read_csv("./PKreport/data/nmdat_PK_CH50_cohort5_25mg_IV_removeOutliers.csv")  

tdata = nmdat %>% filter(ARMA == "15mkg IV+400 mg SC") %>% as.data.frame() 
ggplot(tdata, aes(x=as_numeric(TIME), y=as_numeric(DVOR),group=USUBJID,label=as_numeric(ID) )) +
  geom_point() + geom_line() + geom_text()









# merge with  
tdata = adpx %>% mutate(DVOR = as_numeric(DVOR), 
                        ARMA = gsub("-", " ", ARMA, fix=TRUE)
)

arma.lst = c("1 mg/kg IV",   "3 mg/kg IV",   "10 mg/kg IV",  "30 mg/kg IV",  "300 mg SC",    "600 mg SC",    "400 mg SC qw")
tdata = tdata %>% mutate(ARMA = ordered(ARMA, levels =arma.lst))


tdata = tdata %>% filter(TESTN==1) %>% 
  mutate(TIME=as.numeric(TIME), DVOR=as.numeric(DVOR)) %>% 
  #mutate(ARMA = gsub("-", " ", ARMA, fixed=TRUE)) %>% 
  filter(ARMA %in% c("1 mg/kg IV", "3 mg/kg IV", "10 mg/kg IV", "30 mg/kg IV", "400 mg SC qw"))

fig = ggplot(tdata, aes(x=TIME, y=DVOR, col=ARMA, group=USUBJID))  + geom_line() + #geom_point() + 
  geom_point(data = tdata, aes(x=TIME, y=DVOR))  +  
  
  
  xlab("Time (day)") + 
  ylab("Observed Concentration of Total REGN3918 (mg/L)") +  
  
  coord_cartesian(xlim = c(0, 7)) + 
  
  theme_bw() + base_theme(font.size = as.integer(12)) + 
  guides(col=guide_legend(ncol=4,byrow=TRUE))  


fig = fig  +   scale_y_log10(breaks = trans_breaks("log10", function(x) 10^round(x, digits=0)),
                             labels =   trans_format("log10", math_format(10^.x))
) +
  coord_cartesian(xlim = c(0, 3.), ylim = c(8, 1E+3))   #+ facet_wrap(~ARMA)

fig = fig + annotation_logticks(sides ="l") +  # "trbl", for top, right, bottom, and left.
  theme(panel.grid.minor = element_blank())
fig

attr(fig, 'title') <- "Confirmation Needed for the Loading Dose of Cohort 5"  
FIGURE_ALL[["Cohort5_15mg_25mg"]] = fig 



#----------------------------------------  
# run a quick simulation
#----------------------------------------
seed = 1234
set.seed(seed)

nsubject = 1 
adsl = create_adsl(POP="70kg", nsubject, meanWt=70,   seed =seed) #%>% filter(WGTBL<160)
adsl$WGTBL = 70

Dosing.Regimen = c("10 mg/kg Q1W*1 IV + 400 mg SC QW*4",
                   "15 mg/kg Q1W*1 IV + 400 mg SC QW*4" , 
                   "20 mg/kg Q1W*1 IV + 400 mg SC QW*4" ,   
                   "25 mg/kg Q1W*1 IV + 400 mg SC QW*4", 
                   "30 mg/kg Q1W*1 IV + 400 mg SC QW*4"
)

simData = runCurrentSim(mod  %>% zero_re %>% param( COVARIATE=1, SCALING=0),
                        adsl, Dosing.Regimen, seed)   

#---------------------------------------- 
# priminary plot     
# ---------------------------------------
tdata = simData %>%  mutate(DVOR = CP)
x=setup_axis(xscale='7_14', xlimits=c(0, 112))


fig = ggplot(data=tdata, aes(x=TIME, y=DVOR, group=ID, col=ARMA)) + geom_line() + 
  facet_wrap(~POP) + 
  xlab("Time (day)") + 
  ylab("Predicted Concentration of Total REGN3918 (mg/L)") + 
  
  coord_cartesian(xlim = c(0, 58)) + 
  theme_bw() + base_theme(font.size = as.integer(12)) + 
  scale_x_continuous(breaks=x$xbreaks, label=x$xlabels) +
  
  geom_point(data=xpdb%>% filter(TESTN==1, ARMA =="15mkg IV+400 mg SC"), aes(x=TIME, y=DVOR, group=USUBJID), col="blue" ) + 
  geom_line(data=xpdb%>% filter( TESTN==1, ARMA =="15mkg IV+400 mg SC"), aes(x=TIME, y=DVOR, group=USUBJID), col="blue" ) + 
  
  #scale_y_log10() + 
  guides(col=guide_legend(ncol=2,byrow=TRUE))  

fig

attr(fig, 'title') <-"Comparison Between Predicted Profiles Against Observed Data for Cohort 5"
FIGURE_ALL[["issues_with_loading_dose"]]  = fig 

 






