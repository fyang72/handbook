
```{r SIMDATA2, eval=TRUE }  

setwd("H:\\Pharmacometrics\\r2810-poppk\\")
source("_common.R")
```
 


```{r SIMDATA2, eval=FALSE }  

# Use to simulated time-profiles for 350mg 
# ---------------------------------------------

#tabl = read_PARAMS(MODEL.HOME="H:\\FYANG\\R2810_PD1\\MODEL\\", subdir="/ctl/HPC/",  runno.lst=final.model.runno)
#get_summary_params(tabl, final.model.runno)




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
out = mod %>%  param( COVARIATE=1, SCALING=0) %>%  
#omat(CLQ_VSS=dmat(0,0), IIV_EMAX=dmat(0), IIV_T50=dmat(0)) %>%  
ev(as.ev(adex)) %>% mrgsim(end=sim_end, delta=1, add=tgrid) %>% as.data.frame() #%>% capitalize_names()

out = out %>% left_join(adex %>% as.data.frame() %>% select(ID, POP, STUDYID, USUBJID, ARMA), by="ID") 
colnames(out) = toupper(colnames(out))

OUT= rbind(OUT, cbind(replicate=ireplicate, out))
}


######################################
Predict350<- OUT %>% mutate(DVOR=IPRED)
save(Predict350, file="./data/Predict350.Rdata")
######################################

```




```{r PREDICTION-3mkg-350mg }


plot.Prediction <- function(tdata, 
                    group_by=c("STUDYID", "ARMA", "TIME"), 
                    ARMA.txt=c("3 mg/kg Q2W IV"), 
                    xlab.txt="Time (day)",
                    ylab.txt="Concentration (mg/L)" ,
                    xlim = c(0,168), 
                    ylim = c(0, 300), 
                    major.tick=28, minor.tick=7, 
                    font.size = 14
)  {
tdata = tdata %>% mutate(Mean=ifelse(Mean==0, 0.039, Mean))
tdata = tdata %>% mutate(Median=ifelse(Median==0, 0.039, Median))
tdata = tdata %>% mutate(PCT2P5=ifelse(PCT2P5==0, 0.039, PCT2P5))
tdata = tdata %>% mutate(PCT97P5=ifelse(PCT97P5==0, 0.039, PCT97P5))

tdata = tdata%>% filter(ARMA==ARMA.txt)  # "3 mg/kg Q2W IV")
x = tick_label(x=as_numeric(tdata$TIME), xmin=0, xmax=max(tdata$TIME), major.tick=major.tick, minor.tick=minor.tick, log=FALSE)
y = tick_label(x=as_numeric(tdata$PCT97P5), xmin=1E-2, xmax=1E+3,  log=TRUE) 


fig = ggplot(tdata, aes(x=TIME, y=Median)) + 
geom_line(col="black", show.legend=TRUE  ) +
geom_ribbon(aes(ymin=PCT2P5,ymax=PCT97P5), fill="gray50", alpha="0.5", show.legend=F)   + 
scale_fill_manual(values=c(clear,blue)) + 

xlab(xlab.txt) + 
ylab(ylab.txt) + 

base_theme(font.size=font.size) + 
scale_x_continuous(breaks=x$breaks, label=as.character(x$labels)) + 


coord_cartesian(xlim =xlim) + 
coord_cartesian(ylim = ylim) + 

theme(#legend.title=element_text("EXSEQ"), 
#legend.position=c(0.90, 0.85), 
legend.position= "bottom", 
panel.grid.minor= element_line(colour = "gray95",size=0.5),
panel.grid.major= element_line(colour = "gray95",size=0.5),
panel.spacing = unit(0.2, "lines"), 
panel.border=element_rect(colour="black", fill=NA), 
strip.background = element_blank()   # element_rect(colour=NA, fill=NA),
)   

return(fig)
}


load("./data/Predict350.Rdata")



group_by = c("STUDYID", "ARMA", "TIME")
tdata = Predict350 %>% calc_stats(id="USUBJID", group_by=group_by, value="DVOR")  %>%
select(STUDYID, ARMA, TIME, N, Mean, Median, PCT2P5, PCT97P5) #%>% 
#mutate(replicate=ireplicate)

# c("3 mg/kg Q2W IV"), 
#-----------------------------
fig = plot.Prediction(tdata, 
              group_by=c("STUDYID", "ARMA", "TIME"), 
              ARMA.txt=c("3 mg/kg Q2W IV"), 
              xlab.txt="Time (day)",
              ylab.txt="Concentration (mg/L)" ,
              xlim = c(0,168), 
              ylim = c(0.039, 2000),
              major.tick=28, minor.tick=7, 
              font.size = 14
)  


FIGURE_ALL[["PREDICTION_3mkg_LN"]] = fig
y = tick_label(x=as_numeric(tdata$PCT97P5), xmin=1E-2, xmax=1E+3,  log=TRUE) 
fig = fig + scale_y_log10(breaks=y$breaks, label=as.character(y$labels)) # +  coord_cartesian(ylim = c(0.039,300)) 
FIGURE_ALL[["PREDICTION_3mkg_SemiLog"]] = fig

#c("350 mg Q3W IV"), 
#-----------------------------
fig = plot.Prediction(tdata, 
              group_by=c("STUDYID", "ARMA", "TIME"), 
              ARMA.txt=c("350 mg Q3W IV"), 
              xlab.txt="Time (day)",
              ylab.txt="Concentration (mg/L)" ,
              xlim = c(0,168), 
              ylim = c(0.039, 2000),
              major.tick=28, minor.tick=7, 
              font.size = 14
)  
FIGURE_ALL[["PREDICTION_350mg_LN"]] = fig
y = tick_label(x=as_numeric(tdata$PCT97P5), xmin=1E-2, xmax=1E+3,  log=TRUE) 
fig = fig + scale_y_log10(breaks=y$breaks, label=as.character(y$labels)) # +  coord_cartesian(ylim = c(0.039,300)) 
FIGURE_ALL[["PREDICTION_350mg_SemiLog"]] = fig




# c("3 mg/kg Q2W IV"), weekly axis
#-----------------------------
fig = plot.Prediction(tdata %>% mutate(TIME=TIME/7), 
              group_by=c("STUDYID", "ARMA", "TIME"), 
              ARMA.txt=c("3 mg/kg Q2W IV"), 
              xlab.txt="Time (Week)",
              ylab.txt="Concentration (mg/L)" ,
              xlim = c(0, 24), 
              ylim = c(0.039, 2000),
              major.tick=4, minor.tick=1, 
              font.size = 14
)  


FIGURE_ALL[["PREDICTION_3mkg_LN_Weekly"]] = fig
y = tick_label(x=as_numeric(tdata$PCT97P5), xmin=1E-2, xmax=1E+3,  log=TRUE) 
fig = fig + scale_y_log10(breaks=y$breaks, label=as.character(y$labels)) # +  coord_cartesian(ylim = c(0.039,300)) 
FIGURE_ALL[["PREDICTION_3mkg_SemiLog_Weekly"]] = fig

#c("350 mg Q3W IV"), , weekly axis
#-----------------------------
fig = plot.Prediction(tdata %>% mutate(TIME=TIME/7), 
              group_by=c("STUDYID", "ARMA", "TIME"), 
              ARMA.txt=c("350 mg Q3W IV"), 
              xlab.txt="Time (Week)",
              ylab.txt="Concentration (mg/L)" ,
              xlim = c(0,24), 
              ylim = c(0.039, 2000),
              major.tick=4, minor.tick=1, 
              font.size = 14
)  
FIGURE_ALL[["PREDICTION_350mg_LN_Weekly"]] = fig
y = tick_label(x=as_numeric(tdata$PCT97P5), xmin=1E-2, xmax=1E+3,  log=TRUE) 
fig = fig + scale_y_log10(breaks=y$breaks, label=as.character(y$labels)) # +  coord_cartesian(ylim = c(0.039,300)) 
FIGURE_ALL[["PREDICTION_350mg_SemiLog_Weekly"]] = fig






#adpc = read_sas("H:\\FYANG\\R2810_PD1\\MODEL\\data\\GRP3\\pkgrp3.sas7bdat")   ############ old group 3
adpc = read_sas("./data/pkgrp3.sas7bdat")   ############  03/20/2018


adpc = adpc %>% mutate(NTIM=as_numeric(NTIM), DVOR=as_numeric(PCSTRESC))
adpc = adpc %>% mutate(PCTPT=ifelse(PCTPT=="", VISIT, PCTPT))
adpc$PCTPT = gsub("PREINFUSION", "Preinfusion", adpc$PCTPT, fix=TRUE)
adpc$PCTPT = gsub("END OF INFUSION", "End of Infusion", adpc$PCTPT, fix=TRUE)
adpc$PCTPT = ordered(adpc$PCTPT, levels= c("Preinfusion", "End of Infusion", "EOS"))     


fig = FIGURE_ALL[["PREDICTION_350mg_LN"]] 
FIGURE_ALL[["PREDICTION_350mg_LN_Overlay"]]  = fig +   geom_point(data=adpc, aes(x=NTIM, y=DVOR,col=PCTPT))

fig = FIGURE_ALL[["PREDICTION_350mg_SemiLog"]]     
FIGURE_ALL[["PREDICTION_350mg_SemiLog_Overlay"]]  = fig +   geom_point(data=adpc, aes(x=NTIM, y=DVOR,col=PCTPT))



# WEEKLY
fig = FIGURE_ALL[["PREDICTION_350mg_LN_Weekly"]]  
FIGURE_ALL[["PREDICTION_350mg_LN_Weekly_Overlay"]]  = fig +   geom_point(data=adpc, aes(x=NTIM/7, y=DVOR,col=PCTPT))


fig = FIGURE_ALL[["PREDICTION_350mg_SemiLog_Weekly"]]
FIGURE_ALL[["PREDICTION_350mg_SemiLog_Weekly_Overlay"]]  = fig +   
geom_point(data=adpc %>% mutate(DVOR=ifelse(DVOR==0, 0.039, DVOR)), 
     aes(x=NTIM/7, y=DVOR,col=PCTPT)) + 
geom_hline(yintercept = 0.078, lty="dashed")












```







```{r  }


plot.Prediction.Data <- function(tdata, 
                         group_by=c("STUDYID", "ARMA", "TIME"), 
                         ARMA.txt=c("3 mg/kg Q2W IV"), 
                         xlab.txt="Time (day)",
                         ylab.txt="Concentration (mg/L)" ,
                         xlim8 = c(0,168), 
                         ylim8 = c(0, 300), 
                         major.tick=28, minor.tick=7, 
                         font.size = 14
)  {


#tdata = tdata%>% filter(ARMA==ARMA.txt)  # "3 mg/kg Q2W IV")
x = tick_label(x=as_numeric(tdata$TIME), xmin=0, xmax=max(xlim8), major.tick=major.tick, minor.tick=minor.tick, log=FALSE)
y = tick_label(x=as_numeric(tdata$DVOR), xmin=1E-2, xmax=1E+3,  log=TRUE) 


fig = ggplot(tdata, aes(x=TIME, y=DVOR, group=USUBJID, col=factor(PCTPT))) + 
# geom_line(col="black") + 
geom_point() +
#geom_ribbon(aes(ymin=PCT2P5,ymax=PCT97P5), fill="gray50", alpha="0.5")   + 
#scale_fill_manual(values=c(clear,blue)) + 

xlab(xlab.txt) + 
ylab(ylab.txt) + 

base_theme(font.size=font.size) + 
scale_x_continuous(limits = xlim8, breaks=x$breaks, label=as.character(x$labels)) + 
#scale_x_continuous() +  #   removes/extend all data points outside the given range 
coord_cartesian(xlim =xlim8) +            #  only adjusts the visible area
coord_cartesian(ylim = ylim8) + 

theme(#legend.title=element_text("EXSEQ"), 
#legend.position=c(0.90, 0.85), 
legend.position= "none", 
panel.grid.minor= element_line(colour = "gray95",size=0.5),
panel.grid.major= element_line(colour = "gray95",size=0.5),
panel.spacing = unit(0.2, "lines"), 
panel.border=element_rect(colour="black", fill=NA), 
strip.background = element_blank()   # element_rect(colour=NA, fill=NA),
)   

return(fig)
}





adpc = read_sas("H:\\FYANG\\R2810_PD1\\MODEL\\data\\GRP3\\pkgrp3.sas7bdat")
adpc = read_sas("./data/pkgrp3.sas7bdat")   ############  03/20/2018

adpc = adpc %>% mutate(NTIM=as_numeric(NTIM), 
               TIME=as_numeric(NTIM),
               DVOR=as_numeric(PCSTRESC))
adpc = adpc %>% mutate(PCTPT=ifelse(PCTPT=="", VISIT, PCTPT))
adpc$PCTPT = gsub("PREINFUSION", "Preinfusion", adpc$PCTPT, fix=TRUE)
adpc$PCTPT = gsub("END OF INFUSION", "End of Infusion", adpc$PCTPT, fix=TRUE)
adpc$PCTPT = ordered(adpc$PCTPT, levels= c("Preinfusion", "End of Infusion", "EOS"))     



tdata = adpc


#c("350 mg Q3W IV"), daily
#-----------------------------
fig = plot.Prediction.Data(tdata,  
                   group_by=c("STUDYID", "ARMA", "TIME"), 
                   ARMA.txt=c("350 mg Q3W IV"), 
                   xlab.txt="Time (day)",
                   ylab.txt="Concentration (mg/L)" ,
                   xlim = c(0,168), 
                   ylim = c(0.039, 2000),
                   major.tick=28, minor.tick=7, 
                   font.size = 14
)  
FIGURE_ALL[["PREDICTION_350mg_LN_DATA"]] = fig
y = tick_label(x=as_numeric(tdata$DVOR), xmin=1E-2, xmax=1E+3,  log=TRUE) 
fig = fig + scale_y_log10(breaks=y$breaks, label=as.character(y$labels)) # +  coord_cartesian(ylim = c(0.039,300)) 
FIGURE_ALL[["PREDICTION_350mg_SemiLog_DATA"]] = fig




#c("350 mg Q3W IV"), , weekly axis
#-----------------------------
fig = plot.Prediction.Data(tdata %>% mutate(TIME=TIME/7) %>% mutate(DVOR=ifelse(DVOR==0, 0.039, DVOR)),
                   group_by=c("STUDYID", "ARMA", "TIME"), 
                   ARMA.txt=c("350 mg Q3W IV"), 
                   xlab.txt="Time (Week)",
                   ylab.txt="Concentration (mg/L)" ,
                   xlim = c(0,24), 
                   ylim = c(0.01, 2000), 
                   major.tick=4, minor.tick=1, 
                   font.size = 14
)  + 
geom_hline(yintercept = 0.078, lty="dashed")
y = tick_label(x=as_numeric(tdata$DVOR), xmin=1E-2, xmax=2E+3,  log=TRUE) 
fig = fig + scale_y_log10(breaks=y$breaks, label=as.character(y$labels)) # +  coord_cartesian(ylim = c(0.039,300)) 
FIGURE_ALL[["PREDICTION_350mg_SemiLog_Weekly_DATA"]] = fig



fig = plot.Prediction.Data(tdata %>% mutate(TIME=TIME/7),  #  %>% mutate(DVOR=ifelse(DVOR==0, 0.039, DVOR)),
                   group_by=c("STUDYID", "ARMA", "TIME"), 
                   ARMA.txt=c("350 mg Q3W IV"), 
                   xlab.txt="Time (Week)",
                   ylab.txt="Concentration (mg/L)" ,
                   xlim = c(0,24), 
                   ylim = c(0.0, 2000), 
                   major.tick=4, minor.tick=1, 
                   font.size = 14
) # + geom_hline(yintercept = 0.078, lty="dashed")
#y = tick_label(x=as_numeric(tdata$DVOR), xmin=1E-2, xmax=2E+3,  log=TRUE) 
#fig = fig + scale_y_log10(breaks=y$breaks, label=as.character(y$labels)) # +  coord_cartesian(ylim = c(0.039,300)) 
FIGURE_ALL[["PREDICTION_350mg_LN_Weekly_DATA"]] = fig










```

















                                  
                                  
                                  
                                                                                                                      
                                                                                                                      
                                                                                                                      
                                                                                                                      
                                                                                                                      
                                                                                                                      
                                                                                                                      
                                                                                                                      
                                                                                                                      
                                                                                                                      
                                                                                                                      
                                                                                                                      