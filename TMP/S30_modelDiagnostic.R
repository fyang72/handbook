#----------------------------------------------------------------------------
# Version 0.1   Created on 11/08/2018, Feng Yang
# 
#----------------------------------------------------------------------------
# Input
# -----------------
# 1) xpdb = sdtab + patab + nmdat  
#
# Output Tables: 
#------------------
# 1) accounting table for subject, sample 
# 2) summary table for PK, target and PD response by ARMA
#----------------------------------------------------------------------------
# 

##########################################################################
# load xpdb
##########################################################################


if (idebug ==1) {
  FIGURE_ALL = NULL
  TABLE_ALL = NULL
}
 
# load it automatically
#--------------------------
#tt = read_runno(MODEL.HOME=paste0(HOME, "/MODEL/"), subdir="ctl/", runno="MM001")
#tt = read_runno(MODEL.HOME=paste0(HOME, "/MODEL/"), subdir="ctl/", runno="MM001_PKPD_V6") 

nmdat = read_csv("./data/nmdat_PKPD_1024_2018.csv", 
                 col_type=cols(.default=col_character())) %>%   # read as character as defualt
  mutate(ROWID = as.integer(ROWID), 
         ID = as.integer(ID), 
         ARMA = gsub("_", " ", ARMA, fix=TRUE),
         ARMA = ordered(ARMA, levels=unique(as.character(ARMA))), 
         ARMAN = as.integer(ARMA), 
         DVOR = as_numeric(DVOR), 
         TIME = as_numeric(TIME), 
         NTIM = as_numeric(NTIM),
         
         LLOQ = as_numeric(LLOQ)
  )%>% 
  filter(as_numeric(TIME)>=0)   # Do we want to exclude all pre-dose sampels, as the default?


sdtab = read.table("./ctl/MM_PKPD_CV/sdtab001", skip=1, header=TRUE)
patab = read.table("./ctl/MM_PKPD_CV/patab001", skip=1, header=TRUE)

# merge with patab
xpdb = sdtab %>%  
  left_join(patab%>% select(-DV, -PRED, -RES, -WRES) %>% distinct(ID, .keep_all=TRUE), by="ID")

# merge with nmdat
xpdb = xpdb %>% filter(MDV!=1)  %>% 
  left_join(nmdat[, c("ROWID", "C", "CFLAG", "USUBJID", "ARMA", "VISIT", "TIMEPT", "TEST", "DVOR", 
                     "WGTBL", "C5BL", "CH50HBL")], by= "ROWID")  
xpdb = xpdb %>% mutate(DVOR = as_numeric(DVOR), 
                       ARMA = gsub("_", " ", ARMA, fix=TRUE)
)

# ARMA
xpdb$ARMA %>% unique()
arma.lst = c("1 mg/kg IV",   "3 mg/kg IV",   "10 mg/kg IV",  "30 mg/kg IV",  
             "300 mg SC",    "600 mg SC",   "400 mg SC QW")
xpdb = xpdb %>% mutate(ARMA = ordered(ARMA, levels =arma.lst))

# Convert back concentration if needed
xpdb = xpdb  %>% 
  mutate(DV = ifelse(TEST==PK.TEST.NAME, exp(DV), DV), 
         DVOR = DV, 
         PRED = ifelse(TEST==PK.TEST.NAME, exp(PRED), PRED), 
         IPRED = ifelse(TEST==PK.TEST.NAME, exp(IPRED), IPRED)
  ) 

xpdb = xpdb %>% mutate(IPRED = ifelse(TEST==PK.TEST.NAME&IPRED<0.078, 0.078/2, IPRED), 
                       PRED = ifelse(TEST==PK.TEST.NAME&PRED<0.078, 0.078/2, PRED)
)






nmdat = read_csv("./data/nmdat_PKPD_1024_2018.csv", 
                 col_type=cols(.default=col_character()))

# remove outliers
# -------------------------------
cut.point = 3
rowid.lst = xpdb %>% filter(abs(CWRES)>cut.point) %>% pull(ROWID)
print(rowid.lst)

nmdat = nmdat %>% mutate(C = ifelse(ROWID %in% rowid.lst, "C", C), 
                         CFLAG = ifelse(ROWID %in% rowid.lst, paste0("CWRES>", cut.point), CFLAG)
)





tdata = nmdat  #%>% filter(USUBJID %in% c("R3918-HV-1659-826-001-052",  "R3918-HV-1659-826-001-050", "R3918-HV-1659-826-001-075"))
write_csv(tdata, "./data/nmdat_PK_1024_2018_test.csv")

# 
# >R MATRIX ALGORITHMICALLY SINGULAR 
# >COVARIANCE MATRIX UNOBTAINABLE 

# >R MATRIX IS OUTPUT 
# >T MATRIX - EQUAL TO RS*R, WHERE S* IS THE INVERSE OF S - IS OUTPUT 
# > 
# https://cognigencorp.com/nonmem/current/2009-September/1112.html
# re-run the model
# -------------------------------
#nmdat %>% filter(ROWID %in% rowid.lst)

system(command = "scp   ./ctl/LN_BASE_WT/LN_BASE_WT.ctl    10.244.106.127:/home/feng.yang/R3918/ctl/LN_BASE_WT/", intern = T)
system(command = "scp   ./data/nmdat_PK_1024_2018_test.csv    10.244.106.127:/home/feng.yang/R3918/data/", intern = T)

txt = system(command = "ssh 10.244.106.127 'cd /home/feng.yang/R3918/ctl/LN_BASE_WT/; execute  LN_BASE_WT.ctl  -clean=4'", intern = T)
system(command = "scp   10.244.106.127:/home/feng.yang/R3918/ctl/LN_BASE_WT/LN_BASE_WT.lst   ./ctl/LN_BASE_WT/")
system(command = "scp   10.244.106.127:/home/feng.yang/R3918/ctl/LN_BASE_WT/LN_BASE_WT.ext   ./ctl/LN_BASE_WT/")

# fetch the results  
# -------------------------------
directory = "~/FYANG/R3918_C5/KRM/ctl/LN_BASE_WT/"
runno = "LN_BASE_WT"
lst = read.lst(paste0(directory, runno, ".lst")  )
lst

 





##########################################################################################
# Standard Model Diagnostic
##########################################################################################
fig = empty_canvas(label="Standard\n Model Diagnostic")
attr(fig, 'title') <-  ""
FIGURE_ALL[["canvas_std_diagnostic"]] = fig 

 
#-------------------------------------------------
# Predicted/observed PK.DVOR.PRED.IPRED  (PK ONLY)
#-------------------------------------------------
 
tdata = xpdb   %>% 
  filter(TEST==PK.TEST.NAME)  %>% 
  mutate(DV = exp(DV), 
         DVOR = DV, 
         PRED = exp(PRED), 
         IPRED = exp(IPRED))

tdata = tdata %>% mutate(IPRED = ifelse(TEST==PK.TEST.NAME&IPRED<0.078, 0.078, IPRED), 
                       PRED = ifelse(TEST==PK.TEST.NAME&PRED<0.078, 0.078, PRED)
)

# t_PRED  = DVOR.PRED.IPRED(tdata, x="DVOR", y="PRED",  arma ="ARMA", id="USUBJID", legend.nrow=3)
# t_IPRED = DVOR.PRED.IPRED(tdata, x="DVOR", y="IPRED",   arma ="ARMA", id="USUBJID", legend.nrow=3)
# 
# 
# fig = grid.arrange(t_PRED$fig_LN + ggplot2::theme(legend.position = "none") , 
#                    t_PRED$fig_LOG + ggplot2::theme(legend.position = "none") , 
#                    t_IPRED$fig_LN, 
#                    t_IPRED$fig_LOG,  
#                    nrow = 2)

#xlab("Observed Concentration of Total REGN3918 (mg/L)") + 
#ylab("Population Predicted Concentration of Total REGN3918 (mg/L)") +   
#grid::grid.draw(fig)

tdata = xpdb %>% filter(TEST==PK.TEST.NAME)

fig <- GOF1(tdata)     
fig <- recordPlot()
attr(fig, 'title') <- "Predicted vs Observed Concentration in Linear/Log Scale" 
attr(fig, 'width') <- 8
attr(fig, 'height') <- 8
FIGURE_ALL[["PK_GOF1"]] = fig 
 
#
fig = GOF2(tdata) 
fig = recordPlot()
attr(fig, 'title') <- "Conditional Weighted Residuals of REGN3918 vs Time, IPRED and ID from the Final Model" 
attr(fig, 'width') <- 8
attr(fig, 'height') <- 8
FIGURE_ALL[["PK_GOF2"]]  = fig


fig = GOF3(tdata)  + scale_y_log10() 
attr(fig, 'title') <- "Representative Model Fitting of Predicted/Observed Concentration-time Profiles" 
attr(fig, 'width') <- 8
attr(fig, 'height') <- 8
FIGURE_ALL[["PK_GOF3"]]  = fig
 


#-------------------------------------------------
# Predicted/observed time profiles for PK only
#-------------------------------------------------

fig = ggplot()  + 
  geom_line(data=tdata%>%filter(TEST==PK.TEST.NAME), aes(x=TIME, y=PRED, col="PRED", group=USUBJID)) +  
  geom_line(data=tdata%>%filter(TEST==PK.TEST.NAME), aes(x=TIME, y=IPRED, col="IPRED", group=USUBJID)) + 
  geom_point(data = nmdat%>%filter(TEST==PK.TEST.NAME, TIME>=0) , aes(x=as_numeric(TIME), y=as_numeric(DVOR)))  +  
  
  
  xlab("Time (day)") + 
  ylab("Population Predicted/observed Concentration") + 
  
  #coord_cartesian(xlim = c(0, 85)) + 
  theme_bw() + base_theme(font.size = as.integer(12)) + 
  guides(col=guide_legend(ncol=4,byrow=TRUE))    


# in linear scale
# --------------------------
fig_LN = fig
#attr(fig_LN, 'title') <-  "Population Predicted Time Profiles of REGN3918 in Linear Scale"
#FIGURE_ALL[["PK_PRED_TIME_LN"]] = fig_LN 

fig_LN = fig_LN + facet_wrap(~ARMA, scales="free") #+ ggplot2::theme(legend.position = "none")
attr(fig_LN, 'title') <- paste0("Population Predicted Time Profiles of ", PK.TEST.LABEL, " in Linear Scale")
attr(fig_LN, 'width') <- 8
attr(fig_LN, 'height') <- 6
FIGURE_ALL[["PK_PRED_TIME_LN_PANEL"]] = fig_LN  


# In log-scale
# --------------------------
fig_LOG = fig 
fig_LOG = fig_LOG + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^round(x, digits=0)), labels = trans_format("log10", math_format(10^.x))) + 
  annotation_logticks(sides ="l")
# attr(fig_LOG, 'title') <-  "Population Predicted Time Profiles of REGN3918 in Log Scale"
# FIGURE_ALL[["PK_PRED_TIME_LOG"]] = fig_LOG 


fig_LOG = fig_LOG + facet_wrap(~ARMA) #+ ggplot2::theme(legend.position = "none")
attr(fig_LOG, 'title') <- paste0("Population Predicted Time Profiles of ", PK.TEST.LABEL, " in Log Scale")
attr(fig_LOG, 'width') <- 8
attr(fig_LOG, 'height') <- 6
FIGURE_ALL[["PK_PRED_TIME_LOG_PANEL"]] = fig_LOG 

FIGURE_ALL[["PK_PRED_TIME_LOG_PANEL"]]




#########################################################
#########################################################
# diagnostic for PD 
#########################################################
#########################################################


tdata = xpdb   %>%   filter(TEST==PD.TEST.NAME) %>% 
  mutate(DVOR = ifelse(as_numeric(DVOR)==0, 0.01, as_numeric(DVOR)), 
         DV = ifelse(as_numeric(DV)==0, 0.01, as_numeric(DVOR)), 
         PRED = ifelse(as_numeric(PRED)==0, 0.01, as_numeric(PRED)), 
         IPRED = ifelse(as_numeric(IPRED)==0, 0.01, as_numeric(IPRED))
  )
         
# 
# t_PRED  = DVOR.PRED.IPRED(tdata, x="DVOR", y="PRED",  arma ="ARMA", id="USUBJID", legend.nrow=3)
# t_IPRED = DVOR.PRED.IPRED(tdata, x="DVOR", y="IPRED",  arma ="ARMA", id="USUBJID", legend.nrow=3)
# 
# 
# library(gridExtra)
# fig = arrangeGrob(t_PRED$fig_LN + ggplot2::theme(legend.position = "none") , 
#                   t_PRED$fig_LOG + ggplot2::theme(legend.position = "none") , 
#                   t_IPRED$fig_LN, 
#                   t_IPRED$fig_LOG,  
#                   nrow = 2)
# 
# 
# #xlab("Observed Concentration of Total REGN3918 (mg/L)") + 
# #ylab("Population Predicted Concentration of Total REGN3918 (mg/L)") +  
# grid::grid.draw(fig)

fig <- GOF1(tdata)     
fig <- recordPlot()
attr(fig, 'title') <- paste0("Predicted vs Observed ", PD.TEST.LABEL, " in Linear/Log Scale")
attr(fig, 'width') <- 8
attr(fig, 'height') <- 8
FIGURE_ALL[["PD_GOF1"]] = fig 

#
fig = GOF2(tdata)
fig = recordPlot()
attr(fig, 'title') <- paste0("Conditional Weighted Residuals of ", PD.TEST.LABEL, " vs Time, IPRED and ID from the Final Model")
attr(fig, 'width') <- 8
attr(fig, 'height') <- 8
FIGURE_ALL[["PD_GOF2"]]  = fig


fig = GOF3(tdata)  #+ scale_y_log10() 
attr(fig, 'title') <- "Representative Model Fitting of Predicted/Observed Concentration-time Profiles" 
attr(fig, 'width') <- 8
attr(fig, 'height') <- 8
FIGURE_ALL[["PD_GOF3"]]  = fig



#-------------------------------------------------
# Predicted/observed time profiles for PD only
#-------------------------------------------------
fig = ggplot()  + 
  geom_line(data=tdata, aes(x=TIME, y=PRED, col="PRED", group=USUBJID)) +  
  geom_line(data=tdata, aes(x=TIME, y=IPRED, col="IPRED", group=USUBJID)) + 
  geom_point(data =tdata , aes(x=TIME, y=DVOR))  +  
  
  #geom_point(data = nmdat%>%filter(TEST=="LDL") , aes(x=as_numeric(TIME), y=as_numeric(DVOR)))  +  
  
  xlab("Time (day)") + 
  ylab(paste0("Population Predicted/observed ", PD.TEST.LABEL)) +  #LDL(mg/dL)") + 
  
  #coord_cartesian(xlim = c(0, 85)) + 
  theme_bw() + base_theme(font.size = as.integer(12)) + 
  guides(col=guide_legend(ncol=4,byrow=TRUE))    


# in linear scale
# --------------------------
fig_LN = fig
# attr(fig_LN, 'title') <-  "Population Predicted Time Profiles of LDL (mg/dL) in Linear Scale"
# FIGURE_ALL[["PD_PRED_TIME_LN"]] = fig_LN 

fig_LN = fig_LN + facet_wrap(~ARMA, scales="free") #+ ggplot2::theme(legend.position = "none")
attr(fig_LN, 'title') <- paste0("Population Predicted Time Profiles of ", PD.TEST.LABEL, " in Linear Scale")
attr(fig_LN, 'width') <- 8
attr(fig_LN, 'height') <- 6
FIGURE_ALL[["PD_PRED_TIME_LN_PANEL"]] = fig_LN  

# 
# # In log-scale
# # --------------------------
# fig_LOG = fig 
# 
# fig_LOG = fig_LOG + 
#   scale_y_log10(breaks = trans_breaks("log10", function(x) 10^round(x, digits=0)), labels =   trans_format("log10", math_format(10^.x))) + 
#   annotation_logticks(sides ="l")
# attr(fig_LOG, 'title') <-  "Population Predicted Time Profiles of REGN3918 in Log Scale"
# FIGURE_ALL[["PK_PRED_TIME_LOG"]] = fig_LOG 
# 
# 
# fig_LOG = fig_LOG + facet_wrap(~USUBJID) + ggplot2::theme(legend.position = "none")
# attr(fig_LOG, 'title') <- "Population Predicted Time Profiles of REGN3918 in Log Scale" 
# attr(fig_LOG, 'width') <- 8
# attr(fig_LOG, 'height') <- 6
# FIGURE_ALL[["PK_PRED_TIME_LOG_PANEL"]] = fig_LOG 
# 
# FIGURE_ALL[["PK_PRED_TIME_LOG_PANEL"]]


  


 

################################################################################################
# Scatter Plot of a Few Key PK and PD Parameters vs Covaraite (WGTBL, C5BL, CHH50BL)  
################################################################################################
#

tdata = xpdb %>% distinct(USUBJID, .keep_all=TRUE) 
tdata = tdata %>% mutate(MAX_PCHG = -as_numeric(EMAX)/as_numeric(CH50HBL)*100)

tdata %>% select(USUBJID, EMAX, CH50HBL, MAX_PCHG)
  

FIG = ETAvsCOV(tdata, cov_name=c( "WGTBL", "C5BL", "CH50HBL"), 
               eta_name_lst =c("CL", "V2", "VMAX",  "KSS", "EC50",  "GAMMA", "MAX_PCHG"))

fig = FIG$WGTBL
attr(fig, 'title') <- paste0("Scatter Plot of a Few Key PK and PD Parameters vs Baseline Body Weight (kg) in ",  STUDY.NAME)
attr(fig, 'width') <- 8
attr(fig, 'height') <- 6
FIGURE_ALL[["WGTBL_ON_PARAMS"]] = fig

fig = FIG$C5BL
attr(fig, 'title') <- paste0("Scatter Plot of a Few Key PK and PD Parameters vs Baseline Total C5 (mg/L) in ",  STUDY.NAME)
attr(fig, 'width') <- 8
attr(fig, 'height') <- 6
FIGURE_ALL[["C5BL_ON_PARAMS"]] = fig


fig = FIG$CH50HBL
attr(fig, 'title') <- paste0("Scatter Plot of a Few Key PK and PD Parameters vs Baseline CH50 (U/mL) in ",  STUDY.NAME)
attr(fig, 'width') <- 8
attr(fig, 'height') <- 6
FIGURE_ALL[["CH50HBL_ON_PARAMS"]] = fig 
 


# 

tdata = xpdb %>% rename(ETA_CL=ETA1, 
                       ETA_V2=ETA2, 
                       ETA_KA=ETA3,
                       ETA_VMAX=ETA4,
                       ETA_KSS=ETA5,
                       ETA_EC50=ETA6, 
                       ETA_GAMMA=ETA7, 
                       ETA_EMAX=ETA8
                      )
tt = ETAvsCOV(tdata, 
         cov_name_lst=c("WGTBL", "C5BL", "CH50HBL"), 
         eta_name_lst=c("ETA_CL", "ETA_V2", "ETA_KA", "ETA_VMAX", "ETA_KSS", "ETA_EC50", "ETA_GAMMA", "ETA_EMAX"    ))
 
 
fig = tt$WGTBL + theme(panel.border = element_rect(colour = "black", fill=NA, size=1))
attr(fig, 'title') <- "Baseline Body Weight as Covariate Impacting on Model Parameters in the Final Model"
attr(fig, 'width') <- 8
attr(fig, 'height') <- 6
FIGURE_ALL[["WGTBL_ON_ETA"]]  = fig 

fig = tt$C5BL + theme(panel.border = element_rect(colour = "black", fill=NA, size=1))
attr(fig, 'title') <- "Baseline C5 as Covariate Impacting on Model Parameters in the Final Model"
attr(fig, 'width') <- 8
attr(fig, 'height') <- 6
FIGURE_ALL[["C5BL_ON_ETA"]]  = fig 

fig = tt$CH50HBL + theme(panel.border = element_rect(colour = "black", fill=NA, size=1))
attr(fig, 'title') <- "Baseline CH50 as Covariate Impacting on Model Parameters in the Final Model"
attr(fig, 'width') <- 8
attr(fig, 'height') <- 6
FIGURE_ALL[["CH50HBL_ON_ETA"]]  = fig 



fig = ggplot(tdata, aes(x=C5BL , y=CH50HBL)) + geom_point() + 
  xlab("Baseline C5BL (mg/L)") + 
  ylab("Baseline CH50 (U/mL)") + 
  #coord_cartesian(xlim = c(0, 85)) + 
  theme_bw() + base_theme(font.size = as.integer(12))  + 
  geom_smooth(method = "lm", size = 1.5)

fig = add_lmFit(fig)

attr(fig, 'title') <- "Baseline CH50 (U/mL) vs Baseline C5 (mg/L)"
FIGURE_ALL[["CH50HBL_VS_C5BL"]]  = fig 







if (idebug ==1) { 
  
  # use customized version
  myppt <- pptx(title = "title", template = '~/FYANG/LIB/pptTemplate.pptx')
  mydoc <- docx(template = "~/FYANG/LIB/memoTemplate.docx", empty_template = FALSE)
  
  tt = print2_word_ppt(FIGURE_ALL, TABLE_ALL,  mydoc, myppt) 
  
  writeDoc(tt$mydoc, file = './KRM/docs/memo_results.docx')
  
  writeDoc(tt$myppt, file = './KRM/docs/ppt_results.pptx')
  
}


