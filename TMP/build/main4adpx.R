

idebug = 0

#HOME =  "/data/BiCS_RDE_Development/shiny-server_development/pharmacometrics/FYANG/R1500_AngPTL3"
HOME = "~/FYANG/R3918_C5/"
#HOME = "C:/FYANG/R3918/"

setwd(HOME)

source(paste0(dirname(HOME), "/global.R"))
library(gridExtra)

source(paste0(HOME, "/KRM/build/build_adsl.R"))
source(paste0(HOME, "/KRM/build/build_adex.R"))
source(paste0(HOME, "/KRM/build/build_adpc.R"))
source(paste0(HOME, "/KRM/build/build_nmdat.R"))




#   # ---------------------------------------------------------------------------#
#   read raw data
#   # ---------------------------------------------------------------------------#
 

file.name = "./DATA/R3918-HV-1659/adsl.sas7bdat"
adsl.v0 = read_sas(file.name)

file.name = "./DATA/R3918-HV-1659/limspk.sas7bdat"
adpc.v0 = read_sas(file.name)

file.name = "./DATA/R3918-HV-1659/adex.sas7bdat"
adex.v0 = read_sas(file.name)

#file.name = "./DATA/R3918-HV-1659/Regeneron R3918-HV-1659 Audited Cummulative Unblinded 2017-2018 Finalv2.csv"
#adlb.v0 = read_csv(file.name)
 
file.name = "./DATA/R3918-HV-1659/adlbbm.sas7bdat"
adlb.v0 = read_sas(file.name)

##############################################################################
##############################################################################  
# customerize - adsl
##############################################################################
############################################################################## 

# Default check  STUDYID", "USUBJID", "WGTBL
#-----------------------------------------------------------------------------  
head(adsl.v0) %>% as.data.frame()

adsl = build_adsl(adsl.v0 %>% mutate(STUDYID = STUDYID,  # variable must have 
                                     USUBJID = USUBJID, # variable must have 
                                     WGTBL = WGTBL, # variable must have 
                                     
                                     ARMA = TRT01A,
                                     ARMAN = TRT01AN, 
                                     HGTBL = HGTBL))  

 
##############################################################################
##############################################################################  
# customerize - adpc
##############################################################################
############################################################################## 
head(adpc.v0) %>% as.data.frame()

# variable must have 

adpc = adpc.v0 %>% mutate(TIMEPT = gsub("MONTH ", "M", toupper(TIMEPT), fix=TRUE))

adpc = adpc %>% mutate(ARMA = paste(DOSEPLAN1, DOSU1, ROUTE1, sep=" "), 
                       ARMA = ifelse(ARMA %in% c("0 mg/kg IV", "0 mg SC"), "Placebo", ARMA), 
                       ARMA = ifelse(ARMA %in% c("400 mg SC"), "400 mg SC QW", ARMA)
)
adpc$ARMA %>% unique()

adpc = build_adpc(adpc %>% mutate(STUDYID = STUDYID,   
                                     USUBJID = SUBJID,  
                                     ARMA = ARMA, 
                                     VISIT = VISIT, 
                                     TIMEPT = TIMEPT, 
                                     TEST = SOP, 
                                     METHOD = SOP, 
                                     DVOR = as_numeric(RESC_RAW), 
                                     STDUNIT = STDUNIT, 
                                     SAMDTTM = SAMDTTM, 
                                     BLQ = BLQ, 
                                     LLOQ = as_numeric(LLOQ)/1000))

adpc$SAMDTTM

adpc = adpc %>% mutate(TEST = ifelse(SOP=="PCL4775", "C5", 
                                   ifelse(SOP=="PCL4730", "REGN3918", SOP))   
                       
                       
)

tdata = adpc %>% filter( TEST=="REGN3918") #%>% mutate(DVOR=as.numeric(DVOR)/1000)
ggplot(tdata, aes(x=NTIM, y=DVOR, group=USUBJID, col=ARMA)) + geom_point() + geom_line() + scale_y_log10() #+ 
  #coord_cartesian(xlim=c(0, 1000), ylim=c(1E-2, 1E+3))

tdata = adpc %>% filter( TEST=="C5") #%>% mutate(DVOR=as.numeric(DVOR)/1000)
ggplot(tdata, aes(x=NTIM, y=DVOR, group=USUBJID, col=ARMA)) + geom_point() + geom_line() 
#+ scale_y_log10() + 
#coord_cartesian(xlim=c(0, 1000), ylim=c(1E-2, 1E+3))


##############################################################################
##############################################################################  
# customerize - adex
##############################################################################
############################################################################## 
head(adex.v0) %>% as.data.frame()


adex = adex.v0  %>% filter(!is.na(EXSTDTC), EXSTDTC!="")

#adex = adex %>% mutate(EXTDOSE = ifelse(is.na(as_numeric(EXTDOSE))&EXROUTE=="SUBCUTANEOUS", EXDOSE, EXTDOSE))
adex = adex %>% mutate(EXTDOSE = EXDOSE)

adex = build_adex(adex %>% mutate(STUDYID = STUDYID,   
                                     USUBJID = SUBJID,  
                                     EXTRT = EXTRT, 
                                     EXDOSE = EXDOSE, 
                                     EXDOSU = EXDOSU,    # mg/kg  or mg
                                     EXROUTE = EXROUTE,  #   "SUBCUTANEOUS" "INTRAVENOUS" 
                                     EXSTDTC = EXSTDTC, 
                                     EXENDTC = EXENDTC, 
                                     
                                     ARMA = COHORT))
                                     
adex %>% select(USUBJID, EXSTDTC)%>% distinct(USUBJID, .keep_all=TRUE)

# WGTBL
if (!"WGTBL" %in% colnames(adex)) { 
  adex = adex %>% left_join(adsl%>% select(USUBJID, WGTBL)%>% distinct(USUBJID,.keep_all=TRUE), by="USUBJID")
}

# AMT, or EXTDOSE
if (!"EXTDOSE" %in% colnames(adex)) { 
  adex%>% pull(EXDOSU) %>% unique()
  adex = adex %>% mutate(EXTDOSE = ifelse(EXDOSU=="mg/kg", EXDOSE*WGTBL, EXDOSE))
}
 
# update ARMA from adpc
adex = adex %>% select(-ARMA) %>% left_join(adpc %>% select(USUBJID, ARMA)%>% distinct(USUBJID, .keep_all=TRUE), by="USUBJID")
adex$ARMA %>% unique()

adex %>% filter(EXTDOSE==0, ARMA !="Placebo")
##############################################################################
##############################################################################  
# customerize - adlb
##############################################################################
############################################################################## 



# use excel dataset sent from Ming-dauh
#-------------------------------------------
if (1==2) { 
  head(adlb.v0) %>% as.data.frame()
  
  
# Default check
#-----------------------------------------------------------------------------  
# variable must have 
adlb = adlb.v0 
colnames(adlb) = gsub(" ", ".", colnames(adlb), fix=TRUE)
head(adlb) %>% as.data.frame()
 
adlb = adlb %>% gather(TEST, DVOR, AH50, CH50 ) %>% 
                mutate(STUDYID = Protocol,   
                       USUBJID = Subject.ID,  
                       ARMA = ".", 
                       VISIT = Visit.Name, 
                       TIMEPT = Time.Point, 
                       TEST = TEST, 
                       METHOD = TEST, 
                       DVOR = as_numeric(DVOR), 
                       STDUNIT = "U/mL", 
                       SAMDTTM = paste0(Collection.Date, " ", Collection.Time),
                       BLQ = ".", 
                       LLOQ = ".")
 
adlb = build_adpc(adlb, date_time_format = "db!Y HMS" )
 
# get TIMEPT from VISIT
adlb$VISIT %>% unique()
adpc$VISIT %>% unique()

adlb = adlb   %>%
       left_join(adpc%>% select(VISIT, TIMEPT)%>% rename(DAY=TIMEPT)%>% distinct(VISIT, .keep_all=TRUE), 
                          by = "VISIT")  %>% 
       mutate(DAY = gsub(" PRE", "", DAY, fix=TRUE), 
              TIMEPT = gsub("EOI", "0HR", TIMEPT, fix=TRUE),
              TIMEPT = ifelse(is.na(TIMEPT), "", TIMEPT), 
              TIMEPT = gsub("PARENT VISIT", "", TIMEPT, fix=TRUE),
              TIMEPT = gsub("()", "", TIMEPT, fix=TRUE),
              TIMEPT = trim(paste0(DAY, " ", TIMEPT)), 
              TIMEPT = trim(gsub("DAY 22 PRE", "DAY 22", TIMEPT, fix=TRUE)))



# update ARMA from adpc
adlb = adlb %>% select(-ARMA) %>% left_join(adpc %>% select(USUBJID, ARMA)%>% distinct(USUBJID, .keep_all=TRUE), by="USUBJID")
adlb$ARMA %>% unique()


# re-update NTIM
unique(adlb$TIMEPT)
unique(adpc$TIMEPT)
setdiff(adlb$TIMEPT, adpc$TIMEPT)
tt = parseTIMEPT(adlb$TIMEPT %>% unique())
adlb = adlb %>% select(-NTIM) %>% left_join(tt %>% select(TIMEPT, NTIM), by="TIMEPT")

tdata = adlb %>% filter( TEST=="CH50") #%>% mutate(DVOR=as.numeric(DVOR)/1000)
ggplot(tdata, aes(x=NTIM, y=DVOR, group=USUBJID, col=ARMA)) + geom_point() + geom_line() #+ #scale_y_log10() #+ 
  #coord_cartesian(xlim=c(0, 1000), ylim=c(1E-2, 1E+3))

}


head(adlb.v0) %>% as.data.frame()

# Default check
#-----------------------------------------------------------------------------  
# variable must have 
adlb = adlb.v0 
colnames(adlb) = gsub(" ", ".", colnames(adlb), fix=TRUE)
head(adlb) %>% as.data.frame()

# TIMEPT
adlb$TIMEPT = paste0(adlb$AVISIT, " ",  adlb$ATPT)
adlb$TIMEPT = trim(gsub( "([^()]*)\\(([^()]*)\\)", "\\2 ", adlb$TIMEPT ) )
adlb$TIMEPT = gsub("BASELINE", "DAY 1 PRE", adlb$TIMEPT, fix=TRUE)
adlb$TIMEPT = gsub("EOI", "0HR", adlb$TIMEPT, fix=TRUE)

unique(adlb$TIMEPT)

adlb = adlb %>%  
  mutate(STUDYID = STUDYID,   
         USUBJID = USUBJID,  
         ARMA = TRTA, 
         VISIT = VISIT, 
         TIMEPT = TIMEPT, 
         TEST = PARAMCD , 
         METHOD = PARAMCD , 
         DVOR = as_numeric(LBORRES), 
         STDUNIT = LBORRESU, 
         SAMDTTM = LBDTC,
         BLQ = ".", 
         LLOQ = ".")

adlb = build_adpc(adlb )



 

# update ARMA from adpc
adlb = adlb %>% select(-ARMA) %>% left_join(adpc %>% select(USUBJID, ARMA)%>% distinct(USUBJID, .keep_all=TRUE), by="USUBJID")
adlb$ARMA %>% unique()

 
                                        #liposomic assay (U/mL)   CH50L
tdata = adlb %>% filter( TEST=="CH50H") # hemolytic assay (U/mL)   CH50H 
ggplot(tdata, aes(x=NTIM, y=DVOR, group=USUBJID, col=ARMA)) + geom_point() + geom_line() #+ #scale_y_log10() #+ 
#coord_cartesian(xlim=c(0, 1000), ylim=c(1E-2, 1E+3))

 



#-----------------------------------------------------------------------------
# calcualte TIME and then merge adpc, adex, adlb
#-----------------------------------------------------------------------------
# 1) make sure USUBJID must be consistent between adpc, adex, adlb
# 
 

require(VennDiagram)
a.list <- list(adsl=unique(as.character(adsl$USUBJID)), 
               adex=unique(as.character(adex$USUBJID)), 
               adpc=unique(as.character(adpc$USUBJID)))

venn.plot <- venn.diagram(a.list,fill = c("red", "green", "blue"),
                          alpha = c(0.5, 0.5, 0.5), cex = 2,cat.fontface = 4,lty =2, 
                          euler.d = FALSE, scaled = FALSE, #fontfamily =3, 
                          filename = NULL);
dev.off()
grid.draw(venn.plot);    
 
 
 
# 2) calculate TIME for adpc and adlb
adpc = adpc %>% left_join(adex %>% select(USUBJID, EXFIRST)%>% distinct(USUBJID, .keep_all=TRUE), by="USUBJID") %>% 
  mutate(TIME = difftime(SAMDTTM, EXFIRST, units="days") %>% as_numeric(), 
         SAMDTTM = as.character(SAMDTTM), 
         EXFIRST = as.character(EXFIRST)) 

adlb = adlb %>% left_join(adex %>% select(USUBJID, EXFIRST)%>% distinct(USUBJID, .keep_all=TRUE), by="USUBJID")  %>% 
  mutate(TIME = difftime(SAMDTTM, EXFIRST, units="days")%>% as_numeric(), 
         SAMDTTM = as.character(SAMDTTM), 
         EXFIRST = as.character(EXFIRST)) 


# 3) set "." if missing
col.lst.all = unique(c(colnames(adpc), colnames(adlb), colnames(adex)))
adpc[, setdiff(col.lst.all, colnames(adpc))] = "."
adlb[, setdiff(col.lst.all, colnames(adlb))] = "."
adex[, setdiff(col.lst.all, colnames(adex))] = "."

# merge all variables 
adpx = rbind(rbind(adpc[, col.lst.all]%>%as.data.frame(), 
                   adex[, col.lst.all]%>%as.data.frame()), 
                   adlb[, col.lst.all]%>%as.data.frame())  

adpx$ROWID = 1:nrow(adpx)
  
# adsl information #, c("WGTBL","AGE", "AGEU","SEX","SEXN","RACE","RACEN","ETHNIC","ETHNICN","HGTBL","BMIBL")))
#adsl = adsl %>% filter(is.na(WGTBL))
# col.lst = c("WGTBL","AGE", "AGEU","SEX","SEXN","RACE","RACEN","ETHNIC","ETHNICN","HGTBL","BMIBL")
# adpx = adpx %>% select(-one_of(col.lst)) %>% 
#                 left_join(adsl %>% distinct(USUBJID, .keep_all=TRUE)  %>% 
#                           select(one_of(c("USUBJID", col.lst))), by="USUBJID")
# 
adpx$ETHNICN = "."

col.lst = c("WGTBL","AGE", "AGEU","SEX","SEXN","RACE","RACEN","ETHNIC","ETHNICN","HGTBL","BMIBL")
tt = adpx %>% filter(WGTBL!=".") %>% distinct(USUBJID, .keep_all=TRUE) %>% 
  select(USUBJID, one_of(col.lst))

tt = tt %>% mutate(ETHNIC = toupper(ETHNIC), 
                       ETHNIC = ordered(ETHNIC, levels = ETHNIC.lst),
                       
                       ETHNICN = as.integer(ETHNIC),
                       ETHNICN = ifelse(is.na(ETHNICN), -99, ETHNICN) 
)
tt %>% as.data.frame()

adpx  = adpx %>% select(-one_of(col.lst)) %>% left_join(tt, by="USUBJID")

#-----------------------------------------------------------------------------
# add baseline C5 and AH50 and Ch50
#-----------------------------------------------------------------------------
tt = adpx %>% filter(TIMEPT %in% c("DAY 1 PRE"),  TEST %in% c("C5"), TIME<0) 
tt = tt %>% select(USUBJID, TEST, TIMEPT, DVOR) %>% spread(TEST, DVOR) %>% 
  rename(C5BL=C5)
adpx = adpx %>% left_join(tt %>% select(USUBJID, C5BL), by="USUBJID")


# NO BAESLINE FOR R3918-HV-1659-826-001-127
tt = adpx %>% filter(PARAMCD!=".", PARAMCD !="DOSED") %>% select(USUBJID, PARAMCD, BASE) %>% 
        distinct(USUBJID, PARAMCD, .keep_all=TRUE) %>% spread(PARAMCD, BASE) %>% 
       rename(AH50BL = AH50, CH50HBL=CH50H, CH50LBL=CH50L)

adpx = adpx %>% left_join(tt, by="USUBJID")

adpx0 = adpx
 

#-----------------------------------------------------------------------------
#save it to data folder
#-----------------------------------------------------------------------------
# R3918-HV-1659-826-001-001, randomized but not dosed
# NO BAESLINE CH50 and AH50 FOR R3918-HV-1659-826-001-127
adpx %>% filter(USUBJID %in% "R3918-HV-1659-826-001-001")  %>% select(USUBJID, TIME, NTIM, TIMEPT, TEST, DVOR, EXDOSE)

adpx %>% filter(USUBJID %in% "R3918-HV-1659-826-001-127")  %>% select(USUBJID, TIME, NTIM, TIMEPT, TEST, DVOR, EXDOSE)




#liposomic assay (U/mL)   CH50L
#tdata = adlb %>% filter( TEST=="CH50H") # hemolytic assay (U/mL)   CH50H 
adpx = adpx0
col.lst = c(adpx.col.lst, "C5BL", "AH50BL",  "CH50HBL",  "CH50LBL", "BASE", "CHG", "PCHG")
setdiff(col.lst, colnames(adpx))
adpx = adpx[, col.lst] 


tdata = adpx %>% filter(TEST %in% c("REGN3918", "C5", "CH50H", "CH50L", "AH50",  "."   ))

write_csv(tdata, "./KRM/data/adpx.R3918.HV.1659.csv")
write_csv(adsl, "./KRM/data/adsl.R3918.HV.1659.csv")
write_csv(adpc, "./KRM/data/adpc.R3918.HV.1659.csv")
write_csv(adex, "./KRM/data/adex.R3918.HV.1659.csv")
write_csv(adlb, "./KRM/data/adlb.R3918.HV.1659.csv")


