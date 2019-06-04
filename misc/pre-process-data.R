

adsl.var.lst = c("STUDYID", "USUBJID",  
                 "WGTBL", 
                 "HGTBL", 
                 "AGE", "AGEU",  
                 "SEX", "SEXN", 
                 "RACE",  "RACEN", 
                 "ETHNIC","ETHNICN", 
                 "BMIBL", "BSABL", "SITEID", 
                 "SAFFL", "ENRLFL","RANDFL","PKFL","COMPLFL" )

adsl = read_sas("./data/adsl.sas7bdat")

tdata = adsl %>%  
  select(STUDYID, USUBJID, 
         AGE,  AGEU, SEX, SEXN,  RACE, RACEN,  ETHNIC,  ETHNICN, 
         HGTBL, WGTBL, BMIBL,
         FASFL, SAFFL, COMPLFL, RANDFL, ENRLFL
  )   # %>% as.data.frame() %>% head()

tdata = tdata %>% mutate(STUDYID="STUDY-ID", 
                         USUBJID = gsub("R2222-HV-1326-", "", USUBJID, fix=TRUE)
                         #USUBJID = add_prefix(as.integer(as.factor(USUBJID)), digits=3) 
                         #TRTA = gsub("REGN2222 ", "", TRTA, fix=TRUE), 
                         #EXTRT = "DRUGNAME"
)

tdata = tdata %>% mutate(USUBJID = gsub("840", "442", USUBJID, fix=TRUE)) 

tdata = tdata[, intersect(adsl.var.lst , colnames(tdata))]
tdata %>% as.data.frame() %>% head()

adsl = tdata
write_csv(tdata, path="./data/adsl.csv")






adex.var.lst = c("STUDYID",  "USUBJID",  "ARMA", "ARMAN", "VISIT",    "VISITNUM",     "TIME",   
                 "EXTRT", "EXSEQ",  "EXDOSE",   "EXDOSU",  "EXTDOSE",  "EXROUTE", "EXROUTN", "EXDUR" ,  
                 "EXSTDTC",  "EXENDTC", "TRTSDTM")

adex = read_sas("./data/adex.sas7bdat")

tdata = adex %>% 
  mutate(EXSTDTC = paste0(EXSTDT, "T", EXSTTM), 
         EXENDTC = paste0(EXSTDT, "T", EXENTM)
  ) %>% 
  select(STUDYID, USUBJID, 
TRTA, TRTAN,  VISITNUM,  VISIT,  
EXDOSE, EXDOSU, EXSTDTC,   EXENDTC)   # %>% as.data.frame() %>% head()

tdata = tdata %>% mutate(STUDYID="STUDY-ID", 
                         USUBJID = gsub("R2222-HV-1326-", "", USUBJID, fix=TRUE), 
                         #ID = add_prefix(as.integer(as.factor(USUBJID)), digits=3), 
                         ARMA = gsub("REGN2222 ", "", TRTA, fix=TRUE), 
                         EXTRT = "DRUGNAME"
                         )
  
tdata$EXROUTE = NA
tdata$EXROUTE[which(regexpr("IV", tdata$ARMA) >0)] = "IV"
tdata$EXROUTE[which(regexpr("IM", tdata$ARMA) >0)] = "IM"
 
tdata = tdata %>% mutate(EXROUTN=ifelse(EXROUTE=="IV", 2, 1), 
                         EXROUTE=ifelse(EXROUTE=="IM", "SC", EXROUTE), 
                         ARMA = gsub("IM", "SC", ARMA, fix=TRUE), 
                         ARMA = gsub("30", "11", ARMA, fix=TRUE),
                         ARMA = gsub("3", "1", ARMA, fix=TRUE),
                         ARMA = gsub("10", "3", ARMA, fix=TRUE),
                         
                         EXSTDTC = gsub("2014", "1972", EXSTDTC, fix=TRUE), 
                         EXSTDTC = gsub("2015", "1973", EXSTDTC, fix=TRUE), 
                         EXENDTC = gsub("2014", "1972", EXENDTC, fix=TRUE),
                         EXENDTC = gsub("2015", "1973", EXENDTC, fix=TRUE), 
                         
                         EXDOSE = ifelse(EXDOSU=="mL", EXDOSE*150, EXDOSE), 
                         EXDOSU = ifelse(EXDOSU=="mL", "mg", EXDOSU) 
)

tdata = tdata %>% mutate(USUBJID = gsub("840", "442", USUBJID, fix=TRUE)) 

tdata = tdata[, intersect(adex.var.lst , colnames(tdata))]
tdata %>% as.data.frame() %>% head()
adex = tdata
write_csv(tdata, path="./data/adex.csv")

 





adpc.var.lst <- c(
  "STUDYID",   "USUBJID", "ARMA",  "ARMAN",
  "VISIT",   "VISITNUM",   "PCTPT", "TIME", "NTIM",  
  "TEST", "TESTN", "TESTCD", "TESTCAT",   "DVOR", "DVORU", "BLQ", "LLOQ",  "METHOD", "SAMDTTM"      
)  

adpc = read_csv("./data/adpc0.csv")

tdata = adpc %>%  
  select(STUDYID, CLID, DOSEPLAN, DOSU,
         VISIT, TIMEPT,  TEST, RESC,  STDUNIT, SOP, BLQ, 
         SAMDT,    SAMTM
  )   %>% filter(SOP=="PCL3671")

tdata = tdata %>% mutate(STUDYID="STUDY-ID", 
                         USUBJID = gsub("-", "", CLID, fix=TRUE),
                         #CLID = add_prefix(as.integer(as.factor(CLID)), digits=3), 
                         ARMA = paste0(DOSEPLAN, " ", DOSU),
                         ARMA = gsub("30", "11", ARMA, fix=TRUE),
                         ARMA = gsub("3", "1", ARMA, fix=TRUE),
                         ARMA = gsub("10", "3", ARMA, fix=TRUE), 
                         
                         ARMAN = as.integer(as.factor(ARMA)), 
                         SAMDTTM =  paste0(SAMDT,"T", SAMTM), 
                         RESC = as_numeric(RESC)/1000, 
                         STDUNIT = "mg/L",
                         TESTCD = "DRUGNAME", 
                         TEST = "Concentration of Total DRUGNAME",
                         SOP = "SOP1234", 
                         
                         SAMDTTM = gsub("2014", "1972", SAMDTTM, fix=TRUE), 
                         SAMDTTM = gsub("2015", "1973", SAMDTTM, fix=TRUE)
                         
) %>% select(-SAMDT, -SAMTM, -CLID, -DOSEPLAN, -DOSU) 
 
tdata = tdata %>% mutate(USUBJID = gsub("840", "442", USUBJID, fix=TRUE)) 

#tdata = tdata[, intersect(adpc.var.lst , colnames(tdata))]
tdata %>% as.data.frame() %>% head()
adpc = tdata 
write_csv(tdata, path="./data/adpc.csv")
 

adsl0 = adsl
adex0 = adex
adpc0 = adpc
 




date_time_format = c("Ymd HMS", "mdY HMS", "bdY HMS", "dbY HMS")
adsl = build_adsl(adsl0)
adex = build_adex(adex0, date_time_format=date_time_format)


adpc = build_adpc(adpc0 %>% rename(DVOR = RESC, DVORU = STDUNIT, METHOD=SOP, PCTPT=TIMEPT), 
                  date_time_format=date_time_format)
check_adpc(adpc0, adpc)


head(adpc) %>% as.data.frame()


nmdat = build_adpx(adsl, adex, adpc)

head(nmdat) %>% as.data.frame()
 


ggplot(nmdat, aes(x=TIME, y=DVOR, group=USUBJID, col=ARMA)) + 
  geom_point() + geom_line() + 
  scale_y_log10() 
