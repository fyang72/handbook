

# -----------------------------------------------
setwd("~/handbook/")
source("~/handbook/global.R")
# -----------------------------------------------
 
adsl0 = NULL
adex0 = NULL
adpc0 = NULL


# ------------------------------
# TEST
# ------------------------------
adslFile_name <- "~/handbook/data/adsl.csv"
adsl0[["TEST"]] <- read_datafile(inFile=adslFile_name) 
 
adexFile_name <- "~/handbook/data/adex.csv"
adex0[["TEST"]] <- read_datafile(inFile=adexFile_name) 

adpcFile_name <- "~/handbook/data/adpc.csv"
adpc0[["TEST"]] <- read_datafile(inFile=adpcFile_name) 



# ------------------------------
#R1908-HV-1240
# ------------------------------
adslFile_name <- "~/FYANG/R1908_Feld1/DATA/R1908-HV-1240-adsl.sas7bdat"
adsl <- read_datafile(inFile=adslFile_name) 
adsl <- adsl %>% mutate(
  STUDYID = STUDYID, 
  USUJBID = USUBJID, 
  WGTBL = WEIGHTBL, 
  HGTBL =  HEIGHTBL
)
adsl0[["R1908-HV-1240"]] = adsl

adexFile_name <- "~/FYANG/R1908_Feld1/DATA/R1908-HV-1240-adex.sas7bdat"
adex <- read_datafile(inFile=adexFile_name) 
adex <- adex %>% 
  mutate(STUDYID = "R1908-HV-1240", 
         USUBJID = SUBJECT, 
         EXSTDTC = RFSTDTC, 
         EXENDTC = EXENDTC, 
         EXDOSE = DOSE, 
         EXDOSU = "mg", 
         EXROUTE = "SC",
         ARMA = DOSECH  #####
  ) 
adex0[["R1908-HV-1240"]] = adex

adpcFile_name <- "~/FYANG/R1908_Feld1/DATA/R1908-HV-1240-adpc.sas7bdat"
adpc <- read_datafile(inFile=adpcFile_name) 
adpc <- adpc %>%  
  mutate(STUDYID = "R1908-HV-1240", 
         USUBJID = SUBJECT, 
         SAMDTTM = SAMDTTM, 
         DVOR = CONCN, 
         DVORU = STDUNIT,
         TEST = TEST,
         
         ARMA = DOSECH  #####
  )
adpc0[["R1908-HV-1240"]] = adpc






#-------------------------------
#R1908-ALG-1325
# ------------------------------
adslFile_name <- "~/FYANG/R1908_Feld1/DATA/R1908-ALG-1325-adsl.sas7bdat"
adsl <- read_datafile(inFile=adslFile_name) 
adsl <- adsl %>% mutate(
  STUDYID = STUDYID, 
  USUJBID = USUBJID, 
  WGTBL = WEIGHTBL, 
  HGTBL =  HEIGHTBL
)
adsl0[["R1908-ALG-1325"]] = adsl

adexFile_name <- "~/FYANG/R1908_Feld1/DATA/R1908-ALG-1325-adex.sas7bdat"
adex <- read_datafile(inFile=adexFile_name) 
adex <- adex %>% 
  mutate(STUDYID = STUDYID, 
         USUBJID = USUBJID, 
         EXSTDTC = EXSTDTMC, 
         EXENDTC = EXSTDTMC, 
         EXDOSE = EXDOSE , 
         EXDOSU = EXDOSU , 
         EXROUTE = "SC",
         ARMA = paste0(EXDOSE, " ", EXDOSU, " Single SC")  #####
  ) 
adex0[["R1908-ALG-1325"]] = adex

adpcFile_name <- "~/FYANG/R1908_Feld1/DATA/R1908-ALG-1325-adpc.csv"
adpc <- read_datafile(inFile=adpcFile_name) 
adpc <- adpc %>%  
  mutate(STUDYID = STUDYID, 
         USUBJID = USUBJID, 
         SAMDTTM = SAMDTTM, 
         DVOR = DVOR, 
         DVORU = STDUNIT,
         TEST = TEST,
         
         ARMA = ARMA,  #####
         METHOD = SOP
  )
adpc0[["R1908-ALG-1325"]] = adpc


#-------------------------------
# BUILD
# ------------------------------
study_lst <- c("TEST", "R1908-HV-1240", "R1908-ALG-1325")
study_lst <- c("R1908-HV-1240" )


nmdat0 <- NULL
for (istudy in study_lst) { 
  print(istudy)
  
  # adsl
  adsl <- adsl0[[istudy]] %>% build_adsl() 
  check_adsl(adsl)
  
  # adex
  adex <- adex0[[istudy]] %>% build_adex()
  check_adex(adex)
  
  
  # adpc 
  adpc <- adpc0[[istudy]] %>% build_adpc()
  check_adpc(adpc)
  
  # adpx 
  adpx <- build_adpx(adsl, adex, adpc, other=NULL)
  
  # nmdat
  nmdat0[[istudy]] <- adpx %>% build_nmdat()

}


nmdat <- nmdat0[[istudy]]  #%>% select(-ends_with("_ORG"))
check_nmdat(nmdat)
 

# finally, nmdat
sapply(nmdat, class)
nmdat

