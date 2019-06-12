
##############################################################################
# Purpose:
#-----------
# adsl, which should contain all subject characteristics and demographic,

# Input and output
#------------------
# Input:  adsl, 

# Output
# 1) csv dataset, its location specified by users
##############################################################################

# SUBJECT LEVEL:
# STUDYIDL:   R1193-HV-1219             study ID
# USUBJID:    R1193-HV-1219-001-005     universal subject ID;
# SUBJECT:    001-005                   short version subject ID, using "-" among ID
# SUBJECTN:   1005                      numerical subject ID, remove "-" from SUBJECT
# SITEID:     001

# ARMA LEVEL:
# ARMA /ARMCD /ARMN:      1A, 2B
# GROUP:                  GROUP A
# COHORT:                 Group-A
# TRT/TRTN:               0.05 mg/kg,  similar as ARMA
# TRTGROUP:               Group-A 0.05 mg/kg IV

# DEMOGRAPHIC LEVEL:  
# AGE SEX SEXN  RACE RACEN  ETHNIC ETHNICN   
# WEIGHTBL HEIGHTBL BMIBL  


##############################################################################
##############################################################################  
# customerize - adsl
##############################################################################
##############################################################################
if (1==2) { 
  
  source("H:\\FYANG\\aTemplate\\Rpackage\\regnR\\latex\\load_Rpackage.r")
  
  # loading regnR script functions
  #----------------------------------------------------
  folder.loc <- "H:\\FYANG\\aTemplate\\rpackage\\regnR\\R"
  file.lst <-list.files(path = folder.loc, pattern = ".r",  all.files = FALSE,full.names = TRUE, include.dirs = FALSE)     
  for (ifile in 1:length(file.lst)) { print(file.lst[ifile]);     source(file=file.lst[ifile])  }
  
  HOME = "H:\\SHINY\\shiny_0916\\data\\R2222\\R2222-HV-1326\\"
  adsl = read_sas(paste0(HOME, "adsl.sas7bdat"))
  adsl.v0 = adsl
  
  file="H:\\SHINY\\shiny_0916\\dynamicReport\\lib\\pkmeta.xlsx"
  adsl_checkin = read_excel(file, sheet = "adsl", col_names = TRUE) %>% as.data.frame()  
  adsl_checkin[is.na(adsl_checkin)] = ""
  adsl_checkin$Type = as.factor(adsl_checkin$Type)
  adsl_checkin= adsl_checkin %>% select(Domain:Note) %>% filter(Domain%in%c("adsl"))
  
}



#Load adsl_checkin
#----------------------
adsl_bestguess <- function(adsl_checkin, adsl)  {
   
  # fill entries with best guess
  adsl_checkin$Which_Column = as.character(adsl_checkin$Which_Column)
  rownames(adsl_checkin) = adsl_checkin$Std_Name
  
  col.lst = c("STUDYID", "USUBJID", "ARMA", "WEIGHTBL", "HEIGHTBL", "AGE", "BMIBL", "BSABL", "SEX", "ETHNIC", "RACE", "COUNTRY", "SITEID")
  subs.lst = c(STUDYID_VARS(adsl), USUBJID_VARS(adsl), ARMA_VARS(adsl), WEIGHTBL_VARS(adsl), HEIGHTBL_VARS(adsl), 
               AGE_VARS(adsl), BMIBL_VARS(adsl), BSABL_VARS(adsl), SEX_VARS(adsl), ETHNIC_VARS(adsl), 
               RACE_VARS(adsl), COUNTRY_VARS(adsl), SITEID_VARS(adsl) )
              
  adsl_checkin[col.lst, "Which_Column"] = subs.lst
  
  adsl_checkin
}





build_adsl <- function(adsl) { 
 
  #adsl = adsl.v0
  
  #----------------------------------------------------------------------------- 
  # default check
  #-----------------------------------------------------------------------------     
  
  adsl.col.lst <- c(
    "STUDYID", "USUBJID", "SUBJID",  "SITEID", "WEIGHTBL",   
    "AGE",     "AGEU",    "SEX",     "SEXN",    "RACE",    "RACEN",   "ETHNIC",  "ETHNICN"               
  )  
  
  col.lst = setdiff(adsl.col.lst, colnames(adsl))
  if (length(col.lst)>0) {print(paste0("Missing key column(s) of ", paste(col.lst, collapse=", "), " in adsl"))}
  
  adsl <- data.frame(adsl, stringsAsFactors=FALSE)     
 
  #----------------------------------------------------------------------------- 
  # SUBJECT LEVEL: "STUDYID"  "USUBJID"  "SUBJECT"  "SUBJECTN" "SITEID" 
  #-----------------------------------------------------------------------------   
  if (is.null(adsl$STUDYID)) {adsl$STUDYID ="Rxxxx-xxx-xxxx"}
  
  if (is.null(adsl$USUBJID)) {print("No USUBJID in adsl.")}
  stopifnot(!is.null(adsl$USUBJID))
  
  adsl$SUBJID = gsub("-", "", adsl$SUBJID, fix=TRUE)
  adsl$SUBJID = gsub("_", "", adsl$SUBJID, fix=TRUE)
  
  t1 = unique(nchar(adsl$SUBJID))
  if (length(t1)>1)  {print("SUBJID in adsl have irregular ID --length(unique(nchar(adsl$SUBJID)))>1")}
  if (t1==9) { adsl = adsl %>% mutate(SUBJECT=paste(substr(SUBJID, 1, 3), substr(SUBJID, 4,6), substr(SUBJID, 7, 9), sep="-"))}
  if (t1==6) { adsl = adsl %>% mutate(SUBJECT=paste(substr(SUBJID, 1, 3), substr(SUBJID, 4,6), sep="-"))}
  if (t1==3) { adsl = adsl %>% mutate(SUBJECT=paste(substr(SUBJID, 1, 3), sep="-"))}
  if (!t1 %in% c(3, 6, 9)) {print("nchar(SUBJID) in adsl !=3, 6 or 9")}
  
  adsl$USUBJID <- paste(adsl$STUDYID,  adsl$SUBJECT, sep="-") 
  adsl$SUBJECTN <- as_numeric(gsub("-","", adsl$SUBJECT))  
  
  # SITEID, COUNTRY
  if(is.null(adsl$SITEID)) {adsl$SITEID = "."}   # default
  
  #----------------------------------------------------------------------------- 
  # ARMA LEVEL: "GROUP" "GROUPN"   "ARMA"  "ARMAN"  "COHORT"  "COHORTN"  "TRT"  "TRTN" 
  #-----------------------------------------------------------------------------   
  if(is.null(adsl$ARMA)) {adsl$ARMA = "."}   # default
  
  # always use adex$ARMA as the final one
  #if (!is.null(adex)) {
  #  adsl = left_join(adsl, adex%>%distinct(USUBJID, .keep_all=TRUE)%>%select(USUBJID, ARMA), by="USUBJID") # getting actual dosing arm
  #} 
  
  #----------------------------------------------------------------------------- 
  # DEMOGRAPHIC LEVEL: "AGE"    "WEIGHTBL" "HEIGHTBL"  "BMIBL"    "BSABL" 
  #----------------------------------------------------------------------------- 
  adsl$AGE = as_numeric(adsl$AGE)  
  if(is.null(adsl$AGEU))  {adsl$AGEU ="Years"}
  if(is.null(adsl$WEIGHTBL))  {adsl$WEIGHTBL =75}  
  if(is.null(adsl$HEIGHTBL))  {adsl$HEIGHTBL = "."}   
  
  # Commonly accepted BMI ranges are underweight: 
  #   under          18.5, 
  #   normal weight: 18.5 to 25, 
  #   overweight:    25 to 30, 
  #   obese: over    30.
  # adpk$BMIBL =  signif(adpk$WEIGHTBL/(adpk$HEIGHTBL/100)^2, digits=3)     # in kg/m2
  if(is.null(adsl$BMIBL))  {adsl$BMIBL = "."} 
  
  # The most widely used is the Du Bois formula,[3][4] which been shown to be 
  # equally as effective in estimating body fat in obese and non-obese patients, 
  # something the Body mass index fails to do
  # adpk$BSABL = signif(0.007184*adpk$WEIGHTBL^0.425 * adpk$HEIGHTBL^0.725, digits=3)  # in M^2
  adsl$BSABL = signif(0.007184*as_numeric(adsl$WEIGHTBL)^0.425 * as_numeric(adsl$HEIGHTBL)^0.725, digits=3)  # in M^2
  adsl$BSABL = as_numeric(adsl$BSABL)
  
  #-----------------------------------------------------------------------------   
  # DEMOGRAPHIC LEVEL:  "SEX"   "SEXN" "ETHNIC"  "ETHNICN"  "RACE"  "RACEN"  
  #-----------------------------------------------------------------------------
  # SEXN = 0 for FEMALE,  
  # SEXN = 1 for MALE
  if (is.null(adsl$SEX)) {adsl$SEX = "."}
  adsl$SEX = factor(toupper(adsl$SEX))
  adsl$SEXN = ifelse(adsl$SEX=="M", 1,  
                     ifelse(adsl$SEX=="F", 0, -99)) #adsl$SEXN  
  
  # NOT HISPANIC OR LATINO       1
  # HISPANIC OR LATINO           2
  if (is.null(adsl$ETHNIC)) {adsl$ETHNIC = "."}
  adsl$ETHNIC = factor(toupper(adsl$ETHNIC)) 
  adsl$ETHNICN = -99
  adsl$ETHNICN[which(adsl$ETHNIC=="NOT HISPANIC OR LATINO")] = 1 
  adsl$ETHNICN[which(adsl$ETHNIC=="HISPANIC OR LATINO")] = 2 
  
  #                               RACE RACEN
  #                              WHITE    1
  #                           UNKNOWN     7
  #                      NOT REPORTED     8
  #                             OTHER     6
  #                             ASIAN     3
  #         BLACK OR AFRICAN AMERICAN     2
  #  AMERICAN INDIAN OR ALASKA NATIVE     4 
  if (is.null(adsl$RACE)) {adsl$RACE = "."}
  adsl$RACE = toupper(adsl$RACE)
  adsl$RACEN = 0     # ????
  adsl$RACEN[which(adsl$RACE=="WHITE")] = 1 
  adsl$RACEN[which(adsl$RACE=="BLACK OR AFRICAN AMERICAN")] = 2 
  adsl$RACEN[which(adsl$RACE=="ASIAN")] = 3 
  adsl$RACEN[which(adsl$RACE=="AMERICAN INDIAN OR ALASKA NATIVE")] = 4 
  adsl$RACEN[which(adsl$RACE=="OTHER")] = 6 
  adsl$RACEN[which(adsl$RACE=="UNKNOWN")] = 7 
  adsl$RACEN[which(adsl$RACE=="NOT REPORTED")] = 8  
  
  #adsl$COMMENT = "NONE"
  #rownames(adsl) = adsl$USUBJID
  
  #-----------------------------------------------------------------------------
  # population flag
  #-----------------------------------------------------------------------------     
  # "SAFFL"    safety population flag   
  # "COMPLFL"  completers population flag
  # "PPROTFL"  per-protocol population flag
  # "RANDFL"   randomized population flag
  # "ENRLFL"   enrolled population flag
  # "PKFL"     PK population flag
  
  # "ARM"      "ARMN"      planned ARM
  # "TRT01P"   "TRT01PN"   planned treatment
  # "TRT01A"   "TRT01AN"   actual treatment  
  # 
  # "TRTSDT"   "TRTSTM"   "TRTSDTM"    date/time of start treatment 
  # "TRTEDT"   "TRTETM"   "TRTEDTM"    date/time of end of treatment 
  # 
  # "BRTHDT"   date of birth    
  # "SCRNDT"   screen date
  # "RANDDT"   date of randomization
  # "DTHDT"    date of death
  #  
  # "LVISDT"   date of last visit 
  # "EOSDT"    date of end of study
  
  #----------------------------------------------------------------------------- 
  # FINAL: output of standardized adsl
  #----------------------------------------------------------------------------- 
  key.col.lst <- c(
    "STUDYID", "USUBJID", "SUBJECT",  "SITEID", "WEIGHTBL",   
    "AGE",     "AGEU",    "SEX",     "SEXN",    "RACE",    "RACEN",   "ETHNIC",  "ETHNICN"               
  )  
  adsl[, setdiff(key.col.lst, colnames(adsl))] = "." 
  
  adsl = adsl[, c(key.col.lst, setdiff(colnames(adsl), key.col.lst) )]
  
  return(adsl)
  
}

if (1==2) { 
adsl = adsl.v0

adsl_checkin = adsl_bestguess(adsl_checkin, adsl)

std.col.lst = adsl_checkin %>% filter(  Which_Column!="", Which_Column!="NA") %>% pull(Std_Name) # select(Name) %>% as.character()
col.lst = adsl_checkin %>% filter(  Which_Column!="",  Which_Column!="NA") %>% pull(Which_Column) #
col.lst = intersect(col.lst, colnames(adsl))
if (length(col.lst)>0) {adsl[, std.col.lst] = adsl[, col.lst]}

head(adsl) %>% as.data.frame()



}


##############################################################################
##############################################################################
#STATS about adsl
##############################################################################
############################################################################## 


# view,  filter,  select, arrange, mutate, mutate, summarise,

# selct: starts_with,  ends_with, contains, matches, num_range, one_of everything

# summarise: min, median, max, sd, var, n, n_distinct, num_range

##############################################################################
#  functions 
##############################################################################  



# check.adsl(adsl.FINAL)  











