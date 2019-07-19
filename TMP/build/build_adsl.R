
##############################################################################
# Purpose:
#-----------
# adsl, which should contain all subject characteristics and demographic,

# Input and output
#------------------
# Input:  adsl, 
# 1) csv dataset, its location specified by users
##############################################################################

# Output

# SUBJECT LEVEL:
# STUDYIDL:   R1193-HV-1219             study ID
# USUBJID:    R1193-HV-1219-001-005     universal subject ID;
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
  

build_adsl <- function(adsl,
                        adsl.col.lst = c(  "STUDYID", "USUBJID", "ARMA", "WGTBL", "HGTBL", "AGE",     "AGEU",    "SEX",     "SEXN",    "RACE",    "RACEN",   "ETHNIC",  "ETHNICN" ) , 
                        SEX.lst = c( "M", "F"), 
                        ETHNIC.lst =c("NOT HISPANIC OR LATINO",  "HISPANIC OR LATINO"), 
                        RACE.lst =c(  "WHITE", 
                                      "BLACK OR AFRICAN AMERICAN",
                                      "ASIAN",
                                      "AMERICAN INDIAN OR ALASKA NATIVE",
                                      "WHICH.IS.FIVE",
                                      "OTHER", 
                                      "UNKNOWN",
                                      "NOT REPORTED") ) { 
  
  #--------------------------------------------  
  # Default setup
  #--------------------------------------------  
  adsl = as.data.frame(adsl) 
  colnames(adsl) = toupper(colnames(adsl))
  
  
  #---------------------------------------------
  # must have these variables, then if the desired variables missing, fill with "." 
  #---------------------------------------------   
  col.lst =  c("STUDYID", "USUBJID", "WGTBL")  
  col.lst = setdiff(col.lst, colnames(adsl))
  if (length(col.lst)>0) {
    print( paste0("Warning: ", paste0(col.lst, collapse=", "), " must be included in adsl"))
    stopifnot(length(col.lst)==0) }
  
  # if missing desired variables
  col.lst = setdiff(adsl.col.lst, colnames(adsl))
  if (length(col.lst)>0) {print( paste0("Warning: ", paste0(col.lst, collapse=", "), " not included in the original adsl"))}
  adsl[, col.lst] = "."
  
   
  #----------------------------------------------------------------------------- 
  # SUBJECT LEVEL: "STUDYID"  "USUBJID"  "SUBJECT"  "SUBJECTN" "SITEID" 
  #-----------------------------------------------------------------------------   
  
  #----------------------------------------------------------------------------- 
  # SUBJECT LEVEL: "STUDYID"  "USUBJID"   
  #-----------------------------------------------------------------------------
  adsl$STUDYID = adsl$STUDYID 
  
  #USUBJID
  adsl$CLID = adsl$USUBJID 
  study.id = unique(adsl$STUDYID)
  for (i in 1:length(study.id)) { 
    adsl$CLID = gsub(study.id[i], "", adsl$CLID, fix=TRUE)
  }
  adsl$CLID = gsub("-", "", adsl$CLID, fix=TRUE)   
  
  t1 = unique(nchar(adsl$CLID))
  if (length(t1)>1)  {print("the length of CLID in adsl are not the same.")}
  if (t1==9) { adsl = adsl %>% mutate(SUBJECT=paste(substr(CLID, 1, 3), substr(CLID, 4,6), substr(CLID, 7, 9), sep="-"))}
  if (t1==6) { adsl = adsl %>% mutate(SUBJECT=paste(substr(CLID, 1, 3), substr(CLID, 4,6), sep="-"))}
  if (t1==3) { adsl = adsl %>% mutate(SUBJECT=paste(substr(CLID, 1, 3), sep="-"))}
  if (!t1 %in% c(3, 6, 9)) {print("nchar(CLID) in adsl !=3, 6 or 9")}
  
  adsl$USUBJID <- paste(adsl$STUDYID,  adsl$SUBJECT, sep="-") 
  adsl = adsl %>% select(-SUBJECT, -CLID)
  
  #----------------------------------------------------------------------------- 
  # ARMA LEVEL: "GROUP" "GROUPN"   "ARMA"  "ARMAN"  "COHORT"  "COHORTN"  "TRT"  "TRTN" 
  #-----------------------------------------------------------------------------   
  adsl$ARMA = adsl$ARMA   # default
  
  
  #----------------------------------------------------------------------------- 
  # DEMOGRAPHIC LEVEL: "AGE"    "WGTBL" "HGTBL"  "BMIBL"    "BSABL" 
  #----------------------------------------------------------------------------- 
  adsl = adsl %>% mutate(AGE=as_numeric(AGE), 
                         WGTBL = as_numeric(WGTBL), 
                         HGTBL = as_numeric(HGTBL))    
  # Commonly accepted BMI ranges are underweight: 
  #   under          18.5, 
  #   normal weight: 18.5 to 25, 
  #   overweight:    25 to 30, 
  #   obese: over    30.
  # adpk$BMIBL =  signif(adpk$WGTBL/(adpk$HGTBL/100)^2, digits=3)     # in kg/m2
 
  
  # The most widely used is the Du Bois formula,[3][4] which been shown to be 
  # equally as effective in estimating body fat in obese and non-obese patients, 
  # something the Body mass index fails to do
  # adpk$BSABL = signif(0.007184*adpk$WEIGHTBL^0.425 * adpk$HEIGHTBL^0.725, digits=3)  # in M^2
  #adsl$BSABL = signif(0.007184*as_numeric(adsl$WGTBL)^0.425 * as_numeric(adsl$HGTBL)^0.725, digits=3)  # in M^2
  #adsl$BSABL = as_numeric(adsl$BSABL)
  
  #-----------------------------------------------------------------------------   
  # DEMOGRAPHIC LEVEL:  "SEX"   "SEXN" "ETHNIC"  "ETHNICN"  "RACE"  "RACEN"  
  #-----------------------------------------------------------------------------
  # SEXN = 0 for FEMALE,  
  # SEXN = 1 for MALE
 
  adsl = adsl %>% mutate(SEX = toupper(SEX), 
                         SEX = ifelse(SEX %in% c("MALE", "M"), "M",  
                                      ifelse(SEX %in% c("FEMALE", "F"), "F",  ".")),
                         SEX = ordered(SEX, levels= SEX.lst), 
                         
                         SEXN = as.integer(SEX),
                         SEXN = ifelse(is.na(SEXN), -99, SEXN) 
  )
  
   
  
  # NOT HISPANIC OR LATINO       1
  # HISPANIC OR LATINO           2
  adsl = adsl %>% mutate(ETHNIC = toupper(ETHNIC), 
                         ETHNIC = ordered(ETHNIC, levels = ETHNIC.lst),
                         
                         ETHNICN = as.integer(ETHNIC),
                         ETHNICN = ifelse(is.na(ETHNICN), -99, ETHNICN) 
  )
   
  
  #                               RACE RACEN
  #                              WHITE    1
  #                           UNKNOWN     7
  #                      NOT REPORTED     8
  #                             OTHER     6
  #                             ASIAN     3
  #         BLACK OR AFRICAN AMERICAN     2
  #  AMERICAN INDIAN OR ALASKA NATIVE     4 
  adsl = adsl %>% mutate(RACE = toupper(RACE), 
                         RACE = ordered(RACE, levels = RACE.lst),
                         
                         RACEN = as.integer(RACE),
                         RACEN = ifelse(is.na(RACEN), -99, RACEN) 
  )
   
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
  
  #---------------------------------------------
  # order columns, and final output
  #---------------------------------------------   
  col.lst = c(adsl.col.lst, setdiff(colnames(adsl), adsl.col.lst))
  adsl = adsl[, col.lst]
  
   
  
  return(adsl)
  
}




if (1==2) { 
  
  # should be unique USUBJID in adsl
  adsl = adsl.v0 %>% mutate(ARMA = TRT01A,
                            ARMAN = TRT01AN, 
                            WGTBL = WEIGHTBL, 
                            HGTBL = HEIGHTBL)
  build_adsl(adsl)
   
  
  
}


##############################################################################
##############################################################################
#STATS about adsl
##############################################################################
############################################################################## 
 


#' Extracts the time matched concntration vs effect data
#'
#'
#'
#' @param data A data frame.  The data frame must conform to Regeneron standards and contain the variables: `week`, `ndy`, `concn`, `concni`, `pchg`, `actarmcd`, `ada`
#' @return A dataframe with columns `ndy`, `concn`, `effect`.
#' @export
#' @importFrom dplyr left_join group_by summarise
#' @examples
#'   inFile <- system.file("extdata/pk_pd_hysteresis.xls", package = "pkGraph")
#'   library(readxl)
#'   theData <-read_excel(inFile)
#'   timeMatchedPK(data = theData)
##############################################################################
#  check.adsl
##############################################################################
check_adsl <- function(adsl.FINAL) {
  
  # SOME SUBJECT HAVE NOT BEEN ASSIGNED WITH ARMA!
  
  
  #---------------------------------------------  
  # Exclude subjects with Screen Failure
  #---------------------------------------------
  adsl = adsl.FINAL
  adsl = filter(adsl, ARMA !="Screen Failure")
  rownames(adsl) = adsl$USUBJID
  
  
  # Compactly Display the Structure of an Arbitrary R Object
  adsl %>% dplyr::select(STUDYID, USUBJID, ARMA, AGE,WEIGHTBL, SEX, ETHNIC, RACE ) %>% str
  
  
  #---------------------------------------------  
  # List all entries with abnormal values
  #---------------------------------------------   
  nacols <- function(df) { 
    colnames(df)[unlist(lapply(df, function(x) any(is.na(x)|is.nan(x)|x=="NA"|x=="NAN"|x=="NaN"|x==".")))] }
  anyna <- function(df) {
    df = data.frame(df, check.names = FALSE)      
    t.df = data.frame(t(df), check.names = FALSE)
    return(df[nacols(t.df), nacols(df)])  }
  
  
  cat(paste('', '\n','List all entries with abnormal values...\n', sep=''))  
  print(anyna(adsl))
  
  
  #---------------------------------------------
  # Checking USUBJID
  #---------------------------------------------
  # Check the format of USUBJID 
  if(!all(nchar(adsl$USUBJID)==nchar(adsl$STUDYID) + 3*4)) {  # "R2810-ONC-1423" +  "-724-002-007"
    print("Warning: Not uniformly formated USUBJID") 
    print(unique(adsl$USUBJID))}
  
  # Check the number of subject 
  cat(paste("","\n","Number of subjects (exlude Screen Failure): ", length(adsl$USUBJID), "\n", sep=""))
  
  # Check number of subjects within each GROUP and ARMA
  cat(paste("", "\n", "Accouting of USUBJID by ARMA...\n", sep=""))
  adsl %>% dplyr::group_by(ARMA) %>% dplyr::summarise(N=fun.uniqN(USUBJID)) %>% dplyr::arrange(ARMA)
  
  
  cat(paste("", "\n", "Accouting of USUBJID by GROUP...\n", sep=""))      
  adsl %>% dplyr::group_by(GROUP) %>% dplyr::summarise(N=fun.uniqN(USUBJID)) %>% dplyr::arrange(GROUP)
  
  
  ##############################################################################
  # Demographics and Baseline Characteristics (PK analysis set) for each cohort
  # age(yrs), male (n, %), white (n, %), hispanic (n, %), BMI (kg/m2)
  ##############################################################################
  
  #---------------------------------------------
  # Checking demographic (continous variables)
  #---------------------------------------------
  library('dplyr')
  
  demog1= NULL
  col.lst = c("AGE","WEIGHTBL","HEIGHTBL","BMIBL","BSABL")
  adsl[, col.lst]  %>% str
  #%>% dplyr::select(STUDYID, USUBJID, ARMA, AGE,WEIGHTBL, SEX, ETHNIC, RACE ) %>% str
  
  ### Iterate through multiple ETA values
  #split_eta_cov <- g2_eta_cov %>% split(.$cov_name)
  demog1 = lapply(col.lst, function(colname, adsl) {
    calc_stats(adsl, id="USUBJID",group="ARMA",colname, digits=3) %>% mutate(CAT = colname) 
  }, adsl)
  
  
  demog1 = demog1 %>% bind_rows() 
  
  ylab.txt = c("Age(years)", "Weight(kg)", "Height(cm)", "BMI(kg/m^2)",  "BSA(m^2)")    
  bp = lapply(col.lst, function(colname, xlab.txt, ylab.txt, adsl) {
    water.fall.plot(adsl, colname, xlab.txt, ylab.txt) 
  },  xlab.txt="Subject Index", ylab.txt, adsl)
  
  bp = bp %>% bind_rows() %>% gather(KEY, VAL, AGE, WEIGHTBL, HEIGHTBL, BMIBL, BSABL)
  
  
  
  #---------------------------------------------
  # Checking demographic (categorical variable)
  #---------------------------------------------
  
  # check the categorical variables
  adsl[which(!duplicated(adsl$SEX)), c("SEX", "SEXN")] ; as.matrix(table(adsl$SEX))
  adsl[which(!duplicated(adsl$ETHNIC)), c("ETHNIC", "ETHNICN")]  ;  as.matrix(table(adsl$ETHNIC))
  adsl[which(!duplicated(adsl$RACE)), c("RACE", "RACEN")]  ;  as.matrix(table(adsl$RACE))
  
  # assigned value for SEX, ETHNIC and RACE 
  adsl$SEXN = ifelse(adsl$SEX=="M", 1, 0)
  adsl$ETHNICN = ifelse(toupper(adsl$ETHNIC)=="NOT HISPANIC OR LATINO", 1, 0)
  adsl$RACEN = ifelse(toupper(adsl$RACE)=="WHITE", 1, 0)
  
  demog2= NULL
  col.lst = c("SEXN", "ETHNICN", "RACEN")
  for (i in 1:length(col.lst)) {demog2 = rbind(demog2,calc.stats.cat(adsl, id="USUBJID",by="ARMA",colname=col.lst[i]))}
  demog2 = demog2 %>% arrange(CAT, ARMA) %>%   mutate(N_PCT=paste(N,"(",PCT,")",sep=""))
  
  
  t1 = dplyr::rename(demog1, RESULT=Mean_SD)
  t2 = dplyr::rename(demog2, RESULT=N_PCT)
  demog = rbind(t1[, c("CAT","ARMA","N","RESULT")], 
                t2[, c("CAT","ARMA","N","RESULT")])  
  
  
  demog$CAT = as.character(demog$CAT)
  demog$CAT[which(duplicated(demog$CAT))] = "" 
  
  cat(paste("", "\n", "Summary of the population studied in this analysis...\n", sep=""))          
  print(demog)
  
  #u.write.table(demog, file.name="./LATEX/tabl/demog.csv")    
  
}









