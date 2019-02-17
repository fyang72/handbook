
#################################################################
# build_adsl
#################################################################
build_adsl <-function(
  dataset,  
  
  adsl.var.lst = c("STUDYID", "USUBJID",  
                   "WGTBL", 
                   "HGTBL", 
                   "AGE", "AGEU",  
                   "SEX", "SEXN", 
                   "RACE",  "RACEN", 
                   "ETHNIC","ETHNICN" ), 
  
  sex.lst = c("M", "F", "UNKNOWN"),
  
  ethnic.lst =c("NOT HISPANIC OR LATINO",  "HISPANIC OR LATINO", "UNKNOWN"),
  
  race.lst =c(  "WHITE", 
                "BLACK OR AFRICAN AMERICAN",
                "ASIAN",
                "AMERICAN INDIAN OR ALASKA NATIVE",
                "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER",    
                "OTHER", 
                "UNKNOWN",
                "NOT REPORTED") 
) { 

  # must have these desired variables, if missing, fill with NA 
  adsl = dataset %>% fillUpCol_df(adsl.var.lst) 
  
  #---------------------------------------- 
  # Analysis identifiers
  #---------------------------------------- 
  # STUDYID
  adsl$STUDYID = adsl$STUDYID 
  
  # USUBJID
  if (all(is.na(adsl$USUBJID))) {print("error: all(is.na(USUBJID))=TRUE")}

  # standardize USUBJID
  adsl$CLID = adsl$USUBJID 
  study.id = unique(adsl$STUDYID)
  study.id = study.id[which(!is.na(study.id))]
  for (i in 1:max(1,length(study.id))) { 
    adsl$CLID = gsub(study.id[i], "", adsl$CLID, fix=TRUE)
  }
  adsl$CLID = gsub("-", "", adsl$CLID, fix=TRUE)   
  
  t1 = unique(nchar(adsl$CLID))
  t1 = t1[which(!is.na(t1))]
  if (length(t1)==0)  {print("warning: no USUBJID")
  }else if (length(t1)>1)  {print("warning: the length of CLID in adsl are not the same.")
      }else{ 
          if (t1==9) { adsl = adsl %>% mutate(SUBJECT=paste(substr(CLID, 1, 3), substr(CLID, 4,6), substr(CLID, 7, 9), sep="-"))}
          if (t1==6) { adsl = adsl %>% mutate(SUBJECT=paste(substr(CLID, 1, 3), substr(CLID, 4,6), sep="-"))}
          if (t1==3) { adsl = adsl %>% mutate(SUBJECT=paste(substr(CLID, 1, 3), sep="-"))}
          if (!t1 %in% c(3, 6, 9)) {print("nchar(CLID) in adsl !=3, 6 or 9")}
          adsl$USUBJID <- paste(adsl$STUDYID,  adsl$SUBJECT, sep="-") 
          adsl = adsl %>% select(-SUBJECT, -CLID)
      }
    
  #----------------------------------------------------------------------------- 
  # Analysis Treatment Variables 
  #----------------------------------------------------------------------------- 
  # "ARM"      "ARMN"      planned ARM
  # "TRT01P"   "TRT01PN"   planned treatment
  # "TRT01A"   "TRT01AN"   actual treatment  
  # 
  # "TRTSDT"   "TRTSTM"   "TRTSDTM"    date/time of start treatment 
  # "TRTEDT"   "TRTETM"   "TRTEDTM"    date/time of end of treatment 
  
  
  #----------------------------------------------------------------------------- 
  # Demographic continous variables:  "AGE"  "WGTBL" "HGTBL"  "BMIBL" "BSABL" 
  #----------------------------------------------------------------------------- 
  adsl = adsl %>% mutate(AGE=as_numeric(AGE), 
                         WGTBL = as_numeric(WGTBL), 
                         HGTBL = as_numeric(HGTBL))    
   
  # Commonly accepted BMI ranges are underweight: 
  #   under          18.5, 
  #   normal weight: 18.5 to 25, 
  #   overweight:    25 to 30, 
  #   obese: over    30.
  adsl$BMIBL =  signif(adsl$WGTBL/(adsl$HGTBL/100)^2, digits=3)     # in kg/m2
  
  
  # The most widely used is the Du Bois formula,[3][4] which been shown to be 
  # equally as effective in estimating body fat in obese and non-obese patients, 
  # something the Body mass index fails to do
  adsl$BSABL = signif(0.007184*(adsl$WGTBL)^0.425 * (adsl$HGTBL)^0.725, digits=3)  # in M^2
  adsl$BSABL = as_numeric(adsl$BSABL)
  
  #-----------------------------------------------------------------------------   
  # Demographic categorical variables: "SEX" "SEXN" "ETHNIC" "ETHNICN" "RACE" "RACEN"  
  #-----------------------------------------------------------------------------
  # sex.lst = c("MALE", "FEMALE", "UNKNOWN")
  adsl = adsl %>% mutate(SEX_ORG = SEX, 
                         SEX = ordered(toupper(SEX), levels= sex.lst),  
                         SEXN = ifelse(is.na(SEX), -99, as.integer(SEX))# MALE: 1, FEMALE: 2
                      
  )
  
  # ethnic.lst = c("NOT HISPANIC OR LATINO",  "HISPANIC OR LATINO")
  adsl = adsl %>% mutate(ETHNIC_ORG = ETHNIC, 
                         ETHNIC = ordered(toupper(ETHNIC), levels = ethnic.lst),
                         ETHNICN = ifelse(is.na(ETHNIC), -99, as.integer(ETHNIC))
  )
  
  
  # race.lst =c(  "WHITE", 
  #               "BLACK OR AFRICAN AMERICAN",
  #               "ASIAN",
  #               "AMERICAN INDIAN OR ALASKA NATIVE",
  #               "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER",    
  #               "OTHER", 
  #               "UNKNOWN",
  #               "NOT REPORTED")
  adsl = adsl %>% mutate(RACE_ORG = RACE, 
                         RACE = ordered(toupper(RACE), levels = race.lst),
                         RACEN = ifelse(is.na(RACE), -99,  as.integer(RACE)) 
  )
  
  #-----------------------------------------------------------------------------
  # population flag
  #-----------------------------------------------------------------------------     
  # "SAFFL"    safety population flag   
  # "COMPLFL"  completers population flag
  # "RANDFL"   randomized population flag
  # "ENRLFL"   enrolled population flag
  # "PKFL"     PK population flag
  

  
  #---------------------------------------------
  # order columns, and final output
  #---------------------------------------------   
  adsl= adsl[, c(adsl.var.lst, setdiff(colnames(adsl), adsl.var.lst))]
  
  return(adsl) 
  
}


#################################################################
# check_adsl
#################################################################

check_adsl <- function(dataset, topN=20) {
  adsl = dataset%>% ungroup()
  table = NULL
  
  #----------------- 
  # WGTBL
  #-----------------  
  tabl = adsl %>% select(USUBJID, WGTBL) %>% 
    distinct(USUBJID, .keep_all=TRUE) %>% 
    filter(is.na(WGTBL)) %>%  
    arrange(USUBJID)  
  
  if (nrow(tabl)>topN) { tabl = tabl %>% slice(1:topN) }
  attr(tabl, "title") = "List of subject (USUBJID) who do not have his/her baseline weight (WGTBL)" 
  attr(tabl, "key") = "USUBJID"
  attr(tabl, "value") = "WGTBL" 
  if (nrow(tabl)==topN) {attr(tabl, "footnote") = paste0("Note, the default is to display top ", topN, " rows.")}
  table[["WGTBL"]] = tabl
  
  #----------------- 
  # SEX
  #-----------------
  tabl = adsl %>% select(SEX, SEXN, SEX_ORG) %>% 
    distinct(SEX_ORG, .keep_all=TRUE) %>% 
    arrange(SEX)  
  
  if (nrow(tabl)>topN) { tabl = tabl %>% slice(1:topN) }
  attr(tabl, "title") = "List of missing gender information (SEX)" 
  attr(tabl, "key") = "SEX_ORG"
  attr(tabl, "value") = "SEX"
  if (nrow(tabl)==topN) {attr(tabl, "footnote") = paste0("Note, the default is to display top ", topN, " rows.")}
  table[["SEX"]] = tabl
  
  #----------------- 
  # ETHNIC
  #-----------------
  tabl = adsl %>% select(ETHNIC, ETHNICN, ETHNIC_ORG) %>% 
    distinct(ETHNIC_ORG, .keep_all=TRUE) %>% 
    arrange(ETHNIC)
  
  if (nrow(tabl)>topN) { tabl = tabl %>% slice(1:topN) }
  attr(tabl, "title") = "List of missing ethnic information (ETHNIC)" 
  attr(tabl, "key") = "ETHNIC_ORG"
  attr(tabl, "value") = "ETHNIC"
  if (nrow(tabl)==topN) {attr(tabl, "footnote") = paste0("Note, the default is to display top ", topN, " rows.")}
  table[["ETHNIC"]] = tabl
  
  #----------------- 
  # RACE
  #-----------------
  tabl = adsl %>% select(RACE, RACEN, RACE_ORG) %>% 
    distinct(RACE_ORG, .keep_all=TRUE) %>% 
    arrange(RACE)  
  
  if (nrow(tabl)>topN) { tabl = tabl %>% slice(1:topN) }
  attr(tabl, "title") = "List of missing race information (RACE)" 
  attr(tabl, "key") = "RACE_ORG"
  attr(tabl, "value") = "RACE"
  if (nrow(tabl)==topN) {attr(tabl, "footnote") = paste0("Note, the default is to display top ", topN, " rows.")}
  table[["RACE"]] = tabl
  
  return(table)
}


#################################################################
# final output
#################################################################
if (ihandbook) {
  data = NULL
  table = NULL
  
  adsl <-  build_adsl(dataset,  
                      adsl.var.lst = adsl.var.lst, 
                      sex.lst = sex.lst,
                      ethnic.lst =ethnic.lst,
                      race.lst)   
   
  table <- check_adsl(adsl, topN=topN)    
  output <- list(data=adsl, table=table)
}
