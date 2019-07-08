
########################################################################
# build_adsl
########################################################################
build_adsl <-function(dataset) { 

  # must have these desired variables, if missing, fill with NA 
  adsl = dataset %>% fillUpCol_df(adsl_var_lst) 
  
  #--------------------------------------------------------------
  # Analysis identifiers
  #--------------------------------------------------------------
  # STUDYID
  adsl <- adsl %>% mutate(STUDYID = STUDYID %>% as.character())
  
  # USUBJID
  if (all(is.na(adsl$USUBJID))) {
    print("error: all(is.na(USUBJID))=TRUE")}
  
  adsl <- adsl %>% mutate(
    USUBJID = standardise_USUBJID(STUDYID, USUBJID) %>% 
      as.character()
    )
   
    
  #--------------------------------------------------------------
  # Analysis Treatment Variables 
  #--------------------------------------------------------------
  # "ARM"      "ARMN"      planned ARM
  # "TRT01P"   "TRT01PN"   planned treatment
  # "TRT01A"   "TRT01AN"   actual treatment  
  # 
  # "TRTSDT"   "TRTSTM"   "TRTSDTM"  date/time of start treatment 
  # "TRTEDT"   "TRTETM"   "TRTEDTM"  date/time of end of treatment 
  
  # Treatment information will be included in adex dataset
  
  #--------------------------------------------------------------
  # Demographic continous variables:  
  # "AGE"  "WGTBL" "HGTBL"  "BMIBL" "BSABL" 
  #--------------------------------------------------------------
  adsl = adsl %>% mutate(AGE = as_numeric(AGE), 
                         WGTBL = as_numeric(WGTBL), 
                         HGTBL = as_numeric(HGTBL))    
   
  # Commonly accepted BMI ranges are underweight: 
  #   under          18.5, 
  #   normal weight: 18.5 to 25, 
  #   overweight:    25 to 30, 
  #   obese: over    30.
  adsl <- adsl %>% mutate(
      BMIBL = signif(WGTBL/(HGTBL/100)^2, digits=3) %>% as_numeric()
    )  # in kg/m2
    
  
  # The most widely used is the Du Bois formula,[3][4] which been shown to be 
  # equally as effective in estimating body fat in obese and non-obese patients, 
  # something the Body mass index fails to do
  adsl <- adsl %>% mutate(
    BSABL = signif(0.007184*(WGTBL)^0.425*(HGTBL)^0.725, digits=3) %>%  # in M^2
    as_numeric()
    )
  
  #--------------------------------------------------------------  
  # Demographic categorical variables: 
  # "SEX" "SEXN" "ETHNIC" "ETHNICN" "RACE" "RACEN"  
  #--------------------------------------------------------------
  # sex_var_lst = c("MALE", "FEMALE", "UNKNOWN")
  # # MALE: 1, FEMALE: 2
  adsl = adsl %>% mutate(
    #SEX = fuzzy_match(toupper(SEX), sex_var_lst, 
    #         fuzzy_match_method, fuzzy_match_threshold
    #        ),  
    SEX = ordered(toupper(SEX), levels=sex_var_lst), 
    SEXN = ifelse(is.na(SEX), -99, as.integer(SEX)), 
    SEX = as.character(SEX)
                      
  )
 
  # ethnic_var_lst = c("NOT HISPANIC OR LATINO",  "HISPANIC OR LATINO")
  adsl = adsl %>% mutate(
    #ETHNIC = fuzzy_match(toupper(ETHNIC), ethnic_var_lst, 
    #                     fuzzy_match_method, fuzzy_match_threshold
    #),
    ETHNIC = ordered(toupper(ETHNIC), levels=ethnic_var_lst), 
    ETHNICN = ifelse(is.na(ETHNIC), -99, as.integer(ETHNIC)), 
    ETHNIC = as.character(ETHNIC)
  )
  
  
  # race_var_lst =c(  "WHITE", 
  #               "BLACK OR AFRICAN AMERICAN",
  #               "ASIAN",
  #               "AMERICAN INDIAN OR ALASKA NATIVE",
  #               "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER",    
  #               "OTHER", 
  #               "UNKNOWN",
  #               "NOT REPORTED")
  adsl = adsl %>% mutate(
    #RACE = fuzzy_match(toupper(RACE), race_var_lst, 
    #                   fuzzy_match_method, fuzzy_match_threshold
    #),
  RACE = ordered(toupper(RACE), levels=race_var_lst), 
  RACEN = ifelse(is.na(RACE), -99,  as.integer(RACE)), 
  RACE = as.character(RACE)
  )
  
  #--------------------------------------------------------------
  # population flag
  #--------------------------------------------------------------   
  # "SAFFL"    safety population flag   
  # "COMPLFL"  completers population flag
  # "RANDFL"   randomized population flag
  # "ENRLFL"   enrolled population flag
  # "PKFL"     PK population flag

  
  #--------------------------------------------------------------
  # order columns, and final output
  #--------------------------------------------------------------  
  # put variables (tier=1 or 2 fisrt)
  adsl <- adsl[, c(adsl_var_lst, setdiff(colnames(adsl), adsl_var_lst))]
  adsl <- convert_vars_type(adsl, adsl_data_type)
  adsl <- adsl %>% dplyr::arrange(STUDYID, USUBJID) 
  adsl <- adsl %>% ungroup()
  
  return(adsl) 
}


########################################################################
# check_adsl
########################################################################

check_adsl <- function(dataset, adsl, topN=20) {
  adsl <- adsl %>% ungroup()
  
  dataset = dataset %>% ungroup() %>% 
    rename_at(vars(colnames(dataset)),
              ~ paste0(colnames(dataset), "_ORG")
              ) 
  adsl = bind_cols(adsl, dataset)
  
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
  if (nrow(tabl)>topN) {attr(tabl, "footnote") = paste0("Note, the default is to display top ", topN, " rows.")}
  table[["WGTBL"]] = tabl
  
  #----------------- 
  # SEX
  #-----------------
  tabl = adsl %>% select(SEX, SEX_ORG) %>% 
    distinct(SEX_ORG, .keep_all=TRUE) %>% 
    arrange(SEX)  
  
  if (nrow(tabl)>topN) { tabl = tabl %>% slice(1:topN) }
  attr(tabl, "title") = "List of gender information (SEX)" 
  attr(tabl, "key") = "SEX_ORG"
  attr(tabl, "value") = "SEX"
  
  attr(tabl, "footnote") = paste0("Note, the standarad names for gender are ", paste0(sex.lst, collapse=", "), ". ")  
  if (nrow(tabl)>topN) {attr(tabl, "footnote") = paste0(attr(tabl, "footnote"), "The default is to display top ", topN, " rows.")}
  table[["SEX"]] = tabl
  
  #----------------- 
  # ETHNIC
  #-----------------
  tabl = adsl %>% select(ETHNIC, ETHNIC_ORG) %>% 
    distinct(ETHNIC_ORG, .keep_all=TRUE) %>% 
    arrange(ETHNIC)
  
  if (nrow(tabl)>topN) { tabl = tabl %>% slice(1:topN) }
  attr(tabl, "title") = "List of ethnic information (ETHNIC)" 
  attr(tabl, "key") = "ETHNIC_ORG"
  attr(tabl, "value") = "ETHNIC"

  attr(tabl, "footnote") = paste0("Note, the standarad names for ethnic are ", paste0(ethnic.lst, collapse=", "), ". ")  
  if (nrow(tabl)>topN) {attr(tabl, "footnote") = paste0(attr(tabl, "footnote"), "The default is to display top ", topN, " rows.")}
  table[["ETHNIC"]] = tabl
  
  #----------------- 
  # RACE
  #-----------------
  tabl = adsl %>% select(RACE, RACE_ORG) %>% 
    distinct(RACE_ORG, .keep_all=TRUE) %>% 
    arrange(RACE)  
  
  if (nrow(tabl)>topN) { tabl = tabl %>% slice(1:topN) }
  attr(tabl, "title") = "List of race information (RACE)" 
  attr(tabl, "key") = "RACE_ORG"
  attr(tabl, "value") = "RACE"
  
  attr(tabl, "footnote") = paste0("Note, the standarad names for race are ", paste0(race.lst, collapse=", "), ". ")  
  if (nrow(tabl)>topN) {attr(tabl, "footnote") = paste0(attr(tabl, "footnote"), "The default is to display top ", topN, " rows.")}
  table[["RACE"]] = tabl
  
  return(table)
}


########################################################################
# final output
########################################################################
if (ihandbook) {
  data = NULL
  table = NULL
  
  adsl <-  build_adsl(dataset)   
  table <- check_adsl(dataset, adsl, topN=topN)     # dataset: original one, # adsl: parsed one
  output <- list(data=adsl, table=table)
}
