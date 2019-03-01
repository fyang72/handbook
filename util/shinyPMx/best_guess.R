

# 
# library(readxl)
# file.name = "H:\\SHINY\\shiny_0925\\pmx\\lib\\pkmeta.xlsx"
# magicTab <- read_excel(file.name, sheet="cheatsheet")
# magicTab <- magicTab %>% mutate(Domain=as.character(Domain), 
#                                 Alias=as.character(Alias), 
#                                 Freq=as.numeric(Freq), 
#                                 Label=as.character(Label), 
#                                 Where=as.character(Where), 
#                                 Who=as.character(Who))


library(haven)
#data = read_sas("H:\\SHINY\\shiny_0925\\data\\R1500\\R1500-CL-1321\\adsl.sas7bdat")

library(dplyr)
VARS <- function(data, vars.lst=c("USUBJID", "ARMA"), Where="indivProfile", Who="feng.yang")  {
  tt=globalVars$magicTab %>% filter(Domain %in% vars.lst, Alias %in% colnames(data),   Who%in%Who) %>%  
    group_by(Domain) %>% arrange(Domain, -Freq) %>% slice(1) %>% as.data.frame()
  rownames(tt) = tt$Domain
  
  alias.lst = tt[as.character(vars.lst), "Alias"]
  return(factor(alias.lst, levels=c(NA, colnames(data))))
}

# VARS(data, vars.lst=c("USUBJID", "ARMA", "TIMEPT", "WGTBL", "SEX", "BMIBL"))



push2MagicTab <- function(magicTab, Domain="DVOR", Alias="XYZ", Label="", Where="", Who="feng.yang") {
  
  stopifnot(length(Domain)==length(Alias))
  for (i in 1:length(Domain)) {
      iDomain = Domain[i]
      iAlias = Alias[i]
      tt = magicTab %>% filter(Domain==iDomain, Alias==iAlias,   Who==Who)
    if (nrow(tt)>0) { 
      ids = which(magicTab$Domain==iDomain &  magicTab$Alias==iAlias)
      magicTab[ids, "Freq"] = magicTab[ids, "Freq"] + 1
    }else{
      tt=  data.frame(Domain=iDomain,Alias=iAlias, Freq=1, Label=Label, Where=Where, Who=Who, stringsAsFactors = FALSE)
      magicTab <- bind_rows(magicTab, tt )
    }
  }
  
  return(magicTab)
  
}

#magicTab <- push2MagicTab(magicTab, Domain=c("STUDYID", "ARMA"), Alias=c("99", "888"), Label="")
#magicTab  %>% filter(Domain %in% c("STUDYID", "ARMA"))
#magicTab %>% as.data.frame()




#-------------------------------------------------------------------------------
#### adsl ####
#-------------------------------------------------------------------------------
STUDYID_VARS <- function(data)  {
  if(is.null(data)) {return(NULL)}
  col.lst = colnames(data) 
  ifelse("STUDYID"%in% col.lst, "STUDYID", "") }




USUBJID_VARS <- function(data)  {
  if(is.null(data)) {return(NULL)}
  col.lst = colnames(data) 
  ifelse("USUBJID"%in%col.lst, "USUBJID", 
         ifelse("SUBJID"%in%col.lst, "SUBJID", 
                ifelse("CLID"%in%col.lst, "CLID",""))) }





ARMA_VARS <- function(data)  {
  if(is.null(data)) {return(NULL)}
  col.lst = colnames(data) 
  ifelse("ARMA"%in%col.lst, "ARMA", 
         ifelse("DOSECH"%in%col.lst, "DOSECH",          
           ifelse("TRTA"%in%col.lst, "TRTA", 
                  ifelse("TRT01A"%in%col.lst, "TRT01A", 
                         ifelse("TRTGROUP"%in%col.lst, "TRTGROUP",""))))) }



WEIGHTBL_VARS <- function(data)  {
  if(is.null(data)) {return(NULL)}
  col.lst = colnames(data) 
  ifelse("WEIGHTBL"%in%col.lst, "WEIGHTBL", 
         ifelse("WGTBL"%in%col.lst, "WGTBL", 
                ifelse("WT"%in%col.lst, "WT", 
                       ifelse("WGT"%in%col.lst, "WGT","")))) }


HEIGHTBL_VARS <- function(data)  {
  if(is.null(data)) {return(NULL)}
  col.lst = colnames(data) 
  ifelse("HEIGHTBL"%in%col.lst, "HEIGHTBL", 
         ifelse("HGTBL"%in%col.lst, "HGTBL", 
                ifelse("HT"%in%col.lst, "HT", 
                       ifelse("HGT"%in%col.lst, "HGT","")))) }

AGE_VARS <- function(data)  {
  if(is.null(data)) {return(NULL)}
  col.lst = colnames(data) 
  ifelse("AGE"%in%col.lst, "AGE", "") }



BMIBL_VARS <- function(data)  {
  if(is.null(data)) {return(NULL)}
  col.lst = colnames(data) 
  ifelse("BMIBL"%in%col.lst, "BMIBL", 
         ifelse("BMI"%in%col.lst, "BMI",""))  } 

BSABL_VARS <- function(data)  {
  if(is.null(data)) {return(NULL)}
  col.lst = colnames(data) 
  ifelse("BSABL"%in%col.lst, "BSABL", 
         ifelse("BSA"%in%col.lst, "BSA",""))  } 

SEX_VARS <- function(data)  {
  if(is.null(data)) {return(NULL)}
  col.lst = colnames(data) 
  ifelse("SEX"%in%col.lst, "SEX", 
         ifelse("GENDER"%in%col.lst, "GENDER",""))  } 

ETHNIC_VARS <- function(data)  {
  if(is.null(data)) {return(NULL)}
  col.lst = colnames(data) 
  ifelse("ETHNIC"%in%col.lst, "ETHNIC", "") } 


RACE_VARS <- function(data)  {
  if(is.null(data)) {return(NULL)}
  col.lst = colnames(data) 
  ifelse("RACE"%in%col.lst, "RACE", "")} 


COUNTRY_VARS <- function(data)  {
  if(is.null(data)) {return(NULL)}
  col.lst = colnames(data) 
  ifelse("COUNTRY"%in%col.lst, "COUNTRY", "")  }  


SITEID_VARS <- function(data)  {
  if(is.null(data)) {return(NULL)}
  col.lst = colnames(data) 
  ifelse("SITEID"%in%col.lst, "SITEID", "")}    

#-------------------------------------------------------------------------------
#### adpc / adpd /adex  ####
#-------------------------------------------------------------------------------

TIME_VARS <- function(data)  {
  if(is.null(data)) {return(NULL)}
  col.lst = colnames(data)
  ifelse("TIME" %in% col.lst, "TIME",
         ifelse("PKDAY_C" %in% col.lst, "PKDAY_C", ""))
}

NTIM_VARS <- function(data)  {
  if(is.null(data)) {return(NULL)}  
  col.lst = colnames(data)
  ifelse("NTIM" %in% col.lst, "NTIM", 
         ifelse("NOM_DAY" %in% col.lst, "NOM_DAY", ""))
}

DVOR_VARS <- function(data)  {
  if(is.null(data)) {return(NULL)}  
  col.lst = colnames(data)
  ifelse("DVOR" %in% col.lst, "DVOR", 
         ifelse("DV" %in% col.lst, "DV", 
                ifelse("IPRED" %in% col.lst, "IPRED", 
                       ifelse("CONCN" %in% col.lst, "CONCN",
                              ifelse("RESULT" %in% col.lst, "RESULT", 
                                     ifelse("Mean" %in% col.lst, "Mean", 
                                            ifelse("RESC_RAW"%in%col.lst, "RESC_RAW",
                                                   ifelse("RESC"%in%col.lst, "RESC",
                                                          ""))))))))
}

  
VISIT_VARS <- function(data)  {
  if(is.null(data)) {return(NULL)}  
  col.lst = colnames(data)
  ifelse("VISIT"%in%col.lst, "VISIT", "") 
}



TIMEPT_VARS <- function(data)  {
  if(is.null(data)) {return(NULL)}  
  col.lst = colnames(data)
  ifelse("TIMEPT"%in%col.lst, "TIMEPT", "") 
}



INFHR_VARS <- function(data)  {
  if(is.null(data)) {return(NULL)}  
  col.lst = colnames(data)
  ifelse("INFHR"%in%col.lst, "INFHR", "")
}


EXTRT_VARS <- function(data)  {
  if(is.null(data)) {return(NULL)}  
  col.lst = colnames(data)
  ifelse("EXTRT"%in%col.lst, "EXTRT", "") 
}


EXROUTE_VARS <- function(data)  {
  if(is.null(data)) {return(NULL)}  
  col.lst = colnames(data)
  ifelse("EXROUTE"%in%col.lst, "EXROUTE", 
         ifelse("ROUTE"%in%col.lst, "ROUTE", ""))
}


EXDOSE_VARS <- function(data)  {
  if(is.null(data)) {return(NULL)}  
  col.lst = colnames(data)
  ifelse("EXDOSE"%in%col.lst, "EXDOSE", 
         ifelse("DOSE"%in%col.lst, "DOSE","")) 
}


EXDOSU_VARS <- function(data)  {
  if(is.null(data)) {return(NULL)}  
  col.lst = colnames(data)
  ifelse("EXDOSU"%in%col.lst, "EXDOSU", 
         ifelse("DOSU"%in%col.lst, "DOSU", ""))
}


EXSTDTC_VARS <- function(data)  {
  if(is.null(data)) {return(NULL)}  
  col.lst = colnames(data)
  ifelse("EXSTDTC"%in%col.lst, "EXSTDTC", "")
}


EXENDTC_VARS <- function(data)  {
  if(is.null(data)) {return(NULL)}  
  col.lst = colnames(data)
  ifelse("EXENDTC"%in%col.lst, "EXENDTC", "") 
}


TRTSTDTM_VARS <- function(data)  {
  if(is.null(data)) {return(NULL)}  
  col.lst = colnames(data)
  ifelse("TRTSTDTM"%in%col.lst, "TRTSTDTM", 
         ifelse("TRTSDTM"%in%col.lst, "TRTSDTM",""))
  
} 


TRTENDTM_VARS <- function(data)  {
  if(is.null(data)) {return(NULL)}  
  col.lst = colnames(data)
  ifelse("TRTENDTM"%in%col.lst, "TRTENDTM", 
         ifelse("TRTEDTM"%in%col.lst, "TRTEDTM","")) 
}



SAMDTTM_VARS <- function(data)  {
  if(is.null(data)) {return(NULL)}  
  col.lst = colnames(data)
  ifelse("SAMDTTM"%in%col.lst, "SAMDTTM", "")
}
 

SOP_VARS <- function(data)  {
  if(is.null(data)) {return(NULL)}  
  col.lst = colnames(data)
  ifelse("SOP"%in%col.lst, "SOP", "") 
}
 

TEST_VARS <- function(data)  {
  if(is.null(data)) {return(NULL)}  
  col.lst = colnames(data)
  ifelse("TEST"%in%col.lst, "TEST", "") 
}


STDUNIT_VARS <- function(data)  {
  if(is.null(data)) {return(NULL)}  
  col.lst = colnames(data)
  ifelse("STDUNIT"%in%col.lst, "STDUNIT", 
         ifelse("UNIT"%in%col.lst, "UNIT", "")) 
}


BLQ_VARS <- function(data)  {
  if(is.null(data)) {return(NULL)}  
  col.lst = colnames(data)
  ifelse("BLQ"%in%col.lst, "BLQ", "") 
}


LLOQ_VARS <- function(data)  {
  if(is.null(data)) {return(NULL)}  
  col.lst = colnames(data)
  ifelse("LLOQ"%in%col.lst, "LLOQ", "") 
}
