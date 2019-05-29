

 source("./script/build_adsl.R")
 source("./script/build_adpx.R")
 source("./script/build_adex.R")
 source("./script/build_adpc.R")
 source("./script/build_nmdat.R")

 
  
 adsl=read_sas("./data/adsl.sas7bdat")  # %>% filter(TESTCAT=="RESP1")
 
 
 
 adex=read_sas("./data/adex.sas7bdat") %>%  # %>% filter(TESTCAT=="RESP1")
    mutate(EXSTDTC = paste0(EXSTDT, "H", EXSTTM ), 
           EXENDTC = paste0(EXSTDT, "H", EXENTM )
    )%>% 
   mutate(ARMA=TRTA, 
          ARMAN=TRTAN,
          EXDOSU = gsub("mL", "mg", EXDOSU, fix=TRUE), 
          EXROUTE = ifelse(EXDOSU=="mg/kg", "INTRAVENOUS", 
                           ifelse(EXDOSU=="mg", "SUBCUTANEOUS", NA)), 
          EXROUTN = ifelse(EXDOSU=="mg/kg", 2, 
                           ifelse(EXDOSU=="mg", 1, EXROUTE)) 
   )
  
 adpc=read_csv("./data/adpc.csv", col_type=cols(.default=col_character())) %>%  # %>% filter(TESTCAT=="RESP1")
   mutate(USUBJID = CLID, 
          ARMA = paste0(doseplan_lim, " ", DOSU),
          PCTPT = TIMEPT, 
          DVOR = RESC, 
          DVORU = STDUNIT
   )
  
 
 
 
 adsl = build_adsl(adsl,param=NULL)
 adex = build_adex(adex)
 adpc = build_adpc(adpc)
 
 
 nmdat = build_adpx(adsl, adex, adpc)
 