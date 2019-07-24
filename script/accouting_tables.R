################################################################################
# Individual time profile
################################################################################

accouting_tables <-function(dataset, params=NULL) {
  
  # initialize variables
  figure=NULL
  table =NULL
  data = NULL
   
  
  #------------------------------
  # these key varaibles needed
  #------------------------------
  key.column.lst <- c("STUDYID", "USUBJID", "ARMA", "NTIM", "TIMEPT", 
                      "TESTCD", "DVOR", "TIME", "LLOQ", "EXTRT")
  missing.column.lst <- 
    key.column.lst[which(!key.column.lst %in% colnames(dataset))]
  
  #message <- paste0("missing variable(s) of ", 
  # paste0(missing.column.lst, sep=", ", collapse=""))
  
  #validate(
  #  need(all(key.column.lst %in% colnames(dataset)), message=message)
  #)
  
##########################################################################################
# Overall population and samples 
##########################################################################################


# make ARMA as a ordered factor

tdata <- dataset %>% convert_vars_type(nmdat_data_type)

tdata = tdata %>% 
  #filter(as_numeric(TIME)>=0) %>%    # Do we want to exclude all pre-dose sampels, as the default?
  filter(TEST!=".", !is.na(DVOR)) 


study_name = ifelse(
  !"STUDYID" %in% colnames(tdata), "STUDYID",
  paste0(tdata %>% drop_na(STUDYID) %>% pull(STUDYID) %>% unique(), collapse=", ")
)

drug_name = ifelse(
  !"EXTRT" %in% colnames(tdata), "EXTRT",
  paste0(tdata %>% drop_na(EXTRT) %>% pull(EXTRT) %>% unique(), collapse=", ")
)

test_name = ifelse(
  !"TESTCD" %in% colnames(tdata), "TESTCD",
  paste0(tdata %>% drop_na(TESTCD) %>% pull(TESTCD) %>% unique(), collapse=", ")
)

test_label = ifelse(
  !"TEST" %in% colnames(tdata), "TEST",
  paste0(tdata %>% drop_na(TEST) %>% pull(TEST) %>% unique(),  collapse=", ")
)


#----------------------------------------------------------------------------
# Table accounting_subject:  Accounting of All Subjects by Analyte, Treatment Group, and Overall  
#----------------------------------------------------------------------------

tabl = tdata %>% group_by(TEST, ARMA) %>% summarise(N=length(unique(USUBJID)) ) %>% 
  spread(ARMA, N) %>% ungroup()

tabl = cbind(tabl,  Overall =  rowSums(tabl%>%select(-TEST)))
tabl


attr(tabl, 'title') <- paste0("Accounting of Subjects by Analyte, Treatment Group, and Overall (", study_name, ")") # 
table[["accounting_subject"]]  = tabl 

#----------------------------------------------------------------------------
# Table accounting_sample: Accounting of All Samples by Analyte, Treatment Group, and Overall
#----------------------------------------------------------------------------
tabl = tdata %>% group_by(TEST, ARMA) %>% summarise(N=length(DVOR) )    %>% 
  spread(ARMA, N) %>% ungroup()


t1 = tdata %>% group_by(TEST) %>% summarise(N=length(DVOR), 
                                            Quantifiable.samples = length(which(DVOR!=0))) %>% 
  ungroup()


tabl = cbind(cbind(tabl,  TOTAL =  rowSums(tabl%>%select(-TEST))), Quantifiable=t1$Quantifiable.samples)
tabl = tabl %>% mutate(Quantifiable=paste0(Quantifiable, "(", u.signif(Quantifiable/TOTAL*100, digits=3), "%)"))
tabl


attr(tabl, 'title') <- paste0("Accounting of All Samples by Analyte, Treatment Group, and Overall (", study_name, ")") # 
table[["accounting_sample"]]  = tabl 


#----------------------------------------------------------------------------
# Table accounting_sample: Accounting of All Samples by Analyte, Treatment Group, and Overall
#----------------------------------------------------------------------------
tabl <- summary_of_studies(tdata, 
                           group_by=c("STUDYID", "ARMA"), 
                           value="DVOR", 
                           id="USUBJID"
)

caption = 'Summary of Population Included in the Study'
#knitr::kable(tabl, booktabs = TRUE, caption=caption)      #, padding = 2

attr(tabl, 'title') <- caption
table[["accounting_sample_2"]]  = tabl 


return(list(figure=figure, table=table ))
}

#################################################################
# final output
#################################################################
if (ihandbook) {
  output = suppressWarnings(
    filtered_dataset() %>% accouting_tables(params=NULL)
  )
  
}
