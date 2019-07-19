
idebug = 0

#HOME =  "/data/BiCS_RDE_Development/shiny-server_development/pharmacometrics/FYANG/R1500_AngPTL3"
HOME = "~/FYANG/R3918_C5/"
#HOME = "C:/FYANG/R3918/"

setwd(HOME)

##############################################################################
##############################################################################
# build nmdat and narrow it down
##############################################################################
##############################################################################
adsl = read_csv("./KRM/data/adsl.R3918.HV.1659.csv", col_type=cols(.default=col_character()))    # read as character as defualt
adpc = read_csv("./KRM/data/adpc.R3918.HV.1659.csv", col_type=cols(.default=col_character()))    # read as character as defualt
adex = read_csv("./KRM/data/adex.R3918.HV.1659.csv", col_type=cols(.default=col_character()))    # read as character as defualt
adlb = read_csv("./KRM/data/adlb.R3918.HV.1659.csv", col_type=cols(.default=col_character()))    # read as character as defualt

adpx = read_csv("./KRM/data/adpx.R3918.HV.1659.csv")
 
# filter it
# intersect(intersect(intersect(adsl$USUBJID, adpc$USUBJID), adex$USUBJID), adlb$USUBJID)
#nmdat = build_nmdat(adpx %>% filter(USUBJID %in% Reduce(intersect, list(adsl$USUBJID, adpc$USUBJID, adex$USUBJID, adlb$USUBJID))))
nmdat = build_nmdat(adpx %>% filter(USUBJID %in% Reduce(intersect, list(adsl$USUBJID, adpc$USUBJID))))

nmdat = nmdat %>% mutate( 
  C = ifelse(USUBJID %in% "R3918-HV-1659-826-001-127", "C", as.character(C)), 
  CFLAG = ifelse(USUBJID %in% "R3918-HV-1659-826-001-127", "Randomized But Not Dosed", as.character(CFLAG))
)


nmdat = nmdat %>% mutate(DV = ifelse(TEST=="REGN3918", log(as_numeric(DV)), as_numeric(DV)), 
                         DV = ifelse(is.na(DV)|is.infinite(DV), ".", DV), 
                         MDV = ifelse(DV==".", 1, MDV), 
                         
                         CMT = ifelse(TEST=="REGN3918", 2, CMT)
)


nmdat %>%  filter(C!="C", USUBJID %in% "R3918-HV-1659-826-001-057") %>%as.data.frame()
adex %>% filter(USUBJID %in% "R3918-HV-1659-826-001-057")


subj.lst = nmdat %>% filter(WGTBL==".") %>% pull(USUBJID) %>% unique()
adex %>% filter(USUBJID %in% subj.lst) %>% pull(WGTBL)

#-----------------------------------------------------------------------------
# output nmdat
#-----------------------------------------------------------------------------  

col.lst = c("CFLAG",  "STUDYID",  "USUBJID", "USUBJIDORG",  "ARMA", 
            "VISIT", "TIMEPT",  "TEST",  "METHOD",   "STDUNIT",   "SAMDTTM",
            "EXDOSE",   "EXDOSU",   "EXROUTE", "EXSTDTC", "EXENDTC",  
            "AGEU",  "SEX",   "RACE",  "ETHNIC")

name_lst = colnames(nmdat)
ids = which(name_lst %in% col.lst)
name_lst[ids] = paste0(name_lst[ids], "=DROP")  #, collapse=" ")
paste0(name_lst, collapse=" ")


tt = nmdat # %>%filter(C==".", TEST %in% c("REGN3918",  "."))   # "C5"       "AH50"     "CH50"     "REGN3918" "." 
write_csv(tt, "./KRM/data/nmdat_PKPD_0118_2019.csv")



#----------------------------------------------------------------------------- 
# the stats for the covariates
#----------------------------------------------------------------------------- 
nmdat %>% distinct(USUBJID,.keep_all=TRUE) %>% pull(WGTBL) %>%as_numeric() %>% summary()

nmdat %>% distinct(USUBJID,.keep_all=TRUE) %>% pull(C5BL) %>%as_numeric() %>% summary()

nmdat %>% distinct(USUBJID,.keep_all=TRUE) %>% pull(CH50HBL) %>%as_numeric() %>% summary()




 
     



 
