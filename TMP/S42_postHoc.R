# Version 0.1   Created on 11/08/2018, Feng Yang
# 
#----------------------------------------------------------------------------
# What are needed 
# 1) a model file (.cpp)
# 2) nmdat:  nonmem dataset; 
# 3) xpdb:  nonmem output results with individual parameters 

# Goals: Bayesian Approach
# 0) validate the cpp model against xpdb (NONMEM output file)
# 1) use actual dose regimens, calculate post-hoc exposure  
# 2) use ideal dose regimens, to assess covariate Effects on Exposure  
#----------------------------------------------------------------------------
# 
  # Load model file
  #---------------------
  source("./cpp/mod_R3918_human_MM_final.cpp")
  mod <- mread("R2810", tempdir(),mymodel)     # ?mread , ?mcode
  
  
  # Load nmdat 
  #---------------------
  nmdat = read_csv("./data/nmdat_PKPD_1024_2018.csv", 
                   col_type=cols(.default=col_character())) %>%   # read as character as defualt
    mutate(ROWID = as.integer(ROWID), 
           ID = as.integer(ID), 
           ARMA = gsub("_", " ", ARMA, fix=TRUE),
           ARMA = ordered(ARMA, levels=unique(as.character(ARMA))), 
           ARMAN = as.integer(ARMA), 
           DVOR = as_numeric(DVOR), 
           TIME = as_numeric(TIME), 
           NTIM = as_numeric(NTIM),
           
           LLOQ = as_numeric(LLOQ)
    )%>% 
    filter(as_numeric(TIME)>=0)   # Do we want to exclude all pre-dose sampels, as the default?
  
  
  # Load xpdb 
  #---------------------
  # see "modelDiagnostic.R"
  
  
  
  fig = empty_canvas(label="Cpp Model\n Validation")
  attr(fig, 'title') <-  ""
  FIGURE_ALL[["cppModel_validation"]] = fig 
  
  
  
  #############################################################################
  #############################################################################
  # Goal 1: use actual dose regimens, calculate post-hoc exposure  
  #############################################################################
  #############################################################################
  library("mrgsolve") 
  
  adex = nmdat %>%  filter(EVID==1) %>% 
    select(STUDYID, USUBJID, ID, ARMA, TIME, AMT, EXROUTE, RATE, EVID, CMT, MDV, TESTN, CH50HBL, C5BL, WGTBL) %>% 
    mutate(ARMA = gsub("-", " ", ARMA, fixed=TRUE))
  
  # add individual parameters
  adex = adex %>% left_join(xpdb %>% 
                                select(ID, one_of(setdiff(colnames(xpdb), colnames(adex)))) %>%  # no need of WGT_ON_CLQ WGT_ON_VSS
                                distinct(ID, .keep_all=TRUE), 
                              by="ID")  
  
  # assign individual parameters as poulation parameters
  adex=adex %>% mutate(POP="POSTHOC",
                       MDV = 1, 
                       TESTN=1, 
                       ID = as.integer(ID), 
                       TIME = as_numeric(TIME), 
                       AMT = as_numeric(AMT),  ################
                       RATE = as_numeric(RATE), 
                       EVID = as.integer(EVID), 
                       CMT = as.integer(CMT), 
                       MDV = as.integer(MDV), 
                       CH50HBL = as_numeric(CH50HBL), 
                       C5BL = as_numeric(C5BL) 
                       )  
    
  
  #---------------------------------------- 
  # 0) validate the cpp model against xpdb (NONMEM output file)  
  # ---------------------------------------
   
  # order ARMA for plotting purpose
  arma.lst = c("1 mg/kg IV",   "3 mg/kg IV",   "10 mg/kg IV",  "30 mg/kg IV",  
               "300 mg SC",    "600 mg SC",   "400 mg SC QW")
  
  # run on individual model parameters
  simData_IPRED <- 
    runSim_by_adpx(mod %>% zero_re %>% param(COVARIATE=0, SCALING=0),
                   adex %>% select(-starts_with("TV")) %>% 
                     rename(TVCL = CL, 
                            TVV2 = V2, 
                            TVQ = Q, 
                            TVV3 = V3, 
                            TVF1 = F1, 
                            TVKA = KA, 
                            TVVMAX = VMAX, 
                            TVKSS = KSS, 
                            TVEC50 = EC50, 
                            TVGAMMA = GAMMA, 
                            TVEMAX = EMAX
                     )) %>% 
    mutate(NTIM = TIME,
           IPRED = CENTRAL/V2, 
           IPRED = ifelse(IPRED<0.078, 0.078, IPRED), 
           DVOR = IPRED, 
           ARMA = ordered(ARMA, levels = arma.lst)
    )
  
  # run on population model parameters
  simData_PRED <- runSim_by_adpx(mod %>% zero_re %>% param(COVARIATE=0, SCALING=0),
                                 adex) %>%  
    mutate(NTIM = TIME,
           IPRED = CENTRAL/V2, 
           IPRED = ifelse(IPRED<0.078, 0.078, IPRED), 
           DVOR = IPRED, 
           ARMA = ordered(ARMA, levels = arma.lst)
    )
  
  
   
  
  
  
  #---------------------------------------- 
  # priminary PK plot     
  # ---------------------------------------
  tdata = nmdat %>% filter(TEST=="REGN3918") %>% filter(ARMA=="30 mg/kg IV", USUBJID=="R3918-HV-1659-826-001-147")
  
  fig = ggplot() + 
     geom_point(data = tdata, aes(x=as_numeric(TIME),y=as_numeric(DVOR)))  + 
    
     geom_line(data = xpdb%>%filter(TEST=="REGN3918") %>% filter(ARMA=="30 mg/kg IV", USUBJID=="R3918-HV-1659-826-001-147") %>%
                 mutate(DVOR = (DV), PRED = (PRED), IPRED = (IPRED)),  aes(x=TIME, y=IPRED, group=USUBJID),  col="blue" ) +   # from S03_modelDiagnostic
     geom_line(data = xpdb%>%filter(TEST=="REGN3918") %>% filter(ARMA=="30 mg/kg IV", USUBJID=="R3918-HV-1659-826-001-147")  %>%
                 mutate(DVOR = (DV), PRED = (PRED), IPRED = (IPRED)),  aes(x=TIME, y=PRED, group=USUBJID),  col="green" ) + 
    # 
    
    geom_line(data=simData_IPRED %>% filter(ARMA=="30 mg/kg IV", USUBJID=="R3918-HV-1659-826-001-147"), 
              aes(x=TIME, y=IPRED, group=USUBJID, col="IPRED")  ) + 
    
    geom_line(data=simData_PRED %>% filter(ARMA=="30 mg/kg IV", USUBJID=="R3918-HV-1659-826-001-147"), 
              aes(x=TIME, y=IPRED, group=USUBJID, col="PRED") ) + 
    
    xlab("Time (day)") + 
    ylab("Population Predicted/observed Concentration (mg/L)") + 
    
    theme_bw() + base_theme(font.size = as.integer(12)) + 
    guides(col=guide_legend(ncol=4,byrow=TRUE))    
  fig
  
  fig = fig + facet_wrap(~ARMA)
  attr(fig, 'title') <- "Predicted/observed Time Profiles of REGN1500 in Linear Scale"
  attr(fig, 'width') <- 8
  attr(fig, 'height') <- 6
  FIGURE_ALL[["sim_CppModel_validation_PK_LN"]] = fig  
  
  # log scale
  fig = fig + facet_wrap(~USUBJID) + 
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^round(x, digits=0)), labels=trans_format("log10", math_format(10^.x))) + 
    annotation_logticks(sides ="l")
  attr(fig, 'title') <- "Population Predicted Time Profiles of REGN1500 in Log Scale"
  attr(fig, 'width') <- 8
  attr(fig, 'height') <- 6
  FIGURE_ALL[["sim_CppModel_validation_PK_LOG"]] = fig   
  
  
  
  
  
  #---------------------------------------- 
  # priminary PD plot, similar for PD    
  # --------------------------------------- 
  # omitted
  
  
  
  #---------------------------------------
  #Steady-state PK parameters
  #---------------------------------------
  treat_end = 24*14;  sim_end = 28*14
    
  OUT <-  simData_IPRED %>%  #filter(TIME>=(treat_end-28) & TIME<=(treat_end))  # for the Steady state 
    filter(EVID==0) %>% 
    group_by_(.dots =c("STUDYID", "ARMA", "EXROUTE", "USUBJID", "CL", "V2", "V3", "Q")) %>%   
    dplyr::summarise( 
      Cmin = DVOR[n()],  # the last conc at this time interval
      Cmax = max(DVOR),
      Time2Cmax =  TIME[DVOR==max(DVOR)],  #  paste(TIME[DVOR==max(DVOR,na.rm=TRUE)], collapse="_"),   #  # 
      AUCinf = auc_partial(TIME, DVOR)#,
    )   %>% ungroup() %>%
    
    mutate( VSS = V2 + V3,  # volume of distribution
            KE = CL/V2,   # calculate half life
            K32 = Q/V3, 
            K23 = Q/V2,
            alpha.plus.beta  = KE + K23 + K32,
            alpha.multi.beta = KE * K32,
            alpha =(alpha.plus.beta + sqrt(alpha.plus.beta^2-4*alpha.multi.beta))/2,
            beta  = (alpha.plus.beta - sqrt(alpha.plus.beta^2-4*alpha.multi.beta))/2,
            alpha.half.life = 0.693/alpha,
            beta.half.life = 0.693/beta
    ) %>% select(-V2, -V3, -KE, -K32, -K23, -alpha.plus.beta, -alpha.multi.beta, -alpha, -beta)
  
  OUT %>% filter(EXROUTE=="SUBCUTANEOUS")  %>% pull(Time2Cmax)  %>% summary()   # "INTRAVENOUS"  "SUBCUTANEOUS"
   
  # calculate exposure metrics fo each individual 
  OUT = OUT %>% gather(PARAMS, DVOR, -STUDYID, -ARMA, -EXROUTE,-USUBJID)
  tabl =  OUT %>% calc_stats(id="USUBJID", group_by=c("STUDYID", "ARMA", "PARAMS"), value="DVOR") %>% 
    select( STUDYID, PARAMS, ARMA, N, Mean_CV, SE, SD, Median, PCT2P5, PCT97P5, GEOmean, GEOSD)  
  
  # choose the metrics for output
  metrics.lst = c("CL", "VSS", "Cmin",  "Cmax", "Time2Cmax", "AUCinf" )
  
  tabl = tabl %>% ungroup() %>% filter(PARAMS %in% metrics.lst) %>%  
    mutate(PARAMS=ordered(PARAMS, levels=metrics.lst) ) %>%  
    filter(!is.na(PARAMS)) %>% 
    arrange(PARAMS)  %>% 
    select(-STUDYID)
  
  tabl4.NCA.PARAMS <- function(tabl) {
    # cosmetic 
    #------------------------
   # Median_CI95 and GEOmean_SD
    tabl = tabl %>% mutate(Median_CI95=paste0(u.signif(Median, digits=3), "(", 
                                              u.signif(PCT2P5, digits=3), "-", 
                                              u.signif(PCT97P5, digits=3),")"),
                           
                           GEOmean_SD=paste0(u.signif(GEOmean, digits=3), "(", 
                                             u.signif(GEOmean/GEOSD, digits=3), "-", 
                                             u.signif(GEOmean*GEOSD, digits=3),")") ) 
  
    # or 
    tabl =  tabl %>% 
      mutate(Median=u.signif(Median, digits=3), 
             PCT2P5=u.signif(PCT2P5, digits=3),
             PCT97P5=u.signif(PCT97P5, digits=3), 
             SE=u.signif(SE, digits=3),
             SD=u.signif(SD, digits=3), 
             GEOmean=u.signif(GEOmean, digits=3)
      )
    
    tabl = tabl %>% select(-Median, -PCT2P5, -PCT97P5, -GEOmean, -GEOSD) %>% as.data.frame()
    
    # extend parameters
    tabl$PARAMS = as.character(tabl$PARAMS)
    tabl$PARAMS[duplicated(tabl$PARAMS)] = ""  
    tabl$PARAMS =   str_replace_all(tabl$PARAMS, 
                                         c("CL" = "Clearance\n(L/day)", 
                                           "VSS"= "Volume of distribution (L)",
                                           "Cmin"= "Ctrough\n(mg/L)", 
                                           "Cmax"= "Cmax\n(mg/L)", 
                                           "Time2Cmax"= "Time to Cmax\n(day)", 
                                           "AUCinf"= "AUCinf\n(mg*day/L)"
                                           ))
    # extend colnames of tabl    
    colnames(tabl) = colnames(tabl) %>% str_replace_all(
                      c("ARMA" = "Dose",  
                        "Median_CI95" = "Median(CI95)",
                        "GEOmean_SD" = "GEOmean(SD)",
                        "Mean_CV"= "Mean(CV)",
                        "PARAMS"= "Metrics"
                      ))
      
    return(tabl)
    
  }
  
   
  tabl = tabl4.NCA.PARAMS(tabl)
  attr(tabl, 'title') <- paste0("Descriptive Statistics of Post-hoc Estimates of Pharmacokinetic Parameters of ", DRUG.NAME)  
  TABLE_ALL[["postHoc_exposure_metric"]] = tabl
  
  
  
  
  
  #############################################################################
  #############################################################################
  # use ideal dose regimens, to assess covariate Effects on Exposure to 
  # REGN3918 (400/600/800 QW SC, AND 800 mg Q4W)  
  #############################################################################
  #############################################################################
   
  #----------------------------------------
  # run simulation
  #----------------------------------------
  
  # ideal dose regimen (not actual)
  dosing.regimen = c("400 mg SC QW*24", 
                     "600 mg SC QW*24", 
                     "800 mg SC QW*24", 
                     
                     "600 mg SC Q4W*6", 
                     "800 mg SC Q4W*6" 
                     
  )
   
  # adsl
  adsl = nmdat %>% distinct(USUBJID, .keep_all=TRUE)  %>% select(USUBJID, ID, C5BL, CH50HBL, WGTBL)
  
  # add individual parameters
  adsl = adsl %>% left_join(xpdb %>% 
                              select(ID, one_of(setdiff(colnames(xpdb), colnames(adsl)))) %>%  # no need of WGT_ON_CLQ WGT_ON_VSS
                              distinct(ID, .keep_all=TRUE), 
                            by="ID") %>% 
    select(-ARMA)  # NO existing ARMA
  
  # assign individual parameters as poulation parameters
  adsl = adsl %>% mutate(POP="POSTHOC") %>% select(-starts_with("TV")) %>% 
                  rename(TVCL = CL, 
                         TVV2 = V2, 
                         TVQ = Q, 
                         TVV3 = V3, 
                         TVF1 = F1, 
                         TVKA = KA, 
                         TVVMAX = VMAX, 
                         TVKSS = KSS, 
                         TVEC50 = EC50, 
                         TVGAMMA = GAMMA, 
                         TVEMAX = EMAX)
  
  #
  simData= runSim_by_dosing_regimen(mod  %>% zero_re %>% param( COVARIATE=0, SCALING=0), 
                  adsl, dosing.regimen, 
                  infusion.hour = 1,
                  delta = 1,
                  followup.period = 196,
                  seed=1234)
  
  arma.lst = simData$ARMA %>% unique()
  simData = simData %>% mutate(STUDYID="1659", DVOR=CENTRAL/V2,
                               ARMA = ordered(ARMA, levels =arma.lst))  %>% 
            filter(!is.na(DVOR))   
  
  #----------------------------------------
  # caculate exposure metrics
  #----------------------------------------
  end=24*7 #
  
  tdata1 = simData %>% filter(ARMA %in% c("400 mg SC QW*24",
                                          "600 mg SC QW*24",
                                          "800 mg SC QW*24") & 
                             TIME >=(end-7) & TIME<=(end))         # or EXSEQ to select the last dose interval
  
  tdata2 = simData %>% filter(ARMA %in% c("600 mg SC Q4W*6",
                                          "800 mg SC Q4W*6") & 
                             TIME >=(end-28) & TIME<=(end))
    
  tdata = rbind(tdata1, tdata2)  
    
  OUT <- tdata %>% filter(EVID==0) %>% 
    group_by_(.dots =c("STUDYID", "ARMA",   "USUBJID" )) %>%   
    dplyr::summarise(
      #CL  = CL,
      #VSS = V2 + V3,
      Cmin = DVOR[n()],  # the last conc at this time interval
      #Cmax = max(DVOR),
      #Time2Cmax = TIME[DVOR==max(DVOR)],    # paste(TIME[DVOR==max(DVOR,na.rm=TRUE)], collapse="_"),   #  # 
      AUCtau = auc_partial(TIME, DVOR),
      
      PCHG.CH50 = min(round((CH50-CH50HBL)/CH50HBL*100, digits=3))
      
      # CL.ss = min(CL, na.rm=TRUE),
      # Vss = unique(Vss), 
      # alpha.half.life.ss = max(alpha.half.life.ss), 
      # beta.half.life.ss = max(beta.half.life.ss)
    )   %>% ungroup()  
   
  
  OUT = OUT %>% gather(PARAMS, DVOR, -STUDYID, -ARMA,  -USUBJID)
  
  OUT = OUT %>% left_join(nmdat%>%distinct(USUBJID, .keep_all=TRUE) %>% select(USUBJID, C5BL, CH50HBL, WGTBL, SEX, RACE), 
                          by = "USUBJID")
  
  tdata = OUT %>% gather(COV_NAME, COV_VALUE, C5BL, CH50HBL, WGTBL, SEX, RACE)
  
  #----------------------------------------
  # create categorical varaibles(quantile)
  #----------------------------------------
  # for numeric covariate
  t1 = NULL
  for (iCOV_NAME in c("C5BL", "CH50HBL", "WGTBL")) {
    t1 = rbind(t1, tdata %>% filter(COV_NAME %in% iCOV_NAME) %>% 
               mutate(COV_VALUE=as_numeric(COV_VALUE)) %>% group_by(COV_NAME) %>% assign_Q(value="COV_VALUE", quartile=c(0, 0.25, 0.5, 0.75, 1)))
  }
   
  # for categorical covariates
  t2 = tdata %>% filter(COV_NAME %in% c("SEX", "RACE")) %>% 
    mutate(Q_LABEL = COV_VALUE,  Quantile = COV_VALUE) 
  
  tdata = rbind(t1, t2) 
  
  # calcualte the statistics
  tabl =  tdata %>% calc_stats(id="USUBJID", group_by=c("STUDYID", "ARMA", "PARAMS", "COV_NAME", "Q_LABEL", "Quantile"), value="DVOR") %>% 
   ungroup() %>%  select( STUDYID, PARAMS, ARMA, COV_NAME, Q_LABEL, Quantile, N, Mean_SD)  #%>% 
         #mutate(SE=as.character(u_signif(SE, digits = 3)), 
          #      Median=as.character(u_signif(Median, digits=3)))
  
  #----------------------------------------
  # cosmetic touch-up for final output
  #----------------------------------------
  tabl = tabl %>% arrange(STUDYID, ARMA, PARAMS, COV_NAME, Quantile) %>% 
    select(-STUDYID, -Quantile)
   
  tabl = tabl %>% filter(ARMA %in% c(
    "400 mg SC QW*24",
    "800 mg SC QW*24"
  )) %>% as.data.frame() # %>% select(-ARMA)
  
  tabl = tabl %>% mutate(Mean_SD = gsub("(±  NA)", "", Mean_SD, fix=TRUE))
  tabl = tabl %>% spread(key=ARMA, value=Mean_SD)
  
  tabl =  tabl %>% mutate(PARAMS = str_replace_all(PARAMS, 
                                  c("AUCtau" = "AUCtau\n(day*mg/L)", 
                                    "Cmin"= "Ctrough\n(mg/L)",
                                    "PCHG.CH50"= "Percent Change from\nBaseline in CH50 (%)" 
                                  )),
                          
                          COV_NAME = str_replace_all(COV_NAME, 
                                                     c("C5BL" = "Baseline C5\n(mg/L)", 
                                                       "CH50HBL"= "Baseline CH50\n(U/mL)",
                                                       "RACE"= "Race", 
                                                       "SEX"= "Sex",
                                                       "WGTBL"= "Baseline Weight\n(kg)"
                                                     )),
                          
                          Q_LABEL = str_replace_all(Q_LABEL, 
                                                     c("BLACK_OR_AFRICAN_AMERICAN" = "Black", 
                                                       "ASIAN"= "Asian",
                                                       "WHITE"= "White", 
                                                       "OTHER"= "Other"
                                                     ))#, 
                          
                           
                          
                          #SE = ifelse(is.na(SE), "---", SE)
                          
  )
  
    
  # Remove duplicates
  tabl = tabl %>% group_by(PARAMS) %>%  mutate(COV_NAME=ifelse(duplicated(COV_NAME), "", COV_NAME)) 
  tabl$PARAMS[duplicated(tabl$PARAMS)] = ""
    
  # extend colnames of tabl    
  colnames(tabl) = colnames(tabl) %>% str_replace_all(
    c("PARAMS" = "Metrics",  
      "COV_NAME" = "Covariate",
      "Q_LABEL" = "Category",
      "Mean_SD"= "Mean(SD)" 
    ))
   
  
  tabl %>% as.data.frame()
  
  
  attr(tabl, 'title') <- paste0("Summary Statistics of Post-hoc Steady-State Ctrough and AUCtau, by Relevant Covariates at 800 mg SC QW*24" )
  TABLE_ALL[["postHoc_800QW_by_cov"]] = tabl
  
  
  
  
  



