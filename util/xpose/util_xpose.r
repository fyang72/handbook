

 
 
GOF_FIT <- function(xpdb, adpx, runno="001", FIGURE_ALL) {
 
#   how many standard column name needed for this to work??
 
 
  # tdata0:  left_join (xpdb, adpx, by ROWID)
  xpdb = xpdb %>% filter(MDV==0) 
  tt = adpx %>% filter(EVID==0) %>% select(one_of(c("ROWID", setdiff(colnames(adpx), colnames(xpdb)))))                   
  tdata0 = left_join(xpdb, tt, by="ROWID")
  tdata0$ARMA = gsub("-", " ", tdata0$ARMA, fix=TRUE)
 
  #maxV = max(xpdb$DVOR, na.rm=TRUE)
  #tdata <- tdata0 <- xpdb %>% filter(IPRED<=maxV, PRED<=maxV)
   
#  xpdb %>% filter(USUBJID=="R2810-ONC-1423-724-001-004") %>% as.data.frame()                  
#  xpdb %>% filter(PRED > 1E+5) %>% as.data.frame()
#      
#
 
  # Overall GOF
  #----------------------
  fig1 = GOF1(tdata0, MDV="MDV", IPRED="IPRED", PRED="PRED", DV="DV")       # PRED + IPRED vs. DV
  fig1 = recordPlot()
  eval(parse(text=paste("FIGURE_ALL = lstAppend(FIGURE_ALL, ",  "GOF1_", runno, ", fig1)", sep="")))
  
  fig1 = GOF2(tdata0, MDV="MDV", IPRED="IPRED", PRED="PRED", DV="DV")       ## Goodness of PLot
  fig1 = recordPlot()  
  eval(parse(text=paste("FIGURE_ALL = lstAppend(FIGURE_ALL, ",  "GOF2_", runno,", fig1)", sep="")))
  
  
  # individual GOF
  #----------------------  
 
  tdata = tdata0
  study.lst = unique(tdata$STUDYID)
  tdata$SUBJECT = tdata$USUBJID
  for (i in 1:length(study.lst)) {
     tdata$SUBJECT = gsub(paste(study.lst[i], "-",sep=""), "", tdata$SUBJECT, fix=TRUE)
  }
 
  fig1 = indiv_GOF(tdata, ID="SUBJECT", TIME="TIME", IPRED="IPRED", PRED="PRED", DV="DV")   
  eval(parse(text=paste("FIGURE_ALL = lstAppend(FIGURE_ALL, ",  "indiv_GOF_", runno, ", fig1)", sep="")))
  
  # ETA vs ETA
  #---------------------- 
  tdata = tdata0  
  ETA.lst = colnames(tdata)[which(substr(colnames(tdata), 1, 3)=="ETA")]

  fig1 = ETAvsETA(tdata, ID="ID", x= ETA.lst) 
  fig1 = recordPlot() 
  eval(parse(text=paste("FIGURE_ALL = lstAppend(FIGURE_ALL, ",  "ETAvsETA_", runno, ", fig1)", sep="")))
  
  
 
  # ETA vs COV
  #---------------------- 
  tdata = tdata0 
  ETA.lst = colnames(tdata)[which(substr(colnames(tdata), 1, 3)=="ETA")]  
  COV.lst = c("AGE", "WEIGHTBL", "HEIGHTBL", "BMIBL" ) 
  
  fig1 = ETAvsCOV(tdata, cov_name=COV.lst,  eta_name=ETA.lst)  
  eval(parse(text=paste("FIGURE_ALL = lstAppend(FIGURE_ALL, ",  "ETAvsCOV_", runno, ", fig1)", sep="")))
   
   
  ##############################################################################  
  # Goal: VPC by dose group, to test any bias in fitting dosing group
  #  1) Within tdata0, compare mean IPRED/PRED to DV by dosing groups
  #  2) No R simulation, only NONMEM model estimation
  ##############################################################################  
  tdata  = tdata0 %>% filter(ARMA %in% unique(tdata0$ARMA))  # "R2810: 10 mg/kg")
  #tdata  = tdata0 %>% filter(ARMA %in% "R2810: 1 mg/kg")  # "R2810: 10 mg/kg")

  # calculate pkSummary, and plot DV vs TIME
  pkSummary = tdata %>% calc_stats(id="USUBJID", group_by=c("ARMA","NTIM"), value="DVOR")      
  figLog = plot_mean_profile(pkSummary, group="ARMA", time="NTIM", errorbar="SD")
  
  # DV vs TIME overlapped with IPRED vs TIME
  pkSummary_IPRED = tdata %>% calc_stats(id="USUBJID",group_by=c("ARMA","NTIM"), value="IPRED")  
  pkSummary_IPRED = pkSummary_IPRED %>% filter(!is.na(NTIM) & !is.na(Mean)) %>% mutate(NTIM=as_numeric(NTIM)) %>% arrange(ARMA, NTIM)

  # DV vs TIME overlapped with PRED vs TIME
  pkSummary_PRED = tdata %>% calc_stats(id="USUBJID",group_by=c("ARMA","NTIM"), value="PRED")  
  pkSummary_PRED = pkSummary_PRED %>% filter(!is.na(NTIM) & !is.na(Mean)) %>% mutate(NTIM=as_numeric(NTIM)) %>% arrange(ARMA, NTIM)
  
  fig1 = figLog + geom_line(data=pkSummary_PRED, aes(x=NTIM, y=Mean), col="orange", lwd=0.8, lty="dashed") +  
                  geom_line(data=pkSummary_IPRED, aes(x=NTIM, y=Mean), col="orchid", lwd=0.8, lty="dashed")  + 
                  geom_point(data=tdata, aes(x=as_numeric(TIME), y=as_numeric(DVOR))) 
  eval(parse(text=paste("FIGURE_ALL = lstAppend(FIGURE_ALL, ",  "GOF_VPC_", runno, ", fig1)", sep="")))
  
  return(FIGURE_ALL)
  }
  
      
  

  ##############################################################################  
  # Goal: Test R.cpp model, using population profile (OMEGA=0), TVCL, TVV2...
  #  1) R simulation: cpp model (OMEGA=0) + adpx,  compare mean PRED to DV by dosing groups
  ##############################################################################  
 

  VPC_adpx <-   function(mod, adpx, treat_end=56, sim_end=112, delta=1, xscale="month") {
         
      # Check adpx
      #------------- 
      # Need adpx[, c("ROWID", "TAD", "TIME", "DVOR", "ARMA", "USUBJID")]  
      if (!"ID" %in% colnames(adpx) & "USUBJID" %in% colnames(adpx)) {adpx$ID = as.integer(as.factor(adpx$USUBJID))}
      if (!"ROWID" %in% colnames(adpx)) {adpx$ROWID = 1:nrow(adpx) }
      if (!"NTIM" %in% colnames(adpx)) {adpx$NTIM = adpx$TIME }

      col.lst = c("DVOR" ,"ID", "TIME", "DV",  "AMT", "RATE","CMT", "EVID", "MDV")
      stopifnot(all(col.lst %in% colnames(adpx)) )
      stopifnot("ARMA" %in% colnames(adpx))
 
      adpx = convert_colType(adpx, col.lst, types="numeric")  
                     
                     
#      figLog = ggplot(adpx, aes(x=TIME, y=DVOR, group=ID, col=ARMA)) + 
#                    geom_point()  + geom_line()   + scale_y_log10() + base_theme()  + 
#                    facet_wrap(~ARMA)
                    
      figLog = base_xyplot(adpx, xval="TIME", yval="DVOR", color_by="ARMA", xscale=xscale) +  
                    facet_wrap(~ARMA)        
 
      # run cpp model and merge with meta data
      #---------------------------------------- 
      tgrid = sim_timept(start=0, end=sim_end, delta=1, dtime=seq(0,treat_end, by=7))
      out = mod %>% data_set(adpx) %>%  carry.out(ROWID, DVOR) %>% 
                    mrgsim(end=sim_end, delta=delta, add=tgrid) %>% 
                    as.data.frame() %>% capitalize_names()
      
      # merge simulated results with adpx
      # select(POP, ID, USUBJID, SUBJID, STUDYID, GROUP, DOSEID, ARMA), by="ID") %>% 
      out = left_join(out, adpx[, c("ROWID", setdiff(colnames(adpx), colnames(out)))], by="ROWID") %>% 
                    mutate(ARMA = trim(gsub("INFUSION", "", ARMA, fix=TRUE)))
      
      # plot it
      #-----------  
      pkSummary = out %>% calc_stats(id="ID",group_by=c("ARMA","TIME"), value="IPRED")  
      pkSummary = pkSummary %>% filter(!is.na(TIME) & !is.na(Mean)) %>% mutate(TIME=as_numeric(TIME)) %>% arrange(ARMA, TIME)
     
      output = figLog + geom_line(data=pkSummary, aes(x=TIME, y=Mean, group=ARMA), col="orange", lwd=0.8, lty="dashed") +
            ggtitle("Observed Data Compared to R Simulated Result") 
      
      list(fig=output, out=out)
  }
 
 

  VPC_adex <-   function(mod, adpx, adsl, ADEX, treat_end=56, sim_end=112, delta=1, tgrid, xscale="month") {
    
      # run model
      if(is.null(tgrid)) {
           tgrid = sim_timept(start=0, end=sim_end, delta=1, dtime=seq(0,treat_end, by=7))  }
           
              
      # Check adpx
      #------------ 
      # Need adpx[, c("ROWID", "TAD", "TIME", "DVOR", "ARMA", "USUBJID")]  
      if (!"ID" %in% colnames(adpx) & "USUBJID" %in% colnames(adpx)) {adpx$ID = as.integer(as.factor(adpx$USUBJID))}
      if (!"ROWID" %in% colnames(adpx)) {adpx$ROWID = 1:nrow(adpx) }
      if (!"NTIM" %in% colnames(adpx)) {adpx$NTIM = adpx$TIME }
 
      col.lst = c("DVOR" ,"ID", "TIME", "DV",  "AMT", "RATE","CMT", "EVID", "MDV")
      stopifnot(all(col.lst %in% colnames(adpx)) )
      stopifnot("ARMA" %in% colnames(adpx))
         
      adpx = convert_colType(adpx, col.lst, types="numeric")  
                     
      #figLog = ggplot(adpx, aes(x=TIME, y=DVOR, group=ID, col=ARMA)) + 
      #              geom_point()  + geom_line() + scale_y_log10() + base_theme()  + 
      #              facet_wrap(~ARMA)
                    
      figLog = base_xyplot(adpx, xval="TIME", yval="DVOR", color_by="ARMA", xscale=xscale) +  
                    facet_wrap(~ARMA)
                                        
                       
      if (is.null(ADEX) & is.null(adsl)) {
      
          # run cpp model and merge with meta data
          #---------------------------------------- 
          tgrid = sim_timept(start=0, end=sim_end, delta=1, dtime=seq(0,treat_end, by=7))
          out = mod %>% data_set(adpx) %>%  carry.out(ROWID, DVOR) %>% 
                        mrgsim(end=sim_end, delta=delta, add=tgrid) %>% 
                        as.data.frame() %>% capitalize_names()
          
          # merge simulated results with adpx
          # select(POP, ID, USUBJID, SUBJID, STUDYID, GROUP, DOSEID, ARMA), by="ID") %>% 
          out = left_join(out, adpx[, c("ROWID", setdiff(colnames(adpx), colnames(out)))], by="ROWID") %>% 
                        mutate(ARMA = trim(gsub("INFUSION", "", ARMA, fix=TRUE)))
        }
                                                       
      # run cpp model and merge with meta data
      #----------------------------------------- 
      if (!is.null(ADEX) & !is.null(adsl)) {
       
          stopifnot("POP" %in% colnames(adsl))         
          adex = create_adex(adsl, ADEX ) %>% as.ev()
           

          out = mod %>% data_set(adex) %>% mrgsim(end=sim_end, delta=delta, add=tgrid) %>% as.data.frame() %>% capitalize_names()
         
          # merge with meta data  
          out = out %>% left_join(adex %>% as.data.frame() %>% select(POP, ID, USUBJID, SUBJID, STUDYID, GROUP, DOSEID, ARMA), by="ID") %>% 
                        mutate(ARMA = trim(gsub("INFUSION", "", ARMA, fix=TRUE)))
                        
      }
       
      # plot it
      #-----------       
      # calculate pkSummary and then plot it 
      pkSummary = out %>% mutate(NTIM=TIME) %>% calc_stats(id="USUBJID",group_by=c("ARMA","NTIM"), value="IPRED" )  
      pkSummary = pkSummary %>% filter(!is.na(NTIM) & !is.na(Mean)) %>% mutate(NTIM=as_numeric(NTIM)) %>% arrange(ARMA, NTIM)
      output = figLog + geom_line(data=pkSummary, aes(x=NTIM, y=Mean, group=ARMA), col="orange", lwd=0.8, lty="dashed") +
               ggtitle("Observed Data Compared to R Simulated One")   
  
      list(fig=output, out=out)
  }
 
 
 
 
GOF_FIT_CPP <- function(xpdb, adpx, mod, sim_end=16*7, delta=1, tgrid=0, ADEX, FIGURE_ALL=NULL) {
  # adpx:  ROWID, POP, USUBJID, ID, ARMA, NTIM, TIME, TAD, DVOR, TEST...
  # xpdb:  ID, TIME, IPRED, PRED, DV, DVOR, MDV, ETA..., CL, V2... RUVCV, RUVSD
  # mod:   cpp model
  # ADEX:  excel work sheet
  
  print("VPC0...")
  
  # tdata0:  left_join (xpdb, adpx, by ROWID)
  xpdb = xpdb %>% filter(MDV==0)               
  tdata0 = left_join(xpdb, 
                     adpx %>% filter(EVID==0) %>% select(one_of(c("ROWID", setdiff(colnames(adpx), colnames(xpdb))))),     
                     by="ROWID")
  tdata0$ARMA = gsub("-", " ", tdata0$ARMA, fix=TRUE)
   
  # calculate pkSummary, and plot DV vs TIME
  pkSummary = tdata0 %>% calc_stats(id="USUBJID", group_by=c("POP", "ARMA","NTIM"), value="DVOR", signif=FALSE)      
  figLog = plot_mean_profile(pkSummary, type="b",group="ARMA", time="NTIM", errorbar="SD")
  figLog
    
    
  # use adpx as data_set, but with population parameters only (TVCL, TVV2...)
  #-------------------------------------------------------- 
  print("VPC1...")
   
  # run simulation
  out = mod %>% omat(CL_V2_Q=dmat(0,0,0), IIV_EKA=dmat(0), IIV_EV3=dmat(0), IIV_EVMAX=dmat(0), IIV_EKSS=dmat(0)) %>%  
              data_set(adpx) %>%  carry.out(ROWID, TAD, DVOR) %>% mrgsim(end=sim_end, delta=1, add=tgrid) %>% as.data.frame() %>% capitalize_names()
  
  # merge simulated results with adpx
  out = left_join(out, adpx[, c("ROWID", setdiff(colnames(adpx), colnames(out)))], by="ROWID") %>% 
                mutate(ARMA = trim(gsub("INFUSION", "", ARMA, fix=TRUE)))
  # debug
#  m_WT = 75
#  ECL = 0
#  WT_ON_CL=0.534992
#  out$COVCL2 = WT_ON_CL*(log(out$WEIGHTBL)-log(m_WT)); 
#  out$CL2 = exp(log(out$TVCL) + out$COVCL2 + ECL)
#  out$DIFF = out$CL2 - out$CL
# 
#  out %>%  as.data.frame() %>%  filter(USUBJID=="R2810-ONC-1423-840-001-001") %>% select(USUBJID, CL, CL2, DIFF, TVCL, COVCL, COVCL2, WT_ON_CL, M_WT, WEIGHTBL)
 
  # plot it
  pkSummary = out %>% calc_stats(id="USUBJID",group_by=c("ARMA","NTIM"), value="IPRED", signif=FALSE)  
  pkSummary = pkSummary %>% filter(!is.na(NTIM) & !is.na(Mean)) %>% mutate(NTIM=as_numeric(NTIM)) %>% arrange(ARMA, NTIM)
  fig1 = figLog + geom_line(data=pkSummary, aes(x=NTIM, y=Mean), col="orange", lwd=0.8, lty="dashed") +
  ggtitle("Observed Data Compared to R Simulated One (adsl=adpx, adex=adpx, TVCL, OMEGA=0)") 
 
  eval(parse(text=paste("FIGURE_ALL = lstAppend(FIGURE_ALL, ",  "CPP_VPC1_", runno, ", fig1)", sep="")))
  

  ##############################################################################  
  # Goal: Test R.cpp model, using individual parameter from xpdb  
  #  1) R simulation: cpp model (OMEGA=0) + adpx + xpdb,  compare mean PRED to DV by dosing groups
  #  2) adpx is an actual dose regimen(s)
  ##############################################################################  
 
  # use adpx as data_set, but with individual parameters
  #--------------------------------------------------------
  print("VPC2...")
  
  # attached individual parameter to adpx
  parm.lst = c("CL", "V2",  "Q" , "V3",  "F1", "KA",  "VMAX", "KSS")
  adsl = xpdb %>% filter(MDV==0) %>% distinct(ID, .keep_all=TRUE) %>% select(one_of(c("ID", parm.lst, "RUVCV", "RUVSD" ))) 
  colnames(adsl)[colnames(adsl) %in% parm.lst] = paste("TV", colnames(adsl)[colnames(adsl) %in% parm.lst], sep="")

  adex = adpx %>% left_join(adsl, by="ID")
  #adex = adex %>%  as.data.frame() %>%  filter(USUBJID=="R2810-ONC-1423-840-052-003")
  adex$AMT = as_numeric(adex$AMT)
  adex$RATE = as_numeric(adex$RATE)
  adex$MDV = as_numeric(adex$MDV)  
   
  # run simulation
  out = mod %>% param(WT_ON_CL=0, WT_ON_V2=0) %>% omat(CL_V2_Q=dmat(0,0,0), IIV_EKA=dmat(0), IIV_EV3=dmat(0), IIV_EVMAX=dmat(0), IIV_EKSS=dmat(0)) %>%  
              data_set(adex) %>%  carry.out(ROWID, TAD, DVOR) %>% mrgsim(end=sim_end, delta=1, add=tgrid) %>% as.data.frame() %>% capitalize_names()
  
  # merge simulated results with adpx
  out = left_join(out, adex[, c("ROWID", setdiff(colnames(adex), colnames(out)))], by="ROWID") %>% 
                mutate(ARMA = trim(gsub("INFUSION", "", ARMA, fix=TRUE)))
  
  #out %>%  as.data.frame() %>%  filter(USUBJID=="R2810-ONC-1423-840-052-003")
  
  # plot it
  pkSummary = out %>% calc_stats(id="USUBJID",group_by=c("ARMA","NTIM"), value="IPRED", signif=FALSE)  
  pkSummary = pkSummary %>% filter(!is.na(NTIM) & !is.na(Mean)) %>% mutate(NTIM=as_numeric(NTIM)) %>% arrange(ARMA, NTIM)
  fig1 = figLog + geom_line(data=pkSummary, aes(x=NTIM, y=Mean), col="orange", lwd=0.8, lty="dashed")  +
  ggtitle("Observed Data Compared to R Simulated One (adsl=xpdb, adex=adpx, OMEGA=0)") 
 
  eval(parse(text=paste("FIGURE_ALL = lstAppend(FIGURE_ALL, ",  "CPP_VPC2_", runno, ", fig1)", sep="")))
  
 
 

  ##############################################################################  
  # Goal: Test cpp that it can use xpdb (individual parameter), to run ADEX as well as other doses  
  #  1) cpp model (OMEGA=0) + ADEX + xpdb,  compare mean PRED to DV by dosing groups
  #  2) ADEX is an ideal dose regimen
  ##############################################################################  
  print("VPC3...")
  
  # create adsl with individual model parameters
  parm.lst = c("CL", "V2",  "Q" , "V3",  "F1", "KA",  "VMAX", "KSS")
  adsl = xpdb %>% filter(MDV==0) %>% distinct(ID, .keep_all=TRUE) %>% select(one_of(c("ID", parm.lst, "RUVCV", "RUVSD" ))) 
  colnames(adsl)[colnames(adsl) %in% parm.lst] = paste("TV", colnames(adsl)[colnames(adsl) %in% parm.lst], sep="")
  adsl = adsl %>% left_join(adpx %>% distinct(USUBJID, .keep_all=TRUE) %>% select(POP, USUBJID, ID, ARMA, WEIGHTBL), by="ID")
 
  #adsl = adsl %>%  as.data.frame() %>%  filter(USUBJID=="R2810-ONC-1423-840-052-003") 
  # create adex from ADEX
  adex = create_adex(adsl, ADEX )
   
  # run model
  out = mod %>% param(WT_ON_CL=0, WT_ON_V2=0) %>% omat(CL_V2_Q=dmat(0,0,0), IIV_ETA=dmat(0), IIV_EV3=dmat(0), IIV_EVMAX=dmat(0), IIV_EKSS=dmat(0)) %>%  
              ev(adex) %>% mrgsim(end=sim_end, delta=1, add=tgrid) %>% as.data.frame() %>% capitalize_names()
 
  # merge with meta data  
  out = out %>% left_join(adex %>% as.data.frame() %>% select(POP, ID, USUBJID, SUBJID, STUDYID, GROUP, DOSEID, ARMA), by="ID") %>% 
                mutate(ARMA = trim(gsub("INFUSION", "", ARMA, fix=TRUE)))
 
  #out %>%  as.data.frame() %>%  filter(SUBJID=="R2810-ONC-1423-840-052-003")  %>% head()
  
    
  # calculate pkSummary and then plot it 
  pkSummary = out %>% mutate(NTIM=TIME) %>% calc_stats(id="USUBJID",group_by=c("ARMA","NTIM"), value="IPRED", signif=FALSE)  
  pkSummary = pkSummary %>% filter(!is.na(NTIM) & !is.na(Mean)) %>% mutate(NTIM=as_numeric(NTIM)) %>% arrange(ARMA, NTIM)
  fig1 = figLog + geom_line(data=pkSummary, aes(x=NTIM, y=Mean), col="orange", lwd=0.8, lty="dashed") +
  ggtitle("Observed Data Compared to R Simulated One (adsl=xpdb, adex=ADEX, OMEGA=0)")   
 
  eval(parse(text=paste("FIGURE_ALL = lstAppend(FIGURE_ALL, ",  "CPP_VPC3_", runno, ", fig1)", sep="")))
  #
#
#  if (1==2) {
#  
#
#
#   out3$USUBJID = out3$SUBJID
#   out = left_join(out3[, c("USUBJID", "ARMA", "TIME", "IPRED")], 
#             out2[, c("USUBJID", "ARMA", "TIME", "IPRED")], 
#             by = c("USUBJID", "ARMA", "TIME"))
#              
#   ggplot(data=out, aes(x=IPRED.x, y=IPRED.y, group=USUBJID)) + geom_point(col="blue") +   
#        geom_abline(intercept = 0)
#        
#        
#   out$DIFF = abs(out$IPRED.x - out$IPRED.y) 
#   head(out[order(out$DIFF, decreasing=TRUE), ])
#       
#        
#   tdata3 = filter(out3, USUBJID=="R2810-ONC-1423-840-001-044" & ARMA =="3 mg/kg Q2W*24 IV")
#   tdata2 = filter(out2, USUBJID=="R2810-ONC-1423-840-001-044" & ARMA =="3 mg/kg Q2W*24 IV")   
#   ggplot(data=tdata3, aes(x=TIME, y=IPRED, group=USUBJID)) + geom_line(col="blue") + 
#      geom_line(data=tdata2 , aes(x=TIME, y=IPRED, group=USUBJID), col="red") + 
#      geom_vline(xintercept=112)
#      
#      
#        
#  }
#
  ##############################################################################  
  # Goal: Test cpp that it can use adpx as adsl, to run ADEX as well as other doses  
  #  1) cpp model + adsl + ADEX    compare mean PRED to DV by dosing groups
  #  2) NO individual parameter  
  #  3) ADEX is an ideal dose regimen  
  ##############################################################################  
  print("VPC4...")
  
  # create adsl
  adsl = adpx %>% distinct(USUBJID, .keep_all=TRUE) %>% select(POP, USUBJID, ID, ARMA, WEIGHTBL) 
 
  # create adex from ADEX
  adex = create_adex(adsl, ADEX )
   
  # run model  
  out = mod %>% #omat(CL_V2_Q=dmat(0,0,0), IIV_ETA=dmat(0), IIV_EV3=dmat(0), IIV_EVMAX=dmat(0), IIV_EKSS=dmat(0)) %>%  
              ev(adex) %>% mrgsim(end=sim_end, delta=1, add=tgrid) %>% as.data.frame() %>% capitalize_names()
  
  # merge with meta data
  out = out %>% left_join(adex %>% as.data.frame() %>% select(POP, ID, USUBJID, SUBJID, STUDYID, GROUP, DOSEID, ARMA), by="ID") %>% 
                mutate(ARMA = trim(gsub("INFUSION", "", ARMA, fix=TRUE)))
 
  # calculate pkSummary
  pkSummary = out %>% mutate(NTIM=TIME) %>% calc_stats(id="USUBJID",group_by=c("ARMA","NTIM"), value="IPRED", signif=FALSE)  
  pkSummary = pkSummary %>% filter(!is.na(NTIM) & !is.na(Mean)) %>% mutate(NTIM=as_numeric(NTIM)) %>% arrange(ARMA, NTIM)
   
  # plot it
  fig1 = plot_mean_profile(pkSummary, type="l", group="ARMA", time="NTIM", errorbar="NONE") +  
          geom_point(data=adpx, aes(x=TIME, y=DVOR, group=USUBJID,col=ARMA)) + 
          geom_ribbon(aes(ymin=PCT2P5, ymax=PCT97P5), alpha=0.2)   +
  ggtitle("Observed Data Compared to R Simulated One (adsl=adpx, adex=ADEX, OMEGA)")   
   
  eval(parse(text=paste("FIGURE_ALL = lstAppend(FIGURE_ALL, ",  "CPP_VPC4_", runno, ", fig1)", sep="")))
         
  return(FIGURE_ALL)
  } 
  
  
  
  
  
  
  
  
  
  
  

mycol = brewer.pal(7,"Dark2") 
alphacol <-function(col) rgb(t(col2rgb(col)),alpha=100,max=255)
 
  
################################################################################
# 
#  plot.hist.density
#
################################################################################

plot.hist.density <- function(x, label="Clearance") {
#------------------------------------------------------------
#   histgram plot
#------------------------------------------------------------
main.title=paste("Histogram with Normal Curve of ", label, sep="")
h<-hist(x, breaks=25, col="gray", xlab=label,
   main=main.title)
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2) 
box()
 
#------------------------------------------------------------
#  density plot
#------------------------------------------------------------  
main.title=paste("Kernel Density of ", label, sep="")
d <- density(x) # returns the density data 
plot(d, main=main.title)    # plots the results
polygon(d, col="gray", border="blue")

}


  
if (1==2 ) {
#The sm.density.compare( ) function in the sm package allows you to superimpose the 
#kernal density plots of two or more groups. The format is sm.density.compare(x, factor) 
#where x is a numeric vector and factor is the grouping variable. 
# Compare MPG distributions for cars with
# 4,6, or 8 cylinders
library(sm)
attach(mtcars)

# create value labels
cyl.f <- factor(cyl, levels= c(4,6,8),
  labels = c("4 cylinder", "6 cylinder", "8 cylinder"))

# plot densities
sm.density.compare(mpg, cyl, xlab="Miles Per Gallon")
title(main="MPG Distribution by Car Cylinders")

# add legend via mouse click
colfill<-c(2:(2+length(levels(cyl.f))))
legend(locator(1), levels(cyl.f), fill=colfill) 

}
  
  
  
################################################################################
# 
#  Use ggplot as the way to plot
#
################################################################################

#' Extracts the time matched concntration vs effect data
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
multiplot <- function(..., plotlist=NULL, cols=2) {
##    http://wiki.stdout.org/rcookbook/Graphs/Multiple%20graphs%20on%20one%20page%20%28ggplot2%29/
    require(grid)

    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)

    numPlots = length(plots)

    # Make the panel
    plotCols = cols                          # Number of columns of plots
    plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols

    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
    vplayout <- function(x, y)
        viewport(layout.pos.row = x, layout.pos.col = y)

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
        curRow = ceiling(i/plotCols)
        curCol = (i-1) %% plotCols + 1
        print(plots[[i]], vp = vplayout(curRow, curCol ))
    }

}


#-------------------------------------------------------------------------------
# basic plot using ggplot (PRED + IPRED vs. DV in log and non-log)
#-------------------------------------------------------------------------------
#' Extracts the time matched concntration vs effect data
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
GOF1  <- function(xpdb, MDV="MDV", IPRED="IPRED", PRED="PRED", DV="DV",ARMA="ARMA", add_legend=FALSE){
  require('ggplot2')
  x = xpdb
   

  # substitute dummy variables
  #--------------------------------------
  x$MDV = x[, MDV]
  x$DV = x[, DV]  
  x$IPRED = x[, IPRED]
  x$PRED = x[, PRED]
  x$ARMA = x[, ARMA]
  
  
  limits = range(x$DV, na.rm=TRUE)
  
  #x = read.table(paste(model,".tab",sep=""),skip=1,header=T)
  if(!"MDV"%in%names(x))  x$MDV=rep(0,nrow(x))
  x = filter(x, MDV==0)
  
  #population prediction PRED
  p1 <- ggplot(data=x, aes(x=PRED,y=DV, col=ARMA)) +
  xlab("Population Predictions") + ylab("Observed") +
  geom_point()+   
  coord_fixed(ratio=1, xlim = limits, ylim = limits) +   
  #geom_smooth(show.legend = FALSE,lwd=1,alpha=0.5)+ 
  geom_abline(show.legend = FALSE,intercept=0,slope=1)+
  theme_bw() +
  theme(legend.position="none") 
  
  if (add_legend) {
  p1 = p1 + 
  theme(legend.position=c(0.75, 0.2), 
        legend.title = element_blank(), 
        legend.background = element_rect(fill="white", size=0.5, linetype="solid", colour ="white"), 
        legend.key =  element_rect(colour = NA)) +  # ggplot2::element_blank() )
  guides(color=guide_legend(override.aes=list(fill=NA)))
  }
    
  p2 <- ggplot(data=x, aes(x=log(PRED),y=log(DV), col=ARMA)) +     #log(x$PRED[x$MDV==0]),log(x$DV[x$MDV==0]),xlab="Population Predictions (log)",ylab="Observed (log)",alpha=0.5)+
  xlab("Population Predictions (log)") + ylab("Observed (log)") +
  geom_point()+ 
  coord_fixed(ratio=1, xlim = log(limits), ylim = log(limits)) +       
  #geom_smooth(lwd=1,alpha=0.5)+ 
  geom_abline(intercept=0,slope=1)+
    theme_bw() +
  theme(legend.position="none")  

  # individual prediction IPRED
  p3 <- ggplot(data=x, aes(x=IPRED,y=DV, col=ARMA)) +    # qplot(x$IPRED[x$MDV==0],x$DV[x$MDV==0],xlab="Individual Predictions",ylab="Observed",alpha=0.5)+
  xlab("Individual Predictions") + ylab("Observed") + 
  geom_point()+   
  coord_fixed(ratio=1, xlim = limits, ylim = limits) +    
  #geom_smooth(lwd=1,alpha=0.5)+ 
  geom_abline(intercept=0,slope=1)+
    theme_bw() +
  theme(legend.position="none")
  
  p4 <- ggplot(data=x, aes(x=log(IPRED),y=log(DV), col=ARMA)) +    # qplot(log(x$IPRED[x$MDV==0]),log(x$DV[x$MDV==0]),xlab="Individual Predictions (log)",ylab="Observed (log)",alpha=0.5)+
  xlab("Individual Predictions (log)") + ylab("Observed (log)") + 
  geom_point()+  
  coord_fixed(ratio=1, xlim = log(limits), ylim = log(limits)) +       
  #geom_smooth(lwd=1,alpha=0.5)+ 
  geom_abline(intercept=0,slope=1)+
    theme_bw() +
  theme(legend.position="none")
 
  return(multiplot(p1,p2,p3,p4,cols=2) )
 
}



#-------------------------------------------------------------------------------
# GOF plot using ggplot
#-------------------------------------------------------------------------------
#' Extracts the time matched concntration vs effect data
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
GOF2 <- function(xpdb, MDV="MDV", IPRED="IPRED", PRED="PRED", DV="DV" ){
  require('ggplot2')
  x = xpdb
   
    
  if(!"MDV"%in%names(x))  x$MDV=rep(0,nrow(x))
  
  limits = range(x$DV, na.rm=TRUE)
  p1 <- qplot(data=x, x=IPRED[MDV==0],y=DV[MDV==0])+
   xlab("Individual Predictions") + ylab("Observed") + 
  geom_point(alpha=0.5) +   
  coord_fixed(ratio=1, xlim = limits, ylim = limits) +     
  geom_smooth(lwd=1,alpha=0.5)+ 
  geom_abline(intercept=0,slope=1)+
    theme_bw() +
  theme(legend.position="none")

  
  p2 <- 
  #ggplot(x[x$MDV==0,],aes(TIME,IWRES,label=ID))+
  ggplot(x,aes(x=TIME,y=CWRES,label=ID))+  
  geom_point(alpha=0.3) + 
  geom_hline(yintercept=c(0),col="red") + geom_hline(yintercept=c(-4,4),lwd=0.5) + geom_hline(yintercept=c(-6,6),lwd=0.5,lty="dashed") + 
  geom_text(data=x[abs(x$CWRES)>1,],aes(size=abs(CWRES)),hjust=0, vjust=0)+
  geom_smooth(lwd=1,alpha=0.5)+
  theme_bw()+
  theme(legend.position="none") +
  xlab("Time") + 
  ylab("Conditional weighted residuals")

  p3 <- 
  ggplot(x[x$MDV==0,],aes(IPRED,CWRES,label=ID))+
  geom_point(alpha=0.3) + 
  geom_hline(yintercept=c(0),col="red") + geom_hline(yintercept=c(-4,4)) + geom_hline(yintercept=c(-6,6),lty="dashed") + 
  geom_text(data=x[abs(x$CWRES)>1,],aes(size=abs(CWRES)),hjust=0, vjust=0)+
  geom_smooth(lwd=1,alpha=0.5)+
  theme_bw()+
  theme(legend.position="none") +
  ylab("Conditional weigthed residuals")+ 
  xlab("Individual Predictions")
  
  p4 <-   
  #ggplot(x[x$MDV==0,],aes(TIME,IWRES,label=ID))+
  ggplot(x,aes(ID,CWRES,label=ID))+  
  geom_point(alpha=0.3) + 
  geom_hline(yintercept=c(0),col="red") + geom_hline(yintercept=c(-4,4)) + geom_hline(yintercept=c(-6,6),lty="dashed") + 
  geom_text(data=x[abs(x$CWRES)>1,],aes(size=abs(CWRES)),hjust=0, vjust=0)+
  geom_smooth(lwd=1,alpha=0.5)+
  theme_bw()+
  theme(legend.position="none") +
  xlab("Subject ID") + 
  ylab("Conditional weighted residuals")
    
  qq <- qqnorm(x$CWRES[x$MDV==0],plot.it = F)
  p5 <- 
  qplot(qq$x,qq$y,ylab="Conditional weigthed residuals",xlab="Theoretical Quantiles",alpha=0.3)+ 
  geom_abline(intercept=0,slope=1)+
  theme_bw()+
  theme(legend.position="none") 
  
    
  # Note: Solid red line represents CWRES = 0. Solid black lines represent CWRES = 4.  The dotted lines represent CWRES = 6.
  multiplot(p2,p3,p4,p5,cols=2)
}

 
#-------------------------------------------------------------------------------
# individual fitting plot using ggplot
#-------------------------------------------------------------------------------
#' Extracts the time matched concntration vs effect data
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
GOF3 <- function(
xpdb, ID="ID", TIME="TIME", IPRED="IPRED", PRED="PRED", DV="DV", 
n=25, ids=NA) # Plot n individual profiles at random
{
#set.seed(123)
  x=xpdb;   
  
  n = min(n, length(unique(x$ID)))
  if(as.logical(sum(is.na(ids)))) ids = c(sample(unique(x$ID),n)) 
    
  ggplot(x[x$ID%in%ids,],aes(TIME,DV))+ 
  geom_point()+ 
  facet_wrap(~ID)+
  geom_line(aes(x=TIME,y=IPRED),color="darkorange",lwd=0.7) +
  geom_line(aes(x=TIME,y=PRED),color="blue",lwd=0.7) +  
  #geom_vline(data=x[!duplicated(x$ID) &x$ID%in%ids,],aes(xintercept=TTG),alpha=0.5,colour="grey",lwd=1)+
    theme(legend.position="none") +
  ylab("Observed & model prediction(mg/L)") +   theme_bw()+
  xlab(expression(Time~(days)))
}


#-------------------------------------------------------------------------------
# ETAvsETA:   ETA correlation
#-------------------------------------------------------------------------------
#' Extracts the time matched concntration vs effect data
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
ETAvsETA <- function(xpdb, ID="ID", x= paste("ETA", 1:3, sep="")) {
  
  ETA=t(combn(x, m=2))
  ETA.lst = paste(ETA[, 1], ETA[, 2], sep="-")


  tdata = xpdb
  tdata$ID = tdata[, ID]  
  tdata = tdata %>% select(ID, starts_with("ETA")) %>% distinct(ID, .keep_all=TRUE)
   
  #for (i in 1:nrow(ETA)) {
     
  tt = lapply(ETA.lst, function(ETA, tdata) {
    tt = unlist(strsplit(ETA, "-"))
    t1 = tt[1]
    t2 = tt[2]
    
    tdata$ETA_1 = tdata[, t1]
    tdata$ETA_2 = tdata[, t2]
    
    ggplot(tdata, aes(x = ETA_1, y = ETA_2)) +
    #title(paste(t1, " vs. ", t2, sep="")) + 
    geom_point() + #+ stat_smooth(se = F) + 
    stat_smooth(method = "loess", color = "blue", se = F, size = 1.3) + 
    #geom_rug(col=rgb(.5,0,0,alpha=.2)) + 
    base_theme() + 
    xlab(t1) + ylab(t2)
    }, tdata)  
 
  multiplot(plotlist=tt, cols=2) 
  }
  
  

 # it is common to add marginal-histograms around a scatter plot
# http://stackoverflow.com/questions/8545035/scatterplot-with-marginal-histograms-in-ggplot2

 # add lowess fit
  
 
#' Extracts the time matched concntration vs effect data
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
ETAvsCOV <- function(tdata, cov_name_lst=c("AGE", "WEIGHTBL", "HEIGHTBL", "BMIBL", "BSABL"), 
                            eta_name_lst=c("ETA1", "ETA2", "ETA3", "ETA4")) {
  
library(PKPDmisc)
#library(knitr)
library(lazyeval)
library(tidyverse)
  
  #tdata = adpx %>% distinct(USUBJID, .keep_all =TRUE)
   
  # gather all covariates from BW to CRCL to cov_name and cov_value
  #col.lst = c("AGE", "WEIGHTBL", "HEIGHTBL", "BMIBL",    "BSABL")
  g_eta_cov <- tdata %>%  gather(cov_name, cov_value, one_of(cov_name_lst))
  g_eta_cov$cov_value = as_numeric(g_eta_cov$cov_value)
  
  ## Double stack, We can actually gather again
  #col.lst = c("ETA1", "ETA2", "ETA3", "ETA4")
  g2_eta_cov <- g_eta_cov %>% gather(eta_name, eta_value, one_of(eta_name_lst) )
  g2_eta_cov$eta_value = as_numeric(g2_eta_cov$eta_value)
  
  g2_eta_cov = g2_eta_cov %>% mutate(eta_name =ordered(eta_name, levels=eta_name_lst))  
  g2_eta_cov <- g2_eta_cov %>% filter(!is.na(eta_value), !is.na(cov_value))
  #kable(head(g2_eta_cov))
  #kable(tail(g2_eta_cov))
   
  
  eta_cov_scatter0 <- function(df, xval = "cov_value", yval, cov_name = "cov_name") {
  lazy_plot <- lazyeval::interp(~ggplot(distinct(df, cov_value, cov_name, ETA1, eta_name), 
    aes(x = cov_value, y = ETA1)) +
    geom_point() + #+ stat_smooth(se = F) + 
    stat_smooth(method = "loess", color = "blue", se = F, size = 1.3) + 
    #geom_rug(col=rgb(.5,0,0,alpha=.2)) + 
    base_theme() +
    ylab("") +   
    facet_wrap(~cov_name, scales="free"),
    cov_value = as.name(xval),
    ETA1 = as.name(yval),
    cov_name = as.name(cov_name))
  return(lazyeval::lazy_eval(lazy_plot))
  }
  
  ### plot all releationships 
  split_eta_cov <- g2_eta_cov %>% split(.$cov_name)  
  bp = lapply(split_eta_cov, function(x) {
     cov_name <- unique(x$cov_name)
     eta_cov_scatter0(x, xval = "cov_value", yval="eta_value", cov_name = "cov_name") +  
      facet_wrap(~eta_name, scales = "free") +
      #ggtitle(cov_name) +
      xlab(cov_name) + 
      theme(legend.position = 'none')   
     })
   
    
return(bp)
}

 


  
  
  
  
  
  
  
  
  
################################################################################
# 
#  Use xyplot as the way to plot
#
################################################################################

require(lattice)
require(latticeExtra)

#-------------------------------------------------------------------------------
# individual fitting plot using xyplot
#-------------------------------------------------------------------------------

indiv.xyplot<-function(
model,
n=25,
ids=NA) # Plot n individual profiles at random
{
#set.seed(123)
  .df=read.table(paste(model,".tab",sep=""),skip=1,header=T)
  if(is.null(.df$TTG)) .df$TTG=0
  if(as.logical(sum(is.na(ids)))) ids = c(sample(unique(.df$ID[.df$PRED==1]),n))#,7001,8408,5001)
  xyplot(DV+IPRED~TIME|format(ID),.df[.df$ID%in%ids,],distribute.type=T,
    lwd=2,
    type=c("p","l"),col=c(alphacol(mycol[3]),mycol[2]),pch=19,
    xlab="Time", scales=list(cex=0.7),
    ylab="Observations & model predictions")
}


#-------------------------------------------------------------------------------
# individual fitting plot using xyplot
#-------------------------------------------------------------------------------

indiv.xyplot<-function(model,log="y",n=25, ids=NA) # Plot n individual profiles at random
{ 

oopt <- lattice.options(ggplot2like.opts())
mycol = brewer.pal(7,"Dark2")
alphacol <-function(col) rgb(t(col2rgb(col)),alpha=100,max=255)

  .df=model;   # read.table(paste(model,".tab",sep=""),skip=1,header=T)
  #if(as.logical(sum(is.na(ids)))) ids = c(sample(unique(.df$ID[.df$PRED==1]),n))#,7001,8408,5001)
  
  #set.seed(123)
  n = min(n, length(unique(.df$ID)))
  if(as.logical(sum(is.na(ids)))) ids = c(sample(unique(.df$ID),n))#,7001,8408,5001)
  
  if(is.null(.df$TTG)) .df$TTG=0
  ttg = .df[!duplicated(.df$ID),]
  ttg$TIME = ttg$TTG
  ttg$mx = max(.df$DV)
  ttg$DV=NA
  ttg$IPRED=NA
  #.df$mx=0
  #.df = rbind(.df,ttg[,names(.df)])
  #.df=.df[order(.df$ID,.df$TIME),]
  #browser()
  
  require('latticeExtra')  
  t1 <- xyplot(DV+IPRED~TIME|format(ID),.df[.df$ID%in%ids,],distribute.type=T,
    lwd=2,
    type=c("p","l"),col=c("steelblue",mycol[2]),pch=19,
    #type=c("p","l"),col=c(alphacol(mycol[3]),mycol[2]),pch=19,   
    xlab="Time", 
    scales=list(y=list(log=10, cex=0.7)),
    yscale.components = yscale.components.log10ticks,
    ylab="Observations & model predictions")
    
  t2 <- xyplot(mx~TIME|format(ID),ttg[ttg$ID%in%ids,],col=alphacol("grey"),type="h",lwd=3)
  t1 + as.layer(t2)
}


#-------------------------------------------------------------------------------
# individual fitting plot using xyplot, by GROUP
#-------------------------------------------------------------------------------

################################################################################
 

 IPRED.DV.TIME.BY.GROUP <- function(model,log="y",n=25, ids=NA) # Plot n individual profiles at random
{ 

oopt <- lattice.options(ggplot2like.opts())
mycol = brewer.pal(7,"Dark2")
alphacol <-function(col) rgb(t(col2rgb(col)),alpha=100,max=255)

  .df=model;   # read.table(paste(model,".tab",sep=""),skip=1,header=T)
  #if(as.logical(sum(is.na(ids)))) ids = c(sample(unique(.df$ID[.df$PRED==1]),n))#,7001,8408,5001)
  
  #set.seed(123)
  n = min(n, length(unique(.df$ID)))
  if(as.logical(sum(is.na(ids)))) ids = c(sample(unique(.df$ID),n))#,7001,8408,5001)
  
  if(is.null(.df$TTG)) .df$TTG=0
  ttg = .df[!duplicated(.df$ID),]
  ttg$TIME = ttg$TTG
  ttg$mx = max(.df$DV)
  ttg$DV=NA
  ttg$IPRED=NA
  #.df$mx=0
  #.df = rbind(.df,ttg[,names(.df)])
  #.df=.df[order(.df$ID,.df$TIME),]
  #browser()
                   # DV +                                  #  [.df$ID%in%ids,]                  #groups=.df$ID,
                   
xlab.txt <- seq(0, 120, by=14)
scale.x <- list(cex=1.2,at=xlab.txt,labels=xlab.txt, tick.number = 200)
                   
  require('latticeExtra')  
  t.PRED <- xyplot(PRED~TIME|format(.df$TRTGROUP),.df, groups=.df$ID, distribute.type=T,
    lwd=2,
    #type=c("p","l"),col=c("steelblue",mycol[2]),pch=19,     # "p"
    type=c("l"),col=c(mycol[2]),pch=19,     # "p"    
    #type=c("p","l"),col=c(alphacol(mycol[3]),mycol[2]),pch=19,   
    xlab="Time", 
    scales=list(y=list(log=10, cex=0.7)),
    yscale.components = yscale.components.log10ticks,
    ylab="Observations & model predictions (population)")
      
  t.IPRED <- xyplot(IPRED~TIME|format(.df$TRTGROUP),.df, groups=.df$ID, distribute.type=T,
    lwd=2,
    #type=c("p","l"),col=c("steelblue",mycol[2]),pch=19,     # "p"
    type=c("l"),col=c(mycol[2]),pch=19,     # "p"    
    #type=c("p","l"),col=c(alphacol(mycol[3]),mycol[2]),pch=19,   
    xlab="Time", 
    scales=list(y=list(log=10, cex=0.7)),
    yscale.components = yscale.components.log10ticks,
    ylab="Observations & model predictions (individual)")
    
  t.DV <- xyplot(DV~TIME|format(.df$TRTGROUP),.df, groups=.df$ID, distribute.type=T,
    lwd=2,
    #type=c("p","l"),col=c("steelblue",mycol[2]),pch=19,     # "p"
    #type=c("l"),col=c(mycol[2]),pch=19,     # "p"    
    type=c("p"),col=c("steelblue"),pch=19,   
    xlab="Time", 
    scales=list(y=list(log=10, cex=0.7)),
    yscale.components = yscale.components.log10ticks) 
    #ylab="Observations & model predictions")
        
  #t2 <- xyplot(mx~TIME|format(ID),ttg[ttg$ID%in%ids,],col=alphacol("grey"),type="h",lwd=3)
  t.DV + as.layer(t.IPRED)
  #t1
}


 IPRED.DV.TIME.BY.GROUP.V2 <- function(simdata,log="y",n=25, ids=NA, PRED.OR.IPRED="IPRED",group.by="ARMA") # Plot n individual profiles at random
{                       
  #simdata = model
  simdata$ARMA = simdata[, group.by] #$TRTGROUP
  range( simdata$TIME)
  range( simdata$DV)
                                     
  xtck = list(logx=FALSE, cex=1, scale=1, lim=range( simdata$TIME), by.7=7)
  ytck = list(logy=TRUE, cex=1, scale=1, lim=range( simdata$DV), by.7=10)                              
  scale.x <- setup.tck.mark(simdata$TIME, xtck)
  scale.y <- setup.tck.mark(simdata$DV, ytck)

  scale.x$labels[which(as.numeric(scale.x$labels)%%7 != 0)] = " "
  
  require('latticeExtra')  
  t.DV =xyplot(DV~TIME|format(ARMA),simdata, groups=USUBJID , distribute.type=T,
  lwd=2, type=c("p"),col=c("steelblue"),pch=19, 
  xlab="Time (day)",  
  ylab="Observations & model predictions (mg/L)",
  scales=list(x=scale.x,  y=scale.y),     
  #yscale.components = yscale.components.log10ticks,
  par.settings=list(axis.components=list(bottom=list(tck=scale.x$tck), left=list(tck=scale.y$tck))),
  xlim=range(scale.x$at), ylim=range(scale.y$at)       
  )

  
  t.PRED=xyplot(PRED~TIME|format(ARMA),simdata, groups=USUBJID , distribute.type=T,
  lwd=2, type=c("l"),col=c(mycol[2]),pch=19, 
  xlab="Time (day)",  
  ylab="Observations & model predictions (mg/L)",
  scales=list(x=scale.x,  y=scale.y),     
  #yscale.components = yscale.components.log10ticks,
  par.settings=list(axis.components=list(bottom=list(tck=scale.x$tck), left=list(tck=scale.y$tck))),
  xlim=range(scale.x$at), ylim=range(scale.y$at)       
  )
  
  t.IPRED=xyplot(IPRED~TIME|format(ARMA),simdata, groups=USUBJID , distribute.type=T,
  lwd=2, type=c("l"),col=c(mycol[2]),pch=19, 
  xlab="Time (day)",  
  ylab="Observations & model predictions (mg/L)",
  scales=list(x=scale.x,  y=scale.y),     
  #yscale.components = yscale.components.log10ticks,
  par.settings=list(axis.components=list(bottom=list(tck=scale.x$tck), left=list(tck=scale.y$tck))),
  xlim=range(scale.x$at), ylim=range(scale.y$at)       
  )
  
  
  return(list(t.IPRED=t.IPRED, t.PRED=t.PRED, t.DV=t.DV))
  #png(file = "./scripts/figs/Indiv.group.panel.PRED.png", width = 720, height =480); 
  #t.DV + as.layer(t.PRED) 
  #dev.off()
  
  #png(file = "./scripts/figs/Indiv.group.panel.IPRED.png", width = 720, height =480);    
  #if (PRED.OR.IPRED=="IPRED") { t.DV + as.layer(t.IPRED)  }
  #if (PRED.OR.IPRED=="PRED")  { t.DV + as.layer(t.PRED) }
    
  #dev.off()
}      
      
      
      
#-------------------------------------------------------------------------------
# GOF plot using xyplot
#-------------------------------------------------------------------------------

GOF.xyplot <- function(
model ## FileName of nonmem tab output
){
  #x = read.table(paste(model,".tab",sep=""),skip=1,header=T)
  x = model;
  if(!"MDV"%in%names(x))  x$MDV=rep(0,nrow(x))

  p1<-xyplot(IPRED~DV,x[x$MDV==0,],xlab="Individual Predictions",ylab="Observed",pch=19,
        panel=function(...){
        panel.xyplot(...,col=alphacol(mycol[3]))
        panel.smoother(...,col=mycol[2],lwd=3)
      })

  
  p2 <- 
  xyplot(CWRES~TIME,x[x$MDV==0,],xlab="Time (day)",ylab="Conditional weighted residuals",pch=19,
        panel=function(...){
        panel.abline(0)
        panel.xyplot(...,col=alphacol(mycol[3]))
        panel.smoother(...,col=mycol[2],lwd=3)
      })

 
  p3 <- 
  xyplot(CWRES~IPRED,x[x$MDV==0,],xlab="Individual Predictions",ylab="Conditional weighted residuals",pch=19,
        panel=function(...){
        panel.abline(0)
        panel.xyplot(...,col=alphacol(mycol[3]))
        panel.smoother(...,col=mycol[2],lwd=3)
      })
  
  
  qq <- qqnorm(x$CWRES[x$MDV==0],plot.it = F)
  p4 <- 
  xyplot(y~x,qq,pch=19,col=alphacol(mycol[3]),ylab="Conditional weigthed residuals",xlab="Theoretical Quantiles",
  panel=function(...) 
  {
    panel.xyplot(...)
    panel.abline(0,1)
    })
  
  print(p1,position=c(0, .5, 0.5, 1), more=TRUE )
  print(p2,position=c(0.5, .5, 1, 1), more=TRUE )
  print(p3,position=c(0, 0, 0.5, 0.5), more=TRUE )
  print(p4,position=c(0.5, 0, 1, 0.5)) 
}



################################################################################
# plot.it using xyplot
################################################################################

plot.it <-function(tdata) {
  #tdata$ARMA = tdata$DOSEDESP 
  tdata$DV = tdata$PK   
  #tdata$USUBJID = tdata$ID    
                             
                            
  xlim =  c(0, 140) # (0,round( max(tdata$TIME)/7)*7)  
  ylim = c(1,  round( max(tdata$DV)/10)*10)                          
  xtck = list(logx=FALSE, cex=1, scale=1, lim=xlim, by.7=7)
  ytck = list(logy=TRUE, cex=1, scale=1, lim=ylim, by.7=10)                              
  scale.x <- setup.tck.mark(tdata$TIME, xtck)
  scale.y <- setup.tck.mark(tdata$DV, ytck)
 
  scale.x$labels[which(as.numeric(scale.x$labels)%%28 != 0)] = " "
  
  myPanel <- function(x,y,...) {
     # panel.abline(h = c( log10(0.8)),  lty = "dotted", lwd =0.3, col = "red")
      #panel.abline(v = c( 2,14), lty = "dotted", lwd =0.3, col = "blue")           
      #panel.abline(h = c( log10(0.078),5,40), v = c(140),  lty = "dotted", lwd =0.3, col = "blue")    
      panel.abline(h = c( 5,30,40) ,  lty = "dotted", lwd =2.3, col = "red")          
      panel.xyplot(x,y,...)        
      }   
      
  color.scheme =  c("black","red","blue","yellow","green","purple","cyan")
  key.txt = as.character(unique(tdata$EXTRT)  )
  n.key = length(key.txt)
  key.space = "bottom"
  key.lcol.lst2 = color.scheme[1:n.key]
  key.llwd.lst2 = rep(2, times= n.key)
  key.lty.lst2  = rep(1, times= n.key)
  key.columns = min(n.key, 3)   # maximum 3 in a row
  key.rows  = ceiling(n.key/3)
  key.cex = 1
  key.xlab.padding = 3
   
  require('latticeExtra')                       
  t.DV =xyplot(DV~TIME ,tdata,   groups=USUBJID , distribute.type=T,
  lwd=3, type=c("l"),col=color.scheme[1:n.key],#pch=19, 
  xlab="Time (day)",  
  ylab="Model Predictions (mg/L)",
  scales=list(x=scale.x,  y=scale.y),     
  #yscale.components = yscale.components.log10ticks,                     
  par.settings=list(axis.components=list(bottom=list(tck=scale.x$tck), left=list(tck=scale.y$tck)),
                    layout.heights = list(xlab.key.padding =key.xlab.padding ) ),
                 
  key= list( text= list(key.txt,cex=key.cex),
               space=key.space, just = 0.5,   # right  bottom     # just: up goes left
               #points=list(pch=key.pch.lst2, col=key.pcol.lst2,cex=key.pcex.lst2),
               lines=list(col=key.lcol.lst2,lwd=key.llwd.lst2,lty=key.lty.lst2),
               columns=key.columns, rows=key.rows, between.columns=2),
               
  xlim=range(scale.x$at), ylim=range(scale.y$at),   
  #xlim=c(0,140), ylim=c(10,600),
       
  panel = myPanel
  )
  
  return(t.DV)
  }
 
 # plot.it(tdata)