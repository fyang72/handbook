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
 adex2adpc <- function(adex, timepoints, CMT=2) {
################################################################################
################################################################################
# adpc: build adpc  
################################################################################
################################################################################
# Note: 
# 1) adpk$CMT   <- 2  as default
#
#
   
    subj.lst = unique(adex$USUBJID)
    adpc <- adex[which(!duplicated(adex$USUBJID)), ]
    rownames(adpc)<- subj.lst
    adpc = adpc[rep(subj.lst, each=length(timepoints)) , ]
    
    adpc$TIME = rep(timepoints, times= length(subj.lst))
    adpc$DVOR = ""
    adpc$DV = "."
    adpc$CMT = CMT
    adpc$STDUNIT = "mg/L"
    adpc$TEST <- 1
    adpc$TESTN = 1
      
  return(adpc)}
  
  
  
  
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
  check_adpc <- function(adpc.FINAL) {
       
          adpc =  adpc.FINAL   
          
    #Ensure that the following columns are numeric and not text: TIME, COBS, WT, AGE, AMT and DOSEs
    
    
   # if DVOR is a character column, therefore want to find out what character values exist
 
# check what character values are present
#unique_non_numerics(adpc$DVOR)
   # BQLFLAG which takes a value of "0" if there is a numerical value in CObs and "1" if there is "BQL" in CObs.
     
# if don't manually specify to handle NA COBS, will also get NA values for BQLFLAG
adpc <- adpc %>% mutate(BLQ_FLAG = ifelse(is.na(DVOR), 0, 
                                                 ifelse(DVOR == "BLQ", 1, 0)),
                        DVOR = as_numeric(DVOR))
 
      #----------------------------------------------------------------------------- 
      # check PKDAY_C(time) against NOM_DAY(NTIM)
      #----------------------------------------------------------------------------- 
      print("Checking PKDAY_C(time) against NOM_DAY(NTIM) ...") 
#Consistency checks should be in place to check that relative times of blood sample reflect the planned relative time, and especially that they are on the expected side of drug administration, i.e. if they are planned to be after drug administration then they actually are after drug administration! If these can be captured early on, then the data manager can ask the site to pay particular attention to these issues.
#


      adpc =  adpc.FINAL
      ids = order(abs(adpc$PKDAY_C - adpc$NOM_DAY), decreasing=TRUE)[1:10]
      adpc[ids, c("USUBJID", "ARMA", "PKDAY_C", "NOM_DAY")]
      
      adpc$NTIM = as.my.numeric(adpc$NTIM)
      adpc$TIME = as.my.numeric(adpc$TIME)
      ggplot(data=adpc, aes(x=NTIM, y=TIME, group=ARMA, col=ARMA, shape=ARMA)) +     # size
      geom_point() +   
     scale_x_continuous(breaks=seq(0,max(adpc$TIME), by=7),minor_breaks=seq(0,max(adpc$TIME),by=1))  +        
      scale_color_manual("Dose Group", 
                 #breaks=c("A","B","C", "D", "E", "F"),  
                 values =fun.color.scheme() ,  
                 labels=unique(adpc$ARMA))  + 
      scale_shape_manual("Dose Group",
                 #breaks=c("A","B","C", "D", "E", "F"), 
                 values =rep(19, time=100), 
                 labels=unique(adpc$ARMA))  +   
      xlab("Nominal Time (day)") +
      ylab("Actual Time (day)") +
      theme_bw() 
       
      #-----------------------------------------------------------------------------
      # Double check DAY 1 PRE, its NOM_DAY AND NOM_HR, then update NOM_DAY for "DAY 1 PRE"
      #-----------------------------------------------------------------------------
      print("Checking DAY-1, DAY 1 PRE ...") 
              
      adpc =  adpc.FINAL
      ids = which(adpc$TIME<0)
      
      tt = cbind(
        adpc[ids, c("USUBJID", "NOM_DAY", "TIMEPT")], 
        signif(adpc$TIME[ids], digits=5), 
        signif(adpc$TIME[ids]*24, digits=5), 
        signif(adpc$TIME[ids]*24*60,digits=5)) 
      colnames(tt) = c("USUBJID", "NOM_DAY", "TIMEPT", "DAY", "HOUR", "MIN")
      
      #tt = tt[which(!duplicated(tt)), ]
      tt = tt[order(tt$TIMEPT),]
      print(tt)
      
      unique(tt$TIMEPT)
      plot(tt$DAY, xlab="Subject Index", ylab="Actual Time (Day)")
      
      #-----------------------------------------------------------------------------
      # Double check who do not have predose or post-dose quantifiable sample."
      #-----------------------------------------------------------------------------
      print("Checking who do not have predose or post-dose quantifiable sample ...") 
        
      subj1 <- unique(adpc$USUBJID[which(adpc$CONCN!=0)])  
      subj2 <- unique(adpc$USUBJID[which(adpc$TIME<=0 & adpc$CONCN==0  )])
      if (length(setdiff(subj1, subj2))>0) {
        subj.lst <- setdiff(subj1, subj2)
        print.subj.info(subj.lst, message="who do not have predose sample.")  
        
        #adpc[which(adpc$USUBJID %in% subj.lst), ]
      } 
          
      if (length(setdiff(subj2, subj1))>0)  {  
        subj.lst <- setdiff(subj2, subj1)
        print.subj.info(subj.lst, message="who do not have post-dose quantifiable PK sample.")   
      }
       
      
      #-----------------------------------------------------------------------------
      # Number of quantifiable measurement per subject
      #-----------------------------------------------------------------------------
      
      adpc = adpc.FINAL
      #adpc = adpc[which(adpc$SOP=="PCL3950"), ]  # PK samples 
      adpc = adpc[which(adpc$DVOR!="0"), ]
      tt = aggregate(adpc$DVOR, by=list(adpc$USUBJID, adpc$ARMA), length)
      colnames(tt) <- c("USUBJID", "ARMA", "N")
      tt = tt[order(tt$N), ]
      
      barplot(tt$N, names.arg=tt$USUBJID)
      box()

 
     
      
}
 