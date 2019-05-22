

#' Creates a Concentration vs Time graph
#'
#' Creates a Concentration vs Time graph using Regeneron data.
#'
#' @param data A data frame with which to create the plots.  The data frame must conform to Regeneron standards and contain the variables: `week`, `ndy`, `concn`, `concni`, `pchg`, `actarmcd`, `ada`
#' @param drug Drug name
#' @param log Should the log axis be used?
#' @param adjust Should an adjustment be applied to the errorbars?
#' @return A ggplot graphic.
#' @import gridExtra
#' @export
#' @examples
#'   inFile <- system.file("extdata/pk_pd_ada_all3.sas7bdat", package = "pkGraph")
#'   library(haven)
#'   theData <- read_sas(inFile)
#'   theData <- dataCheck(theData)
#'   concTime(data = theData)
#'   concTime(data = theData, adjust = TRUE) 

#group_by = "NTIM"
#tdata = pkpd %>% filter(TEST=="Functional Dupilumab", !ARMA %in% c("Placebo") )  %>% 
#  group_by_(.dots=group_by)  %>% 
#  do(assign_Q(., value="DVOR", quartile=c(0, 0.25, 0.5, 0.75, 1)))



# add geomean
calc_stats  <- function(adsl, id="USUBJID",group_by="ARMA", value="DVOR") { 
  
  #
  #    data <- if(errval == "SE"){
  #      data %>%
  #        summarize_(mean = lazyeval::interp(~ mean(value), value = as.name(y)),
  #                   lower = lazyeval::interp(~ mean(value) - (sd(value)/sqrt(length(value))), value = as.name(y)),
  #                   upper = lazyeval::interp(~ mean(value) + (sd(value)/sqrt(length(value))), value = as.name(y)))
  #                   
  #    CAT = "ADA"
  #    group_by = c("ARMA", paste0(CAT, "_CAT"))
  #    t1 = t0 %>% group_by_(.dots=group_by ) %>% dplyr::mutate(MEDIAN_AUC=median(AUC.SS, na.rm=TRUE),  #####################
  #                                                              MEDIAN_CMIN=median(CMIN, na.rm=TRUE)) %>% 
  #
  #                          dplyr::summarise(MEAN_AUC_SS=mean(AUC.SS, na.rm=TRUE) - unique(MEDIAN_AUC), 
  #                                           MEAN_CMIN=mean(CMIN, na.rm=TRUE) - unique(MEDIAN_CMIN)
  #                                  ) %>%
  #         mutate(CAT=CAT) %>% rename_(.dots=setNames(list(paste0(CAT, "_CAT")), "Q"))     #    ####################
  #         
  
  
  
  
  adsl = as.data.frame(adsl)
  if (nrow(adsl)==0 | is.null(adsl)) {return(NULL)}
  
  as_numeric <- function(x) {suppressWarnings(as.numeric(as.character(x)))}
  
  library('dplyr')
  adsl$USUBJID = adsl[, id]
  adsl$DVOR = as_numeric(adsl[, value])   # take a long time
  
  stats <-  adsl %>%                     #lazyeval::interp(~ adsl %>%
    group_by_(.dots = group_by) %>%
    dplyr::summarise(
      N = length(unique(USUBJID)),
      N_missing= length(unique(USUBJID[is.na(DVOR)])),   # number of subjects who don't have measurable DVOR.
      Mean = mean(DVOR, na.rm=TRUE), 
      GEOmean= geomean(DVOR, na.rm=TRUE),
      Median=median(DVOR, na.rm=TRUE),
      Min = min(DVOR, na.rm=TRUE),
      Max = max(DVOR, na.rm=TRUE),
      SD = sd(DVOR, na.rm=TRUE), 
      GEOSD = geosd(DVOR, na.rm=TRUE), 
      SE = SD/sqrt(length(DVOR)), 
      
      PCT97P5 = fun.pct97p5(DVOR),  
      PCT2P5 = fun.pct2p5(DVOR),     # 95% confident interval
      PCT95 = fun.pct95(DVOR),  
      PCT5 = fun.pct5(DVOR),       # 90% confident interval
      PCT90 = fun.pct90(DVOR),  
      PCT10 = fun.pct10(DVOR)
    ) %>%      # 80% confident interval
    
    mutate(Range= paste0("(", u_signif(Min, digits=3), "-", u_signif(Max, digits=3), ")"),
           CV = abs(SD/Mean)*100,     #### ? abs???  01/05/2018
           meanMinusSD = Mean - SD, 
           meanPlusSD  = Mean + SD, 
           meanMinusSE = Mean - SE, 
           meanPlusSE  = Mean + SE,
           Median_Range = paste0(u_signif(Median,digits=3), "", Range),
           Mean_CV=paste(u_signif(Mean,digits=3)," (", u_signif(CV,digits=3), "%)",sep=""),   # paste( expression(Mean %+-% SD~or~N~"(%)")  , sep="")                              
           Mean_SD=paste(u_signif(Mean,digits=3), " (\u00b1", u_signif(SD,digits=3), ")",sep=""),   # paste( expression(Mean %+-% SD~or~N~"(%)")  , sep="")  
           Mean_SE=paste(u_signif(Mean,digits=3)," (\u00b1", u_signif(SE,digits=3), ")",sep="") ,
           
           GEOmean_SD=paste(u_signif(GEOmean,digits=3), " (\u00b1", u_signif(GEOSD,digits=3), ")",sep="") , 
           GEOmean_CV=paste(u_signif(GEOmean,digits=3)," (", u_signif(GEOSD/GEOmean*100,digits=3), "%)",sep="") 
           
           
    )                                            #"(\u00b1"
  
  
  #arrange_(group_by) 
  #          USUBJID = as.name(id), 
  #          ARMA  = as.name(group_by), 
  #          DVOR = as.name(value)) 
  #   stats = lazyeval::lazy_eval(stats)
  #  
  #   if (signif) {
  #   stats = as.data.frame(stats)
  #   stats = stats %>%  mutate(
  #                  Mean=u_signif(Mean, digits=digits), 
  #                  Median=u_signif(Median, digits=digits), 
  #                  Range=Range,
  #                  SD=u_signif(SD, digits=digits), 
  #                  SE=u_signif(SE, digits=digits), 
  #                  CV=u_signif(CV, digits=digits), 
  #                  meanMinusSD=u_signif(meanMinusSD, digits=digits), 
  #                  meanPlusSD=u_signif(meanPlusSD, digits=digits), 
  #                  meanMinusSE=u_signif(meanMinusSE, digits=digits), 
  #                  meanPlusSE=u_signif(meanPlusSE, digits=digits), 
  #                  Mean_CV=paste(Mean, "(",u_signif(CV,digits=digits),"%)",sep=""),   # paste( expression(Mean %+-% SD~or~N~"(%)")  , sep="")                   
  #                  Mean_SD=paste(Mean, "(\u00b1",u_signif(SD,digits=digits),")",sep=""),   # paste( expression(Mean %+-% SD~or~N~"(%)")  , sep="")  
  #                  Mean_SE=paste(Mean, "(\u00b1",u_signif(SE,digits=digits),")",sep=""),
  #                                              
  #                  PCT97P5=u_signif(PCT97P5, digits=digits), 
  #                  PCT2P5=u_signif(PCT2P5, digits=digits), 
  #                  PCT95=u_signif(PCT95, digits=digits), 
  #                  PCT5=u_signif(PCT5, digits=digits), 
  #                  PCT90=u_signif(PCT90, digits=digits), 
  #                  PCT10=u_signif(PCT10, digits=digits))                   
  #                }  
  # 
  return(stats)
}


