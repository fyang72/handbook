
################################################################################
# Table-1     
#Accounting of Subjects for Total REGN2222 Measurements by Dose Group and Overall 
#Following a Single and Repeat IV or IM Dose(s) of REGN2222 (All Subjects in Study R2222-HV-1326)
################################################################################
summary_of_studies <- function(adpx, group_by=c("STUDYID", "ARMA", "EXROUTE"), value="DVOR", id="USUBJID") {
  # Note:   == "0",  mean  BLQ
  #         == ".",  missing, not available.
  
  summ = adpx %>% group_by_(.dots = group_by) %>% dplyr::summarise(
    N = fun.uniqN(USUBJID), 
    N_mininum_one_measurable_sample = fun.uniqN(USUBJID[DVOR!="." & DVOR!="0"]),
    N_measurable_samples = length(DVOR[DVOR!="." & DVOR!="0"]),
    N_postdose_BLQ_samples = length(DVOR[TIME>=0 & DVOR=="0"]),
    pct_postdose_BLQ_samples = round(N_postdose_BLQ_samples/(N_postdose_BLQ_samples+N_measurable_samples)*100, digits=2))
  
  colnames(summ) <- c(group_by,  
                      "Number of Subjects Included in Analysis",	
                      "Number of Subjects with at Least One Measurable PK Sample",	
                      "Number of Measurable PK Samples",	
                      "Number of Post-dose BLQ Samples",	
                      "Percent (%) of Post-dose BLQ Samplesa")
  return(summ)
  #
  #   Total by Column			577	544	4251	875	17.1
  #   Total
  #   
  #   footnote:
  #   PK = pharmacokinetic; BLQ = below the level of quantification; IV = intravenous; SC = subcutaneous
  #   a Percentages of BLQ samples were computed relative to the total (measurable  + BLQ) sums of PK samples
  
}



#  
#library(data.table) ## v >= 1.9.6
#dcast(setDT(df), month ~ student, value.var = c("A", "B")) 
##    month Amy_A Bob_A Amy_B Bob_B
## 1:     1     9     8     6     5
## 2:     2     7     6     7     6
## 3:     3     6     9     8     7
#Or a possible tidyr solution
#
#df %>% 
#  gather(variable, value, -(month:student)) %>%
#  unite(temp, student, variable) %>%
#  spread(temp, value)
#  
#  

################################################################################
################################################################################
# calc_pkSummary <- function(adpc,  BLQ.level=0.078)
################################################################################
################################################################################

errbarTab8 <- function(data, input, session, EPS = 1e-3) { 
  
  # 
  # req(input$indiv_errbar,  input$color_by, 
  #     input$plotType, input$facet_by,input$xrefline,
  #     
  #     input$xvar, input$yvar,
  #     input$xlab, input$ylab, 
  #     input$xscale, input$yscale,
  #     input$xRange, input$yRange, 
  #     input$fontSize)
  # 
  #uiOutput(ns("errbar_or_CI_selector")),
  #uiOutput(ns("errbar_selector")),
  #uiOutput(ns("CI_selector")),
  
  cat(file=stderr(), "##############Step: calculate errbarTab #################", "\n")
  
  if (is.null(data)) {return(NULL)}
  if (is.null(input$summarise_by)) {return(NULL)}
  if (is.null(input$valueCol)) {return(NULL)}
  
  # do we need to impute DVOR?
  #ids = which(data$DVOR==0 | is.na(data$DVOR))
  #data$DVOR[ids] = as_numeric(data$LLOQ[ids])/2 
  
  # stats table for pkSummary   "STUDYID", "VISIT", "VISITNM", "TIMEPT", "NTIM","ARMA","VALUE" 
  #--------------------------------------
  #data = data %>% mutate_(NTIM=input$xvar, DVOR=input$yvar) %>% 
  #  mutate(NTIM=as_numeric(NTIM), DVOR=as_numeric(DVOR))
  
  pkSummary = data %>% calc_stats(id="USUBJID", group_by=input$summarise_by, value=input$valueCol) 
  if (is.null(pkSummary)) {return(NULL)}
  
  #pkSummary = pkSummary %>% filter(!is.na(NTIM) & !is.na(Mean))
  pkSummary = pkSummary %>% filter(!is.na(Mean))  
  if (is.null(pkSummary)) {return(NULL)}
  
  data = pkSummary  
  if (input$errorbar == "SE") { 
    data$meanMinus = data$meanMinusSE 
    data$meanPlus = data$meanPlusSE
  }
  
  if (input$errorbar == "SD") { 
    data$meanMinus = data$meanMinusSD 
    data$meanPlus = data$meanPlusSD  
  } 
  
  if (input$errorbar == "NONE") { 
    data$meanMinus = data$Mean 
    data$meanPlus = data$Mean
  } 
  
  EPS= 1E-3
  if (input$yscale=="log") {
    data = data %>% mutate(Mean=Mean+EPS, meanPlus=meanPlus+EPS, meanMinus=meanMinus+EPS)
    
    t1 = c(-Inf, Inf, NA)
    #data=filter(data, !is.na(NTIM), !is.na(log(Mean)), !is.na(log(meanMinus)) , !is.na(log(meanPlus)))
    #data=filter(data, NTIM%in%t1,  log(Mean)%in%t1,  log(meanMinus)%in%t1 ,  log(meanPlus)%in%t1)
  }  ##### careful here
  if (is.null(data)|nrow(data)==0) {return(NULL)}
  
  data = data %>% mutate(Mean = as_numeric(Mean),
                         meanMinus = as_numeric(meanMinus), 
                         meanPlus = as_numeric(meanPlus)
  ) 
  
  data
  
}


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
calc_pkSummary <- function(adpc, adjust=TRUE, BLQ.level=0.078) { 
  
  library('dplyr')
  
  # data manipulation
  #-----------------------------------------------  
  adpc = adpc %>% select_("STUDYID", "USUBJID", "VISIT", "VISITNM", "NTIM", "TIMEPT", "ARMA", "DVOR")
  
  adpc$DVOR[which(adpc$DVOR==0 | is.na(adpc$DVOR))] = BLQ.level/2
  
  as.my.numeric <- function(x) {suppressWarnings(as.numeric(as.character(x)))}
  adpc$NTIM = as.my.numeric(adpc$NTIM) 
  adpc$DVOR = as.my.numeric(adpc$DVOR) 
  
  # calculate pkSummary from adpc
  #-----------------------------------------------
  pkSummary <- adpc %>%
    group_by(STUDYID, VISIT, VISITNM, NTIM, TIMEPT, ARMA) %>%
    summarise(N = fun.uniqN(USUBJID), 
              MEAN=mean(DVOR, na.rm=TRUE), 
              MEDIAN=median(DVOR, na.rm=TRUE),
              SD = sd(DVOR, na.rm=TRUE), 
              SE = sd(DVOR,na.rm=TRUE)/sqrt(length(DVOR)), 
              meanMinusSD=MEAN - SD, 
              meanPlusSD =MEAN + SD, 
              meanMinusSE=MEAN - SE, 
              meanPlusSE =MEAN + SE
    )
  
  if (adjust) {        
    pkSummary$meanMinusSD = ifelse(pkSummary$meanMinusSD<0, NA, pkSummary$meanMinusSD)        
    pkSummary$meanMinusSE = ifelse(pkSummary$meanMinusSE<0, NA, pkSummary$meanMinusSE) 
  }
  
  pkSummary = pkSummary[order(pkSummary$STUDYID, pkSummary$VISITNM, pkSummary$NTIM), ]
  
  # ARMA must be ordered factors
  #----------------------------------------------- 
  stopifnot(is.factor(adpc$ARMA))
  #if(!is.factor(adpc$ARMA)) {adpc$ARMA = as.factor(adpc$ARMA)}
  pkSummary$ARMA = ordered(pkSummary$ARMA, levels=levels(adpc$ARMA))
  
  return(pkSummary)
}



# in PKPDmisc

#Summaries
#
#s_cmax
#s_pauc
#s_quantiles
#


################################################################################
################################################################################
# calc_pkSummary <- function(adpc,  BLQ.level=0.078)
################################################################################
################################################################################

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

calc_PKstats <- function(tdata, STUDYID="STUDYID", USUBJID="USUBJID", TIME="TIME", DVOR="DVOR", ARMA="ARMA") {
  
  library('dplyr')
  library('PKPDmisc')
  
  
  as_numeric <- function(x) {suppressWarnings(as.numeric(as.character(x)))}
  fun.pct97p5 <- function(x) quantile(x, c(0.975), type=2, na.rm=TRUE)  # consistent with SAS definition.
  fun.pct2p5 <- function(x) quantile(x, c(0.025), type=2, na.rm=TRUE)  # consistent with SAS definition.
  
  
  tdata = as.data.frame(tdata)
  tdata$STUDYID = as.character(tdata[, STUDYID])
  tdata$USUBJID =  as.character(tdata[, USUBJID])
  tdata$ARMA = as.character(tdata[, ARMA])
  
  tdata$TIME = as_numeric(tdata[, TIME])
  tdata$DVOR = as_numeric(tdata[, DVOR])
  
  
  col.lst = c("STUDYID", "USUBJID", "ARMA", "TIME", "DVOR")
  tdata[, col.lst]
  
  #tdata$DV = tdata$PK  
  tdata1 = tdata %>% group_by(STUDYID, USUBJID, ARMA ) %>% dplyr::summarise(
    CMAX = fun.max(DVOR), 
    CMIN = fun.min(DVOR), 
    AUCtau = auc_partial(TIME, DVOR, range=range(TIME))
    #AUCinf = auc_inf(TIME, DVOR) 
  ) 
  
  #tdata1$USUBJID = tdata1$USUBJID.ORG  
  if (1==2) {
    t1 = calc_stats(tdata1, id="USUBJID",group="ARMA",colname="CMAX")  
    t2 = calc_stats(tdata1, id="USUBJID",group="ARMA",colname="CMIN")  
    t3 = calc_stats(tdata1, id="USUBJID",group="ARMA",colname="AUCtau")  
    t4 = calc_stats(tdata1, id="USUBJID",group="ARMA",colname="AUCinf")  
    tdata2 = bind_rows(t1, t2, t3, t4)
  }
  
  tdata2 = tdata1 %>% group_by(STUDYID, ARMA) %>% dplyr::summarise(
    N=fun.uniqN(USUBJID),
    CMAX.MEAN = fun.mean(CMAX), 
    CMAX.SD = fun.SD(CMAX), 
    CMAX.SE = fun.SE(CMAX), 
    CMAX.CV = fun.SD(CMAX)/fun.mean(CMAX)*100, 
    CMAX.PCT97P5 = fun.pct97p5(CMAX),  
    CMAX.PCT2P5 = fun.pct2p5(CMAX),     # 95% confident interval         
    CMAX.PCT95 = fun.pct97p5(CMAX),  
    CMAX.PCT5 = fun.pct2p5(CMAX),       # 90% confident interval
    CMAX.PCT90 = fun.pct97p5(CMAX),  
    CMAX.PCT10 = fun.pct2p5(CMAX),      # 80% confident interval
    
    CMIN.MEAN = fun.mean(CMIN), 
    CMIN.SD = fun.SD(CMIN), 
    CMIN.SE = fun.SE(CMIN), 
    CMIN.CV = fun.SD(CMIN)/fun.mean(CMIN)*100,
    CMIN.PCT97P5 = fun.pct97p5(CMIN),  
    CMIN.PCT2P5 = fun.pct2p5(CMIN),  
    CMIN.PCT95 = fun.pct97p5(CMIN),  
    CMIN.PCT5 = fun.pct2p5(CMIN),       # 90% confident interval
    CMIN.PCT90 = fun.pct97p5(CMIN),  
    CMIN.PCT10 = fun.pct2p5(CMIN),      # 80% confident interval
    
    AUCtau.MEAN  = fun.mean(AUCtau), 
    AUCtau.SD = fun.SD(AUCtau), 
    AUCtau.SE = fun.SE(AUCtau), 
    AUCtau.CV = fun.SD(AUCtau)/fun.mean(AUCtau)*100, 
    AUCtau.PCT97P5 = fun.pct97p5(AUCtau),  
    AUCtau.PCT2P5 = fun.pct2p5(AUCtau), 
    AUCtau.PCT95 = fun.pct97p5(AUCtau),  
    AUCtau.PCT5 = fun.pct2p5(AUCtau),       # 90% confident interval
    AUCtau.PCT90 = fun.pct97p5(AUCtau),  
    AUCtau.PCT10 = fun.pct2p5(AUCtau)      # 80% confident interval
    
    # AUCinf.MEAN  = fun.mean(AUCinf), 
    #          AUCinf.SD = fun.SD(AUCinf), 
    #          AUCinf.SE = fun.SE(AUCinf), 
    #          AUCinf.CV = fun.SD(AUCinf)/fun.mean(AUCinf)*100, 
    #          AUCinf.PCT97P5 = fun.pct97p5(AUCinf),  
    #          AUCinf.PCT2P5 = fun.pct2p5(AUCinf),      
    #          AUCinf.PCT95 = fun.pct97p5(AUCinf),  
    #          AUCinf.PCT5 = fun.pct2p5(AUCinf),       # 90% confident interval
    #          AUCinf.PCT90 = fun.pct97p5(AUCinf),  
    #          AUCinf.PCT10 = fun.pct2p5(AUCinf)      # 80% confident interval                 
  )
  
  
  return(tdata2)
}

################################################################################
# Table-2     
#Accounting of Subjects for Total REGN2222 Measurements by Dose Group and Overall 
#Following a Single and Repeat IV or IM Dose(s) of REGN2222 (All Subjects in Study R2222-HV-1326)
################################################################################

# summary stats for the continuous variables
#----------------------------------------------------

summary_continuous_var <- function(adpx, id="USUBJID", group_by="ARMA", 
                                   cov_name=c( "AGE", "WEIGHTBL", "HEIGHTBL", "BMIBL"), 
                                   stats_name=c("N", "Mean_SD", "Median", "Range") ) {
  
  t0 = lapply(cov_name, function(value, adpx, id, group_by  ) {
    cbind("cov_name"=value, calc_stats(adpx, id, group_by, value))}, adpx, id, group_by)  %>%     ############
  bind_rows() %>% 
    select(ARMA, cov_name, one_of(stats_name)) %>%   
    gather("stats_name", "value", one_of(stats_name))  %>%  
    spread(ARMA, value )
  
  # add the Overall column
  t1 = adpx  %>% gather("cov_name", "cov_value",  one_of(cov_name) ) %>% 
    calc_stats(id, group="cov_name", value="cov_value") %>%        
    select(cov_name, one_of(stats_name)) %>%   
    gather("stats_name", "Overall", one_of(stats_name)) 
  #%>%
  #rename(ARMA = cov_name)
  
  # merge 
  tcon = t0 %>% left_join(t1, by=c("cov_name", "stats_name"))
  
  tcon$cov_name = ordered(tcon$cov_name, levels=cov_name)
  tcon$stats_name = ordered(tcon$stats_name, levels=stats_name)      
  
  tcon <- tcon %>% arrange(cov_name, stats_name)
  return(tcon)
}




# summary stats for the categorical variables
#----------------------------------------------------  

summary_categorical_var <- function(adpx, id="USUBJID", group_by="ARMA", 
                                    cov_name=c( "SEX", "RACE", "ETHNIC"), 
                                    stats_name=c("N_PCT") ) {
  
  t0 = lapply(cov_name, function(value, adpx, id, group_by ) {
    cbind("cov_name"=value, calc_stats_cat(adpx, id, group_by, value ))}, adpx, id, group_by)  %>% 
    bind_rows() %>% 
    select(ARMA, cov_name, cov_value,  one_of(stats_name)) %>%   
    #gather("stats_name", "value",   CAT, N_PCT)  %>%  
    spread(ARMA, N_PCT )
  
  # add the Overall column   
  adpx$USUBJID = adpx[, id]       
  t1 = adpx %>% gather(cov_name, cov_value, one_of(cov_name)) %>% group_by(cov_name, cov_value) %>% summarise(Overall=as.character(fun.uniqN(USUBJID)))
  
  # merge
  tcat = t0 %>% left_join(t1, by=c("cov_name", "cov_value"))
  
  return(tcat)
}


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
calc_stats_cat <- function(adsl, id="USUBJID", group_by="ARMA", value="SEX") {
  adsl$USUBJID = adsl[, id]
  adsl$ARMA = adsl[, group]               
  adsl$cov_value  = adsl[, value]
  
  t1 = adsl %>% group_by_(.dots = group_by) %>% dplyr::summarise(N_ALL=fun.uniqN(USUBJID))       
  t2 = adsl %>% group_by_(.dots = c(group_by, cov_value)) %>%  dplyr::summarise(N=fun.uniqN(USUBJID)) %>%       
    left_join(t1, by=group_by) %>% 
    mutate(PCT = as.character(u.signif(N/N_ALL*100, digits=3)), 
           N_PCT = as.character(paste(N, "(", PCT, ")", sep="")) 
    )
  t2 = as.data.frame(t2)     
  
  return(t2)
}




if (1==2 ) {
  cov_name = c( "AGE", "WEIGHTBL", "HEIGHTBL", "BMIBL")
  stats_name = c("N", "N_missing", "Mean_SD", "Median", "Range")   
  tcon = summary_continuous_var(adpx, id="USUBJID", group="ARMA", cov_name, stats_name, signif=TRUE, digits=3)
  
  cov_name = c( "SEX", "RACE", "ETHNIC")
  stats_name = c("N_PCT")
  tcat = summary_categorical_var(adpx, id="USUBJID", group="ARMA")
  
  tt = tcon %>% rename(cov_value = stats_name) %>% bind_rows(tcat)
  tt[(is.na(tt))] = "---" 
  
}





#
#  
#calc_conf <- function(data0, conf=0.95) {
##
##How do I calculate a confidence interval for the mean of a log-normal data set?
##http://stats.stackexchange.com/questions/33382/how-do-i-calculate-a-confidence-interval-for-the-mean-of-a-log-normal-data-set
## 
#
#
#library(boot)
#
#set.seed(1)
#
## Simulated data
#data0 = exp(rnorm(100))
#
## Statistic (MLE)
#
#mle = function(dat){
#m = mean(log(dat))
#s = mean((log(dat)-m)^2)
#return(exp(m+s/2))
#}
#
## Bootstrap
#boots.out = boot(data=data0, statistic=function(d, ind){mle(d[ind])}, R = 10000)
#plot(density(boots.out$t))
#
## 4 types of Bootstrap confidence intervals
#boot.ci(boots.out, conf = 0.95, type = "all")
#
#
#For the sample mean
#
#Now, considering the estimator d~=x?d~=x? instead of the MLE. Other type of estimators might be considered as well.
#
#rm(list=ls())
#library(boot)
#
#set.seed(1)
#
## Simulated data
#data0 = exp(rnorm(100))
#
## Statistic (MLE)
#
#samp.mean = function(dat) return(mean(dat))
#
## Bootstrap
#boots.out = boot(data=data0, statistic=function(d, ind){samp.mean(d[ind])}, R = 10000)
#plot(density(boots.out$t))
#
## 4 types of Bootstrap confidence intervals
#boot.ci(boots.out, conf = 0.95, type = "all")
#
#
#
#
#
##Profile likelihood
##
##For the definition of likelihood and profilke likelihood functions, see. Using the invariance property of the likelihood we can reparameterise as follows (?,s)?(d,s)(?,s)?(d,s), where d=exp(?+s2/2)d=exp?(?+s2/2) and then calculate numerically the profile likelihood of dd.
##
##Rp(d)=supsL(d,s)supd,sL(d,s).
##Rp(d)=supsL(d,s)supd,sL(d,s).
##This function takes values in (0,1](0,1]; an interval of level 0.1470.147 has an approximate confidence of 95%95%. We are going to use this property for constructing a confidence interval for dd. The following R codes shows how to obtain this interval.
## 
#
#set.seed(1)
#
## Simulated data
#data0 = exp(rnorm(100))
#
## Log likelihood
#ll = function(mu,sigma) return( sum(log(dlnorm(data0,mu,sigma))))
#
## Profile likelihood
#Rp = function(delta){
#temp = function(sigma) return( sum(log(dlnorm(data0,log(delta)-0.5*sigma^2,sigma)) ))
#max=exp(optimize(temp,c(0.25,1.5),maximum=TRUE)$objective  -ll(mean(log(data0)),sqrt(mean((log(data0)-mean(log(data0)))^2))))
#return(max)
#}
##
##vec = seq(1.2,2.5,0.001)
##rvec = lapply(vec,Rp)
##plot(vec,rvec,type="l")
#
## Profile confidence intervals
#tr = function(delta) return(Rp(delta)-0.147)
#c(uniroot(tr,c(1.2,1.6))$root,uniroot(tr,c(2,2.3))$root)
#
#
#
#
##?? Bayesian
##
##In this section, an alternative algorithm, based on Metropolis-Hastings sampling and the use of the Jeffreys prior, for calculating a credibility interval for dd is presented.
##
##Recall that the Jeffreys prior for (?,s)(?,s) in a lognormal model is
##
##p(?,s)?s-2,
##p(?,s)?s-2,
##and that this prior is invariant under reparameterisations. This prior is improper, but the posterior of the parameters is proper if the sample size n=2n=2. The following R code shows how to obtain a 95% credibility interval using this Bayesian model.
## 
#library(mcmc)
#
#set.seed(1)
#
## Simulated data
#data0 = exp(rnorm(100))
#
## Log posterior
#lp = function(par){
#if(par[2]>0) return( sum(log(dlnorm(data0,par[1],par[2]))) - 2*log(par[2]))
#else return(-Inf)
#}
#
## Metropolis-Hastings
#NMH = 260000
#out = metrop(lp, scale = 0.175, initial = c(0.1,0.8), nbatch = NMH)
#
##Acceptance rate
#out$acc
#
#deltap = exp(  out$batch[,1][seq(10000,NMH,25)] + 0.5*(out$batch[,2][seq(10000,NMH,25)])^2  )
#
#plot(density(deltap))
#
## 95% credibility interval
#c(quantile(deltap,0.025),quantile(deltap,0.975))
#
#}
#
#



