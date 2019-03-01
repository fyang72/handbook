
 

  ############################################################################## 
  # calculate half life
  ############################################################################## 
  calc.half.life <- function(THETA) {
  # alpha and beta
  THETA$KE  = THETA$CL/THETA$V2    # 1/day, 	Elimination rate constant     0.365
  THETA$K32 = THETA$Q/THETA$V3;  
  THETA$K23 = THETA$Q/THETA$V2; 
    
  parms = THETA
  alpha.plus.beta  <- parms$KE + parms$K23 + parms$K32
  alpha.multi.beta <- parms$KE * parms$K32
  alpha <- (alpha.plus.beta + sqrt(alpha.plus.beta^2-4*alpha.multi.beta))/2
  beta  <- (alpha.plus.beta - sqrt(alpha.plus.beta^2-4*alpha.multi.beta))/2
  alpha.half.life <- 0.693/alpha
  beta.half.life <- 0.693/beta
  #print(paste("beta.half.life=", beta.half.life, sep=""))
  
   
  
  return(data.frame(alpha.half.life, beta.half.life))
  }
    
    
    
#auc_inf {PKPDmisc} R Documentation 
#
#Calculate AUCt-inf
#
#Description
#
#Calculate AUCt-inf 
#
#Usage
#auc_inf(idv, dv, last_points = c(3, 4, 5), na.rm = TRUE)
#
#
#Arguments
#
#idv 
#column name for independent variable such as time
# 
#dv 
#column name for dependent variable such as concentration
# 
#last_points 
#vector of amount of points in terminal phase that will be evaluated for extrapolation
# 
#na.rm 
#remove any NAs from the idv/dv column before calculating AUC
# 
#
  
  # 
  # auc_partial <- function (idv, dv, range = c(0, Inf)) 
  # {
  #   if (!is.numeric(idv) || !is.numeric(dv) || !is.numeric(range)) {
  #     stop("idv, dv, and range inputs must all be numeric")
  #   }
  #   if (length(idv) != length(dv)) {
  #     stop("idv and dv columns must be equal lengths, maybe you filtered NA's only in one?")
  #   }
  #   if (length(range) != 2 || range[1] > range[2]) {
  #     stop("range must be a numeric vector containing a low then high(er) value")
  #   }
  #   aucp <- auc_partial_cpp(idv, dv, range)
  #   tlast <- ifelse(range[2] == Inf, "tlast", range[2])
  #   return(setNames(aucp, paste0("pAUC", range[1], "_", tlast)))
  # }
  # 
  # 
  
  # auc_partial_cpp <- function (time, dv, range) 
  # {
  #   .Call("PKPDmisc_auc_partial_cpp", PACKAGE = "PKPDmisc", time, 
  #         dv, range)
  # }
  

  
  ################################################################################
  ################################################################################
  # Level 1: Calculate the AUC_tau (very useful)
  ################################################################################
  ################################################################################
    
  auc_partial_fy <- function(idv, dv, range = c(0, Inf)) {
    if (!is.numeric(idv) || !is.numeric(dv) || !is.numeric(range)) {
      stop("idv, dv, and range inputs must all be numeric")
    }
    if (length(idv) != length(dv)) {
      stop("idv and dv columns must be equal lengths, maybe you filtered NA's only in one?")
    }
    if (length(range) != 2 || range[1] > range[2]) {
      stop("range must be a numeric vector containing a low then high(er) value")
    }
    
    #tdata$USUBJID must be unique!!!!!
    #
    #tdata = tdata[, c("STUDYID", "USUBJID", "TIME", "DVOR")]
    
    # normalize TIME, to setup it starting from zero, 0!
    # #---------------------------------------------------------
    # t1 = aggregate(as.numeric(as.character(tdata$TIME)), by=list(tdata$USUBJID), fun.min)
    # colnames(t1) = c("USUBJID","MIN.TIME")
    # rownames(t1) = t1$USUBJID      
    # tdata$TIME = tdata$TIME - t1[match(tdata$USUBJID,rownames(t1)), "MIN.TIME"]
    idv = idv - min(idv, na.rm=TRUE)
    
    data <- data.frame(idv, dv)
    data <- data %>% filter(idv>=range[1], idv<=range[2])
    
    # remove all "."
    data[data=="."] <- NA
   
    
    # calculate AUC
    data <- data %>% arrange(-idv)  # [order(-data[[idv]]), ]
    nrec <- length(data$idv)
    data$diff <- c(as.numeric(data$idv[-nrec]) - as.numeric(data$idv[-1]),     0)
    data$midT <- c((as.numeric(data$idv[-nrec]) + as.numeric(data$idv[-1]))/2, 0)
    
    data$meanDV <- c((as.numeric(data$dv[-1]) + as.numeric(data$dv[-nrec]))/2,   0)
    data$dAUC <- as.numeric(data$diff) * as.numeric(data$meanDV)
    data$dAUMC <- as.numeric(data$diff) * as.numeric(data$meanDV) * as.numeric(data$midT)
    
    data <- data[order(data$idv), ] 
    
    # remove the starting points
    #data <- data[which(!is.na(data[, dv])), ]
    data[which(data$diff < 0 ), "dAUC"] <- 0
    data[which(data$diff < 0 ), "dAUMC"] <- 0
    data[which(data$diff < 0 ), "diff"] <- 0
    
    data = data %>% summarise(AUCtau=sum(dAUC, na.rm=TRUE))
    data %>% pull(AUCtau)
   # return(data)
    #AUC <- aggregate.data.frame(data$dAUC, by = list(data[[id]]),
    #      FUN = sum)
    #  names(AUC) <- c(id, "AUC")
    
  }
  
  
  
  
#last_points defaults to 3, 4, 5 see auc_partial for other details 
#
#
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
auc_inf <- function (idv, dv, last_points = c(3, 4, 5), na.rm = TRUE, BLQ=0.078) 
{
  
  dv[which(dv==0)] = BLQ/2 
  
    if (!na.rm) {
        idv <- idv
        dv <- dv
    }else {
        if (all(is.na(dv)) | all(is.na(idv))) {
            return(setNames(NA, paste0("AUC0_inf")))
        }
        if (any(is.na(dv))) {
            warning("removing at least 1 NA value")
        }
        idv <- idv[!is.na(dv)]
        dv <- dv[!is.na(dv)]
    }
    if (isTRUE(all(dv == 0))) {
        return(setNames(0, paste0("AUC0_inf")))
    }
    time.points <- length(idv)
    auci <- vector("numeric", time.points - 1)
    for (i in 1:(time.points - 1)) {
        auci[i] <- (dv[i] + dv[i + 1]) * (idv[i + 1] - idv[i])/2
    }
    auc.start <- 0
    last <- last_points
    start <- time.points - last + 1
    auc.end <- vector("numeric", length(last))
    lambda_z <- vector("numeric", length(last))
    adj.r.squared <- vector("numeric", length(last))
    for (j in 1:length(last)) {
        t <- idv[start[j]:time.points]
        con <- dv[start[j]:time.points]
        if (isTRUE(all(con == 0))) {
            lambda_z[j] <- 0
            adj.r.squared[j] <- 0
        }else {
            xt <- lm(log(con) ~ t)
            lambda_z[j] <- as.numeric(xt$coef[2])
            adj.r.squared[j] <- summary(xt)$adj.r.squared
        }
    }
    if (length(which(adj.r.squared == max(adj.r.squared))) > 
        1) {
        best.fit.pointer <- min(which(adj.r.squared == max(adj.r.squared)))
    }else {
        best.fit.pointer <- which(adj.r.squared == max(adj.r.squared))
    }
    lambda_z.final <- lambda_z[best.fit.pointer] * (-1)
    AUC.last <- sum(auci) + auc.start
    if (length(lambda_z.final)==0  ) {          #| lambda_z.final == 0
        return(setNames(AUC.last, paste0("AUC0_inf")))
    }
    
    if (lambda_z.final == 0) {          
        return(setNames(AUC.last, paste0("AUC0_inf")))
    }
        
    AUC.add = ifelse(dv[length(dv)] == BLQ/2, 0, dv[length(dv)]/lambda_z.final) 
    AUC.inf <- AUC.last + AUC.add
    return(setNames(AUC.inf, paste0("AUC0_inf")))
} 
 
 
 
auc_inf0 <- function (idv, dv, last_points = c(3, 4, 5), na.rm = TRUE) 
{
  if (!na.rm) {
    idv <- idv
    dv <- dv
  }
  else {
    if (all(is.na(dv)) | all(is.na(idv))) {
      return(setNames(NA, paste0("AUC0_inf")))
    }
    if (any(is.na(dv))) {
      warning("removing at least 1 NA value")
    }
    idv <- idv[!is.na(dv)]
    dv <- dv[!is.na(dv)]
  }
  if (isTRUE(all(dv == 0))) {
    return(setNames(0, paste0("AUC0_inf")))
  }
  time.points <- length(idv)
  auci <- vector("numeric", time.points - 1)
  for (i in 1:(time.points - 1)) {
    auci[i] <- (dv[i] + dv[i + 1]) * (idv[i + 1] - idv[i])/2
  }
  auc.start <- 0
  last <- last_points
  start <- time.points - last + 1
  auc.end <- vector("numeric", length(last))
  lambda_z <- vector("numeric", length(last))
  adj.r.squared <- vector("numeric", length(last))
  for (j in 1:length(last)) {
    t <- idv[start[j]:time.points]
    con <- dv[start[j]:time.points]
    if (isTRUE(all(con == 0))) {
      lambda_z[j] <- 0
      adj.r.squared[j] <- 0
    }
    else {
      xt <- lm(log(con) ~ t)
      lambda_z[j] <- as.numeric(xt$coef[2])
      adj.r.squared[j] <- summary(xt)$adj.r.squared
    }
  }
  if (length(which(adj.r.squared == max(adj.r.squared))) > 
      1) {
    best.fit.pointer <- min(which(adj.r.squared == max(adj.r.squared)))
  }
  else {
    best.fit.pointer <- which(adj.r.squared == max(adj.r.squared))
  }
  lambda_z.final <- lambda_z[best.fit.pointer] * (-1)
  AUC.last <- sum(auci) + auc.start
  if (lambda_z.final == 0) {
    return(setNames(AUC.last, paste0("AUC0_inf")))
  }
  AUC.inf <- AUC.last + dv[length(dv)]/lambda_z.final
  return(setNames(AUC.inf, paste0("AUC0_inf")))
}
 

 
  ############################################################################## 
  # add cycle or EXSEQ information
  ##############################################################################
#' @param data A data frame.  The data frame must conform to Regeneron standards and contain the variables: `week`, `ndy`, `concn`, `concni`, `pchg`, `actarmcd`, `ada`
#' @return A dataframe with columns `ndy`, `concn`, `effect`.
#' @export
#' @importFrom dplyr left_join group_by summarise
#' @examples
#'   inFile <- system.file("extdata/pk_pd_hysteresis.xls", package = "pkGraph")
#'   library(readxl)
#'   theData <-read_excel(inFile)
#'   timeMatchedPK(data = theData)   
  calc.EXSEQ <- function(t0) {

     #t0[t0$TIME==".", "TIME"] <- NA
     t0$TIME <- as.my.numeric(t0$TIME)
     t0$EXSEQ = 1
   
     t0.ex <- t0[which(t0$EVID==1), ]
     
     dosing.time.lst <- t0.ex$TIME
     for (i in 2:length(dosing.time.lst)) {
        dosing.event <- dosing.time.lst[i]
        t0$EXSEQ[which(t0$TIME >= dosing.event)] = i
     }
    return(t0$EXSEQ)
    
  }
  
  
  ##############################################################################
  # calc.EXSEQ.TAD  
  ##############################################################################     
#' @param data A data frame.  The data frame must conform to Regeneron standards and contain the variables: `week`, `ndy`, `concn`, `concni`, `pchg`, `actarmcd`, `ada`
#' @return A dataframe with columns `ndy`, `concn`, `effect`.
#' @export
#' @importFrom dplyr left_join group_by summarise
#' @examples
#'   inFile <- system.file("extdata/pk_pd_hysteresis.xls", package = "pkGraph")
#'   library(readxl)
#'   theData <-read_excel(inFile)
#'   timeMatchedPK(data = theData)    
  calc.EXSEQ.TAD <- function(t0) {

     #t0[t0$TIME==".", "TIME"] <- NA
     t0 =data.frame(t0)
     
     t0$TIME <- as.my.numeric(t0$TIME)
     #t0 = t0[which(!is.na(t0$TIME)),  ]

     t0$TAD = t0$TIME
     t0$EXSEQ = 0     
    
     idose = 0
     dosing.time = 0
     for (i in 1:nrow(t0)) {
        t0[i,"TAD"] = t0[i,"TAD"] - dosing.time
        if (is.na(t0[i,"TIME"])) {
            t0[i, "EXSEQ"] = idose
        }else if (t0[i,"TIME"]>=dosing.time) {
           t0[i, "EXSEQ"] = idose
        }
        
        ids = min(nrow(t0),i+1)
        if (t0[ids, "EVID"]==1) {
           dosing.time = ifelse(is.na(t0[ids, "TIME"]), dosing.time, t0[ids, "TIME"])
           idose = idose + 1}
     } 
    return(t0[, c("EXSEQ", "TAD")])
    
  }
   
   


#' @param data A data frame.  The data frame must conform to Regeneron standards and contain the variables: `week`, `ndy`, `concn`, `concni`, `pchg`, `actarmcd`, `ada`
#' @return A dataframe with columns `ndy`, `concn`, `effect`.
#' @export
#' @importFrom dplyr left_join group_by summarise
#' @examples
#'   inFile <- system.file("extdata/pk_pd_hysteresis.xls", package = "pkGraph")
#'   library(readxl)
#'   theData <-read_excel(inFile)
#'   timeMatchedPK(data = theData)  
  default.parms <- function()  {
 
     THETA.CL = 0.202
     THETA.VC = 2.7 
     THETA.Q  = 0.418 
     THETA.VP = 1.83 
     THETA.F1 = 0.7 
     THETA.KA = 0.245
     
     THETA.KON = 482 
     THETA.KOFF = 4.41 
     THETA.KSS = 0.032 
     THETA.KINT = 0.0668
     THETA.KSYN = 2.06
     THETA.KDEG = 1.69
  
     THETA.VMAX = 2  
     THETA.KM = 0.245
   
     
    # reading from spotfire input
    parms= c(THETA.CL, THETA.VC,  THETA.Q,  THETA.VP,  THETA.F1,  THETA.KA,
    THETA.KON,  THETA.KOFF,  THETA.KSS,  THETA.KINT, THETA.KSYN, THETA.KDEG,
    THETA.VMAX,  THETA.KM)
    
    names(parms) = c("THETA.CL", "THETA.VC",  "THETA.Q",  "THETA.VP",  "THETA.F1",  "THETA.KA",
    "THETA.KON",  "THETA.KOFF",  "THETA.KSS",  "THETA.KINT", "THETA.KSYN", "THETA.KDEG",
    "THETA.VMAX",   "THETA.KM")  
    
    # parms$THETA
    parms = transform2.parms.lst(parms)
    
    # parms$OMEGA
    parms$OMEGA = matrix(0.0001, ncol=length(parms$THETA), nrow=length(parms$THETA), dimnames=list(names(parms$THETA), names(parms$THETA)))
    
    # parms$SIGMA 
    parms$SIGMA = NULL
    parms$SIGMA$PROP <- 0   
    return(parms)
    }
    
        
        

   
  ############################################################################## 
  # Continous variables for subject demographic
  ############################################################################## 
#' @param data A data frame.  The data frame must conform to Regeneron standards and contain the variables: `week`, `ndy`, `concn`, `concni`, `pchg`, `actarmcd`, `ada`
#' @return A dataframe with columns `ndy`, `concn`, `effect`.
#' @export
#' @importFrom dplyr left_join group_by summarise
#' @examples
#'   inFile <- system.file("extdata/pk_pd_hysteresis.xls", package = "pkGraph")
#'   library(readxl)
#'   theData <-read_excel(inFile)
#'   timeMatchedPK(data = theData)   
  continous.vars.pairsplot <- function(tdata, VAR1=c("CL","VC","KA"),  #  "THETA.KINT", "THETA.KSYN", "THETA.KDEG",
                                          VAR2=c("WEIGHTBL","AGE","HEIGHTBL")) {                                    
    data=data.matrix(tdata[, c(VAR1, VAR2)])  
    par(mfrow=c(2,2)) 
    for (i in 1:length(VAR1)) {
       pairs(data[, c(VAR1[i], VAR2)], upper.panel=panel.smooth, lower.panel=panel.cor,diag.panel=panel.hist,gap=0.18)  }   } 
       
       
  ##############################################################################     
  # boxplot for discontinuous variables.
  # boxplot of CL vs DOSE, TITER, GENDER, RACE, CO-ADMIN DRUG, FORMULATION
  ############################################################################## 
#' @param data A data frame.  The data frame must conform to Regeneron standards and contain the variables: `week`, `ndy`, `concn`, `concni`, `pchg`, `actarmcd`, `ada`
#' @return A dataframe with columns `ndy`, `concn`, `effect`.
#' @export
#' @importFrom dplyr left_join group_by summarise
#' @examples
#'   inFile <- system.file("extdata/pk_pd_hysteresis.xls", package = "pkGraph")
#'   library(readxl)
#'   theData <-read_excel(inFile)
#'   timeMatchedPK(data = theData)   
 discrete.vars.boxplot <- function(tdata, VAR1=c("CL","VC","KA"),   
                                          VAR2=c("SEX","RACE","ETHNIC","ARMA","TITER"), nrow=2, ncol=2) {
    par(mfrow=c(nrow,ncol)) 
    for (i in 1:length(VAR1)) {
      for (j in 1:length(VAR2))  {
         tdata$CL1 = tdata[, VAR1[i]]; 
         tdata$ARMA1 =  tdata[, VAR2[j]];
         boxplot(CL1 ~ ARMA1, data = tdata, ylab=VAR1[i], xlab=VAR2[j], col = "lightgray") }}
  }
     
     
       
       
 
  #--------------------------------------------- 
  #  calc.stats.cat
  #--------------------------------------------- 
#' @param data A data frame.  The data frame must conform to Regeneron standards and contain the variables: `week`, `ndy`, `concn`, `concni`, `pchg`, `actarmcd`, `ada`
#' @return A dataframe with columns `ndy`, `concn`, `effect`.
#' @export
#' @importFrom dplyr left_join group_by summarise
#' @examples
#'   inFile <- system.file("extdata/pk_pd_hysteresis.xls", package = "pkGraph")
#'   library(readxl)
#'   theData <-read_excel(inFile)
#'   timeMatchedPK(data = theData)   
  calc.stats.cat <- function(adsl, id="USUBJID",by="ARMA",colname="SEXN") { 
    adsl$USUBJID = adsl[, id]
    adsl$ARMA = adsl[, by]               
    adsl$SEXN  = as.integer(adsl[, colname])
     
    library('plyr')  
    STATS = ddply(adsl, "ARMA", summarise, 
                  #CAT = colname,
                  N = sum(SEXN),
                  ALL= fun.uniqN(USUBJID), 
                  PCT =paste(round(sum(SEXN)/fun.uniqN(USUBJID), digits=digits)*100, "%", sep=""))
    data.frame("CAT"=colname, STATS)
    }
    

             
           
  #--------------------------------------------- 
  #   water.fall.plot
  #--------------------------------------------- 
   water.fall.plot <- function(adsl, colname="AGE", xlab.txt="Subject Index", ylab.txt="Age(years)")  { 
        tdata = adsl 
        tdata[, colname] = as.my.numeric(tdata[, colname])
        
        tdata = tdata[order(tdata[, colname]),]
        rownames(tdata) = 1:nrow(tdata)
 
         barplot(tdata[, colname], xlab=xlab.txt,ylab=ylab.txt,main=paste("Waterfall plot of ",colname," in the population",sep="")); box()
        #print(t1)
         
        x = tdata[, colname]
        ids.bottom = order(x)[1:(min(10,length(x)/10))]
        ids.top = order(x,decreasing=TRUE)[1:(min(10,length(x)/10))]
        tdata = tdata[c(ids.bottom, ids.top), c("STUDYID","USUBJID","ARMA",colname)]
        tdata = tdata[order(tdata[, colname]), ]
        
        cat(paste("", "\n", "List of top or bottom USUBJID in ",colname, "...\n", sep="")) 
        print(tdata )
    }
      


    
  ##############################################################################
  # VennDiagram
  ############################################################################## 
#' @param data A data frame.  The data frame must conform to Regeneron standards and contain the variables: `week`, `ndy`, `concn`, `concni`, `pchg`, `actarmcd`, `ada`
#' @return A dataframe with columns `ndy`, `concn`, `effect`.
#' @export
#' @importFrom dplyr left_join group_by summarise
#' @examples
#'   inFile <- system.file("extdata/pk_pd_hysteresis.xls", package = "pkGraph")
#'   library(readxl)
#'   theData <-read_excel(inFile)
#'   timeMatchedPK(data = theData)    
  plot.overlap.adsl.adpk.adex <- function(adsl,adpk,adex) {  
    require(VennDiagram)
    a.list <- list(ADSL=unique(as.character(adsl$USUBJID)), 
                   ADEX=unique(as.character(adex$USUBJID)), 
                   ADPK=unique(as.character(adpk$USUBJID)))
    
    venn.plot <- venn.diagram(a.list,fill = c("red", "green", "blue"),
     alpha = c(0.5, 0.5, 0.5), cex = 2,cat.fontface = 4,lty =2, 
     euler.d = FALSE, scaled = FALSE, #fontfamily =3, 
     filename = NULL);
    grid.draw(venn.plot);
  } 
  
  
 
                  