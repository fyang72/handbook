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
  ##############################################################################
  #  check.adsl
  ##############################################################################
  check_adsl <- function(adsl.FINAL) {
    
     # SOME SUBJECT HAVE NOT BEEN ASSIGNED WITH ARMA!
     
     
      #---------------------------------------------  
      # Exclude subjects with Screen Failure
      #---------------------------------------------
      adsl = adsl.FINAL
      adsl = filter(adsl, ARMA !="Screen Failure")
      rownames(adsl) = adsl$USUBJID
      
      
      # Compactly Display the Structure of an Arbitrary R Object
      adsl %>% dplyr::select(STUDYID, USUBJID, ARMA, AGE,WEIGHTBL, SEX, ETHNIC, RACE ) %>% str
  

      #---------------------------------------------  
      # List all entries with abnormal values
      #---------------------------------------------   
      nacols <- function(df) { 
         colnames(df)[unlist(lapply(df, function(x) any(is.na(x)|is.nan(x)|x=="NA"|x=="NAN"|x=="NaN"|x==".")))] }
      anyna <- function(df) {
         df = data.frame(df, check.names = FALSE)      
         t.df = data.frame(t(df), check.names = FALSE)
         return(df[nacols(t.df), nacols(df)])  }
             
                           
      cat(paste('', '\n','List all entries with abnormal values...\n', sep=''))  
      print(anyna(adsl))
      
      
      #---------------------------------------------
      # Checking USUBJID
      #---------------------------------------------
      # Check the format of USUBJID 
      if(!all(nchar(adsl$USUBJID)==nchar(adsl$STUDYID) + 3*4)) {  # "R2810-ONC-1423" +  "-724-002-007"
        print("Warning: Not uniformly formated USUBJID") 
        print(unique(adsl$USUBJID))}
        
      # Check the number of subject 
      cat(paste("","\n","Number of subjects (exlude Screen Failure): ", length(adsl$USUBJID), "\n", sep=""))
     
      # Check number of subjects within each GROUP and ARMA
      cat(paste("", "\n", "Accouting of USUBJID by ARMA...\n", sep=""))
      adsl %>% dplyr::group_by(ARMA) %>% dplyr::summarise(N=fun.uniqN(USUBJID)) %>% dplyr::arrange(ARMA)
      
      
      cat(paste("", "\n", "Accouting of USUBJID by GROUP...\n", sep=""))      
      adsl %>% dplyr::group_by(GROUP) %>% dplyr::summarise(N=fun.uniqN(USUBJID)) %>% dplyr::arrange(GROUP)
      
      
      ##############################################################################
      # Demographics and Baseline Characteristics (PK analysis set) for each cohort
      # age(yrs), male (n, %), white (n, %), hispanic (n, %), BMI (kg/m2)
      ##############################################################################
      
      #---------------------------------------------
      # Checking demographic (continous variables)
      #---------------------------------------------
      library('dplyr')
    
      demog1= NULL
      col.lst = c("AGE","WEIGHTBL","HEIGHTBL","BMIBL","BSABL")
      adsl[, col.lst]  %>% str
      #%>% dplyr::select(STUDYID, USUBJID, ARMA, AGE,WEIGHTBL, SEX, ETHNIC, RACE ) %>% str
   
      ### Iterate through multiple ETA values
      #split_eta_cov <- g2_eta_cov %>% split(.$cov_name)
      demog1 = lapply(col.lst, function(colname, adsl) {
           calc_stats(adsl, id="USUBJID",group="ARMA",colname, digits=3) %>% mutate(CAT = colname) 
      }, adsl)
   
       
      demog1 = demog1 %>% bind_rows() 
      
      ylab.txt = c("Age(years)", "Weight(kg)", "Height(cm)", "BMI(kg/m^2)",  "BSA(m^2)")    
      bp = lapply(col.lst, function(colname, xlab.txt, ylab.txt, adsl) {
           water.fall.plot(adsl, colname, xlab.txt, ylab.txt) 
      },  xlab.txt="Subject Index", ylab.txt, adsl)
      
      bp = bp %>% bind_rows() %>% gather(KEY, VAL, AGE, WEIGHTBL, HEIGHTBL, BMIBL, BSABL)
      
       
       
      #---------------------------------------------
      # Checking demographic (categorical variable)
      #---------------------------------------------
              
      # check the categorical variables
      adsl[which(!duplicated(adsl$SEX)), c("SEX", "SEXN")] ; as.matrix(table(adsl$SEX))
      adsl[which(!duplicated(adsl$ETHNIC)), c("ETHNIC", "ETHNICN")]  ;  as.matrix(table(adsl$ETHNIC))
      adsl[which(!duplicated(adsl$RACE)), c("RACE", "RACEN")]  ;  as.matrix(table(adsl$RACE))
    
      # assigned value for SEX, ETHNIC and RACE 
      adsl$SEXN = ifelse(adsl$SEX=="M", 1, 0)
      adsl$ETHNICN = ifelse(toupper(adsl$ETHNIC)=="NOT HISPANIC OR LATINO", 1, 0)
      adsl$RACEN = ifelse(toupper(adsl$RACE)=="WHITE", 1, 0)
        
      demog2= NULL
      col.lst = c("SEXN", "ETHNICN", "RACEN")
      for (i in 1:length(col.lst)) {demog2 = rbind(demog2,calc.stats.cat(adsl, id="USUBJID",by="ARMA",colname=col.lst[i]))}
      demog2 = demog2 %>% arrange(CAT, ARMA) %>%   mutate(N_PCT=paste(N,"(",PCT,")",sep=""))
               
      
      t1 = dplyr::rename(demog1, RESULT=Mean_SD)
      t2 = dplyr::rename(demog2, RESULT=N_PCT)
      demog = rbind(t1[, c("CAT","ARMA","N","RESULT")], 
                    t2[, c("CAT","ARMA","N","RESULT")])  
      
      
      demog$CAT = as.character(demog$CAT)
      demog$CAT[which(duplicated(demog$CAT))] = "" 
              
      cat(paste("", "\n", "Summary of the population studied in this analysis...\n", sep=""))          
      print(demog)
      
      #u.write.table(demog, file.name="./LATEX/tabl/demog.csv")    
   
   }
           




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
if (1==2)  {
 # examples of build a population
  #nmax = 1200        # maximum number of subjects that could be simulated.
  #nsubject = 1000    # practical number of subjects to be simulalted.
  

  adsl1 = read_csv(system.file("extdata", "adsl.R1500.HV.1214.csv", package = "regnR", mustWork = TRUE))
  adsl2 = read_csv(system.file("extdata", "adsl.R1500.CL.1321.csv", package = "regnR", mustWork = TRUE))
  col.lst = unique(c(colnames(adsl1), colnames(adsl2)))
  adsl1[setdiff(col.lst, colnames(adsl1))] = "."
  adsl2[setdiff(col.lst, colnames(adsl2))] = "."
  adsl <- rbind(adsl1, adsl2)
 
  adsl = filter(adsl, !is.na(WEIGHTBL))
  adsl.FINAL = adsl 
                          # , !is.na(AGE), 
#                      !is.na(SEXN), !is.na(RACEN), !is.na(ETHNICN), 
#                      !is.na(HEIGHTBL), !is.na(BMIBL), !is.na(BSABL))

  col.lst = c("USUBJID", "AGE", "WEIGHTBL", "HEIGHTBL", "BMIBL", "BSABL","SEX", "SEXN", "RACE", "RACEN", "ETHNIC", "ETHNICN")
  ids = sample.int(nrow(adsl), nmax-nrow(adsl), replace=TRUE )
  adsl = rbind(adsl[, col.lst],  adsl[ids, col.lst] )
  adsl$USUBJID = paste( u.add.prefix(1:nmax, prefix="", add.number.zero=4), sep="")
  adsl.adult = adsl[1:nsubject, ]
  adsl.adult = filter(adsl.adult, !is.na(WEIGHTBL))
  
  
  build.adsl = function(adsl, nsubject=100, meanWt=75, sdWt=0.2*75, lowerWt=40, upperWt=160, seed =1234) { 
     
    set.seed(seed);adsl$WEIGHTBL <- rnorm(1:nrow(adsl), meanWt, sdWt); 
    out = filter(adsl, WEIGHTBL>=lowerWt, WEIGHTBL<=upperWt);   
    out = out[1:nsubject, ] ; 
    out$USUBJID = paste(u.add.prefix(1:nrow(out), prefix="", add.number.zero=4), sep="")   ##########make sure all population having the same ID list
    if (any(is.na(out$WEIGHTBL)))  stop
    return(out)
  }
  
  # For any simulation or number generation set.seed(<number>) to maintain reproducibility
  seed = 124634; # set.seed(<number>)

  # boy
  adsl = adsl.adult
  sub.nsubject = nsubject / 2
  adsl.kid12 = build.adsl(adsl, sub.nsubject, meanWt=50.4, sdWt=1.3*sqrt(300), lowerWt=40, upperWt=120, seed =seed) 
  adsl.kid13 = build.adsl(adsl, sub.nsubject, meanWt=53.9, sdWt=1.9*sqrt(300), lowerWt=40, upperWt=120, seed =seed) 
  adsl.kid14 = build.adsl(adsl, sub.nsubject, meanWt=63.9, sdWt=1.6*sqrt(300), lowerWt=40, upperWt=120, seed =seed) 
  adsl.kid15 = build.adsl(adsl, sub.nsubject, meanWt=68.3, sdWt=1.1*sqrt(300), lowerWt=40, upperWt=120, seed =seed) 
  adsl.kid16 = build.adsl(adsl, sub.nsubject, meanWt=74.4, sdWt=1.4*sqrt(300), lowerWt=40, upperWt=120, seed =seed) 
  adsl.kid17 = build.adsl(adsl, sub.nsubject, meanWt=75.6, sdWt=1.4*sqrt(300), lowerWt=40, upperWt=120, seed =seed) 
  adsl.kid18 = build.adsl(adsl, sub.nsubject, meanWt=75.6, sdWt=1.1*sqrt(300), lowerWt=40, upperWt=120, seed =seed) 
  adsl.boy = rbind(adsl.kid12, adsl.kid13, adsl.kid14, adsl.kid15, adsl.kid16, adsl.kid17, adsl.kid18)
  adsl.boy = adsl.boy[sample.int(nrow(adsl.boy), sub.nsubject, replace=TRUE ), ]
  adsl.boy$SEX = "M";    adsl.boy$SEXN = 1
  summary(adsl.boy$WEIGHTBL)
   
  # girl
  adsl.kid12 = build.adsl(adsl, sub.nsubject, meanWt=52, sdWt=1.1*sqrt(300), lowerWt=40, upperWt=100, seed =seed) 
  adsl.kid13 = build.adsl(adsl, sub.nsubject, meanWt=57.7, sdWt=1.4*sqrt(300), lowerWt=40, upperWt=100, seed =seed) 
  adsl.kid14 = build.adsl(adsl, sub.nsubject, meanWt=59.9, sdWt=1.0*sqrt(300), lowerWt=40, upperWt=100, seed =seed) 
  adsl.kid15 = build.adsl(adsl, sub.nsubject, meanWt=61.1, sdWt=1.7*sqrt(300), lowerWt=40, upperWt=100, seed =seed) 
  adsl.kid16 = build.adsl(adsl, sub.nsubject, meanWt=63, sdWt=1.2*sqrt(300), lowerWt=40, upperWt=100, seed =seed) 
  adsl.kid17 = build.adsl(adsl, sub.nsubject, meanWt=61.7, sdWt=1.2*sqrt(300), lowerWt=40, upperWt=100, seed =seed) 
  adsl.kid18 = build.adsl(adsl, sub.nsubject, meanWt=65.2, sdWt=1.5*sqrt(300), lowerWt=40, upperWt=100, seed =seed) 
  adsl.girl = rbind(adsl.kid12, adsl.kid13, adsl.kid14, adsl.kid15, adsl.kid16, adsl.kid17, adsl.kid18)
  adsl.girl = adsl.girl[sample.int(nrow(adsl.girl), sub.nsubject, replace=TRUE ), ]
  adsl.girl$SEX = "F";    adsl.girl$SEXN = 0  
  summary(adsl.girl$WEIGHTBL)
   
  adsl.kid = rbind(adsl.boy, adsl.girl)
  adsl.kid$USUBJID = paste( u.add.prefix(1:nrow(adsl.kid), prefix="", add.number.zero=4), sep="")
  adsl.kid = adsl.kid[1:nsubject, ]
  if (any(is.na(adsl.kid$WEIGHTBL)))  stop
  
  
 adsl.lst = list(adsl.adult, adsl.kid)
 
 return(adsl.lst)
 }
 
 
  
  
  
  