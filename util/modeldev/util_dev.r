



##############################################################################
# Base model development procedure
##############################################################################
# Step 1:  Create NONMEM dataset
#      1-1: Read adsl, adex, adpc
#      1-2: Check them and bind into adpx,  
#      1-3: Filter outliers
#      1-4: Create NONMEM dataset
#
# Step 2:  Update NONMEM control stream
#      2-1:  Read template ctl
#      2-2:  Update it accordingly (structure model variation)
#      2-3:  Run it and compile the results
#
# Step 3: Run it 
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
update_base_model <- function(template_ctl, runno="002_001", DATA_NAME, DATA_INPUT) {   # template.ctl
 
  #-----------------------------------------------------------------------------
  # loading model template  
  #-----------------------------------------------------------------------------  
  
 
  #model.descp = "Linear two-compartmental model (Concentration only)"
  model.DESCP = "$PROBLEM 002, Model with parallel linear and Michelis-Menten elimination (concentration only)"  
  #model.descp =  " Michael-Menton model (Concentration only)"
  
  #-----------------------------------------------------------------------------
  # loading dataset
  #-----------------------------------------------------------------------------
  model.DATA  = paste("$DATA  ../data/", DATA_NAME, " IGNORE = C", sep="")
  model.INPUT = DATA_INPUT
     
    
  #-----------------------------------------------------------------------------
  # THETA, OMEGA, SIGMA, [must be consistent with model control stream]
  #-----------------------------------------------------------------------------
  # Estimation of model parameters  
  
  if (runno=="001_001")  {  
      THETA = c( 
             " (0,0.15)    ;THETA-1 CL", 
             " (0,3.00)    ;THETA-2 V2", 
             " (0,0.45)    ;THETA-3 Q", 
             " (0,1.50)    ;THETA-4 V3", 
             " (0,0.60)    ;THETA-5 F1", 
             " (0,1.00)    ;THETA-6 KA",
             " (-0.9)      ;THETA-7 RUVCV",                                                                                        
             " (-0.9)      ;THETA-8 RUVSD")             


      # OMEGA
      OMEGA = c("0.4              ;OMEGA-2 CL",
                "0.4              ;OMEGA-3 V2",
                "0.05             ;OMEGA-5 Q", 
                "0.05             ;OMEGA-1 KA")
                
      SIGMA = c("1 FIXED ;SIGMA-1")                
  }
  
  
  
  
    
  #for typical MM model without MU referencing
  if (runno=="002_001")  {  
      THETA = c( 
             " (0,0.128)    ;THETA-1 CL", 
             " (0,3.00)    ;THETA-2 V2", 
             " (0,0.45)    ;THETA-3 Q", 
             " (0,1.50)    ;THETA-4 V3", 
             " (0,0.7)     ;THETA-5 F1", 
             " (0, 0.4)    ;THETA-6 KA", 
             " (0, 1)    ;THETA-7 VMAX", 
             " (0, 1)      ;THETA-8 KSS", 
             " (-0.9)      ;THETA-9 RUVCV",                                                                                        
             " (-0.9)      ;THETA-10 RUVSD")             


      # OMEGA
      OMEGA = c(
                "0.04       ;OMEGA-1 CL",
                "0.04       ;OMEGA-2 V2",
                "0.05       ;OMEGA-3 Q",
                "0.05       ;OMEGA-4 KA",                
                "0.05       ;OMEGA-5 KSS")
                
      SIGMA = c("1 FIXED ;SIGMA-1")
                      
  }
  
  #for MM model with MU referencing
  if (runno=="002_002")  {
      THETA = c(                                                                                    
          " (-2.1)      ;THETA-1 CL",                                                                                           
          " (1.20)      ;THETA-2 VC",                                                                                   
          " (-1.6)      ;THETA-3 Q", 
          " (1.5)       ;THETA-4 VP",                                                                                            
          " (0.6)       ;THETA-5 LF1",  
          " (0.2)       ;THETA-6 KA",  
          " (1.78)      ;THETA-7 VMAX",
          " (0.8)       ;THETA-8 KSS",                                                                                        
          " (-0.9)      ;THETA-9 RUVCV",                                                                                        
          " (-0.9)      ;THETA-10 RUVSD")
    
      OMEGA = c( 
                "0.07              ;OMEGA-1 CL",
                "0.04              ;OMEGA-2 VC",
                "0.05              ;OMEGA-3 Q", 
                "0.12              ;OMEGA-4 KSS") 
                
      SIGMA = c("1 FIXED ;SIGMA-1")
  }    
  
  
  library("gdata") 
  THETA = trim(colsplit(matrix(THETA,ncol=1),";", c("RANGE", "PARAM")))
   
  OMEGA = trim(colsplit(matrix(OMEGA,ncol=1),";", c("RANGE", "PARAM")))
   
  SIGMA = trim(colsplit(matrix(SIGMA,ncol=1),";", c("RANGE", "PARAM")))
     
  #update control stream
  #model.descp = " Linear 2-compartmental model (Concentration only)"
  #model.number = "base"
  base_ctl = update_CTL(template_ctl, 
        model.DESCP, 
        model.DATA, 
        model.INPUT, 
        THETA, OMEGA, SIGMA)
           


  
  #----------------------------------------------------------------------------- 
  # Step 3: Run it!
  #-----------------------------------------------------------------------------  
   
  if (1==2 ) {     #debug
  
  setwd("C:/FYANG/R2810/mmModel/")  
  t001 = readLines("runit_template.bat")                  #  parms.TRIG = c(1, -0.79, 19.1, 1)   #  fitemax$coefs
  t001[which(regexpr('c:/nm73g64/wfn7/bin/nmgo.bat', t001)>0)]  =  paste("c:/nm73g64/wfn7/bin/nmgo.bat  c:/FYANG/R2810/mmModel/", nmmodel.number, ".ctol",sep="") 
  writeLines(t001, con="runit.bat", sep = "\n", useBytes = FALSE)                  #  parms.TRIG = c(1, -0.79, 19.1, 1)   #  fitemax$coefs
  system('c:\\WINDOWS\\system32\\cmd.exe /c   runit.bat', wait = TRUE) 
   
  } 
   
   
   
  return(base_ctl)
  
  }
  
  
   
   
               


  ##############################################################################
  # Covriate model development procedure
  ##############################################################################
  # Step 1:  Forward addition
  #      1-1: read base model
  #      1-2: copy the covariate tempplate model and update with base model result 
  #      1-3: update the CTL and re-run the model
  #      1-4: use it as the base model for the forward addition and get the result
  #      1-5: re-run the fad_final CTL
  #
  # Step 2:  Backward elimiation
  #      2-1:  backward elimiation
  #      2-2:  re-run the bel_final CTL
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
  build_cov_model <- function(MODEL_HOME,
        step = c("base","fad", "bel"),
        base_runno,
        cov_ctl_template, cov_name_lst,
        fad_threshold=-6.7, bel_threshold=13.7,
        max_wait_min=2,
        to_do_lst=c("write",  "run",  "read")) {
                        
  ##############################################################################                        
  # transition from base_model to base_of_cov_model
  ##############################################################################                        
  library("xpose4")
    
  if ("base" %in% step) {

    # Step 1: read base model
    #base_runno = "002"
    print("update the covariate templdate with the results from base model...")
    base_lst = read.lst(paste(MODEL_HOME, "ctl/", base_runno, ".NM7\\", base_runno, ".lst", sep="")  )
    base_ctl = readLines(paste(MODEL_HOME, "ctl/",  base_runno, ".ctl", sep=""),warn=FALSE)
    base_THETA = extract_ctl(base_ctl)[["THETA"]]

    # Step 2: copy from template and update it with the base model result

    cov_THETA = extract_ctl(cov_ctl_template)[["THETA"]]
    cov_THETA[1:length(base_lst$thetas), "RANGE"] = paste(" ", u.signif(base_lst$thetas, digits =3), " FIX", sep="")
    #cov_THETA[match(rownames(cov_THETA), rownames(base_THETA)), "RANGE"] =

    # the difference between base_THETA and cov_THETA is the list of potential covariates
    cov_name_lst_all = setdiff(cov_THETA[, "PARAM"], base_THETA[, "PARAM"])
    #print(cov_name_lst_all)

    # Step 3: update the template, write it
    temp.CTL = update_CTL(cov_ctl_template,
        model.DESCP = NULL,
        model.DATA = extract_ctl(base_ctl)[["DATA"]],
        model.INPUT = extract_ctl(base_ctl)[["INPUT"]],
        THETA=cov_THETA, OMEGA= NULL, SIGMA= NULL)

    cov_runno = paste(base_runno, "_cov", sep="")  #"002_cov"
    ctl.file = paste(MODEL_HOME, "ctl/", cov_runno,".ctl", sep="")
    writeLines(temp.CTL, con=ctl.file,sep = "\n", useBytes = FALSE)

   # Step 4:  run it!
    if ("run" %in% to_do_lst)  {stopifnot(runit(MODEL_HOME, cov_runno, max_wait_min=max_wait_min))}

    # for next step
    base_runno = cov_runno
    #cov_name_lst = fad_cov_name_lst # c("THETA-23 WEIGHT ON V2", "THETA-16 WEIGHT ON CL")
  }


  ##############################################################################                        
  # forward addition 
  ############################################################################## 
  #fad_threshold=-7 
  
  if ("fad" %in% step) {
    print("forward addition...")

    #cov_name_lst = c( "THETA-9 WEIGHT ON KA",  "THETA-16 WEIGHT ON CL",     "THETA-23 WEIGHT ON V2")  # ,  "THETA-10 AGE ON KA", "THETA-17 AGE ON CL", "THETA-24 AGE ON V2"   )
    #fad_threshold = 0
    #base_runno = cov_runno
    #to_do_lst=c("write",  "run",  "read")  # c("write", "run", "read")

    tt = dev_cov_model(MODEL_HOME, step="fad", base_runno, cov_name_lst, threshold=fad_threshold, max_wait_min, to_do_lst)
    fad_CTL = tt$CTL
    fad_cov_name_lst= tt$COV  # cov.list.found
    print(tt$OBJ)
    print(paste("Selected covariates: ", paste(tt$COV, collapse=", "), sep=""))


    runno = paste(cov_runno, "_fad_final", sep="")
    ctl.file = paste(MODEL_HOME, "ctl/", runno,".ctl", sep="")
    writeLines(fad_CTL, con=ctl.file, sep = "\n", useBytes = FALSE)

    if ("run" %in% to_do_lst)  {stopifnot(runit(MODEL_HOME, runno, max_wait_min))}
    
    # new base_runno for backward elimiation
    base_runno = runno
    cov_name_lst = fad_cov_name_lst # c("THETA-23 WEIGHT ON V2", "THETA-16 WEIGHT ON CL")
  }
  
  
  ##############################################################################                        
  # backward elimiation 
  ##############################################################################
  if ("bel" %in% step) {
    print("backward elimiation ...")

    # backward elimination
    #to_do_lst=c("write", "run",  "read")  # c("write", "run", "read")
    tt = dev_cov_model(MODEL_HOME, step="bel", base_runno, cov_name_lst, threshold=bel_threshold, max_wait_min, to_do_lst)
    bel_cov_name_lst= tt$COV  # cov.list.found
    print(tt$OBJ)
    print(paste("Selected covariates: ", paste(tt$COV, collapse=", "), sep=""))

    # remove any fake covariates
    fad_THETA = extract_ctl(fad_CTL)[["THETA"]]
    fad_THETA[setdiff(fad_cov_name_lst,  bel_cov_name_lst), "RANGE"] = "(0.001 FIX)"
    bel_CTL = update_CTL(fad_CTL,
      model.DESCP = NULL,  model.DATA = NULL, model.INPUT = NULL,
      THETA=fad_THETA, OMEGA=NULL, SIGMA= NULL)

    runno = paste(cov_runno, "_bel_final", sep="")
    ctl.file = paste(MODEL_HOME, "ctl/", runno,".ctl", sep="")
    writeLines(bel_CTL, con=ctl.file,sep = "\n", useBytes = FALSE)

    if ("run" %in% to_do_lst)  {stopifnot(runit(MODEL_HOME, runno, max_wait_min))}

    base_runno = runno
  }
       
  ##############################################################################                        
  # final model
  ############################################################################## 
  # run it  "bel_final"
  print("final model updating ...")

  #base_runno = paste(cov_runno, "_bel_final", sep="")
  base_lst = read.lst(paste(MODEL_HOME, "ctl/", base_runno, ".NM7\\", base_runno, ".lst", sep="")  )
  base_ctl = readLines(paste(MODEL_HOME, "ctl/",  base_runno, ".ctl", sep=""),warn=FALSE) 
  
  tt = extract_ctl(base_ctl)
  THETA = tt[["THETA"]]
  
  OMEGA = tt[["OMEGA"]]   
  OMEGA[, "RANGE"] = paste(unlist(lapply(base_lst$omega, function(x) paste(tail(unlist(x), n=1), collapse=" "))), " FIX", sep="")
  
  SIGMA= tt[["SIGMA"]]  
  SIGMA[, "RANGE"] = paste(unlist(lapply(base_lst$sigma, function(x) paste(tail(unlist(x), n=1), collapse=" "))), " FIX", sep="")
  
    
  sim_CTL = update_CTL(base_ctl, 
    model.DESCP = NULL,  model.DATA = NULL, model.INPUT = NULL, 
    THETA=THETA, OMEGA=OMEGA, SIGMA= SIGMA)
  
  sim_CTL[which(regexpr('$EST', sim_CTL, fixed = TRUE)>0)]  =  ""
  sim_CTL[which(regexpr('$COV', sim_CTL, fixed = TRUE)>0)]  =  "" 
  sim_CTL[which(regexpr('$SIMULATION', sim_CTL, fixed = TRUE)>0)]  = "$SIMULATION (20102) (330333 UNIFORM) ONLYSIM SUBPROBLEMS=1"
                  
  runno = paste(cov_runno, "_sim_final", sep="")
  ctl.file = paste(MODEL_HOME, "ctl/", runno,".ctl", sep="")
  writeLines(sim_CTL, con=ctl.file,sep = "\n", useBytes = FALSE) 
   
  #if ("run" %in% to_do_lst)  {stopifnot(runit(MODEL_HOME, runno, max_wait_min))}

  return(sim_CTL)
  
  }
  
  





  ##############################################################################
  ##############################################################################
  # Both forward addition and backward elimination
  ##############################################################################
  ##############################################################################  
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

  dev_base_model <- function(MODEL_HOME, step="bel", base_runno, cov_name_lst, threshold=14, max_wait_min=10, to_do_lst = c("write", "run", "read")) {
   
  # read the base cov model
  #-----------------------------------------------------------------------------
  #print(base_runno)
  
  base_lst = read.lst(paste(MODEL_HOME, "ctl/", base_runno, ".NM7\\", base_runno, ".lst", sep="")  )
  base_ctl = readLines(paste(MODEL_HOME, "ctl/",  base_runno, ".ctl", sep=""),warn=FALSE) 

  return(NULL)
  }

  ##############################################################################
  ##############################################################################
  # Both forward addition and backward elimination
  ##############################################################################
  ##############################################################################  
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

  dev_cov_model <- function(MODEL_HOME, step="bel", base_runno, cov_name_lst, threshold=14, max_wait_min=10, to_do_lst = c("write", "run", "read")) {
   
  # read the base cov model
  #-----------------------------------------------------------------------------
  #print(base_runno)
  
  base_lst = read.lst(paste(MODEL_HOME, "ctl/", base_runno, ".NM7\\", base_runno, ".lst", sep="")  )
  base_ctl = readLines(paste(MODEL_HOME, "ctl/",  base_runno, ".ctl", sep=""),warn=FALSE) 

  
  # alternate the covariate model parameters
  #-----------------------------------------------------------------------------   
 
  cov_name_lst0 =   cov_name_lst
  THETA = extract_ctl(base_ctl)[["THETA"]]
  #THETA[1:length(base_lst$thetas), "RANGE"] = paste(" ", u.signif(base_lst$thetas, digits =3), " FIX", sep="")
  THETA0 = THETA
  
  base.OFV = base_lst$ofv
  base.COV = c(base_runno, "", round(base.OFV,digits=3), "Base")
 
  # Initialization for the loop
  temp.CTL = base_ctl    
  iround = 1
  cov.list.found = NULL
  OBJ.OUT = NULL
  
  while (length(cov_name_lst)>0)    { 
      tt = ifelse(step=="fad", "forward_addition_iter: ", "backward_elimiation_iter: ")
      print(paste(tt, iround, sep=""))
    
      runno_lst = NULL
      ctl.file.final = NULL
      runit.file.final = NULL
      THETA0 = THETA
      for (i in 1:length(cov_name_lst)) {
       
          # ctl.file
          #---------------------------   
          if (step=="fad") {    # forward_addition) {
          THETA = THETA0
          THETA[which(THETA$PARAM %in% cov_name_lst), "RANGE"]    = " 0.001 FIX"         # put everything into fix
          THETA[which(THETA$PARAM %in% cov_name_lst[i]), "RANGE"] = " 0.010"        # except one covariate
          model.NUM = paste(base_runno,"_", "fad", iround, add_prefix(i, prefix="", digits=2), sep="")          
          }
          
          if (step=="bel") {    # backward_elimination) {
          THETA = THETA0
          #THETA[which(THETA$PARAM %in% cov_name_lst), "RANGE"]    = " 0 FIX"         # put everything into fix
          THETA[which(THETA$PARAM %in% cov_name_lst[i]), "RANGE"] = " 0.001 FIX"       # except one covariate
          model.NUM = paste(base_runno,"_", "bel", iround, add_prefix(i, prefix="", digits=2), sep="")          
          }
          runno_lst = c(runno_lst, model.NUM)
          
          tt = update_CTL(temp.CTL,  
               model.DESCP=NULL,  model.DATA=NULL,  model.INPUT=NULL, 
               THETA, OMEGA=NULL,  SIGMA=NULL)
        
          ctl.file = paste(MODEL_HOME, "ctl/", model.NUM,".ctl", sep="")
          if ("write" %in% to_do_lst) {writeLines(tt, con=ctl.file,sep = "\n", useBytes = FALSE)}
          ctl.file.final = rbind(ctl.file.final, ctl.file) 
           
          # runit.file
          #---------------------------
          runit.file = runit.ctl(MODEL_HOME, runno=model.NUM)
          runit.file.final = rbind(runit.file.final, runit.file)      
       }
        
      #ctl.file.lst = gsub(paste(MODEL_HOME, "ctl/",sep=""),"", ctl.file.final, fix=TRUE)
      #ctl.file.lst = gsub(".ctl", "", ctl.file.lst, fix=TRUE)
      ctl.file.lst = runno_lst
      
      # if save in 36318, and run in 28695
      #-----------------------------------------------------------------------------
      tt = runit.file.final
      tt = paste("system('c:/WINDOWS/system32/cmd.exe /c   ", tt, "', wait = FALSE)", sep="")
      tt = c(paste("setwd('", MODEL_HOME, "ctl//", "')", sep=""), tt )        
      tt[length(tt)+1] = "print('all runs were finished!')"
      
      t1 = ifelse(step=="fad", "forward", "backward")
      file.name=paste(MODEL_HOME, paste(t1, "_iter",iround,".txt", sep=""), sep="")
      if ("write" %in% to_do_lst) {writeLines(tt, con = file.name, sep = "\n", useBytes = FALSE) }
       
       
       # if run in 28695
      #-----------------------------------------------------------------------------

      if ("run" %in% to_do_lst) {stopifnot(runit(MODEL_HOME, runno_lst, max_wait_min))}

      #------------------------------------
      # Read the model results
      #------------------------------------
      if ("read" %in% to_do_lst) {
        OBJ = NULL
        for (i in 1:length(ctl.file.lst) ) {
            file.name = paste(MODEL_HOME, "ctl/", ctl.file.lst[i], ".NM7\\", ctl.file.lst[i], ".lst", sep="")
             #print(file.name)
            tt = read.lst(file.name)
            if (is.null(tt$ofv)) next
            OBJ =  rbind(OBJ,  c(ctl.file.lst[i], cov_name_lst[i], round(tt$ofv,digits=3), ""))
      }}
      OBJ = data.frame(OBJ, stringsAsFactors = FALSE)
      colnames(OBJ) = c("runno", "cov.name", "obj", "note")
      OBJ$obj = round(as_numeric(OBJ$obj), digits=3)
      if (step=="fad") {OBJ = OBJ[order(OBJ$obj), ] }
      if (step=="bel") {OBJ = OBJ[order(OBJ$obj, decreasing=TRUE), ] } 
      
      # the current covariate with the lowest OFV
      curr.OFV = as_numeric(OBJ[1, "obj"])   # the lowest OFV
      curr.COV = OBJ[1, ]

      # in relatvie to the base.COV
      OBJ$obj = OBJ$obj - round(base.OFV, digits=3)
      if (step=="fad") {OBJ[which(OBJ$obj<=(threshold)), "note"] = "Y" }
      if (step=="bel") {OBJ[which(OBJ$obj>=(threshold)), "note"] = "Y" }      
      OBJ.OUT = rbind(OBJ.OUT, "", base.COV, OBJ)
      
      cov_name_lst = OBJ[which(OBJ$note=="Y"), "cov.name"]     # candidate cov list 
      if (length(cov_name_lst)==0)  {
       base_runno = OBJ.OUT[tail(which(OBJ.OUT$note=="Selected"), n=1), "runno"]
       temp.CTL = update_CTL_with_LST(MODEL_HOME, base_runno, cov.list.found)  
       return(list(COV=unlist( cov.list.found), RUNNO=base_runno, OBJ=OBJ.OUT, THETA=THETA, CTL=temp.CTL))}  
      
      
      #------------------------------------
      # Prepare for next interaction
      #------------------------------------
      iround = iround + 1
      cov.list.found = c(cov.list.found, as.character(curr.COV["cov.name"]))
      cov_name_lst = setdiff(cov_name_lst,  cov.list.found)  
      if (length(cov_name_lst)==0)  {
       #OBJ.OUT[tail(which(OBJ.OUT$note=="Y"), n=1), "note"] = Selected
       base_runno = OBJ.OUT[tail(which(OBJ.OUT$note=="Y"), n=1), "runno"]
       temp.CTL = update_CTL_with_LST(MODEL_HOME, base_runno, cov.list.found)  
       return(list(COV=unlist( cov.list.found), RUNNO=base_runno, OBJ=OBJ.OUT, THETA=THETA, CTL=temp.CTL))} 
                 
      curr.COV["note"] = "Selected"  #  paste(in cycle ", iround, " forward addition", sep="")            
      base.OFV = curr.OFV
      base.COV = curr.COV
  
      # update the current.CTL by newly founded cov.param value
      if (step=="fad") {
        temp.CTL = readLines(paste(MODEL_HOME,"ctl/", curr.COV["runno"], ".CTL", sep=""))  
        temp.RUNNO = curr.COV["runno"]   
        temp.LST = read.lst(paste(MODEL_HOME, "ctl/", temp.RUNNO, ".NM7\\", temp.RUNNO, ".lst", sep="")       )
        ttt = unlist(temp.LST$theta); names(ttt) = THETA$PARAM;   
        THETA[which(THETA$PARAM %in% setdiff(cov_name_lst0, cov.list.found)), "RANGE"]    = " 0.001 FIX"           
        THETA[which(THETA$PARAM %in% curr.COV["cov.name"]), "RANGE"] = paste(ttt[as.character(curr.COV["cov.name"])], "  FIX", sep="")      # except one covariate
      }
      
      if (step=="bel") {
        # Doing nothing
      }
      
                  
      temp.CTL = update_CTL(temp.CTL, 
          model.DESCP = NULL,  model.DATA = NULL, model.INPUT = NULL, 
          THETA, OMEGA= NULL, SIGMA= NULL)
               
    
      OBJ.OUT
      THETA
      temp.CTL
  }   # end of round
  
   
 }
  
    
    
  ##############################################################################
  ##############################################################################
  # update a CTL with the corresponding LST [under MODEL_HOME]
  ##############################################################################
  ##############################################################################  
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
 update_CTL_with_LST <- function(MODEL_HOME, base_runno, cov_name_lst) {
      # update the current.CTL by newly founded cov.param value, i.e. cov_name_lst
      #cov_name_lst= c( "THETA-9 WEIGHT ON KA",  "THETA-16 WEIGHT ON CL" ) 
            
      #print(base_runno)
      base_lst = read.lst(paste(MODEL_HOME, "ctl/", base_runno, ".NM7\\", base_runno, ".lst", sep="")  )
      base_ctl = readLines(paste(MODEL_HOME, "ctl/",  base_runno, ".ctl", sep=""),warn=FALSE) 

      THETA = extract_ctl(base_ctl)[["THETA"]]
      tt = paste(" ", u.signif(base_lst$thetas, digits =3), " FIX", sep="")
      names(tt) = THETA[, "PARAM"]
      THETA[as.character(cov_name_lst), "RANGE"] = tt[as.character(cov_name_lst)] 
                
      temp.CTL = update_CTL(base_ctl, 
        model.DESCP = NULL,  model.DATA = NULL, model.INPUT = NULL, 
        THETA=THETA, OMEGA=NULL, SIGMA= NULL)
  
  return(temp.CTL )
  }
  
  
        


################################################################################
################################################################################
#  update.nonmem.model
################################################################################
################################################################################
#' Extracts the time matched concntration vs effect data
#' @param data A data frame.  The data frame must conform to Regeneron standards and contain the variables: `week`, `ndy`, `concn`, `concni`, `pchg`, `actarmcd`, `ada`
#' @return A dataframe with columns `ndy`, `concn`, `effect`.
#' @export
#' @importFrom dplyr left_join group_by summarise
#' @examples
#'   inFile <- system.file("extdata/pk_pd_hysteresis.xls", package = "pkGraph")
#'   library(readxl)
#'   theData <-read_excel(inFile)
#'   timeMatchedPK(data = theData)
  update_CTL <- function(template.CTL,
        model.DESCP=NULL,
        model.DATA=NULL,
        model.INPUT=NULL,

        THETA=NULL,
        OMEGA=NULL,
        SIGMA=NULL) {

      t001 = template.CTL

     ids.EST = which(regexpr('$EST',t001, fix=TRUE)>0)


      # PROBLEM
      if (!is.null(model.DESCP )) {
      t001[which(regexpr('$PROBLEM', t001, fixed = TRUE)>0)]  =  model.DESCP }


      # DATA
      if (!is.null(model.DATA )) {
      t001[which(regexpr('$DATA', t001, fixed = TRUE)>0)]  =    model.DATA }

      # INPUT    #### not model.INPUT has $INPUT
      if (!is.null(model.INPUT )) {
      ids.from=which(regexpr('$INPUT', t001, fixed = TRUE)>0)
      ids.to = which(regexpr('$DATA', t001, fixed = TRUE)>0)
      t001[seq(ids.from,ids.to-1,by = 1)] = ""

      tt = strwrap(model.INPUT, width=100)
      t001 = c(t001[1:ids.from], tt, "",  t001[ids.to:length(t001)])   }

      # Parameters: THETA, OMEGA, SIGMA
      if (!is.null(THETA )) {
      for (i in 1:nrow(THETA)) {
         parms.name = THETA[i,"PARAM"]
         range.name = paste(THETA[i,"RANGE"], "  ;",  THETA[i,"PARAM"],sep="")
         t001[which(regexpr(parms.name, t001)>0)]  =  range.name
      }}

      if (!is.null(OMEGA )) {
      for (i in 1:nrow(OMEGA)) {
         parms.name =OMEGA[i,"PARAM"]
         range.name = paste(OMEGA[i,"RANGE"], "  ;",  OMEGA[i,"PARAM"],sep="")
         t001[which(regexpr(parms.name, t001)>0)]  =  range.name
      }}

      if (!is.null(SIGMA )) {
      for (i in 1:nrow(SIGMA)) {
         parms.name =SIGMA[i,"PARAM"]
         range.name = paste(SIGMA[i,"RANGE"], "  ;",  SIGMA[i,"PARAM"],sep="")
         t001[which(regexpr(parms.name, t001)>0)]  =  range.name
      }}

      # sdtab file
      #
      template.RUNNO = "001"   # names(template.CTL)
      model.RUNNO= "001"

      curr.file = paste('sdtab', template.RUNNO, sep="")
      ids = which(regexpr(curr.file, t001, fixed = TRUE)>0)
      t001[ids]  = gsub(curr.file, paste("sdtab",model.RUNNO,sep=""), t001[ids],fix=TRUE)

      # patab file
      curr.file = paste('patab', template.RUNNO,sep="")
      ids = which(regexpr(curr.file, t001, fixed = TRUE)>0)
      t001[ids]  = gsub(curr.file, paste("patab",model.RUNNO,sep=""), t001[ids],fix=TRUE)

      # catab file
      curr.file = paste('catab', template.RUNNO,sep="")
      ids = which(regexpr(curr.file, t001, fixed = TRUE)>0)
      t001[ids]  = gsub(curr.file, paste("catab",model.RUNNO,sep=""), t001[ids],fix=TRUE)

      # cotab file
      curr.file = paste('cotab', template.RUNNO,sep="")
      ids = which(regexpr(curr.file, t001, fixed = TRUE)>0)
      t001[ids]  = gsub(curr.file, paste("cotab",model.RUNNO,sep=""), t001[ids],fix=TRUE)

      return(t001)
  }



#' Extracts the time matched concntration vs effect data
#' @param data A data frame.  The data frame must conform to Regeneron standards and contain the variables: `week`, `ndy`, `concn`, `concni`, `pchg`, `actarmcd`, `ada`
#' @return A dataframe with columns `ndy`, `concn`, `effect`.
#' @export
#' @importFrom dplyr left_join group_by summarise
#' @examples
#'   inFile <- system.file("extdata/pk_pd_hysteresis.xls", package = "pkGraph")
#'   library(readxl)
#'   theData <-read_excel(inFile)
#'   timeMatchedPK(data = theData)
   extract_ctl <- function(base_ctl) {
       
     #base_ctl = base_ctl[which(unlist(lapply(strsplit(base_ctl, ""), function(x) {x[1]})) != ";")]
     base_ctl = base_ctl[which(substr( trim(base_ctl), 1,1)!=";")]   

   
      #ids.DESCP = which(regexpr(';', base_ctl, fix=TRUE)>0)
      DESCP  = "" # base_ctl[ids.DESCP]  # t
      
   
      # DESCP
      ids.DESCP = which(regexpr('$PROBLEM', base_ctl, fix=TRUE)>0)
      DESCP  = base_ctl[ids.DESCP]  # t
      #template.DESCP = "$PROBLEM  Linear 2-compartmental model (Concentration only)"
      #template.DESCP =  "$PROBLEM  Michael-Menton model (Concentration only)"

      # DATA
      ids.DATA = which(regexpr('$DATA', base_ctl, fix=TRUE)>0)
      DATA  = base_ctl[ids.DATA]  # template.CTL[ids.DATA:(ids.SUBROUTINE-1)]

      # INPUT
      ids.INPUT = which(gregexpr('$INPUT', base_ctl, fix=TRUE)>0)
      INPUT = base_ctl[ids.INPUT:(ids.DATA-1)]

      # SUBROUTINE
      ids.SUBROUTINE =  which(regexpr('$SUBROUTINE', base_ctl, fix=TRUE)>0)
      SUBROUTINE  = base_ctl[ids.SUBROUTINE]  # template.CTL[ids.DATA:(ids.SUBROUTINE-1)]

      #-----------------------------------------------------------------------------
      # THETA, OMEGA, SIGMA, [must be consistent with model control stream]
      #-----------------------------------------------------------------------------
      # Estimation of model parameters

      ids.THETA = which(regexpr('$THETA', base_ctl, fix=TRUE)>0)
      ids.OMEGA = which(regexpr('$OMEGA', base_ctl, fix=TRUE)>0)
      ids.SIGMA = which(regexpr('$SIGMA', base_ctl, fix=TRUE)>0)
      ids.EST = which(regexpr('$EST', base_ctl, fix=TRUE)>0)

      THETA = base_ctl[(ids.THETA[1]+1):(ids.OMEGA[1]-1)]
      OMEGA = base_ctl[setdiff((ids.OMEGA[1]+1):(ids.SIGMA[1]-1), ids.OMEGA[2:length(ids.OMEGA)])]
      SIGMA = base_ctl[(ids.SIGMA[1]+1):(ids.EST[1]-1)]

      library("gdata")
      THETA = gdata::trim(colsplit(matrix(THETA,ncol=1),";", c("RANGE", "PARAM")))
      OMEGA = gdata::trim(colsplit(matrix(OMEGA,ncol=1),";", c("RANGE", "PARAM")))
      SIGMA = gdata::trim(colsplit(matrix(SIGMA,ncol=1),";", c("RANGE", "PARAM")))

      THETA = THETA[which(THETA$PARAM!=""),]; rownames(THETA)= THETA[, "PARAM"]
      OMEGA = OMEGA[which(OMEGA$PARAM!=""),]; rownames(OMEGA)= OMEGA[, "PARAM"]
      SIGMA = SIGMA[which(SIGMA$PARAM!=""),]; rownames(SIGMA)= SIGMA[, "PARAM"]

  return(list(DESCP=DESCP, DATA=DATA, INPUT=INPUT, SUBROUTINE=SUBROUTINE, THETA=THETA, OMEGA=OMEGA, SIGMA=SIGMA))
  }




################################################################################
################################################################################
#  make runit batch file
################################################################################
################################################################################
#' Extracts the time matched concntration vs effect data
#' @param data A data frame.  The data frame must conform to Regeneron standards and contain the variables: `week`, `ndy`, `concn`, `concni`, `pchg`, `actarmcd`, `ada`
#' @return A dataframe with columns `ndy`, `concn`, `effect`.
#' @export
#' @importFrom dplyr left_join group_by summarise
#' @examples
#'   inFile <- system.file("extdata/pk_pd_hysteresis.xls", package = "pkGraph")
#'   library(readxl)
#'   theData <-read_excel(inFile)
#'   timeMatchedPK(data = theData)
    runit.ctl <- function(MODEL_HOME, runno) {

          ctl.file = paste(runno, ".ctl", sep="")
          runit.file = paste(MODEL_HOME,"ctl/", "run_", runno, ".bat", sep="")

          t001 = readLines(paste(MODEL_HOME,"lib/", "runit.bat", sep=""))
          ids = which(regexpr('c:/nm73g64/wfn7/bin/nmgo.bat', t001)>0)
          t001[ids-1] = paste("cd  ", MODEL_HOME, "ctl/", sep="")
          t001[ids]  =  paste("c:/nm73g64/wfn7/bin/nmgo.bat  ", ctl.file,sep="")

          writeLines(t001, con=runit.file, sep = "\n", useBytes = FALSE)                  #  parms.TRIG = c(1, -0.79, 19.1, 1)   #  fitemax$coefs
        return(runit.file)
    }



          #
#
#        # output to drive X:
#        runit.file = paste(MODEL_HOME, "ctl/", "runit_", runno, ".bat", sep="")
#
#        # run in drive C:
#        #WORK.DIREC = gsub("X:/", "C:/FYANG/", WORK.DIREC, fix=TRUE)
#        ctl.file = paste(runno, ".ctl", sep="")
#
#        t001 = readLines("H:/FYANG/aTemplate/NONMEM MODEL LIB/runit.bat")                  #  parms.TRIG = c(1, -0.79, 19.1, 1)   #  fitemax$coefs
#        ids = which(regexpr('c:/nm73g64/wfn7/bin/nmgo.bat', t001)>0)
#
#        t001[ids-1] = paste("cd  ", WORK.DIREC, sep="")
#        t001[ids]  =  paste("c:/nm73g64/wfn7/bin/nmgo.bat  ", ctl.file,sep="")
#
#        writeLines(t001, con=runit.file, sep = "\n", useBytes = FALSE)                  #  parms.TRIG = c(1, -0.79, 19.1, 1)   #  fitemax$coefs
#
#        runit.file = gsub("X:/", "C:/FYANG/", runit.file, fix=TRUE)
#        return(runit.file)
#    }
#


#' Extracts the time matched concntration vs effect data
#' @param data A data frame.  The data frame must conform to Regeneron standards and contain the variables: `week`, `ndy`, `concn`, `concni`, `pchg`, `actarmcd`, `ada`
#' @return A dataframe with columns `ndy`, `concn`, `effect`.
#' @export
#' @importFrom dplyr left_join group_by summarise
#' @examples
#'   inFile <- system.file("extdata/pk_pd_hysteresis.xls", package = "pkGraph")
#'   library(readxl)
#'   theData <-read_excel(inFile)
#'   timeMatchedPK(data = theData)
    batch.runit.ctl <- function(runit.file.final, batch.file="c:/fyang/runbat888.txt") {
        #tt = gsub("X:/", "C:/FYANG/", runit.file.final, fix=TRUE)
        tt = runit.file.final
        tt = paste("system('c:/WINDOWS/system32/cmd.exe /c   ", tt, "', wait = FALSE)", sep="")
        tt[length(tt)+1] = "print('all runs were finished!')"
        #file.name=paste(WORK.DIREC, batch.file, sep="")
        writeLines(tt, con = batch.file, sep = "\n", useBytes = FALSE)
    }







  #------------------------------------
  # Read the model results
  #------------------------------------

#' Extracts the time matched concntration vs effect data
#' @param data A data frame.  The data frame must conform to Regeneron standards and contain the variables: `week`, `ndy`, `concn`, `concni`, `pchg`, `actarmcd`, `ada`
#' @return A dataframe with columns `ndy`, `concn`, `effect`.
#' @export
#' @importFrom dplyr left_join group_by summarise
#' @examples
#'   inFile <- system.file("extdata/pk_pd_hysteresis.xls", package = "pkGraph")
#'   library(readxl)
#'   theData <-read_excel(inFile)
#'   timeMatchedPK(data = theData)
  runit <- function(MODEL_HOME, subdir="ctl/", runno, max_wait_min=60) {
    time0 = Sys.time()
    
    # before runs, remove the existing folders
    folder.name = paste(MODEL_HOME, subdir, runno, ".nm7", sep="")
    tt = lapply(folder.name, function(x) {
           unlink(x, recursive = TRUE, force = FALSE) } )
    
    # Sys.sleep(x)
    testit <- function(x) {
       p1 <- proc.time()
       Sys.sleep(x)
       proc.time() - p1 # The cpu usage should be negligible
    }

    # put them onto the NONMEM runs
    for (i in 1:length(runno))  {
      tt = runit.ctl(MODEL_HOME, runno[i])
      system(paste('c:\\WINDOWS\\system32\\cmd.exe /c   ',  tt, sep=""), wait = FALSE)  }

    # wait until check whether all runs are finished
    time0 = Sys.time()
    while (check_runs(MODEL_HOME, runno)!=TRUE )     {
       time1 = Sys.time()
       if (difftime(time1, time0, units="mins")> max_wait_min) {break;}   # if greater than 1 hour, break
       testit(15) }  # Wait 15 seconds

    #final return
    Sys.sleep(15)   # wait another 15 second for all files finished to write on the hard drive
    YES = check_runs(MODEL_HOME, runno)
    
    if(YES) {
    time1 = Sys.time()
      print(paste("All runs finished within ", round(difftime(time1, time0, units="mins"),digits=2), " minutes.",sep=""))  
      }else{
      print(paste("Failed after ", round(difftime(time1, time0, units="mins"),digits=2), " minutes.",sep=""))  
      }
      
    return(YES)
    }


#' Extracts the time matched concntration vs effect data
#' @param data A data frame.  The data frame must conform to Regeneron standards and contain the variables: `week`, `ndy`, `concn`, `concni`, `pchg`, `actarmcd`, `ada`
#' @return A dataframe with columns `ndy`, `concn`, `effect`.
#' @export
#' @importFrom dplyr left_join group_by summarise
#' @examples
#'   inFile <- system.file("extdata/pk_pd_hysteresis.xls", package = "pkGraph")
#'   library(readxl)
#'   theData <-read_excel(inFile)
#'   timeMatchedPK(data = theData)
  check_runs <- function(MODEL_HOME, runno) {
    for (i in 1:length(runno) ) {
        file.name = paste(MODEL_HOME, "ctl/", runno[i], ".NM7\\", "SDTAB001", sep="")
        if(file.exists(file.name)) {next
        }else{
        return(FALSE) }

    }
    return(TRUE)
 }

       



#' Extracts the time matched concntration vs effect data
#' @param data A data frame.  The data frame must conform to Regeneron standards and contain the variables: `week`, `ndy`, `concn`, `concni`, `pchg`, `actarmcd`, `ada`
#' @return A dataframe with columns `ndy`, `concn`, `effect`.
#' @export
#' @importFrom dplyr left_join group_by summarise
#' @examples
#'   inFile <- system.file("extdata/pk_pd_hysteresis.xls", package = "pkGraph")
#'   library(readxl)
#'   theData <-read_excel(inFile)
#'   timeMatchedPK(data = theData)
  load_runno <- function(MODEL_HOME, subdir="ctl/", runno)  {
   
    # paste(table.names, runno, tab.suffix, sep="")
    # paste(cwres.name,runno,cwres.suffix,tab.suffix,sep="") The default CWRES table file name is called: 

    #If there are simulation files present then Xpose looks for the files to be named: 
    # paste(table.names, runno, sim.suffix, tab.suffix, sep="")
    # paste(cwres.name,runno,sim.suffix,cwres.suffix,tab.suffix,sep="")
 
 
 # need both sdtab001 and patab001,  "IPRED", +  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    library(xpose4) #read in the xopse library 
    
    xpdb <- xpose.data("001", tab.suffix = "", directory=paste(MODEL_HOME, subdir, runno, ".nm7/", sep="")) # run number of xpose table files
  
    xpdb <- slot(xpdb, "Data")
  
    xpdb$DV <- exp(as_numeric(xpdb$DV))
    xpdb$IPRED <- exp(as_numeric(xpdb$IPRED))
    xpdb$PRED <- exp(as_numeric(xpdb$PRED))
   
 
    #-----------------------------------------------------------------------------
    # attach adpx information (from nonmem datafile)
    #----------------------------------------------------------------------------- 
    
    base_ctl = readLines(paste(MODEL_HOME, subdir,  runno, ".ctl", sep=""),warn=FALSE) 

    library("readr")
    library("dplyr")
  
    # need to remove all ; comments
    # need to add ROWID in the dataset (adpx)
  
    tt = unlist(strsplit(base_ctl[which(regexpr('$DATA', base_ctl, fix=TRUE)>0)], " "))     
    tt = gsub("..", "", tt[which(regexpr('.csv', tt, fix=TRUE)>0)], fix=TRUE)
    tt = gsub(",", "", tt, fix=TRUE)
        
    adpx <- adpx0 <-  read_csv(file = paste(MODEL_HOME, tt, sep=""), skip=0)
    colnames(adpx) = gsub("=DROP", "", colnames(adpx), fix=TRUE)
    adpx$ARMA = gsub("-", " ", adpx$ARMA, fix=TRUE)
    adpx$ARMA = trim(gsub("INFUSION", "", adpx$ARMA, fix=TRUE))
         
    #xpdb2: extended version of xpdb 
    xpdb2 = xpdb #%>% filter(MDV==0) 
    tt = adpx %>% filter(EVID==0) %>% select(one_of(c("ROWID", setdiff(colnames(adpx), colnames(xpdb)))))  #%>% distinct(USUBJID, .keep_all=TRUE)               
    xpdb2 = left_join(xpdb2, tt, by="ROWID")
    
    #adpx2: extended version of adpx     
    tt = xpdb %>% select(one_of(c("ID", setdiff(colnames(xpdb), colnames(adpx))))) %>% distinct(ID, .keep_all=TRUE)               
    adpx2 = left_join(adpx, tt, by="ID")
      
        
    base_lst = read.lst(paste(MODEL_HOME, subdir, runno, ".NM7\\", runno, ".lst", sep="")  )
    THETA = base_lst$thetas
        
    # obtain the names of THETA           
    tdata = extract_ctl(base_ctl)$THETA  
    tdata = colsplit(tdata$PARAM, " ", c("THETA", "PARAM"))
    names(base_lst$thetas) <- tdata$PARAM
     
      
              
    return(list(xpdb=xpdb, xpdb2=xpdb2, adpx=adpx, adpx2=adpx2, adpx0=adpx0, base_lst=base_lst, base_ctl=base_ctl))
  }
  


#' Extracts the time matched concntration vs effect data
#' @param data A data frame.  The data frame must conform to Regeneron standards and contain the variables: `week`, `ndy`, `concn`, `concni`, `pchg`, `actarmcd`, `ada`
#' @return A dataframe with columns `ndy`, `concn`, `effect`.
#' @export
#' @importFrom dplyr left_join group_by summarise
#' @examples
#'   inFile <- system.file("extdata/pk_pd_hysteresis.xls", package = "pkGraph")
#'   library(readxl)
#'   theData <-read_excel(inFile)
#'   timeMatchedPK(data = theData)
################################################################################
#  output parameter estimate from CTL model result
################################################################################

  extract_parms <- function(MODEL_HOME, runno) {
  
  # Step 1: read base model
  #base_runno = "002" 
  model_lst = read.lst(paste(MODEL_HOME, "ctl/", runno, ".NM7\\", runno, ".lst", sep="")  )
  model_ctl = readLines(paste(MODEL_HOME, "ctl/",  runno, ".ctl", sep=""),warn=FALSE)
  
  
  #model_ctl = model_ctl[which(unlist(lapply(strsplit(model_ctl, ""), function(x) {x[1]})) != ";")]
  THETA = extract_ctl(model_ctl)[["THETA"]]
  OMEGA = extract_ctl(model_ctl)[["OMEGA"]]
  SIGMA = extract_ctl(model_ctl)[["SIGMA"]]


  # calcualte RSE
  parms = c(unlist(model_lst$thetas), diag(do.call(cbind, model_lst$omega)), unlist(model_lst$sigma))
  rse =  c(round(model_lst$sethetas/model_lst$thetas*100 , digits=2) ,
  round(diag(do.call(cbind, model_lst$seomega))/diag(do.call(cbind, model_lst$omega))*100 , digits=2),
  round(diag(do.call(cbind, model_lst$sesigmas))/diag(do.call(cbind, model_lst$sigma))*100 , digits=2) )
  
  #THETA = NULL  
  ttt = unlist(model_lst$theta);  n.parms = length(ttt) 
  names(ttt) = THETA$PARAM[1:n.parms];   
  THETA[1:n.parms, "RANGE"]    = paste(unlist(model_lst$theta), "  FIX", sep="")
  
  #temp.LST$omega[[2]] =   temp.LST$omega[[2]][2:length(temp.LST$omega[[2]])]
  #temp.LST$omega[[3]] =   temp.LST$omega[[3]][2:length(temp.LST$omega[[3]])]
  #temp.LST$omega[[4]] =   temp.LST$omega[[4]][2:length(temp.LST$omega[[4]])]  
  #temp.LST$omega[[5]] =   temp.LST$omega[[5]][5:length(temp.LST$omega[[5]])] 
  #OMEGA = NULL          
  OMEGA[, "RANGE"]    = unlist(lapply(model_lst$omega, function(x) paste(tail(unlist(x), n=1), collapse=" ")))
 
  #SIGMA = NULL 
  SIGMA[, "RANGE"]    = unlist(lapply(model_lst$sigma, function(x) paste(unlist(x), collapse=" ")))

  names = c(THETA$PARAM, OMEGA$PARAM, SIGMA$PARAM)
  parms = round(parms, digits=2)
  out = data.frame(names, parms, rse)
    
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
#     x = as.character(out$names)
#     DF <- read.table(textConnection(x), sep = " ", fill =NA)  #  DF 
##      as.data.frame( t(sapply(1:4, function(x) strsplit(vec, "_")[[x]])) ) 
##      out <- data.frame( do.call( rbind, strsplit( string, '_' ) ) ) 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -    
x = as.character(out$names)
x=cbind(read.table(textConnection(x), sep = "", fill =NA), out[, 2:3])  
x=cbind(read.table(textConnection(as.character(x[, 1])), sep = "-", fill =NA), x[, 2:4])
colnames(x) = c("TYPE", "INDEX", "NAME", "PARMS", "RSE")
x$ID= paste(x$TYPE, x$NAME, sep="-")
x

  
  
################################################################################
#  confident interval
################################################################################
 calc_conf <- function(data0, conf=0.95) {
  library(boot) 
      data0 = as_numeric(data0)
      data0 = data0[!is.na(data0)]
      if (length(unique(data0))==1)  {return(as.character("---"))}

      # Statistic (MLE)
      mle = function(dat){
        m = median(log(dat))
        s = median((log(dat)-m)^2)
        return(exp(m+s/2))
      }
     samp.mean = function(dat) return(median(dat))
 
      # Bootstrap
        set.seed(1)
      #boots.out = boot(data=data0, statistic=function(d, ind){mle(d[ind])}, R = 10000)
      boots.out = boot(data=data0, statistic=function(d, ind){samp.mean(d[ind])}, R = 10000)
      #plot(density(boots.out$t))
      # 4 types of Bootstrap confidence intervals
#boot.ci(boots.out, type = c("norm", "basic", "perc", "stud"), 
#        h = log, hdot = function(x) 1/x)
#        
#   quantile(data0, c(0.95), type=2, na.rm=TRUE)      
#        
              
  tt = boot.ci(boots.out, conf = conf, type = "bca")
  tt = u.signif(tt$bca, digits=3)[4:5]
  tt = as.character(paste("[", tt[1], "-", tt[2], "]", sep="")) 
  return(tt)  
  }
  
   
 
  parm.lst = x %>% filter(TYPE=="OMEGA") %>% select(NAME)
  parm.lst = as.character(unlist(parm.lst))
 
 
  adpx = load_runno(MODEL_HOME, runno )
  #adpx$ARMA = factor(adpx$ARMA, levels=c("R2810: 1 mg/kg",     "R2810: 3 mg/kg", "R2810: 10 mg/kg",  "R2810: 200 mg"   ), ordered=TRUE)
  
  tt = lapply(parm.lst, function(parm.name, adpx) { 
      #print(parm.name)
      #print("Sfsf")
      tdata = adpx %>% distinct(USUBJID, .keep_all=TRUE)
      tdata[, "CL"] =  tdata[, parm.name] 
      tdata = tdata %>%  filter(!is.na(CL))
      tt= calc_conf(as_numeric(tdata$CL), conf=0.95)  
      return(tt)
  }, adpx)  %>% unlist()
  tt = data.frame("ID"=paste("THETA", parm.lst, sep="-"), "CI"=tt)
   
  
  x= left_join(x, tt, by="ID") 
    
  return(x)
  }
  


