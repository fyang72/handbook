
readLST <- function(runno) {
  library(xpose4)
  
  out <- tryCatch(
    {
      # Just to highlight: if you want to use more than one 
      # R expression in the "try" part then you'll have to 
      # use curly brackets.
      # 'tryCatch()' will return the last evaluated expression 
      # in case the "try" part was completed successfully
      
      #message("This is the 'try' part")
      
      model.lst = read.lst(paste(MODEL.HOME, "ctl/BST/", runno, ".NM7\\", runno, ".lst", sep="")  )
      
      # The return value of `readLines()` is the actual value 
      # that will be returned in case there is no condition 
      # (e.g. warning or error). 
      # You don't need to state the return value via `return()` as code 
      # in the "try" part is not wrapped insided a function (unlike that
      # for the condition handlers for warnings and error below)
    },
    error=function(cond) {
      message(paste("read error:", runno))
      message("Here's the original error message:")
      message(cond)
      # Choose a return value in case of error
      return(NULL)
    },
    warning=function(cond) {
      message(paste("readLST caused a warning:", runno))
      message("Here's the original warning message:")
      message(cond)
      # Choose a return value in case of warning
      return(NULL)
    },
    finally={
      # NOTE:
      # Here goes everything that should be executed at the end,
      # regardless of success or error.
      # If you want more than one expression to be executed, then you 
      # need to wrap them in curly brackets ({...}); otherwise you could
      # just have written 'finally=<expression>' 
      #message(paste("Processed runno:", runno))
      message(paste0("Successfully read...", runno))
    }
  )    
  return(out)
}



summary.bootstrap.results <- function(tdata) {
  PARAMS=NULL
  PARAMS[[1]]=tdata%>% filter(model==0)
  names(PARAMS) = runno.lst
  output_params = get_summary_params(PARAMS, runno.lst)
  
  param.lst = tdata %>% select(dplyr::starts_with("TV", ignore.case=FALSE), EMAX, T50, HILL, RUVCV, RUVSD, OMEGA.2.1.,  # one_of("OMEGA(2,1)"), 
                               dplyr::contains("_ON_"), 
                               dplyr::starts_with("IIV_"), 
                               -dplyr::starts_with("se")
  )  %>% colnames()
  
  tdata = tdata %>% gather(params, value, -model)
  tdata = tdata %>% filter(params %in% param.lst)  %>% mutate(value=as_numeric(value))
  
  tdata =  tdata %>% filter(model==0) %>% group_by(params) %>%
    summarise(n=n(), 
              `2.5%`=quantile(value, probs=0.025),
              `5%`=quantile(value, probs=0.05),
              `25%`=quantile(value, probs=0.25),
              `50%`=quantile(value, probs=0.5),
              `75%`=quantile(value, probs=0.75),
              `95%`=quantile(value, probs=0.95),
              `97.5%`=quantile(value, probs=0.975),
              Mean=mean(value, na.rm=TRUE), 
              Median=median(value, na.rm=TRUE)
    )
  
  output_params = output_params %>% mutate(params=rownames(output_params))
  output_params =  output_params %>% left_join(tdata, by="params")  %>% mutate(params.name = params)
  
  output_params
}


noWarning<-function(x) {suppressWarnings(suppressMessages(x))}


read_PARAMS <- function(MODEL.HOME="H:\\FYANG\\R2810_PD1\\MODEL\\", subdir="/ctl/HPC/", runno.lst) {
  # load all modeling result
  PARAMS = lapply(runno.lst, function(runno) {
    library(readr)
    
    
    
    
    tt <- tryCatch(
      {
        # Just to highlight: if you want to use more than one 
        # R expression in the "try" part then you'll have to 
        # use curly brackets.
        # 'tryCatch()' will return the last evaluated expression 
        # in case the "try" part was completed successfully
        
        #message("This is the 'try' part")
        file.name = paste0(MODEL.HOME, subdir, runno, "/modelfit_dir1/raw_results_", runno, ".csv")
        read_csv(file.name)
        #read.lst(paste(MODEL.HOME, "ctl/BST2/", runno, ".NM7\\", runno, ".lst", sep="")  )
        
        # The return value of `readLines()` is the actual value 
        # that will be returned in case there is no condition 
        # (e.g. warning or error). 
        # You don't need to state the return value via `return()` as code 
        # in the "try" part is not wrapped insided a function (unlike that
        # for the condition handlers for warnings and error below)
      },
      error=function(cond) {
        message(paste("read error:", runno))
        message("Here's the original error message:")
        message(cond)
        # Choose a return value in case of error
        return(NULL)
      },
      warning=function(cond) {
        message(paste("readLST caused a warning:", runno))
        message("Here's the original warning message:")
        message(cond)
        # Choose a return value in case of warning
        return(NULL)
      },
      finally={
        # NOTE:
        # Here goes everything that should be executed at the end,
        # regardless of success or error.
        # If you want more than one expression to be executed, then you 
        # need to wrap them in curly brackets ({...}); otherwise you could
        # just have written 'finally=<expression>' 
        #message(paste("Processed runno:", runno))
        message(paste0("Successfully read...", runno))
      }
    )     
    
    colnames(tt) = trim(colnames(tt))
    tt
  })
  
  names(PARAMS) = runno.lst
  return(PARAMS)
}


read_OUTPUT <- function(MODEL.HOME="H:\\FYANG\\R2810_PD1\\MODEL\\", subdir="/ctl/HPC/", runno.lst, adpx00) {
  OUTPUT = lapply(runno.lst, function(runno) {
    tt = read_runno(MODEL.HOME, subdir=subdir, runno=runno, postfix="")  
    adpx = tt$adpx %>% left_join(tt$xpdb[, c("ROWID", setdiff(colnames(tt$xpdb), colnames(tt$adpx)))], by="ROWID")
    #adpx = adpx %>% mutate(ARMA=gsub("-", " ", ARMA, fix=TRUE), 
    #                       TEST=gsub("-", " ", TEST, fix=TRUE) )
    adpx = adpx %>% left_join(adpx00[, c("ROWID", setdiff(colnames(adpx00), colnames(adpx)))], by="ROWID")
    
    adpx[is.na(adpx)]<-"."
    adpx[adpx==""]<-"."
    
    adpx
  })
  names(OUTPUT) = runno.lst
  return(OUTPUT)
}




get_summary_params <- function(PARAMS, runno.lst) {
  
  tdata= lapply(runno.lst, function(runno) PARAMS[[runno]])
  names(tdata) = runno.lst
  
  out = merge_all(tdata)
  out$model_run_time =  format(out$model_run_time, "%H:%M:%S") #as.character(out$model_run_time)
  
  out$diff_ofv = as_numeric(out$ofv) - as_numeric(out$ofv[1])
  out[, c("ofv", "diff_ofv")] = u.signif(out[, c("ofv", "diff_ofv")], digits=5)
  
  
  
  col.lst = out %>% select(starts_with("TV"), starts_with("RUV", ignore.case=FALSE), EMAX,T50,HILL, matches("_ON_"), starts_with("IIV", ignore.case=FALSE), OMEGA.2.1.) %>% 
    select(-starts_with("se", ignore.case=FALSE))  %>% colnames()
  
  
  
  
  #out = out %>% select(model:ofv, diff_ofv, starts_with("TV"), starts_with("IIV"), starts_with("SIGMA"))shrinkage
  #out = out %>%  select(ofv, diff_ofv, starts_with("TV"), starts_with("IIV"))# %>% select(ofv, diff_ofv, one_of(col.lst)))
  out = out %>%  select(-TVF1, -TVKA,  -SIGMA_1,  -seTVF1,-seTVKA, -seSIGMA_1, -starts_with("se", ignore.case=FALSE), -starts_with("EI", ignore.case=FALSE), -starts_with("shrinkage"))
  out = out %>% select(ofv, diff_ofv, minimization_successful,  covariance_step_successful, model_run_time, condition_number, 
                       starts_with("TV", ignore.case=FALSE), starts_with("RUV", ignore.case=FALSE), EMAX,T50,HILL, matches("_ON_"), starts_with("IIV", ignore.case=FALSE), OMEGA.2.1.)
  
  out = t(out)
  out[is.na(out)]= "---"
  
  summary_all_base_models = out %>% as.data.frame(stringsAsFactors=FALSE)
  # summary_all_base_models["TVCL", "LN014"]
  summary_all_base_models
}





get_summary_params_kable <- function(PARAMS, runno.lst) {
  
  params.name = c("TVCL",       "TVV2" ,      "TVQ",        "TVV3",       "TVVMAX",     "TVKSS" ,     "RUVCV" ,     "RUVSD" ,     "EMAX",       "T50" ,       "HILL"  ,     
                  "WGT_ON_CLQ",  "WGT_ON_VSS", "ALB_ON_CLQ", "IGG_ON_CLQ", "BMI_ON_VSS", "BLK_ON_T50", 
                  "IIV_CLQ",    "IIV_VSS",    "IIV_EMAX" ,  "IIV_T50",    "IIV_VMAX" ,  "IIV_KSS" ,   "OMEGA.2.1.")
  length(params.name)
  params.descp = c("clearance", "central volume \n of distribution", "inter-compartmental \n clearance", "peripheral volume \n of distribution", "maximum rate in \n nonlinear elimination", "Michaelis-Menten \n constant", "proportional error", "additive error", "maximum effect \n in sigmoid model", "half-life to achieve \n half of the maximum effect", "hill exponent \n in Sigmoid model", 
                   "Weight on CL/Q", "Weight on Vss", "Albumin on CL/Q", "IGG on CL/Q", "BMI on Vss", "Black on T50", 
                   "IIV on CL/Q", "IIV of Vss", "IIV of Emax", "IIV on T50", "", "", "IIV between \n CLQ and VSS")
  length(params.descp)
  
  params.unit = c("L/day",       "L" ,      "L/day",        "L",       "L/day",     "mg/L" ,     "" ,     "mg/L" ,     "",       "day" ,       ""  ,     "",  "", "", 
                  "",    "" ,  "",    "" ,  "" ,   "", "" ,  "",    "" ,  "")
  length(params.unit)
  
  meta.param = data.frame(params.name, params.descp, params.unit)
  rownames(meta.param) = params.name
  
  
  tdata= lapply(runno.lst, function(runno) PARAMS[[runno]])
  names(tdata) = runno.lst
  
  out = merge_all(tdata)
  out$model_run_time =  format(out$model_run_time, "%H:%M:%S") #as.character(out$model_run_time)
  
  out$diff_ofv = as_numeric(out$ofv) - as_numeric(out$ofv[1])
  out[, c("ofv", "diff_ofv")] = u.signif(out[, c("ofv", "diff_ofv")], digits=5)
  
  
  #nazeroCols <- function(df) { 
  #  colnames(df)[unlist(lapply(df, function(x) any(is.na(x)|is.nan(x)|x=="NA"|x=="NAN"|x=="NaN"|x==0)))] }
  #
  #out = out %>% select(-one_of(nazeroCols(out)))
  
  
  col.lst = out %>% select(starts_with("TV"), starts_with("RUV", ignore.case=FALSE), EMAX,T50,HILL, matches("_ON_"), starts_with("IIV", ignore.case=FALSE), OMEGA.2.1.) %>% 
    select(-starts_with("se", ignore.case=FALSE))  %>% colnames()
  
  
  out0 = out %>% select(one_of(col.lst))  
  out1 = out0 %>%  sapply(., as.numeric) #%>% round(., digits=3)
  out2 = out %>% select(one_of(paste0("se", col.lst)))  %>%  sapply(., as.numeric)
  out3 =  abs(out2/out1*100 ) ######????  abs() 01/05/2018
  
  
  txt=gsub("  NA(  NA)", "---", paste0(u.signif(out1, digits=3), "(",   u.signif(out3,digits=3), "%)"), fix=TRUE)
  txt=gsub("(  NA)", "(---)", txt, fix=TRUE)
  
  out3 = matrix(txt, nrow=nrow(out0), dimnames=list(rownames(out0), colnames(out0)))
  
  out3 = out3 %>% as.data.frame()   %>%  select(-TVF1, -TVKA )  %>% t()
  OUT=  cbind(params=rownames(out3), out3)
  rownames(OUT) = 1:nrow(OUT)
  OUT = OUT %>% as.data.frame()
  
  OUT =  cbind(meta.param[OUT%>%pull(params)%>%as.character(), ],  OUT) %>% as.data.frame()
  OUT[, "params.name"] = as.character(OUT[, "params.name"])
  OUT[, "params"] = as.character(OUT[, "params"])
  OUT[, "params.descp"] = as.character(OUT[, "params.descp"])  
  OUT[, "params.unit"] = as.character(OUT[, "params.unit"])    
  
  OUT[which(is.na(OUT$params.name)), "params.name"] = as.character(OUT[which(is.na(OUT$params.name)), "params"])
  OUT = OUT %>% select(params.name, params.descp, params.unit, one_of( runno.lst))
  
  OUT[is.na(OUT)] = ""
  
  return(OUT)
}  
# 


write_backward_elimination <- function(directory,  base.runno, param.lst, batch.run.name="run1.txt") {
  
  base_ctl = readLines(paste0(directory, "LN", base.runno, ".ctl"),warn=FALSE) 
  
  lapply(1:length(param.lst), function(i) {
    param = param.lst[i]
    base_ctl2 = base_ctl
    print(param)
    
    ids = grep(paste0(";",param), base_ctl2)
    stopifnot(length(ids)==1)
    base_ctl2[ids] = paste0(" 0 FIX     ;", param)    #ALB_ON_VSS 
    
    runno = paste0("LN", base.runno + i)
    writeLines(base_ctl2, paste0(directory, runno, ".ctl")) 
    
  })
  
  
  # submit jobs on cluster 
  writeLines(paste0("./run.sh  ", "LN", seq(base.runno, length(param.lst)+base.runno), "    10"), 
             paste0(directory, batch.run.name))  
  
  
}  



process_backward_elimination <- function(PARAMS, runno.lst, param.lst) {
  # load all modeling result
  # PsN_HOME <- "H:\\FYANG\\R2810_PD1\\MODEL\\ctl\\PsN\\"
  # PARAMS = lapply(runno.lst, function(runno) {
  #   library(readr)
  #   file.name = paste0(PsN_HOME, runno, "/modelfit_dir1/raw_results_", runno, ".csv")
  #   tt = read_csv(file.name)
  #   colnames(tt) = trim(colnames(tt))
  #   tt
  # })
  # names(PARAMS) = runno.lst
  # 
  # 
  # forward-addition-step-1
  #param.lst = preliminary.selection.covariates %>% pull(params.name)
  #param.lst = setdiff(param.lst, c("HGT_ON_EMAX", "CRCL_ON_EMAX", "WGT_ON_CLQ", "WGT_ON_VSS"))
  
  out = get_summary_params(PARAMS, runno.lst)
  tdata1 =  rbind("COV.params"= param.lst, out)  
  tdata1 = cbind(params=rownames(tdata1), tdata1)
  
  
  tt = tdata1 %>% mutate(params=as.character(params)) %>% filter(params %in% c("diff_ofv", "COV.params")) %>% 
    rbind(., colnames(tdata1))   %>% t() %>% as.data.frame() %>% slice(2:n())
  colnames(tt) = c("COV.name", "diff.ofv", "model")
  tt = tt %>% mutate(diff.ofv = as_numeric(diff.ofv)) %>% arrange(-diff.ofv) %>% filter(diff.ofv>(10.83))
  
  tt = tt %>% mutate(COV.name=as.character(COV.name), model=as.character(model))
  return(tt)
  
}





