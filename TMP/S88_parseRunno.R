



read_PARAMS <- function(MODEL_HOME, runno.lst) {
  # load all modeling result
  PARAMS = lapply(runno.lst, function(runno) {
    library(readr)
    
    tt <- tryCatch(
      {
        #message("This is the 'try' part")
        file.name = paste0(MODEL_HOME, runno, "/raw_results_", runno, ".csv")
        read_csv(file.name)
        #read.lst(paste(MODEL.HOME, "ctl/BST2/", runno, ".NM7\\", runno, ".lst", sep="")  )
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
        message(paste0("Successfully read...", runno))
      }
    )     
    
    colnames(tt) = trim(colnames(tt))
    tt
  })
  
  names(PARAMS) = runno.lst
  return(PARAMS)
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



