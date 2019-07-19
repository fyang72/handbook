

#https://stackoverflow.com/questions/24753969/knitr-run-all-chunks-in-an-rmarkdown-document
runAllChunks <- function(rmd, envir=globalenv()){
  tempR <- tempfile(tmpdir = ".", fileext = ".R")
  on.exit(unlink(tempR))
  knitr::purl(rmd, output=tempR)
  sys.source(tempR, envir=envir)
}



addPlot2docx <- function(mydoc, x, bookmark=NULL, fun=print, pointsize=12, width=6.4, height=4.8) {
  ## S3 method for class 'docx'
  #addPlot(doc, fun, pointsize = getOption("ReporteRs-fontsize"),
  #  vector.graphic = FALSE, width = 6, height = 6,
  #  fontname = getOption("ReporteRs-default-font"),
  #  fontname_serif = "Times New Roman", fontname_sans = "Calibri",
  #  fontname_mono = "Courier New", fontname_symbol = "Symbol",
  #  editable = TRUE, bookmark, par.properties = parProperties(text.align =
  #  "center", padding = 5), bg = "transparent", ...)
  # 
  
  ## S3 method for class 'pptx'
  #addPlot(doc, fun, pointsize = 11, vector.graphic = TRUE,
  #  fontname = getOption("ReporteRs-default-font"),
  #  fontname_serif = "Times New Roman", fontname_sans = "Calibri",
  #  fontname_mono = "Courier New", fontname_symbol = "Symbol",
  #  editable = TRUE, offx, offy, width, height, bg = "transparent", ...)
  #   
  addPlot(mydoc, x=x, fun=print, pointsize=12, bookmark=bookmark, vector.graphic = FALSE, width = width, height = height
          , fontname_serif = "Times New Roman", fontname_sans = "Calibri"  # "Calibri" 
          , par.properties = parProperties(text.align = "center", padding = 5)
  )
}

#y posted by the @latemail in the comments above. You can use regular expressions for the second and subsequent arguments of filter like this:
# https://stackoverflow.com/questions/22850026/filtering-row-which-contains-a-certain-string-using-dplyr
# dplyr::filter(df, !grepl("RTB",TrackingPixel))


cat_boxplot <- function(tdata, xvar, yvar, theTitle=NULL)  {     
  
  #if (is.null(theTitle)) { theTitle = paste0("Impact of ", xvar, " on ", yvar)}
  
  fig = ggplot(data = tdata, aes_string(x = xvar, y = yvar, fill=xvar )) +
    geom_boxplot(position = position_dodge(width=0.9)) + #  outlier.size=outlier.size, outlier.color=outlier.color) +                     #space between boxplots
    xlab("" ) + 
    ggtitle(theTitle ) +                                                      #add title
    #scale_y_continuous(ylab.txt ) +       #label y axis and set limtis to allow space to plot summary stats beneath plots
    # scale_x_discrete("") +                    #label x axis
    #scale_color_manual(name="Population", values=c("black","black"), labels=c("Japanese", "Non-Japanese")) +   
    # scale_fill_manual(name="Population", values=c("cyan","green"), labels=c("Japanese", "Non-Japanese"))  + 
    base_theme(font.size=10, legend_position= "none") + 
    theme(#legend.title=element_text("EXSEQ"), 
      legend.position="none", 
      panel.grid.minor= element_line(colour = "gray95",size=0.5),
      panel.grid.major= element_line(colour = "gray95",size=0.5),
      panel.spacing = unit(0.2, "lines"), 
      panel.border=element_rect(colour="black", fill=NA), 
      strip.background = element_blank()   # element_rect(colour=NA, fill=NA),
    )   
  
  fig = fig + theme(axis.text.x = element_text(angle = 30, hjust = 1))  
  
  fig  
}





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




write_forward_addition <- function(directory, base.runno, param.lst, batch.run.name="run1.txt") {
  base_ctl = readLines(paste0(directory, "LN", base.runno, ".ctl"),warn=FALSE) 
  
  lapply(1:length(param.lst), function(i) {
    param = param.lst[i]
    base_ctl2 = base_ctl
    print(param)
    
    ids = grep(paste0(";",param), base_ctl2)
    stopifnot(length(ids)==1)
    base_ctl2[ids] = paste0(" 0.01      ;", param)    #ALB_ON_VSS 
    
    runno = paste0("LN", base.runno + i)
    writeLines(base_ctl2, paste0(directory, runno, ".ctl")) 
    
  })
  
  
  # submit jobs on cluster 
  writeLines(paste0("./run.sh  ", "LN", seq(base.runno, length(param.lst)+base.runno), "    10"), 
             paste0(directory, batch.run.name))   
  
}  


process_forward_addition <- function(PARAMS, runno.lst, param.lst) {
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
  
  # forward-addition-step-1
  #param.lst = preliminary.selection.covariates %>% pull(params.name)
  #param.lst = setdiff(param.lst, c("HGT_ON_EMAX", "CRCL_ON_EMAX", "WGT_ON_CLQ", "WGT_ON_VSS"))
  
  out = get_summary_params(PARAMS, runno.lst)
  tdata1 =  rbind("COV.params"= param.lst, out)  
  tdata1 = cbind(params=rownames(tdata1), tdata1)
  
  
  tt = tdata1 %>% mutate(params=as.character(params)) %>% filter(params %in% c("diff_ofv", "COV.params")) %>% 
    rbind(., colnames(tdata1))   %>% t() %>% as.data.frame() %>% slice(2:n())
  colnames(tt) = c("COV.name", "diff.ofv", "model")
  tt = tt %>% mutate(diff.ofv = as_numeric(diff.ofv)) %>% arrange(diff.ofv) %>% filter(diff.ofv<(-6.7))
  
  tt = tt %>% mutate(COV.name=as.character(COV.name), model=as.character(model))
  return(tt)
  
}








geomean <- function(x, na.rm = FALSE, trim = 0, ...)
{
  exp(mean(log(x[is.finite(log(x))], ...), na.rm = na.rm, trim = trim, ...))
}

geosd <- function(x, na.rm = FALSE, ...)
{
  exp(sd(log(x[is.finite(log(x))], ...), na.rm = na.rm, ...))
}



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
    
    mutate(Range= paste0("(", u.signif(Min, digits=3), "-", u.signif(Max, digits=3), ")"),
           CV = abs(SD/Mean)*100,     #### ? abs???  01/05/2018
           meanMinusSD = Mean - SD, 
           meanPlusSD  = Mean + SD, 
           meanMinusSE = Mean - SE, 
           meanPlusSE  = Mean + SE,
           Median_Range = paste0(u.signif(Median,digits=3), "", Range),
           Mean_CV=paste(u.signif(Mean,digits=3)," (", u.signif(CV,digits=3), "%)",sep=""),   # paste( expression(Mean %+-% SD~or~N~"(%)")  , sep="")                              
           Mean_SD=paste(u.signif(Mean,digits=3), " (\u00b1", u.signif(SD,digits=3), ")",sep=""),   # paste( expression(Mean %+-% SD~or~N~"(%)")  , sep="")  
           Mean_SE=paste(u.signif(Mean,digits=3)," (\u00b1", u.signif(SE,digits=3), ")",sep="") ,
            
           GEOmean_SD=paste(u.signif(GEOmean,digits=3), " (\u00b1", u.signif(GEOSD,digits=3), ")",sep="") , 
           GEOmean_CV=paste(u.signif(GEOmean,digits=3)," (", u.signif(GEOSD/GEOmean*100,digits=3), "%)",sep="") 
           
           
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
  #                  Mean=u.signif(Mean, digits=digits), 
  #                  Median=u.signif(Median, digits=digits), 
  #                  Range=Range,
  #                  SD=u.signif(SD, digits=digits), 
  #                  SE=u.signif(SE, digits=digits), 
  #                  CV=u.signif(CV, digits=digits), 
  #                  meanMinusSD=u.signif(meanMinusSD, digits=digits), 
  #                  meanPlusSD=u.signif(meanPlusSD, digits=digits), 
  #                  meanMinusSE=u.signif(meanMinusSE, digits=digits), 
  #                  meanPlusSE=u.signif(meanPlusSE, digits=digits), 
  #                  Mean_CV=paste(Mean, "(",u.signif(CV,digits=digits),"%)",sep=""),   # paste( expression(Mean %+-% SD~or~N~"(%)")  , sep="")                   
  #                  Mean_SD=paste(Mean, "(\u00b1",u.signif(SD,digits=digits),")",sep=""),   # paste( expression(Mean %+-% SD~or~N~"(%)")  , sep="")  
  #                  Mean_SE=paste(Mean, "(\u00b1",u.signif(SE,digits=digits),")",sep=""),
  #                                              
  #                  PCT97P5=u.signif(PCT97P5, digits=digits), 
  #                  PCT2P5=u.signif(PCT2P5, digits=digits), 
  #                  PCT95=u.signif(PCT95, digits=digits), 
  #                  PCT5=u.signif(PCT5, digits=digits), 
  #                  PCT90=u.signif(PCT90, digits=digits), 
  #                  PCT10=u.signif(PCT10, digits=digits))                   
  #                }  
  # 
  return(stats)
}



namedVec2df <- function(x) {setNames(data.frame(lapply(x, type.convert), stringsAsFactors=FALSE), names(x))}


############################################################################## 
# calculate half life
############################################################################## 
calc.half.life <- function(THETA) {
  # alpha and beta
  parms=NULL
  parms$KE  = THETA['TVCL']/THETA['TVV2']    # 1/day, 	Elimination rate constant     0.365
  parms$K32 = THETA['TVQ']/THETA['TVV3'];  
  parms$K23 = THETA['TVQ']/THETA['TVV2']; 
  
  alpha.plus.beta  <- parms$KE + parms$K23 + parms$K32
  alpha.multi.beta <- parms$KE * parms$K32
  alpha <- (alpha.plus.beta + sqrt(alpha.plus.beta^2-4*alpha.multi.beta))/2
  beta  <- (alpha.plus.beta - sqrt(alpha.plus.beta^2-4*alpha.multi.beta))/2
  alpha.half.life <- 0.693/alpha
  beta.half.life <- 0.693/beta
  #print(paste("beta.half.life=", beta.half.life, sep=""))
  
  
  
  return(data.frame(alpha.half.life, beta.half.life))
}



# #adpk = read_sas("Y:\\R2810\\POOL_01\\NONMEM\\Derived_Data\\ADS\\pk.sas7bdat")
# load_all_runno <- function(runno.lst, HOME, subdir="ctl/LN/")  {
#   parms =NULL
#   out= NULL
#   for (runno in runno.lst) {
#     print(paste0("runno=", runno))
#     
#     tt = load_runno(paste(HOME, "MODEL/", sep="") , subdir=subdir, runno ) 
#     
#     # parameters
#     parms[[runno]]$ofv = tt$base_lst$ofv
#     parms[[runno]]$thetas = tt$base_lst$thetas
#     nMax <- max(sapply(tt$base_lst$omega, length))
#     parms[[runno]]$omega = t(sapply(tt$base_lst$omega, function(i) i[1:nMax]))  
#     
#     # adpx
#     xpdb=tt$xpdb;  adpx=tt$adpx 
#     xpdb = xpdb %>% filter(MDV==0) 
#     tt = adpx %>% filter(EVID==0) %>% dplyr::select(one_of(c("ROWID", setdiff(colnames(adpx), colnames(xpdb)))))                   
#     tdata = left_join(xpdb, tt, by="ROWID")
#     tdata$ARMA = gsub("-", " ", tdata$ARMA, fix=TRUE)
#     
#     out[[runno]] = tdata
#     
#     
#   }
#   return(list(parms=parms, out=out))
# }
# 
# 
# 
# load_runno  <- function(MODEL_HOME, subdir="ctl/", runno)  {
#   
#   # paste(table.names, runno, tab.suffix, sep="")
#   # paste(cwres.name,runno,cwres.suffix,tab.suffix,sep="") The default CWRES table file name is called: 
#   
#   #If there are simulation files present then Xpose looks for the files to be named: 
#   # paste(table.names, runno, sim.suffix, tab.suffix, sep="")
#   # paste(cwres.name,runno,sim.suffix,cwres.suffix,tab.suffix,sep="")
#   
#   
#   # need both sdtab001 and patab001,  "IPRED", +  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#   library(xpose4) #read in the xopse library 
#   
#   xpdb <- xpose.data("001", tab.suffix = "", directory=paste(MODEL_HOME, subdir, runno, ".nm7/", sep="")) # run number of xpose table files
#   
#   directory=paste(MODEL_HOME, subdir, runno, ".nm7/", sep="")
#   sdtab = read.table(file=paste0(directory, "sdtab001"), skip=1, sep="", header=TRUE)
#   patab = read.table(file=paste0(directory, "patab001"), skip=1, sep="", header=TRUE)
#   
#   xpdb = left_join(sdtab, patab[, c("ID", setdiff(colnames(patab), colnames(sdtab)))] %>% distinct(ID, .keep_all=TRUE), by="ID")
#   
#   
#   xpdb$DV <- exp(as_numeric(xpdb$DV))
#   xpdb$IPRED <- exp(as_numeric(xpdb$IPRED))
#   xpdb$PRED <- exp(as_numeric(xpdb$PRED))
#   
#   
#   #-----------------------------------------------------------------------------
#   # attach adpx information (from nonmem datafile)
#   #----------------------------------------------------------------------------- 
#   
#   base_ctl = readLines(paste(MODEL_HOME, subdir,  runno, ".ctl", sep=""),warn=FALSE) 
#   
#   library("readr")
#   library("dplyr")
#   
#   # need to remove all ; comments
#   # need to add ROWID in the dataset (adpx)
#   
#   tt = unlist(strsplit(base_ctl[which(regexpr('$DATA', base_ctl, fix=TRUE)>0)], " "))     
#   tt = gsub("..", "", tt[which(regexpr('.csv', tt, fix=TRUE)>0)], fix=TRUE)
#   tt = gsub(",", "", tt, fix=TRUE)
#   
#   adpx <- adpx0 <-  read_csv(file = paste(MODEL_HOME, tt, sep=""), skip=0)
#   colnames(adpx) = gsub("=DROP", "", colnames(adpx), fix=TRUE)
#   adpx$ARMA = gsub("-", " ", adpx$ARMA, fix=TRUE)
#   adpx$ARMA = trim(gsub("INFUSION", "", adpx$ARMA, fix=TRUE))
#   
#   #xpdb2: extended version of xpdb 
#   xpdb2 = xpdb #%>% filter(MDV==0) 
#   tt = adpx %>% filter(EVID==0) %>% select(one_of(c("ROWID", setdiff(colnames(adpx), colnames(xpdb)))))  #%>% distinct(USUBJID, .keep_all=TRUE)               
#   xpdb2 = left_join(xpdb2, tt, by="ROWID")
#   
#   #adpx2: extended version of adpx     
#   tt = xpdb %>% select(one_of(c("ID", setdiff(colnames(xpdb), colnames(adpx))))) %>% distinct(ID, .keep_all=TRUE)               
#   adpx2 = left_join(adpx, tt, by="ID")
#   
#   
#   base_lst = read.lst(paste(MODEL_HOME, subdir, runno, ".NM7\\", runno, ".lst", sep="")  )
#   THETA = base_lst$thetas
#   
#   # obtain the names of THETA           
#   tdata = extract_ctl(base_ctl)$THETA  
#   tdata = colsplit(tdata$PARAM, " ", c("THETA", "PARAM"))
#   names(base_lst$thetas) <- tdata$PARAM
#   
#   
#   
#   return(list(xpdb=xpdb, xpdb2=xpdb2, adpx=adpx, adpx2=adpx2, adpx0=adpx0, base_lst=base_lst, base_ctl=base_ctl))
# }
# 



read_runno  <- function(MODEL.HOME, subdir="ctl/", runno="LN001", postfix="")  {
  
  #postfix = ".nm7/"
  # need both sdtab001 and patab001,  "IPRED", +  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  library(xpose4) #read in the xopse library 
  
  #xpdb <- xpose.data("001", tab.suffix="", directory=paste(PsN_HOME, subdir, runno, postfix, "\\", sep="")) # run number of xpose table files
  #xpdb <- xpose.data("001", tab.suffix="", directory="H:\\FYANG\\R2810_PD1\\MODEL\\PsN\\LN001\\")
  
  
  directory=paste0(MODEL.HOME, subdir, runno, postfix, "\\")
  
  sdtab = read.table(file=paste0(directory, "sdtab001"), skip=1, sep="", header=TRUE)
  patab = read.table(file=paste0(directory, "patab001"), skip=1, sep="", header=TRUE)
  
  xpdb = left_join(sdtab, 
                   patab[, c("ID", setdiff(colnames(patab), colnames(sdtab)))] %>% distinct(ID, .keep_all=TRUE),
                   by="ID")
  
  
  xpdb$DV <- exp(as_numeric(xpdb$DV))
  xpdb$IPRED <- exp(as_numeric(xpdb$IPRED))
  xpdb$PRED <- exp(as_numeric(xpdb$PRED))
  
  
  #-----------------------------------------------------------------------------
  # attach adpx information (from nonmem datafile)
  #----------------------------------------------------------------------------- 
  
  base_ctl = readLines(paste0(directory, runno, ".ctl"),warn=FALSE) 
  
  library("readr")
  library("dplyr")
  
  # need to remove all ; comments
  # need to add ROWID in the dataset (adpx)
  
  tt = unlist(strsplit(base_ctl[which(regexpr('$DATA', base_ctl, fix=TRUE)>0)], " "))     
  tt = gsub("..", "", tt[which(regexpr('.csv', tt, fix=TRUE)>0)], fix=TRUE)
  tt = gsub(",", "", tt, fix=TRUE)
  
  # not use read_csv,  it auto convert double to integer
  adpx <- read.csv(file = paste0(MODEL.HOME, tt), skip=0,  stringsAsFactors=FALSE)
  #adpx <- read_csv(file = paste0(MODEL_HOME, tt), skip=0)
  #adpx %>% filter(ID=="1423390") %>% pull(ALBBL)
  
  
  head(adpx)
  
  return(list(xpdb=xpdb, adpx=adpx))
  
}





#-----------------------------------------------
# GOF_PD1
#-----------------------------------------------
GOF_PD1 <- function(runno, tdata) { 
  tdata = tdata %>% mutate(PRED=as_numeric(PRED), 
                           IPRED=as_numeric(IPRED), 
                           DVOR=as_numeric(DVOR)
  )
  
  DVOR_vs_PRED_LN= ggplot(tdata, aes(x=PRED, y=DVOR, col=ARMA)) + geom_point() + 
    geom_abline(show.legend = FALSE,intercept=0,slope=1,lty="dashed") + coord_fixed(ratio = 1) + 
    facet_wrap(~ARMA) + base_theme() + 
    theme(legend.position='none', 
          panel.grid.minor= element_line(colour = "gray95",size=0.5),
          panel.grid.major= element_line(colour = "gray95",size=0.5),
          panel.spacing = unit(0.2, "lines"), 
          panel.border=element_rect(colour="black", fill=NA), 
          strip.background = element_blank()   # element_rect(colour=NA, fill=NA),
    ) 
  
  
  DVOR_vs_PRED_LOG = DVOR_vs_PRED_LN + scale_x_log10() +  scale_y_log10()   # + 
  #coord_fixed(ratio = 1)
  
  
  DVOR_vs_IPRED_LN= ggplot(tdata, aes(x=IPRED, y=DVOR, col=ARMA)) + geom_point() + 
    geom_abline(show.legend = FALSE,intercept=0,slope=1,lty="dashed") + coord_fixed(ratio = 1) + 
    facet_wrap(~ARMA) + base_theme() + 
    theme(legend.position='none', 
          panel.grid.minor= element_line(colour = "gray95",size=0.5),
          panel.grid.major= element_line(colour = "gray95",size=0.5),
          panel.spacing = unit(0.2, "lines"), 
          panel.border=element_rect(colour="black", fill=NA), 
          strip.background = element_blank()   # element_rect(colour=NA, fill=NA),
    ) 
  
  
  DVOR_vs_IPRED_LOG = DVOR_vs_IPRED_LN + scale_x_log10() +  scale_y_log10()   #+ 
  # coord_fixed(ratio = 1)
  
  
  
  #FIGURE_ALL[[paste0(runno, "_CWRES_vs_")]] =
  #  GOF2(tdata0, MDV="MDV", IPRED="IPRED", PRED="PRED", DV="DV")       ## Goodness of PLot
  # fig1 = recordPlot()  
  
  return(list(DVOR_vs_PRED_LN=DVOR_vs_PRED_LN, 
              DVOR_vs_PRED_LOG=DVOR_vs_PRED_LOG, 
              DVOR_vs_IPRED_LN=DVOR_vs_IPRED_LN, 
              DVOR_vs_IPRED_LN=DVOR_vs_IPRED_LN
  )) 
}



#-----------------------------------------------
# generate_parmsTab
#-----------------------------------------------
generate_parmsTab <- function(PARAMS, runno.lst) {
  
  # how to subset a list
  #runno.lst <- c("LN001", "LN002")
  tdata= lapply(runno.lst, function(runno) PARAMS[[runno]])
  names(tdata) = runno.lst
  
  
  
  out = merge_all(tdata)
  out$model_run_time =  format(out$model_run_time, "%H:%M:%S") #as.character(out$model_run_time)
  #out
  #out = out %>% select(model:ofv, starts_with("TV"), starts_with("RUV"), starts_with("WGT_ON"), starts_with("IIV"), starts_with("SIGMA"), starts_with("se"))
  out$diff_ofv = as_numeric(out$ofv) - as_numeric(out$ofv[1])
  
  #col.lst = out %>% select(TVCL:seSIGMA_1) %>% colnames()
  #out[, col.lst] = u.signif(out[, col.lst], digits=3)
  
  #out[which(out=="  NA")] = "---"
  out[, c("ofv", "diff_ofv")] = u.signif(out[, c("ofv", "diff_ofv")], digits=5)
  #col.lst = c("ofv", "diff_ofv", col.lst, "condition_number")
  #out = out[, col.lst]
  
  #out = out %>% select(model:ofv, diff_ofv, starts_with("TV"), starts_with("IIV"), starts_with("SIGMA"))shrinkage
  #out = out %>%  select(ofv, diff_ofv, starts_with("TV"), starts_with("IIV"))# %>% select(ofv, diff_ofv, one_of(col.lst)))
  
  out = out %>%  select(-TVF1, -TVKA,  -SIGMA_1,  -seTVF1,-seTVKA, -seSIGMA_1, -starts_with("se"), -starts_with("EI"), -starts_with("shrinkage")) 
  
  out = out %>% select(ofv, diff_ofv, minimization_successful,  covariance_step_successful, model_run_time, condition_number, 
                       starts_with("TV"), starts_with("RUV"), EMAX,T50,HILL, starts_with("WGT_ON"), starts_with("IIV"), OMEGA.2.1.)
  
  out = t(out)
  out[is.na(out)]= "---"
  out
  
  # 
  # runno.lst = names(parms)
  # # obj
  # obj = sapply(runno.lst, function(runno)  parms[[runno]]$ofv) 
  # 
  # # thetas
  # tmp = sapply(runno.lst, function(runno)  parms[[runno]]$thetas )  
  # parms.lst = sapply(runno.lst, function(runno)  names(parms[[runno]]$thetas )) %>%unlist() %>% unique() 
  # nMax <- max(sapply(tmp, length))
  # thetas = (sapply(tmp, function(i) i[parms.lst])) 
  # rownames(thetas) = parms.lst
  # thetas = round((thetas), digits=3)
  # thetas[is.na(thetas)] = "---"
  # 
  # 
  # # omega 
  # tmp = sapply(runno.lst, function(runno) {
  #   omega=parms[[runno]]$omega
  #   tt = omega[1:2, 1:2][lower.tri(omega[1:2, 1:2],diag=TRUE)]
  #   if (ncol(omega)==3) {tt = c(tt, omega[3:ncol(omega), 3:ncol(omega)])}
  #   if (ncol(omega)>3) {tt = c(tt, diag(omega[3:ncol(omega), 3:ncol(omega)]))}
  #   
  #   
  #   names(tt) = omega.name[1:length(tt)]
  #   return(tt)
  # } )
  # parms.lst = sapply(runno.lst, function(runno)  names(tmp[[runno]]  )) %>%unlist() %>% unique() 
  # nMax <- max(sapply(tmp, length))
  # omega = (sapply(tmp, function(i) i[parms.lst])) 
  # rownames(omega) = parms.lst
  # omega = round(omega, digits=3)
  # omega[is.na(omega)] = "---" 
  # 
  # # finaly output of parameters 
  # thetas = thetas[setdiff(rownames(thetas),c("TVF1", "TVKA" )), ]
  # rbind("OBJ"=round(obj, digits=3), 
  #       "DIFF_OBJ"=round(obj-obj[1], digits=3),      #"---", 
  #       thetas[setdiff(rownames(thetas),c("RUVCV","RUVSD")), ], #"---", 
  #       omega, # "---", 
  #       thetas[c("RUVCV","RUVSD"), ]   
  # )
} 



#-----------------------------------------------
# remove CWRES>6 outliers
#-----------------------------------------------

remove_CWRES6 <- function(xpdb, adpx, CWRES_threshold=6) {
  
  #xpdb %>% filter(ID=="1423061") %>% mutate(ABS_CWRES=abs(CWRES)) %>% 
  #                          select(ID, TIME, DV, IPRED, PRED, CWRES, ABS_CWRES)  
  
  # flag those outliers 
  tdata = adpx %>% left_join(xpdb[, c("ROWID", setdiff(colnames(xpdb), colnames(adpx)))], by="ROWID")
  
  tdata = tdata %>% mutate(HIGHCWRES = ifelse(abs(CWRES) > CWRES_threshold | 
                                                abs(WRES) > CWRES_threshold |
                                                abs(IWRES) > CWRES_threshold, 1, 0))  
  #paste("Outliers: ", nrow(tdata), " out of ", nrow(xpdb %>% filter(MDV==0) ), sep="")
  
  #tdata %>% top_n(20) %>% select(ROWID, ID, TIME, DV, IPRED, PRED, WRES, CWRES, IWRES) %>% arrange(ID, TIME)  %>% kable(digits = 2)
  
  # comment it out in the dataset
  tdata$CFLAG=as.character(tdata$CFLAG)
  tdata$C=as.character(tdata$C)
  tdata[which(tdata$HIGHCWRES==1 & tdata$MDV==0 & tdata$C=="."), "CFLAG"] = "Outliers" 
  tdata[which(tdata$HIGHCWRES==1 & tdata$MDV==0 & tdata$C=="."), "C"] = "C" 
  
  
  #unique(adpx$CFLAG)
  #adpx = adpx %>% mutate(C = ifelse(CFLAG!=".", "C", ""))
  
  # check it
  tt = tdata %>% filter(CFLAG=="Outliers", !is.na(IPRED)) %>% 
    select(C, CFLAG, ROWID, ID, TAD, TIME, DV, DVOR, IPRED, PRED, MDV, EVID, MDV, WRES, CWRES, IWRES)
  print(tt%>% as.data.frame())
  print(paste0(nrow(tt), " samples have removed, since were considered to be outliers"))
  #tt = adpx %>% filter(CFLAG!=".") %>% select( C, CFLAG, ROWID, ID) %>% as.data.frame()  
  #t(t(as.matrix(table(tt$CFLAG))))
  
  # output
  #DATA_INPUT = ifelse(!colnames(adpx) %in% key.col.lst, paste0(colnames(adpx),"=DROP"), colnames(adpx))
  #paste0("$INPUT ", paste0(DATA_INPUT, sep=" ", collapse=" "))
  
  return(tdata[, colnames(adpx)])
}














rename2 <- function(dat, oldnames, newnames) {
  datnames <- colnames(dat)
  datnames[which(datnames %in% oldnames)] <- newnames
  colnames(dat) <- datnames
  dat
}


anyna <- function(df) {
  df = data.frame(df, check.names = FALSE)      
  t.df = data.frame(t(df), check.names = FALSE)
  return(df[nacols(t.df),  c("USUBJID", nacols(df))])  }



merge_all <- function(mydflist) {
  
  # put data.frames into list (dfs named df1, df2, df3, etc)
  #mydflist <- mget(ls(pattern="df\\d+"))
  
  # get all variable names
  allNms <- unique(unlist(lapply(mydflist, names)))
  
  # https://stackoverflow.com/questions/3402371/combine-two-data-frames-by-rows-rbind-when-they-have-different-sets-of-columns
  # put em all together
  out =  do.call(rbind,
                 lapply(mydflist,
                        function(x) {
                          if (is.null(x)) {
                            NULL
                          }else{
                            data.frame(c(x, sapply(setdiff(allNms, names(x)),
                                                   function(y) NA)))
                          }
                        }
                 )
  )  
  
  return(out)
}



check_adpx <- function(adpx)   {
  
  #-------------------------------------------------
  # clean adpx for use in nonmem dataset
  #-------------------------------------------------
  
  adpx = adpx     # create an cleaned adpx
  
  adpx.col.lst = c(  "USUBJID",   "ARMA",     "TIMEPT",    "NTIM",      "TIME",      
                     "EXSEQ",     "EXROUTE",    "EXDOSE",    "EXDOSU",    "AMT",       "RATE",      
                     "EVID",      "DVOR",      "STDUNIT",        "MDV",       "CMT") 
  
  adpx.key.col = c(  "USUBJID",   "ARMA",     "CYCLE", "TIMEPT",    "NTIM",      "TIME",      
                     "EXSEQ",     "EXROUTE",    "EXDOSE",    "EXDOSU",    "AMT",       "RATE",      
                     "EVID",      "DVOR",      "STDUNIT",        "MDV",       "CMT")  
  
  
  if (1==2) {
    # record -wise 
    adpx = adpx %>% filter(ARMA != "Placebo")
    adpx = adpx %>% filter(TIME >=0)
    
    # column-wise
    minimal.col.lst = c("USUBJID", "ARMA","ARMAN","ID", "TIME", "TEST","DVOR", "DV","MDV","CMT","AMT","RATE", "EVID", "EXSEQ","EXROUTN")
    adpx = adpx[, minimal.col.lst]
  }
  
  #-----------------------------------------------------   
  # remove explicit placebo subject  
  #----------------------------------------------------- 
  #adpx$ARMA = toupper(adpx$ARMA)
  
  subj.lst = unique(adpx$USUBJID[which(toupper(adpx$ARMA) %in% c("PLACEBO"))])
  if (length(subj.lst)>0) {print(subj.lst)  }
  
  tdata <- adpx %>% filter(toupper(ARMA)=="PLACEBO")     # adpx[which(!adpx$USUBJID %in% subj.lst),]
  if(nrow(tdata)>0) print(paste(nrow(tdata), " of records has been removed due to explicit placebo subject",sep=""))
  adpx = adpx %>% mutate(CFLAG=ifelse(ROWID %in% tdata$ROWID, "PLACEBO.SUBJECT", CFLAG), 
                         C=ifelse(ROWID %in% tdata$ROWID, "C", C)
  ) 
  #print("remove explicit placebo subject")
  #print(dim(adpx))
  
  
  #-----------------------------------------------------
  # remove implicit placebo subject, we may suspect such subjects may be dosed with placebo
  #-----------------------------------------------------
  adpx$DVOR =  as_numeric(adpx$DVOR) 
  tdata = adpx %>% filter(EVID==0) %>% group_by(USUBJID) %>% summarise(SUSPECT_SUBJ = all(DVOR %in% c(0, NA)))   # 
  which(tdata$SUSPECT_SUBJ)
  
  subj.lst = tdata %>% filter(!SUSPECT_SUBJ) %>% select(USUBJID) %>% .[[1]]
  subj.lst = setdiff(unique(adpx$USUBJID), subj.lst)
  tdata = adpx %>% filter(USUBJID %in% subj.lst )  %>% select(ROWID, USUBJID, DV, TIME, DVOR)
  
  if(nrow(tdata)>0) print(paste(nrow(tdata), " of records has been removed due to implicit placebo subject",sep=""))
  adpx = adpx %>% mutate(CFLAG=ifelse(ROWID %in% tdata$ROWID, "IMPLICIT.PLACEBO.SUBJECT", CFLAG), 
                         C=ifelse(ROWID %in% tdata$ROWID, "C", C)
  ) 
  
  #print("remove implicit placebo subject")
  #print(dim(adpx))
  
  #-----------------------------------------------------
  # check TIME....
  #-----------------------------------------------------
  adpx = adpx
  adpx$TIME =  as_numeric(adpx$TIME)  
  
  # checking TIME in adpx, must not be "NA" or ".", 
  tdata = adpx[which(is.na(adpx$TIME)),  ]
  if(nrow(tdata)>0) print(paste(nrow(tdata), " of records has been removed due to is.na(TIME)",sep=""))
  adpx = adpx %>% mutate(CFLAG=ifelse(ROWID %in% tdata$ROWID, "missing.time", CFLAG), 
                         C=ifelse(ROWID %in% tdata$ROWID, "C", C)
  ) 
  
  
  # checking TIME in adpx, must be >=0
  tdata = adpx[which(adpx$TIME<0 | (adpx$TIME==0&adpx$EVID==0)),  ]
  if(nrow(tdata)>0) print(paste(nrow(tdata), " of records has been removed due to TIME<=0",sep=""))
  adpx = adpx %>% mutate(CFLAG=ifelse(ROWID %in% tdata$ROWID, "Predose.samples", CFLAG), 
                         C=ifelse(ROWID %in% tdata$ROWID, "C", C)
  ) 
  
  # # checking DVOR=0, TIME==0, EVID==0  
  # tdata = adpx[which(adpx$TIME==0 & adpx$DVOR==0 & adpx$TIME==0),  ]
  # if(nrow(tdata)>0) print(paste(nrow(tdata), " of records has been removed due to DVOR=0,TIME=0,EVID=0",sep=""))
  # adpx = adpx %>% mutate(CFLAG=ifelse(ROWID %in% tdata$ROWID, "TIME.LE.ZERO", CFLAG), 
  #                        C=ifelse(ROWID %in% tdata$ROWID, "C", C)
  # ) 
  
  # records must start the first dose, i.e. EVID=1, TIME=0 and EXSEQ=1
  tdata = adpx[which(adpx$TIME==0 & adpx$EVID==1 & adpx$EXSEQ==1),  ]      # R2810-ONC-1423-840-003-011    R2810-ONC-1423-840-008-001  why EXSEQ=0
  #print(tdata)
  
  adpx[which(adpx$USUBJID %in% setdiff(adpx$USUBJID, tdata$USUBJID)), ]   
  
  adpx = adpx
  
  #print("check TIME placebo subject")
  #print(dim(adpx))
  #-----------------------------------------------------
  # for a given ID, TEST, TIME, any duplicated records?
  #-----------------------------------------------------
  
  
  
  #-----------------------------------------------------
  # check unrealistic peak : trough ratios in DVOR
  #-----------------------------------------------------
  if (1==2 ) {
    adpx = adpx
    adpx$TIME = as_numeric(adpx$TIME)
    adpx$DVOR = as_numeric(adpx$DVOR)  
    
    # only concern about PRE and 0HR
    ids =  regexpr('PRE', adpx$TIMEPT) >=0 | regexpr('0HR', adpx$TIMEPT) >=0      
    adpx = adpx[ids, ]
    
    adpx$DVOR = as_numeric(adpx$DVOR)      
    adpx$EVENT = NA            
    adpx$EVENT[which(regexpr('PRE', adpx[, "TIMEPT"]) >=0)] = "PRE"
    adpx$EVENT[which(regexpr('0HR', adpx[, "TIMEPT"]) >=0)] = "POST"
    
    # extract the DVOR for POST and PRE, and calculate the RATIO
    tt = acast(adpx, USUBJID+ARMA+EXSEQ ~ EVENT, fun.aggregate = fun.mean, value.var="DVOR")
    tt = data.frame(colsplit(rownames(tt),"_",c("USUBJID","ARMA","EXSEQ")), tt)
    tt = tt[order(tt$USUBJID, tt$EXSEQ),]
    rownames(tt) = 1:nrow(tt)
    tt$PRE = as_numeric(tt$PRE)
    tt$POST = as_numeric(tt$POST)  
    tt$PRE[which(tt$PRE==0)] = 0.078
    tt$POST[which(tt$POST==0)] = 0.078
    tt$RATIO = as_numeric(tt$POST)/as_numeric(tt$PRE) 
    tt$RATIO[which(tt$RATIO>10)] = 10
    tt = tt[order(tt$RATIO),]
    
    # what is the mean value for POST and PRE
    acast(adpx, ARMA ~ EVENT, fun.aggregate = fun.mean, value.var="DVOR")
    barplot(tt$RATIO[which(!is.na(tt$RATIO))])
    
    # identify the abnormal POST and PRE for each dosing period, USUBJID + EXSEQ
    
    tt2 = tt[which(tt$RATIO<1.5), ]
    tt2 = tt2[order(tt2$USUBJID, tt2$EXSEQ),]  
    tt2
    
    # Finally, exclude those observations (DVOR) at POST and PRE for each abnormal dosing event  
    adpx = adpx
    ids = adpx$EVID==0 & 
      (regexpr('PRE', adpx$TIMEPT) >=0 | regexpr('0HR', adpx$TIMEPT) >=0) &          
      paste(adpx$USUBJID,adpx$EXSEQ,sep="-") %in% paste(tt2$USUBJID,tt2$EXSEQ,sep="-")  
    adpx[ids, adpx.col.lst]
    
    tdata = adpx[!ids, ]
    if(nrow(adpx)!=nrow(tdata)) print(paste(nrow(adpx)-nrow(tdata), " of records has been removed due to abnormal peak:trough ratio",sep=""))
    adpx = tdata
    
    adpx = adpx
  } 
  
  
  
  #-----------------------------------------------------
  # AMT can't be zero
  #-----------------------------------------------------
  
  tdata = adpx %>% mutate(AMT=as_numeric(AMT)) %>% filter(EVID==1, AMT %in% c(0, NA))
  if(nrow(tdata)>0) print(paste(nrow(tdata), " of records has been removed due to AMT.IS.ZERO",sep=""))
  adpx = adpx %>% mutate(CFLAG=ifelse(ROWID %in% tdata$ROWID, "AMT.IS.ZERO", CFLAG), 
                         C=ifelse(ROWID %in% tdata$ROWID, "C", C)
  ) 
  
  # print("AMT can't be zero")
  # print(dim(adpx))
  
  
  #-----------------------------------------------------
  # no dosing event, but having concentration, should removed. 
  #-----------------------------------------------------
  adpx = adpx
  
  tdata = adpx  
  tdata$AMT = as_numeric(tdata$AMT)
  tdata$DVOR = as_numeric(tdata$DVOR)  
  subj.adex = unique(tdata[which(tdata$EVID==1 & tdata$AMT!=0),"USUBJID"] )     # subjects having valid dosing
  subj.adpc = unique(tdata[which(tdata$EVID==0 & !is.na(tdata$DVOR)),"USUBJID"] )   # subject having valid conc
  
  # identify subjects who have adpc information but no dosing information                                               
  subj.lst = setdiff(subj.adpc, subj.adex)
  tdata = filter(adpx, (USUBJID %in% subj.lst)) 
  if(nrow(tdata)>0) print(paste(nrow(tdata), " of records has been removed due to having adpc information but no dosing information ",sep=""))
  adpx = adpx %>% mutate(CFLAG=ifelse(ROWID %in% tdata$ROWID, "NO.DOSING.INFO", CFLAG), 
                         C=ifelse(ROWID %in% tdata$ROWID, "C", C)
  ) 
  
  #print("no dosing event, but having concentration, should removed. ")
  #print(dim(adpx))
  
  
  #-----------------------------------------------------
  # whether or not remove all DV = 0 or BLQ data entries;
  #----------------------------------------------------- 
  adpx = adpx
  
  if (1==2) {
    ids <- (as_numeric(adpx$DVOR) == 0 | is.na(as_numeric(adpx$DVOR)))  &  
      as_numeric(adpx$EVID) == 0 
    #adpx[ids, "DVOR"] = ".";     adpx[ids, "MDV"] = 1
    
    tdata = adpx[!ids, ]      #  adpx[ids, adpx.col.lst]
    if(nrow(adpx)!=nrow(tdata)) print(paste(nrow(adpx)-nrow(tdata), " of records has been removed due to BLQ",sep=""))
    adpx = tdata
    
    adpx = adpx
  }
  
  
  return(adpx)
}



build_nmdat  <- function(adpx) {
  
  adpx = as.data.frame(adpx)
  
  adpx$AMT[which(is.na(adpx$AMT))] = "."
  adpx$DV[which(is.na(adpx$DV))] = "."
  
  adpx[is.na(adpx)] = "."
  adpx[which(adpx$DV=="."), "MDV"] = 1
  
  # remove NA entries
  nacols <- function(df) { 
    colnames(df)[unlist(lapply(df, function(x) any(is.na(x)|is.nan(x)|x=="NA"|x=="NAN"|x=="NaN")))] }
  if (length(nacols(adpx))>0) {
    print(nacols(adpx))
    print(anyna(adpx))
  }
  
  
  # adpx$EVID = as_numeric(adpx$EVID) 
  # adex = adpx[which(adpx$EVID!=0), ]; t.adex = paste(adex$USUBJID, adex$TIME, sep="")
  # adpc = adpx[which(adpx$EVID==0), ]; t.adpc = paste(adpc$USUBJID, adpc$TIME, sep="")
  # adpc = adpc[which(!t.adpc %in% t.adex), ]
  # adpx = rbind(adex, adpc)
  # adpx = adpx %>% arrange(USUBJID, TIME, -EVID) 
  #adpx = adpx[order(adpx$STUDYID, adpx$USUBJID, adpx$TIME, -adpx$EVID), ]  # no ARMA, Since it becomes complicated for multilple doses per subject
  
  
  nmdat = adpx
  colnames(nmdat) <- gsub(".", "_", colnames(nmdat), fixed=TRUE)
  colnames(nmdat) <- gsub(" ", "_", colnames(nmdat), fixed=TRUE)
  
  # remove "," and " " in some columns 
  for (icol in 1:ncol(nmdat)) {nmdat[, icol] = gsub(" ", "-", nmdat[, icol])} 
  for (icol in 1:ncol(nmdat)) {nmdat[, icol] = gsub(",", "-", nmdat[, icol])} 
  
  
  
  
  # add header
  for (i in colnames(nmdat)) {
    nmdat[,i] <- as.character(nmdat[,i]) 
    nmdat[,i] <- gsub(" ", "_", nmdat[,i], fixed=TRUE)
  }
  
  return(nmdat) 
  
  #nmdat <- rbind("C"=colnames(nmdat),  nmdat) 
  
  #nmdat["C1", 1] <- descp  # in order to use PDxPop
  #rownames(nmdat)[2:nrow(nmdat)] <- 1:(nrow(nmdat)-1)
  
  # paste(colnames(nmdat), collapse=" ")
  #nonmem.file.name <- paste(".\\NONMEM\\R1193_HV_1219_simulation_01", ".csv", sep="")
  #write_table(nmdat, path, sep=",", col.names = FALSE, row.names = FALSE, quote = FALSE)
  
  
}


every_nth <- function(x, nth, empty = TRUE, inverse = FALSE) 
{
  
  if (length(x)==0)  {return(NULL)}
  if (!inverse) {
    if(empty) {
      x[1:nth == 1] <- ""
      x
    } else {
      x[1:nth != 1]
    }
  } else {
    if(empty) {
      x[1:nth != 1] <- ""
      x
    } else {
      x[1:nth == 1]
    }
  }
}

tick_label <-function(x, xmin, xmax, major.tick=28,  minor.tick=7, log=FALSE) {
  
  
  if (1==2) {
    print("inside tick_label")
    print(x)
    print(xmin)
    print(xmax)
    print(minor.tick)
    print(major.tick)  
    
    xmin = -80.9
    xmax = 1.9
    minor.tick=5
    major.tick =10
    
    xmin = 0
    xmax = 28
    minor.tick= 7
    major.tick =14
    
  }
  
  #breaks = seq(xmin, xmax, by=minor.tick)
  xmin = floor(xmin/minor.tick )*minor.tick
  xmax = ceiling(xmax/minor.tick)*minor.tick
  
  if (xmin<=0 & xmax<=0) {breaks = seq(xmin, xmax, by=minor.tick)}
  if (xmin>=0 & xmax>=0) {breaks = seq(0, xmax, by=minor.tick)}
  
  if (xmin<=0 & xmax>=0) { 
    brk.neg <- seq(round(xmin/minor.tick, digits=0)*minor.tick, 0, by=minor.tick)
    brk.pos <- seq(0, round(xmax/minor.tick, digits=0)*minor.tick, by=minor.tick)
    breaks <- sort(unique(c(brk.neg, brk.pos)))
  }
  
  #neg = rev(every_nth(rev(breaks[which(breaks<=0)]), major.tick/minor.tick, inverse = TRUE))
  #pos = every_nth(breaks[which(breaks>=0)], major.tick/minor.tick, inverse = TRUE)
  #if (last(neg)=="0" & first(pos)=="0") {labels=c(neg, pos[2:length(pos)])}
  #labels = labels[1:length(breaks)]  
  
  labels = breaks
  labels[which(!(as_numeric(labels) %% major.tick) %in% c(0, NA))] = " "
  labels
  
  #print(data.frame(breaks, labels)     )
  #out = data.frame(breaks, labels)              
  
  if (log) {
    
    breaks = NULL
    x = log10(x + 1E-10)
    x = x[which(!is.na(x))]
    lst = 10^(seq(floor(min(x,na.rm=TRUE)), ceiling(max(x, na.rm=TRUE)), by=1))
    breaks = expand.grid(x=seq(1,9), y=lst)
    breaks$value = breaks$x * breaks$y
    breaks
    breaks = unique(sort(breaks$value))
    labels = every_nth(breaks, 9, inverse = TRUE)
    #out = data.frame(breaks, labels) #cbind(breaks, labels) %>% as.data.frame()
  }
  
  out = data.frame(breaks, labels)
  return(out)
}


assign_Q <- function(df, value="DVOR", quartile=c(0, 0.25, 0.5, 0.75, 1), breaks=NULL, 
                     include.lowest = TRUE, right = TRUE, ordered_result = FALSE) {
  
  df = as.data.frame(df)
  DVOR = df[, value]
  
  # using quartile
  if (!is.null(quartile) & is.null(breaks)) {
    breaks=quantile(DVOR, probs=quartile, type=2, na.rm=TRUE)
    breaks = breaks # [2:(length(breaks)-1)]
    
    breaks = breaks + seq_along(breaks) * 10^(round(log10(  .Machine$double.eps), digits=0))
    
  }
  stopifnot(!is.null(quartile) | !is.null(breaks))   # one of them must be not NULL
  if(is.null(quartile) & is.null(breaks))   {return(df)}
  
  
  #breaks = sort(unique(c(-Inf,breaks, Inf) ))  # commented out from 0922_2017
  CUT = cut(DVOR, breaks, include.lowest = include.lowest, right = right, ordered_result = ordered_result) 
  
  df$Q_LABEL = gsub(",", ", ", CUT, fix=TRUE)    # added 02/11/2018
  
  INT = as.integer(as.factor(CUT))
  MAX_INT = max(INT, na.rm=TRUE)  #######################
  if (MAX_INT<10) {df$Quantile = paste0("Q", INT)}
  if (MAX_INT>=10) {df$Quantile = paste0("Q", add_prefix(INT, digits=2))}
  if (MAX_INT>=100) {df$Quantile = paste0("Q", add_prefix(INT, digits=3))}
  df
}



base_hist <- function(adpx, xval="WEIGHTBL", group="SEX", binwidth=10, title="Histogram plot of Weight", xlab.txt="Weight(kg)") {            
  lazy_plot =  lazyeval::interp(~ggplot(adpx, aes(x=WEIGHTBL, color=GROUP, fill=GROUP)) +
                                  geom_histogram(#aes(y=..density..), 
                                    binwidth=binwidth, 
                                    colour="black", 
                                    #position="dodge", #   identity  dodge 
                                    alpha=0.5,  
                                    #fill="white", 
                                    linetype="solid"   # dashed
                                  ) + 
                                  
                                  geom_density(alpha=0.5) +     # , fill="#FF6666"
                                  
                                  #scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9")) +     # to use custom colors
                                  #scale_color_grey() + 
                                  #scale_color_brewer(palette="Paired") +    # Continuous colors 
                                  scale_color_brewer(palette="Dark2") +    # Discrete colors
                                  #scale_color_brewer(palette="Accent") +    # Gradient colors
                                  
                                  #scale_fill_manual(breaks=c("F", "M"), values=c("blue","green","red","orange"))  + 
                                  #scale_fill_brewer() +     # to use color palettes from RColorBrewer package
                                  #scale_fill_grey() +   #  to use grey color palettes
                                  scale_fill_brewer(palette="Dark2") +    # Discrete colors
                                  
                                  labs(title=title,x=xlab.txt, y = "Frequency")+     # Density
                                  
                                  theme_classic() + regnR::base_theme(font.size=14, legend_position="bottom") +  
                                  #facet_wrap(~SEX, scales = "free") + 
                                  #facet_grid( .~SEX)  + 
                                  
                                  geom_vline(data =adpx %>%  group_by(GROUP) %>% dplyr::summarize(meanWT = median(WEIGHTBL, na.rm=TRUE)), 
                                             aes(xintercept = meanWT, color=GROUP), size = 0.8,  linetype="dashed", show.legend=FALSE),  # color = "red",
                                #  geom_bar() + 
                                #  geom_bar(colour="black", show_guide=FALSE)
                                
                                WEIGHTBL = as.name(xval),
                                GROUP = as.name(group))
  return(lazyeval::lazy_eval(lazy_plot))
  
  
  
  
  
  ggplot(adpx, aes(x=WEIGHTBL, color=GROUP, fill=GROUP)) +
    geom_histogram( #aes(y=..density..), 
      binwidth=binwidth, 
      colour="black", 
      #position="dodge", #   identity  dodge 
      alpha=0.5,  
      #fill="white", 
      linetype="solid"   # dashed
    )
  
  
  
  
  
}





