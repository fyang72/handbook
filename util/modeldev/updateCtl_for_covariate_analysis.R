

updateCtl_for_covariate_analysis <- function(
  base_runno, 
  pair_lst, 
  istep="forward_addition",    # backward_elimination
  local_ctl_dir="./ctl/"
  ) {
  
  # purpose: create a number of submodel based on the specified pair_lst for covariate analysis
  #   base_runno: a string for a model, for example "LN001"
  #   pair_lst: a string vector, such as c(ALBBL_ON_VSS, WGTBL_ON_CLQ)
  #   local_ctl_dir: a directory hold all ctl models
  
  # read base model ----
  ctl_file_name <- paste0(local_ctl_dir, base_runno, ".ctl")
  assert(file.exists(ctl_file_name), 
         msg=paste0("No such a ctl file (", ctl_file_name, ")", 
                    " found in the directory of ", local_ctl_dir))
  
  assert(istep %in% c("forward_addition", "backward_elimination"),
         msg=paste0("Must be one of the option: 'forward_addition' or 'backward_elimination'")
         )
   
  base_ctl = readLines(ctl_file_name, warn=FALSE) 
  start <- which(str_detect(base_ctl, "^\\$THETA\\s+"))
  end <- which(str_detect(base_ctl, "^\\$OMEGA\\s+"))
  ids0 <- intersect(start:end, which(!str_detect(base_ctl, "^\\s*;")))
   
  # loop for all pairs
  runno_lst <- NULL
  for (i in 1:length(pair_lst)) {
    #print(pair_lst[i] )
    new_ctl <- base_ctl
    
    # update base model based on pair_lst[i]
    ids = intersect(ids0, grep(paste0(";(\\s+|)", pair_lst[i]), new_ctl))
    assert(length(ids)==1, 
           msg=paste0("Cov-param not found or found multiple cov-param in your ctl model file (", base_runno, ")"))
    
    if (istep=="forward_addition") {
      new_ctl[ids] = paste0(" 0.01      ;", pair_lst[i])    #ALB_ON_VSS 
    }else if (istep=="backward_elimination") {
      new_ctl[ids] = paste0(" 0   FIX   ;", pair_lst[i])    #ALB_ON_VSS 
    }

    # new model name = base model + 1
    runno = base_runno %>% extractExpr(regexpr='(\\s+|)[0-9]+')  
    prefix_runno = base_runno %>% removeExpr(regexpr='(\\s+|)[0-9]+') 
    new_runno <- paste0(prefix_runno, 
                        add_prefix(as.numeric(runno) + i, digits=nchar(runno))
    )
    
    # write new model to the same diredtory
    writeLines(new_ctl, paste0(local_ctl_dir, new_runno, ".ctl")) 
    runno_lst <- c(runno_lst, new_runno)
  }
  
  return(runno_lst)
}  



if (1==2) { 
  base_runno = "LN100"
  pair_lst = c("WGTBL_ON_CLQ", "AGE_ON_CLQ",  "BMIBL_ON_CLQ")
  local_ctl_dir = "./PopPKPD/ctl/"
  updateCtl_for_covariate_analysis(base_runno, pair_lst, local_ctl_dir)
}






process_forward_addition <- function(covS, runno.lst, cov.lst) {
  # load all modeling result
  # PsN_HOME <- "H:\\FYANG\\R2810_PD1\\MODEL\\ctl\\PsN\\"
  # covS = lapply(runno.lst, function(runno) {
  #   library(readr)
  #   file.name = paste0(PsN_HOME, runno, "/modelfit_dir1/raw_results_", runno, ".csv")
  #   tt = read_csv(file.name)
  #   colnames(tt) = trim(colnames(tt))
  #   tt
  # })
  # names(covS) = runno.lst
  # 
  
  # forward-addition-step-1
  #cov.lst = preliminary.selection.covariates %>% pull(covs.name)
  #cov.lst = setdiff(cov.lst, c("HGT_ON_EMAX", "CRCL_ON_EMAX", "WGT_ON_CLQ", "WGT_ON_VSS"))
  
  out = get_summary_covs(covS, runno.lst)
  tdata1 =  rbind("COV.covs"= cov.lst, out)  
  tdata1 = cbind(covs=rownames(tdata1), tdata1)
  
  
  tt = tdata1 %>% mutate(covs=as.character(covs)) %>% filter(covs %in% c("diff_ofv", "COV.covs")) %>% 
    rbind(., colnames(tdata1))   %>% t() %>% as.data.frame() %>% slice(2:n())
  colnames(tt) = c("COV.name", "diff.ofv", "model")
  tt = tt %>% mutate(diff.ofv = as_numeric(diff.ofv)) %>% arrange(diff.ofv) %>% filter(diff.ofv<(-6.7))
  
  tt = tt %>% mutate(COV.name=as.character(COV.name), model=as.character(model))
  return(tt)
  
}