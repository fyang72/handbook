

#runno = c("MM0025_nmdat_0226_2019", "MM0026_nmdat_0321_2019")

batch_read_runSummary_table <- function(server_IP_address, program_name, runno, 
                                         local_home = "./KRM/output/") {

  library(readr)

  runno_df = cbind(runno, str_split_fixed(runno, pattern="_", n=2)) %>% as.data.frame()
  colnames(runno_df) <- c("runno", "ctl", "dat")
  runno_df = runno_df %>% mutate(runno=as.character(runno))
 
  PARAMS = NULL   # lapply(1:nrow(runno_df), function(i) {
  for (i in 1:nrow(runno_df)) { 
    irunno =   as.character(runno_df[i, "runno"])
    #local_model_name = as.character(runno_df[i, "ctl"])
          
    folder.loc <- paste0(local_home, "ctl/", irunno)
    file.lst <-list.files(path = folder.loc, all.files = FALSE,full.names = TRUE, 
                         include.dirs = TRUE, recursive =TRUE)     
    file.lst <- file.lst[which(substr(basename(file.lst), 1, 3)=="fit")]
           
    if (length(file.lst)>0) {
      for (j in 1:length(file.lst)) { 
        print(paste0("read ", irunno))
        
        file.name = file.lst[j]
        base.name = tools::file_path_sans_ext(basename(file.name))
        PARAMS[[paste0(irunno, "_", base.name)]] <- read_csv(file.name, 
                                   col_names=TRUE,  
                                   col_type=cols(.default=col_character())
                              ) %>% as.data.frame()
     }}
  }
  return(PARAMS)
}

     
 
 
#-----------------------------------------------
# generate_parmsTab
#----------------------------------------------- 
generate_runSummary_table <- function(PARAMS) {
  
  # how to subset a list
  #runno.lst <- c("LN001", "LN002")
  #tdata= lapply(runno, function(irunno) PARAMS[[irunno]])
  #names(tdata) = runno 
   
  out = merge_all(PARAMS)
  library(lubridate)
  
  out$model_run_time =  lubridate::hms(as.character(out$model_run_time))   #as.character(out$model_run_time)
 
  
  #out = out %>% select(model:ofv, starts_with("TV"), starts_with("RUV"), starts_with("WGT_ON"), starts_with("IIV"), starts_with("SIGMA"), starts_with("se"))
  out$ofv = as_numeric(out$ofv)
  out$diff_ofv = as_numeric(out$ofv) - as_numeric(out$ofv[1])
  
  #col.lst = out %>% select(TVCL:seSIGMA_1) %>% colnames()
  #out[, col.lst] = u.signif(out[, col.lst], digits=3)
  
  #out[which(out=="  NA")] = "---"
  out[, c("ofv", "diff_ofv")] = u.signif(out[, c("ofv", "diff_ofv")], digits=5)
  #col.lst = c("ofv", "diff_ofv", col.lst, "condition_number")
  #out = out[, col.lst]
  
  #out = out %>% select(model:ofv, diff_ofv, starts_with("TV"), starts_with("IIV"), starts_with("SIGMA"))shrinkage
  #out = out %>%  select(ofv, diff_ofv, starts_with("TV"), starts_with("IIV"))# %>% select(ofv, diff_ofv, one_of(col.lst)))
  
  out = out %>%  select(-starts_with("se"), -starts_with("EI"), -starts_with("shrinkage")) 
  
  # out = out %>% select(ofv, diff_ofv, minimization_successful,  covariance_step_successful, 
  #                      est_methods, model_run_time, condition_number, 
  #                      starts_with("TV"), starts_with("RUV"), EMAX,T50,HILL, starts_with("WGT_ON"), starts_with("IIV") )
  
  out = t(out)
  out = cbind(parms = rownames(out), out)
  out[is.na(out)]= "---"
 
   out
  
  return(out)
   
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

