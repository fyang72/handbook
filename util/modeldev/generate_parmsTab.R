
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

