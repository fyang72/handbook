
#-------------------------------
# check the running status
#-------------------------------
batch_check_run_status <- function(server_IP_address, program_name, runno){
  
  runno_df = cbind(runno, str_split_fixed(runno, pattern="_", n=2)) %>% as.data.frame()
  colnames(runno_df) <- c("runno", "ctl", "dat") 
  runno_df = runno_df %>% mutate(runno=as.character(runno))
  
  for (i in 1:nrow(runno_df)) {
    local_model_name = paste0(HOME, "/ctl/", runno_df[i, "ctl"], ".ctl")
    local_data_name = paste0(HOME, "/data/", runno_df[i, "dat"], ".csv") # "./KRM/data/nmdat_0321_2019.csv"   #_0226_2019.csv"    # "./data/nmdat_0226_2019.csv" 
    
    runno = runno_df[i, "runno"]
    
    main_directory <- paste0("/home/", tolower(Sys.info()["user"]), "/", program_name, "/")
    server_model_dir = paste0(main_directory, "ctl/", runno, "/")
    
    file_full_path_name = paste0(server_model_dir,  "output.log  ")
    
    cat(runno, sep="\n")
    cat(paste0(check_run_status(server_IP_address, file_full_path_name), collapse="\n"), sep="\n\n") 
    cat("\n")
  }
  
}





check_run_status <- function(server_IP_address, 
                             file_full_path_name
) {
  
  owd <- setwd(tempdir())
  on.exit(setwd(owd))  
  
  system(command = paste0("scp ", server_IP_address, ":", file_full_path_name, "./"))
  readLines("./output.log")
  
  
}