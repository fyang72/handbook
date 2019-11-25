




run_covariate_analysis <- function(
  base_runno = "LN0100",   # master model file
  data_name = "DAT1002",
  pair_lst = c("WGTBL_ON_CLQ", "AGE_ON_CLQ",  "BMIBL_ON_CLQ"),
  
  server_IP_address = "10.244.64.97",
  program_name="TEST", 
  
  server_home = "/home/",   # /data/feng.yang/     /home/feng.yang/
  server_ctl_dir = paste0(server_home, tolower(Sys.info()["user"]), "/", program_name, "/ctl/"),
  server_data_dir = paste0(server_home, tolower(Sys.info()["user"]), "/", program_name, "/data/"), 
  
  local_home = "./",
  local_ctl_dir = paste0(local_home, "/ctl/"),    # local directory that holds all ctl files
  local_data_dir = paste0(local_home, "/data/"),   # local directory that holds all data files
  local_output_dir = paste0(local_home, "/output/"),  # local directory that holds all nonmem output files from server
    
  execute_option = " -clean=4 -node=1 -sge_prepend_flags='-pe mpich 2' -parafile=/apps/packages/nm741/run/mpilinux8.pnm -no-handle_crashes", 
  
  ofv_threshold_for_forward_addition = -6.63, # (α = 0.01, one degree of freedom) 
  ofv_threshold_for_backward_elimination = 10.83, #(α = 0.001, one degree of freedom) when removed from the model
  
  sleep_before_check = 15,   # mins
  sleep_check_freq = 5,  # mins
  max_cycle = 20, 
  max_running_hours = 24,   # 24 hours
  
  want2_updateCtl = TRUE,
  want2_submit_job = TRUE, 
  want2_sleep = TRUE, 
  want2_fetch_job = TRUE, 
  want2_verbose_print = TRUE
){ 
  
  # Purpose: run both "forward_addition",   "backward_elimination"
  #
  # Input:  many inputs
  # Output: a summary table for both steps of "forward_addition",   "backward_elimination"
  #
  
  # how to auto-find the pair_lst from the control file?  
  if (is.null(pair_lst)) {
    pair_lst <- readLines(paste0(local_ctl_dir, base_runno, ".ctl")) %>%
      extract_pair_lst_from_ctlFile(key_word="_ON_")
  }
  
  # initialization ----
  icycle = 1
  clock_start <- Sys.time()
  selected_runno <- base_runno
  
  # for-loop of forward_addition and backward_elimination ----
  OUT = NULL
  for (istep in c("forward_addition",   "backward_elimination")) {
    elapse_hours <- difftime(Sys.time(), clock_start, unit="hours") %>% as.numeric()
    
    # mega-while-loop ----
    while (length(pair_lst)>0  &  icycle < max_cycle  & elapse_hours <= max_running_hours) {
    if(want2_verbose_print) {print(paste0("Cycle-", icycle))}
    
    # step 1:  modify the full_model to generate a number of submodel; ---- 
    if(want2_updateCtl) {   
      runno_lst <- updateCtl_for_covariate_analysis(
          base_runno = base_runno,
          pair_lst = pair_lst, 
          istep = istep, # "forward_addition",    # backward_elimination
          local_ctl_dir = local_ctl_dir
      )
      runno_lst <- c(base_runno, runno_lst)
    }
      
    # step 2: submit the submodel;  ----
    if(want2_submit_job) {
      batch_submit_job_to_HPC(
        server_IP_address = server_IP_address, 
        program_name = program_name, 
        runno_lst = paste0(runno_lst, "_", data_name), 
        
        server_home = server_home,
        server_ctl_dir = server_ctl_dir,
        server_data_dir = server_data_dir,
        
        local_home = local_home,
        local_ctl_dir = local_ctl_dir,  # local directory that holds all ctl files
        local_data_dir = local_data_dir, # local directory that holds all data files
        local_output_dir = local_output_dir,  # local directory that holds all nonmem output files from server
        
        execute_option = execute_option, 
        want2_verbose_print = want2_verbose_print
      )
    }
    
    # step 3:  sleep and check whether finish or not----
    if(want2_sleep) {Sys.sleep(as.numeric(sleep_before_check)*60)}  #until all results are available to download
     
    # small-while-loop ----
    # either all lst files found or elapse_hours>24, then stop the following 'while' loop
    runno_full_path_lst <- paste0(server_ctl_dir, "/", paste0(runno_lst, "_", data_name))
    found <- check_lst_file_exist(server_IP_address, runno_full_path_lst)
    elapse_hours <- difftime(Sys.time(), clock_start, unit="hours") %>% as.numeric()
    while(!all(found) & elapse_hours <= max_running_hours) {
      if(want2_sleep) {
        print(paste0("waiting for another ", sleep_check_freq, " mins"))
        Sys.sleep(as.numeric(sleep_check_freq)*60)   # every 5 min check the status 
      }
      
      elapse_hours <- difftime(Sys.time(), clock_start, unit="hours") %>% as.numeric()
      found <- check_lst_file_exist(server_IP_address, runno_full_path_lst)
    }
    
    # step 4: fetch results, and tabulize it ----
    if (all(found) & want2_fetch_job) {
      # print("local_home")
      # print(local_home)
      # 
      # print("server_home")
      # print(server_home)
      
      batch_fetch_job_from_HPC(
        server_IP_address, 
        program_name, 
        runno_lst = paste0(runno_lst, "_", data_name), 
        
        server_home = server_home,
        local_home = local_home  # /output/ctl
      )
    }
      
    # step 5: extract parameters ----
    #remove those insignificant covariates, the remaining covariates will be the new potential list for further testing, pick up the most significant covaraite and add it into the full_model; 
    
    runSummary_tab <- batch_read_runSummary_table( 
          runno_lst = paste0(runno_lst, "_", data_name), 
          
          local_home,
          local_ctl_dir,    # local directory that holds all ctl files
          local_data_dir,   # local directory that holds all data files
          local_output_dir
        )  %>% 
      generate_runSummary_table()  %>% 
      as.data.frame()
    
    # summary of ofv, to be included in OUT
    tdata <- runSummary_tab %>% filter(parms == "ofv") %>% 
      gather(RUNNO_ID, VALUE, -parms)
    
    tdata2 <- tdata$RUNNO %>% strVec2matrix("_")
    colnames(tdata2) <- c("RUNNO", "DATA", "FIT", "NOTE")
    tdata2 <- tdata2 %>% mutate(NOTE="", STEP=istep)
    
    tdata <- bind_cols(tdata, tdata2) #%>% select(-NOTE)
    tdata <- tdata %>% mutate(
      VALUE=as_numeric(VALUE), 
      DELTA=VALUE-VALUE[1], 
      COV = c("BASE", pair_lst), 
      CYCLE = icycle
    )
    
    if (istep == "forward_addition") {    # backward_elimination
      tdata <- bind_rows(tdata%>% slice(1)%>% 
                           mutate(NOTE = ifelse(row_number()==1, paste0("Same as ", selected_runno), NOTE)), 
                         
                         tdata%>% slice(2:nrow(tdata)) %>% 
                           filter(DELTA <= ofv_threshold_for_forward_addition) %>% 
                           arrange(DELTA) %>% 
                           mutate(NOTE = ifelse(row_number()==1, "Selected", NOTE))
      )
    }else if (istep== "backward_elimination") {
      tdata <- bind_rows(tdata%>% slice(1)%>% 
                           mutate(NOTE = ifelse(row_number()==1, paste0("Same as ", selected_runno), NOTE)), 
                         
                         tdata%>% slice(2:nrow(tdata)) %>% 
                           filter(DELTA > ofv_threshold_for_backward_elimination) %>% 
                           arrange(-DELTA)  %>% 
                           mutate(NOTE = ifelse(row_number()==1, "Selected", NOTE))
      )
    }
    OUT <- bind_rows(OUT, tdata)
    
    # step 6) update the base model and pair_lst   
   
    new_pair_lst <- setdiff(tdata%>% pull(COV),  c("BASE"))
    
    if (length(new_pair_lst)>0) {
      deleted_cov_lst <- setdiff(pair_lst, new_pair_lst)
      
      selected_runno <- tdata %>% slice(2) %>% pull(RUNNO) %>% as.character() 
      selected_cov <- tdata %>% slice(2) %>% pull(COV) %>% as.character() 
      new_pair_lst <- setdiff(new_pair_lst,  selected_cov)
      
      # extract estimated parms_value for the identified covariate
      base_ctl = readLines(paste0(local_ctl_dir, base_runno, ".ctl"), warn=FALSE) 
      if (istep == "forward_addition") {    
        t1 <- runSummary_tab %>% 
          filter(parms == selected_cov) %>% 
          gather(RUNNO_ID, VALUE, -parms)
        t2 <- t1$RUNNO_ID %>% strVec2matrix("_")
        colnames(t2) <- c("RUNNO", "DATA", "FIT", "NOTE")
        parms_value <- bind_cols(t1, t2) %>% filter(RUNNO==selected_runno) %>% pull(VALUE)
        
        # write new model
        ids = grep(paste0(";(\\s+|)", selected_cov), base_ctl)
        base_ctl[ids] = paste0(" ", parms_value, " FIX  ;", selected_cov)    #ALB_ON_VSS 
         
        }else if (istep== "backward_elimination") {
          for (cov_name in deleted_cov_lst) {
            ids = grep(paste0(";(\\s+|)", cov_name), base_ctl)
            base_ctl[ids] = paste0(" 0     FIX  ;", cov_name)    #ALB_ON_VSS 
          }
        }
      
      icycle <- icycle + 1
      prefix_runno = base_runno %>% removeExpr(regexpr='(\\s+|)[0-9]+') 
      new_base_runno <- paste0(prefix_runno, add_prefix(icycle*100, digits=4)) 
      writeLines(base_ctl, paste0(local_ctl_dir, new_base_runno, ".ctl"))   
      
      # assign the new base_runno and pair_lst
      base_runno <- new_base_runno
      pair_lst <- new_pair_lst 
    }  # end of if (length(new_pair_lst)>0)
    
    elapse_hours <- difftime(Sys.time(), clock_start, unit="hours") %>% as.numeric()
    
  }  # end of mega-while-loop
  
    # swtich to backward_elimination
    base_runno <- base_runno
    pair_lst <- OUT %>% filter(NOTE=="Selected") %>% pull(COV) 
  } # end of "forward_addition" and backward_elimination
  
  # report elapse_hours
  elapse_hours <- difftime(Sys.time(), clock_start, unit="hours") %>% as.numeric()
  print(paste0("elapse_hours ", elapse_hours))
  
  # output
  OUT <- OUT %>% select(-parms) %>% 
    select( RUNNO_ID, STEP, CYCLE, RUNNO,  VALUE, DELTA,  COV,  DATA,  FIT, NOTE  )
   
  return(OUT )
}



if (1==2) { 


source("~/YANG/global.R")
setwd("~/TEMPLATE/04_popPK_report/")


FIGURE = NULL
TABLE = NULL
 
server_IP_address = "10.244.64.97"
program_name="TEST" 

server_home = "/home/"   # /data/feng.yang/     /home/feng.yang/
server_ctl_dir = paste0(server_home, tolower(Sys.info()["user"]), "/", program_name, "/ctl/")
server_data_dir = paste0(server_home, tolower(Sys.info()["user"]), "/", program_name, "/data/") 

local_home = "./"
local_ctl_dir = paste0(local_home, "ctl/")    # local directory that holds all ctl files
local_data_dir = paste0(local_home, "data/")   # local directory that holds all data files
local_output_dir = paste0(local_home, "output/")  # local directory that holds all nonmem output files from server

base_runno = "LN0100"   # master model file
#pair_lst = c("WGTBL_ON_CLQ", "AGE_ON_CLQ",  "BMIBL_ON_CLQ")
pair_lst =c("ALT_ON_CLQ", 
            "ALB_ON_CLQ",
            "IGG_ON_CLQ", 
            "ALP_ON_CLQ",
            "BMI_ON_VSS",
            "BMI_ON_EMAX",
            "BLK_ON_EMAX",
            "ASIA_ON_EMAX",
            "OTHER_ON_EMAX",
            "MONO_ON_EMAX",
            "CREAT_ON_EMAX",
            "IGG_ON_EMAX",
            "ALP_ON_EMAX",
            "BSA_ON_T50", 
            "BLK_ON_T50",
            "ASIA_ON_T50",
            "OTHER_ON_T50", 
            "CREAT_ON_T50", 
            "ALT_ON_T50")

data_name = "DAT1002"

execute_option = " -clean=4 -node=1 -sge_prepend_flags='-pe mpich 2' -parafile=/apps/packages/nm741/run/mpilinux8.pnm -no-handle_crashes" 

max_running_hours = 24
max_cycle = 20
sleep_before_check = 15   # mins
sleep_check_freq = 5  # mins

ofv_threshold_for_forward_addition = -6.7 
ofv_threshold_for_backward_elimination = 13 

if (1==2) {
 want2_submit_job = FALSE 
 want2_sleep = FALSE 
 want2_fetch_job = FALSE
 
 want2_updateCtl = TRUE
 want2_verbose_print = TRUE
 
 max_cycle = 2
}

 want2_updateCtl = TRUE
 want2_submit_job = TRUE 
 want2_sleep = TRUE 
 want2_fetch_job = TRUE
 want2_verbose_print = TRUE

clock_start = Sys.time()

OUT <- run_covariate_analysis(
  base_runno = base_runno,   # master model file
  data_name = data_name,
  pair_lst = pair_lst ,
  
  server_IP_address=server_IP_address,
  program_name=program_name, 
  
  server_home=server_home,   # /data/feng.yang/     /home/feng.yang/
  server_ctl_dir=server_ctl_dir,
  server_data_dir=server_data_dir, 
  
  local_home = local_home,
  local_ctl_dir =local_ctl_dir,    # local directory that holds all ctl files
  local_data_dir = local_data_dir,   # local directory that holds all data files
  local_output_dir = local_output_dir,  # local directory that holds all nonmem output files from server
   
  execute_option = execute_option , 
  
  sleep_before_check =sleep_before_check ,   # mins
  sleep_check_freq = sleep_check_freq,  # mins
  max_cycle = max_cycle, 
  max_running_hours = max_running_hours,   # 24 hours
  
  want2_updateCtl = want2_updateCtl, 
  want2_submit_job =want2_submit_job, 
  want2_sleep = want2_sleep, 
  want2_fetch_job =want2_fetch_job
) 
  
OUT 
elapse_hours <- difftime(Sys.time(), clock_start, unit="hours") %>% as.numeric()

print(elapse_hours)

 

}

