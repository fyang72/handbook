

# 
# system(command = "scp   ./ctl/LN_BASE_WT/LN_BASE_WT.ctl    10.244.106.127:/home/feng.yang/R3918/ctl/LN_BASE_WT/", intern = T)
# system(command = "scp   ./data/nmdat_PK_1024_2018_test.csv    10.244.106.127:/home/feng.yang/R3918/data/", intern = T)
# 
# txt = system(command = "ssh 10.244.106.127 'cd /home/feng.yang/R3918/ctl/LN_BASE_WT/; execute  LN_BASE_WT.ctl  -clean=4'", intern = T)
# system(command = "scp   10.244.106.127:/home/feng.yang/R3918/ctl/LN_BASE_WT/LN_BASE_WT.lst   ./ctl/LN_BASE_WT/")
# system(command = "scp   10.244.106.127:/home/feng.yang/R3918/ctl/LN_BASE_WT/LN_BASE_WT.ext   ./ctl/LN_BASE_WT/")
# 

run.model.in.HPC <- function(
         local.model.name = "./ctl/LN_BASE_WT/LN_BASE_WT.ctl", 
         local.data.name = "./data/nmdat_PK_1024_2018_test.csv", 
         local.dir = NULL,   #"./ctl/LN_BASE_WT/", 
         server.model.dir = "/home/feng.yang/R3918/ctl/LN_BASE_WT/", 
         server.data.dir= "/home/feng.yang/R3918/data/"
)  {
  
  # derive model and data information
  model.name = basename(local.model.name)  # "/home/feng.yang/R3918/ctl/LN_BASE_WT/LN_BASE_WT.ctl"
  model.runno = gsub(paste0(".", tools::file_ext(model.name)), "", model.name,  fix=TRUE)
  data.name = basename(local.data.name)  # "/home/feng.yang/R3918/ctl/LN_BASE_WT/LN_BASE_WT.ctl"
   
  # upload model and dataset to server
  system(command = paste0("scp  ", local.model.name, "  ",  "10.244.106.127:", server.model.dir), intern = T)
  system(command = paste0("scp  ", local.data.name,  "  ",  "10.244.106.127:", server.data.dir), intern = T)
  
  # run the commands
  command1 = paste0('cd ',  server.model.dir)            #'cd /home/feng.yang/R3918/ctl/LN_BASE_WT/;'
  command2 = paste0('execute ',  model.name, ' -clean=4 &') 
  command = paste(command1, command2, sep="; ")
  system(command = paste0("ssh ", "10.244.106.127 '", command, "'"), intern = T)
   
  return(lst)
}

fetch.result.from.HPC <- function(
  local.model.name = "./ctl/LN_BASE_WT/LN_BASE_WT.ctl", 
  local.data.name = "./data/nmdat_PK_1024_2018_test.csv", 
  local.dir = NULL,   #"./ctl/LN_BASE_WT/", 
  server.model.dir = "/home/feng.yang/R3918/ctl/LN_BASE_WT/", 
  server.data.dir= "/home/feng.yang/R3918/data/"
)  {
  # derive model and data information
  model.name = basename(local.model.name)  # "/home/feng.yang/R3918/ctl/LN_BASE_WT/LN_BASE_WT.ctl"
  model.runno = gsub(paste0(".", tools::file_ext(model.name)), "", model.name,  fix=TRUE)
  data.name = basename(local.data.name)  # "/home/feng.yang/R3918/ctl/LN_BASE_WT/LN_BASE_WT.ctl"
  
  
  # fetch the results
  system(command = paste0("scp 10.244.106.127:", paste0(server.model.dir, model.runno, ".lst   ", local.dir)))
  system(command = paste0("scp 10.244.106.127:", paste0(server.model.dir, model.runno, ".ext   ", local.dir)))
  
  
  # read the results  
  # -------------------------------
  if (is.null(local.dir)) {local.dir = dirname(local.model.name)}
  lst = read.lst(paste0(local.dir, model.runno, ".lst"))
  return(lst)
}
 
 
  local.model.name = "./ctl/LN_BASE_WT/MM_PKPD_CV_V2.ctl"  
  local.data.name = "./data/nmdat_PK_1024_2018.csv" 
  #local.dir = "./ctl/LN_BASE_WT/", 
  server.model.dir = "/home/feng.yang/R3918/ctl/LN_BASE_WT/"  
  server.data.dir= "/home/feng.yang/R3918/data/"
 
  # submit the jobs
  run.model.in.HPC(local.model.name = local.model.name, 
                   local.data.name = local.data.name, 
                   local.dir=NULL, 
                   server.model.dir = server.model.dir,
                   server.data.dir = server.data.dir)
  
  # fetch the results
  lst= fetch.result.from.HPC(local.model.name = local.model.name, 
                             local.data.name = local.data.name, 
                             local.dir=NULL, 
                             server.model.dir = server.model.dir,
                             server.data.dir = server.data.dir)

  lst 
  
 
  local.model.name = "./ctl/MM_PK/MM_PK_CV.ctl"
  model.name = basename(local.model.name)  # "/home/feng.yang/R3918/ctl/LN_BASE_WT/LN_BASE_WT.ctl"
  model.runno = gsub(paste0(".", tools::file_ext(model.name)), "", model.name,  fix=TRUE)
  
  local.dir = paste0(dirname(local.model.name), "/")
  lst = read.lst(paste0(local.dir, model.runno, ".lst"))
  lst


  u.signif(lst$sethetas/lst$thetas * 100, digits=3)
  
  
  omega = lst$omega %>% unlist()
  seomega = lst$seomegas %>% unlist()
  u.signif(seomega/omega * 100, digits=3)
  
  
  # how to calculate CV of Pi
  # The coefficient of variation of P is only approximately SQRT(OMEGA) when you define
  # Pi = TVP * EXP(ETAi). The exact value is SQRT[exp(OMEGA) - 1]. It is helpful to
  # recognize that exp(x) is approximately 1+x (when x is small) which is why CV is
  # approximately SQRT(OMEGA). You also need to multiply by 100 to get CV%. Please also
  # note that ETA is a random variable while OMEGA is the variance of ETA.
  # 
  #https://www.cognigencorp.com/nonmem/nm/99sep202004.html
  
   CV = sqrt(lst$omega %>% unlist() )
    u.signif(CV[CV!=0]*100, digits=3)
  
  