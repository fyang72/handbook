
sim_timept <- function(start=0, end=112, delta=1, dtime=seq(0,112, by=7)) {
  
  #time.interval = 1  # days           group.dosing.info
  #t.infty <- 133   # in days, how long a simulation should run, used to calculate AUCinfty
  #t.last <- 133    # in days, used to calculate AUClast in "numerical.solution" 
  
  infhr.lst = c(0.5, 1, 2)
  
  # a set of special time events after each dosing event
  start.time <- unique(sort(c(infhr.lst/24,   # 0.5, 1, 2 hours after infusion, start.time2)))    #  in days
                              seq(1, 7, by=1))))  # day  
  
  #dosing.time <- seq(0, t.infty, by=7)  #group.dosing.info[[1]]$subject.dosing.info$dtime   #    #  in days
  special.time <- sort(apply(expand.grid(start.time, dtime),1,sum))
  regular.time <- seq(start, end, by=delta)
  
  #timepoints <- sort(unique(c(start.time,special.time, seq(1e-2, t.infty, by=time.interval))))
  timepoints <- sort(unique(c(special.time, regular.time   )))
  
  return(timepoints)
}
