
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

