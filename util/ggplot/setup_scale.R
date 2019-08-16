# mylimit = c(4, 150)
# 
# setup_scale(myscale='7_14', mylimit = c(-4, 150)) 


setup_scale <- function(myscale='7_14', mylimit) {
  library(shiny)
  
  shiny::validate(need(length(mylimit)==2, message="Need a vector with length of 2"), 
           need(mylimit[2]>mylimit[1], message="The first entry must be small than the second entry")
           )
  x=NULL
  if (mylimit[1]>0 & mylimit[2]>0) {
    return(setup_scale0(myscale='7_14', mylimit))
  }else{
    xpos=setup_scale0(myscale=myscale, mylimit=c(0, max(mylimit, na.rm=TRUE)))
    xneg=setup_scale0(myscale=myscale, mylimit=c(0, -min(mylimit, na.rm=TRUE)))
    x$breaks = c(-rev(xneg$breaks), 
                 xpos$breaks[2:length(xpos$breaks)])
    
    x$labels = c( ifelse(rev(xneg$labels)%in%c("","0"), 
                         rev(xneg$labels), 
                         paste0("-", rev(xneg$labels))), 
                  xpos$labels[2:length(xpos$labels)])
    
    return(x)
  }
}

  


setup_scale0 <- function(myscale='7_14', mylimit) {
  xbreaks = waiver()
  xlabels = waiver()
  
  break.by.what = NULL
  label.by.what = NULL
   
  setup.scale <- function(mylimit, break.by.what, label.by.what,  minor.break.by.what) {
    
    insert_minor <- function(major_labs, n_minor) {
      labs <- c( sapply( major_labs, function(x) c(x, rep("", n_minor) ) ) )
      labs #[1:(length(labs)-n_minor)]   
    }
    
    insert.n.minor = label.by.what/break.by.what-1
    
    mylimit = round(mylimit)
    xbreaks = seq(min(mylimit), max(mylimit), by=break.by.what); 
    xminor_breaks=seq(min(mylimit), max(mylimit),by=minor.break.by.what)
    xlabels = insert_minor(seq(min(mylimit), max(mylimit), by=label.by.what), insert.n.minor ) 
    xlabels = xlabels[1:length(xbreaks)]
    return(list(xbreaks, xlabels))
  }
   
  
  
  # specified scale (5-20)   100_200  must be multiple of the first variable
  tt = grep("_", myscale)>0  
  if (length(tt)>0) {
    tt = as.numeric(unlist(strsplit(myscale, "_"))); 
    #    t2 = as.numeric(unlist(strsplit(myscale, "_"))); 
    #    if (!is.na(t1)) {tt = t1
    #    }else{ tt = t2} 
    names(tt) = c("break", "label") 
    # stopifnot(mod(tt["label"], tt["break"])==0) 
    
    stopifnot(tt["label"] %% tt["break"]==0)   # 0712_2019
    
    break.by.what = tt["break"]; 
    label.by.what=tt["label"];  
    minor.break.by.what=1; 
    insert.n.minor=label.by.what/break.by.what-1
  } 
  
  if (length(break.by.what)>0 & length(label.by.what)>0 ){    
    tt=setup.scale(mylimit, break.by.what, label.by.what,  minor.break.by.what)
    xbreaks = tt[[1]]
    xlabels = tt[[2]]      
  }
  
  return(list(breaks=xbreaks, labels=xlabels))
}

 
  
  
  
  