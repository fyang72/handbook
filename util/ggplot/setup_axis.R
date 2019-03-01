

#' Creates a Concentration vs Time graph
#'
#' Creates a Concentration vs Time graph using Regeneron data.
#'
#' @param data A data frame with which to create the plots.  The data frame must conform to Regeneron standards and contain the variables: `week`, `ndy`, `concn`, `concni`, `pchg`, `actarmcd`, `ada`
#' @param drug Drug name
#' @param log Should the log axis be used?
#' @param adjust Should an adjustment be applied to the errorbars?
#' @return A ggplot graphic.
#' @import gridExtra
#' @export
#' @examples
#'   inFile <- system.file("extdata/pk_pd_ada_all3.sas7bdat", package = "pkGraph")
#'   library(haven)
#'   theData <- read_sas(inFile)
#'   theData <- dataCheck(theData)
#'   concTime(data = theData)
#'   concTime(data = theData, adjust = TRUE)
setup_axis <-function(xscale='log', xlimits) {
  xbreaks = waiver()
  xlabels = waiver()
  
  break.by.what = NULL
  label.by.what = NULL
  
  # log-scale
  if (xscale=="log") {
    
    t1 = min(floor(log10(xlimits)),na.rm=TRUE)  
    t2 = max(ceiling(log10(xlimits)),na.rm=TRUE) 
    ylog10range = seq(t1, t2, by=1)
    
    library(scales)
    
    xbreaks = NULL;
    xlabels = NULL
    for (i in 1:(length(ylog10range)-1)) {
      xbreaks = c(xbreaks, seq(10^ylog10range[i], 10^ylog10range[i+1], by= 10^ylog10range[i]) )
      xlabels = c(xlabels, c(10^ylog10range[i], rep("", time=8), 10^ylog10range[i+1]))
    }
  } 
  
  
  setup.scale <- function(xlimits, break.by.what, label.by.what,  minor.break.by.what) {
    
    insert_minor <- function(major_labs, n_minor) {
      labs <- c( sapply( major_labs, function(x) c(x, rep("", n_minor) ) ) )
      labs #[1:(length(labs)-n_minor)]   
    }
    
    insert.n.minor = label.by.what/break.by.what-1
    
    xlimits = round(xlimits)
    xbreaks = seq(min(xlimits), max(xlimits), by=break.by.what); 
    xminor_breaks=seq(min(xlimits), max(xlimits),by=minor.break.by.what)
    xlabels = insert_minor(seq(min(xlimits), max(xlimits), by=label.by.what), insert.n.minor ) 
    xlabels = xlabels[1:length(xbreaks)]
    return(list(xbreaks, xlabels))
  }
  
  # month
  if (xscale=="month") { 
    break.by.what = 7;  
    label.by.what=28; 
    minor.break.by.what=1; 
    insert.n.minor=label.by.what/break.by.what-1;}
  
  
  # week
  if (xscale=="week") { 
    break.by.what = 1; 
    label.by.what=7;  
    minor.break.by.what=1; 
    insert.n.minor=label.by.what/break.by.what-1}
  
  
  
  # specified scale (5-20)   100_200  must be multiple of the first variable
  tt = grep("_", xscale)>0  
  if (length(tt)>0) {
    tt = as.numeric(unlist(strsplit(xscale, "_"))); 
    #    t2 = as.numeric(unlist(strsplit(xscale, "_"))); 
    #    if (!is.na(t1)) {tt = t1
    #    }else{ tt = t2} 
    names(tt) = c("break", "label") 
    stopifnot(mod(tt["label"], tt["break"])==0) 
    
    break.by.what = tt["break"]; 
    label.by.what=tt["label"];  
    minor.break.by.what=1; 
    insert.n.minor=label.by.what/break.by.what-1
  } 
  
  if (length(break.by.what)>0 & length(label.by.what)>0 ){    
    tt=setup.scale(xlimits, break.by.what, label.by.what,  minor.break.by.what)
    xbreaks = tt[[1]]
    xlabels = tt[[2]]      
  }
  
  return(list(breaks=xbreaks, labels=xlabels))
}



# for debug 
if (1==2 ) { 
  setup_axis(xscale='week', xlimits=c(0.01, 125)) 
  setup_axis(xscale='month', xlimits=c(0.01, 125)) 
  setup_axis(xscale='log', xlimits=c(0.01, 125)) 
  setup_axis(xscale='2_10', xlimits=c(0.01, 125)) 
  tt = setup_axis(xscale='nonlog', xlimits=c(0.01, 125)) 
  
}



# as.numeric(1:10 %o% 10 ^ (0:4))       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!###



