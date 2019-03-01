


  ##############################################################################
  ##############################################################################
  # myPanel
  ##############################################################################
  ##############################################################################
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
  prepanel.ci <- function(x, y, ly, uy, subscripts, arrow.length=1, BLQ.level=0.078,...) {
    x  <- as.numeric(x)
    y  <- as.numeric(y)
         
    ly <- as.numeric(ly[subscripts])                                 
    uy <- as.numeric(uy[subscripts])

    list(ylim = range(y, uy, ly, finite = TRUE))  
    #list(ylim = range(ytck$lim, finite = TRUE),
    #     xlim = range(xtck$lim, finite = TRUE))    
    }
  
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
  myPanel <- function(x,y,ly,uy,subscripts,arrow.length=1,BLQ.level=0.078, ...) { 
    #grid.x = as.my.numeric(scale.x$labels[which(scale.x$labels!=" ")])
    #grid.y = as.my.numeric(scale.y$labels[which(scale.y$labels!=" ")])
      
    panel.abline(h = c( BLQ.level ),  lty = "dotted", lwd =0.3, col = "gray")  
    
    panel.abline(v = c( 112 ),  lty = "dotted", lwd =0.3, col = "gray") 
      
    #panel.abline(BLQ.level, lty = "dotted", lwd =0.3, col = "black")
    
    # add errorbar
    x <- as.numeric(x)
    y <- as.numeric(y)
    ly <- as.numeric(ly[subscripts])
    uy <- as.numeric(uy[subscripts])
    panel.xyplot(x,y,...)   
    
    #panel.polygon(c(x, rev(x)), c(uy, rev(ly)),  ...)
        
    panel.arrows(x, ly, x, uy, col = "black", lwd=1.2,
                 length = arrow.length, unit = "native",  # width of arrowbar may depend on the xlim
                 #length = 0.5*(max(xlim)-min(xlim)), unit = "native",  # width of arrowbar may depend on the xlim                 
                 angle = 90, code = 3) 
    #panel.grid(0,0, col="transparent")                     
   
    }     
       
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
  panel.bands <- function(x, y, upper, lower, fill, col, 
             subscripts, BLQ.level=0.078, ..., font, fontface)
 {
    upper <- upper[subscripts]
    lower <- lower[subscripts]
    panel.polygon(c(x, rev(x)), c(upper, rev(lower)), col = fill, border = FALSE,  ...)
    
    panel.abline(h = c( BLQ.level ),  lty = "dotted", lwd =0.3, col = "gray") 
    panel.text(12, log10(10^BLQ.level[1]*1.1), "EC99=4.9 mg/L(subtype A)")
    panel.text(12, log10(10^BLQ.level[2]*1.1), "EC97=7.0 mg/L(subtype B)") 
    panel.text(12, log10(10^BLQ.level[3]*1.1), "EC99=27.8 mg/L(subtype B)")     
         
 }
 


  ##############################################################################
  ##############################################################################
  # par.settings setup
  ##############################################################################
  ##############################################################################
    #par.settings=list(axis.components=list(bottom=list(tck=scale.x$tck), left=list(tck=scale.y$tck)),
    #                  layout.heights = list(xlab.key.padding =key.xlab.padding ) ),    
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
  setup.par.settings <- function() {
  par.settings <- list(superpose.line = list(
                             lty = "solid",    # lty.lst2[1:n.key],
                             lwd = 1    # llwd.lst2[1:n.key]),
                             #col = factor(color.scheme[1:n.key]),    already included in COL
                             ),                             
                       superpose.symbol = list(
                             alpha = 1,
                             cex = 1.2, #pcex.lst2[1:n.key], #c(1.2, 1.2),
                             pch = 19   #pch.lst2[1:n.key]
                             #fill= pcol.lst2[1:n.key],
                             #col = factor(color.scheme[1:n.key])
                             ),
                       #axis.components=list(bottom=list(tck=scale.x$tck), left=list(tck=scale.y$tck)),      
                       layout.heights = list(xlab.key.padding = 3)
                       #axis.components=list(bottom=list(tck=scale.x$tck),  #############
                        #                    left=list(tck=scale.y$tck))     #################
                        )
                
                # when use double Y plot        
                # layout.widths=list(ylab.right=5, right.padding=0)) , 
                        
   }
              
              

  ##############################################################################
  ##############################################################################
  # legend setup
  ##############################################################################
  ##############################################################################   
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
  setup.key  <- function(key.txt, 
                       key.col=c("black","blue","green","red"), 
                       key.space="top", 
                       key.lty="solid", 
                       key.lwd=2,
                       key.pch=19,
                       key.pcex=1,
                       key.cex=1, 
                       key.columns=1, 
                       key.rows=2, 
                       between.columns = 2
                       ) {                 
  
    key.txt =  as.character(key.txt) 
    n.key = length(key.txt)
 
    key.lcol = key.col[1:n.key]
    key.lwd = rep(key.lwd , times= n.key)
    key.lty =  key.lty 
 
    key.pcol = key.col[1:n.key]
    key.pch = rep(key.pch, times= n.key)
    key.pcex = rep(key.pcex, times= n.key)
    
    key.xlab.padding = 3     
    
    key = NULL
    key= list( text= list(txt=key.txt,cex=key.cex),
                 space=key.space, just = 0.5,   # right  bottom     # just: up goes left
                 #points=list(pch=key.pch, col=key.pcol,cex=key.pcex),
                 lines=list(col=key.lcol,lwd=key.lwd,lty=key.lty),
                 columns=key.columns, rows=key.rows, between.columns=2)
    return(key)
    }

 
  
  
  ##############################################################################
  ##############################################################################
  ##############################################################################
  ##############################################################################  
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
xscale.components.subticks <- function (lim, ..., n = 5, n2 = n * 5, min.n2 = n + 5) 
{
    ans <- xscale.components.default(lim = lim, ..., n = n)
    ans2 <- xscale.components.default(lim = lim, ..., n = n2, 
        min.n = min.n2)
    ticks <- ans$bottom$ticks$at
    ticks2 <- ans2$bottom$ticks$at
    ticks2 <- ticks2[!(ticks2 %in% ticks)]
    ans$bottom$ticks$at <- c(ticks, ticks2)
    ans$bottom$ticks$tck <- c(rep(1, length(ticks)), rep(0.5, 
        length(ticks2)))
    ans$bottom$labels$at <- ans$bottom$ticks$at
    ans$bottom$labels$labels <- c(ans$bottom$labels$labels, rep(" ", 
        length(ticks2)))
    ans$bottom$labels$check.overlap <- FALSE
    ans
    
    print(ans)
} 

  
  ##############################################################################
  ##############################################################################
  # xscale.components.subticks.FY
  ##############################################################################
  ##############################################################################
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
  .xscale.components.subticks.days <- function(..., by.week=28, by.day=7, n = 5, n2 = n * 5, min.n2 = n + 5) {
    xscale.components.subticks.days(...,  by.week=by.week,  by.day=by.day, n = n, n2 = n2, min.n2 = min.n2)
  }
  
  
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
  xscale.components.subticks.days <- function (lim, ..., by.week=7, by.day=1, n = 5, n2 = n * 5, min.n2 = n + 5) 
  {
  #lim = c( -7.394583,  112.352917)
  #n = 5
  #n2 = n * 5
  #min.n2 = n + 5
  
  #print(lim)
    #ans <- xscale.components.default(lim = lim,   n = n)
    #ans2 <- xscale.components.default(lim = lim,   n = n2, min.n = min.n2)
    
      
    ans <- xscale.components.default(lim = lim, ..., n = n)
    ans2 <- xscale.components.default(lim = lim, ..., n = n2, min.n = min.n2)
    
    #ticks <- ans$bottom$ticks$at
    #ticks2 <- ans2$bottom$ticks$at
    #ticks2 <- ticks2[!(ticks2 %in% ticks)]
    #lim = c(ceiling(min(lim)), ceiling(max(lim))) # , digits=0)
    lim = round(lim, digits=0)
    lim.week = round(lim/by.week, digits=0)
     
    ticks <- seq(min(lim.week), max(lim.week), by=1)*by.week #  major tick, i.e. weeks
    ticks2 <- seq(min(lim), max(lim), by=by.day) #   ans2$bottom$ticks$at    # i.e. days
    ticks2 <- ticks2[!(ticks2 %in% ticks)]
        
    ans$bottom$ticks$at <- c(ticks, ticks2)
    ans$bottom$ticks$tck <- c(rep(1, length(ticks)), rep(0.5, 
        length(ticks2)))
    ans$bottom$labels$at <- ans$bottom$ticks$at
    
    ans$bottom$labels$labels <- ticks
    ans$bottom$labels$labels <- c(ans$bottom$labels$labels, rep(" ", 
        length(ticks2)))
    ans$bottom$labels$check.overlap <- TRUE
    
    ans   
    #print(ans)
}


  
  ##############################################################################
  ##############################################################################
  # yscale.components.log10ticks.FY
  ##############################################################################
  ##############################################################################
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
  logTicks <- function (lim, loc = c(1, 5)) { 
  ii <- floor(log10(range(lim))) + c(-1, 2) 
  main <- 10^(ii[1]:ii[2]) 
  r <- as.numeric(outer(loc, main, "*")) 
  r[lim[1] <= r & r <= lim[2]] 
  }


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
  yscale.components.log10 <- function(lim, ...) { 
  
  print(lim)
  ans <- yscale.components.default(lim=lim)
  tick.at <- logTicks(10^lim, loc=1:9)
  
  tick.at.major <- logTicks(10^lim, loc=1)
  major <- tick.at %in% tick.at.major
  
  # ans$bottom$ticks$at <- log(tick.at, 10)
  # ans$bottom$ticks$tick <- ifelse(major, 1.5, 0.75) 
  # ans$bottom$labels$at <- log(tick.at, 10)
  # ans$bottom$labels$labels <- as.character(tick.at)
  # ans$bottom$labels$labels[!major] <- "" 
  # ans$bottom$labels$check.overlap <- FALSE   
  
  #ans = NULL
  #ans$left = TRUE
  ans$left$ticks$at <- log(tick.at, 10)
  ans$left$ticks$tck <- ifelse(major, 1.5, 0.5) 
  ans$left$ticks$tick <- ifelse(major, 1.5, 0.5) 
  ans$left$labels$at <- log(tick.at, 10)
  ans$left$labels$labels <- as.character(tick.at)
  ans$left$labels$labels[!major] <- " " 
  ans$left$labels$check.overlap <- FALSE 
  
  #ans$right = FALSE
  print(ans)
  ans
  
  }
  
  
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
xscale.components.log10ticks <- function (lim, logsc = FALSE, at = NULL, ...) 
{
    ans <- xscale.components.default(lim = lim, logsc = logsc, 
        at = at, ...)
    if (is.null(at)) 
        return(ans)
    if (identical(logsc, FALSE)) 
        return(ans)
    logbase <- logsc
    if (identical(logbase, TRUE)) 
        logbase <- 10
    if (identical(logbase, "e")) 
        logbase <- exp(1)
    tick.at <- logTicks(logbase^lim, loc = 1:9)
    tick.at.major <- logTicks(logbase^lim, loc = 1)
    major <- tick.at %in% tick.at.major
    ans$bottom$ticks$at <- log(tick.at, logbase)
    ans$bottom$ticks$tck <- ifelse(major, 1, 0.5)
    ans$bottom$labels$at <- log(tick.at, logbase)
    ans$bottom$labels$labels <- as.character(tick.at)
    ans$bottom$labels$labels[!major] <- " "
    ans$bottom$labels$check.overlap <- FALSE
    ans
}



###default .log10ticks: 
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
 yscale.components.log10ticks <- function (lim, logsc = FALSE, at = at, ...) 
{ 
#lim = log10(c(0.01, 20))
# print(lim)
# print(logsc)
# print(at)
 
    ans <- yscale.components.default(lim = lim, logsc = logsc, 
        at = at) 
    if (is.null(at)) 
        return(ans) 
    if (identical(logsc, FALSE)) 
        return(ans) 
    logbase <- logsc 
    if (identical(logbase, TRUE)) 
        logbase <- 10 
    if (identical(logbase, "e")) 
        logbase <- exp(1)
         
    tick.at <- logTicks(logbase^lim, loc = 1:9) 
    tick.at.major <- logTicks(logbase^lim, loc = 1) 
    major <- tick.at %in% tick.at.major 
    
    ans$left$ticks$at <- log(tick.at, logbase) 
    ans$left$ticks$tck <- ifelse(major, 1, 0.5) 
    
    ans$left$labels$at <- log(tick.at, logbase) 
    ans$left$labels$labels <- as.character(tick.at) 
    ans$left$labels$labels[!major] <- " " 
    ans$left$labels$check.overlap <- FALSE 
    
    ans 
} 
      
      

  ##############################################################################
  ##############################################################################
  # Final plot
  ##############################################################################
  ##############################################################################  
  if (1 ==2) {  
  tdata.stats = data.process.4errorbar(tdata.org, 
      TIME.NAME = "NOM_DAY",
      DV.NAME = "DVOR", BLQ.level = 0.078, log="y", 
      ARMA.NAME = "ARMA",
      USUBJID.NAME = "USUBJID",
      TIMEPT.NAME = "TIMEPT" )  
  
  # if plot errorbar
  color.scheme = c(  "black",     "red",       "blue",      "green",     "cyan",     "brown",     "tan",   "yellow",    "violet",    "tomato",   
                   "orange",    "magenta",   "pink",      "salmon",    "chocolate", "plum",      "purple",    "navy")
                       
  require('latticeExtra')  
      
  xyplot(DV~TIME, tdata.stats, groups=ARMA , #distribute.type=T,   
    lwd=3, type="l",col=color.scheme, pch=19, 
    xlab="Nominal Sampling Time (Day)",    # Day or Week
    ylab="Observed Concentration (mg/L)",    
    scales = list(y=list(log=10), 
                  x=list(lim=c(-3, 45))  ),                  
    xscale.components = xscale.components.subticks.FY,         
    yscale.components = yscale.components.log10ticks.FY,             
    ly = tdata2$LOGLY, uy = tdata2$LOGUY,  arrow.length = 0.3, #grid.x, grid.y,   
    prepanel = prepanel.ci,  panel = function(...) panel.superpose(panel.groups=myPanel,  ...), # panel.superpose,   
    par.settings = setup.par.settings(),                                    
    key= setup.key(unique(tdata2$ARMA))   #,        
    #xlim=c(-3, 56)   
    )
  }
  
  

    
 
  
  
  
   
   
   
   
   
   
   
  
  
  
   
  
  
  
  
  
  
  
  
  
  
  
  
  
  

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%                                                     %
#%% Initial setup for all table, figure and data       %
#%                                                     %
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#==============
# Figure
#==============
u.setup.fig.parms <- function(fig.parms=NULL, n.max=100) {

  #fig.parms <- NULL

  # color list for all figures  c("black","red","blue","green","yellow","purple","cyan"
  col.lst <- c("black", "red", "blue", "green",   "yellow", "brown",  "tan" , "cyan",  "violet",  "tomato",
  "orange",  "magenta", "pink",  "salmon", "chocolate",   "plum",  "purple", "navy",
   "tan")
  col.lst <- rep(col.lst,100)

  lty.lst <- rep(c("solid","dashed", "dotdash", "longdash", "twodash", "dashed", "solid",   "longdash"), 100)
  pch.lst <- rep(c(15:25,0:6), 100)  # http://127.0.0.1:28237/library/graphics/html/points.html

  #n.max <- 100
  fig.parms$col.lst <- col.lst[1:n.max]
  fig.parms$pch.lst <- pch.lst[1:n.max]
  fig.parms$lty.lst <- lty.lst[1:n.max]
  fig.parms$lwd.lst <- rep(1, time=n.max)

  fig.parms$xlab <- "Nominal Sampling Day"
  fig.parms$ylab <- "Observed Concentration (mg/L)"

  fig.parms$lty.lst2  <- fig.parms$lty.lst[1:n.max]
  fig.parms$lcol.lst2 <- fig.parms$col.lst[1:n.max]
  fig.parms$llwd.lst2 <- fig.parms$lwd.lst[1:n.max]
  fig.parms$pch.lst2  <- fig.parms$pch.lst[1:n.max]
  fig.parms$pcol.lst2 <- fig.parms$col.lst[1:n.max]
  fig.parms$pcex.lst2 <- rep(1.2, time=n.max)  #

  fig.parms$xlab.txt <- ""
  fig.parms$ylab.txt <- ""

  fig.parms$logy <- TRUE
  fig.parms$xyplot.type <- "b"
  fig.parms$plot.errorbar <- TRUE
  #fig.parms$BLQ.level <- 0.078  # mg/L  ###########################
  
  fig.parms$xlab.txt <- ""
  fig.parms$ylab.txt <- 10^(seq(-3,+3,by=1))

  fig.parms$xyplot.key <- NULL
  fig.parms$key.points.plot <-  "plot"
  fig.parms$key.lines.plot <-  "plot"

  fig.parms$key.txt <- ""
  fig.parms$key.title <- ""
  fig.parms$key.columns <- 3
  fig.parms$key.rows <- 10

  fig.parms$key.cex <- 1.2
  fig.parms$key.columns <- 2 #length(fig.parms$key.txt)
  fig.parms$key.space <- "bottom"
  fig.parms$key.xlab.key.padding <- 3
  fig.parms$key.lty.lst2  <- fig.parms$lty.lst[1:n.max]
  fig.parms$key.lcol.lst2 <- fig.parms$col.lst[1:n.max]
  fig.parms$key.llwd.lst2 <- fig.parms$lwd.lst[1:n.max]
  fig.parms$key.pch.lst2  <- fig.parms$pch.lst[1:n.max]
  fig.parms$key.pcol.lst2 <- fig.parms$col.lst[1:n.max]
  fig.parms$key.pcex.lst2 <- rep(1, time=n.max)

  fig.parms$xyplot.main <- ""

return(fig.parms)
}


#==============
# Data
#==============

u.setup.data.parms <- function(data.parms=NULL, n.max=100) {

  #data.parms <- NULL

  #data.parms$SUBJECT.COLNM  <- "SUBJECT"     # x
  data.parms$VISIT.COLNM  <- "VISIT"     # x
  data.parms$TIME.COLNM  <- "NOM_DAY"     # x
  data.parms$CONCN.COLNM <- "DVOR"      # y
  data.parms$GROUP.COLNM <- "ARMA"         # which columns is for group
  data.parms$TEST.COLNM  <- "TEST"    # which measurement
  data.parms$SE <- "SE"
  data.parms$SD <- "SD"  
  

  data.parms$GROUP.VAR <- ""  # "150 mg A"         # which group
  data.parms$TEST.VAR  <- "" # Total REGN727"    # which measurement ***********************
  data.parms$TIME.UNIT <- "Day"

  return(data.parms)
}



setup.tck.mark <- function(day, xtck) {
    day = as.numeric(as.character(day))
    day = day[which(!is.na(day))]
       day = day[which(day!=0)]    
    if (xtck$log==FALSE) scale.x <- set.linear.tck.mark(day, xtck)
    if (xtck$log==TRUE)  {

       scale.x <- set.log.tck.mark(day, xtck)
    }  
    return(scale.x)
    }
       
#############################################################
#############################################################
set.linear.tck.mark <- function(day, xtck) {

  #day = y
  #xtck = ytck
  
  xlim = xtck$lim;
  by.7= xtck$by
  scale <- xtck$scale    # 10 # by.7
   
  day = day/scale
  xlim = xlim/scale
  by.7 = by.7/scale
  
  if(is.null(xlim)) xlim= range(day, na.rm=TRUE)
  
  s1 = floor((min(c(min(xlim),day),na.rm=TRUE)/by.7)) -1 
  s2 = ceiling((max(c(max(xlim),day),na.rm=TRUE)/by.7)) + 1
 
 
  weeks = seq(s1, s2, by=1)
  weeks    
 
  # at
  at.lst <- NULL; lab.lst <- NULL; tck.lst = NULL;                              
  for (i in 1:(length(weeks)-1)) { 
      at.lst  <- c(at.lst, seq(weeks[i]*by.7, weeks[i+1]*by.7, by=1)*scale) 
      lab.lst <- c(lab.lst, c(weeks[i]*by.7*scale, rep(' ', time=by.7-1), weeks[i+1]*by.7*scale) )
      tck.lst <- c(tck.lst, c(1, rep(0.5, time=by.7-1), 1) ) 
      
      ids <- which(!duplicated(as.character(at.lst)))
      at.lst = at.lst[ids]           
      lab.lst = lab.lst[ids] 
      tck.lst = tck.lst[ids]       
  }
  #cbind(at.lst, lab.lst, tck.lst)
   
  ids <- which(as.numeric(at.lst) >= (min(xlim*scale))  & as.numeric(at.lst) <= (max(xlim*scale)))
  ids <- seq(min(ids)-1, max(ids)+1, by=1)
  
  #tck.lst[which(lab.lst==" ")] = 0
  #at.lst[which(lab.lst==" ")] = NA
  
  # check it  
  tt = cbind('at'=at.lst[ids], 'tck'=tck.lst[ids], 'lab'=lab.lst[ids])
 
 
 #
#   
#  at.lst <- seq(min(weeks)*by.7+1,max(weeks)*by.7,1) 
#  at.lst <- at.lst * scale;                       ####################################
#   at.lst ; length(at.lst)
#  
#  lab.lst <- NULL;  for (i in weeks) {lab.lst <- c(lab.lst, rep(' ', time=by.7-1), by.7*i*scale)}
#  lab.lst <- lab.lst[(by.7+1):length(lab.lst)] 
#  lab.lst
#  
#  tck.lst <- c(rep(0, time=by.7-1), 0.5) + 0.5   # c(0,0,0,0,0,0,0.5)+0.5  
#  tck.lst <- rep(tck.lst, time=length(lab.lst)/by.7)   
#  #tck.lst <- rep(tck.lst, time=max(weeks)*by.7/by.7)   
#  tck.lst ; length(tck.lst)
#    
#  ids <- which(at.lst >= (min(xlim)*scale-scale)  & at.lst <= max(xlim)*scale+scale)     ######################
#  
#  ids <- which(at.lst >= (min(xlim)*scale)  & at.lst <= max(xlim)*scale)     ######################    
#  ids <- seq(min(ids)-1, max(ids)+1, by=1)
#  
  #t1=list(tck.lst=tck.lst[ids],at.lst=at.lst[ids],lab.lst=lab.lst[ids])
 
  scale.x <- list(cex=xtck$cex,at=at.lst[ids], labels=lab.lst[ids], tck=tck.lst[ids])
 
  return(scale.x)
}



#############################################################
#############################################################
set.log.tck.mark <- function(day, xtck) {
  EPS = 1E-10
  day = day + EPS

  xlim = xtck$lim;
  by.7= xtck$by
  scale <- xtck$scale    # 10 # by.7
  
  day = day/scale
  xlim = xlim/scale
  by.7 = by.7/scale
  
  if(is.null(xlim)) xlim= range(day, na.rm=TRUE)
  
  s1 = by.7^(floor(log((min(c(min(xlim),day))), base=by.7)) - 1)
  #s1 = min(s1, min(xlim))
  
  s2 = by.7^(ceiling(log((max(c(max(xlim),day))), base=by.7)) + 1)
  #s2 = max(s2, max(xlim))
  weeks = by.7^seq(log(s1,base=by.7), log(s2,base=by.7), by=1)
 
  # at
  at.lst <- NULL; lab.lst <- NULL; tck.lst = NULL;                              
  for (i in 1:(length(weeks)-1)) { 
      at.lst  <- c(at.lst, seq(weeks[i], weeks[i+1], by=weeks[i])) 
      lab.lst <- c(lab.lst, c(weeks[i], rep(' ', time=by.7-2), weeks[i+1]) )
      tck.lst <- c(tck.lst, c(1, rep(0.5, time=by.7-2), 1) ) 
      
      ids <- which(!duplicated(as.character(at.lst)))
      at.lst = at.lst[ids]           
      lab.lst = lab.lst[ids] 
      tck.lst = tck.lst[ids]       
  }
  
  

  ids <- which(as.numeric(at.lst) >= (min(xlim))  & as.numeric(at.lst) <= (max(xlim)))
  ids <- seq(min(ids)-1, max(ids)+1, by=1)
  
  # check it  
  tt = cbind('at'=at.lst[ids], 'tck'=tck.lst[ids], 'lab'=lab.lst[ids])
 
  
#  at.lst <- unique(at.lst)
#  at.lst; length(at.lst)
#   
#  # label                                                                                      ##########
#  lab.lst <- NULL;  for (i in 1:length(weeks)) {lab.lst <- c(lab.lst, rep(' ', time=by.7-2), weeks[i])}
#  #lab.lst <- NULL;  for (i in 1:length(weeks)) {lab.lst <- c(lab.lst, rep(' ', time=by.7-1), weeks[i])}   ################
#  #lab.lst <- lab.lst[(by.7+1):length(lab.lst)]   ##############
#  lab.lst <- lab.lst[(by.7-1):length(lab.lst)] 
#  lab.lst ; length(lab.lst)
#  
#
#  
#  # tick
#  #tck.lst <-    c(rep(0, time=by.7-1), 0.5) + 0.5   # c(0,0,0,0,0,0,0.5)+0.5   
#  tck.lst <-    c( rep(0, time=by.7-2), 0.5) + 0.5   # c(0,0,0,0,0,0,0.5)+0.5    
#  #tck.lst <- rep(tck.lst, time=length(lab.lst)/by.7)
#  tck.lst <- c(1.0, rep(tck.lst, time=length(weeks)-1) )
#  tck.lst ; length(tck.lst)
  


  #tt[,'at'] = as.numeric(as.character(tt[, 'at'])  )

  #t1=list(tck.lst=tck.lst[ids],at.lst=at.lst[ids],lab.lst=lab.lst[ids])
    
  scale.x <- list(cex=xtck$cex,log=xtck$by, at=at.lst[ids], labels=lab.lst[ids], tck=tck.lst[ids])
      
  return(scale.x)
}



 

#xyplot( result ~ time | location, data=dataset, groups=genotype, pch=19,
#type="b", col = c('red', 'green', 'blue'))
#
#The problem with trying to use color as a variable to 'match' to genotype is
#that both genotype and color are factors, but the default ordering of factor
#levels is alphabetic. This is no problem for genotype, but it is for color,
#so the matching of genotype-color pairs in dataset doesn't actually occur;
#instead, R will render colors blue, green and red, respectively, to match to
#genotypes A-C. To fix that problem, make color an ordered factor:
# 

if (1==2) { 
      TIME.NAME = "NOM_DAY";
      DV.NAME = "DVOR";
      ARMA.NAME = "TRTGROUP";
      USUBJID.NAME = "USUBJID";
      TIMEPT.NAME = "TIMEPT";
      color.scheme = c(  "black",     "red",       "blue",      "green",     "cyan",     "brown",     "tan",   "yellow",    "violet",    "tomato",   
                   "orange",    "magenta",   "pink",      "salmon",    "chocolate", "plum",      "purple",    "navy");
      xlim1=c(0, 150); ylim1=c(0.1, 1000); 
      log = "y"; BLQ.level=0.078; # mg/L
      xtck.scale1=1; xtck.scale2=28;
      ytck.scale1=1; ytck.scale2=10;
      xlab1="Nominal Sampling Time (Day)";    # Day or Week
      ylab1="Observed Concentration (mg/L)";
      
 }     
  
  
  
