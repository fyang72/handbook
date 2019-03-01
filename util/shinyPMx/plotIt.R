

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
       x = x[which(!is.infinite(x))]
 
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
  
   
   
    
   
   
  
if (1==2) { 
  
  #----------------------------------------------------------------------------- 
  # summary of concentation by ARMA and nominal time point 
  #-----------------------------------------------------------------------------
  tdata = adpc 
  tdata <- calc_stats(tdata, group_by=c("ARMA", "NTIM"), value="DVOR") %>% 
    select(ARMA, NTIM, N, Mean, SD,  Mean_SD, Median_Range) # , AGE, GESAGE, RACE, ETHINIC)
  tdata <- tdata %>% filter(ARMA !="Placebo")
  
  
  out = tdata %>% 
    gather(variable, value, -(ARMA:NTIM)) %>%
    unite(temp, ARMA, variable) %>%
    spread(temp, value)
  out[is.na(out)] = "---"
  
  colst = expand.grid( c("N", "Mean_SD", "Median_Range"), unique(tdata$ARMA))
  colnames(colst) = c("STATS", "ARMA")
  col.lst = paste(colst$ARMA, colst$STATS, sep="_")
  
  
  #----------------------------------------------------------------------------- 
  # for individual plot
  #-----------------------------------------------------------------------------
  x = tick_label(x=tdata$TIME, xmin=0, xmax=200, major.tick=28, minor.tick=7, log=FALSE)
  y = tick_label(x=tdata$DVOR, xmin=1E-2, xmax=1E+3,  log=TRUE) 
   
  tdata$USUBJID = tdata$ID
  tdata$ARMA ="SINGLE DOSE"
  xmax = max(tdata$TIME,na.rm=TRUE)
  ymax = max(tdata$DVOR,na.rm=TRUE)
  ggplot(tdata , aes(x=TIME, y=DVOR, group=USUBJID, col=ARMA)) + 
     geom_point() + 
     geom_line(lwd=0.5)  + 
     xlab("Time (day)") + 
     ylab("Concentration (mg/L)") + 
      
     scale_x_continuous(breaks=x$breaks, label=x$labels) + 
     scale_y_log10(breaks=y$breaks, label=y$labels) + 
  
     coord_cartesian(xlim = range(tdata$TIME)) + 
     coord_cartesian(ylim = range(tdata$DVOR)) + 
    
     base_theme(font.size = 14) + 
     theme(legend.title = element_blank())  + 
     theme(panel.grid.major = element_line(colour = "gray95",size=0.25))  + 
    
     facet_wrap(~ARMA) + #scale_y_log10()  +       #+ scale_y_log10()
    
     geom_point(data=tdata, aes(x=TIME, y=DVOR, group=USUBJID), col="blue")        # what if two datasets have different ARMA (legend)?
  
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
  plot_mean_profile <- function(data,  xval="NTIM", yval="Mean", color_by="ARMA", type="b", errorbar="SD" , ...) {
 
 
 # if(errbar && (!errval %in% c("SE", "SD"))){
#    stop('Error bar type must be one of "SD" or "SE".')
#  }
#  
      pkSummary = data
      pkSummary = pkSummary %>% ungroup() %>% as.data.frame()
      pkSummary$ARMA = pkSummary[, color_by]
      pkSummary$NTIM = pkSummary[, xval]
      pkSummary$Mean = pkSummary[, yval]
      
      # log & nonlog, mean & indiv   
      pkSummary$meanMinus = pkSummary$meanMinusSD 
      pkSummary$meanPlus = pkSummary$meanPlusSD
      if (errorbar=="SE") { pkSummary$meanMinus = pkSummary$meanMinusSE }
      if (errorbar=="SE") { pkSummary$meanPlus = pkSummary$meanPlusSE  } 
    
      #       
      tdata = pkSummary %>% dplyr::mutate(NTIM = as_numeric(NTIM),
                                   Mean=as_numeric(Mean), 
                                   meanMinus=as_numeric(meanMinus), 
                                   meanPlus=as_numeric(meanPlus)) %>% 
              filter(!is.na(NTIM))                                    
       #       filter(!is.na(NTIM), !is.na(log(Mean))) 
              
      #if (yscale=="log") {tdata = tdata %>% mutate(meanMinus=ifelse(is.na(log(meanMinus)), !is.na(log(meanPlus))                         
       
                                                    
      figLog = tdata %>%                                               # capitalize_names(tdata)
             base_xyplot("NTIM", "Mean", "ARMA", "ARMA", ... )     
             
             
             
#  pos <- if(adjust){
#    position_dodge(width = 1)
#  } else {
#    "identity"
#  }
  #
#    # Add on the error bars
#    if(errbar){
#
#      # Error bar widths
#      errwid <- ifelse(adjust, 1, 0.4)
#
#      output <- output +
#        geom_errorbar(mapping = aes_string(
#          x = x ,
#          ymin = "lower",
#          ymax = "upper",
#          color = colorBy),
#          width = errwid,
#          position = pos
#        )
#    }
#
         
       if(errorbar %in% c("SE", "SD")) {figLog = figLog + geom_errorbar(data=tdata, aes(ymin = meanMinus, ymax = meanPlus), width=2)}
       
       if (type == "b") {figLog = figLog + geom_point(size=1) + geom_line(size=1, lwd=1)}
       if (type == "l") {figLog = figLog + geom_line(size=1, lwd=1)}
       if (type == "p") {figLog = figLog + geom_point(size=1)}
                      
       return(figLog)
   }
     

 

plotIt8 <- function(data, input, session) {
  # aes(x=XVAL, y=YVAL, group=GROUP_BY, col=COLOR_BY, shape=SHAPE_BY
  if (is.null(data)) {return(NULL)}
  
    x = data %>% pull(XVAR) %>% as_numeric()
    y = data %>% pull(YVAR) %>% as_numeric()
    
    xmin = min(x, na.rm=TRUE)
    xmax = max(x, na.rm=TRUE)
    
    ymin = min(y, na.rm=TRUE)
    ymax = max(y, na.rm=TRUE)
    
    # x-scale, y-scale
    tick = strsplit(input$xscale, ",") %>% unlist() %>% as.numeric()
    if (length(tick)==2) {minor.tick=tick[1]; major.tick=tick[2]}
    if (length(tick)==1) {minor.tick=tick[1]; major.tick=tick[1]}
    
    xscale = tick_label(x, xmin, xmax, major.tick, minor.tick, log=FALSE)
    yscale = tick_label(y, xmin=1E-2, xmax=1E+3,  log=TRUE)   ##########
    
    myFig=ggplot(data , aes(x=XVAL, y=YVAL, group=GROUP_BY, col=COLOR_BY, shape=SHAPE_BY)) + 
      
      xlab(input$xlab) + 
      ylab(input$ylab) + 
      
      scale_x_continuous(breaks=as_numeric(xscale$breaks), label=as.character(xscale$labels)) + 
      #scale_y_log10(breaks=yscale$breaks, label=yscale$labels) + 
      
      coord_cartesian(xlim = as_numeric(input$xRange)) + 
      coord_cartesian(ylim = as_numeric(input$yRange)) + 
      
      base_theme(font.size = as.integer(input$fontSize)) + 
      theme(legend.title = element_blank())  + 
      theme(panel.grid.major = element_line(colour = "gray95",size=0.25)) + 
      #theme(panel.background = element_rect(colour = "black", size=2, fill=NA)) panel.background is drawn underneath the plot and panel.border is drawn on top of the plot.
      theme(panel.border = element_rect(colour = "black", fill=NA, size=1.5))
    
    #if log scale in Y-axis
    if (input$yscale=="log") {myFig = myFig + scale_y_log10(breaks=yscale$breaks, label=yscale$labels) }      
    
    # add facet_wrap
    if (input$facet_by!="") {myFig = myFig + facet_wrap(input$facet_by) }
    
    # add BLQ level
    eval(parse(text=paste("xrefline=c(", input$xrefline, ")", sep="")))
    xrefline = as_numeric(xrefline)
    if (!is.null(xrefline) & !is.na(xrefline)) {myFig = myFig + geom_hline(yintercept=xrefline, color="gray10", lty="dashed")  }      
    
    eval(parse(text=paste("yrefline=c(", input$yrefline, ")", sep="")))
    yrefline = as_numeric(yrefline)
    if (!is.null(yrefline) & !is.na(yrefline)) {myFig = myFig + geom_vline(xintercept=yrefline, color="gray10", lty="dashed")  }      
    
    if (input$plotType=="both")  {myFig = myFig+geom_point(show_guide=FALSE, size=4)+geom_line(lwd=1)}
    if (input$plotType=="line")  {myFig = myFig+geom_line(lwd=1)}
    if (input$plotType=="point") {myFig = myFig+geom_point(show_guide=FALSE,size=4)}    
    
    # only for errbar/CI plot
    if (!is.null(input$errbar_or_CI))  {
      if (input$errbar_or_CI=="errbar" & input$errorbar!="") {
        myFig = myFig + geom_errorbar(aes(ymin = meanMinus, ymax = meanPlus), width=2)  
      }
      
      # for confidence Interval
      if (input$errbar_or_CI=="CI") {
        if (input$CI=="95%") {myFig = myFig + geom_ribbon(data=data,aes(ymin=PCT2P5,ymax=PCT97P5),alpha=0.3, col="black")}
        if (input$CI=="90%") {myFig = myFig + geom_ribbon(data=data,aes(ymin=PCT5,ymax=PCT95),alpha=0.3, col="black")}
        if (input$CI=="80%") {myFig = myFig + geom_ribbon(data=data,aes(ymin=PCT10,ymax=PCT90),alpha=0.3, col="black")}
      }
    }
    return(myFig)
}





plotIt <- function(data, input, session, EPS = 1e-3) {  
  
  # data:   XVAR, YVAR, GROUP_BY, COLOR_BY, SHAPE_BY
  # input: 
  #    xscale, yscale, yscale2
  #    xRange, yRange
  #    xlab, ylab
  #    fontSize    facet_by  
  #    xrefline    yrefline  
  #    plotType
  #    indiv_errbar    
 
  
  #data = filterData()
  if (is.null(data)) {return(NULL)}
  if (is.null(input$xscale)) {return(NULL)}
  if (is.null(input$yscale)) {return(NULL)}
  
  if (is.null(input$xtick)) {return(NULL)}
  if (is.null(input$ytick)) {return(NULL)}
  
  
  x = data %>% pull(XVAR) %>% as_numeric()
  y = data %>% pull(YVAR) %>% as_numeric()
  
  xmin = min(x, na.rm=TRUE)
  xmax = max(x, na.rm=TRUE)
  
  ymin = min(y, na.rm=TRUE)
  ymax = max(y, na.rm=TRUE)
  
  # Log-scale
  xtick_log = tick_label(x, xmin=1E-2, xmax=1E+5,  log=TRUE)   # log, nonlog 
  ytick_log = tick_label(y, xmin=1E-2, xmax=1E+3,  log=TRUE)   # log, nonlog
  
  # linear x-tick ;
  xtick =  strsplit(as.character(input$xtick), ",") %>% unlist() %>% as.numeric()
  if (length(xtick)==2) {minor.tick.x=xtick[1]; major.tick.x=xtick[2]}
  if (length(xtick)==1) {minor.tick.x=xtick[1]; major.tick.x=xtick[1]}
  if (length(xtick)>2) {print("Only two values in xtick.")}
  xtick_ln = tick_label(x, xmin=xmin, xmax=xmax, major.tick=major.tick.x, minor.tick=minor.tick.x, log=FALSE)
   
  # linear y-tick
  ytick = strsplit(as.character(input$ytick), ",") %>% unlist() %>% as.numeric()
  if (length(ytick)==2) {minor.tick.y=ytick[1]; major.tick.y=ytick[2]}
  if (length(ytick)==1) {minor.tick.y=ytick[1]; major.tick.y=ytick[1]}  
  if (length(ytick)>2) {print("Only two values in ytick.")}
   
  
  ytick_ln = tick_label(x=y, xmin=ymin, xmax=ymax, major.tick=major.tick.y, minor.tick=minor.tick.y, log=FALSE)
   
  # final plot
  if (is.null(input$xRange)) {return(NULL)}
  data = data %>% filter(XVAR >= (input$xRange[1]) & XVAR <= (input$xRange[2]))  
  
  # final plot
  if (nrow(data)==0) {print("No data found before ggplot.")}
  myFig=ggplot(data , aes(x=XVAR, y=YVAR, group=GROUP_BY, col=COLOR_BY, shape=SHAPE_BY)) + 
    
    xlab(input$xlab) + 
    ylab(input$ylab) + 
    
    #scale_x_continuous(breaks=as_numeric(xtick$breaks), label=as.character(xtick$labels))  + 
    #scale_y_log10(breaks=yscale$breaks, label=yscale$labels) + 
    
    coord_cartesian(xlim = input$xRange) + 
    coord_cartesian(ylim = input$yRange+EPS) +    # can not be zero if log scale!!!!
    
    base_theme(font.size = as.integer(input$fontSize)) + 
    theme(legend.title = element_blank())  + 
    theme(panel.grid.major = element_line(colour = "gray95",size=0.1)) + 
    #theme(panel.background = element_rect(colour = "black", size=2, fill=NA)) panel.background is drawn underneath the plot and panel.border is drawn on top of the plot.
    theme(panel.border = element_rect(colour = "black", fill=NA, size=1.0))
  
  #if log scale in Y-axis
  if (input$xscale=="log") {myFig = myFig + scale_x_log10(breaks=xtick_log$breaks, label=xtick_log$labels) }   
  if (input$yscale=="log") {myFig = myFig + scale_y_log10(breaks=ytick_log$breaks, label=ytick_log$labels) }      
  
  if (input$xscale=="nonlog") {myFig = myFig + scale_x_continuous(breaks=as_numeric(xtick_ln$breaks), label=as.character(xtick_ln$labels)) }
  if (input$yscale=="nonlog") {myFig = myFig + scale_y_continuous(breaks=as_numeric(ytick_ln$breaks), label=as.character(ytick_ln$labels)) }
  
  # add facet_wrap
  if (input$facet_by!="") {
        myFig = myFig + facet_wrap(input$facet_by, scales = "free")  
        myFig = myFig + theme(panel.grid.minor= element_line(colour = "gray95",size=0.5),
              panel.grid.major= element_line(colour = "gray95",size=0.5),
              panel.spacing = unit(0.2, "lines"), 
              panel.border=element_rect(colour="black", fill=NA), 
              strip.background = element_blank()   # element_rect(colour=NA, fill=NA),
        )
  }
  
  # add BLQ level
  eval(parse(text=paste("xrefline=c(", input$xrefline, ")", sep="")))
  xrefline = as_numeric(xrefline)
  if (!is.null(xrefline) & !is.na(xrefline)) {myFig = myFig + geom_hline(yintercept=xrefline, color="gray10", lty="dashed")  }      
  
  eval(parse(text=paste("yrefline=c(", input$yrefline, ")", sep="")))
  yrefline = as_numeric(yrefline)
  if (!is.null(yrefline) & !is.na(yrefline)) {myFig = myFig + geom_vline(xintercept=yrefline, color="gray10", lty="dashed")  }      
  
  if (input$addline=="diagonal") {myFig = myFig + geom_abline(show.legend = FALSE, intercept=0, slope=1)  }
  if (input$addline=="loess") {myFig = myFig +stat_smooth(method = "loess", color = "blue", se = F, size = 1.0)   } 
  
  # 
  
  
  
  
  if (input$plotType=="both")  {myFig = myFig+geom_point(show_guide=FALSE, size=4)+geom_line(lwd=1)}
  if (input$plotType=="line")  {myFig = myFig+geom_line(lwd=1)}
  if (input$plotType=="point") {myFig = myFig+geom_point(show_guide=FALSE,size=4)}    
  
  # only for errbar/CI plot
  if (!is.null(input$errbar_or_CI))  {
      if (input$errbar_or_CI=="errbar") {
        if (input$errorbar!="") {myFig = myFig + geom_errorbar(aes(ymin = meanMinus, ymax = meanPlus), width=2) }  
      }
      
      # for confidence Interval
      if (input$errbar_or_CI=="CI") {
        if (input$CI=="95%") {myFig = myFig + geom_ribbon(data=data,aes(ymin=PCT2P5,ymax=PCT97P5),alpha=0.3, col="black")}
        if (input$CI=="90%") {myFig = myFig + geom_ribbon(data=data,aes(ymin=PCT5,ymax=PCT95),alpha=0.3, col="black")}
        if (input$CI=="80%") {myFig = myFig + geom_ribbon(data=data,aes(ymin=PCT10,ymax=PCT90),alpha=0.3, col="black")}
      }
  }
  
  
  myFig
}






