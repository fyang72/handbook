
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
#'   concTime(data = theData, adjust = TRUE)      ("NTIM", "Mean", "ARMA", "ARMA", ...
base_xyplot <- function(adpc, xval="NTIM", yval="DVOR", group="USUBJID", color_by="ARMA", type="b",         
  xlab="Day",     
  ylab="Concentration (mg/L)",
  
  xlimits=range(adpc[, xval], na.rm=TRUE),   
  ylimits=range(adpc[, yval], na.rm=TRUE), 
  
  xscale="month",             
  yscale="log", 
  
  font.size=14, 
  EPS = 1E-2, ...)   { 
 
#  group = interaction(USUBJID, ARMA), 
#  color = factor(ARMA)
 
  
#-------------------------------------------------------------------------------
# internal functions
#-------------------------------------------------------------------------------

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

 

#-------------------------------------------------------------------------------
# checking data and input parameters
#-------------------------------------------------------------------------------
 

  adpc = as.data.frame(adpc)      ########## important!!!!
  
  
  #stopifnot(c(xval, yval, group, color_by) %in% colnames(adpc))
  # ARMA must be ordered factors
  #stopifnot(is.factor(adpc[, color_by]))
  #adpc$ARMA = ordered(adpc$ARMA, levels=levels(adpc$ARMA))
  
  as_numeric <- function(x) {suppressWarnings(as.numeric(as.character(x)))}
  adpc[, xval] = as_numeric(adpc[, xval])  
  adpc[, yval] = as_numeric(adpc[, yval]) 
  if (yscale=="log")  {adpc[, yval] = adpc[, yval] + EPS}  

  adpc$ARMA = adpc[, color_by]
  if(!is.factor(adpc$ARMA)) {adpc$ARMA = ordered(adpc$ARMA, levels =unique(adpc$ARMA))} 
 
  adpc = adpc %>% filter(adpc[, xval] <= xlimits[2] & adpc[, xval] >= xlimits[1])  
  adpc = adpc %>% filter(adpc[, yval] <= ylimits[2] & adpc[, yval] >= ylimits[1])  
  adpc = adpc %>% filter(!is.na(adpc[, xval]) & !is.na(adpc[, yval]))
  stopifnot(nrow(adpc)>1)
   
#-------------------------------------------------------------------------------
# setup axis
#-------------------------------------------------------------------------------       
  library('scales') 
  ytrans="identity"
  if (yscale=="log") {ylimits=c(EPS+min(ylimits,na.rm=TRUE), max(ylimits,na.rm=TRUE)); ytrans=log10_trans() } 
  
  xtrans="identity"
  if (xscale=="log") {xlimits=c(EPS+min(xlimits,na.rm=TRUE), max(xlimits,na.rm=TRUE)); xtrans=log10_trans() } 
        



 
  # Pre-plot---------------
  bp <- lazyeval::interp(~ggplot(data=adpc, aes(x=TIME, y=DVOR,  group=USUBJID, col=ARMA, shape=ARMA )) + 
    geom_point(), 
    
  # substitute dummy variables
  #--------------------------------------
  TIME = as.name(xval),
  DVOR = as.name(yval),
  USUBJID = as.name(group), 
  ARMA = as.name(color_by) )
  
   
  # if log scale
  # packageVersion("ggplot2")  ?2.2.1?
  
  bp = lazyeval::lazy_eval(bp)
  tt = ggplot_build(bp)$layout$panel_ranges[[1]]
  x_breaks = tt$x.minor_source
  x.major.tick = tt$x.major_source[2] - tt$x.major_source[1]   
  x.minor.tick = tt$x.minor_source[2] - tt$x.minor_source[1]   
 
  y_breaks = tt$y.minor_source
  y.major.tick = tt$y.major_source[2] - tt$y.major_source[1]   
  y.minor.tick = tt$y.minor_source[2] - tt$y.minor_source[1]   
  
  if (tolower(trim(xscale))=="week")  {
      x.minor.tick=1; x.major.tick = 7; 
      x_breaks = unique(c(seq(min(0, floor(min(xlimits)/7)*7), 0, by=x.minor.tick), 
                   seq(0, max(0, ceiling(max(xlimits)/7)*7), by=x.minor.tick) ))}

  if (tolower(trim(xscale))=="biweek")  {
      x.minor.tick=7; x.major.tick = 14; 
      x_breaks = unique(c(seq(min(0, floor(min(xlimits)/7)*7), 0, by=x.minor.tick), 
                   seq(0, max(0, ceiling(max(xlimits)/7)*7), by=x.minor.tick) ))}
                                      
  if (tolower(trim(xscale))=="month")  {
      x.minor.tick=7;  x.major.tick = 28; 
      x_breaks = unique(c(seq(min(0, floor(min(xlimits)/7)*7), 0, by=x.minor.tick), 
                   seq(0, max(0, ceiling(max(xlimits)/7)*7),  by=x.minor.tick)))}
     
  if (tolower(trim(xscale))=="bimonth")  {
      x.minor.tick=28;  x.major.tick = 56; 
      x_breaks = unique(c(seq(min(0, floor(min(xlimits)/7)*7), 0, by=x.minor.tick), 
                   seq(0, max(0, ceiling(max(xlimits)/7)*7),  by=x.minor.tick)))}
                        
    x_labels = ifelse(x_breaks %% x.major.tick==0, x_breaks, " ")
    y_labels = ifelse(y_breaks %% y.major.tick==0, y_breaks, " ")
      
  #
#  x_labels =  c(every_nth(x_breaks[which(x_breaks<0)], x.major.tick/x.minor.tick, inverse = TRUE)[1:length(x_breaks[which(x_breaks<0)])], 
#                every_nth(x_breaks[which(x_breaks>=0)], x.major.tick/x.minor.tick, inverse = TRUE)
#                 )
#  x_labels = x_labels[1:length(x_breaks)]  
#  x_labels[which(!mod(as_numeric(x_labels), x.major.tick) %in% c(0, NA))] = " "
#  x_labels  
#                      
#                           
#  y_labels = c(rev(every_nth(rev(y_breaks[which(y_breaks<=0)]), y.major.tick/y.minor.tick, inverse = TRUE))[1:length(y_breaks[which(y_breaks<=0)])], 
#               every_nth(y_breaks[which(y_breaks>=0)], y.major.tick/y.minor.tick, inverse = TRUE)
#               )
#  y_labels = y_labels[1:length(y_breaks)]                                                       
#  y_labels[which(!mod(as_numeric(y_labels), y.major.tick) %in% c(0, NA))] = " "
#                 
#
    
    
                                    
#my.ggp.yrange <- ggplot_build(bp)$layout$panel_ranges[[1]]$y.range
#my.ggp.xrange <- ggplot_build(bp)$layout$panel_ranges[[1]]$x.range
# 
   
#-------------------------------------------------------------------------------
# final ggplot
#-------------------------------------------------------------------------------   
 bp <- lazyeval::interp(~ggplot(data=adpc, aes(x=TIME, y=DVOR,  group=USUBJID, col=factor(ARMA), shape=factor(ARMA))) +     # ,size, fill, 
   
  # geom and its color, shape, and size
  #-------------------------------------- 
  #geom_point(aes(shape=ARMA, size=ARMA)) +
  #geom_line(lwd=1) +  
  labs(list(x=xlab, y=ylab)) +  
 
  # scale_color_discrete(name="sf", labels = c("Male", "Female")
  scale_color_manual(name="",      # name of the color legend
                   #breaks=c("A","B","C", "D", "E", "F"),  
                   values = colScheme(), #[1:length(unique(adpc$ARMA))],  
                   labels=unique(adpc$ARMA))  + 
  #scale_shape(name="", solid=FALSE, guide=FALSE,
  #                 values = rep(seq(0, 14), time=10)[1:length(unique(adpc$ARMA))]) + #, breaks=c("a","b","c", "d", "e", "f"), labels=unique(adpc$ARMA)) +                   
  scale_shape_manual(guide=FALSE,    name="",        # name of the shape legend
                   #breaks=c("a","b","c", "d", "e", "f"),  
                   #values = rep(seq(0, 14), time=10), #[1:length(unique(adpc$ARMA))], 
                   values = rep(seq(15, 25), time=100), #[1:length(unique(adpc$ARMA))], 
                   labels=unique(adpc$ARMA))  +  
  scale_size_manual(guide=FALSE, name="", values=rep(2, time=100), #length(unique(adpc$ARMA))), 
                   labels=unique(adpc$ARMA)) + 
  
  #geom_point(size=1.5) +                   #  shape=c("a","b","c","d","e","f"), 
  #geom_line(lwd=1.2) + 
  
  # x-axis and y-axis
  #--------------------------------------
  coord_cartesian(xlim = xlimits) + 
  coord_cartesian(ylim = ylimits) + 
  


  scale_x_continuous(trans=xtrans, 
                     #limits = xlimits, #expand = c(0,0),  
                     breaks= x_breaks, 
                     #minor_breaks=seq(0, max.axis,by=y.minor.tick), 
                     labels = x_labels) +        
    
                    
  scale_y_continuous(trans=ytrans, 
                     breaks=y_breaks,
                     #minor_breaks=seq(0, max.axis,by=y.minor.tick),
                     #limits = xlimits, #expand = c(0,0),  
                     labels = y_labels) +
   #geom_errorbar(data=filter(adpc, !is.na(log(meanMinus)), !is.na(log(meanPlus))), aes(ymin = abs(meanMinus), ymax = abs(meanPlus)), width=2) +                    
  #scale_y_log10() +                    

  # default theme
  #--------------------------------------
  #theme_set(theme_gray(base_size = 20))
  theme_bw() ,
  
  # substitute dummy variables
  #--------------------------------------
  TIME = as.name(xval),
  DVOR = as.name(yval),
  USUBJID = as.name(group), 
  ARMA =as.name(color_by)) 
 
 
  # if log scale
  library(ggplot2)
  # print(regnR::base_theme(font.size))
 
  bp0 = lazyeval::lazy_eval(bp) +  base_theme(font.size)  
     
    
 #
# 
#      # Add the relevant legend label
#      if(is.factor(data[[colorBy]]) || is.character(data[[colorBy]])){
#        output <- output + scale_fill_discrete(lgndlab)
#
#      }
#      else if(is.numeric(data[[colorBy]])){
#        output <- output +  scale_fill_continuous(lgndlab)
#
#      }
#      
#      # Add the relevant legend label
#      if(is.factor(data[[colorBy]]) || is.character(data[[colorBy]])){
#        output <- output + scale_color_discrete(lgndlab)
#
#      }
#      else if(is.numeric(data[[colorBy]])){
#        output <- output +  scale_color_continuous(lgndlab)
#      }
#      
#
#  # Add the relevant x label and log scale if necessary
#  if(logx){
#    output <- output + scale_x_log10(xlab)
#  } else if(is.factor(data[[x]])){
#    output <- output + scale_x_discrete(xlab)
#  } else if(is.numeric(data[[x]])){
#
#    # If the data is weekly,
#    breakVals <- if(!is.null(scale) && scale=="week"){
#      seq(from = min(data[x]), to = max(data[x])+6, by = 7)
#    } else if(!is.null(scale) && scale=="month") {
#      seq(from = min(data[x]), to = max(data[x])+27, by = 28)
#    } else {
#      waiver()
#    }
#
#    output <- output + scale_x_continuous(xlab, limits = xlim, breaks = breakVals)
#  }
#
#  # Add the relevant y label and log scale if necessary
#  if(logy){
#    output <- output + scale_y_log10(ylab)
#  } else {
#    output <- output + scale_y_continuous(ylab, limits = ylim)
#  }     

 
   bp = bp0     
  if (yscale=="log") {           
      bp <- bp +   
          scale_y_continuous(trans=log10_trans(), 
                                      breaks = trans_breaks("log10", function(x) 10^x, n=5),
                                      labels = trans_format("log10", math_format(10^.x))) # +  
          #annotation_logticks(base=10, sides = "lr" , scaled=TRUE, 
          #              short = unit(1,"mm"),  mid = unit(1.5,"mm"), long = unit(2,"mm"))      #  "trbl"   # All four sides
          
     bp
     
        
  }

#
#  extract_label <- function(x) {eval(parse(text=gsub("`", ")", gsub("^`", "^(",  x , fix=TRUE), fix=TRUE))) }
#   
#
  # add grid to each label line 
  #tt =  ggplot_build(bp)$panel$ranges[[1]]$x.labels 
#  x.label = as.numeric(tt[which(tt!="")])
# 
 
  tt = ggplot_build(bp)$layout$panel_ranges[[1]]
  x_labels = tt$x.major_source
  
  y_labels = tt$y.major_source
  
    
#  tt =  ggplot_build(bp)$panel$ranges[[1]]$y.labels 
#  y_labels = as.numeric(tt[which(tt!="")])
      
    #  print(x_labels)
#   print(y_labels)
     
  if (yscale=="log") {y_labels =  10^(y_labels) }
  bp = bp + 
  geom_vline(xintercept= x_labels, color="gray95") + 
  geom_hline(yintercept= y_labels, color="gray95")  
   

 if (type == "b") {bp = bp + geom_point(size=4, show.legend = FALSE) + geom_line(size=1, lwd=1)}
 if (type == "l") {bp = bp + geom_line(size=1, lwd=1)}
 if (type == "p") {bp = bp + geom_point(size=3)}
 
 
  # add guides                       
  bp = bp +  guides(color = guide_legend(
                     nrow = 2, #ceiling(nchar(paste(unique(adpc[, color_by]), collapse=""))/100), 
                     byrow = TRUE, 
                     direction = "horizontal"))     
            #        order=1,                                
            #        title.position = "top",
            #        label.position="bottom", 
            #        label.hjust = 0.5, label.vjust = 0.5,
            #        label.theme = element_text(angle = 90))  
           #scale_shape(guide=FALSE))       # or Remove legend for the point shape p+scale_shape(guide=FALSE)
           #scale_size(guide=FALSE)    # or Remove legend for size   p +scale_size(guide=FALSE)
          # scale_color_manual(guide=FALSE)
                               
   # adjust the title position                               
  bp = bp + theme(plot.title = element_text(hjust = 0.5, vjust=2.12))
          
          

                
                
                                               
 return(bp)
}
 
   

 
  
 ###############################################################################
 ###############################################################################
 ###############################################################################
 ###############################################################################
 
 # in case base_xyplot is not working
 if (1==2) {

      # DVOR + IPRED/PRED vs TIME 
      #-------------------------------------
      library('regnR')
      #source("H:\\FYANG\\aTemplate\\Rpackage\\regnR\\latex\\_common.R")
        # filter(adpx, STUDYID == "R1500-CL-1321")
      #tdata = tdata %>% filter(ARMA == "R2810: 3 mg/kg")
      tdata$TIME = as_numeric(tdata$TIME)
      tdata$DVOR = as_numeric(tdata$DVOR)
      tdata = tdata %>% filter(!is.na(TIME) & !is.na(DVOR))
      tdata = tdata %>% filter(!is.na(log(TIME)), !is.na(log(DVOR)) )   
     # figLog =capitalize_names(tdata) %>%
    #         base_xyplot("TIME", "DVOR", "USUBJID", "ARMA",  
    #              xlab.txt="Day",   ylab.txt="Concentration", 
    #              xlimits=c(0, 150),     ylimits=range(tdata$DVOR,na.rm=TRUE), 
    #              xscale="month",       yscale="log", 
    #              font.size=12 )    +   
                  
      figLog =capitalize_names(tdata) %>%
               ggplot(aes(x=TIME, DVOR, group=USUBJID, ARMA=ARMA)) + 
      scale_y_log10() +             
      coord_cartesian(xlim = c(0, 1000)) + 
      coord_cartesian(ylim = c(1, 1000)) + 
        theme_bw() +  regnR::base_theme(font.size=12 )+           
               
                  #xlab.txt="Day",   ylab.txt="Concentration", 
                  #xlimits=c(0, 150),     ylimits=range(tdata$DVOR,na.rm=TRUE), 
                  #xscale="month",       yscale="log", 
                  #font.size=12 )                          
             geom_point(size=1) + 
             geom_line(size=1, lwd=1) + 
             geom_hline(yintercept=0.078, color="gray10", lty="dashed")  
      bp = figLog #+ facet_wrap(~ARMA)   
      
      #out$USUBJID= out$ID
      #out$ARMA = "R2810: 3 mg/kg"
      #bp = bp + geom_line(data=tdata,aes(x=TIME, y=IPRED, group=USUBJID, ARMA=ARMA  ))  
      #bp = bp + geom_line(data=simdat,aes(x=TIME, y=PRED), col="red")  
      

 #geom_ribbon(data=predframe,aes(ymin=lwr,ymax=upr),alpha=0.3))
 
       
     # bp
      
  }
  
  
  
  
  
  
  
 
 
  