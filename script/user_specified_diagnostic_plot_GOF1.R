
diagnostic_GOF1 <- function(dataset, params=NULL)  {
 
  xpdb = dataset
  
  # dataset = read_csv("./data/nmdat_0226_2019.csv", col_names=TRUE,  
  #                  col_type=cols(.default=col_character()))  # read as character  
    
  figure=NULL
  table =NULL
  data = NULL
  
  #--------------------------------------
  # setup  
  #--------------------------------------
  tdata0 = slot(xpdb, "Data")    # or xpdb@Data
  runno = slot(xpdb, "Runno")  # or xpdb@Runno
  
  # Need key variables
  if (!all(c("PRED", "IPRED", "DV") %in% colnames(tdata0))) {
    # c("default", "message", "warning", "error"), 
    showNotification("need PRED, IPRED and DV in the xpdb@Data", type = "error")
  }
  
  validate(need(all(c("PRED", "IPRED", "DV") %in% colnames(tdata0)), 
                message="need PRED, IPRED and DV in the data")
  )
           
  #--------------------------------------
  # PRED vs DV linear
  #--------------------------------------
  tdata <- tdata0 %>% mutate(xvar=PRED, yvar=DV)
  limits = range(c(tdata$DV, tdata$PRED), na.rm=TRUE)
  fig = tdata   %>% 
    ##filter(WRES!=0) %>%
    ggplot(aes(x=xvar, y=yvar))+
    geom_point(color="black",size=2, alpha=0.5)+
    coord_fixed(ratio=1, xlim = limits, ylim = limits) + 
    #geom_smooth(show.legend = FALSE,lwd=1,alpha=0.5)+ 
    stat_smooth(show.legend = FALSE,method="lm",se=FALSE,color="blue",size=1,lwd=1,alpha=0.5)+  
    geom_abline(show.legend = FALSE,intercept=0,slope=1, color="black",size=1)+
    theme_bw() + base_theme(font.size = 12) + 
    labs(#title ="PK Base model GOF",
      caption = paste0("source:", runno, ".lst"),
      x=xpdb@Prefs@Labels$PRED, y=xpdb@Prefs@Labels$DV)+
    theme(plot.title = element_text(size=12,face="bold"),
          plot.caption = element_text(face="italic"))
  
  # meta
  attr(fig, 'title') <- paste0("Diagnostic plot for population predicted vs observed in linear-scale")
  attr(fig, 'width') <- 6
  attr(fig, 'height') <- 6                             
  figure[["PRED_DVOR_LN"]] = fig 
  figure[["PRED_DVOR_LN"]]$data =  tdata
  figure[["PRED_DVOR_LN"]]$dataset = slot(xpdb, "Data")  # or xpdb
  
  
  #--------------------------------------
  # PRED vs DV log
  #--------------------------------------
  values = c(tdata$DV, tdata$PRED)
  limits = range(values[is.finite(log(values))], na.rm=TRUE)
  fig = tdata %>%  
    filter(is.finite(log(xvar)), is.finite(log(yvar))) %>% 
    ##filter(WRES!=0) %>%
    ggplot(aes(x=xvar, y=yvar))+
    geom_point(color="black",size=2, alpha=0.5)+
    coord_fixed(ratio=1, xlim = limits, ylim = limits) + 
    scale_y_log10() + scale_x_log10() + 
    #geom_smooth(show.legend = FALSE,lwd=1,alpha=0.5)+ 
    stat_smooth(show.legend = FALSE,method="lm",se=FALSE,color="blue",size=1,lwd=1,alpha=0.5)+  
    geom_abline(show.legend = FALSE,intercept=0,slope=1, color="black",size=1)+
    theme_bw() + base_theme(font.size = 12) + 
    labs(#title ="PK Base model GOF",
      caption = paste0("source:", runno, ".lst"),
      x=xpdb@Prefs@Labels$PRED, y=xpdb@Prefs@Labels$DV)+
    theme(plot.title = element_text(size=12,face="bold"),
          plot.caption = element_text(face="italic"))+ 
    
    annotation_logticks(sides ="bl")     # "trbl", for top, right, bottom, and left.
  
  # meta
  attr(fig, 'title') <- paste0("Diagnostic plot for population predicted vs observed in log-scale")
  attr(fig, 'width') <- 6
  attr(fig, 'height') <- 6                             
  figure[["PRED_DVOR_LOG"]] = fig 
  figure[["PRED_DVOR_LOG"]]$data =  tdata
  figure[["PRED_DVOR_LOG"]]$dataset =  slot(xpdb, "Data")  # or xpdb
   
  
  
  #--------------------------------------
  # IPRED vs DV linear
  #--------------------------------------
  tdata <- tdata0 %>% mutate(xvar=IPRED, yvar=DV)
  
  values = c(tdata$DV, tdata$IPRED)
  limits = range(values[is.finite(log(values))], na.rm=TRUE)
  
  limits = range(c(tdata$DV, tdata$IPRED), na.rm=TRUE)
  fig = tdata   %>% 
    #filter(WRES!=0) %>%
    ggplot(aes(x=xvar, y=yvar))+
    geom_point(color="black",size=2, alpha=0.5)+
    coord_fixed(ratio=1, xlim = limits, ylim = limits) + 
    #geom_smooth(show.legend = FALSE,lwd=1,alpha=0.5)+ 
    stat_smooth(show.legend = FALSE,method="lm",se=FALSE,color="blue",size=1,lwd=1,alpha=0.5)+  
    geom_abline(show.legend = FALSE,intercept=0,slope=1, color="black",size=1)+
    theme_bw() + base_theme(font.size = 12) + 
    labs(#title ="PK Base model GOF",
      caption = paste0("source:", runno, ".lst"),
      x=xpdb@Prefs@Labels$IPRED, y=xpdb@Prefs@Labels$DV)+
    theme(plot.title = element_text(size=12,face="bold"),
          plot.caption = element_text(face="italic"))
  
  # meta
  attr(fig, 'title') <- paste0("Diagnostic plot for individual predicted vs observed in linear-scale")
  attr(fig, 'width') <- 6
  attr(fig, 'height') <- 6                             
  figure[["IPRED_DVOR_LN"]] = fig 
  figure[["IPRED_DVOR_LN"]]$data =  tdata
  figure[["IPRED_DVOR_LN"]]$dataset =  slot(xpdb, "Data")  # or xpdb
   
  
  #--------------------------------------
  # iPRED vs DV log
  #--------------------------------------
  
  values = c(tdata$DV, tdata$IPRED)
  limits = range(values[is.finite(log(values))], na.rm=TRUE)
  
  fig = tdata %>%  
    filter(is.finite(log(xvar)), is.finite(log(yvar))) %>% 
    #filter(WRES!=0) %>%
    ggplot(aes(x=xvar, y=yvar))+
    geom_point(color="black",size=2, alpha=0.5)+
    coord_fixed(ratio=1, xlim = limits, ylim = limits) + 
    scale_y_log10() + scale_x_log10() + 
    #geom_smooth(show.legend = FALSE,lwd=1,alpha=0.5)+ 
    stat_smooth(show.legend = FALSE,method="lm",se=FALSE,color="blue",size=1,lwd=1,alpha=0.5)+  
    geom_abline(show.legend = FALSE,intercept=0,slope=1, color="black",size=1)+
    theme_bw() + base_theme(font.size = 12) + 
    labs(#title ="PK Base model GOF",
      caption = paste0("source:", runno, ".lst"),
      x=xpdb@Prefs@Labels$IPRED, y=xpdb@Prefs@Labels$DV)+
    theme(plot.title = element_text(size=12,face="bold"),
          plot.caption = element_text(face="italic")) + 
    
    annotation_logticks(sides ="bl")    # "trbl", for top, right, bottom, and left.
   
  # meta
  attr(fig, 'title') <- paste0("Diagnostic plot for individual predicted vs observed in log-scale")
  attr(fig, 'width') <- 6
  attr(fig, 'height') <- 6                             
  figure[["IPRED_DVOR_LOG"]] = fig 
  figure[["IPRED_DVOR_LOG"]]$data =  tdata
  figure[["IPRED_DVOR_LOG"]]$dataset =  slot(xpdb, "Data")  # or xpdb
   
  
  add_legend = FALSE
  if (add_legend) {
    p1 = p1 + 
      theme(legend.position=c(0.75, 0.2), 
            legend.title = element_blank(), 
            legend.background = element_rect(fill="white", size=0.5, linetype="solid", colour ="white"), 
            legend.key =  element_rect(colour = NA)) +  # ggplot2::element_blank() )
      guides(color=guide_legend(override.aes=list(fill=NA)))
    
    fig <- GOF1(tdata)     
    fig <- recordPlot()
    attr(fig, 'title') <- "Predicted vs Observed Concentration in Linear/Log Scale" 
    attr(fig, 'width') <- 8
    attr(fig, 'height') <- 8
    FIGURE[["PK_GOF1"]] = fig 
    
  }

  return(list(figure=figure, table=table, message=message))
}



#################################################################
# final output
#################################################################
if (ihandbook) {
  output = suppressWarnings(diagnostic_GOF1(dataset, params=NULL))
}

