
xpdb_diagnostic_GOF1 <- function(xpdb, values4xpdb)  {
  validate(need(xpdb, message=FALSE))
  
  tdata = slot(xpdb, "Data")   # or xpdb@Data
  runno = slot(xpdb, "Runno")  # or xpdb@Runno
  
  limits = range(c(tdata$DV, tdata$PRED), na.rm=TRUE)
  
  fig.PRED.DV = tdata %>%
    filter(WRES!=0) %>%
    ggplot(aes(x=PRED, y=DV))+
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
  
  values = c(tdata$DV, tdata$PRED)
  limits = range(values[is.finite(log(values))], na.rm=TRUE)
  fig.PRED.DV.LOG = tdata %>%
    filter(WRES!=0) %>%
    ggplot(aes(x=PRED, y=DV))+
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
  
  
  
  limits = range(c(tdata$DV, tdata$IPRED), na.rm=TRUE)
  fig.IPRED.DV = tdata %>%
    filter(WRES!=0) %>%
    ggplot(aes(x=IPRED, y=DV))+
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
  
  values = c(tdata$DV, tdata$IPRED)
  limits = range(values[is.finite(log(values))], na.rm=TRUE)
  
  fig.IPRED.DV.LOG = tdata %>%
    filter(WRES!=0) %>%
    ggplot(aes(x=IPRED, y=DV))+
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
  
  values4xpdb$diagnostic$PRED_DVOR = fig.PRED.DV 
  values4xpdb$diagnostic$PRED_DVOR_LOG = fig.PRED.DV.LOG
  
  values4xpdb$diagnostic$IPRED_DVOR = fig.IPRED.DV
  values4xpdb$diagnostic$IPRED_DVOR_LOG = fig.IPRED.DV.LOG
  
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
    FIGURE_ALL[["PK_GOF1"]] = fig 
    
  }
  
  
  
  return(values4xpdb)
}
