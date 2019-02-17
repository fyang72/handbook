
xpdb_diagnostic_GOF2 <- function(xpdb, values4xpdb)  {
  validate(need(xpdb, message=FALSE))
  
  tdata = slot(xpdb, "Data")   #%>% mutate(DV=CP) # or xpdb@Data
  runno = slot(xpdb, "Runno")  # or xpdb@Runno
  
   
  require('ggplot2')
  x = tdata %>% filter(MDV==0)
   
  fig_CWRES_TIME <- 
    #ggplot(x[x$MDV==0,],aes(TIME,IWRES,label=ID))+
    ggplot(x,aes(x=TIME,y=CWRES))+   #,label=ID
    geom_point(size=2, alpha=0.5) + 
    geom_hline(yintercept=c(0),col="red") + 
    geom_hline(yintercept=c(-4,4),lwd=0.5,lty="dashed") + 
    geom_hline(yintercept=c(-6,6),lwd=0.5,lty="solid") + 
    #geom_text(data=x[abs(x$CWRES)>1,],aes(size=abs(CWRES)),hjust=0, vjust=0)+
    geom_smooth(lwd=1,alpha=0.5)+
    theme_bw() + base_theme(font.size = 12) + 
    theme(legend.position="none") +
    labs(caption = paste0("source:", runno, ".lst")) + 
    xlab(xpdb@Prefs@Labels$TIME) + 
    ylab( xpdb@Prefs@Labels$CWRES)
  
  fig_CWRES_IPRED <- 
    ggplot(x[x$MDV==0,],aes(IPRED,CWRES))+  #,label=ID
    geom_point(size=2, alpha=0.5) + 
    geom_hline(yintercept=c(0),col="red") + 
    geom_hline(yintercept=c(-4,4),lwd=0.5,lty="dashed") + 
    geom_hline(yintercept=c(-6,6),lwd=0.5,lty="solid") +  
    #geom_text(data=x[abs(x$CWRES)>1,],aes(size=abs(CWRES)),hjust=0, vjust=0)+
    geom_smooth(lwd=1,alpha=0.5)+
    theme_bw() + base_theme(font.size = 12) + 
    theme(legend.position="none") +
    labs(caption = paste0("source:", runno, ".lst")) + 
    ylab( xpdb@Prefs@Labels$CWRES)+ 
    xlab(xpdb@Prefs@Labels$IPRED)
  
  fig_CWRES_ID <-   
    #ggplot(x[x$MDV==0,],aes(TIME,IWRES,label=ID))+
    ggplot(x,aes(ID,CWRES))+   #,label=ID
    geom_point(size=2, alpha=0.5) + 
    geom_hline(yintercept=c(0),col="red") + 
    geom_hline(yintercept=c(-4,4),lwd=0.5,lty="dashed") + 
    geom_hline(yintercept=c(-6,6),lwd=0.5,lty="solid") + 
    #geom_text(data=x[abs(x$CWRES)>1,],aes(size=abs(CWRES)),hjust=0, vjust=0)+
    geom_smooth(lwd=1,alpha=0.5)+
    theme_bw() + base_theme(font.size = 12) + 
    theme(legend.position="none") +
    labs(caption = paste0("source:", runno, ".lst")) + 
    xlab(xpdb@Prefs@Labels$ID) + 
    ylab(xpdb@Prefs@Labels$CWRES)
  
  qq <- qqnorm(x$CWRES[x$MDV==0],plot.it = F)
  
  fig_CWRES_QUANTILE <- 
    ggplot(data.frame(x=qq$x,y=qq$y), aes(x, y)) + 
    geom_point(size=2, alpha=0.5)+ 
    geom_abline(intercept=0,slope=1)+
    theme_bw() + base_theme(font.size = 12) + 
    theme(legend.position="none") +
    labs(caption = paste0("source:", runno, ".lst")) + 
    ylab(xpdb@Prefs@Labels$CWRES) + 
    xlab("Theoretical Quantiles")   
  
  #fig_CWRES_TIME   fig_CWRES_IPRED    fig_CWRES_ID   fig_CWRES_QUANTILE
  # Note: Solid red line represents CWRES = 0. Solid black lines represent CWRES = 4.  The dotted lines represent CWRES = 6.
  values4xpdb$diagnostic$CWRES.TIME = fig_CWRES_TIME 
  values4xpdb$diagnostic$CWRES.IPRED = fig_CWRES_IPRED
  
  values4xpdb$diagnostic$CWRES.ID = fig_CWRES_ID
  values4xpdb$diagnostic$CWRES.QUANTILE = fig_CWRES_QUANTILE
  
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
