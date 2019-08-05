
xpdb_diagnostic_GOF2 <- function(xpdb, values4xpdb)  {
  validate(need(xpdb, message=FALSE))
  
  tdata = slot(xpdb, "Data")   #%>% mutate(DV=CP) # or xpdb@Data
  runno = slot(xpdb, "Runno")  # or xpdb@Runno
  
  
  require('ggplot2')
  tdata0 = tdata %>% filter(MDV==0)
  
  
  #-------------------
  # fig_CWRES_TIME
  #-------------------
  tdata = tdata0%>%mutate(xvar=TIME, yvar=CWRES)
  fig_CWRES_TIME <- 
    #ggplot(x[x$MDV==0,],aes(TIME,IWRES,label=ID))+
    ggplot(tdata, aes(x=xvar,y=yvar))+   #,label=ID
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
    ylab(xpdb@Prefs@Labels$CWRES)
  
  values4xpdb$diagnostic$CWRES.TIME$fig = fig_CWRES_TIME 
  values4xpdb$diagnostic$CWRES.TIME$data = tdata
  
  #-------------------
  # fig_CWRES_IPRED
  #-------------------
  tdata = tdata0%>%mutate(xvar=IPRED, yvar=CWRES)
  fig_CWRES_IPRED <- 
    ggplot(tdata,aes(x=xvar,y=yvar))+  #,label=ID
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
  
  values4xpdb$diagnostic$CWRES.IPRED$fig = fig_CWRES_IPRED
  values4xpdb$diagnostic$CWRES.IPRED$data = tdata
  
  #-------------------
  # fig_CWRES_ID
  #-------------------
  tdata = tdata0%>%mutate(xvar=ID, yvar=CWRES)
  fig_CWRES_ID <-   
    #ggplot(x[x$MDV==0,],aes(TIME,IWRES,label=ID))+
    ggplot(tdata,aes(x=xvar,y=yvar))+   #,label=ID
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
  
  values4xpdb$diagnostic$CWRES.ID$fig = fig_CWRES_ID
  values4xpdb$diagnostic$CWRES.ID$data = tdata
  
  #-------------------
  # QQ plot
  #-------------------
  qq <- qqnorm(tdata0$CWRES,plot.it = F) 
  qq <- data.frame(xvar=qq$x, 
                   yvar=qq$y
  )   
  
  fig_CWRES_QUANTILE <- 
    ggplot(qq, aes(x=xvar, y=yvar)) + 
    geom_point(size=2, alpha=0.5)+ 
    geom_abline(intercept=0,slope=1)+
    theme_bw() + base_theme(font.size = 12) + 
    theme(legend.position="none") +
    labs(caption = paste0("source:", runno, ".lst")) + 
    ylab(xpdb@Prefs@Labels$CWRES) + 
    xlab("Theoretical Quantiles")   
  
  values4xpdb$diagnostic$CWRES.QUANTILE$fig = fig_CWRES_QUANTILE
  values4xpdb$diagnostic$CWRES.QUANTILE$data = qq
  
  #fig_CWRES_TIME   fig_CWRES_IPRED    fig_CWRES_ID   fig_CWRES_QUANTILE
  # Note: Solid red line represents CWRES = 0. Solid black lines represent CWRES = 4.  The dotted lines represent CWRES = 6.
  
  
  
  
  
  
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
