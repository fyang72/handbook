
diagnostic_GOF2 <- function(dataset, params=NULL)  {
  
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
  if (!all(c("PRED", "IPRED", "CWRES", "ID", "TIME") %in% colnames(tdata0))) {
    # c("default", "message", "warning", "error"), 
    showNotification("need PRED, IPRED, CWRES, ID, and TIME in the xpdb@Data", type = "error")
  }
  
  validate(need(all(c("PRED", "IPRED", "DV") %in% colnames(tdata0)), 
                message="need PRED, IPRED, CWRES, ID, and TIME in the data")
  )
  
  #-------------------
  # fig_CWRES_TIME
  #-------------------
  tdata = tdata0%>%mutate(xvar=TIME, yvar=CWRES)
  fig <- 
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
    ylab( xpdb@Prefs@Labels$CWRES)
  
  # meta
  attr(fig, 'title') <- paste0("Conditional weighted residue vs time")
  attr(fig, 'width') <- 6
  attr(fig, 'height') <- 6                             
  figure[["CWRES_TIME"]] = fig 
  figure[["CWRES_TIME"]]$data =  tdata
  figure[["CWRES_TIME"]]$dataset = slot(xpdb, "Data")  # or xpdb
  
  
  #-------------------
  # fig_CWRES_IPRED
  #-------------------
  tdata = tdata0%>%mutate(xvar=IPRED, yvar=CWRES)
  fig <- 
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
  
  # meta
  attr(fig, 'title') <- paste0("Conditional weighted residue vs IPRED")
  attr(fig, 'width') <- 6
  attr(fig, 'height') <- 6                             
  figure[["CWRES_IPRED"]] = fig 
  figure[["CWRES_IPRED"]]$data =  tdata
  figure[["CWRES_IPRED"]]$dataset =  slot(xpdb, "Data")  # or xpdb
  
  #-------------------
  # fig_CWRES_ID
  #-------------------
  tdata = tdata0%>%mutate(xvar=ID, yvar=CWRES)
  fig <-   
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
  
  # meta
  attr(fig, 'title') <- paste0("Conditional weighted residue vs ID")
  attr(fig, 'width') <- 6
  attr(fig, 'height') <- 6                             
  figure[["CWRES_ID"]] = fig 
  figure[["CWRES_ID"]]$data =  tdata
  figure[["CWRES_ID"]]$dataset =  slot(xpdb, "Data")  # or xpdb
  
  
  
  #-------------------
  # QQ plot
  #-------------------
  qq <- qqnorm(tdata0$CWRES,plot.it = F) 
  qq <- data.frame(xvar=qq$x, 
                   yvar=qq$y
  )   
  
  fig <- 
    ggplot(qq, aes(x=xvar, y=yvar)) + 
    geom_point(size=2, alpha=0.5)+ 
    geom_abline(intercept=0,slope=1)+
    theme_bw() + base_theme(font.size = 12) + 
    theme(legend.position="none") +
    labs(caption = paste0("source:", runno, ".lst")) + 
    ylab(xpdb@Prefs@Labels$CWRES) + 
    xlab("Theoretical Quantiles")   
  
  # meta
  attr(fig, 'title') <- paste0("QQ plot of CWRES")
  attr(fig, 'width') <- 6
  attr(fig, 'height') <- 6                             
  figure[["CWRES_QQ"]] = fig 
  figure[["CWRES_QQ"]]$data =  qq
  figure[["CWRES_QQ"]]$dataset =  slot(xpdb, "Data")  # or xpdb
  
  
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
  
  return(list(figure=figure, table=table))
}



#################################################################
# final output
#################################################################
if (ihandbook) {
  output = suppressWarnings(diagnostic_GOF2(dataset, params=NULL))
}
