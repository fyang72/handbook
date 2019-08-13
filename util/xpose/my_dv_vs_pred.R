


my_dv_vs_pred <- function(xpdb) {
  
  tdata <- get_data(xpdb) #%>% mutate(xvar=PRED, yvar=DV)
  limits = range(c(tdata$DV, tdata$PRED), na.rm=TRUE)
  
  fig  = tdata   %>% 
    ##filter(WRES!=0) %>%
    ggplot(aes(x=DV, y=PRED))+
    geom_point(color="black",size=2, alpha=0.5)+
    #coord_fixed(ratio=1, xlim = limits, ylim = limits) + 
    #geom_smooth(show.legend = FALSE,lwd=1,alpha=0.5)+ 
    #stat_smooth(show.legend = FALSE,method="lm",se=FALSE,color="blue",size=1,lwd=1,alpha=0.5)+  
    #geom_abline(show.legend = FALSE,intercept=0,slope=1, color="black",size=1)+
    theme_bw() + theme_regn(font_size = 12) + 
    #labs(#title ="PK Base model GOF",
     # caption = paste0("source:", runno, ".lst"),
     # x=xpdb@Prefs@Labels$PRED, y=xpdb@Prefs@Labels$DV)+
    
    theme(plot.title = element_text(size=12,face="bold"),
          plot.caption = element_text(face="italic"))
  
  return(fig)
  
}
