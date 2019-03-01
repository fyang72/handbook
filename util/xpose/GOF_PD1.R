
#-----------------------------------------------
# GOF_PD1
#-----------------------------------------------
GOF <- function(tdata) { 
  tdata = tdata %>% mutate(PRED=as_numeric(PRED), 
                           IPRED=as_numeric(IPRED), 
                           DVOR=as_numeric(DVOR)
  )
  
  DVOR_vs_PRED_LN= ggplot(tdata, aes(x=PRED, y=DVOR, col=ARMA)) + geom_point() + 
    geom_abline(show.legend = FALSE,intercept=0,slope=1,lty="dashed") + coord_fixed(ratio = 1) + 
    facet_wrap(~ARMA) + base_theme() + 
    theme(legend.position='none', 
          panel.grid.minor= element_line(colour = "gray95",size=0.5),
          panel.grid.major= element_line(colour = "gray95",size=0.5),
          panel.spacing = unit(0.2, "lines"), 
          panel.border=element_rect(colour="black", fill=NA), 
          strip.background = element_blank()   # element_rect(colour=NA, fill=NA),
    ) 
  
  
  DVOR_vs_PRED_LOG = DVOR_vs_PRED_LN + scale_x_log10() +  scale_y_log10()   # + 
  #coord_fixed(ratio = 1)
  
  
  DVOR_vs_IPRED_LN= ggplot(tdata, aes(x=IPRED, y=DVOR, col=ARMA)) + geom_point() + 
    geom_abline(show.legend = FALSE,intercept=0,slope=1,lty="dashed") + coord_fixed(ratio = 1) + 
    facet_wrap(~ARMA) + base_theme() + 
    theme(legend.position='none', 
          panel.grid.minor= element_line(colour = "gray95",size=0.5),
          panel.grid.major= element_line(colour = "gray95",size=0.5),
          panel.spacing = unit(0.2, "lines"), 
          panel.border=element_rect(colour="black", fill=NA), 
          strip.background = element_blank()   # element_rect(colour=NA, fill=NA),
    ) 
  
  
  DVOR_vs_IPRED_LOG = DVOR_vs_IPRED_LN + scale_x_log10() +  scale_y_log10()   #+ 
  # coord_fixed(ratio = 1)
  
  
  
  #FIGURE_ALL[[paste0(runno, "_CWRES_vs_")]] =
  #  GOF2(tdata0, MDV="MDV", IPRED="IPRED", PRED="PRED", DV="DV")       ## Goodness of PLot
  # fig1 = recordPlot()  
  
  return(list(DVOR_vs_PRED_LN=DVOR_vs_PRED_LN, 
              DVOR_vs_PRED_LOG=DVOR_vs_PRED_LOG, 
              DVOR_vs_IPRED_LN=DVOR_vs_IPRED_LN, 
              DVOR_vs_IPRED_LN=DVOR_vs_IPRED_LN
  )) 
}

