
add_formula_pvalue <- function(fig) {
  # add formula and p-value on top of scatter plot
    #
  # p = ggplot(tdata, aes(x= PK, y=PD, col=as.factor(TIMEPT))) + geom_point() + #scale_x_log2()
  #   ggtitle(title.txt) + 
  #   facet_wrap(~TIMEPT) + 
  #   #coord_cartesian(ylim=c(0, 25))
  #   scale_x_continuous(trans='log10')   + 
  #   geom_smooth(method = "mysmooth", size = 1)   
  # 
  
  formula <- y ~ x
  
  library(ggpmisc)
  fig = fig + stat_poly_eq(formula = y ~ x,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                         parse=TRUE,label.x.npc = "right") + 
    
    stat_fit_glance(method = 'lm', method.args = list(formula = formula),
                    geom = 'text', aes(label = paste("P-value = ", 
                                                     signif(..p.value.., digits = 4), sep = "")),label.x.npc = 'right',
                    label.y.npc = 0.35, size = 3) + 
     
  

return(fig)

}




