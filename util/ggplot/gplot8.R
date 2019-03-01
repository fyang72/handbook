


ggplot8 <- function(data) {
# diagnostic plot
fig = ggplot(tdata %>% filter(TESTN==1), aes(x=PRED, y=DVOR, col=ARMA)) + 
  geom_point() +  
  geom_abline(slope=1, lty="dashed") + 
  
  #coord_cartesian(xlim=c(0, 120), ylim=c(0.8, 1E+3)) + 
  
  #scale_y_continuous(labels=fancy_scientific)  + 
  #scale_x_continuous(breaks=x$xbreaks, label=x$xlabels) +
  
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  
  
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^round(x, digits=0)),
                labels =   trans_format("log10", math_format(10^.x))
  ) +
  
  xlab("Population Predicted Concentration of Total REGN3918 (mg/L)") + 
  ylab("Observed Concentration of Total REGN3918 (mg/L)") + 
  
  #scale_colour_manual(values = c("black", "blue", "green")) + 
  
  
  theme_bw() +  base_theme() + #font.size = as.integer(12)) + 
  guides(col=guide_legend(ncol=4,byrow=TRUE)) +  
  
  
  fig = fig + annotation_logticks(sides ="bl")    # "trbl", for top, right, bottom, and left.
  FIGURE_ALL[["PRED_DVOR_LN"]] = fig
  
   }

  