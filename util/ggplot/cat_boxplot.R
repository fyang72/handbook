
cat_boxplot <- function(tdata, xvar, yvar, theTitle=NULL)  {     
  
  #if (is.null(theTitle)) { theTitle = paste0("Impact of ", xvar, " on ", yvar)}
  
  fig = ggplot(data = tdata, aes_string(x = xvar, y = yvar, fill=xvar )) +
    geom_boxplot(position = position_dodge(width=0.9)) + #  outlier.size=outlier.size, outlier.color=outlier.color) +                     #space between boxplots
    xlab("" ) + 
    ggtitle(theTitle ) +                                                      #add title
    #scale_y_continuous(ylab.txt ) +       #label y axis and set limtis to allow space to plot summary stats beneath plots
    # scale_x_discrete("") +                    #label x axis
    #scale_color_manual(name="Population", values=c("black","black"), labels=c("Japanese", "Non-Japanese")) +   
    # scale_fill_manual(name="Population", values=c("cyan","green"), labels=c("Japanese", "Non-Japanese"))  + 
    base_theme(font.size=10, legend_position= "none") + 
    theme(#legend.title=element_text("EXSEQ"), 
      legend.position="none", 
      panel.grid.minor= element_line(colour = "gray95",size=0.5),
      panel.grid.major= element_line(colour = "gray95",size=0.5),
      panel.spacing = unit(0.2, "lines"), 
      panel.border=element_rect(colour="black", fill=NA), 
      strip.background = element_blank()   # element_rect(colour=NA, fill=NA),
    )   
  
  fig = fig + theme(axis.text.x = element_text(angle = 30, hjust = 1))  
  
  fig  
}

