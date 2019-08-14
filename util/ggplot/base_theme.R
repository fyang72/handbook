
#' Creates a Concentration vs Time graph
#'
#' Creates a Concentration vs Time graph using Regeneron data.
#'
#' @param data A data frame with which to create the plots.  The data frame must conform to Regeneron standards and contain the variables: `week`, `ndy`, `concn`, `concni`, `pchg`, `actarmcd`, `ada`
#' @param drug Drug name
#' @param log Should the log axis be used?
#' @param adjust Should an adjustment be applied to the errorbars?
#' @return A ggplot graphic.
#' @import gridExtra
#' @export
#' @examples
#'   inFile <- system.file("extdata/pk_pd_ada_all3.sas7bdat", package = "pkGraph")
#'   library(haven)
#'   theData <- read_sas(inFile)
#'   theData <- dataCheck(theData)
#'   concTime(data = theData)
#'   concTime(data = theData, adjust = TRUE)
base_theme <- function (font.size = 10,
                        plot_title=font.size+2, legend_text = font.size, legend_title = font.size,
                        axis_title=font.size, axis_title_x = axis_title, axis_title_y = axis_title, 
                        axis_text=font.size, axis_text_x = axis_text, axis_text_y = axis_text, 
                        strip_text = font.size, strip_text_x = strip_text, strip_text_y = strip_text, 
                        legend_position= "bottom",  legend_box = "horizontal",
                        panel.background = ggplot2::element_blank() 
)  {
  # font face ("plain", "italic", "bold", "bold.italic")
  
  ggplot2::theme(    
    plot.title = ggplot2::element_text(hjust = 0.5, color="black", size = plot_title, face = "bold") ,
    
    legend.text = ggplot2::element_text(size = legend_text), 
    legend.title = element_blank(),  # ggplot2::element_text(size = legend_title),     # Remove only the legend title by set legend.title = element_blank()
    legend.key =  ggplot2::element_blank() , 
    # left,top, right, bottom.
    legend.position = legend_position,       # legend.position='none', Remove the plot legend   c(0,0) corresponds to the "bottom left" and c(1,1) corresponds to the "top right" position.
    #legend.justification = c(1, 1),     ??
    legend.box = legend_box ,  # Horizontal legend box  
    legend.background = element_rect(fill="white", size=0.5, linetype="solid", colour ="white"),
    
    
    axis.title   = ggplot2::element_text(size = axis_title, face="bold"),        
    axis.title.x = ggplot2::element_text(size = axis_title_x, face = "bold"), 
    axis.title.y = ggplot2::element_text(size = axis_title_y, face = "bold"), 
    
    axis.text   = ggplot2::element_text(color = "black", size = axis_text), #axis tick text   
    axis.text.x = ggplot2::element_text(color = "black", size = axis_text_x), 
    axis.text.y = ggplot2::element_text(color = "black", size = axis_text_y), 
    
    axis.line = ggplot2::element_line(color='black'), 
    
    strip.text = ggplot2::element_text(color = "black", size = strip_text, face = "bold"),     # strip text in facetted plots.
    strip.text.x = ggplot2::element_text(color = "black", size = strip_text_x, face = "bold"), 
    strip.text.y = ggplot2::element_text(color = "black", size = strip_text_y, face = "bold"),
    
    panel.background = panel.background,
    #panel.grid = ggplot2::element_blank(), 
    panel.grid.minor = ggplot2::element_blank(),   # element_line(colour = "gray98",size=0.5),   # 
    panel.grid.major = element_line(colour = "gray97",size=0.75),  #ggplot2::element_blank()   # element_line(colour = "gray90",size=0.75))      
    
    panel.border = element_rect(colour = "black", fill=NA, size=1), 
    panel.spacing = unit(0.2, "lines"),
    strip.background = element_rect(colour="transparent", fill="white")
  )
  
}


# https://stackoverflow.com/questions/35108443/ggplot2-make-legend-key-fill-transparent/35108774
# plot + guides(color=guide_legend(override.aes=list(fill=NA)))
# 
# 
# 
# 
# This answer seems to be the simplest solution, setting legend.key = element_blank() in the theme() definition.
# 
#  
# Additionally to legend.key = element_blank() you can put legend.background=element_blank() within theme(), to make the text transparent as well
# 


#' @import gridExtra
#' @export
#' @examples
#'   inFile <- system.file("extdata/pk_pd_ada_all3.sas7bdat", package = "pkGraph")
#'   library(haven)
#'   theData <- read_sas(inFile)
#'   theData <- dataCheck(theData)
#'   concTime(data = theData)
#'   concTime(data = theData, adjust = TRUE)
theme_regn <- function (font_size = 10,
                        plot_title=font_size+2, legend_text = font_size, legend_title = font_size,
                        axis_title=font_size, axis_title_x = axis_title, axis_title_y = axis_title, 
                        axis_text=font_size, axis_text_x = axis_text, axis_text_y = axis_text, 
                        strip_text = font_size, strip_text_x = strip_text, strip_text_y = strip_text, 
                        legend_position= "bottom",  legend_box = "horizontal",
                        panel.background = ggplot2::element_blank() 
)  {
  # font face ("plain", "italic", "bold", "bold.italic")
  
  ggplot2::theme(    
    plot.title = ggplot2::element_text(hjust = 0.5, color="black", size = plot_title, face = "bold") ,
    
    legend.text = ggplot2::element_text(size = legend_text), 
    legend.title = element_blank(),  # ggplot2::element_text(size = legend_title),     # Remove only the legend title by set legend.title = element_blank()
    legend.key =  ggplot2::element_blank() , 
    # left,top, right, bottom.
    legend.position = legend_position,       # legend.position='none', Remove the plot legend   c(0,0) corresponds to the "bottom left" and c(1,1) corresponds to the "top right" position.
    #legend.justification = c(1, 1),     ??
    legend.box = legend_box ,  # Horizontal legend box  
    legend.background = element_rect(fill="white", size=0.5, linetype="solid", colour ="white"),
    
    
    axis.title   = ggplot2::element_text(size = axis_title, face="bold"),        
    axis.title.x = ggplot2::element_text(size = axis_title_x, face = "bold"), 
    axis.title.y = ggplot2::element_text(size = axis_title_y, face = "bold"), 
    
    axis.text   = ggplot2::element_text(color = "black", size = axis_text), #axis tick text   
    axis.text.x = ggplot2::element_text(color = "black", size = axis_text_x), 
    axis.text.y = ggplot2::element_text(color = "black", size = axis_text_y), 
    
    axis.line = ggplot2::element_line(color='black'), 
    
    strip.text = ggplot2::element_text(color = "black", size = strip_text, face = "bold"),     # strip text in facetted plots.
    strip.text.x = ggplot2::element_text(color = "black", size = strip_text_x, face = "bold"), 
    strip.text.y = ggplot2::element_text(color = "black", size = strip_text_y, face = "bold"),
    
    panel.background = panel.background,
    #panel.grid = ggplot2::element_blank(), 
    panel.grid.minor = ggplot2::element_blank(),   # element_line(colour = "gray98",size=0.5),   # 
    panel.grid.major = element_line(colour = "gray97",size=0.75),  #ggplot2::element_blank()   # element_line(colour = "gray90",size=0.75))      
    
    panel.border = element_rect(colour = "black", fill=NA, size=1), 
    panel.spacing = unit(0.2, "lines"),
    strip.background = element_rect(colour="transparent", fill="white")
  )
  
}


