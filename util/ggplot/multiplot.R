#' Extracts the time matched concntration vs effect data
#'
#' @param data A data frame.  The data frame must conform to Regeneron standards and contain the variables: `week`, `ndy`, `concn`, `concni`, `pchg`, `actarmcd`, `ada`
#' @return A dataframe with columns `ndy`, `concn`, `effect`.
#' @export
#' @importFrom dplyr left_join group_by summarise
#' @examples
#'   inFile <- system.file("extdata/pk_pd_hysteresis.xls", package = "pkGraph")
#'   library(readxl)
#'   theData <-read_excel(inFile)
#'   timeMatchedPK(data = theData) 
multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
#   
# https://stackoverflow.com/questions/29583849/save-a-plot-in-an-object 
  # Save a plot in an object
#   You could use
#   
#   1- recordPlot
#   
#   2- the recently introduced gridGraphics package, to convert base graphics to their grid equivalent
#   
#   Here's a minimal example,
# 
# plot(1:10) 
# 
# p <- recordPlot()
# plot.new() ## clean up device
# p # redraw
# 
# ## grab the scene as a grid object
# library(gridGraphics)
# library(grid)
# grid.echo()
# a <- grid.grab()
# 
# ## draw it, changes optional
# grid.newpage()
# a <- editGrob(a, vp=viewport(width=unit(2,"in")), gp=gpar(fontsize=10))
# grid.draw(a)
# 

  require(grid)
  
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots == 1) {
    print(plots[[1]])
    
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



#' 
#' 
#' ################################################################################
#' # 
#' #  Use ggplot as the way to plot
#' #
#' ################################################################################
#' 
#' #' Extracts the time matched concntration vs effect data
#' #'
#' #' @param data A data frame.  The data frame must conform to Regeneron standards and contain the variables: `week`, `ndy`, `concn`, `concni`, `pchg`, `actarmcd`, `ada`
#' #' @return A dataframe with columns `ndy`, `concn`, `effect`.
#' #' @export
#' #' @importFrom dplyr left_join group_by summarise
#' #' @examples
#' #'   inFile <- system.file("extdata/pk_pd_hysteresis.xls", package = "pkGraph")
#' #'   library(readxl)
#' #'   theData <-read_excel(inFile)
#' #'   timeMatchedPK(data = theData) 
#' multiplot <- function(..., plotlist=NULL, cols=2) {
#'   ##    http://wiki.stdout.org/rcookbook/Graphs/Multiple%20graphs%20on%20one%20page%20%28ggplot2%29/
#'   require(grid)
#'   
#'   # Make a list from the ... arguments and plotlist
#'   plots <- c(list(...), plotlist)
#'   
#'   numPlots = length(plots)
#'   
#'   # Make the panel
#'   plotCols = cols                          # Number of columns of plots
#'   plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols
#'   
#'   # Set up the page
#'   grid.newpage()
#'   pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
#'   vplayout <- function(x, y)
#'     viewport(layout.pos.row = x, layout.pos.col = y)
#'   
#'   # Make each plot, in the correct location
#'   for (i in 1:numPlots) {
#'     curRow = ceiling(i/plotCols)
#'     curCol = (i-1) %% plotCols + 1
#'     print(plots[[i]], vp = vplayout(curRow, curCol ))
#'   }
#'   
#' }



