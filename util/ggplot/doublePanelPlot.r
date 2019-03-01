#' Double panel plot
#'
#' Creates a double panel plot with a common X axise using Regeneron data.
#'
#' @param plot1 First plot
#' @param plot2 Second plot
#' @param title Plot title
#' @return A graphics object.
#' @import gridExtra
#' @import ggplot2
#' @export


# give me warning message. # #' 
# #' Warning messages:
# 1: In grepl("\n", lines, fixed = TRUE) :
#   input string 48 is invalid in this locale
# 2: In grepl("\n", lines, fixed = TRUE) :
#   input string 53 is invalid in this locale
# 
# 
# doublePanelPlot <- function (plot1, plot2, title = ""){
# 
#   plot1 <- plot1+
#     theme(axis.title.x = element_blank(),
#           axis.text.x = element_blank(),
#           plot.margin = unit(c(1, 1, -0.1, 1), "cm")) +
#     ggtitle(title)
# 
#   plot2 <- plot2 +
#     theme(plot.margin = unit(c(0, 1, 1, 1), "cm"))
# 
#   #specify grobs
# 
#   gA <- ggplotGrob(plot1)
#   gB <- ggplotGrob(plot2)
# 
#   #set Grob widths
#   maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
#   gA$widths[2:5] <- as.list(maxWidth)
#   gB$widths[2:5] <- as.list(maxWidth)
#   #arrange the grobs within grid
#   #output <- grid.arrange(gA, gB, newpage = FALSE, ncol=1, heights = c(9/10, 9/10))
#   output <- arrangeGrob(gA, gB, ncol=1, heights = c(9/10, 9/10))
# 
#   output
# }



#
# data <- data.frame(x = seq(0, 100, 1))
# #
# data = transform(data,
#                  y1 = sin(x * pi / 10),
#                  y2 = x**2
#                  )
##Then we?ll import a couple of libraries: ggplot2, of course, and gridExtra which will give us the stacking functionality.
#
# library(ggplot2)
# library(gridExtra)
#
##We?ll generate the two plots.
#
# p1 <- ggplot(data, aes(x = x)) + geom_line(aes(y = y1)) + theme_classic()
# p2 <- ggplot(data, aes(x = x)) + geom_bar(aes(y = y2), stat = "identity") + theme_classic()
#
#
#p1 <- ggplot_gtable(ggplot_build(p1))
# p2 <- ggplot_gtable(ggplot_build(p2))
#
#
# maxWidth = unit.pmax(p1$widths[2:3], p2$widths[2:3])
# 
# p1$widths[2:3] <- maxWidth
# p2$widths[2:3] <- maxWidth
#
#  grid.arrange(p1, p2, heights = c(3, 2))
#  
#  
#  
#
