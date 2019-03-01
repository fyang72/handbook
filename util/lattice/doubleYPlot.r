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
doubleYPlot <- function(data, adjust = FALSE, drug = "Drug") {
  
  # extract the columns required
  data <- data %>%
    select_("PKCONCN", "DVOR", "NTIM", "ARMA")
  
  data$PKCONCN = as_numeric(data$PKCONCN)
  data$DVOR = as_numeric(data$DVOR)
  data$NTIM = as_numeric(data$NTIM)
    
  colours <- colScheme()  # gg_color_hue(4)  #For consistancy, gg_colour_hue function(find in helpers file) used to set colours to match the default ggplot red and blue colours
  
  # Extract the SEs from the data
  upperLower <- data %>%
    group_by(NTIM, ARMA) %>%
    summarise(
      medianP = median(DVOR),                            #median for DVOR
      lowerP = medianP - sqrt(var(DVOR) / length(DVOR)), #SE for DVOR
      upperP = medianP + sqrt(var(DVOR) / length(DVOR)),
      medianC = median(PKCONCN),                             #median for PKCONCN
      lowerC = medianC - sqrt(var(PKCONCN) / length(PKCONCN)), #SE for PKCONCN
      upperC = medianC + sqrt(var(PKCONCN) / length(PKCONCN))
    )
  
  #assign the data to be plotted, the title and the x-axis label
  output <- ggplot(data = upperLower) +
    ggtitle(paste0("Mean (+/- SE) Percentage Change from Baseline\n and Mean (+/- SE) Concentration (mg/L) of Functional\n ", drug,
                   " in Serum by Nominal Day")) +
    xlab("Nominal Day") #assign the x-axis label
  
  if(adjust){   # Produces plot with error bars adjusted for readability
    #produce the first plot
    p1 <-  output +
      geom_line(mapping = aes_string("NTIM", "medianP", group = "ARMA"), colour = colours[3]) +
      geom_errorbar(mapping= aes_string(x= "NTIM" ,ymin= "lowerP", ymax= "upperP"), colour = colours[3], position = position_dodge(width = 1), width=2) +
      ylab("% Change from Baseline") +
      theme(axis.title.y = element_text(colour = colours[3])) #colours the first x-axis label blue
    
    #produce the second plot
    p2 <-  output +
      geom_line(mapping= aes_string("NTIM", "medianC", group = "ARMA"), colour = colours[1]) +
      theme(panel.background = element_rect(fill = NA)) +
      geom_errorbar(mapping= aes_string(x= "NTIM" ,ymin= "lowerC", ymax= "upperC"), colour =colours[1], position = position_dodge(width = 1), width=2) +
      ylab("Concentration (mg/L)") +
      theme(axis.title.y = element_text(colour = colours[1])) +                         #colours the second y-axis label red
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())     #remove the grid lines from the second plot to avoid duplication
    
  } else { #Produces plots without adjusted errorbars
    p1 <-  output +
      geom_line(aes_string("NTIM", "medianP", group = "ARMA"), colour = colours[3]) +
      geom_errorbar(mapping= aes_string(x= "NTIM" ,ymin= "lowerP", ymax= "upperP"), colour =colours[3], width= 2) +
      ylab("% Change from Baseline") +
      theme(axis.title.y = element_text(colour = colours[3])) #colours the first x-axis label blue
    
    p2 <-  output +
      geom_line(aes_string("NTIM", "medianC", group = "ARMA"), colour = colours[1]) +
      theme(panel.background = element_rect(fill = NA)) +
      geom_errorbar(mapping= aes_string(x= "NTIM" ,ymin= "lowerC", ymax= "upperC"), colour = colours[1], width= 2) +
      ylab("Concentration (mg/L)") +
      theme(axis.title.y = element_text(colour = colours[1])) + #colours the second y-axis label red
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    
  }
  
  # The following code takes the plots produced above and overlays them to produce the double y-axis plot
  # extract gtable (contains information about each plot in the form of an object)
  g1 <- ggplot_gtable(ggplot_build(p1))
  g2 <- ggplot_gtable(ggplot_build(p2))
  
  # overlap the panel of 2nd plot on that of 1st plot
  pp <- c(subset(g1$layout, name == "panel", se = t:r))
  g <-  gtable::gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t,
                                pp$l, pp$b, pp$l)
  
  # axis tweaks
  ia <- which(g2$layout$name == "axis-l")
  ga <- g2$grobs[[ia]]
  ax <- ga$children[[2]]
  ax$widths <- rev(ax$widths)
  ax$grobs <- rev(ax$grobs)
  ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
  g <- gtable::gtable_add_cols(g, g2$widths[g2$layout[ia,]$l], length(g$widths) - 1)
  g <- gtable::gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
  
  #adds the second y-axis label
  ia2 <- which(g2$layout$name == "ylab")
  ga2 <- g2$grobs[[ia2]]
  ga2$rot <- 90
  g <- gtable::gtable_add_cols(g, g2$widths[g2$layout[ia2, ]$l], length(g$widths) - 1)
  g <- gtable::gtable_add_grob(g, ga2, pp$t, length(g$widths) - 1, pp$b)
  
  # draw plot
  return(g)
  
  
  
  
# http://stackoverflow.com/questions/13875637/changing-the-y-axis-text-size-with-doubleyscale-plot
if (1==2) {
library(ggplot2)
library(reshape)

set.seed(123)
foo <- list(x = 1:100, y = cumsum(rnorm(100)))

foo <- as.data.frame(foo)
foo$z <- foo$y^2
mymelt <- melt(foo, id.var = 'x')
mymelt$label <- ifelse(mymelt$variable == 'y', "Produktion", "Summa.skulder")
mymelt$line.colour <- ifelse(mymelt$variable == 'y', "red", "blue") # specify colours here


ggplot(data = mymelt, aes(x = x, y = value)) +
    geom_line(aes(colour = mymelt$line.colour)) +
    facet_wrap(~ label, ncol = 1, scales = "free_y") +
    scale_colour_manual(values = unique(mymelt$line.colour)) +
    ggtitle("TOtalProduktion VS SummaSkulder/TotaltKapital i procent") +
    theme(strip.text.x = element_text(size = 12)) +
    theme(axis.text.x = element_text(size = 9)) +
    theme(axis.text.y = element_text(size = 9)) +
    theme(axis.title.x = element_text(size = 15)) +
    theme(axis.title.y = element_text(size = 15)) +
    theme(axis.title.x = element_blank()) + # comment out this line if you want an x axis title
    theme(axis.title.y = element_blank()) + # comment out this line if you want a y axis title
    theme(legend.position = "none")
    
}





  
  
}



    
    
    
 
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
  doubleYPlot2 <- function(p1, p2) {
 
  # The following code takes the plots produced above and overlays them to produce the double y-axis plot
  # extract gtable (contains information about each plot in the form of an object)
  g1 <- ggplot_gtable(ggplot_build(p1))
  g2 <- ggplot_gtable(ggplot_build(p2))
  
  # overlap the panel of 2nd plot on that of 1st plot
  pp <- c(subset(g1$layout, name == "panel", se = t:r))
  g <-  gtable::gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t,
                                pp$l, pp$b, pp$l)
  
  # axis tweaks
  ia <- which(g2$layout$name == "axis-l")
  ga <- g2$grobs[[ia]]
  ax <- ga$children[[2]]
  ax$widths <- rev(ax$widths)
  ax$grobs <- rev(ax$grobs)
  ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
  g <- gtable::gtable_add_cols(g, g2$widths[g2$layout[ia,]$l], length(g$widths) - 1)
  g <- gtable::gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
  
  #adds the second y-axis label
  ia2 <- which(g2$layout$name == "ylab-l")
  ga2 <- g2$grobs[[ia2]]
  ga2$rot <- 90
  g <- gtable::gtable_add_cols(g, g2$widths[g2$layout[ia2, ]$l], length(g$widths) - 1)
  g <- gtable::gtable_add_grob(g, ga2, pp$t, length(g$widths) - 1, pp$b)
  
  return(g)
  }
  
  
  
     