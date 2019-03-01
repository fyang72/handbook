



ggpaircor <- function(mydata, method="spearman", use = "complete.obs")  {
    cormat <- round(cor(mydata, method=method, use=use),2)
    
    library(reshape2)
    melted_cormat <- melt(cormat)
    
    # Get lower triangle of the correlation matrix
    get_lower_tri<-function(cormat){
      cormat[upper.tri(cormat)] <- NA
      return(cormat)
    }
    # Get upper triangle of the correlation matrix
    get_upper_tri <- function(cormat){
      cormat[lower.tri(cormat)]<- NA
      return(cormat)
    }
    
    
    
    # Melt the correlation matrix
    library(reshape2)
    melted_cormat <- melt(upper_tri, na.rm = TRUE)
     
    
    reorder_cormat <- function(cormat){
      # Use correlation between variables as distance
      dd <- as.dist((1-cormat)/2)
      hc <- hclust(dd)
      cormat <-cormat[hc$order, hc$order]
    }
    
    
    # Reorder the correlation matrix
    cormat <- reorder_cormat(cormat)
    upper_tri <- get_upper_tri(cormat)
    # Melt the correlation matrix
    melted_cormat <- melt(upper_tri, na.rm = TRUE)
    # Create a ggheatmap
    ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
      geom_tile(color = "white")+
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                           midpoint = 0, limit = c(-1,1), space = "Lab", 
                           name=paste0(method, "\ncorrelation")) +
      theme_minimal()+ # minimal theme
      theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                       size = 12, hjust = 1))+
      coord_fixed()
    
    
    
    fig = ggheatmap + 
      geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.6, 0.7),
        legend.direction = "horizontal")+
      guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                   title.position = "top", title.hjust = 0.5))


     return(fig)
}


# 
# mydata <- mtcars[, c(1,3,4,5,6,7)]
# mydata <- OUT %>% ungroup() %>% select(one_of(c("Cat.Dander.IgE" ,    "Cat.Dander.IgE.BL",
#                                                 "rFel.d.1.Cat.IgE",    "rFel.d.1.Cat.IgE.BL",
#                                                 "rFel.d.2.Cat.IgE",  "rFel.d.2.Cat.IgE.BL") ))
# 
# 
# ggpaircor(mydata, method="spearman", use = "complete.obs")
