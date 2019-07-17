

notyet <- function() {
cormat <- round(cor(tdata),2)
head(cormat) 

upper_tri <- get_upper_tri(cormat)
melted_cormat <- melt(upper_tri, na.rm = TRUE)





tdata = tdata %>% gather(TEST,DVOR, -USUBJID)

ggplot( tdata, aes(USUBJID, TEST) ) +
  geom_tile(aes(fill = DVOR)) +
  scale_fill_gradient() # low=muted("blue"), high=muted("red"))









get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}


tdata = OUT84 %>% select(#USUBJID, 
  REGN1908, REGN1909, REGN1908_1909, 
  AUC_1908,   AUC_1909,  AUC_1908_1909, 
  TNSS_AUC01, VAS_AUC01, PNIF_AUC01, 
  Cat.Dander.IgE.BL, rFel.d.1.Cat.IgE.BL, 
  rFel.d.2.Cat.IgE.BL, 
  Total.IgE.BL)

cormat <- round(cor(tdata),2)
head(cormat) 

upper_tri <- get_upper_tri(cormat)
melted_cormat <- melt(upper_tri, na.rm = TRUE)




ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()





################################################
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

tdata = OUT84 %>% select(#USUBJID, 
  REGN1908, REGN1909, REGN1908_1909, 
  AUC_1908,   AUC_1909,  AUC_1908_1909, 
  TNSS_AUC01, VAS_AUC01, PNIF_AUC01, 
  Cat.Dander.IgE.BL, rFel.d.1.Cat.IgE.BL, 
  #rFel.d.2.Cat.IgE.BL, 
  Total.IgE.BL)

cormat <- round(cor(tdata),2)

cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)



ggheatmap + 
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


}
