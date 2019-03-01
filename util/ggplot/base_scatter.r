 
 


#
#################################################################################
## Single stack  [ID  ETA1   ETA10    cov_name cov_value  ..... ] 
#################################################################################
## [ID  ETA1   ETA10    WT RACE ETHNIC ..... ] 
#eta_cov <- read.csv("./data/ebe_cov.csv")
#
## gather all covariates from BW to CRCL to cov_name and cov_value
#g_eta_cov <- eta_cov %>%  gather(cov_name, cov_value, BW:CRCL)
# 
#eta_cov_scatter <- function(df, xval = "cov_value", yval, cov_name = "cov_name") {
#lazy_plot <- lazyeval::interp(~ggplot(df, aes(x = cov_value, y = ETA1)) +
#  geom_point() + facet_wrap(~cov_name, scales="free"),
#  cov_value = as.name(xval),
#  ETA1 = as.name(yval),
#  cov_name = as.name(cov_name))
#return(lazyeval::lazy_eval(lazy_plot))
#}
# 
#### Single plot example
#eta_cov_scatter(g_eta_cov, yval = "ETA1")
#
#
#### Iterate through multiple ETA values
#lapply(paste0("ETA", 1:4), function(eta, g_eta_cov) {
#eta_cov_scatter(g_eta_cov, yval = eta)
#}, g_eta_cov)
#
#
#
#### Iterate through multiple ETA values
#
#lapply(paste0("ETA", 1:4), function(eta, g_eta_cov) {
#  p = basePlot(g_eta_cov, "cov_value", eta, "cov_name", "cov_name", xscale="nonlog", yscale="nonlog") + 
#       xlab("cov value") +  ylab("eta value") + 
#       geom_point() + facet_wrap(~cov_name, scales = "free") + stat_smooth(se = F) +     #ggtitle(facet_template) + 
#       theme(legend.position = 'none')     
#        
#}, g_eta_cov)
#
#  
#################################################################################
## Double stack    [ID  eta_name  eta_value  cov_name cov_value  ..... ] 
#################################################################################
#eta_cov <- read.csv("./data/ebe_cov.csv")
#
## gather all covariates from BW to CRCL to cov_name and cov_value
#g_eta_cov <- eta_cov %>%  gather(cov_name, cov_value, BW:CRCL)
### Double stack, We can actually gather again
#g2_eta_cov <- g_eta_cov %>% gather(eta_name, eta_value, ETA1:ETA9 )
#
#kable(head(g2_eta_cov))
#kable(tail(g2_eta_cov))
#
#
##Then we can split up the plots
#split_eta_cov <- g2_eta_cov %>% split(.$cov_name)
# 
#
#### plot all releationships 
# 
#plot_all = NULL
#plot_all = split_eta_cov %>% lapply(basePlot, "cov_value", "eta_value", "cov_name", "eta_name", xscale="nonlog", yscale="nonlog")
#plot_all
#
#OUT = lapply(plot_all, function(x) {
#  cov_name <- names(plot_all)
#  p <- x + xlab(cov_name) +  ylab("ETA Value") + 
#       geom_point() + facet_wrap(~eta_name, scales = "free") + stat_smooth(se = F) +     #ggtitle(facet_template) + 
#       theme(legend.position = 'none')     
#  return(p)
#})
#
# 


  
  #
#  
## scatter_plot 
#resid <- read_phx("./data/Residuals.csv")
#theta <- read_phx("./data/Theta.csv")
#etacov_gathered <- read_phx("./data/EtaCov_gathered.csv")
#
#theta %>% 
#  select(-one_of(c("Scenario", "Var. Inf. factor"))) %>% 
#  kable(digits = 2)
#  
#  
#gg_cwres_tad <- function(df) {
#df %>%
#  ggplot(aes(x = TAD, y = CWRES)) + geom_point() +
#  stat_smooth(method = "loess", se=F, color = "red") +
#  stat_smooth(data = df %>% mutate(CWRES = abs(CWRES)), 
#              se = F, color = "blue") +
#  stat_smooth(data = df %>% mutate(CWRES = -abs(CWRES)), 
#              se = F, color = "blue") +
#    theme_bw() +
#    base_theme()
#}
# 
#
#
#   update the CWRES vs Time plot to flag anything with CWRES > 2.5 as a red value
#5) print a table of key information for all points with CWRES > 2.5 
#gg_cwres_tad(resid) + 
#geom_point(data=mutate(resid, HIGHCWRES = ifelse(abs(CWRES) > 2.5, 1, 0)), aes(color=factor(HIGHCWRES))) + 
#scale_color_manual(values = c("black", "red"), name = "Outlier", labels = c("not outlier", "outlier")) 
#
#
#resid %>% 
#  mutate(HIGHCWRES = ifelse(abs(CWRES) > 2.5, 1, 0)) %>%
#  filter(HIGHCWRES ==1) %>% select(ID, IVAR, TAD, IPRED, DV) %>% kable(digits = 2)
#  
#  
#


