
# cheatsheet: https://uupharmacometrics.github.io/xpose/reference/figures/cheatsheet.pdf

# https://cran.r-project.org/web/packages/xpose/xpose.pdf
# https://uupharmacometrics.github.io/xpose/articles/customize_plots.html
# https://github.com/UUPharmacometrics/xpose

#1) In scatter plots type can be any combination of: 
  #points 'p', lines 'l', smooth 's', text 't'.
# dv_vs_ipred(xpdb_ex_pk, type = 'pls')


#2) In distribution plots type can be any combination of: 
# histogram 'h', density 'd', or rug 'r'.

#3) In visual predictive checks plots type can be any combination of: 
# areas 'a', lines 'l', points 'p', text 't' or rug 'r'.

#4) customized title = NULL, subtitle = NULL, caption = NULL)
# help("template_titles")

#5)
#dv_vs_ipred(xpdb, type = 'p', aes(point_color = SEX))

#6) n basic goodness-of-fit plots, the layers have been named as: point_xxx, line_xxx, smooth_xxx, guide_xxx, xscale_xxx, yscale_xxx where xxx can be any option available in the ggplot2 layers: geom_point, geom_line, geom_smooth, geom_abline, scale_x_continuous, etc.

# dv_vs_ipred(xpdb, 
#             # Change points aesthetics
#             point_color = 'blue', point_alpha = 0.5, 
#             point_stroke = 0, point_size = 1.5, 
#             # Change lines aesthetics 
#             line_alpha = 0.5, line_size = 0.5, 
#             line_color = 'orange', line_linetype = 'solid', 
#             # Change smooth aesthetics
#             smooth_method = 'lm')


#7) Panels (or faceting) can be created by using the facets argument as follows: If facets is a string e.g facets = "SEX", ggforce::facet_wrap_paginate will be used
# If facets is a formula e.g facets = SEX~MED1, ggforce::facet_grid_paginate will be used
# Example with a string
#dv_vs_ipred(xpdb, facets = c('SEX', 'MED1'), ncol = 2, nrow = 1, page = 1, margins = TRUE)





xpose_all_diagnostic_plots <- function(xpdb, options=NULL) {
  
  model_diagnostic_plots <- NULL
  
  # "na"      "id"      "idv"     "ipred"   "pred"    "dv"      "mdv"     "res"     
  # "catcov"     "contcov" "param"   "eta"
  pred_label <- xpdb$data$index[[1]] %>% filter(type=="pred")  %>% pull(label)
  ipred_label <- xpdb$data$index[[1]] %>% filter(type=="ipred")  %>% pull(label)
  idv_label <- xpdb$data$index[[1]] %>% filter(type=="idv")  %>% pull(label)
  dv_label <- xpdb$data$index[[1]] %>% filter(type=="dv")  %>% pull(label)
   

  #----------------------------------
  # Basic goodness-of-fit plots
  #----------------------------------
  
  # dv_vs_pred linear-linear scale
  fig <- xpdb %>% dv_vs_pred(quiet=TRUE) + xlab(pred_label) + ylab(dv_label)
  
  attr(fig, "title") <- "Linear-scale: Observed (DV) versus Population Predicted (PRED) from the Model @run"
  attr(fig, "footnote") <- "teststeste4tsetsests"
  model_diagnostic_plots[["dv_vs_pred_ln_ln"]] <- fig   
  
  # dv_vs_pred log-log scale 
  fig <- xpdb %>% 
    dv_vs_pred(quiet=TRUE) + 
    scale_x_log10() + scale_y_log10() + 
    annotation_logticks(sides ="bl") +  # "trbl", for top, right, bottom, and left.
    xlab(pred_label) + ylab(dv_label)
  
  attr(fig, "title") <- "Log-scale: Observed (DV) versus Population Predicted (PRED) from the Model @run"
  attr(fig, "footnote") <- "teststeste4tsetsests"
  model_diagnostic_plots[["dv_vs_pred_log_log"]] <- fig   
  
  
  # dv_vs_ipred linear-linear scale
  fig <- xpdb %>% dv_vs_ipred(quiet=TRUE) + xlab(ipred_label) + ylab(dv_label)
  
  attr(fig, "title") <- "Linear-scale: Observed (DV) versus Individual Predicted (IPRED) from the Model @run"
  attr(fig, "footnote") <- "teststeste4tsetsests"
  model_diagnostic_plots[["dv_vs_ipred_ln_ln"]] <- fig
  
  
  # dv_vs_ipred log-log scale
  fig <- xpdb %>% dv_vs_ipred(quiet=TRUE) + 
    scale_x_log10() + scale_y_log10() + 
    annotation_logticks(sides ="bl") +  # "trbl", for top, right, bottom, and left.
    xlab(ipred_label) + ylab(dv_label)
  
  attr(fig, "title") <- "Log-scale: Observed (DV) versus Individual Predicted (IPRED) from the Model @run"
  attr(fig, "footnote") <- "teststeste4tsetsests"
  model_diagnostic_plots[["dv_vs_ipred_log_log"]] <- fig
  

  #----------------------------------
  #Individual plots
  #----------------------------------
  fig <- xpdb %>% ind_plots(quiet=TRUE, page = 1) + 
    xlab(idv_label) + 
    ylab("Observed/Predicted")
  
  attr(fig, "title") <- "Illustration of Model Fitting between Observed (DV), Individual Predicted (IPRED), and Population Predicted (PRED), versus Time"
  attr(fig, "footnote") <- "teststeste4tsetsests"
  model_diagnostic_plots[["indiv_plots"]] <- fig
  
  
  #----------------------------------------
  # Plots of the independent variable (IDV)
  #----------------------------------------
  fig <- xpdb %>% dv_vs_idv(quiet=TRUE) + xlab(idv_label) + ylab(dv_label)
  attr(fig, "title") <- "Observed (DV) versus Time from the Model @run"
  attr(fig, "footnote") <- "teststeste4tsetsests"
  model_diagnostic_plots[["dv_vs_idv"]] <- fig
  
  fig <- xpdb %>% ipred_vs_idv(quiet=TRUE) + xlab(idv_label) + ylab(ipred_label)
  attr(fig, "title") <- "Individual Predicted (IPRED) versus Time from the Model @run"
  attr(fig, "footnote") <- "teststeste4tsetsests"
  model_diagnostic_plots[["ipred_vs_idv"]] <- fig
  
  fig <- xpdb %>% pred_vs_idv(quiet=TRUE) + xlab(idv_label) + ylab(pred_label)
  attr(fig, "title") <- "Populated Predicted (PRED) versus Time from the Model @run"
  attr(fig, "footnote") <- "teststeste4tsetsests"
  model_diagnostic_plots[["pred_vs_idv"]] <- fig
  
  fig <- xpdb %>% dv_preds_vs_idv(quiet=TRUE) + xlab(idv_label) + ylab("Observed/Predicted")
  attr(fig, "title") <- "Observed (DV) and Predicted (IPRED/PRED) versus Time from the Model @run"
  attr(fig, "footnote") <- "teststeste4tsetsests"
  model_diagnostic_plots[["dv_preds_vs_idv"]] <- fig
  
  #------------------------
  # Residual plots
  #------------------------
  fig <- xpdb %>% res_vs_idv(quiet=TRUE, res = 'CWRES')  
  attr(fig, "title") <- "Conditional Weighted Residue (CWRES) versus Time from the Model @run"
  attr(fig, "footnote") <- "teststeste4tsetsests"
  model_diagnostic_plots[["cwres_vs_idv"]] <- fig
  
  
  fig <- xpdb %>% res_vs_pred(quiet=TRUE, res = 'CWRES')  
  attr(fig, "title") <- "Conditional Weighted Residue (CWRES) versus Population Predicted (PRED) from the Model @run"
  attr(fig, "footnote") <- "teststeste4tsetsests"
  model_diagnostic_plots[["cwres_vs_pred"]] <- fig
    
  
  fig <- xpdb %>% absval_res_vs_idv(quiet=TRUE, res = 'CWRES')  
  attr(fig, "title") <- "Absolute Conditional Weighted Residue (CWRES) versus Time from the Model @run"
  attr(fig, "footnote") <- "teststeste4tsetsests"
  model_diagnostic_plots[["absval_cwres_vs_idv"]] <- fig
  
  
  fig <- xpdb %>% absval_res_vs_pred(quiet=TRUE, res = 'CWRES')  
  attr(fig, "title") <- "Absolute Conditional Weighted Residue (CWRES) versus Population Predicted (PRED) from the Model @run"
  attr(fig, "footnote") <- "teststeste4tsetsests"
  model_diagnostic_plots[["absval_cwres_vs_pred"]] <- fig
  
  
  
  #Distributions
  #----------------------------------
  #Histograms and density plots
  fig <- xpdb %>% prm_distrib(quiet=TRUE)
  attr(fig, "title") <- "Parameter Distribution from the Model @run"
  attr(fig, "footnote") <- "teststeste4tsetsests"
  model_diagnostic_plots[["prm_distrib"]] <- fig
  
  fig <- xpdb %>% eta_distrib(quiet=TRUE, labeller = 'label_value')  
  attr(fig, "title") <- "Eta Distribution from the Model @run"
  attr(fig, "footnote") <- "teststeste4tsetsests"
  model_diagnostic_plots[["eta_distrib"]] <- fig
  
  fig <- xpdb %>% res_distrib(quiet=TRUE)
  attr(fig, "title") <- "CWRES Distribution from the Model @run"
  attr(fig, "footnote") <- "teststeste4tsetsests"
  model_diagnostic_plots[["cwres_distrib"]] <- fig
  
  fig <- xpdb %>% cov_distrib(quiet=TRUE)
  attr(fig, "title") <- "Continous Covariate Distribution from the Model @run"
  attr(fig, "footnote") <- "teststeste4tsetsests"
  model_diagnostic_plots[["cov_distrib"]] <- fig
  
  
  # QQ plots
  fig <- xpdb %>% prm_qq(quiet=TRUE)
  attr(fig, "title") <- "QQ Plot of Parameters from the Model @run"
  attr(fig, "footnote") <- "teststeste4tsetsests"
  model_diagnostic_plots[["prm_qq"]] <- fig
  
  fig <- xpdb %>% eta_qq(quiet=TRUE)
  attr(fig, "title") <- "QQ Plot of Eta from the Model @run"
  attr(fig, "footnote") <- "teststeste4tsetsests"
  model_diagnostic_plots[["eta_qq"]] <- fig
  
  fig <- xpdb %>% res_qq(quiet=TRUE)
  attr(fig, "title") <- "QQ Plot of CWRES from the Model @run"
  attr(fig, "footnote") <- "teststeste4tsetsests"
  model_diagnostic_plots[["res_qq"]] <- fig
  
  fig <- xpdb %>% cov_qq(quiet=TRUE)
  attr(fig, "title") <- "QQ Plot of Continous Covariates from the Model @run"
  attr(fig, "footnote") <- "teststeste4tsetsests"
  model_diagnostic_plots[["cov_qq"]] <- fig
  
 
    #Visual predictive checks (VPC)
    #----------------------------------
    #Continuous vpc
  fig <- xpdb_ex_pk %>% 
      vpc_data(stratify = 'SEX', opt = vpc_opt(n_bins = 7, lloq = 0.1)) %>% 
      vpc(quiet=TRUE)
    attr(fig, "title") <- "Visual Predictive Check from the Model @run"
    attr(fig, "footnote") <- "teststeste4tsetsests"
    model_diagnostic_plots[["vpc_stratify_sex"]] <- fig
    
  if (1==2) { 
    #Censored vpc
    fig <- xpdb %>% 
      vpc_data(vpc_type = 'censored', opt = vpc_opt(lloq = 1, n_bins = 7)) %>% 
      vpc(quiet=TRUE)
    attr(fig, "title") <- "Visual Predictive Check from the Model @run"
    attr(fig, "footnote") <- "teststeste4tsetsests"
    model_diagnostic_plots[["vpc_type_censored"]] <- fig
    
    #Categorical vpc
    fig <- xpdb %>% 
      vpc_data(vpc_type = 'categorical') %>% 
      vpc(quiet=TRUE)
    attr(fig, "title") <- "Visual Predictive Check from the Model @run"
    attr(fig, "footnote") <- "teststeste4tsetsests"
    model_diagnostic_plots[["vpc_type_categorical"]] <- fig
  }
  
  
  
  #  Minimization diagnostics
  # Graphics to evaluate the estimation step
    #fig <- xpdb %>% prm_vs_iteration()    #Parameter value or gradient vs. iterations
    fig <- xpdb %>%prm_vs_iteration(labeller = 'label_value')
  
    #fig <- xpdb %>%grd_vs_iteration(labeller = 'label_value')
  
  
  
  # Kinetic plots
  # Compartment’s amount
  # xpdb %>% amt_vs_idv()
  
  
 return(model_diagnostic_plots) 
}