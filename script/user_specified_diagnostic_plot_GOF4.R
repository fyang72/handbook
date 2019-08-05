
basic_diagnostic_plots <- function(dataset=NULL, params=NULL) {  
  # initialize variables
  figure=NULL
  table =NULL
  data = NULL
  
  
  library(xpose)
  #xpdb_ex_pk
  xpdb <- xpdb_ex_pk 
  xpdb <- dataset
  
  xpdb <- xpdb %>% 
    update_themes(gg_theme = theme_bw() + base_theme(font.size = 12),
                  xp_theme = list(point_color = 'blue',
                                  line_color  = 'black'))  
  
  
  
  #Basic goodness-of-fit plots
  #----------------------------------
  # DV vs. PREDs plots
  figure[["dv_vs_pred"]] <- xpdb %>% dv_vs_pred(quiet=TRUE)
  figure[["dv_vs_ipred"]] <- xpdb %>% dv_vs_ipred(quiet=TRUE)
  
  # Plots of the independent variable (IDV)
  figure[["dv_vs_idv"]] <-xpdb %>% dv_vs_idv(quiet=TRUE)
  figure[["ipred_vs_idv"]] <-xpdb %>% ipred_vs_idv(quiet=TRUE)
  figure[["pred_vs_idv"]] <-xpdb %>% pred_vs_idv(quiet=TRUE)
  figure[["dv_preds_vs_idv"]] <-xpdb %>% dv_preds_vs_idv(quiet=TRUE)
   
  # Residual plots
  figure[["res_vs_idv"]] <- xpdb %>% res_vs_idv(quiet=TRUE, res = 'CWRES')
  figure[["res_vs_pred"]] <- xpdb %>% res_vs_pred(quiet=TRUE, res = 'CWRES')
  figure[["absval_res_vs_idv"]] <- xpdb %>% absval_res_vs_idv(quiet=TRUE, res = 'CWRES')
  figure[["absval_res_vs_pred"]] <- xpdb %>% absval_res_vs_pred(quiet=TRUE, res = 'CWRES')
  figure[["absval_res_vs_pred"]] <- xpdb %>% absval_res_vs_pred(quiet=TRUE, res = 'CWRES')
  
  
  #Individual plots
  #----------------------------------
  figure[["ind_plots"]] <- xpdb %>% ind_plots(quiet=TRUE, page = 1)   
  
  #Distributions
  #----------------------------------
  #Histograms and density plots
  figure[["prm_distrib"]] <- xpdb %>% prm_distrib(quiet=TRUE)
  figure[["eta_distrib"]] <- xpdb %>% eta_distrib(quiet=TRUE, labeller = 'label_value')  
  figure[["res_distrib"]] <- xpdb %>% res_distrib(quiet=TRUE)
  figure[["cov_distrib"]] <- xpdb %>% cov_distrib(quiet=TRUE)
  
  
  # QQ plots
  figure[["prm_qq"]] <- xpdb %>% prm_qq(quiet=TRUE)
  figure[["eta_qq"]] <- xpdb %>% eta_qq(quiet=TRUE)
  figure[["res_qq"]] <- xpdb %>% res_qq(quiet=TRUE)
  figure[["cov_qq"]] <- xpdb %>% cov_qq(quiet=TRUE)
  
  if (1==2) { 
  #Visual predictive checks (VPC)
  #----------------------------------
  #Continuous vpc
  figure[["vpc_stratify_sex"]] <- xpdb %>% 
    vpc_data(stratify = 'SEX', opt = vpc_opt(n_bins = 7, lloq = 0.1)) %>% 
    vpc(quiet=TRUE)
  
  #Censored vpc
  figure[["vpc_type_censored"]] <- xpdb %>% 
    vpc_data(vpc_type = 'censored', opt = vpc_opt(lloq = 1, n_bins = 7)) %>% 
    vpc(quiet=TRUE)
  
  #Categorical vpc
  figure[["vpc_type_categorical"]] <- xpdb %>% 
    vpc_data(vpc_type = 'categorical') %>% 
    vpc(quiet=TRUE)
  
  }
  
  
  #Kinetic plots
  #----------------------------------
  #Compartment’s amount
  figure[["amt_vs_idv"]] <- xpdb %>% amt_vs_idv(quiet=TRUE)
  
  
  #Minimization plots
  #----------------------------------
  figure[["prm_vs_iteration"]] <- xpdb %>% prm_vs_iteration(quiet=TRUE, labeller = 'label_value')
  
  figure[["grd_vs_iteration"]] <- xpdb %>% grd_vs_iteration(quiet=TRUE, labeller = 'label_value')
  
  # run001.lst overview: 
  #  - Software: nonmem 7.3.0 
  #  - Attached files (memory usage 1.4 Mb): 
  #    + obs tabs: $prob no.1: catab001.csv, cotab001, patab001, sdtab001 
  #    + sim tabs: $prob no.2: simtab001.zip 
  #    + output files: run001.cor, run001.cov, run001.ext, run001.grd, run001.phi, run001.shk 
  #    + special: <none> 
  #  - gg_theme: theme_readable 
  #  - xp_theme: theme_xp_default 
  #  - Options: dir = analysis/models/pk/, quiet = TRUE, manual_import = NULL
  # Model summary
  
  
  #slot(xpdb, "Data") =  slot(xpdb, "Data") %>%  mutate(WRES=1)
  
  #input$which_program = "TEST"
  # input$which_run = "control5_THEOPP"
  #user.name = tolower(Sys.info()["user"])  # determineCurrentUser()
  
  #slotNames(xpdb)
  # "Xvardef"       "Labels"   "Graph.prefs"   "Miss"   "Cat.levels"    "DV.Cat.levels" "Subset"        "Gam.prefs"     "Bootgam.prefs"
  #slotNames(slot(xpdb,"Prefs"))
  
  #slot(slot(xpdb,"Prefs"), "Xvardef")
  #names(slot(slot(xpdb,"Prefs"), "Xvardef"))
  
  #xpdb =  slot(xpose4.obj,"Data")
  # runno = slot(xpose4.obj,"Runno")
  
   
  return(list(data=data, figure=figure, table=table))
}

#################################################################
# final output
#################################################################
if (ihandbook) {
  output = suppressWarnings(
     dataset %>% basic_diagnostic_plots(params=NULL)
  )
  
}

