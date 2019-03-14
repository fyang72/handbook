# 
# STUDYID ARMA  CMIN_N CMIN_Mean CMIN_SE CMIN_SD CMIN_Median CMAX_N CMAX_Mean CMAX_SE CMAX_SD CMAX_Median AUC_N AUC_Mean AUC_SE
# <chr>   <ord>  <dbl>     <dbl>   <dbl>   <dbl>       <dbl>  <dbl>     <dbl>   <dbl>   <dbl>       <dbl> <dbl>    <dbl>  <dbl>
#   1 SFS     3 mg~      2         0       0       0           0      2      195.    17.0    24.1        195.     2   22316.  1453.
# 2 SFS     3 mg~      2         0       0       0           0      2      229.    65.1    92.1        229.     2   23648.  7075.
# # ... w

order_cols <- function(tt, group_lst, param_lst, stats_lst) {
  library(stringr)
  t1 = str_split_fixed(colnames(tt), "_", 2) %>% as.data.frame()%>% 
    mutate(param= ordered(.[[1]], levels=c(group_lst, param_lst)), 
           stats = ordered(.[[2]], levels=stats_lst)
    ) %>% 
    arrange(param, stats) %>% 
    mutate(param_stats = ifelse(is.na(stats), as.character(param), paste0(param,"_",stats))) %>% 
    pull(param_stats)
  tt[, t1]
}