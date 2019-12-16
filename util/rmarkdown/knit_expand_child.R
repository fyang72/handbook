
knit_expand_child_figure <- function(figure, child_Rmd_dir=paste0('./lib/child_figure.Rmd')) {
  
  fig_name_lst <- names(figure)  
    
  out_figure <- lapply(1:length(fig_name_lst), function(i) {
    knit_expand(child_Rmd_dir,  #paste0(HOME, '/lib/child_figure.Rmd'), 
                figure_name = fig_name_lst[i],             
                figure_label= gsub("_", "-", fig_name_lst[i], fix=TRUE), 
                figure_caption = attr(figure[[fig_name_lst[i]]], "title")
    )
  }) %>% unlist()
  
  return(out_figure)
}



knit_expand_child_table <- function(table, child_Rmd_dir=paste0('./lib/child_table.Rmd')) {
  
  tabl_name_lst <- names(table)  
  
  out_table <- lapply(1:length(tabl_name_lst), function(i) {
    knit_expand(child_Rmd_dir, #paste0(HOME, '/lib/child_table.Rmd'), 
                table_name = tabl_name_lst[i],             
                table_label= gsub("_", "-", tabl_name_lst[i], fix=TRUE), 
                table_caption = attr(table[[tabl_name_lst[i]]], "title")
    )
  }) %>% unlist()
  
  return(out_table)
}