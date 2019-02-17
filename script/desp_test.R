
mytest <-function(dataset) {
  
  figure=NULL
  table =NULL
  data = NULL
  
  #-------------------------------------------------------
  dataset <- dataset %>% mutate(TIME=as.numeric(TIME), 
                                DV = as.numeric(DV)
                                )
  #-------
  #fig-1
  #-------
  fig = ggplot(dataset, aes(x=TIME, y=DV, group=ID)) + 
    geom_point() + geom_line()
  attr(fig, 'title') <-  "sfgdhgdhdgdgdgdgsfs-LN"
  figure[["fig_LN"]] = fig
  
  #-------
  #fig-2
  #-------
  fig = ggplot(dataset, aes(x=TIME, y=DV, group=ID)) + 
    geom_point() + geom_line() + 
    scale_y_log10() 
  attr(fig, 'title') <-  "sfgdhgdhdgdgdgdgsfs-LOG"
  figure[["fig_LOG"]] = fig
  
  #-------
  #tab-1
  #-------
  tabl = head(dataset, n=10)
  attr(tabl, 'title') <-  "sfgsfs"
  table[["dhfghdf1"]] = tabl
  
  #-------
  #tab-2
  #-------
  tabl = dataset  %>% mutate(ID=as.integer(ID)+100) %>% head(n=10)
  attr(tabl, 'title') <-  "sfgsfs100"
  table[["dhfghdf100"]] = tabl
  
  
  #-------
  #data-1
  #-------
  tabl = dataset  %>% mutate(ID=as.integer(ID)+500)
  attr(tabl, 'title') <-  "sfgsfs"
  data[["dhfghdf1"]] = tabl
  
  #-------
  #data-2
  #-------
  tabl = dataset  %>% mutate(ID=as.integer(ID)+600)
  attr(tabl, 'title') <-  "sfgsfs100"
  data[["dhfghdf100"]] = tabl
  
  
  
  #-------------------------------------------------------
  
  return(list(figure=figure, table=table, data=data))
}

#################################################################
# final output
#################################################################
if (ihandbook) {

output = mytest(dataset)

}
