
#' Extracts the time matched concntration vs effect data
#'
#'
#'
#' @param data A data frame.  The data frame must conform to Regeneron standards and contain the variables: `week`, `ndy`, `concn`, `concni`, `pchg`, `actarmcd`, `ada`
#' @return A dataframe with columns `ndy`, `concn`, `effect`.
#' @export
#' @importFrom dplyr left_join group_by summarise
#' @examples
#'   inFile <- system.file("extdata/pk_pd_hysteresis.xls", package = "pkGraph")
#'   library(readxl)
#'   theData <-read_excel(inFile)
#'   timeMatchedPK(data = theData)
myFlexTable <-function(tab_errbar_pk) {
  
  tdata = tab_errbar_pk 
  #tdata = tab_errbar_pd1
  
  tdata$NTIM = as.my.numeric(tdata$NTIM)
  
  col.lst = c("Mean","Median","SD","SE")
  tdata[, col.lst] = u.signif(tdata[, col.lst], digits = 3)
  
  tdata$VALUE = paste(tdata$N, tdata$Mean, tdata$Median, tdata$SD, tdata$SE, sep="-")
  tdata = tdata %>% select_("STUDYID", "VISIT", "VISITNM", "TIMEPT", "NTIM","ARMA","VALUE" ) %>% spread(ARMA, VALUE)
  
  arma.lst = unique(tab_errbar_pk$ARMA)
  for (iarma in arma.lst) {
    txt = paste("tdata = tdata %>% separate(", "'",iarma, "'", ", c('N', 'Mean','Median','SD','SE'), sep ='-', fill='right')", sep="")
    eval(parse(text=txt))
    
  }
  
  tdata = tdata[order(tdata$STUDYID, tdata$NTIM), ]
  
  col.head= c("STUDYID","TIMEPT", "NTIM")
  col.head= c( "TIMEPT" ) 
  col.stat = c("N", "Mean", "SD" ) 
  tdata = tdata[, colnames(tdata) %in%  c(col.head, col.stat)  ]
  #colnames(tdata) = str_to_title(colnames(tdata))
  
  
  #FlexTable(tdata, numrow, numcol, header.columns = TRUE, add.rownames = FALSE,
  #  body.cell.props = cellProperties(padding=0.5), 
  #  body.par.props = parProperties(padding= 0.5), body.text.props = textProperties(),
  #  header.cell.props = cellProperties(),
  #  header.par.props = parProperties(padding = 0),
  #  header.text.props = textProperties(font.weight = "bold"))
  #
  
  
  
  MyFTable = vanilla.table(tdata)
  # MyFTable[, col.stat] = parProperties( text.align = "center", padding= 0 )
  # MyFTable[, col.head] = parProperties( text.align = "left", padding= 0 )
  # 
  # MyFTable[, colnames(MyFTable)] <- textProperties(color='black',font.size = 10,
  #                font.weight = 'normal', font.family = 'Times New Roman' ) 
  #
  
  MyFTable
  
  #  ?FlexTable {ReporteRs}
  
  #  spanFlexTableRows. 
  #  spanFlexTableColumns. 
  #  
  MyFTable = addHeaderRow( MyFTable, 
                           value = c(rep("",time=length(col.head)), as.character(arma.lst)), 
                           colspan = c(rep(1,time=length(col.head)), rep(length(col.stat), time=length(arma.lst))),
                           par.properties =   parProperties( text.align = "center", padding= 0 ), 
                           text.properties =   textProperties(font.size = 10, font.weight='bold'),
                           first=TRUE)
  
  #MyFTable =spanFlexTableRows( MyFTable, j = 1, from = 1, to = nrow(tdata) )
  
  return(MyFTable)
}

   
   
  