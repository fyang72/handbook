
vanilla_table <- function (dataset, add.rownames = FALSE, text.direction = "lrtb") 
{
  
  font10_style <- textProperties(color='black',font.size = 10,
                                 font.weight = 'normal', font.family = 'Times New Roman' ) 
  bold10_style <- textProperties(color='black',font.size = 10,
                                 font.weight = 'bold', font.family = 'Times New Roman' ) 
  
  
  for (j in names(dataset)) {
    if (is.numeric(dataset[, j])) 
      dataset[, j] = format(dataset[, j])
  }
  ft = FlexTable(dataset, add.rownames = add.rownames, header.cell.props = cellProperties(text.direction = text.direction))
  ft[, , to = "header"] = bold10_style  # textBold()
  ft[, , to = "header"] = parProperties( text.align = "center", padding.left = 1, padding.right = 1 )       # parRight(padding.left = 1, padding.right = 1)
  ft[, , to = "body"] = font10_style  # textNormal()
  ft[, , to = "body"] =  parProperties( text.align = "center", padding.left = 1, padding.right = 1 )       #
  
  ft = setFlexTableBorders(ft, inner.vertical = borderProperties(width = 0), 
                           inner.horizontal = borderProperties(width = 0), outer.vertical = borderProperties(width = 0), 
                           outer.horizontal = borderProperties(width = 2))
  ft
}
 