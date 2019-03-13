
# if missing desired variables
# if nmdat is null, return a empty data.frame with column name assigned to nmdat.var.lst
# if not null, return a fillup data.frame with NA in column that not exist before.
fillUpCol_df <- function(nmdat=NULL, nmdat.var.lst="")  {
  col.lst = setdiff(nmdat.var.lst, colnames(nmdat))
  if (length(col.lst)>0) {print(paste0("Warning: missing variable(s) of ", paste0(col.lst, sep=", ", collapse="")))}
  
  if (is.null(nmdat) || nrow(nmdat)==0) {
    nmdat = setNames(data.frame(matrix(ncol = length(nmdat.var.lst), nrow = 0)), nmdat.var.lst)
    colnames(nmdat) = toupper(colnames(nmdat))
    nmdat[] <- lapply(nmdat, as.character)
  }else{
    nmdat[, col.lst] = NA
    colnames(nmdat) = toupper(colnames(nmdat))
  }
  return(nmdat)
}