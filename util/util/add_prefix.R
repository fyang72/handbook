
############################################################################## 
# u.add.prefix
############################################################################## 
#' @param data A data frame.  The data frame must conform to Regeneron standards and contain the variables: `week`, `ndy`, `concn`, `concni`, `pchg`, `actarmcd`, `ada`
#' @return A dataframe with columns `ndy`, `concn`, `effect`.
#' @export
#' @importFrom dplyr left_join group_by summarise
#' @examples
#'   inFile <- system.file("extdata/pk_pd_hysteresis.xls", package = "pkGraph")
#'   library(readxl)
#'   theData <-read_excel(inFile)
#'   timeMatchedPK(data = theData) 
u.add.prefix <- function(x, prefix="", add.number.zero=6){
  #TrialTroveID-023621
  x <- as.numeric(as.character(x))
  x<- paste(paste(rep(0,time=add.number.zero),collapse=""), x, sep="" )
  x <- paste(prefix,    substr(x, nchar(x)-add.number.zero+1, nchar(x)), sep="")
  return(x)
}

#' @param data A data frame.  The data frame must conform to Regeneron standards and contain the variables: `week`, `ndy`, `concn`, `concni`, `pchg`, `actarmcd`, `ada`
#' @return A dataframe with columns `ndy`, `concn`, `effect`.
#' @export
#' @importFrom dplyr left_join group_by summarise
#' @examples
#'   inFile <- system.file("extdata/pk_pd_hysteresis.xls", package = "pkGraph")
#'   library(readxl)
#'   theData <-read_excel(inFile)
#'   timeMatchedPK(data = theData)     
add_prefix <- function(x, prefix="", digits=3){
  #TrialTroveID-023621
  x <- as.numeric(as.character(x))
  x<- paste(paste(rep(0,time=digits),collapse=""), x, sep="" )
  x <- paste(prefix,    substr(x, nchar(x)-digits+1, nchar(x)), sep="")
  return(x)
}

