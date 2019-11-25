
##############################################################################
# colScheme
##############################################################################
#' Extracts the time matched concntration vs effect data

#' @param data A data frame.  The data frame must conform to Regeneron standards and contain the variables: `week`, `ndy`, `concn`, `concni`, `pchg`, `actarmcd`, `ada`
#' @return A dataframe with columns `ndy`, `concn`, `effect`.
#' @export
#' @importFrom dplyr left_join group_by summarise
#' @examples
#'   inFile <- system.file("extdata/pk_pd_hysteresis.xls", package = "pkGraph")
#'   library(readxl)
#'   theData <-read_excel(inFile)
#'   timeMatchedPK(data = theData)
colScheme <- function() {
  
  color.scheme = c( "black",     "red",       "blue",      "green",     "cyan",
                    "brown",     "tan",       "violet",    "tomato",
                    "orange",    "magenta",   "pink",      "salmon",    "chocolate",
                    "plum",      "purple",    "navy",      "peru" ,     "khaki",
                    "orchid",    "maroon",    "ivory",     "turquoise", "gold", "yellow", 
                    
                    "black",     "red",       "blue",      "green",     "cyan",
                    "brown",     "tan",       "violet",    "tomato",
                    "orange",    "magenta",   "pink",      "salmon",    "chocolate",
                    "plum",      "purple",    "navy",      "peru" ,     "khaki",
                    "orchid",    "maroon",    "ivory",     "turquoise", "gold", "yellow", 
                    
                    "black",     "red",       "blue",      "green",     "cyan",
                    "brown",     "tan",       "violet",    "tomato",
                    "orange",    "magenta",   "pink",      "salmon",    "chocolate",
                    "plum",      "purple",    "navy",      "peru" ,     "khaki",
                    "orchid",    "maroon",    "ivory",     "turquoise", "gold", "yellow")
  return(color.scheme)
  
}


##############################################################################
# shapeScheme
##############################################################################
#' Extracts the time matched concntration vs effect data

#' @param data A data frame.  The data frame must conform to Regeneron standards and contain the variables: `week`, `ndy`, `concn`, `concni`, `pchg`, `actarmcd`, `ada`
#' @return A dataframe with columns `ndy`, `concn`, `effect`.
#' @export
#' @importFrom dplyr left_join group_by summarise
#' @examples
#'   inFile <- system.file("extdata/pk_pd_hysteresis.xls", package = "pkGraph")
#'   library(readxl)
#'   theData <-read_excel(inFile)
#'   timeMatchedPK(data = theData)
#'   
shapeScheme = function() {
  
  shape.scheme = c( 15, 16, 17, 18, 0,  1,  2,  5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 
                    15, 16, 17, 18, 0,  1,  2,  5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 
                    15, 16, 17, 18, 0,  1,  2,  5, 6, 7, 8, 9, 10, 11, 12, 13, 14) 
  return(shape.scheme)
  
}
