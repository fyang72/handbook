

# https://blog.exploratory.io/5-most-practically-useful-operations-when-working-with-date-and-time-in-r-9f9eb8a17465




##############################################################################  
# as.Date, as.Time
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
as_Time <-  function(TIME) {
  library(chron)
  TIME2 <- as.character(TIME)
  ids <- which(!is.na(TIME))
  TIME2[ids] <-  as.character(chron::times(format(TIME[ids], "%H:%M:%S")))
  return(TIME2) }


as.Time <-  function(TIME) {
  library(chron)
  TIME2 <- as.character(TIME)
  ids <- which(!is.na(TIME))
  TIME2[ids] <-  as.character(chron::times(format(TIME[ids], "%H:%M:%S")))
  return(TIME2) }

