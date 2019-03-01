# functions
is.POSIXct <- function(x) inherits(x, "POSIXct")
is.POSIXlt <- function(x) inherits(x, "POSIXlt")
is.POSIXt <- function(x) inherits(x, "POSIXt")
is.Date <- function(x) inherits(x, "Date")