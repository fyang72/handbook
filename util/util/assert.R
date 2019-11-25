
# https://stackoverflow.com/questions/8343509/better-error-message-for-stopifnot

assert <- function (expr, msg) {
  if(missing(msg)) msg <- paste("Condition", deparse(as.list(match.call())$expr), "is not TRUE") 
  if (! expr) stop(msg, call. = FALSE) }