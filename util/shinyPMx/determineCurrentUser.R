
determineCurrentUser <- function(session=NULL)
{
  if(!is.null(session$user))
  {
    # Extract the username directly, if available.
    username <- tolower(session$user)
  }else if(serverDetails[[AWSnodeName]]$serverType=="Development")
  {
    # If currently being run on the development server, replace the username with the name of the current developer.
    username <- tolower(Sys.info()["user"])
  }else
  {
    # If the username is not directly available and the current server is not the Dev server, the username is unknown.
    username <- "Unknown"
  }
  return(username)
}
