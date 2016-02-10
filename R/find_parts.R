#' findParts
#' 
#' @description Helper function to retrieve valid values for part 
#' (mostly used internally)
#' 
#' @param FUN Function to retrieve the valid \code{part}
#' 
#' @details 
#' 
#' Valid \code{FUN}
#' \itemize {
#' \item \code{getActivities}
#' \item \code{getCaptions}
#' }
#' 
#' @examples {
#' # Authenticate
#' token <- youOauth(client.id = "something.apps.googleusercontent.com",
#'                   client.secret = "XxxXX1XxXxXxxx1xxx1xxXXX")
#'                   
#' # search channels about cats
#' search <- searchTube(token, query = "cats", type = "channel")
#' 
#' # random channel id
#' set.seed(19880525)
#' chan <- sample(search$id.channelId, 1)
#' 
#' # get valid part
#' p <- findParts(FUN = "getActivities")
#' 
#' # fetch data
#' act <- getActivities(token, channel.id = chan, part = p)
#' }
#' 
#' @author \email{jcoenep@@hotmail.com}
#' 
#' @export
#' 
findParts <- function(FUN) {
  
  if(FUN == "getActivities") {
    
    valid <- c("contentDetails", "id", "snippet")
    
  } else if (FUN == "getCaptions") {
    
    valid <- c("id", "snippet")
    
  }
  
  return(valid)
  
}