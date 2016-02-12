#' findParts
#' 
#' @description Helper function to retrieve valid values for part 
#' (mostly used internally).
#' 
#' @param FUN Function to retrieve the valid \code{part}.
#' 
#' @details Valid \code{FUN}: 
#' \itemize{
#' \item \code{\link{getActivities}}
#' \item \code{\link{getCaptions}}
#' \item \code{\link{getChannels}}
#' \item \code{\link{getChannelSections}}
#' \item \code{\link{getComments}}
#' \item \code{\link{getCommentThreads}}
#' \item \code{\link{getPlaylistItems}}
#' \item \code{\link{getPlaylists}}
#' \item \code{\link{getSubscriptions}}
#' \item \code{\link{getVideos}}
#' }
#' Alternatively see \code{scope} in \code{\link{youOAuth}}.
#' 
#' @examples 
#' \dontrun{
#' # Authenticate
#' token <- youOAuth(client.id = "something.apps.googleusercontent.com",
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
#' p <- findParts(FUN = "getActivities")[1]
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
    
  } else if (FUN == "getChannels") {
    
    valid <- c("auditDetails", "brandingSettings", "contentDetails", 
               "contentOwnerDetails", "id", "invideoPromotion", 
               "localizations", "snippet", "statistics", "status", 
               "topicDetails")
    
  } else if (FUN == "getChannelSections") {
    
    valid <- c("contentDetails", "id", "localizations", "snippet")
    
  } else if (FUN == "getComments") {
    
    valid <- c("id", "snippet")
    
  } else if (FUN == "getCommentThreads") {
    
    valid <- c("id", "replies", "snippet")
    
  } else if (FUN == "getPlaylistItems") {
    
    valid <- c("contentDetails", "id", "snippet", "status")
    
  } else if (FUN == "getPlaylists") {
    
    valid <- c("contentDetails", "id", "localizations", "player", "snippet", 
               "status")
    
  } else if (FUN == "getSubscriptions") {
    
    valid <- c("snippet", "contentDetails", "id", "subscriberSnippet")
    
  } else if (FUN == "getVideos") {
    
    valid <- c("contentDetails", "fileDetails", "id", "liveStreamingDetails",
               "localizations", "player", "processingDetails", 
               "recordingDetails", "snippet", "statistics", "status", 
               "suggestions", "topicDetails")
    
  } else {
    
    stop("wrong FUN passed, see @details")
    
  }
  
  return(valid)
  
}