#' findParams
#' 
#' @description Helper function to retrieve valid values for parameters 
#' (mostly used internally)
#' 
#' @param param Parameter to retrieve
#' 
#' @details 
#' Given the pletoric number of parameters a this helper function is provided. 
#' Please see the 
#' \href{https://developers.google.com/youtube/v3/docs/search/list}{official documentation} 
#' for more information. Note that not all parameters (i.e.: \code{channel.id}) 
#' cannot, by nature, be tested.
#' 
#' Valid parameters: 
#' \itemize{
#' \item \code{order}
#' \item \code{video.dimension}
#' \item \code{video.caption}
#' \item \code{video.duration}
#' \item \code{video.definition}
#' \item \code{video.embeddable}
#' \item \code{video.syndicated}
#' \item \code{video.license}
#' \item \code{video.type}
#' \item \code{safe.search}
#' \item \code{event.type}
#' \item \code{channel.type}
#' \item \code{type}
#' \item \code{part}
#' }
#' 
#' @examples 
#' \dontrun{
#' # Authenticate
#' token <- youOauth(client.id = "something.apps.googleusercontent.com",
#'                   client.secret = "XxxXX1XxXxXxxx1xxx1xxXXX")
#' 
#' # find valid values for video.dimension
#' dims <- findParams(param = "video.dimension")
#' 
#' # set seed for reproducability
#' set.seed(19880525)
#' 
#' # fetch data using parameter
#' cats <- searchVideos(token, query = "cats", 
#'                      video.dimension = sample(dims, 1))
#' }
#' 
#' @author John Coene \email{jcoenep@hotmail.com}
#' 
#' @export
findParams <- function(param) {
  
  if(param == "order") {
    
    valid <- c("date", "rating", "relevance", "title", "videoCount",
               "viewCount")
  } else if (missing(param)) {
    
    stop("Please specify a parameter")
    
  } else if (param == "video.dimension") {
    
    valid <- c("2d", "3d", "any")
    
  } else if (param == "video.caption") {
    
    valid <- c("any", "closedCaption", "none")
    
  } else if (param == "video.duration") {
    
    valid <- c("any", "long", "medium", "short")
    
  } else if (param == "video.definition") {
    
    valid <- c("any", "high", "standard")
    
  } else if (param == "safe.search") {
    
    valid <- c("moderate", "none", "strict")
    
  } else if (param == "event.type") {
    
    valid <- c("completed", "live", "upcoming")
    
  } else if (is.null(param)) {
    
    valid <- NULL
    
  } else if (param == "channel.type") {
    
    valid <- c("any", "show")
    
  } else if (param == "video.embeddable" || param == "video.syndicated") {
    
    valid <- c("any", "true")
    
  } else if (param == "video.license") {
    
    valid <- c("any", "creative", "youtube")
    
  } else if (param == "video.type") {
    
    valid <- c("any", "episode", "movie")
    
  } else if (param == "type"){
    
    valid <- c("any", "channel", "playlist", "video")
    
  } else {
    
    stop("wrong param. See @details")
    
  }
  
  return(valid)
}