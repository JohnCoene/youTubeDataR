#' findParams
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
    
  } else {
    
    stop("wrong param. See @details")
    
  }
  
  return(valid)
}