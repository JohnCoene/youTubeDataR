#' findParams
#' 
#' @export
findParams <- function(param) {
  
  if(param == "order") {
    
    valid <- c("date", "rating", "relevance", "title", "videoCount",
               "viewCount")
  } else if (missing(param)) {
    
    stop("Please specify a parameter")
    
  } else if (param == "video.dimensions") {
    
    valid <- c("2d", "3d", "any")
    
  } else if (param == "type"){
    
    valid <- c("channel", "playlist", "video")
    
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
    
  }
  
  return(valid)
}