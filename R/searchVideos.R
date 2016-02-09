#' searchVideos
#' 
#' @export
searchVideos <- function(token, query, order = "relevance", 
                         video.dimension = "any", 
                         video.caption = "any", video.category.id = NULL, 
                         video.duration = "any", video.embeddable = "any",
                         video.syndicated = "any", video.type = "any",
                         video.definition = "any", video.license = "any",
                         channel.type = "any", max.results = 5, location = NULL, 
                         location.radius = NULL, region.code = NULL,
                         safe.search = "none", event.type = NULL, 
                         published.before = Sys.Date(), published.after = NULL,
                         channel.id = NULL, relevance.language = NULL) {
  
  # check required arguments
  if (missing(query)) {
    stop("query is missing")
  }
  # check token
  checkToken(token)
  
  # parameters to list
  arguments <- namedList(order, video.dimension, video.caption, video.category.id, 
                    video.duration, video.embeddable, video.syndicated, 
                    video.type, video.definition, video.license,
                    channel.type, max.results, location, location.radius, 
                    region.code, safe.search, event.type, published.before,
                    published.after, channel.id, relevance.language)
  
  # buildParameters
  x <- list()
  for (i in 1:length(arguments)) {
   y <- buildParam(param = names(arguments[i]), 
                       values = arguments[[i]])
   x[[i]] <- ifelse(!is.null(y), y, "")
  }
  
  # collapse
  suffix <- paste(x, collapse = "")
  
  # time
  published.before <- buildTime(published.before)
  published.after<- buildTime(published.after)
  
  # query
  query <- paste0("&q=",query)
  
  # build uri
  uri <- paste0("https://www.googleapis.com/youtube/v3/search?part=snippet",
                "&type=video", query, published.before, published.after, 
                suffix, "&access_token=", token$credentials$access_token)
  
  return(uri)
}