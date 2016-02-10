#' searchTube
#' 
#' @description Search YouTube for videos, channels and/or playlists.
#' 
#' @param token 
#' Your token as returned by \code{\link{youOAuth}}.
#' @param query 
#' The query term to search for. Your request can also use the Boolean NOT (-) 
#' and OR (|) operators to exclude videos or to find videos that are 
#' associated with one of several search terms. For example, to search for 
#' videos matching either "boating" or "sailing", set the query parameter value 
#' to \code{boating|sailing}. Similarly, to search for videos matching either 
#' "boating" or "sailing" but not "fishing", set the query parameter value to 
#' \code{boating|sailing -fishing}.
#' @param n 
#' Number of results to fecth. The default value is \code{50}.
#' @param type 
#' Restricts a search query to only retrieve a particular type of resource. 
#' The value is a comma-separated list of resource types. The default value is 
#' \code{any} (includes \code{channel}, \code{video} and \code{playlist}).
#' @param order 
#' Specifies the method that will be used to order resources in the API 
#' response. The default value is \code{relevance}. 
#' See \code{\link{findParams}} for valid values.
#' @param video.dimension 
#' Restricts a search to only retrieve 2D or 3D videos. 
#' The default value is \code{any}. 
#' If you specify a value for this parameter, you must also set the \code{type} 
#' parameter's value to \code{video}.
#' See \code{\link{findParams}} for valid values.
#' @param video.caption 
#' Indicates whether the API should filter video search results based on 
#' whether they have captions. The default value is \code{any}. 
#' See \code{\link{findParams}} for valid values.
#' If you specify a value for this parameter, you must also set the \code{type} 
#' parameter's value to \code{video}.
#' @param video.category.id 
#' Filters video search results based on their 
#' \href{https://developers.google.com/youtube/v3/docs/videoCategories}{category}.
#' @param video.duration 
#' Filters video search results based on their duration. The default value is 
#' \code{any}. See \code{\link{findParams}} for valid values.
#' If you specify a value for this parameter, you must also set the \code{type} 
#' parameter's value to \code{video}.
#' @param video.embeddable 
#' Restricts a search to only videos that can be embedded into a webpage. 
#' The default value is \code{any}. See \code{\link{findParams}} for valid 
#' values.
#' If you specify a value for this parameter, you must also set the \code{type} 
#' parameter's value to \code{video}.
#' @param video.syndicated 
#' Restricts a search to only videos that can be played outside youtube.com. 
#' The default value is \code{any}. See \code{\link{findParams}} for valid 
#' values.
#' If you specify a value for this parameter, you must also set the \code{type} 
#' parameter's value to \code{video}.
#' @param video.type 
#' Restricts a search to a particular type of videos. 
#' The default value is \code{any}. See \code{\link{findParams}} for valid 
#' values.
#' If you specify a value for this parameter, you must also set the \code{type} 
#' parameter's value to \code{video}.
#' @param video.definition 
#' Restricts a search to only include either high definition (HD) or standard 
#' definition (SD) videos. HD videos are available for playback in at least 
#' 720p, though higher resolutions, like 1080p, might also be available.
#' The default value is \code{any}. See \code{\link{findParams}} for valid 
#' values.
#' If you specify a value for this parameter, you must also set the \code{type} 
#' parameter's value to \code{video}.
#' @param video.license 
#' Filters search results to only include videos with a particular license. 
#' YouTube lets video uploaders choose to attach either the Creative Commons 
#' license or the standard YouTube license to each of their videos. 
#' The default value is \code{any}. See \code{\link{findParams}} for valid 
#' values.
#' If you specify a value for this parameter, you must also set the \code{type} 
#' parameter's value to \code{video}.
#' @param max.results 
#' Specifies the maximum number of results that should be returned 
#' by each API call. Acceptable values are \code{0} to \code{50}, inclusive. 
#' The default value is \code{50}.
#' @param location 
#' Must be used in conjunction with the locationRadius parameter, defines a circular 
#' geographic area and also restricts a search to videos that specify, in 
#' their metadata, a geographic location that falls within that area. The 
#' parameter value is a vector that specifies latitude and longitude coordinates 
#' e.g. \code{c(lat = 37.42307, long = -122.08427)} or 
#' \code{c(37.42307, -122.08427)}. The default value is \code{NULL}.
#' @param location.radius 
#' Must be used in conjunction with the location parameter, defines a circular 
#' geographic area. The parameter value must be a floating point number 
#' followed by a measurement unit. Valid measurement units are \code{m}, 
#' \code{km}, \code{ft}, and \code{mi}. For example, valid parameter values 
#' include \code{1500m}, \code{5km}, \code{10000ft}, and \code{0.75mi}. 
#' The API does NOT support parameter values larger than 1000 kilometers. 
#' The default value is \code{NULL}.
#' @param region.code 
#' Instructs the API to return search results for the specified country. The 
#' parameter value is an 
#' \href{http://www.iso.org/iso/country_codes/iso_3166_code_lists/country_names_and_code_elements.htm}{ISO 3166-1 alpha-2} 
#' country code. The default value is \code{NULL}.
#' @param safe.search 
#' Indicates whether the search results should include restricted content as 
#' well as standard content. 
#' The default value is \code{none}. See \code{\link{findParams}} for valid 
#' values.
#' @param event.type 
#' Restricts a search to broadcast events. 
#' The default value is \code{NULL}. See \code{\link{findParams}} for valid 
#' values.
#' If you specify a value for this parameter, you must also set the \code{type} 
#' parameter's value to \code{video}.
#' @param published.before 
#' Indicates that the API response should only contain resources created 
#' before the specified time. Can be either of class \code{Date} ("%Y-%m-%d") 
#' or \code{POSIXct} or \code{POSIXlt} with time, or an RFC 3339 formatted 
#' date-time value (i.e.: \code{1970-01-01T00:00:00Z}). 
#' The default value is \code{Sys.time()}.
#' @param published.after 
#' Indicates that the API response should only contain resources created after 
#' the specified time. Can be either of class \code{Date} ("%Y-%m-%d") 
#' or \code{POSIXct} or \code{POSIXlt} with time, or an RFC 3339 formatted 
#' date-time value (i.e.: \code{1970-01-01T00:00:00Z}). 
#' The default value is \code{NULL}.
#' @param channel.id 
#' Indicates that the API response should only contain resources created by 
#' the channel. 
#' @param relevance.language 
#' Instructs the API to return search results that are most relevant to the 
#' specified language. The parameter value is typically an 
#' \href{http://www.loc.gov/standards/iso639-2/php/code_list.php}{ISO 639-1 two-letter language code}. 
#' However, you should use the values \code{zh-Hans} for simplified Chinese 
#' and \code{zh-Hant} for traditional Chinese. Please note that results in 
#' other languages will still be returned if they are highly relevant to the 
#' search query term.
#' @param topic.id 
#' Indicates that the API response should only contain resources associated 
#' with the specified topic. The value identifies a 
#' \href{https://developers.google.com/freebase/v1/topic-overview}{Freebase topic ID}.
#' @param verbose 
#' If \code{TRUE} prints infromational messages in the console. 
#' The default value is \code{FALSE}.
#' 
#' @details For more information on the parameters and the API call this 
#' function refers to please see 
#' \url{https://developers.google.com/youtube/v3/docs/search/list}. 
#' Some parameters are only applicable to \code{type=video} namely that which 
#' are preceded by \code{video.} and \code{event.type}.
#' 
#' @return Returns a collection of search results that match the query 
#' parameters specified in the API request.
#' 
#' @return Returns \code{data.frame}
#' 
#' @examples 
#' \dontrun{
#' # Authneticate
#' token <- youOauth(client.id = "something.apps.googleusercontent.com",
#'                   client.secret = "XxxXX1XxXxXxxx1xxx1xxXXX")
#' 
#' # search
#' cats <- searchVideos(token, query = "cats")
#' 
#' # filter 2d videos
#' 2d.cats <- searchVideos(token, query = "cats", 
#'                         video.dimension = findParams("video.dimension")[1],
#'                         type = "video")
#'                         
#' # find valid values for video.dimension
#' dims <- findParams(param = "video.dimension")
#' 
#' # fetch 3D cats videos from Belgium
#' dull.cats <- searchVideos(token, query = "cats", region.code = "BE",
#'                           video.dimension = dims[2], type = "video")
#'                           
#' # fetch "clean cat" channels from area 51 using coordinates
#' safety <- findParams("safe.search")[grep("strict", 
#'                                          findParams("safe.search"))]
#'                                          
#' clean.cats <- searchVideos(token, query = "clean cats", safe.search = safety,
#'                            location = c(lat = 37.2350, long = 115.8111),
#'                            location.radius = "5km", type = "channel")
#' }
#' 
#' @author John Coene \email{jcoenep@hotmail.com}
#' 
#' @seealso \code{\link{findParams}}, \code{link{youOAuth}}
#' 
#' @export
searchTube <- function(token, query, n = 50, type = "any", order = "relevance", 
                       video.dimension = NULL, video.caption = NULL, 
                       video.category.id = NULL, video.duration = NULL, 
                       video.embeddable = NULL, video.syndicated = NULL, 
                       video.type = NULL, video.definition = NULL, 
                       video.license = NULL, max.results = 50, location = NULL, 
                       location.radius = NULL, region.code = NULL,
                       safe.search = "none", event.type = NULL, 
                       published.before = Sys.time(), published.after = NULL,
                       channel.id = NULL, relevance.language = NULL,
                       topic.id = NULL, verbose = FALSE) {
  
  # check required arguments
  if (missing(query)) {
    stop("query is missing")
  }
  # check token
  checkToken(token)
  
  # check optional arguments
  # max results
  if(max.results > 50) {
    
    warning("max.results > 50, overriden to 50")
    max.results <- 50
    
  } else if (max.results == 0) {
    
    warning("max.results == 0, overriden to 5")
    max.results <- 5
  }
  
  # location
  if(length(location) && !length(location.radius)) {
    
    stop("location must be used in conjonction with location.radius")
    
  } else if (!length(location) && length(location.radius)) {
    
    stop("location.radius must be used in conjonction with location")
    
  } else if (length(location) && length(location.radius)) {
    
    location <- buildLocation(location)
    
  }
  
  # test type
  if(type != "video") {
    
    if(length(video.dimension) || length(video.caption) || 
       length(video.category.id) || length(video.type) || length(video.duration) || length(event.type) ||
       length(video.embeddable) || length(video.syndicated) || 
       length(video.license)) {
      
      warning("conflicting arguments, type set to 'video'. See @details.")
      
      type <- "&type=video"
      
    } else if(type == "channel" || type == "playlist") {
      
      type <- paste0("&type=", type)
      
    } else if (type == "any") {
      
      type <- NULL
      
    }
    
  } else if (type == "video") {
    
    type <- "&type=video"
    
  }
  
  # parameters to list
  arguments <- namedList(order, video.dimension, video.caption, video.category.id, 
                         video.duration, video.embeddable, video.syndicated, 
                         video.type, video.definition, video.license, location, 
                         location.radius, region.code, safe.search, event.type, 
                         channel.id, relevance.language, topic.id, max.results)
  
  # buildParameters
  x <- list()
  for (i in 1:length(arguments)) {
    y <- buildParam(param = names(arguments[i]), values = arguments[[i]])
    x[[i]] <- ifelse(!is.null(y), y, "")
  }
  
  # collapse
  suffix <- paste(x, collapse = "")
  
  # time
  published.before <- paste0("&publishedBefore=", buildTime(published.before))
  if (length(published.after)) {
    published.after <- paste0("&publishedAfter=", buildTime(published.after))
  }
  
  # query
  query <- buildTerms(query)
  
  # build uri
  uri <- paste0("https://www.googleapis.com/youtube/v3/search?part=snippet",
                type, query, suffix, published.after, published.before)
  
  # GET
  response <- httr::GET(uri, config = (token = token))
  
  # parse
  json <- jsonlite::fromJSON(rawToChar(response$content),
                             simplifyDataFrame = F)
  
  # check if error
  if(length(json$error)) {
    stop(paste0("API returned the following error (code ", 
                json$error$code,"): ", 
                json$error$message))
    
    # else parse
  } else {
    
    dat <- paginate(response, n, verbose)
    
  }
  
  if(verbose == TRUE){
    cat(paste0(n, " results queried, API returned ", nrow(dat),
               " results."))
  }
  
  return(dat)
  
}