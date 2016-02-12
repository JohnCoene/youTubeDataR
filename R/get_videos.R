#' getVideos
#' 
#' @description Returns a list of videos that match the API request parameters.
#' 
#' @param token 
#' Your token as returned by \code{\link{youOAuth}}.
#' @param part 
#' The part parameter specifies a comma-separated list of one or more activity 
#' resource properties that the API response will include. The default value 
#' is \code{snippet}, see \code{link{findParts}} for all valid values.
#' @param n 
#' Number of results to fecth. The default value is \code{50}.
#' @param max.results 
#' Specifies the maximum number of results that should be returned 
#' by each API call. Acceptable values are \code{0} to \code{50}, inclusive. 
#' The default value is \code{50}.
#' @param chart 
#' The chart parameter identifies the chart that you want to retrieve. Only 
#' valid value is \code{mostPopular} or \code{NULL}. 
#' The default value if \code{mostPopular}.
#' @param id 
#' Specifies a comma-separated list of the YouTube video ID(s) for the 
#' resource(s) that are being retrieved. In a video resource, the id property 
#' specifies the video's ID.
#' @param on.behalf.of.content.owner 
#' This parameter can only be used in a properly authorized request. Note: This 
#' parameter is intended exclusively for YouTube content partners.
#' Indicates that the request's authorization credentials identify a YouTube 
#' CMS user who is acting on behalf of the content owner specified in the 
#' parameter value. This parameter is intended for YouTube content partners 
#' that own and manage many different YouTube channels. It allows content 
#' owners to authenticate once and get access to all their video and channel 
#' data, without having to provide authentication credentials for each 
#' individual channel. The CMS account that the user authenticates with must be 
#' linked to the specified YouTube content owner.
#' @param video.category.id 
#' Identifies the video category for which the chart should be retrieved. 
#' This parameter can only be used in conjunction with the chart parameter. 
#' By default, charts are not restricted to a particular category. The default 
#' value is 0.
#' @param region.code 
#' Instructs the API to select a video chart available in the specified region. 
#' This parameter can only be used in conjunction with the chart parameter. 
#' The parameter value is an ISO 3166-1 alpha-2 country code.
#' @param my.rating 
#' This parameter can only be used in a properly authorized request. 
#' Set this parameter's value to \code{like} or \code{dislike} to instruct the 
#' API to only return videos liked or disliked by the authenticated user.
#' @param verbose 
#' If \code{TRUE} prints infromational messages in the console. 
#' The default value is \code{FALSE}.
#' @param hl 
#' Specifies the language that will be used for text values in the API 
#' response. The default value (from YouTube) is \code{NULL}. See 
#' \code{\link{getLanguages}}. If localized resource details are available 
#' in that language, the resource's \code{snippet} (\code{part = "snippet"}). 
#' 
#' @details Must specify one (and only one) of \code{chart}, \code{id} or 
#' \code{my.rating}
#' 
#' @examples 
#' \dontrun{
#' # Authenticate
#' token <- youOAuth(client.id = "something.apps.googleusercontent.com",
#'                   client.secret = "XxxXX1XxXxXxxx1xxx1xxXXX")
#' 
#' # get videos
#' videos <- getVideos(token)
#' }
#' 
#' @author John Coene \email{jcoenep@@hotmail.com}
#' 
#' @export
getVideos <- function(token, part = "snippet", n = 50 , chart = "mostPopular",
                      id, my.rating, hl = NULL, max.results = 50, 
                      on.behalf.of.content.owner = NULL, region.code = NULL,
                      video.category.id = NULL, verbose = FALSE) {
  
  checkToken(token)
  
  if(missing(id)) id <- NULL
  if(missing(my.rating)) my.rating <- NULL
  
  # check required parameters
  if(!length(chart) && !length(id) && !length(my.rating)) {
    stop("must specify either chart, id or my.rating")
  } else {
    
    c <- length(chart) + length(id) + length(my.rating)
    
    if(c > 1) {
      stop("can only one: id, my.rating, chart")
    }
    
    if (length(chart) && chart != "mostPopular") {
      stop("mostPopular is the only valid value for chart")
    }
    
  }
  
  arguments <- namedList(id, my.rating, chart, hl, max.results, 
                         on.behalf.of.content.owner, region.code, 
                         video.category.id)
  
  if(length(video.category.id) && !length(chart)) {
    stop("video.category.id must be used in conjonction with chart")
  } else if (length(region.code) && !length(chart)) {
    stop("region.code must be used in conjonction with chart")
  }
  
  # check optional arguments
  # max results
  if(length(max.results)) {
    if(max.results > 50) {
      
      warning("max.results > 50, overriden to 100")
      max.results <- 50
      
    } else if (max.results == 0) {
      
      warning("max.results == 0, overriden to 5")
      max.results <- 5
    }
  }
  
  # buildParameters
  x <- list()
  for (i in 1:length(arguments)) {
    y <- buildParam(param = names(arguments[i]), values = arguments[[i]])
    x[[i]] <- ifelse(!is.null(y), y, "")
  }
  
  # collapse
  suffix <- paste(x, collapse = "")
  
  testPart("getVideos", part)
  
  # build uri
  uri <- paste0("https://www.googleapis.com/youtube/v3/videos?part=", 
                part, suffix)
  
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
    
    if(length(id)) {
      
      dat <- paginate(response , n, verbose)
      
    } else {
      
      dat <- do.call(plyr::"rbind.fill", lapply(json$items, as.data.frame))
      
    }
    
    
  }
  
  if(verbose == TRUE && nrow(dat)){
    cat(paste0(n, " results queried, API returned ", nrow(dat),
               " results."))
  }
  
  return(dat)
  
}