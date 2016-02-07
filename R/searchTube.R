#' searchTube
#' 
#' @description Search YouTube videos or playlists
#' 
#' @param token 
#' Your OAuth token see \code{link{youOAuth}}, required.
#' @param query 
#' Your search Query, required.
#' @param type 
#' Content to search, either \code{video} (default), \code{channel} or 
#' \code{playlist}, required.
#' @param video.dimension 
#' Restrict a search to only retrieve 2D or 3D videos. 
#' Can be \code{2d}, \code{3d} or \code{any} (default). 
#' \code{type} must also be set to \code{video}.
#' @param video.caption 
#' Filter video search results based on whether they 
#' have captions, \code{type} must also be set to \code{video}.
#' @param video.category.id 
#' Filters video search results based on their category, \code{type} must also 
#' be set to \code{video}.
#' @param video.duration 
#' Filters video search results based on their duration, 
#' \code{type} must also be set to \code{video}.
#' @param video.definition 
#' restrict a search to only include either high definition (HD) or standard 
#' definition (SD) videos. HD videos are available for playback in at least 
#' 720p, though higher resolutions, like 1080p, might also be available, 
#' \code{type} must also be set to \code{video}.
#' @param video.license 
#' filters search results to only include videos with a particular license. 
#' YouTube lets video uploaders choose to attach either the Creative Commons 
#' license or the standard YouTube license to each of their videos, 
#' \code{type} must also be set to \code{video}.
#' @param location 
#' The location parameter, in conjunction with the locationRadius parameter, 
#' defines a circular geographic area and also restricts a search to videos 
#' that specify, in their metadata, a geographic location that falls within 
#' that area. 
#' @param location.radius 
#' The locationRadius parameter, in conjunction with the location parameter, 
#' defines a circular geographic area in kilometers. The API does not support 
#' radius values larger than 1000 kilometers.
#' @param order 
#' Method used to order resources in the API response. defaults to 
#' \code{relevance} see \code{link{findParams}}
#' @param region.code 
#' Return search results for the specified country. The parameter value is an 
#' \href{https://www.iso.org/obp/ui/#search/code/}{ISO 3166-1 alpha-2} 
#' country code (i.e.: \code{US}).
#' @param safe.search 
#' Whether the search results should include restricted content as well as 
#' standard content. Can be one of \code{moderate}, \code{none} (default) or 
#' \code{strict}
#' @param event.type 
#' Restricts a search to broadcast events, can be \code{completed}, 
#' \code{live} or \code{upcoming}. 
#' \code{type} must also be set to \code{video}.
#' @param published.before 
#'  indicates that the API response should only contain resources created 
#'  before the specified time. Can be either of class \code{Date} ("%Y-%m-%d") 
#' or \code{POSIXct} or \code{POSIXlt} with time, or an RFC 3339 formatted 
#' date-time value (i.e.: \code{(1970-01-01T00:00:00Z}).
#' @param published.after 
#' indicates that the API response should only contain resources created after 
#' the specified time. Can be either of class \code{Date} ("%Y-%m-%d") 
#' or \code{POSIXct} or \code{POSIXlt} with time, or an RFC 3339 formatted 
#' date-time value (i.e.: \code{(1970-01-01T00:00:00Z}).
#' @param channel.id 
#' Filter by channel. 
#' Note: Search results are constrained to a maximum of 500 videos if your 
#' request specifies a value for the channelId parameter and sets the type 
#' parameter value to video
#' 
#' @details Visit the official documentation on the parameters for more 
#' information; 
#' \url{https://developers.google.com/youtube/v3/docs/search/list}
#' 
#' Visit the official documention on the actual API call this function refers 
#' to; 
#' \url{https://developers.google.com/youtube/v3/guides/implementation/search}
#' 
#' @export
#' 
#' @author John Coene <jcoenep@@hotmai.com>
searchTube <- function(token, query = "cats", type = "video", 
                       video.dimensions = "any", video.caption, video.category.id, 
                       video.duration, video.definition, video.license, 
                       location, location.radius, order = "relevance",
                       region.code, safe.search = "none", event.type, 
                       published.before, published.after, channel.id){
  
  # check required arguments
  checkToken(token)
  checkQuery(query)
  
  # check optional arguments
  if(missing(video.caption)) video.caption <- NULL
  if(missing(video.category.id)) video.category.id <- NULL
  if(missing(video.duration)) video.duration <- NULL
  if(missing(video.definition)) video.definition <- NULL
  if(missing(video.license)) video.license <- NULL
  if(missing(location)) location <- NULL
  if(missing(location.radius)) location.radius <- NULL
  if(missing(region.code)) region.code <- NULL
  if(missing(event.type)) event.type <- NULL
  if(missing(published.before)) published.before <- NULL
  if(missing(published.after)) published.after <- NULL
  if(missing(channel.id)) channel.id <- NULL
  
  # performs checks
  video.caption <- buildParam(param = "video.caption", values = video.caption)
  video.duration <- buildParam(param = "video.duration", values = video.duration)
  video.definition <- buildParam(param = "video.definition", values = video.definition)
  video.license <- buildParam(param = "video.license", values = video.license)
  order <- buildParam(param = "order", values = order)
  video.dimensions <- testParam(param = "video.dimensions", values = video.dimensions)
  testParam(param = "safe.search", values = safe.search)
  testParam(param = "type", values = type)
  testParam(param = "event.type", values = event.type)
  published.before <- buildTime(published.before)
  published.after <- buildTime(published.after)
  
  uri <- paste0("https://www.googleapis.com/youtube/v3/search?",
                "part=snippet", "video", 
                video.dimensions, video.caption, video.category.id, 
                video.duration, video.definition, video.license, 
                location, location.radius, order,
                region.code, safe.search, event.type, 
                published.before, published.after, channel.id)
  
  response <- GET(uri, config = (token = token))
  
}