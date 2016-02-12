#' getSubscriptions
#' 
#' @description Returns subscription resources that match the API request 
#' criteria.
#' 
#' @param token 
#' Your token as returned by \code{\link{youOAuth}}.
#' @param part 
#' The part parameter specifies a comma-separated list of one or more activity 
#' resource properties that the API response will include. The default value 
#' is \code{snippet}, see \code{\link{findParts}} for all valid values.
#' @param n 
#' Number of results to fecth. The default value is \code{50}.
#' @param mine 
#' Set this parameter's value to true to retrieve a feed of the authenticated 
#' user's subscriptions. The default value is \code{FALSE}
#' @param max.results 
#' Specifies the maximum number of results that should be returned 
#' by each API call. Acceptable values are \code{0} to \code{50}, inclusive. 
#' The default value is \code{50}.
#' @param for.channel.id 
#' Specifies a comma-separated list of channel IDs. The API response will then 
#' only contain subscriptions matching those channels.
#' @param id 
#' Specifies a comma-separated list of the YouTube subscription ID(s) for the 
#' resource(s) that are being retrieved. In a subscription resource, the id 
#' property specifies the YouTube subscription ID.
#' @param order 
#' Specifies the method that will be used to sort resources in the API 
#' response. The default value is \code{relevance}, see 
#' \code{\link{findParams}}.
#' @param channel.id 
#' Specifies a YouTube channel ID. The API will only return that channel's 
#' subscriptions.
#' @param my.subscribers
#' This parameter can only be used in a properly authorized request. Set this 
#' parameter's value to \code{TRUE} to retrieve a feed of the subscribers of 
#' the authenticated user.
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
#' linked to the specified YouTube content owner. The default value is 
#' \code{NULL}, must be used in conjonction with 
#' \code{on.behalf.of.content.owner.channel.channel}
#' @param on.behalf.of.content.owner.channel
#' This parameter can only be used in a properly authorized request. 
#' Note: This parameter is intended exclusively for YouTube content partners.
#' Specifies the YouTube channel ID of the channel to which a video is being 
#' added. This parameter is required when a request specifies a value for the 
#' \code{on.behalf.of.content.owner} parameter, and it can only be used in 
#' conjunction with that parameter. In addition, the request must be authorized 
#' using a CMS account that is linked to the content owner that the 
#' \code{on.behalf.of.content.owner} parameter specifies. Finally, the channel 
#' that the \code{on.behalf.of.content.owner.channel} parameter value specifies 
#' must be linked to the content owner that the 
#' \code{on.behalf.of.content.owner} parameter specifies.
#' @param verbose 
#' If \code{TRUE} prints infromational messages in the console. 
#' The default value is \code{FALSE}.
#' 
#' @details Must specify one (and only one) \code{channel.id}, \code{id}, 
#' \code{mine} (\code{TRUE}), \code{my.subscribers} (\code{TRUE})
#' 
#' Official documentation on this API call; 
#' \url{https://developers.google.com/youtube/v3/docs/playlists/list}
#' 
#' @examples 
#' \dontrun{
#' # Authenticate
#' token <- youOAuth(client.id = "something.apps.googleusercontent.com",
#'                   client.secret = "XxxXX1XxXxXxxx1xxx1xxXXX")
#'                   
#' # search channels
#' search <- searchTube(token, query = "cats", type = "channel")
#' 
#' # pick random channel
#' set.seed(19880525)
#' chan <- sample(search$snippet.id.channelId, 1)
#' 
#' # fetch subs
#' subs <- getSubscriptions(token, channel.id = chan)
#' }
#' 
#' @author John Coene \email{jcoenep@@hotmail.com}
#' 
#' @export
getSubscriptions <- function(token, part = "snippet", channel.id, id, 
                             mine = FALSE, my.subscribers = FALSE, 
                             max.results = 50, n = 50, for.channel.id = NULL, 
                             on.behalf.of.content.owner = NULL,
                             order = "relevance", 
                             on.behalf.of.content.owner.channel = NULL, 
                             verbose = FALSE) {
  
  if(missing(channel.id)) channel.id <- NULL
  if(missing(id)) id <- NULL
  
  checkToken(token)
  
  if(!length(channel.id) && !length(id) && mine == FALSE && 
     my.subscribers == FALSE) {
    stop("must specify channel.id, id, mine or my.subscribers")
  } else {
    
    c <- length(channel.id) + length(id) + mine + my.subscribers
    
    if(c > 1) {
      stop("can only specify one of id, channel.id or my.subscribers")
    }
    
  }
  
  if(mine == TRUE) {
    mine <- "&mine=true"
  } else {
    mine <- NULL
  }
  
  if(my.subscribers == TRUE) {
    my.subscribers <- "&mySubscribers=true"
  } else {
    my.subscribers <- NULL
  }
  
  # check if used in conjonction with
  if(length(on.behalf.of.content.owner) &&
     !length(on.behalf.of.content.owner.channel)){
    stop(paste0("must use on.behalf.of.content.owner in conjonction with",
                "on.behalf.of.content.owner.channel"))
  }
  
  arguments <- namedList(channel.id, id, max.results, for.channel.id, 
                         on.behalf.of.content.owner, 
                         on.behalf.of.content.owner.channel)
  
  # buildParameters
  x <- list()
  for (i in 1:length(arguments)) {
    y <- buildParam(param = names(arguments[i]), values = arguments[[i]])
    x[[i]] <- ifelse(!is.null(y), y, "")
  }
  
  # collapse
  suffix <- paste(x, collapse = "")
  
  testPart("getSubscriptions", part)
  
  # build uri
  uri <- paste0("https://www.googleapis.com/youtube/v3/subscriptions?part=", 
                part, suffix, mine, my.subscribers)
  
  # GET
  response <- httr::GET(uri, config = (token = token))
  
  # parse
  json <- jsonlite::fromJSON(rawToChar(response$content),
                             simplifyDataFrame = FALSE)
  
  # check if error
  if(length(json$error)) {
    stop(paste0("API returned the following error (code ", 
                json$error$code,"): ", 
                json$error$message))
    
    # else parse
  } else {
    
    dat <- paginate(response , n, verbose)
    
  }
  
  if(verbose == TRUE && nrow(dat)){
    cat(paste0(n, " results queried, API returned ", nrow(dat),
               " results."))
  }
  
  return(dat)
  
}