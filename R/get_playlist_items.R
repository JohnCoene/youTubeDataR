#' getPlaylistItems
#' 
#' @description Returns a collection of playlist items that match the API 
#' request parameters.
#' 
#' @param token 
#' Your token as returned by \code{\link{youOAuth}}.
#' @param part 
#' The part parameter specifies a comma-separated list of one or more activity 
#' resource properties that the API response will include. The default value 
#' is \code{snippet}, see \code{\link{findParts}} or 
#' \href{https://developers.google.com/youtube/v3/docs/channels/list}{official documentation} 
#' for all valid values.
#' @param n 
#' Number of results to fecth. The default value is \code{50}.
#' @param id 
#' The id parameter specifies a comma-separated list of one or more unique 
#' playlist item IDs.
#' @param playlist.id 
#' Specifies the unique ID of the playlist for which you want to retrieve 
#' playlist items. Note that even though this is an optional parameter, 
#' every request to retrieve playlist items must specify a value for either 
#' the id parameter or the \code{playlist.id} parameter.
#' @param max.results 
#' Specifies the maximum number of results that should be returned 
#' by each API call. Acceptable values are \code{0} to \code{50}, inclusive. 
#' The default value is \code{50}.
#' @param on.behalf.of.content.owner 
#' This parameter can only be used in a properly authorized request. Note: This 
#' parameter is intended exclusively for YouTube content partners. 
#' Indicates that the request's authorization credentials identify a YouTube 
#' CMS user who is acting on behalf of the content owner specified in the 
#' parameter value. This parameter is intended for YouTube content partners 
#' that own and manage many different YouTube channels. It allows content 
#' owners to authenticate once and get access to all their video and channel 
#' data, without having to provide authentication credentials for each 
#' individual channel. The CMS account that the user authenticates with must 
#' be linked to the specified YouTube content owner.
#' @param video.id 
#' Specifies that the request should return only the playlist items that 
#' contain the specified video.
#' @param verbose 
#' If \code{TRUE} prints infromational messages in the console. 
#' The default value is \code{FALSE}.
#' 
#' @details Must pass either \code{id} or \code{playlist.id}
#' 
#' @examples 
#' \dontrun{
#' # Authenticate
#' token <- youOAuth(client.id = "something.apps.googleusercontent.com",
#'                   client.secret = "XxxXX1XxXxXxxx1xxx1xxXXX")
#' 
#' # search playlists
#' search <- searchTube(token, query = "cats", type = "playlist")
#' 
#' # sample playlist id
#' id <- sample(search$id.playlistId, 1)
#' 
#' # fetch
#' items <- getPlaylistItems(token, playlist.id = id)
#' }
#' 
#' @export
#' 
#' @author John Coene \email{jcoenep@hotmail.com}
getPlaylistItems <- function(token, part = "snippet", n = 50, id, 
                             playlist.id, max.results = 50, 
                             on.behalf.of.content.owner = NULL, 
                             video.id = NULL, verbose = FALSE) {
  
  if(missing(id)) id <- NULL
  if(missing(playlist.id)) playlist.id <- NULL
  
  # check token
  checkToken(token)
  
  # check required inputs
  if(!length(id) && !length(playlist.id)) {
    stop("must use id or playlist.id")
  } else if(length(id) && length(playlist.id)) {
    stop("must supply one of id or playlist.id")
  }
  
  arguments <- namedList(max.results, on.behalf.of.content.owner,
                         video.id, id, playlist.id)
  
  # buildParameters
  x <- list()
  for (i in 1:length(arguments)) {
    y <- buildParam(param = names(arguments[i]), values = arguments[[i]])
    x[[i]] <- ifelse(!is.null(y), y, "")
  }
  
  # collapse
  suffix <- paste(x, collapse = "")
  
  # test part
  testPart("getPlaylistItems", part)
  
  # build uri
  uri <- paste0("https://www.googleapis.com/youtube/v3/playlistItems",
                "?part=", part, suffix)
  
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
    
    # else paginate
  } else {
    
    dat <- paginate(response, n, verbose, token)
    
  }
  
  if(verbose == TRUE && nrow(dat)){
    cat(paste0(n, " results queried, API returned ", nrow(dat),
               " results."))
  }
  
  return(dat)
  
}