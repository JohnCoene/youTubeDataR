#' getChannels
#' 
#' @description Returns a collection of zero or more channel resources that 
#' match the request criteria.
#' 
#' @param token 
#' Your token as returned by \code{\link{youOAuth}}.
#' @param n 
#' Number of results to fecth. The default value is \code{50}.
#' @param part 
#' The part parameter specifies a comma-separated list of one or more activity 
#' resource properties that the API response will include. The default value 
#' is \code{snippet}, see \code{\link{findParts}} or 
#' \href{https://developers.google.com/youtube/v3/docs/channels/list}{official documentation} 
#' for all valid values.
#' @param category.id 
#' specifies a YouTube guide category, thereby requesting YouTube channels 
#' associated with that category. The default value is \code{NULL}.
#' @param for.username 
#' Specifies a YouTube username, thereby requesting the channel associated 
#' with that username. The default value is \code{NULL}.
#' @param id 
#' specifies a comma-separated list of the YouTube channel ID(s) for the 
#' resource(s) that are being retrieved. In a channel resource, the id property 
#' specifies the channel's YouTube channel ID. The default value is \code{NULL}
#' @param managed.by.me 
#' This parameter can only be used in a properly authorized request. Note: This 
#' parameter is intended exclusively for YouTube content partners. See scopes 
#' under \code{\link{youOAuth}}.
#' Set this parameter's value to true to instruct the API to only return 
#' channels managed by the content owner that the 
#' \code{on.behalf.of.content.owner} parameter specifies. 
#' The user must be authenticated as a CMS account linked to the specified 
#' content owner and \code{on.behalf.of.content.owner} must be provided.
#' @param mine 
#' Set this parameter's value to true to retrieve a feed of the authenticated 
#' user's activities. The default value is \code{FALSE}.
#' @param hl 
#' The hl parameter instructs the API to retrieve localized resource metadata 
#' for a specific application language that the YouTube website supports. The 
#' parameter value must be a language code included in the list returned by 
#' \code{\link{getLanguages}}.
#' @param max.results 
#' Specifies the maximum number of results that should be returned 
#' by each API call. Acceptable values are \code{0} to \code{50}, inclusive. 
#' The default value is \code{50}.
#' @param on.behalf.of.content.owner 
#' Indicates that the request's 
#' authorization credentials identify a YouTube CMS user who is acting on 
#' behalf of the content owner specified in the parameter value. This parameter 
#' is intended for YouTube content partners that own and manage many different 
#' YouTube channels. It allows content owners to authenticate once and get 
#' access to all their video and channel data, without having to provide 
#' authentication credentials for each individual channel. The actual CMS 
#' account that the user authenticates with must be linked to the specified 
#' YouTube content owner. This parameter can only be used in a properly 
#' authorized request. Note: This parameter is intended exclusively for 
#' YouTube content partners. See scope under \code{\link{youOAuth}}. 
#' The default value is \code{NULL}.
#' @param verbose 
#' If \code{TRUE} prints infromational messages in the console. 
#' The default value is \code{FALSE}.
#' 
#' @details MUST specify at least one of the following:
#' \itemize{
#' \item \code{category.id}
#' \item \code{for.username}
#' \item \code{id}
#' \item \code{managed.by.me} (\code{TRUE})
#' \item \code{mine} (\code{TRUE})
#' }
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
#' # pick random id
#' set.seed(19880525)
#' chan <- sample(search$id.channelId, 1)
#' 
#' # fetch
#' chan.dat <- getChannels(token, id = chan)
#' }
#' 
#' @export
#' 
#' @author John Coene \email{jcoenep@@hotmail.com}
getChannels <- function(token, n = 50, part = "snippet", category.id, 
                        for.username, id, managed.by.me = FALSE, 
                        mine = FALSE, hl = NULL, max.results = 50, 
                        on.behalf.of.content.owner = NULL, verbose = FALSE) {
  
  if(missing(category.id)) category.id <- NULL
  if(missing(for.username)) for.username <- NULL
  if(missing(id)) id <- NULL
  
  # check token
  checkToken(token)
  
  # check required inputs
  if (is.null(category.id) && is.null(for.username) && is.null(id) && 
      managed.by.me == FALSE && mine == FALSE) {
    
    stop(paste0("must specify one of category.id, for.username, id",
                ", managed.by.me or mine"))
    
  } else {
    
    c <- length(category.id) + length(for.username) + length(id) + 
      managed.by.me + mine
    
    if(c > 1) {
      
      stop(paste0("can only specify one of category.id, for.username, id",
                  ", managed.by.me or mine"))
      
    } else {
      
      # mine
      if (mine == TRUE) {
        mine <- paste0("&mine=true")
      } else {
        mine <- NULL
      }
      
      # content owner and managed by me
      if(managed.by.me == TRUE && !length(on.behalf.of.content.owner)) {
        stop(paste0("managed.by.me must be used in conjonction with",
                    " on.behalf.of.content.owner"))
      } else if (length(managed.by.me) && length(on.behalf.of.content.owner)) {
        managed.by.me <- "true"
      }
      
      if (managed.by.me == FALSE) {
        managed.by.me <- NULL
      }
      
    }
    
  }
  
  # check optional arguments
  # max results
  if(max.results > 50) {
    
    warning("max.results > 50, overriden to 50")
    max.results <- 50
    
  } else if (max.results == 0) {
    
    warning("max.results == 0, overriden to 5")
    max.results <- 5
  }
  
  # parameters to list
  arguments <- namedList(category.id, for.username, id, managed.by.me, 
                         hl, max.results, on.behalf.of.content.owner)
  
  # buildParameters
  x <- list()
  for (i in 1:length(arguments)) {
    y <- buildParam(param = names(arguments[i]), values = arguments[[i]])
    x[[i]] <- ifelse(!is.null(y), y, "")
  }
  
  # collapse
  suffix <- paste(x, collapse = "")
  
  testPart("getChannels", part)
  
  # build uri
  uri <- paste0("https://www.googleapis.com/youtube/v3/channels?part=", part,
                suffix, mine)
  
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
    
    dat <- paginate(response , n, verbose)
    
  }
  
  if(verbose == TRUE && nrow(dat)){
    cat(paste0(n, " results queried, API returned ", nrow(dat),
               " results."))
  }
  
  return(dat)
  
}