#' getChannelSections
#' 
#' @description Returns a list of channelSection resources that match the API 
#' request criteria.
#' 
#' @param token 
#' Your token as returned by \code{\link{youOAuth}}.
#' @param channel.id 
#' Indicates that the API response should only contain resources created by 
#' the channel. The default value is \code{NULL}.
#' @param part 
#' The part parameter specifies a comma-separated list of one or more activity 
#' resource properties that the API response will include. The default value 
#' is \code{snippet}, can take any of \code{contentDetails}, \code{id} or 
#' \code{snippet}. See \code{\link{findParts}}.
#' @param mine 
#' Set this parameter's value to true to retrieve a feed of the authenticated 
#' user's activities. The default value is \code{FALSE}.
#' @param id 
#' Specifies a comma-separated list of IDs that uniquely identify the 
#' channelSection resources that are being retrieved. In a 
#' \code{getChannelSections} resource, the id property specifies the section's 
#' ID.
#' @param hl 
#' The hl parameter instructs the API to retrieve localized resource metadata 
#' for a specific application language that the YouTube website supports. The 
#' parameter value must be a language code included in the list returned by 
#' \code{\link{getLanguages}}
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
#' @details Must specify one (and only one) of \code{mine} (\code{TRUE}), 
#' \code{id} or \code{channel.id}.
#' 
#' @examples 
#' \dontrun{
#' #Authenticate
#' token <- youOAuth(client.id = "something.apps.googleusercontent.com",
#'                   client.secret = "XxxXX1XxXxXxxx1xxx1xxXXX")
#' 
#' # search channels on cats
#' search <- searchTube(token, query = "cats", type = "channel")
#' 
#' # get channel sections
#' sections <- getChannelSections(token, 
#'                                channel.id = search$snippet.channelId [1])
#' }
#' 
#' @export
#' 
#' @author John Coene \email{jcoenep@@hotmail.com}
getChannelSections <- function(token, channel.id, part = "snippet", 
                               mine = FALSE, id, hl = NULL, 
                               on.behalf.of.content.owner = NULL, 
                               verbose = FALSE) {
  
  if(missing(channel.id)) channel.id <- NULL
  if(missing(id)) id <- NULL
  
  # check required arguments
  # check token
  checkToken(token)
  if(is.null(channel.id) && mine == FALSE && is.null(id)) {
    stop("must provide channel.id or mine or id")
  } else {
    
    c <- mine + length(id) + length(channel.id)
    
    if(c > 1) {
      
      stop("can only specify one of id, mine or channel.id")
      
    } else {
      
      # mine
      if (mine == TRUE) {
        mine <- paste0("&mine=true")
      } else {
        mine <- NULL
      }
      
    }
    
  }
  
  arguments <- namedList(channel.id, hl, on.behalf.of.content.owner, id)
  
  # buildParameters
  x <- list()
  for (i in 1:length(arguments)) {
    y <- buildParam(param = names(arguments[i]), values = arguments[[i]])
    x[[i]] <- ifelse(!is.null(y), y, "")
  }
  
  # collapse
  suffix <- paste(x, collapse = "")
  
  testPart("getChannelSections", part)
  
  # build uri
  uri <- paste0("https://www.googleapis.com/youtube/v3/channelSections?part=",
                part, suffix, mine)
  
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
    
    dat <- do.call(plyr::"rbind.fill", lapply(json$items, as.data.frame))
    
  }
  
  if(verbose == TRUE && nrow(dat)){
    cat(paste0("API returned ", nrow(dat),
               " results."))
  }
  
  return(dat)
  
}