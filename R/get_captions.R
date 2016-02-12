#' getCaptions
#' 
#' @description Returns a list of caption tracks that are associated with a 
#' specified video.
#' 
#' @param token 
#' Your token as returned by \code{\link{youOAuth}}.
#' @param video.id 
#' specifies the YouTube video ID of the video for which the API should return 
#' caption tracks. Required.
#' @param part 
#' specifies the caption resource parts that the API response will include. 
#' The default vlaue is \code{snippet}, can take any of \code{id} or 
#' \code{snippet}. See \code{\link{findParts}}.
#' @param id 
#' The id parameter specifies a comma-separated list of IDs that identify the 
#' caption resources that should be retrieved. Each ID must identify a caption 
#' track associated with the specified video. The default value is \code{NULL}.
#' @param on.behalf.of.content.owner 
#' The \code{on.behalf.of.content.owner} parameter indicates that the request's 
#' authorization credentials identify a YouTube CMS user who is acting on 
#' behalf of the content owner specified in the parameter value. This parameter 
#' is intended for YouTube content partners that own and manage many different 
#' YouTube channels. It allows content owners to authenticate once and get 
#' access to all their video and channel data, without having to provide 
#' authentication credentials for each individual channel. The actual CMS 
#' account that the user authenticates with must be linked to the specified 
#' YouTube content owner. This parameter can only be used in a properly 
#' authorized request. Note: This parameter is intended exclusively for 
#' YouTube content partners. See scopes under \code{\link{youOAuth}}. 
#' The default value is \code{NULL}.
#' 
#' @details See scope in \code{\link{youOAuth}}. Required authorisation: 
#' \itemize{
#' \item \code{force-ssl}
#' \item \code{partner-channel-audit}
#' }
#' 
#' @examples 
#' \dontrun{
#' # Authenticate
#' token <- youOAuth(client.id = "something.apps.googleusercontent.com",
#'                   client.secret = "XxxXX1XxXxXxxx1xxx1xxXXX")
#'                   
#' # search videos about cats
#' search <- searchTube(token, query = "cats", type = "video")
#' 
#' # random channel id
#' set.seed(19880525)
#' vid <- sample(search$snippet.id.videoId, 1)
#' 
#' # fetch data
#' act <- getActivities(token, video.id = vid)
#' }
#' 
#' @seealso \code{link{youOAuth}}, \code{\link{findParts}}
#' 
#' @author John Coene \email{jcoenep@@hotmail.com}
#' 
#' @export
getCaptions <- function(token, video.id, part = "snippet", id = NULL,
                        on.behalf.of.content.owner = NULL) {
  
  if(missing(video.id)) video.id <- NULL
  
  # check required arguments
  if (is.null(video.id)) {
    stop("video.id is missing")
  }
  # check token
  checkToken(token)
  
  # parameters to list
  arguments <- namedList(on.behalf.of.content.owner, video.id)
  
  # buildParameters
  x <- list()
  for (i in 1:length(arguments)) {
    y <- buildParam(param = names(arguments[i]), values = arguments[[i]])
    x[[i]] <- ifelse(!is.null(y), y, "")
  }
  
  # collapse
  suffix <- paste(x, collapse = "")
  
  testPart("getCaptions", part)
  
  # build uri
  uri <- paste0("https://www.googleapis.com/youtube/v3/captions?part=", part, 
                suffix)
  
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
    
    dat <- do.call(plyr::"rbind.fill", lapply(json$items, as.data.frame))
  
  }
  
  return(dat)
  
}