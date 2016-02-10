#' getCaptions
#' 
#' @description Returns a list of caption tracks that are associated with a 
#' specified video.
#' 
#' @param token 
#' Your token as returned by \code{\link{youOAuth}}.
#' @param video.id 
#' specifies the YouTube video ID of the video for which the API should return 
#' caption tracks.
#' @param part 
#' specifies the caption resource parts that the API response will include. 
#' The default vlaue is \code{snippet}, can take any of \code{id} or 
#' \code{snippet}. See \code{link{findParts}}.
#' 
#' @details 
#' Required authorisation: 
#' \itemize{
#' \item \code{force-ssl}
#' \item \code{partner-channel-audit}
#' }
#' 
#' See scope in \code{\link{youOAuth}}
#' 
#' @examples 
#' \dontrun{
#' # Authenticate
#' token <- youOauth(client.id = "something.apps.googleusercontent.com",
#'                   client.secret = "XxxXX1XxXxXxxx1xxx1xxXXX")
#'                   
#' # search videos about cats
#' search <- searchTube(token, query = "cats", type = "video")
#' 
#' # random channel id
#' set.seed(19880525)
#' vid <- sample(search$id.videoId, 1)
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
getCaptions <- function(token, video.id = NULL, part = "snippet", id,
                        on.behalf.of.content.owner = NULL) {
  
  # check required arguments
  if (missing(video.id) || is.null(video.id)) {
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
  
  testPart("getActivities", part)
  
  # build uri
  uri <- paste0("https://www.googleapis.com/youtube/v3/search?part=", part, 
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