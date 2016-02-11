#' getVideoCategories
#' 
#' @description Returns a list of categories that can be associated with 
#' YouTube videos.
#' 
#' @param token 
#' Your token as returned by \code{\link{youOAuth}}.
#' @param id 
#' Specifies a comma-separated list of video category IDs for the resources 
#' that you are retrieving.
#' @param region.code 
#' Instructs the API to return the list of video categories available in the 
#' specified country. The parameter value is an 
#' \href{https://www.iso.org/obp/ui/#search}{ISO 3166-1 alpha-2} 
#' country code.
#' @param hl 
#' Specifies the language that should be used for text values in the API 
#' response. The default value is \code{en_US}.
#' 
#' @examples 
#' \dontrun{
#' # Authenticate
#' token <- youOAuth(client.id = "something.apps.googleusercontent.com",
#'                   client.secret = "XxxXX1XxXxXxxx1xxx1xxXXX")
#' 
#' # get categories
#' video.cat <- getVideoCategories(token, region.code = "US")
#' }
#' 
#' @details Must specify either \code{id} or \code{region.code}
#' 
#' @author John Coene \email{jcoenep@@hotmail.com}
#' 
#' @export
getVideoCategories <- function(token, id, region.code, hl = "en_US") {
  
  checkToken(token)
  
  if(missing(id)) id <- NULL
  if(missing(region.code)) region.code <- NULL
  
  if(!length(id) && !length(region.code)) {
    stop("must specify either id or region.code")
  } else {
    
    c <- length(id) + length(region.code)
    
    if(c > 1) {
      stop("must specify only one of id or region.code")
    }
    
  }
  
  arguments <- namedList(id, region.code, hl)
  
  # buildParameters
  x <- list()
  for (i in 1:length(arguments)) {
    y <- buildParam(param = names(arguments[i]), values = arguments[[i]])
    x[[i]] <- ifelse(!is.null(y), y, "")
  }
  
  # collapse
  suffix <- paste(x, collapse = "")
  
  # build uri
  uri <- paste0("https://www.googleapis.com/youtube/v3/videoCategories?part=",
                "snippet", suffix)
  
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
  
  return(dat)
}