#' getGuideCategories
#' 
#' @description Returns a list of categories that can be associated with 
#' YouTube channels.
#' 
#' @param token 
#' Your token as returned by \code{\link{youOAuth}}.
#' @param id 
#' Specifies a comma-separated list of the YouTube channel category ID(s) for 
#' the resource(s) that are being retrieved. In a \code{guide.category} 
#' resource, the id property specifies the YouTube channel category ID.
#' @param region.code 
#' Instructs the API to return the list of guide categories available in the 
#' specified country. The parameter value is an 
#' \href{https://www.iso.org/obp/ui/#search}{ISO 3166-1 alpha-2} country code.
#' @param hl 
#' Specifies the language that will be used for text values in the API 
#' response. The default value (from YouTube) is \code{en-US}. See 
#' \code{\link{getLanguages}}
#' 
#' @details Must specify one (and only one) filter \code{id} or 
#' \code{region.code}
#' 
#' @examples 
#' \dontrun{
#' token <- youOAuth(client.id = "something.apps.googleusercontent.com",
#'                   client.secret = "XxxXX1XxXxXxxx1xxx1xxXXX")
#' 
#' # fetch categories from Belgium
#' dull.cat <- getCategories(token, region.code = "BE")
#' }
#' 
#' @export
#' 
#' @author John Coene \email{jcoenep@@hotmail.com}
getGuideCategories <- function (token, id, region.code, hl = NULL) 
  {
  
  if(missing(id)) id <- NULL
  if(missing(region.code)) region.code <- NULL
  
  # check token
  checkToken(token)
  
  # check required parameters
  if(!length(id) && !length(region.code)) {
    stop("must provide either id or region.code")
  } else {
    
    c <- length(id) + length(region.code)
    
    if (c > 1) {
      stop("can only supply id OR region.code")
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
  uri <- paste0("https://www.googleapis.com/youtube/v3/guideCategories",
                "?part=snippet", suffix)
  
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