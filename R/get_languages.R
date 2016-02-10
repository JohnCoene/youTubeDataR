#' getLanguages
#' 
#' @description Returns a list of application languages that the YouTube 
#' website supports.
#' 
#' @param token 
#' Your token as returned by \code{\link{youOAuth}}.
#' @param hl 
#' Specifies the language that should be used for text values in the API 
#' response. The default value is \code{en_US}. Optional argument.'
#' @param verbose 
#' If \code{TRUE} prints infromational messages in the console. 
#' The default value is \code{FALSE}.
#' 
#' @examples 
#' \dontrun{
#' # Authenticate
#' token <- youOAuth(client.id = "something.apps.googleusercontent.com",
#'                   client.secret = "XxxXX1XxXxXxxx1xxx1xxXXX")
#' 
#' # fetch languages
#' lang <- getLanguages(token)
#' }
#' 
#' @author John Coene \email{jcoenep@@hotmail.com}
#' 
#' @export
getLanguages <- function(token, hl = NULL, verbose = FALSE) {
  
  # check required arguments
  checkToken(token)
  
  if(length(hl)) hl <- paste0("&hl=", hl)
  
  
  # build uri
  uri <- paste0("https://www.googleapis.com/youtube/v3/i18nLanguages", 
                "?part=snippet", hl)
  
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
  
  if(verbose == TRUE && nrow(dat)){
    cat(paste0("API returned ", nrow(dat), " results."))
  }
  
  return(dat)
  
}