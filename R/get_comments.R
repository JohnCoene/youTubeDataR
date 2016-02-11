#' getComments
#' 
#' @description Returns a list of comments that match the API request 
#' parameters. 
#' 
#' @param token 
#' Your token as returned by \code{\link{youOAuth}}.
#' @param part 
#' The part parameter specifies a comma-separated list of one or more activity 
#' resource properties that the API response will include. The default value 
#' is \code{snippet}, can take any of \code{snippet} or \code{id}. 
#' See \code{link{findParts}}.
#' @param n 
#' Number of results to fecth. The default value is \code{50}.
#' @param max.results 
#' Specifies the maximum number of results that should be returned 
#' by each API call. Acceptable values are \code{0} to \code{50}, inclusive. 
#' The default value is \code{50}.
#' @param id 
#' Specifies a comma-separated list of comment IDs for the resources that are 
#' being retrieved. In a comment resource, specifies the 
#' comment's ID.
#' @param parent.id 
#' Specifies the ID of the comment for which replies should be retrieved. Note: 
#' YouTube currently supports replies only for top-level comments. However, 
#' replies to replies may be supported in the future.
#' @param text.format 
#' Indicates whether the API should return comments formatted as HTML 
#' or as plain text. The default value is \code{html}. 
#' @param verbose 
#' If \code{TRUE} prints infromational messages in the console. 
#' The default value is \code{FALSE}.
#' 
#' @details Must specify one (and only one) of \code{id} or 
#' \code{parent.id}.
#' 
#' @examples 
#' \dontrun{
#' # Authenticate
#' token <- youOAuth(client.id = "something.apps.googleusercontent.com",
#'                   client.secret = "XxxXX1XxXxXxxx1xxx1xxXXX")
#' }
#' 
#' @author John Coene \email{jcoenep@@hotmail.com}
#' 
#' @export
getComments <- function(token, part = "snippet", n = 50, max.results = 50, 
                        id, parent.id, text.format = "html", verbose = FALSE){
  
  if(missing(id)) id <- NULL
  if(missing(parent.id)) parent.id <- NULL
  
  # check required arguments
  checkToken(token)
  
  if(!length(id) && !length(parent.id)) {
    stop("must provide id of parent.id")
  } else {
    
    c <- length(id) + length(parent.id)
    
    if(c > 1) {
      stop("can only specify id or parent.id")
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
  arguments <- namedList(id, parent.id, text.format, max.results)
  
  # buildParameters
  x <- list()
  for (i in 1:length(arguments)) {
    y <- buildParam(param = names(arguments[i]), values = arguments[[i]])
    x[[i]] <- ifelse(!is.null(y), y, "")
  }
  
  # collapse
  suffix <- paste(x, collapse = "")
  
  testPart("getComments", part)
  
  # build uri
  uri <- paste0("https://www.googleapis.com/youtube/v3/comments?part=", part,
                suffix)
  
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