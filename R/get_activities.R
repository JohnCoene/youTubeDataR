#' getActivities
#' 
#' @param token 
#' Your token as returned by \code{\link{youOAuth}}.
#' @param n 
#' Number of results to fecth. The default value is \code{50}.
#' @param max.results 
#' Specifies the maximum number of results that should be returned 
#' by each API call. Acceptable values are \code{0} to \code{50}, inclusive. 
#' The default value is \code{50}.
#' @param channel.id 
#' Indicates that the API response should only contain resources created by 
#' the channel. 
#' @param mine 
#' Set this parameter's value to true to retrieve a feed of the authenticated 
#' user's activities. The default value is \code{FALSE}.
#' @param home 
#' Set this parameter's value to true to retrieve the activity feed that 
#' displays on the YouTube home page for the currently authenticated user. 
#' The default value is \code{FALSE}.
#' @param published.before 
#' Indicates that the API response should only contain resources created 
#' before the specified time. Can be either of class \code{Date} ("%Y-%m-%d") 
#' or \code{POSIXct} or \code{POSIXlt} with time, or an RFC 3339 formatted 
#' date-time value (i.e.: \code{1970-01-01T00:00:00Z}). 
#' The default value is \code{Sys.time()}.
#' @param published.after 
#' Indicates that the API response should only contain resources created after 
#' the specified time. Can be either of class \code{Date} ("%Y-%m-%d") 
#' or \code{POSIXct} or \code{POSIXlt} with time, or an RFC 3339 formatted 
#' date-time value (i.e.: \code{1970-01-01T00:00:00Z}). 
#' The default value is \code{NULL}.
#' @param region.code 
#' Instructs the API to return search results for the specified country. The 
#' parameter value is an 
#' \href{http://www.iso.org/iso/country_codes/iso_3166_code_lists/country_names_and_code_elements.htm}{ISO 3166-1 alpha-2} 
#' country code. The default value is \code{NULL}.
#' @param verbose 
#' If \code{TRUE} prints infromational messages in the console. 
#' The default value is \code{FALSE}.
#' 
#' @export
#' 
#' @author John Coene \email{jcoenep@@hotmail.com}
getActivities <- function(token, n = 50, max.results = 50, channel.id = NULL, 
                          mine = FALSE, home = FALSE, 
                          published.before = Sys.time(), published.after = NULL, 
                          region.code = NULL, verbose = FALSE) {
  
  
  # check required arguments
  if (missing(query)) {
    stop("query is missing")
  }
  # check token
  checkToken(token)
  
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
  arguments <- namedList(region.code, max.results)
  
  # buildParameters
  x <- list()
  for (i in 1:length(arguments)) {
    y <- buildParam(param = names(arguments[i]), values = arguments[[i]])
    x[[i]] <- ifelse(!is.null(y), y, "")
  }
  
  # collapse
  suffix <- paste(x, collapse = "")
  
  # time
  published.before <- paste0("&publishedBefore=", buildTime(published.before))
  if (length(published.after)) {
    published.after <- paste0("&publishedAfter=", buildTime(published.after))
  }
  
  # mine
  if (mine == TRUE) {
    mine <- paste0("&mine=true")
  }
  
  # home
  if(home == TRUE){
    home <- paste0("&home=true")
  }
  
  # query
  query <- buildTerms(query)
  
  # build uri
  uri <- paste0("https://www.googleapis.com/youtube/v3/search?part=snippet",
                suffix, mine, home, published.before, published.after)
  
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
    
    dat <- paginate(response, n, verbose)
    
  }
  
  if(verbose == TRUE){
    cat(paste0(n, " results queried, API returned ", nrow(dat),
               " results."))
  }
  
  return(dat)
  
}