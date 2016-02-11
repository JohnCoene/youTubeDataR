#' getActivities
#' 
#' @description Returns a list of caption tracks matching a specific critera.
#' 
#' @param token 
#' Your token as returned by \code{\link{youOAuth}}.
#' @param channel.id 
#' Indicates that the API response should only contain resources created by 
#' the channel.
#' @param part 
#' The part parameter specifies a comma-separated list of one or more activity 
#' resource properties that the API response will include. The default value 
#' is \code{snippet}, can take any of \code{contentDetails}, \code{id} or 
#' \code{snippet}. See \code{link{findParts}}.
#' @param n 
#' Number of results to fecth. The default value is \code{50}.
#' @param max.results 
#' Specifies the maximum number of results that should be returned 
#' by each API call. Acceptable values are \code{0} to \code{50}, inclusive. 
#' The default value is \code{50}.
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
#' @details MUST specify one of \code{channel.id} OR \code{mine} OR 
#' \code{home}.
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
#' # pick random channel id
#' set.seed(19880525)
#' chan <- sample(search$snippet.id.channelId, 1)
#' 
#' # fetch data
#' act <- getActivities(token, channel.id = chan)
#' }
#' 
#' @seealso \code{link{youOAuth}}, \code{\link{findParts}}
#' 
#' @export
#' 
#' @author John Coene \email{jcoenep@@hotmail.com}
getActivities <- function(token, channel.id, mine = FALSE, home = FALSE, 
                          part = "snippet", n = 50, max.results = 50,  
                          published.before = Sys.time(), published.after = NULL, 
                          region.code = NULL, verbose = FALSE) {
  
  # check required arguments
  # check token
  checkToken(token)
  
  if(missing(channel.id)) channel.id <- NULL
  
  if(is.null(channel.id) && mine == FALSE && home == FALSE) {
    stop("must provide channel.id or mine or home")
  } else {
    
   c <- mine + home + length(channel.id)
   
   if(c > 1) {
     
     stop("can only specify one of home, mine or channel.id")
     
   } else {
     
     # mine
     if (mine == TRUE) {
       mine <- paste0("&mine=true")
     } else {
       mine <- NULL
     }
     
     # home
     if(home == TRUE) {
       home <- paste0("&home=true")
     } else {
       home <- NULL
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
  arguments <- namedList(region.code, max.results, channel.id)
  
  # buildParameters
  x <- list()
  for (i in 1:length(arguments)) {
    y <- buildParam(param = names(arguments[i]), values = arguments[[i]])
    x[[i]] <- ifelse(!is.null(y), y, "")
  }
  
  # collapse
  suffix <- paste(x, collapse = "")
  
  # time
  if(length(published.before)){
    published.before <- paste0("&publishedBefore=", buildTime(published.before))
  }
  
  if (length(published.after)) {
    published.after <- paste0("&publishedAfter=", buildTime(published.after))
  }
  
  testPart("getActivities", part)
  
  # build uri
  uri <- paste0("https://www.googleapis.com/youtube/v3/activities?part=", part,
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
    
    dat <- paginate(response , n, verbose)
    
  }
  
  if(verbose == TRUE && nrow(dat)){
    cat(paste0(n, " results queried, API returned ", nrow(dat),
               " results."))
  }
  
  return(dat)
  
}