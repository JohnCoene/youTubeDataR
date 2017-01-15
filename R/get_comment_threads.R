#' getCommentThreads
#' 
#' @description Returns a list of comment threads that match the API request 
#' parameters.
#' 
#' @param token 
#' Your token as returned by \code{\link{youOAuth}}.
#' @param part 
#' The part parameter specifies a comma-separated list of one or more activity 
#' resource properties that the API response will include. The default value 
#' is \code{snippet}, can take any of \code{snippet} or \code{id}. 
#' See \code{\link{findParts}}.
#' @param n 
#' Number of results to fecth. The default value is \code{20}.
#' @param all.threads.related.to.channel.id 
#' Instructs the API to return all comment threads associated with the 
#' specified channel. The response can include comments about the channel or 
#' about the channel's videos.
#' @param channel.id 
#' Instructs the API to return comment threads containing comments about the 
#' specified channel. (The response will not include comments left on videos 
#' that the channel uploaded.).
#' @param id 
#' Specifies a comma-separated list of comment thread IDs for the resources 
#' that should be retrieved.
#' @param video.id 
#' Instructs the API to return comment threads associated with the specified 
#' video ID.
#' @param max.results 
#' Specifies the maximum number of results that should be returned 
#' by each API call. Acceptable values are \code{0} to \code{100}, inclusive. 
#' The default value is \code{NULL}.
#' @param moderation.status 
#' Specifies the maximum number of items that should be returned in the result 
#' set. Note: This parameter is not supported for use in conjunction with the 
#' id parameter. Acceptable values are 1 to 100, inclusive. The default value 
#' is 20.
#' @param order 
#' Specifies the method that will be used to order resources in the API 
#' response. The default value is \code{NULL}. 
#' See \code{\link{findParams}} for valid values.
#' @param search.terms 
#' Instructs the API to limit the API response to only contain comments that 
#' contain the specified search terms.
#' @param text.format 
#' Set this parameter's value to html or plainText to instruct the API to 
#' return the comments left by users in html formatted or in plain text. 
#' The default value is \code{NULL} - which YouTube defaults to \code{html}.
#' See \code{\link{findParams}} for all valid values.
#' @param verbose 
#' If \code{TRUE} prints infromational messages in the console. 
#' The default value is \code{FALSE}.
#' 
#' @details Must specify one (and only one) of 
#' \code{all.threads.related.to.channel.id} or \code{channel.id} or \code{id} or 
#' \code{video.id} 
#' 
#' The \code{part} parameter has great impact on your quota, visit the 
#' \href{https://developers.google.com/youtube/v3/getting-started#quota}{official documentation} 
#' for more information.
#' 
#' @examples 
#' \dontrun{
#' # Authenticate
#' token <- youOAuth(client.id = "something.apps.googleusercontent.com",
#'                   client.secret = "XxxXX1XxXxXxxx1xxx1xxXXX")
#' 
#' # search channel on cats
#' search <- searchTube(token, query = "cats", type = "channel")
#' 
#' # sample random channel
#' set.seed(19880525)
#' channel <- sample(search$channelId, 1)
#' 
#' # fetch comment thread
#' cats <- getCommentThreads(token, all.threads.related.to.channel.id = channel)
#' }
#' 
#' @export
#' 
#' @author John Coene \email{jcoenep@@hotmail.com}
getCommentThreads <- function(token, part = "snippet", n = 20, 
                              channel.id, all.threads.related.to.channel.id, 
                              id, video.id, max.results = NULL, 
                              moderation.status = "published", order = NULL, 
                              search.terms = NULL, text.format = NULL, 
                              verbose = FALSE) {
  
  if(missing(channel.id)) channel.id <- NULL
  if(missing(all.threads.related.to.channel.id)) {
    all.threads.related.to.channel.id <- NULL
  }
  if(missing(id)) id <- NULL
  if(missing(video.id)) video.id<- NULL
  
  # check token
  checkToken(token)
  
  # check required parameters
  if(!length(channel.id) && !length(all.threads.related.to.channel.id) && 
     !length(id) && !length(video.id)) {
    stop(paste0("Must specify one of video.id, id, ", 
                "all.threads.related.to.channel.id or channel.id"))
  } else {
    
    c <- length(channel.id) + length(all.threads.related.to.channel.id) +
      length(id) + length(video.id)
    
    if(c > 1) {
      stop(paste0("can only specify one of one of video.id, id, ",
                  "all.threads.related.to.channel.id or channel.id"))
    }
  }
  
  # test moderation.status
  if(length(moderation.status) && length(id)) {
    stop("cannot use moderation.status in conjonction with id")
  } else if (length(max.results) && length(id)) {
    warning(paste0("cannot use max.results in conjonction with id,", 
                   " setting max.results to NULL"))
    max.results <- NULL
  } else if (length(id) && length(order)) {
    warning("cannot use id in conjonction with order, setting order to NULL")
    order <- NULL
  } else if (length(id) && length(search.terms)) {
    stop("cannot use if in conjonction with search.terms")
  }
  
  # check optional arguments
  # max results
  if(length(max.results)) {
    if(max.results > 100) {
      
      warning("max.results > 100, overriden to 100")
      max.results <- 100
      
    } else if (max.results == 0) {
      
      warning("max.results == 0, overriden to 5")
      max.results <- 5
    }
  }
  
  # parameters to list
  arguments <- namedList(channel.id, all.threads.related.to.channel.id, id, 
                         video.id, max.results, moderation.status, order, 
                         text.format)
  
  # buildParameters
  x <- list()
  for (i in 1:length(arguments)) {
    y <- buildParam(param = names(arguments[i]), values = arguments[[i]])
    x[[i]] <- ifelse(!is.null(y), y, "")
  }
  
  # collapse
  suffix <- paste(x, collapse = "")
  
  # search terms
  # replace space with "+"
  if(length(search.terms)) {
    search.terms <- gsub("[[:space:]]", "+", search.terms)
    search.terms <- paste0("&searchTerms=", search.terms)
  }
   
  
  testPart("getCommentThreads", part)
  
  # build uri
  uri <- paste0("https://www.googleapis.com/youtube/v3/commentThreads?part=", 
                part, suffix, search.terms)
  
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
    
    if(length(id)) {
      
      dat <- paginate(response , n, verbose)
      
    } else {
      
      dat <- do.call(plyr::"rbind.fill", lapply(json$items, as.data.frame))
      
    }
    
    
  }
  
  if(verbose == TRUE && nrow(dat)){
    cat(paste0(n, " results queried, API returned ", nrow(dat),
               " results."))
  }
  
  return(dat)
  
}