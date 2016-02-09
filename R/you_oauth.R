#' youOAuth
#' 
#' @description OAuth 2.0 authentication
#' 
#' @param client.id Your \code{client.id}. See details below.
#' @param client.secret Your \code{client.secret}. See details below.
#' @param scope Scope of token, defaults to \code{NULL}. See details below.
#' 
#' @details 
#' To get your client.id and client.secret please follow the instructions 
#' \href{https://developers.google.com/youtube/registering_an_application}{here}
#'  , you may also want to watch the 
#'  \href{https://www.youtube.com/watch?v=Im69kzhpR3I}{video}
#' Also set the redirect Authorized redirect URIs to your localhost.
#' 
#' Scopes:
#' \itemize{
#' \item \code{force-ssl} Manage your YouTube account. This scope requires 
#' communication with the API server to happen over an SSL connection.
#' \item \code{NULL} Manage your YouTube account. This scope is functionally 
#' identical to the youtube.force-ssl scope listed above because the YouTube 
#' API server is only available via an HTTPS endpoint. As a result, even 
#' though this scope does not require an SSL connection, there is actually no 
#' other way to make an API request.
#' \item \code{readonly} View your YouTube account.
#' \item \code{upload} Upload YouTube videos and manage your YouTube videos.
#' \item \code{partner-channel-audit} Retrieve the 
#' \href{https://developers.google.com/youtube/v3/docs/channels#auditDetails}{auditDetails} part in a channel 
#' resource.
#' }
#' 
#' See all scopes from documentation 
#' \href{https://developers.google.com/youtube/v3/guides/auth/installed-apps}{here}
#' 
#' @return Returns object of class \code{token2.0} from the \code{httr} package.
#' 
#' @examples 
#' \dontrun{
#' token <- youOauth(client.id = "something.apps.googleusercontent.com",
#'                   client.secret = "XxxXX1XxXxXxxx1xxx1xxXXX")
#' }
#' 
#' @author John Coene \email{jcoenep@hotmail.com}
#' 
#' @export
youOAuth <- function(client.id, client.secret, scope = NULL) {
  
  youtube <- httr::oauth_endpoints("google")	
  
  # build app
  you.app <- httr::oauth_app("youtube", client.id, client.secret)
  
  # set scope
  if(is.null(scope)){
    
    scope <- paste0("https://www.googleapis.com/auth/youtube")
    
  } else if (scope == "force-ssl" || scope == "read.only" ||
             scope == "upload"){
    
    scope <- paste0("https://www.googleapis.com/auth/youtube.", scope)
    
  } else if (scope == "partner-channel-audit"){
    
    scope <- paste0("https://www.googleapis.com/auth/youtube", scope)
    
  } else {
    stop("Invalid scope. See @details.", call. = FALSE)
  }
  
  # OAuth
  youtube.token <- httr::oauth2.0_token(youtube, you.app,
                                        scope = scope,
                                        cache = FALSE)
  
  return(youtube.token)
}




