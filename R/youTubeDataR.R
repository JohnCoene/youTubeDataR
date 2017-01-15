#' youTubeDataR: R and Youtube.
#'
#' youTubeDataR integrates R and the YouTube Data API to allow fetching data from Youtube.
#' 
#' @section youTubeDataR functions:
#' \itemize{
#'  \item{youOAuth}
#'  \item{findParams}
#'  \item{findParts}
#'  \item{getActivities}
#'  \item{getCaptions}
#'  \item{getChannelSections}
#'  \item{getChannels}
#'  \item{getCommentThreads}
#'  \item{getLanguages}
#'  \item{getComments}
#'  \item{getGuideCategories}
#'  \item{getRegion}
#'  \item{getSubscriptions}
#'  \item{getVideoCategories}
#'  \item{getVideos}
#'  \item{searchTube}
#' }
#' 
#' @importFrom methods is
#' @importFrom stats setNames
#' 
#' @examples 
#' \dontrun{
#' # Authenticate
#' TK <- youOauth(client.id = "something.apps.googleusercontent.com",
#'                client.secret = "XxxXX1XxXxXxxx1xxx1xxXXX",
#'                scope = "force-ssl")
#'                
#' # search channels about R tutorials
#' search <- searchTube(TK, query = "R tutorial", type = "channel")
#' 
#' # get REvolutionAnalytics channel
#' revo <- search[grep("REvolutionAnalytics", search$channelTitle), 
#'                "id.channelId"]
#'                
#' # get activities of REvolutionAnalytics channel
#' revo.act <- getActivities(TK, channel.id = revo)
#' 
#' # get REvolutionAnalytics channel sections
#' revo.sect <- getChannelSections(TK, channel.id = revo)
#' 
#' # get my feed
#' my.videos <- getVideos(TK)
#' }
#' 
#' @keywords internal
#'
#' @docType package
#' @name youTubeDataR
NULL