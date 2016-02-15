[![Build Status](https://travis-ci.org/SocialFunction/youTubeDataR.svg?branch=master)](https://travis-ci.org/SocialFunction/youTubeDataR)
[![codecov.io](https://codecov.io/github/SocialFunction/youTubeDataR/coverage.svg?branch=master)](https://codecov.io/github/SocialFunction/youTubeDataR?branch=master)

# youTubeDataR

Integrates R and the YouTube Data API.

### Functions ###

#### OAuth ####

* `youOAuth`

#### Find-family ####

* `findParams`
* `findParts`

#### Search ####

* `searchTube`

#### GET-family ####

* `getActivities`
* `getcaptions`
* `getChannels`
* `getChannelSections`
* `getComments`
* `getCommentThreads`
* `getCategories`
* `getLanguages`

### Example ###

```
# Authenticate
TK <- youOauth(client.id = "something.apps.googleusercontent.com",
               client.secret = "XxxXX1XxXxXxxx1xxx1xxXXX", 
               scope = "force-ssl")
               
# search channels about R tutorials
search <- searchTube(TK, query = "R tutorial", type = "channel")
  
# get REvolutionAnalytics channel
revo <- search[grep("REvolutionAnalytics", search$snippet.channelTitle),
                    "id.channelId"]
  
# get activities of REvolutionAnalytics channel
revo.act <- getActivities(TK, channel.id = revo)

# get REvolutionAnalytics channel sections
revo.sect <- getChannelSections(TK, channel.id = revo)

# get my feed
my.videos <- getVideos(TK)
```

### Install ###

`devtools::install_github("SocialFunction/youTubeDataR")`

### Documentation & Manual ###

WIP
