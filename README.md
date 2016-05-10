[![Build Status](https://travis-ci.org/JohnCoene/youTubeDataR.svg?branch=master)](https://travis-ci.org/JohnCoene/youTubeDataR)
[![Build status](https://ci.appveyor.com/api/projects/status/w6juofhgxemvjtva/branch/master?svg=true)](https://ci.appveyor.com/project/JohnCoene/youtubedatar/branch/master)
[![codecov.io](https://codecov.io/github/JohnCoene/youTubeDataR/coverage.svg?branch=master)](https://codecov.io/github/JohnCoene/youTubeDataR?branch=master)
[![Coverage Status](https://img.shields.io/coveralls/SocialFunction/youTubeDataR.svg)](https://coveralls.io/r/SocialFunction/youTubeDataR?branch=master)


# youTubeDataR

Integrates R and the YouTube Data API.

## Install

```R
# install httpuv to avoid out-of-band authentication
install.packages("httpuv")
library(httpuv)
devtools::install_github("JohnCoene/youTubeDataR")
```

## OAuth

Create OAuth client ID > Web application at [https://console.developers.google.com](https://console.developers.google.com) and specify your `httr::oauth_callback()` as "Authorized redirect URIs" (generally `http://localhost:1410`).

```R
token <- youOAuth("something.apps.googleusercontent.com", "XXxxXxxXXxxXxxXX")
```

### Functions ###

##### OAuth #####

Authenticate

* `youOAuth`

##### Find-family #####

Helper functions

* `findParams`
* `findParts`

##### Search #####

Search `channel`, `video`, `playlist` or `any` (see example below)

* `searchTube`

##### GET-family #####

Get data

* `getActivities`
* `getCaptions`
* `getChannels`
* `getChannelSections`
* `getComments`
* `getCommentThreads`
* `getGuideCategories`
* `getLanguages`

#### Example ####

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

[Manual](http://johncoene.github.io/projects/docs/youTubeDataR.pdf)
