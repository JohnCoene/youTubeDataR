[![Build Status](https://travis-ci.org/JohnCoene/youTubeDataR.svg?branch=master)](https://travis-ci.org/JohnCoene/youTubeDataR)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/JohnCoene/youTubeDataR?branch=master&svg=true)](https://ci.appveyor.com/project/JohnCoene/youTubeDataR)
[![codecov.io](https://codecov.io/github/JohnCoene/youTubeDataR/coverage.svg?branch=master)](https://codecov.io/github/JohnCoene/youTubeDataR?branch=master)
[![Coverage Status](https://coveralls.io/repos/github/JohnCoene/youTubeDataR/badge.svg?branch=master)](https://coveralls.io/github/JohnCoene/youTubeDataR?branch=master)

# youTubeDataR

Integrates R and the [YouTube Data API](https://developers.google.com/youtube/v3/).

See [site](http://john-coene.com/packages/youTubeDataR/) for docs and details.

## Install

```R
install.packages("httpuv") # install httpuv to avoid out-of-band authentication
devtools::install_github("JohnCoene/youTubeDataR")
```

## OAuth

Get your credentials

1. Sign in at [https://console.developers.google.com](https://console.developers.google.com)
2. Click "Credentials" in the sidebar
3. Hit "Create credentials"
4. In the dropdown menu select "OAuth client ID"
5. On the next page select "Web application"
6. Fill in your "Authorized redirect URIs" as returned by `httr::oauth_callback()` as "Authorized redirect URIs" (generally `http://localhost:1410/`).

```R
token <- youOAuth("something.apps.googleusercontent.com", "XXxxXxxXXxxXxxXX")
```

### Versions ###

* Some variable names were changed in `v0.2` please use `v0.1` for backward compatibility
  - `snippet.` prefix removed in `v0.2`
  - code cleanup

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

* `getVideos`
* `getActivities`
* `getCaptions`
* `getChannels`
* `getChannelSections`
* `getComments`
* `getCommentThreads`
* `getVideoCateogries`
* `getGuideCategories`
* `getLanguages`
* `getRegions`
* `getSubscriptions`
* `getPlaylists`
* `getPlaylistItems`

#### See [this post](http://john-coene.com/post/youtube/) for examples ####
