library(youTubeDataR)

test_that("getActivities and getChannelSections getCommentThreads", {
  
  # load token
  TK = readRDS("token_file.rds")
  
  # search channel
  search = searchTube(TK, query = "R tutorial", type = "channel")
  
  # get REvolutionAnalytics
  revo = "UCqIL67lW4Kmsd75h6h7pNog"
  
  # test error if missing required args
  expect_error(getActivities(TK))
  
  # get activities
  act = getActivities(TK, channel.id = revo, 
                      published.before = as.Date("2016-01-01"))
  
  # expect 50 results
  expect_equal(nrow(act), 10)
  
  act = getActivities(TK, channel.id = revo, 
                      published.before = as.Date("2016-01-01"),
                      region.code = "BE", verbose = FALSE)
  
  # expect 50 results
  expect_equal(nrow(act), 10)
  
  # expect data.frame
  expect_is(act, "data.frame")
  
  # getChannelsSections
  sect = getChannelSections(TK, channel.id = revo, verbose = FALSE)
  expect_is(sect, "data.frame")
  expect_equal(nrow(sect), 1)
  
  # My channel sections 
  x <- getChannelSections(TK, mine = TRUE)
  
  # getCommentThreads
  expect_error(getCommentThreads(TK, channel.id = revo))
  expect_error(getCommentThreads(TK))
})

test_that("getChannels and getGuideCategories", {
  
  # load token
  TK = readRDS("token_file.rds")
  
  # error
  expect_error(getChannels())
  expect_error(getChannels(TK))
  
  # test error if missing required values
  expect_error(getGuideCategories(TK))
  
  # get categories
  cat = getGuideCategories(TK, region.code = "US")
  
  # test cat return
  expect_equal(nrow(cat), 20)
  
  # set seed
  set.seed(19880525)
  
  # getChannels
  chan = getChannels(TK, category.id = sample(cat$id, 1))
  
  # test nrow 50
  expect_equal(nrow(chan), 50)
})

test_that("getPlaylists", {
  
  # error
  expect_error(getPlaylists())
  
  # load token
  TK = readRDS("token_file.rds")
  
  # search playlists
  search = searchTube(TK, "cats", type = "playlist")
  
  # pick first two playlist ids
  ids = paste0(search$id.playlistId[1], ",", search$id.playlistId[2])
  
  # get playlists
  pl = getPlaylists(TK, part = "player", id = ids)
  
  #test
  expect_equal(nrow(pl), 2)
  expect_is(pl, "data.frame")
  
})

test_that("getRegions and getLanguages", {
  
  # load token
  TK = readRDS("token_file.rds")
  
  # error
  expect_error(getRegions())
  
  # default
  reg = getRegions(TK)
  
  # test
  expect_equal(nrow(reg), 90)
  
  # hl
  km = getRegions(TK, hl = "km", verbose = T)
  
  # test
  expect_equal(nrow(km), 90)
  
  # grep
  be = reg[grep("BE", reg$gl),]
  
  # test
  expect_equal(nrow(be), 1)
  
  # lang
  lang = getLanguages(TK)
  
  expect_equal(nrow(lang), 76)
})

test_that("getVideos and getCaptions", {
  
  # load token
  TK = readRDS("token_file.rds")
  
  # error
  expect_error(getVideo())
  expect_error(getVideo("error"))
  expect_error(getCaptions())
  expect_error(getCaptions("error"))
  expect_error(getCaptions(TK))
  
  expect_warning(getVideos(TK))
  
  vids <- getVideos(TK, chart = "mostPopular")
  
  expect_equal(class(vids), "data.frame")
  
  expect_error(getCaptions(TK, sample(vids$id, 1)))
})

test_that("getPlaylistItems", {

  # load token
  TK = readRDS("token_file.rds")
  
  # errors
  expect_error(getPlaylistItems())
  expect_error(getPlaylistItems(TK))
  expect_error(getPlaylistItems(TK, n = 100))
})

test_that("getSubscriptions", {
  
  # load token
  TK = readRDS("token_file.rds")
  
  # search
  chans <- searchTube(TK, "programming", type = "channel")
  
  # invalid credentials
  expect_error(getSubscriptions())
  expect_error(getSubscriptions(TK))
  expect_error(getSubscriptions(TK, channel.id = chans$id.channelId[1]))
  
})

test_that("test findParts", {
  
  expect_error(findParts())
  expect_error(findParts("error"))
  
  expect_equal(findParts("getActivities"), c("contentDetails", "id", 
                                             "snippet"))
  expect_equal(length(findParts("getCaptions")), 2)
  expect_equal(length(findParts("getChannels")), 11)
  expect_equal(findParts("getChannelSections"), c("contentDetails", "id",
                                                  "localizations", "snippet"))
  expect_equal(findParts("getComments"), c("id", "snippet"))
  expect_equal(length(findParts("getVideos")), 13)
  expect_equal(findParts("getCommentThreads"), c("id", "replies", "snippet"))
  expect_equal(length(findParts("getPlaylistItems")), 4)
  expect_equal(length(findParts("getPlaylists")), 6)
  expect_equal(findParts("getSubscriptions"), c("snippet", "contentDetails", 
                                                "id", "subscriberSnippet"))
  expect_equal(length(findParts("getVideos")), 13)
})

test_that("test findParams", {
  
  expect_error(findParams())
  expect_error(findParams("error"))
  
  expect_equal(length(findParams("order")), 6)
  expect_equal(findParams("video.dimension"), c("2d", "3d", "any"))
  expect_equal(findParams("video.caption"), c("any", "closedCaption", "none"))
  expect_equal(findParams("video.duration"), c("any", "long", "medium", 
                                               "short"))
  expect_equal(findParams("video.definition"), c("any", "high", "standard"))
  expect_equal(findParams("video.embeddable"), c("any", "true"))
  expect_equal(findParams("video.syndicated"), c("any", "true"))
  expect_equal(findParams("video.license"), c("any", "creative", "youtube"))
  expect_equal(findParams("video.type"), c("any", "episode", "movie"))
  expect_equal(findParams("safe.search"), c("moderate", "none", "strict"))
  expect_equal(findParams("event.type")[3], "upcoming")
  expect_equal(length(findParams("channel.type")), 2)
  expect_equal(findParams("type")[2], "channel")
  expect_equal(findParams("text.format"), c("html", "plainText"))
  expect_equal(findParams("moderation.status")[2], "likelySpam")
})

test_that("videoCategories", {
  
  # load token
  TK = readRDS("token_file.rds")
  
  cats <- getVideoCategories(TK, region.code = "US")
  
  expect_lt(nrow(cats), 50)
  
  expect_error(getVideoCategories())
  expect_error(getVideoCategories(TK))
})