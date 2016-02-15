library(youTubeDataR)

test_that("getActivities and getChannelSections getCommentThreads", {
  
  # load token
  TK = readRDS("token_file.rds")
  
  # search channel
  search = searchTube(TK, query = "R tutorial", type = "channel")
  
  # get REvolutionAnalytics
  revo = search[grep("REvolutionAnalytics", search$snippet.channelTitle), 
                "id.channelId"]
  
  # test error if missing required args
  expect_error(getActivities(TK))
  
  # get activities
  act = getActivities(TK, channel.id = revo)
  
  # expect 50 results
  expect_equal(nrow(act), 50)
  
  # expect data.frame
  expect_is(act, "data.frame")
  
  # getChannelsSections
  sect = getChannelSections(TK, channel.id = revo)
  
  # expect 1
  expect_equal(nrow(sect), 1)
  expect_equal(sect$etag, 
               as.factor('"DsOZ7qVJA4mxdTxZeNzis6uE6ck/rsCZsxGcRtIClh2WI6fKe08MZPM"'))
  expect_is(sect, "data.frame")
  
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
  
  # hl
  reg = getRegions(TK)
  
  # test
  expect_equal(nrow(reg), 89)
  
  # grep
  be = reg[grep("BE", reg$snippet.gl),]
  
  # test
  expect_equal(nrow(be), 1)
  
  # lang
  lang = getLanguages(TK)
  
  expect_equal(nrow(lang), 76)
})