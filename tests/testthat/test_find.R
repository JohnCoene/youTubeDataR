library(youTubeDataR)

test_that("Test findParts return values", {
  
  # errors
  expect_error(findParts())
  expect_error(findParts(FUN = "channels"))
  
  # activities
  act.parts = findParts("getActivities")
  expect_equal(act.parts, c("contentDetails", "id", "snippet"))
  
  # comments
  com.parts = findParts("getComments")
  expect_equal(com.parts, c("id", "snippet"))
  
  # comment threads
  th.parts = findParts("getCommentThreads")
  expect_equal(th.parts, c("id", "replies", "snippet"))
  
  # channels
  expect_equal(findParts("getChannels"), c("auditDetails", "brandingSettings", 
                                           "contentDetails", 
                                           "contentOwnerDetails", "id", 
                                           "invideoPromotion", "localizations",
                                           "snippet", "statistics", "status",
                                           "topicDetails"))
  
  # channel sections
  expect_equal(findParts("getChannelSections"), c("contentDetails", "id", 
                                                  "localizations", "snippet"))
  
  # playlists
  expect_equal(findParts("getPlaylists"), c("contentDetails", "id", 
                                            "localizations", "player", 
                                            "snippet", "status"))
})

test_that("Test findParams return values", {
  
  # error
  expect_error(findParams())
  expect_error(findParams("error"))
  
  # type
  type = findParams("type")
  expect_equal(type, c("any", "channel", "playlist", "video"))
  
  # order
  order = findParams("order")
  expect_equal(order, c("date", "rating", "relevance", "title", "videoCount", 
                        "viewCount"))
  
  # video dimensions
  vid.dim = findParams("video.dimension")
  expect_equal(vid.dim, c("2d", "3d", "any"))
  
  # moderation status
  mod.stat = findParams("moderation.status")
  expect_equal(mod.stat, c("heldForReview", "likelySpam", "published"))
})