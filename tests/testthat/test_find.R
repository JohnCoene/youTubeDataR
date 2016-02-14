library(youTubeDataR)

test_that("Test findParts return values", {
  # activities
  act.parts = findParts("getActivities")
  expect_equal(act.parts, c("contentDetails", "id", "snippet"))
  
  # comments
  com.parts = findParts("getComments")
  expect_equal(com.parts, c("id", "snippet"))
  
  # comment threads
  th.parts = findParts("getCommentThreads")
  expect_equal(th.parts, c("id", "replies", "snippet"))
})

test_that("Test findParams return values", {
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