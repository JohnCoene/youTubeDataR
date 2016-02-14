library(youTubeDataR)

test_that("test searchTube return structure", {
  # load token
  load("token_file")
  
  # search class
  s = searchTube(token = TK, query = "cats")
  expect_is(s, "data.frame")
})