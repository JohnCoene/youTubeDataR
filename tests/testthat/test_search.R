library(youTubeDataR)

test_that("test searchTube return structure", {
  
  # load token
  TK = readRDS("token_file.rds")
  
  # search class
  s = searchTube(token = TK, query = "cats")
  expect_is(s, "data.frame")
})