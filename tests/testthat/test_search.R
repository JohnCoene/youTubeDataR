library(youTubeDataR)

test_that("test searchTube return structure", {
  
  # load token
  source("./tests/testthat/token.R")
  
  # search class
  s = searchTube(token = TK, query = "cats")
  expect_is(s, "data.frame")
})