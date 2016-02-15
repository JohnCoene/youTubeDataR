library(youTubeDataR)

test_that("test searchTube return structure", {
  
  # load token
  TK = readRDS("token_file.rds")
  
  # search class
  s = searchTube(token = TK, query = "cats")
  expect_is(s, "data.frame")
  
  # nrow
  s1 = searchTube(token = TK, query = "cats", n = 100)
  expect_equal(nrow(s1), 100)
})