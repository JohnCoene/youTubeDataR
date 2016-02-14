library(youTubeDataR)

test_that("test searchTube return structure", {
  load("token_file")
  s = searchTube(token = TK, query = "cats")
  expect_is(s, "data.frame")
})