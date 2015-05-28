library(openWAR)

test_that("gameday actually retrieves data", {
  expect_is(gameday(), "gameday")
})
