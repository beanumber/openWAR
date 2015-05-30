library(openWAR)

#test_that("gameday actually retrieves data", {
#  expect_is(gameday(), "gameday")
#})

test_that("you can plot GameDayPlays", {
  expect_is(May14, "GameDayPlays")
  expect_is(plot(May14), "trellis")
})

test_that("getRunEx is a function", {
  expect_is(getRunEx(May14), "function")
})
