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

test_that("summary works as expected", {
  expect_is(summary(May), "grouped_df")
  expect_is(summary(openWAR2012), "tbl_df")
})

test_that("getWAR works on objects of multiple classes", {
  expect_is(getWAR(MayProcessed), "openWARPlayers")
  expect_is(getWAR(MayProcessed$openWAR), "openWARPlayers")
})

test_that("shakeWAR works on objects of multiple classes", {
  expect_is(shakeWAR(MayProcessed, N=1), "do.openWARPlayers")
  expect_is(shakeWAR(MayProcessed$openWAR, N=1), "do.openWARPlayers")
})

