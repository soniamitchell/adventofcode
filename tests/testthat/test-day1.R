context("Test day 1")

# Read in data
dat <- file.path("2021", "day1-test.txt") |>
  scan()

# Count the number of times the depth increases from the previous measurement
day1 <- function(dat) sum(diff(dat) > 0)

answer <- 7

test_that("correct answer is given", {
  expect_equal(day1(dat), answer)
})
