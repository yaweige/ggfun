context("test-stat_arrowmap")

test_that("output is a ggplot2 plot", {
  p <- ggplot(data = data.frame(x = rnorm(10, mean = 0, sd = 1),
                                y = rnorm(10, mean = 0, sd = 1),
                                change = rnorm(10, mean = 0, sd = 1))) +
    stat_arrowmap(aes(x = x, y = y, change = change))
  expect_is(p, "ggplot")
})
