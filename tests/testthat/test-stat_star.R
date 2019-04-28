context("test-stat_star")

test_that("output is a ggplot2 plot", {
  p <- ggplot(data = data.frame(x = rnorm(100, mean = 0, sd = 1),
                                y = rnorm(100, mean = 0, sd = 1))) +
    geom_point(aes(x = x, y = y)) +
    stat_star(aes(x = x, y = y))
  expect_is(p, "ggplot")
})
