context("test-stat_rl")

test_that("output is a ggplot2 plot", {
  p <- ggplot(data = data.frame(x = rnorm(100, mean = 1, sd = 1),
                                y = rnorm(100, mean = 1, sd = 1),
                                id = sample(c("1", "2"), 100, replace = T))) +
    stat_rl(aes(x = x, y = y, id = id))
  expect_is(p, "ggplot")
})
