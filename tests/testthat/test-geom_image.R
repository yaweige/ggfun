context("test-geom_image")

test_that("output is a ggplot2 plot", {
  p <- ggplot(data = data.frame(x = rnorm(10, mean = 0, sd = 1),
                                y = rnorm(10, mean = 0, sd = 1))) +
    geom_image(aes(x = x, y = y))
  expect_is(p, "ggplot")
})

