context("test-layer_PersHomo")

test_that("output is a ggplot2 plot", {
  p <- ggplot() +
    layer_PersHomo(data = data.frame(
      LONGITUDE = rnorm(10, mean = 0, sd = 1),
      LATITUDE = rnorm(10, mean = 0, sd = 1)),
    mapping = aes(x=LONGITUDE, y=LATITUDE),
                   d=0.5, colour = "blue") +
    geom_point()

  expect_is(p, "ggplot")
})
