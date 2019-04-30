context("test-geom_image")

test_that("output is a ggplot2 plot", {
  p <- system.file("extdata", "images.jpg", package = "ggfun")
  img <- magick::image_read(p)
  p <- ggplot(data = data.frame(x = rnorm(10, mean = 0, sd = 1),
                                y = rnorm(10, mean = 0, sd = 1))) +
    geom_image(aes(x = x, y = y), size = 0.05, img =img, na.rm = TRUE)
  expect_is(p, "ggplot")
})

