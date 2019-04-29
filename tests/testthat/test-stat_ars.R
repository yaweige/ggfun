context("test-stat_ars")

testthat::test_that("output is a ggplot2 plot", {
  p <- ggplot(data = data.frame(a = 5,
                                b = 10,
                                n= 5)) +
    stat_ars(aes(a = a, b = b, n = n))
  expect_is(p, "ggplot")
})
