ggplot2 Extension for Fun
================
Yawei Ge, Zhenzhen Chen, Weiquan Luo
3/22/2019

Functions
=========

### `geom_image`

``` r
library(ggplot2)
library(ggfun)

ggplot(mtcars, aes(wt, mpg)) + geom_image()
```

![](README_files/figure-markdown_github/unnamed-chunk-1-1.png)

### `stat_star`

Connect the central point to the farthest point

``` r
madedata <- data.frame(x = rnorm(100, mean = 0, sd = 1),
                       y = rnorm(100, mean = 0, sd = 1))


ggplot(madedata, aes(x = x, y = y)) +
  geom_point() +
  stat_star(color = "red")
```

![](README_files/figure-markdown_github/unnamed-chunk-2-1.png)
