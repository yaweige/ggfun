ggplot2 Extension for Fun
================
Yawei Ge, Zhenzhen Chen, Weiquan Luo
3/22/2019

Functions
=========

### `geom_image`

Elephant and donkey

``` r
library(ggplot2)
library(ggfun)
library(dplyr)

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

### `stat_arrowmap`

Forgive me for this

``` r
madedata_standard %>%
  ggplot() +
  geom_path(aes(x = long, y = lat, group = group)) +
  stat_arrowmap(aes(x = long, y = lat, change = change, group = region))
```

![](README_files/figure-markdown_github/unnamed-chunk-4-1.png)
