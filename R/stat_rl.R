#' To Draw a regression line for each level for categorical variable
#'
#'
#' @param na.rm If `FALSE`, the default, missing values are removed with
#'   a warning. If `TRUE`, missing values are silently removed.
#' @param ... Other arguments passed on to [layer()]. These are
#'   often aesthetics, used to set an aesthetic to a fixed value, like
#'   `colour = "red"` or `size = 3`. They may also be parameters
#'   to the paired geom/stat.
#' @export
#' @importFrom  ggplot2 layer
#' @examples
#' indexf <- read.table("index.txt", header = T),
#'
#' ggplot(indexf, aes(x = length, y = width, id = sex)) +
#' geom_point() +
#' stat_rl(aes(x = length, y =  width, id = sex, colour = sex))


stat_rl <- function(mapping = NULL, data = NULL, geom = "line",
                    position = "identity", na.rm = FALSE, show.legend = NA,
                    inherit.aes = TRUE, ...) {
  layer(
    stat = StatRl, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto
#' @export
#'

StatRl <- ggproto("StatLin", Stat,
                  compute_group = function(data, scales) {

                    rng <- range(data$x, na.rm = TRUE)
                    grid <- data.frame(x = rng)

                    mod <- lm(y ~ x, data = data)

                    grid$y <- predict(mod, newdata = grid)
                    grid$id <- data$id[1]
                    grid$PANEL = data$PANEL[1]
                    grid$group = data$group[1]
                    grid

                  },
                  required_aes = c("x", "y", "id")
)



