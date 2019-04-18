#' To Draw a Archimedean Spiral
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
#'
#' ggplot() +
#'   stat_ars(aes(a = 10, b = 20), col = "blue")

stat_ars <- function(mapping = NULL, data = NULL, geom = "point",
                     position = "identity", na.rm = FALSE, show.legend = NA,
                     inherit.aes = TRUE, ...) {
  layer(
    stat = StatArs, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto
#' @export

StatArs <- ggproto("StatArs", Stat,

                   compute_group = function(data, scales) {

                     theta <- seq(0, 2*pi, length = 1000)
                     r <- data$a + data$b * theta
                     x <- r * sin(theta)
                     y <- r * cos(theta)

                     data.frame(x = x, y = y)
                   },
                   required_aes = c("a", "b")
)

