#' To Draw a Line from the central to the farest point
#'
#' Given a points set in data.frame, extends ggplot2 functionality to
#' draw a line from the central to the farest point
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
#' madedata <- data.frame(x = rnorm(100, mean = 0, sd = 1),
#' y = rnorm(100, mean = 0, sd = 1))
#' ggplot(madedata, aes(x = x, y = y)) +
#' geom_point() +
#' stat_star(color = "red")
#'
#'

stat_star <- function(mapping = NULL, data = NULL, geom = "line",
                      position = "identity", na.rm = FALSE, show.legend = NA,
                      inherit.aes = TRUE, ...){
  ggplot2::layer(
    stat = StatStar, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' @format NULL
#' @usage NULL
#' @import ggplot2 ggproto
#' @export
#'

StatStar <- ggplot2::ggproto("StatStar", Stat,
                    compute_group = function(data, scales){
                      med_x <- median(data$x)
                      med_y <- median(data$y)
                      dist <- unlist(Map(function(x, y) {sqrt((x-med_x)^2 + (y-med_y)^2)},
                                         data$x, data$y))
                      out <- rbind(data[which.min(dist), , drop = F],
                                   data[which.max(dist), , drop = F])
                      return(out)
                    },

                    required_aes = c("x", "y")
)
