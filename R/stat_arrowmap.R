#' (This function is not ready to use)To Draw Arrow Map of US Election
#'
#' Given a data set with the lat, long, group information to draw a US map and
#' change of party preference of each region, it draw an arrows for you.
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

# is there a geom = "curve"? Yes
stat_arrowmap <- function(mapping = NULL, data = NULL, geom = "curve",
                          position = "identity", na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE, ...) {
  layer(
    stat = StatArrowmap, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto
#' @export
#'

StatArrowmap <- ggproto("StatArrowmap", Stat,

                        # the following setup_params may not work as expected
                        # setup_params = function(data, params) {
                        #   if (all(is.null(params$curvature),
                        #           is.null(params$size),
                        #           is.null(params$arrow)))
                        #     return(params)
                        #
                        #   if(is.null(params$curvature))
                        #     params$curvature = 0
                        #
                        #   if(is.null(params$size))
                        #     params$size = 1
                        #
                        #   if(is.null(params$arrow))
                        #     params$arrow = arrow(length = unit(0.05, "inches"))
                        #
                        # },

                        compute_group = function(data, scales) {
                          medx <- mean(range(data$x))
                          medy <- mean(range(data$y))
                          medchange <- mean(data$change)
                          endx <- medx + 0.070*medchange*(max(medx)-min(medy))
                          endy <- medy + 0.02*abs(medchange)*(max(medy)-min(medy))
                          # there are manuly chosen numbers here, we should avoid this
                          data.frame(x = medx, y = medy, xend = endx, yend = endy)
                        },

                        required_aes = c("x", "y", "change")
)

