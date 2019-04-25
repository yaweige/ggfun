#' To Draw Arrow Map of US Election
#'
#' Given a data set with the lat, long, group information to draw a US map and
#' change of party preference of each region, it draw an arrows for you.
#'
#' @param mapping Set of aesthetic mappings created by [aes()] or
#'   [aes_()]. If specified and `inherit.aes = TRUE` (the
#'   default), it is combined with the default mapping at the top level of the
#'   plot. You must supply `mapping` if there is no plot mapping.
#' @param data The data to be displayed in this layer. There are three
#'    options:
#'
#'    If `NULL`, the default, the data is inherited from the plot
#'    data as specified in the call to [ggplot()].
#'
#'    A `data.frame`, or other object, will override the plot
#'    data. All objects will be fortified to produce a data frame. See
#'    [fortify()] for which variables will be created.
#'
#'    A `function` will be called with a single argument,
#'    the plot data. The return value must be a `data.frame`, and
#'    will be used as the layer data.
#' @param geom The geometric object to use display the data
#' @param position Position adjustment, either as a string, or the result of
#'    a call to a position adjustment function.
#' @param na.rm If `FALSE`, the default, missing values are removed with
#'    a warning. If `TRUE`, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends?
#'   `NA`, the default, includes if any aesthetics are mapped.
#'   `FALSE` never includes, and `TRUE` always includes.
#'   It can also be a named logical vector to finely select the aesthetics to
#'   display.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics,
#'   rather than combining with them. This is most useful for helper functions
#'   that define both data and aesthetics and shouldn't inherit behaviour from
#'   the default plot specification, e.g. [borders()].
#' @param ... Other arguments passed on to [layer()]. These are
#'    often aesthetics, used to set an aesthetic to a fixed value, like
#'    `colour = "red"` or `size = 3`. They may also be parameters
#'    to the paired geom/stat.
#' @export
#' @importFrom  ggplot2 layer
#' @examples
#' library(ggplot2)
#' library(maps)
#' usmap <- map_data("state")
#' madedata_standard <- data.frame(region = unique(usmap$region), change = (runif(49)-0.5)*2,
#' stringsAsFactors = FALSE)
#'
#' madedata_standard <- dplyr::left_join(madedata_standard, usmap,by = "region")
#' ggplot(data = madedata_standard) +
#' geom_path(aes(x = long, y = lat, group = group)) +
#' stat_arrowmap(aes(x = long, y = lat, change = change, group = region))


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

