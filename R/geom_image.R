#' Add images to plot
#'
#' Work like geom_point with points replaced by a predefined image
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
#' @param stat The statistical transformation to use on the data for this
#'    layer, as a string.
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
#' ggplot(mtcars, aes(wt, mpg)) + geom_image()
#'


geom_image <- function(mapping = NULL, data = NULL, stat = "identity",
                       position = "identity", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                       ...) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomImage,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, img = img, ...))
}


#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto
#' @importFrom magick image_read
#' @importFrom grid rasterGrob

ImageGrob <- function(x, y, size, img) {
  p <- system.file("extdata", "images.jpg", package = "ggfun")
  img <- magick::image_read(p)
  rasterGrob(x = x, y = y, image = img, default.units = "native", height = size,
             width = size)
}

#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @import grid
#' @importFrom utils getFromNamespace


GeomImage <- ggproto("GeomImage", Geom,
                     draw_panel = function(data, panel_scales,
                                           coord, img, na.rm = FALSE) {
                       coords <- coord$transform(data, panel_scales)
                       ggname = getFromNamespace("ggname", "ggplot2")
                       ggname("geom_image", ImageGrob(coords$x, coords$y, coords$size,
                                                                img))
                     },
                     non_missing_aes = c("image", "size"),
                     required_aes = c("x", "y"),
                     default_aes = aes(size = 0.05),
                     icon = function(.) {
                     },
                     desc_params = list(),
                     seealso = list(geom_point = GeomPoint$desc),
                     examples = function(.) {
                     }
)

