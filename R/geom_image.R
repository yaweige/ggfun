#' (This function is not ready to use)
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

