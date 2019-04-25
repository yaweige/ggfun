#' To draw line segments between locations on map by distance.
#'
#' Given a points set as Geographic locations in a data.frame, extends ggplot2 functionality to
#' draw a line from location to location with defined criterion of distance in km. The concept is inhered from persist homology.
#' The linkage in resulting figures can be used to recognize patterns or cluster of points.
#' Compare to ggplot2 package, plotly package might be better for this functionality because the linkage
#' can be shown on the globe instead of a flat figure. To the purpose of the package, this layer
#' attempt to show the capability of ggplot on a small scale data instead of global data.
#'
#' @param d the magnitude of real Geo distance (in km) used as the criterion to link two locations
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
#' @importFrom  geosphere distm distGeo
#' @examples
#' # This example show the earthquake data with in Pacific Plate
#' library(tidyverse)
#' library(ggplot2)
#' library(dplyr)
#' ## plot base map
#' worldmap <- map_data("world2")
#' p <- ggplot() +
#'   geom_polygon(data=worldmap,
#'   aes(x=long, y=lat, group = group),
#'   fill="white", colour="#7f7f7f", size=0.5) +
#'   theme(axis.line=element_blank(),
#'         axis.text.x=element_blank(),
#'         axis.text.y=element_blank(),
#'         axis.ticks=element_blank(),
#'         axis.title.x=element_blank(),
#'         axis.title.y=element_blank(),
#'         legend.position="none",
#'         panel.background=element_blank(),
#'         panel.border=element_blank(),
#'         panel.grid.major=element_blank(),
#'         panel.grid.minor=element_blank(),
#'         plot.background=element_blank()); p
#' f <- system.file("extdata", "eqData.txt", package = "ggfun")
#' eq <- read.delim(f, as.is=TRUE) %>%
#'   filter(!is.na(LONGITUDE) & !is.na(LATITUDE)) %>%
#'   filter(LONGITUDE > 110 | LONGITUDE < -45) %>%
#'   mutate(LONGITUDE = ifelse(LONGITUDE < 0, LONGITUDE + 360, LONGITUDE)) %>%
#'   select(YEAR, MONTH,DAY, EQ_MAG_MS, COUNTRY, LOCATION_NAME, LATITUDE, LONGITUDE)
#' ## add layer_PersHomo
#' fp <- p + layer_PersHomo(data= eq,
#' mapping = aes(x=LONGITUDE, y=LATITUDE),
#' d=450000, colour = "blue") +
#'   geom_point(); fp
#'
#'

layer_PersHomo <- function(mapping = NULL, data = NULL, geom = "segment", d = NULL,
                           position = "identity", na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE, ...) {
  layer(
    stat = layerPersHomo, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(d = d, na.rm = na.rm, ...)
  )
}

#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto
#'

layerPersHomo <- ggproto("persHomo", Stat,
                         compute_group = function(data, scales, params, d) {

                           # find distance matrix for geographic points
                           # finding the link
                           longlat <- data.frame(long = data$x, lat =data$y)
                           # create distance matrix

                           withCallingHandlers({
                             mat <- distm(longlat,
                                          longlat,
                                          fun=distGeo)
                           }, warning=function(w) {
                             if (startsWith(conditionMessage(w), c("longitude")))
                               invokeRestart("muffleWarning")
                           })

                           link <- which(mat < d, arr.ind=T)
                           link <- link[!(link[,1]==link[,2]),]
                           grid <- data.frame(x=longlat[link[,1],]$long,
                                              xend = longlat[link[,2],]$long,
                                              y= longlat[link[,1],]$lat,
                                              yend = longlat[link[,2],]$lat)
                         },
                         required_aes = c("x", "y")
)
