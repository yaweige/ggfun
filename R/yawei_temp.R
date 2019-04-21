library(ggplot2)
library(dplyr)
library(devtools)
library(graphics)
library(grid)
#library(EBImage)
library(magick)


# stat_star-------------------------------------------------------
# how to draw a star in a scatter plot?

# 1. write out a function or algrithm to find the five points we need
# 2. convert it to a geom

madedata <- data.frame(x = rnorm(100, mean = 0, sd = 1),
                       y = rnorm(100, mean = 0, sd = 1))

madedata %>%
  ggplot(aes(x = x, y = y)) +
  geom_point()

# to find to the five points(simplified)
# 1. find the central point, (median, median)
# 2.
# then horizontal distance)
# We want to draw 4 lines to connect the central to each corner point

StatStar <- ggproto("StatStar", Stat,
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

stat_star <- function(mapping = NULL, data = NULL, geom = "line",
                      position = "identity", na.rm = FALSE, show.legend = NA,
                      inherit.aes = TRUE, ...){
  layer(
    stat = StatStar, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

madedata %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  stat_star(color = "red")


# geom_arrowmap---------------------------------------------------

madedata__seg <- data.frame(x = rnorm(5, mean = 0, sd = 1),
                            y = rnorm(5, mean = 0, sd = 1),
                            xend = rnorm(5, mean = 3, sd = 1),
                            yend = rnorm(5, mean = -3, sd = 1))

madedata__seg %>%
  ggplot() +
  geom_point(aes(x = x, y = y)) +
  geom_point(aes(x = xend, y = yend), color = "red") +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow())

madedata__seg %>%
  ggplot() +
  geom_point(aes(x = x, y = y)) +
  geom_point(aes(x = xend, y = yend), color = "red") +
  geom_curve(aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow())

madedata__seg %>%
  ggplot() +
  geom_curve(aes(x = x, y = y, xend = xend, yend = yend),
             arrow = arrow(type = "closed"), angle = 90, curvature = -0.5,
             arrow.fill = "red", lineend = "round", size = 2, color = "blue")

# the functions above basiclly worked well to produce an arrow
# which makes drawing an arrow map much simpler than I thought


# create a fake data set mimicing the US election
usmap <- map_data("state")

p1 <- usmap %>%
  ggplot() +
  geom_path(aes(x = long, y = lat, group = group))

# find the central point for each state

medpoint <- usmap %>%
  group_by(region) %>%
  summarise(medlong = mean(range(long)),
            medlat = mean(range(lat)))

# produce a fake data set
electiondata <- data.frame(change = (runif(49)-0.5)*2,
                           medpoint)


p1 + geom_point(data = electiondata, aes(x = medlong, y = medlat, size = change))

trans_electiondata <- electiondata %>%
  mutate(medlong_end = medlong + 0.071*change*(max(medlong)-min(medlong)),
         medlat_end = medlat + 0.02*abs(change)*(max(medlat)-min(medlat)))

# used this one below
p1 + geom_curve(data = trans_electiondata,
                aes(x = medlong, y = medlat,
                    xend = medlong_end, yend = medlat_end, color = factor(floor(change))),
                arrow = arrow(length = unit(0.05, "inches")),
                curvature = 0, size = 1)

p1 + geom_segment(data = trans_electiondata,
                aes(x = medlong, y = medlat,
                    xend = medlong_end, yend = medlat_end, color = factor(floor(change))),
                arrow = arrow(length = unit(0.05, "inches"), type = "closed"), size = 1)

p1 + geom_segment(data = trans_electiondata,
                  aes(x = medlong, y = medlat,
                      xend = medlong_end, yend = medlat_end, color = factor(floor(change))),
                  arrow = arrow(length = unit(0.05, "inches")), size = 1)

# problems in this plot:
# 1. the arrow length is not adjusted according to the curve length
# 2. curvature is a same value for those going to right and left, which is turned out weird
# 3. also need a reasonable default
# 4. some arrow are very small, and are not well shown in the picture
# 5. may need a better way to produce the arrow, which turned out better
# 6. need a better way to find the central of each region
# overall, it far less elegent than the one from web

# wrap it into a function-----------------------------------------




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
                        # this means other information in the original data is missed?
                        # so that you can use other aes other than those required?
                        # not really, it still there, but don't know how to connect
                        # with the processed data. ggplot design has some capibility to
                        # fix this, even if this is true
                        # No it is not missed. I'm wrong at the begining
                        },

                        required_aes = c("x", "y", "change")
                        )

# is there a geom = "curve"?
stat_arrowmap <- function(mapping = NULL, data = NULL, geom = "curve",
                          position = "identity", na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE, ...) {
  layer(
    stat = StatArrowmap, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

# GeomArrowmap <- ggproto("GeomArrowmap", GeomCurve,
#                         setup_params = function(data, params) {
#                         if (any(is.null(params$curvature),
#                                 is.null)) aaa
#                         })

# after the first try of making the function-----------------
# We can redefine the default aes, but how to redefine the default value of
# other parameters


madedata_stat_arrowmap_1 <- data.frame(long = c(runif(10), runif(10, min = 3, max = 4)),
                                       lat = c(runif(10), runif(10, min = 3, max = 4)),
                                       change = runif(20),
                                       region = c(rep(1, 10), rep(2, 10)))

madedata_stat_arrowmap_1 %>%
  ggplot() +
  stat_arrowmap(aes(x = long, y = lat, change = change, color = "red"))

madedata_stat_arrowmap_1 %>%
  ggplot() +
  stat_arrowmap(aes(x = long, y = lat, change = change, color = factor(region)))

# something wrong, why is it? There it is, toooooooooo small!!!!!!!!

# A quetion: what kind of data you want to deal with-----------------------------------
# clearly define the question before making up the function
# the data is: (49 rows, each row for a state, contains change, state)
madedata_standard <- data.frame(region = unique(usmap$region), change = (runif(49)-0.5)*2,
                                stringsAsFactors = F)
madedata_standard <- madedata_standard %>%
  left_join(usmap,by = "region")

# have a try, 1) doesn't work, 2) works
# 1)
madedata_standard %>%
  ggplot() +
  geom_path(aes(x = long, y = lat, group = group)) +
  stat_arrowmap(aes(x = long, y = lat, change = change))

# 2)
madedata_standard %>%
  ggplot() +
  geom_path(aes(x = long, y = lat, group = group)) +
  stat_arrowmap(aes(x = long, y = lat, change = change, group = region))

# 3)

madedata_standard %>%
  ggplot() +
  stat_arrowmap(aes(x = long, y = lat, change = change))

# what does your function want to do?
# To generate a arrow, you need a start point and end point forr each arrow (row of our
# defined data). So this function wants to complete this process for you.

# but you don't know the lat and long you want to use, so if you want to produce the
# start and end points, it still needs lat and long in formation. So We have to ajust
# our standard data to join with the map information. Then, let's try the function once
# again.



# is there a geom = "curve"?
stat_arrowmap <- function(mapping = NULL, data = NULL, geom = "curve",
                          position = "identity", na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE, ...) {
  layer(
    stat = StatArrowmap, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

# after doing this, try to replace the point object by elephant and donkey----------

data(mtcars)
mtcars %>%
  ggplot(aes(x = mpg, y = disp)) +
  geom_point(shape = 83)

data(mtcars)
mtcars %>%
  ggplot(aes(x = mpg, y = disp)) +
  geom_point(shape = "%")
# a package called ggimage

# another example from internet: this one works now------------------

img <- image_read(system.file("img", "Rlogo.png", package = "png"))

RlogoGrob <- function(x, y, size, img) {
  rasterGrob(x = x, y = y, image = img, default.units = "native", height = size,
             width = size)
}

GeomRlogo <- ggproto("GeomRlogo", Geom, draw_panel = function(data, panel_scales,
                                                              coord, img, na.rm = FALSE) {
  coords <- coord$transform(data, panel_scales)
  ggplot2:::ggname("geom_Rlogo", RlogoGrob(coords$x, coords$y, coords$size,
                                           img))
}, non_missing_aes = c("Rlogo", "size"), required_aes = c("x", "y"), default_aes = aes(size = 0.05),
icon = function(.) {
}, desc_params = list(), seealso = list(geom_point = GeomPoint$desc),
examples = function(.) {
})

geom_Rlogo <- function(mapping = NULL, data = NULL, stat = "identity",
                       position = "identity", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                       ...) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomRlogo,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, img = img, ...))
}
ggplot(mtcars, aes(wt, mpg))+geom_Rlogo()

# to imitate the one from above -------------------------

# img <- image_read("./images.jpg")
# #save(img, file = "sysdata.rda")
# img <- image_raster(img)
#
# ImageGrob <- function(x, y, size, img) {
#   rasterGrob(x = x, y = y, image = img, default.units = "native", height = size,
#              width = size)
# }
#
# GeomImage <- ggproto("GeomImage", Geom,
#                      draw_panel = function(data, panel_scales,
#                                            coord, img, na.rm = FALSE) {
#                        coords <- coord$transform(data, panel_scales)
#                        ggname = getFromNamespace("ggname", "ggplot2")
#                        ggname("geom_image", ImageGrob(coords$x, coords$y, coords$size,
#                                                                 img))
#                      },
#                      non_missing_aes = c("image", "size"),
#                      required_aes = c("x", "y"),
#                      default_aes = aes(size = 0.05),
#                      icon = function(.) {
#                      },
#                      desc_params = list(),
#                      seealso = list(geom_point = GeomPoint$desc),
#                      examples = function(.) {
#                      }
# )
#
#
# geom_image <- function(mapping = NULL, data = NULL, stat = "identity",
#                        position = "identity", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
#                        ...) {
#   layer(data = data, mapping = mapping, stat = stat, geom = GeomImage,
#         position = position, show.legend = show.legend, inherit.aes = inherit.aes,
#         params = list(na.rm = na.rm, img = img, ...))
# }
#
# ggplot(mtcars, aes(wt, mpg)) + geom_image()
#















