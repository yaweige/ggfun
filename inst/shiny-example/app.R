library(shiny)

# set up package
library(dplyr)
library(ggplot2)
library(ggfun)


# Layer_PersHomo data source: https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1
data(eqRaw)


# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage("ggfun",
             tabPanel("geom_image",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("x_variable",
                                      label = "Choose variable on x axis",
                                      choices = list("mpg" = "mpg", "cyl" = "cyl", "disp" = "disp",
                                                     "drat" = "drat", "wt" = "wt", "qsec" = "qsec",
                                                     "vs" = "vs", "am" = "am", "gear" = "gear",
                                                     "carb" = "carb"),
                                      selected = "mpg"
                          ),
                          selectInput("y_variable",
                                      label = "Choose variable on y axis",
                                      choices = list("mpg" = "mpg", "cyl" = "cyl", "disp" = "disp",
                                                     "drat" = "drat", "wt" = "wt", "qsec" = "qsec",
                                                     "vs" = "vs", "am" = "am", "gear" = "gear",
                                                     "carb" = "carb"),
                                      selected = "mpg"
                          ),
                          sliderInput("size", label = "Choose image size",
                                      min = 0.01, max = 0.3, value = 0.1),
                          radioButtons("img", label = "Choose an image",
                                       choices = list("Donkey" = "donkey.jpg",
                                                      "Elephant" = "elephant.jpg",
                                                      "Donkey and elephant" = "images.jpg"),
                                       selected = "donkey.jpg")

                        ),

                        # Show a plot of the generated distribution
                        mainPanel(
                          plotOutput("geomimage"),
                          helpText("Example code:",
                                   "ggplot(data = mtcars, aes(x = mpg, y = wt)) +
                            geom_image()")
                        )
                      )
             ),

             tabPanel("stat_star",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("datasize", "Data size",
                                      min = 2, max = 200, value = 50),
                          selectInput("x_dist", label = "Choose distribution on x axis",
                                      choices = list("Normal" = "normal", "f-distribution" = "f"),
                                      selected = "f"),
                          selectInput("y_dist", label = "Choose distribution on y axis",
                                      choices = list("Normal" = "normal", "f-distribution" = "f"),
                                      selected = "normal"),
                          selectInput("starcolor", "Line color",
                                      choices = list("black" = "black", "red" = "red", "yellow" = "yellow",
                                                     "green" = "green", "blue" = "blue", "pink" = "pink"),
                                      selected = "black"),
                          sliderInput("starsize", "Lline size",
                                      min = 0.1, max = 3, value = 0.5),

                          conditionalPanel("input.x_dist === 'normal'",
                                           sliderInput("xnorm_mean", "x axis mean",
                                                       min = -10, max = 10, value = 0),
                                           sliderInput("xnorm_var", "x axis variance",
                                                       min = 0.01, max = 10, value = 1)),
                          conditionalPanel("input.y_dist === 'normal'",
                                           sliderInput("ynorm_mean", "y axis mean",
                                                       min = -10, max = 10, value = 0),
                                           sliderInput("ynorm_var", "y axis variance",
                                                       min = 0.01, max = 10, value = 1))

                        ),

                        # Show a plot of the generated distribution
                        mainPanel(
                          plotOutput("statstar"),
                          helpText("Example code:",
                                   "ggplot(data, aes(x, y)) + geom_point() +
                                   stat_star()")
                        )
                      )),

             tabPanel("stat_ars",
                      sidebarLayout(
                        sidebarPanel(

                          helpText("Draw Archimedean spiral:"),

                          sliderInput("a", "Rotating the Spiral a:", 1, min = -50, max = 50),


                          sliderInput("b", "Distance Control b:", 1, min = -50, max = 50),


                          sliderInput("n", "n:", 1, min = 1, max = 50)
                        ),

                        mainPanel(
                          plotOutput("statars"),
                          height = "auto",
                          helpText("Example Code:",
                                   "ggplot() +
                                   stat_ars(aes(a = a, b = b, n = n)")
                                   )

                                   )),
             tabPanel("stat_rl",
                      sidebarLayout(
                        sidebarPanel(

                          helpText("Draw regression line for each level:"),

                          numericInput("Xn", "Choose sample size for the Standardized Normal distribution(X):", min = 1, max = 500, value = 100),


                          numericInput("Yn", "Choose sample size for the Standardized Normal distribution(Y):", min = 1, max = 500, value = 100),


                          numericInput("Id", "Choose categorical variable length", min = 1, max = 500, value = 100),

                          numericInput("l", "Choose number of the levels for categorical variable", min = 2, max = 500, value = 2)
                        ),

                        mainPanel(
                          plotOutput("statrl"),
                          height = "auto",
                          helpText("Example Code:",
                                   "ggplot(data, aes(x = x, y = y)) +
                                    geom_point() +
                                   stat_rl(aes(x = x, y = y, id = id)")
                                   )

                                   )),

             tabPanel("stat_arrowmap",
                      sidebarLayout(
                        sidebarPanel(
                          textInput("saysomething", "Do you want to say something?",
                                    value = ""),
                          verbatimTextOutput("text"),
                          radioButtons("arrowtype", "Arrow type",
                                       choices = list("closed" = "closed", open = "open"),
                                       selected = "open"),
                          conditionalPanel("input.arrowtype === 'closed'",
                                           selectInput("arrowfill", "Arrow head fill color",
                                                       choices = list("black" = "black", "red" = "red", "yellow" = "yellow",
                                                                      "green" = "green", "blue" = "blue", "pink" = "pink"),
                                                       selected = "black")),
                          sliderInput("arrowlength", "Arrow head length",
                                      min = 0.05, max = 0.5, value = 0.01),
                          sliderInput("arrowsize", "Arrow line size",
                                      min = 0.05, max = 2, value = 0.5)

                        ),



                        # Show a plot of the generated distribution
                        mainPanel(
                          plotOutput("statarrowmap"),
                          helpText("Example code:",
                                   "ggplot(certain_data) + geom_path(aes(long, lat, group)) +
                                    stat_arrowmap(aes(long, lat, change, group))")
                        )
                      )),

             tabPanel("layer_PersHomo",
                      sidebarLayout(
                        sidebarPanel(
                          h3("Mode Setup"),
                          radioButtons("maptype", "Set the type of map",
                                       choices = c("Pacific-centred", "Peters projection" ), selected = "Pacific-centred"),
                          radioButtons("cp", "Set the Scenario",
                                       choices = c("Pacific Plate", "Country"), selected = "Pacific Plate"),
                          selectInput("region", "Select the country / region",
                                      choices = c(unique(eqRaw$COUNTRY)), selected = c(), multiple = TRUE),
                          br(),
                          h3("Investigation Control"),
                          sliderInput("eqDate", "Set a Time Range of observation in AD",
                                      min = -70, max = 2019, value = c(-70,2019)),
                          sliderInput("MAG", "Set minimum Magnitude of earthquake in Ms",
                                      min = 0, max = 15, value = 0),
                          sliderInput("d", "Set the Persistent Homology Radius in km",
                                      min = 0, max = 1000000, value = 150000)
                        ),
                        # Show a plot of the generated world map with linkage
                        mainPanel(
                          plotOutput("PersHomoMap"),
                          height="auto"
                        )
                      ))


  )
)

  # Sidebar with a slider input for number of bins

# Define server logic required to draw a histogram
server <- function(input, output,session) {

  output$geomimage <- renderPlot({
    p <- system.file("extdata", input$img, package = "ggfun")
    img <- magick::image_read(p)
    ggplot(data = mtcars, aes_string(x = input$x_variable, y = input$y_variable)) +
      geom_image(size = input$size, img = img) +
      ggtitle("mtcars data set")
  })

  output$statstar <- renderPlot({
    if (input$x_dist == "normal") x <- rnorm(n = input$datasize, mean = input$xnorm_mean, sd = input$xnorm_var)
    if (input$x_dist == "f") x <- rf(n = input$datasize, df1 = 5, df2 = 2)
    if (input$y_dist == "normal") y <- rnorm(n = input$datasize, mean = input$ynorm_mean, sd = input$ynorm_var)
    if (input$y_dist == "f") y <- rf(n = input$datasize, df1 = 5, df2 = 2)

    data <- data.frame(x = x, y = y)
    ggplot(data = data, aes(x = x, y = y)) +
      geom_point() +
      stat_star(size = input$starsize, color = input$starcolor)
  })


  output$statars <- renderPlot({
    ggplot() +
      stat_ars(aes(a = input$a, b = input$b, n = input$n), col = "coral")
  })

  output$statrl <- renderPlot({
    ggplot(data = data.frame(x = rnorm(input$Xn, mean = 0, sd = 1),
                             y = rnorm(input$Yn, mean = 0, sd = 1),
                             id = sample(as.factor(c(1:input$l)), input$Id, replace = T)), aes(x = x, y = y)) +
      geom_point() +
      stat_rl(aes(x = x, y = y, id = id, colour = id))
  })

  usmap <- map_data("state")
  output$statarrowmap <- renderPlot({
    madedata_standard <- data.frame(region = unique(usmap$region), change = (runif(49)-0.5)*2,
                                    stringsAsFactors = F)
    madedata_standard <- madedata_standard %>%
      left_join(usmap,by = "region")

    madedata_standard %>%
      ggplot() +
      geom_path(aes(x = long, y = lat, group = group)) +
      stat_arrowmap(aes(x = long, y = lat, change = change, group = region),
                    curvature = 0.3, angle = 60, arrow.fill = input$arrowfill,
                    size = input$arrowsize,
                    arrow = arrow(type = input$arrowtype,
                                  length = unit(input$arrowlength, "inches")))
  })

  output$PersHomoMap <- renderPlot({
    # data tidying
    if (input$maptype =="Peters projection"){
      if (input$cp == "Pacific Plate"){
        eq <- eqRaw %>% filter(EQ_MAG_MS > input$MAG) %>%
          filter(YEAR > input$eqDate[1] & YEAR < input$eqDate[2]) %>%
          filter(LONGITUDE > 110 | LONGITUDE < -45)
        basemap <- map_data("world")
      }
      else if(input$cp == "Country"){
        eq <- eqRaw %>% filter(COUNTRY %in% input$region) %>%
          filter(EQ_MAG_MS > input$MAG) %>%
          filter(YEAR > input$eqDate[1] & YEAR < input$eqDate[2])
        basemap <- map_data("world")%>% mutate(region = toupper(region)) %>%
          filter(region %in% input$region)
      }
    }
    else if (input$maptype =="Pacific-centred"){
      if (input$cp == "Pacific Plate"){
        eq <- eqRaw %>% filter(EQ_MAG_MS > input$MAG) %>%
          filter(YEAR > input$eqDate[1] & YEAR < input$eqDate[2]) %>%
          mutate(LONGITUDE = ifelse(LONGITUDE < 0, LONGITUDE + 360, LONGITUDE)) %>%
          filter(LONGITUDE > 110 & LONGITUDE < -45+360)
        basemap <- map_data("world2")
      }
      else if(input$cp == "Country"){
        eq <- eqRaw %>% filter(COUNTRY %in% input$region) %>%
          filter(EQ_MAG_MS > input$MAG) %>%
          filter(YEAR > input$eqDate[1] & YEAR < input$eqDate[2]) %>%
          mutate(LONGITUDE = ifelse(LONGITUDE < 0, LONGITUDE + 360, LONGITUDE))

        basemap <- map_data("world2")%>% mutate(region = toupper(region)) %>%
          filter(region %in% input$region)
      }
    }

    ## plot base map
    p <- ggplot() +
      geom_polygon(data=basemap, aes(x=long, y=lat, group = group),
                   fill="white", colour="#7f7f7f", size=0.5) +
      ggtitle("Earthquake around Pacific Plate") +
      ggtitle("Persistent Homology of Historical Earthquake") +
      theme(axis.line=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position="none",
            plot.background=element_blank()
      ); p
    ## add layer_PersHomo
    fp <- p + geom_point(data= eq, mapping= aes(x=LONGITUDE, y=LATITUDE, size = EQ_MAG_MS),colour = "red" , alpha =.05) +
      layer_PersHomo(data= eq, mapping = aes(x=LONGITUDE, y=LATITUDE), d=input$d, colour = "blue", alpha =.07); fp
  },

  height = function() {
    session$clientData$output_PersHomoMap_width * 2/3
  })

}

# Run the application
shinyApp(ui = ui, server = server)

