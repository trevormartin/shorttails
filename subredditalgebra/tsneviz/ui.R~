library(shiny)
library(scatterD3)

load("./data/tsnedata.rdata")
d = data.frame(tsnecoords)
colnames(d) = c("X","Y")
d$cluster = tsneclusters
d$sizes = tsnesizes
d$labels = rownames(d)
names(tsnecolors) = 1:length(tsnecolors)

shinyUI(
fluidPage(
  titlePanel("The Reddit Subreddit Universe"),
  div(class="row",
      div(class="col-md-12",
          div(class="alert alert-warning alert-dismissible",
              HTML('<button type="button" class="close" data-dismiss="alert" aria-label="Close"><span aria-hidden="true">&times;</span></button>'),
              HTML("<strong>Tips for exploring:</strong>
                   <ul>
                   <li>Zoom on the chart with the mousewheel</li>
                   <li>Pan the plot</li>
                   <li>Drag text labels</li>
                   <li>Hover over a dot to display subreddit info</li>
                   <li>Click on a point to open the subreddit</li>
                   </ul>")))),
  fluidRow(
    column(6, align="center",
    sliderInput("scatterD3_nb", "Number of subreddits to show (ordered by size): ", min = 3, max = nrow(d), step = 1, value = 1000, width="100%"),
    tags$p(actionButton("scatterD3-reset-zoom", HTML("<span class='glyphicon glyphicon-search' aria-hidden='true'></span> Reset Zoom")),
           actionButton("scatterD3-lasso-toggle", HTML("<span class='glyphicon glyphicon-screenshot' aria-hidden='true'></span> Toggle Lasso"), "data-toggle" = "button"),
           tags$a(id = "scatterD3-svg-export", href = "#",
                  class = "btn btn-default", HTML("<span class='glyphicon glyphicon-save' aria-hidden='true'></span> Download SVG"))),
    HTML("For details and highlights see the <a href='http://www.shorttails.io' target='_blank'>Short Tails</a> blog.")
    ),
    column(3,align="left",
    checkboxInput("scatterD3_lab", "Label points with subreddit names", value = TRUE),
    checkboxInput("scatterD3_col", "Color points with kmeans cluster", value = TRUE),
    checkboxInput("scatterD3_size", "Scale points with number of commenters", value = FALSE)),
    column(3,align="center",
    sliderInput("scatterD3_labsize", "Label size:", min = 5, max = 25, value = 11),
    sliderInput("scatterD3_opacity", "Point opacity:", min = 0, max = 1, value = 1, step = 0.05))
  ),
  scatterD3Output("scatterPlot", height = "600px")
)
)
