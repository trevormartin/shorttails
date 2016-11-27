library(shiny)
library(scatterD3)

# Great vignette here: https://cran.r-project.org/web/packages/scatterD3/vignettes/introduction.html
# Template available here: https://github.com/juba/scatterD3_shiny_app
load("./data/tsnedata.rdata")
d = data.frame(tsnecoords)
colnames(d) = c("X","Y")
d$cluster = tsneclusters
d$sizes = tsnesizes
d$labels = rownames(d)
names(tsnecolors) = 1:length(tsnecolors)

default_lines <- data.frame(slope = c(0, Inf), 
                            intercept = c(median(d$Y), median(d$X)),
                            stroke = "#000",
                            stroke_width = 1,
                            stroke_dasharray = c(5, 5))

shinyServer(
function(input, output) {
  
  data <- reactive({
    d[1:input$scatterD3_nb,]
  })
  
  lines <- reactive({
    default_lines
  })

  tooltipstext <- reactive({
    paste("<strong>Subreddit: </strong>", rownames(d[1:input$scatterD3_nb,]),"<br /><strong>Number of Commenters: </strong>", d[1:input$scatterD3_nb,"sizes"])
  })
  
  output$scatterPlot <- renderScatterD3({
    #col_var <- if (input$scatterD3_col == "None") NULL else tsnecolors[data()[,"cluster"]]
    col_var <- if (input$scatterD3_col == FALSE) NULL else as.character(data()[,"cluster"])
    size_var <- if (input$scatterD3_size == FALSE) NULL else log10(data()[,"sizes"])
    lab_var <- if (input$scatterD3_lab == FALSE) NULL else rownames(data())
    scatterD3(x = data()[,"X"],
              y = data()[,"Y"],
              lab = lab_var,
              xlab = "tSNE X Axis",
              ylab = "tSNE Y Axis",
              col_var = col_var,
              col_lab = "K-Means Clusters",
              size_var = size_var,
              size_lab = "Commenters (Log10)",
              url_var = paste0("http://www.reddit.com/r/", rownames(data())),
              key_var = rownames(data()),
              point_opacity = input$scatterD3_opacity,
              labels_size = input$scatterD3_labsize,
              transitions = FALSE,
              #left_margin = 90,
              lines = lines(),
              lasso = TRUE,
	      tooltips = TRUE,
	      tooltip_text = tooltipstext(),
              caption = list(title = "The Reddit Subreddit Universe",
                             subtitle = "An interactive tool for exploring subreddit relatedness through tSNE clustering",
                             text = "A positive pointwise mutual information vector was created for each subreddit based on co-commenters across subreddits. The resulting matrix was then clustered using tSNE. See the <a href='http://www.shorttails.io'>Short Tails</a> blog post for details."),
	      lasso_callback = "function(sel) {alert(sel.data().map(function(d) {return d.lab}).join('\\n'));}")
  })
}
)
