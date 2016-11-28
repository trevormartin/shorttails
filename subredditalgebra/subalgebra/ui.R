require(shiny)

initvals = list(
c("dataisbeautiful","",""),
c("nba","cleveland",""),
c("books","scifi",""),
c("sanfrancisco","sanjose","sacramento"),
c("running","bodybuilding","")
)
#curinit = round(length(initvals)*runif(1),0)
curinit = 1

shinyUI(
fluidPage(
tags$head(
  tags$style(HTML("

.shiny-progress .progress-text {
  position: absolute;
  right: 10px;
  height: 40px;
  width: 500px;
  background-color: #eef8ff;
  margin: 0px;
  padding: 2px 3px;
  opacity: 0.85;
}

.shiny-progress .progress-text .progress-message {
  padding: 0px 3px;
  font-weight: bold;
  font-size: 150%;
}
  "))
),

  titlePanel("Reddit Subreddit Similarity and Algebra"),
div(class="row",
      div(class="col-md-12",
          div(class="alert alert-warning alert-dismissible",
              HTML('<button type="button" class="close" data-dismiss="alert" aria-label="Close"><span aria-hidden="true">&times;</span></button>'),
              HTML("<strong>Tips for exploring:</strong>
              <ul>
              <li>Enter a single subreddit to calculate the most related subreddits. </li>
              <li>Add and subtract multiple subreddits to find subreddits most related to their combination. </li>
              </ul>")))),

  # Row with user inputs
  fluidRow(
    column(3,
        textInput("sub1", "Subreddit 1:", 
                    value=initvals[[curinit]][1],placeholder="Enter a subreddit (required)")
    ),
    column(1,
       selectInput("alg1", label = "", 
                    choices = list("+" = "+", "-" = "-"), 
                    selected = "+") 
    ),
    column(3,
        textInput("sub2", "Subreddit 2:", 
                    value=initvals[[curinit]][2],placeholder="Enter a subreddit (optional)")
    ),
    column(1,
       selectInput("alg2", label = "", 
                    choices = list("+" = "+", "-" = "-"), 
                    selected = "+") 
    ),
    column(3,
        textInput("sub3", "Subreddit 3:", 
                    value=initvals[[curinit]][3],placeholder="Enter a subreddit (optional)")
    )
  ),
  HTML("For details and highlights see the <a href='http://www.shorttails.io' target='_blank'>Short Tails</a> blog."),
  hr(),
  # Create a new row for the table.
  fluidRow(
    DT::dataTableOutput("table")
  )
)
)
