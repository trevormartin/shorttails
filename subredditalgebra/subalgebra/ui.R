# Load the ggplot2 package which provides
# the 'mpg' dataset.
library(ggplot2)

shinyUI(
fluidPage(
  titlePanel("Reddit Subreddit Algebra"),

  # Create a new Row in the UI for selectInputs
  fluidRow(
    column(3,
        textInput("sub1", "Subreddit 1:", 
                    value="dataisbeautiful",placeholder="Enter a subreddit name")
    ),
    column(1,
       selectInput("alg1", label = "", 
                    choices = list("+" = "+", "-" = "-"), 
                    selected = "+") 
    ),
    column(3,
        textInput("sub2", "Subreddit 2:", 
                    value="",placeholder="Enter a subreddit name")
    ),
    column(1,
       selectInput("alg2", label = "", 
                    choices = list("+" = "+", "-" = "-"), 
                    selected = "+") 
    ),
    column(3,
        textInput("sub3", "Subreddit 3:", 
                    value="",placeholder="Enter a subreddit name")
    )
  ),
  # Create a new row for the table.
  fluidRow(
    DT::dataTableOutput("table")
  )
)
)
