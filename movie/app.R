library(shiny)
library(bslib)
library(ggplot2)

file <- "https://github.com/rstudio-education/shiny-course/raw/main/movies.RData" # nolint
destfile <- "movies.RData"

download.file(file, destfile)

load("movies.RData")

ui <- page_sidebar(
  sidebar = sidebar(
    selectInput(
      inputId = "y",
      label = "Y-axis:",
      choices = c("imdb_rating", "imdb_num_votes", 
                  "critics_score", "audience_score", "runtime"),
      selected = "audience_score"
    ),
    selectInput(
      inputId = "x",
      label = "X-axis:",
      choices = c("imdb_rating", "imdb_num_votes", 
                  "critics_score", "audience_score", "runtime"),
      selected = "critics_score"
    )
  ),
  card(plotOutput(outputId = "scatterplot"))
)

server <- function(input, output) {
  output$scatterplot <- renderPlot({
    ggplot(data = movies, aes_string(x = input$x, y = input$y)) +
      geom_point()
  })
}

shinyApp(ui, server)