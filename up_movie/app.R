library(shiny)
library(ggplot2) # data visualizaton
library(DT) # datatables
library(stringr)
library(dplyr)
library(tools)

file <- "https://github.com/rstudio-education/shiny-course/raw/main/movies.RData" # nolint
destfile <- "movies.RData"

load("movies.RData")

ui <- fluidPage(
  titlePanel("Movie browser"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "y",
        label = "Y-axis:",
        choices = c("IMDB rating" = "imdb_rating",
                    "IMDB number of votes" = "imdb_num_votes",
                    "Critics Score" = "critics_score",
                    "Audience Score" = "audience_score",
                    "Runtime" = "runtime"),
        selected = "audience score"
      ), # set variable for y-axis

      selectInput(
        inputId = "x",
        label = "X-axis:",
        choices = c("IMDB rating" = "imdb_rating",
                    "IMDB number of votes" = "imdb_num_votes",
                    "Critics Score" = "critics_score",
                    "Audience Score" = "audience_score",
                    "Runtime" = "runtime"),
        selected = "critics_score",
      ), # set variable for x-axis

      selectInput(
        inputId = "z",
        label = "Color by:",
        choices = c("Title Type" = "title_type",
                    "Genre" = "genre",
                    "MPAA Rating" = "mpaa_rating",
                    "Critics Rating" = "critics_rating",
                    "Audience Rating" = "audience_rating"),
        selected = "mpaa_rating"
      ), # variable for color

      sliderInput(
        inputId = "alpha",
        label = "Alpha:",
        min = 0, max = 1,
        value = 0.5
      ), # set alpha level

      sliderInput(
        inputId = "size",
        label = "Size:",
        min = 0, max = 5,
        value = 2
      ), # set point size

      checkboxInput(
        inputId = "show_data",
        label = "Show data table",
        value = TRUE
      ), # toggle showing data table

      textInput(
        inputId = "plot_title",
        label = "Plot title",
        placeholder = "Enter text to be used as a plot title"
      ), # text for plot title

      hr(), # horizontal line

      checkboxGroupInput(
        inputId = "selected_type",
        label = "Select movie type(s):",
        choices = c("Documentary", "Feature Film", "TV Movie"),
        selected = "Feature Film"
      ), # select type of movie

      numericInput(
        inputId = "n_samp",
        label = "Sample size:",
        min = 1,
        max = nrow(movies),
        value = 50
      ), # select sample size

      actionButton(
        inputId = "write_csv",
        label = "Write CSV"
      )
    ),

    # show scatterplot and data table
    mainPanel(
      plotOutput(outputId = "scatterplot"),
      br(),

      uiOutput(outputId = "scatterplot"),
      br(), br(),

      DT::dataTableOutput(outputId = "moviestable")
    )
  )
) # create a page with fluid layout

server <- function(input, output) {
  movies_subset <- reactive({
    req(input$selected_type) # check that the value exists
    filter(movies, title_type %in% input$selected_type)
    # literally filters the movies based on a specified type
  })

  observe({
    updateNumericInput(
      session,
      inputId = "n_samp",
      value = min(50, nrow(movies_subset())),
      max = nrow(movies_subset())
    )
  }) # gets a max sample size based on the input

  movies_sample <- reactive({
    req(input$n_samp)
    sample_n(movies_subset(), input$n_samp)
  }) # gets n_samp rows from the dataset, randomly

  pretty_plot_title <- reactive({ toTitleCase(input$plot_title) })
  # format title properly

  output$scatterplot <- renderPlot({
    ggplot(data = movies_sample(), aes_string(x = input$x, y = input$y,
                                              color = input$z)) +
      geom_point(alpha = input$alpha, size = input$size) +
      labs(x = toTitleCase(str_replace_all(input$x, "_", " ")),
        y = toTitleCase(str_replace_all(input$y, "_", " ")),
        color = toTitleCase(str_replace_all(input$z, "_", " ")),
        title = isolate({ pretty_plot_title() }) 
        # isolate doesnt update title every input change
      )
  })

  output$n <- renderUI({
    types <- movies_sample()$title_type %>%
      factor(levels = input$selected_type)
    counts <- table(types)

    HTML(paste("There are", counts, input$selected_type,
               "movies in this dataset. <br>"))
  })
}

shinyApp(ui, server)