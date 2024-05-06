library(shiny)
library(plotly)
library(gridlayout)
library(bslib)
library(DT)
library(dplyr)


load("imdb.random.RData")

genre.list=c(
  "Comedy",
  "Romance",
  "Horror",
  "Western",
  "Drama",
  "Adventure",
  "Action",
  "Animation",
  "Sci-Fi"
  )

ui <- grid_page(
  layout = c(
    "header      header",
    "sidebar     plotly",
    "recommend   table "
  ),
  gap_size = "1rem",
  col_sizes = c(
    "250px",
    "1.41fr"
  ),
  row_sizes = c(
    "80px",
    "7fr",
    "5fr"
  ),
  grid_card(
    area = "sidebar",
    card_header("Settings"),
    card_body(
      selectInput(
        inputId = "input.x",
        label = "X-axis",
        choices = list(
          "Rating" = "Rating",
          "Year" = "Year",
          "Duration" = "Duration",
          "Genre" = "Genre"
        ),
        selected = "Year"
      ),
      selectInput(
        inputId = "input.y",
        label = "Y-axis",
        choices = list(
          "Rating" = "Rating",
          "Year" = "Year",
          "Duration" = "Duration"
        ),
        selected = "Rating"
      ),
      checkboxGroupInput(
        inputId = "input.g",
        label = "Genre",
        choices = genre.list,
        selected = genre.list
      ),
      sliderInput(
        inputId = "input.r",
        label = "Rating",
        min = 0,
        max = 10,
        value = c(0,10),
        width = "100%",
        step = 0.1
      )
    )
  ),
  grid_card_text(
    area = "header",
    content = "Silverscreen: Letting the Stars Shine",
    alignment = "start",
    is_title = TRUE
  ),
  grid_card(
    area = "table",
    card_header("Summary"),
    card_body(DTOutput(outputId = "myTable", width = "100%"))
  ),
  grid_card(
    area = "plotly",
    card_body(
      plotlyOutput(
        outputId = "myPlot",
        width = "100%",
        height = "100%"
      )
    )
  ),
  grid_card(
    area = "recommend",
    full_screen = TRUE,
    card_header("Randomization"),
    card_body(
      actionButton(inputId = "random", label = "Recommend!"),
      uiOutput(outputId = "myHTML")
    )
  )
)

