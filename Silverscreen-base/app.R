library(shiny)
library(plotly)
library(gridlayout)
library(bslib)
library(DT)
library(dplyr)

library(bslib)


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
    "sidebar     plot",
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
  
  ## Alternate header, with dark mode input
  #   grid_card(
  #   area = "header",
  #   
  #     div(style="display:flex ; align-items: center ; margin: 1.15rem;",
  #         h3("Silverscreen: Letting the Stars Shine"),
  #         input_dark_mode(style = "margin-left: auto;")) ,
  #     wrapper=card_header()
  #   
  # ),
  grid_card(
    area = "table",
    card_header("Summary"),
    card_body(DTOutput(outputId = "myTable", width = "100%"))
  ),
  grid_card(
    area = "plot",
    card_body(
      plotlyOutput(
        outputId = "myPlot",
        width = "100%",
        height = "100%"
      )
    )
  ),
## Alternate plot, using basic ggplot
#   grid_card(
#     area = "plot",
#     card_body(
#       plotOutput(
#         outputId = "myPlot",
#         width = "100%",
#         height = "100%"
#       )
#     )
#   ),
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


server <- function(input, output) {
  selectedData <- reactive({imdb.random[(imdb.random$Genre %in% input$input.g)   & 
                                        (imdb.random$Rating >= input$input.r[1]) & 
                                        (imdb.random$Rating <= input$input.r[2]) ,
                                         ]
                           })
  
  output$myPlot <- renderPlotly({
    plot_ly(data=selectedData(), x = ~ get(input$input.x), y= ~ get(input$input.y),
            color= ~ Genre, colors = "BrBG" ,
            text= ~ Title ,
            type = ifelse(input$input.x=="Genre","box","scatter")) %>%
      layout( xaxis = list(title = input$input.x) , yaxis = list(title = input$input.y))
  })

## Alternate plot, using basic ggplot    
#   output$myPlot <- renderPlot({
#        selectedData() %>% ggplot(aes(x =get(input$input.x), y= get(input$input.y), colour = Genre)) +
#                                labs(x=input$input.x,y=input$input.y )  +
#                                scale_colour_brewer(palette ="BrBG")  +
#                                theme_minimal()+
#                                if(input$input.x=="Genre"){geom_boxplot()} else {geom_point()}
#   })

  output$myTable <- renderDT({
    if(input$input.x != "Genre"){
        selectedData()  %>% group_by(Genre) %>% 
          summarize(X = round(mean(get(input$input.x),na.rm=T),2),
                    Y = round(mean(get(input$input.y),na.rm=T),2)
                    ) %>% datatable(colnames=c("Genre", input$input.x, input$input.y)) 
    } else {
      selectedData()  %>% group_by(Genre) %>% 
        summarize( Y = round(mean(get(input$input.y),na.rm=T),2)
                 ) %>% datatable(colnames=c("Genre", input$input.y))           
      
        }
  })
  
  observeEvent(input$random, {
    output$myText <- renderText({ movie= selectedData()
                                      movie= subset(movie,!is.na(Title) & !is.na(Year))
                                      
                                      if(nrow(movie)!=0) {
                                          movie.sel=movie[sample.int(nrow(movie),1),]
                                          paste0("You should watch:\n\n",movie.sel$Title, " (",movie.sel$Year,")")
                                      } else {"No match!"}
                                     })
    
    output$myHTML <- renderUI({   movie= selectedData()
                                      movie= subset(movie,!is.na(Title) & !is.na(Year))
                                      
                                      if(nrow(movie)!=0) {
                                          movie.sel=movie[sample.int(nrow(movie),1),]
                                          h4("You should watch:\n\n", 
                                             a(paste0(movie.sel$Title, " (",movie.sel$Year,")"), 
                                               href = paste0("https://www.imdb.com/title/",movie.sel$tconst) ,
                                               target="_blank" , rel="noopener noreferrer"
                                               )
                                             )
                                          } else {  h4("No match!") }
                                          
                                      })
  })


}

## Running the app
shinyApp(ui, server)




