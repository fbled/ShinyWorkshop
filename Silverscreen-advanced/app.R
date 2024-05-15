library(shiny)
library(plotly)
library(gridlayout)
library(bslib)
library(DT)
library(dplyr)

library(bslib)
library(thematic)


load("C:\\Users\\Florent.Bled\\Work\\FWC\\Workshops\\ShinyWorkshop\\Silverscreen\\imdb.random.RData")

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



## Creating our own theme
movie.theme <- bs_theme(
  # Controls the default background and foreground palette
  bg = "#404040", fg = "#DDD3B9",
  
  # Controls the accent (e.g., hyperlink, button, etc) colors
  primary = "#B7473C", secondary = "#940490",
  
  # Controls the fonts
  base_font = c("Myriad Pro", "sans-serif"),
  code_font = c("Courier", "monospace"),
  heading_font = font_collection(font_google("Abril Fatface"), "Roboto", "sans-serif")
  )

# Can also add lower-level customization
movie.theme.2 <- bs_add_variables(
  movie.theme,
   "input-border-color" = "#E3EFEF" ,
   "h4-font-family" = "Myriad Pro",
   "info" = "#FFFFFF"
)


# thematic_shiny() to be able to also control the way the plot looks

thematic_shiny(
  bg = "auto",
  fg = "auto",
  accent = "auto",
  font = "auto"
)

ui <- grid_page(
  theme = movie.theme.2 ,  # Running with a theme
  layout = c(      # alternate specification of the layout
    "| 1rem  |  250px   |  1.41fr |
     | 136px  |  header  |  header |
     | 7fr   | sidebar  |  plot   |
     | 5fr   | recommend|  table  |"
     ),
  
  ## Easy header card with icon
  # grid_card_text(
  #   area = "header",
  #   icon="r movie logo 150x150 - nobg.png",   # Note: Might not appear in run_with_themer until before the app is initially called via a regular shinyApp call -strange-
  #   img_height = "100px",
  #   content = "Silverscreen: Letting the Stars Shine",
  #   alignment = "start",
  #   is_title = TRUE
  # ),
  
  ## Alternate header card with icon, and dark mode input on the right
  grid_card(
    area = "header",
    card(
      div(style="display:flex ; align-items: center",
          img(src="r movie logo 150x150 - nobg.png",height = "100px"), h3("Silverscreen: Letting the Stars Shine"),
          input_dark_mode(style = "margin-left: auto;")),
      wrapper=card_title())
    
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

  grid_card(
    area = "recommend",
    full_screen = TRUE,
    card_header("Randomization"),
    card_body(
      img(src="IMDB_Logo_2016.png" ,width="75%" , height="auto" , style="display: block; margin-left: auto; margin-right: auto;" ),
      actionButton(inputId = "random", label = "Recommend!", class="btn-info", ),
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

      ggplotly(    # Right now, plots can be themed via thematic, and rendered interactive by calling ggplotly (instead of directly creating a plot_ly)
        
         selectedData() %>% 
           ggplot(aes(x =.data[[input$input.x]], y=.data[[input$input.y]], colour = Genre,text= Title)) +   # tidy evaluation pronoun .data[[...]] allows us to specify aesthetics as strings, which can then be used by ggplotly !, replacement of aes_string (softly deprecated)
                               labs(x=input$input.x,y=input$input.y )  +
                               scale_colour_brewer(palette ="BrBG")  +
                              # theme_minimal()+
                               if(input$input.x=="Genre"){geom_boxplot()} else {geom_point()} ,
         tooltip=c("text","colour","x","y")    # Re-ordering currently not working, see: https://stackoverflow.com/questions/56351511/why-is-ggplotlys-argument-tooltip-not-ordering
         ) 
  })
  
  
  
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
                                          div(strong("You should watch:"),
                                            div(    
                                               a(paste0(movie.sel$Title, " (",movie.sel$Year,")"), 
                                                 href = paste0("https://www.imdb.com/title/",movie.sel$tconst) ,
                                                 target="_blank" , rel="noopener noreferrer"
                                                 )  ,
                                               align="center"
                                              )
                                          )
                                      } else {  h4("No match!") }
                                          
                                      })
  })


}

## Running the app, as it
 shinyApp(ui, server)   

## Running the app, with style editor
# run_with_themer(shinyApp(ui, server)) 



