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
  
  # output$myPlot <- renderPlot({
  #      selectedData() %>% ggplot(aes(x =get(input$input.x), y= get(input$input.y), colour = Genre)) + 
  #                              labs(x=input$input.x,y=input$input.y )  +
  #                              scale_colour_brewer(palette ="BrBG")  +
  #                              theme_minimal()+
  #                              if(input$input.x=="Genre"){geom_boxplot()} else {geom_point()}  
  # })
  
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

shinyApp(ui, server)
