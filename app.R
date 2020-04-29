library(shiny)
library(shinyWidgets)

title_info = read.csv("/Users/shashanksharma/github/Movie-TV-Show-recommendation-/Data preperation/title_information.csv")

cast_info = read.csv("/Users/shashanksharma/github/Movie-TV-Show-recommendation-/Data preperation/cast_infromation.csv")

ui <- fluidPage(
tags$style("body {background-color: #F0F0F0; }"),
headerPanel(h2(strong("Personalised Watch list"), align = "center", style = "color: brown; font-family: 'georgia';")), ##Main heading

tags$style(HTML(".tabbable > .nav > li > a                  {background-color: #C0C0C0;  color:black}   
                .tabbable > .nav > li[class=active]    > a {background-color: #A0A0A0; color:white}")),

sidebarLayout(
  
  sidebarPanel(
                  radioButtons("type", #to select 
                                     h5(strong("I want to watch:"),style = "color:brown;font-family: 'georgia';"),
                                     choices = c("Movie", "TV Show","Anything"),
                                     selected = c("Movie"),
                                     inline = TRUE),
    
                  radioButtons("I understand", #to select 
                                     h5(strong("Select language:"),style = "color:brown;font-family: 'georgia';" ),
                                     choices = c("English", "Hindi","Both"),
                                     selected = c("English"),
                                     inline = TRUE),
                  
                  
                  radioButtons("add_genre", #to select 
                                     h5(strong("Genre Selection"),style = "color:brown;font-family: 'georgia';" ),
                                     choices = c("Surprise me", "I want to select the genres"),
                                     selected = c("Surprise me")
                                     ),
                  
                  conditionalPanel(condition = "input.add_genre == 'I want to select the genres'",
                                   prettyCheckboxGroup("select_genre", #to select 
                                                h5(strong("Select the genres"),style = "color:brown;font-family: 'georgia';" ),
                                                c("Action", "Adventure", "Animation", "Biography", "Comedy",
                                                  "Crime", "Documentary", "Drama", "Family", "Fantasy", "History",
                                                  "Horror", "Music", "Musical", "Mystery", "Romance", "Sci-Fi",
                                                  "Short", "Sport", "Thriller", "War", "Western"),
                                                
                                                shape= 'curve',
                                                bigger = TRUE
                                     
                                               )
                                  
                                 ),
                  sliderInput("year", 
                              h5(strong("Select year range"),style = "color:brown;font-family: 'georgia';" ),
                              1900, 2020, 
                              value = c(1900, 2020),
                              sep = ""),
                  
                  sliderInput("rating", 
                              h5(strong("Select the IMDB rating range"),style = "color:brown;font-family: 'georgia';"),
                              0,10, 
                              value = c(0, 10),
                              sep = ""),
                  
                  selectizeInput('actors',
                    label = "Select actors", 
                    choices = NULL,
                    selected  = NULL,
                    multiple = TRUE),
                  
                  selectizeInput('director',
                                 label = "Select Director", 
                                 choices = NULL,
                                 selected  = NULL,
                                 multiple = TRUE)
              
                   ),
         mainPanel()
)

)

server <-  function(input,output,session){
  updateSelectizeInput(session, 
                       'actors',
                       label = "Select Actors",
                       choices = unique(cast_info[ which(cast_info$category == 'actor' | cast_info$category == 'actress' ) , ]$name), 
                       server = TRUE)
  
  updateSelectizeInput(session, 
                       'director',
                       label = "Select Director",
                       choices = unique(cast_info[ which(cast_info$category == 'director' ) , ]$name), 
                       server = TRUE)
  
}

shinyApp(ui = ui , server = server)

