library(shiny)
library(shinyWidgets)

######################
##LOADING THE FILES###
######################
title_info = read.csv("/Users/shashanksharma/github/Movie-TV-Show-recommendation-/Data preperation/title_information.csv")
cast_info = read.csv("/Users/shashanksharma/github/Movie-TV-Show-recommendation-/Data preperation/cast_information.csv")

title_info$actual_value = title_info$averageRating * title_info$numVotes

#################################
###USER INTERFACE OF THE APP#####
#################################
ui <- fluidPage(

tags$style("body {background-color: #A9A9A9; }"),

headerPanel(h2(strong("Create your own customised Watchlist "), 
               div(img(src="main_screen.jpg", height = 500, width = 1000,align="center")),
               align = "center", 
               style = "color:black; font-family: 'baskerville';",
               color = "ghostwhite")), ##Main heading


sidebarLayout(
  
  sidebarPanel(tags$style(".well {background-color: #DCDCDC; }"),
                  radioButtons("type", #to select 
                                     h4(strong("I want to watch:"),style = "color:crimson;font-family: 'baskerville';"),
                                     choices = c("Movie", "Series","Anything"),
                                     selected = c("Movie"),
                                     inline = TRUE),
    
                  radioButtons("lang", #to select 
                                     h4(strong("Select language:"),style = "color:crimson;font-family: 'baskerville';" ),
                                     choices = c("English", "Hindi","Both"),
                                     selected = c("English"),
                                     inline = TRUE),
                  
                  
                  radioButtons("add_genre", #to select 
                                     h4(strong("Genre Selection"),style = "color:crimson;font-family: 'baskerville';" ),
                                     choices = c("Surprise me", "I want to select the genres"),
                                     selected = c("Surprise me")
                                     ),
                  
                  conditionalPanel(condition = "input.add_genre == 'I want to select the genres'",
                                   prettyCheckboxGroup("select_genre", #to select 
                                                h4(strong("Select the genres"),style = "color:crimson;font-family: 'baskerville';" ),
                                                c("Action", "Adventure", "Animation", "Biography", "Comedy",
                                                  "Crime", "Documentary", "Drama", "Family", "Fantasy", "History",
                                                  "Horror", "Music", "Musical", "Mystery", "Romance", "Sci-Fi",
                                                  "Short", "Sport", "Thriller", "War", "Western"),
                                                
                                                shape= 'curve',
                                                bigger = TRUE
                                     
                                               )
                                  
                                 ),
                  sliderInput("year", 
                              h4(strong("Select year range"),style = "color:crimson;font-family: 'baskerville';" ),
                              1950, 2020, 
                              value = c(1950, 2020),
                              sep = ""),
                  
                  sliderInput("rating", 
                              h4(strong("Select the IMDB rating range"),style = "color:crimson;font-family: 'baskerville';"),
                              0,10, 
                              value = c(0, 10),
                              sep = ""),
                  
                  selectizeInput('actors',
                    h4(strong("Select actors"),style = "color:crimson;font-family: 'baskerville';"), 
                    choices = NULL,
                    selected  = NULL,
                    multiple = TRUE),
                  
                  selectizeInput('director',
                                 h4(strong("Select directors"),style = "color:crimson;font-family: 'baskerville';"), 
                                 choices = NULL,
                                 selected  = NULL,
                                 multiple = TRUE)
              
                   ),
         mainPanel(DT::dataTableOutput("table"),
                   imageOutput("image"))
)

)


##################
######SERVER#######
##################
server <-  function(input,output,session){

image = 0
  
#Updating the actors selectize inputs with values
updateSelectizeInput(session, 
                       'actors',
                       choices = unique(cast_info[ which(cast_info$category == 'actor' | cast_info$category == 'actress' ) , ]$name), 
                       server = TRUE)
#Updating the director selectize inputs with values
updateSelectizeInput(session, 
                       'director',
                        choices = unique(cast_info[ which(cast_info$category == 'director' ) , ]$name), 
                       server = TRUE)
  
# Filter data based on selections
output$table <- DT::renderDataTable(DT::datatable({
    data <- title_info
    
# Handling type radiobutton for movie/tv show
if (input$type == 'Movie'){
      data = data[which(data$titleType == 'movie'),]
}
    
else if (input$type == 'Series'){
  data = data[-which(data$titleType == 'movie'),]
}
    
#Handling langauge filters
if (input$lang == 'English'){
    data = data[which(data$language == 'English'),]
}


else if((input$lang == 'Hindi')){
  data = data[which(data$language == 'Hindi'),]
}

#Handling Genres
if(input$add_genre == "I want to select the genres"){
 data =  data[grep(paste(input$select_genre, collapse="|"), data$genres),]
}
    
#Handlin year range
data = data[which(data$startYear >= input$year[1] & data$startYear <= input$year[2]),]    

#Handling ratings range
data = data[which(data$averageRating >= input$rating[1] & data$averageRating <= input$rating[2]),]  

#handling actors names
actor_list = unique(cast_info[grep(paste(input$actors, collapse="|"), cast_info$name),]$tconst)
data = data[which(data$titleId %in% actor_list ),]

#handlin director names
director_list = unique(cast_info[grep(paste(input$director, collapse="|"), cast_info$name),]$tconst)
data = data[which(data$titleId %in% director_list ),]


##Managing the output  


data = data[which(data$numVotes >= mean(data$numVotes)),]



if ((input$type == 'Anything') && (input$lang == 'Both')){
  data[with(data,order(-actual_value)), c('title','titleType','language','genres','averageRating','startYear')]
  
}

else if(input$type == 'Anything'){
  data[with(data,order(-actual_value)), c('title','titleType','genres','averageRating','startYear')]
}

else if(input$lang == 'Both'){
  data[with(data,order(-actual_value)), c('title','language','genres','averageRating','startYear')]
}

else{
  data[with(data,order(-actual_value)), c('title','genres','averageRating','startYear')]
}
  }, options = 
  list(searching = FALSE,paging = TRUE,
       language = list(
         zeroRecords = '<img src="error.jpg" width=450 height=450></img>'))              
  ))

  
}

shinyApp(ui = ui , server = server)

