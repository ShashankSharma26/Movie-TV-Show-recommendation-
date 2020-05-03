library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(httr)
library(jsonlite)
library(jpeg)
library(purrr)

######################
##LOADING THE FILES###
######################
title_info = read.csv("Data preperation/title_information.csv")
cast_info = read.csv("Data preperation/cast_information.csv")

colnames(title_info) = c("X" ,"titleId",'Title', "region" ,'Language',"Type", "isAdult",'Year','Genre','Rating',"numVotes")



title_info$multiplier = as.numeric(as.character(cut(title_info$numVotes, breaks=c(0,100, 200,300,500,750,1000,2500,5000,10000,25000,50000, 2224462), 
                                                    labels=c( 0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90, .95, 0.97, 1))))

title_info$truerating = with(title_info,multiplier * Rating)

#################################
###USER INTERFACE OF THE APP#####
#################################
ui <- fluidPage(
 

#<img src="https://image.tmdb.org/t/p/w342/bOGkgRGdhrBYJSLpXaxhXVstddV.jpg" width=450 height=450></img>,
#tags$img(src = "https://image.tmdb.org/t/p/w342/bOGkgRGdhrBYJSLpXaxhXVstddV.jpg"),

titlePanel(h2(strong("Create Your Own Customised Watchlist"), 
               div(img(src="main.jpg", height = "500px", width = "900px" ,align="center")),
              
               align = "center", 
               style = "color:crimson; font-family: 'baskerville';",
              setBackgroundColor("#f0f0f5"),
               color = "ghostwhite"),
            windowTitle = "Movie recommender by Shashank"), ##Main heading


sidebarLayout(
  
  sidebarPanel(tags$style(".well {background-color: #DCDCDC; }"),
                  radioButtons("type", #to select 
                                     h4(strong("Select type:"),style = "color:crimson;font-family: 'baskerville';"),
                                     choices = c("Movie", "Series","Anything"),
                                     selected = c("Movie"),
                                     inline = TRUE),
    
                  radioButtons("lang", #to select 
                                     h4(strong("Select language:"),style = "color:crimson;font-family: 'baskerville';" ),
                                     choices = c("English", "Hindi","Both"),
                                     selected = c("English"),
                                     inline = TRUE),
                  
                  
                  radioButtons("add_genre", #to select 
                                     h4(strong("Select Genre"),style = "color:crimson;font-family: 'baskerville';" ),
                                     choices = c("Any genre", "I want to select the genres"),
                                     selected = c("Any genre")
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
                              h4(strong("Select IMDB rating range"),style = "color:crimson;font-family: 'baskerville';"),
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
         mainPanel(h2("Search Results", align = "center", style = "color:black;font-family: 'baskerville';"),
                  
                   DT::dataTableOutput("table"),
                   imageOutput("image"))
)

)


##################
######SERVER#######
##################
server <-  function(input,output,session){

#image = 0
  
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
      data = data[which(data$Type == 'movie'),]
}
    
else if (input$type == 'Series'){
  data = data[-which(data$Type == 'movie'),]
}
    
#Handling langauge filters
if (input$lang == 'English'){
    data = data[which(data$Language == 'English'),]
}


else if((input$lang == 'Hindi')){
  data = data[which(data$Language == 'Hindi'),]
}

#Handling Genres
if(input$add_genre == "I want to select the genres"){
 data =  data[grep(paste(input$select_genre, collapse="|"), data$Genre),]
}
    
#Handlin year range
data = data[which(data$Year >= input$year[1] & data$Year <= input$year[2]),]    

#Handling ratings range
data = data[which(data$Rating >= input$rating[1] & data$Rating <= input$rating[2]),]  

#handling actors names
actor_list = unique(cast_info[grep(paste(input$actors, collapse="|"), cast_info$name),]$tconst)
data = data[which(data$titleId %in% actor_list ),]

#handlin director names
director_list = unique(cast_info[grep(paste(input$director, collapse="|"), cast_info$name),]$tconst)
data = data[which(data$titleId %in% director_list ),]


# ##Managing the output  
# selectedrowindex <<-     input$table_rows_current[length(input$table_rows_current)]
# selectedrowindex <<- as.numeric(selectedrowindex)
# 
# 
# data[selectedrowindex,]$Posters = map(data[selectedrowindex,]$titleId,poster)


if ((input$type == 'Anything') && (input$lang == 'Both')){
  
  
  data[with(data,order(-truerating,-Rating )), c('Title','Type','Language','Genre','Rating','Year')] 
  
}

else if(input$type == 'Anything'){

  data[with(data,order(-truerating,-Rating )),  c('Title','Type','Genre','Rating','Year')] 

}

else if(input$lang == 'Both'){

  data[with(data,order(-truerating,-Rating)),  c('Title','Language','Genre','Rating','Year')]
}

else{

  data[with(data,order(-truerating,-Rating)),  c('Title','Genre','Rating','Year')]
}
  }, rownames = FALSE, 
  options = 
  list(searching = FALSE,paging = TRUE, pageLength = 15,
       language = list(
         zeroRecords = '<img src="error.jpg" width=450 height=450></img>'))              
  ))

  
}

shinyApp(ui = ui , server = server)

