library(shiny)
library(shinythemes)
library(dplyr)


songs <- read.csv("spotify_songs.csv", stringsAsFactors = FALSE, header = TRUE)

ui <- fluidPage(theme = shinytheme('cyborg'),
                navbarPage(
                  "Spotify User Analysis",
                  
                  tabsetPanel(
                  
                    tabPanel("Intro",
                             sidebarPanel(
                               tags$img(src = "https://storage.googleapis.com/pr-newsroom-wp/1/2018/11/Spotify_Logo_RGB_White.png",
                                        height = 140, width = 400),
                               
                               br(),br(),
                               h6(strong("Team name: ")),
                               h6("Five Directions"),
                               
                               br(),
                               h6(strong("Title of the project:")),
                               h6("Analysis of Spotify songs"),
                               
                               br(),
                               h6(strong("Problem Statement:")),
                               h6("Limited song recommendation."),
                               
                               br(),
                               h6(strong("Goal:")),
                               h6("1.Analyzing and predicting or recommending the song that is suitable for the listener based on the genre."),
                               h6("2.Analyzing and predicting or recommending the song that is suitable for the listener based on the popularity.")

                               
                              ),# sidebarPanel
                             
                             mainPanel(
                               h1("Welcome to the Spotify User Analysis Tool!"),
                               h6("Here you can see different analysis of your music"),
                               h6("Click one of the tabs above and learn more about your music"),
                               tags$a(href="https://github.com/adarisa/Data-Science-group-project", h6("For full UI code, Click Here!")),
                               br(),br(),
                               h5("Meet the team!"),
                               br(),
                               
                               fluidRow(
                                 column(2, 
                                        tags$img(src = "https://media.licdn.com/dms/image/D5603AQHykesj26dL5w/profile-displayphoto-shrink_800_800/0/1674104503154?e=2147483647&v=beta&t=9iJUFXbF0GPVp1s-s8FCHvKI6cYCxBtsnmJg7pYYmxM",
                                                 height = 140, width = 140),
                                        h6(strong("Syukran"))
                                        ),
                                 column(2, 
                                        tags$img(src = "https://media.licdn.com/dms/image/D5603AQHijr5zzePE_w/profile-displayphoto-shrink_400_400/0/1677733135891?e=1684972800&v=beta&t=fcJa0rzqxF8plZZR1NO47sp6CPy9bNLJ7Bc5CH09xic",
                                                 height = 140, width = 140),
                                        h6(strong("Adarisa"))
                                        ),
                                 column(2, 
                                        tags$img(src = "https://media.licdn.com/dms/image/D5603AQHSszuisfaL_g/profile-displayphoto-shrink_800_800/0/1674282873421?e=2147483647&v=beta&t=F8y8hUr7hx24XUdoHQ3GyyJwFwUJeR7ha3PhbKCntHw",
                                                 height = 140, width = 140),
                                        h6(strong("Najiha"))
                                        ),
                                 column(2, 
                                       tags$img(src = "https://media.licdn.com/dms/image/C5603AQFMSkE0xts94Q/profile-displayphoto-shrink_800_800/0/1662047632478?e=2147483647&v=beta&t=d4puaw5iRp6i9Gwp3TtTBb7joHWvBnz71etY6euDCJM",
                                                height = 140, width = 140),
                                       h6(strong("Syazwani"))
                                       ),
                                 column(2, 
                                       tags$img(src = "https://media.licdn.com/dms/image/C5603AQGObfzQNK7jyA/profile-displayphoto-shrink_800_800/0/1624544981722?e=2147483647&v=beta&t=sH62xeG9CDAXWUKmDplh8pYfEViysUWsIugJAiKkWsA",
                                               height = 140, width = 140),
                                       h6(strong("Adam"))
                                       )
                        
                                )
                                 
                               
                             ) # mainPanel
                             
                    ),# tabPanel1
                    
                    tabPanel("Popularity",
                             tabsetPanel(
                                tabPanel("Based on Genre",
                                         sidebarPanel(
                                           
                                           #genre selection
                                           
                                           selectInput(inputId = "Columns", label = "Which genres do you like?",
                                                       unique(songs$playlist_genre), multiple = FALSE),
                                         
                                           verbatimTextOutput("rock"),
                                         
                                           sliderInput(inputId = "range", label = "Range of ratings that you wish to read?",
                                                       min = min(songs$track_popularity), max = 100, value = c(55,100))
                                         
                                         ), #sidebarPanel
                                         
                                         mainPanel(
                                           h2("Top songs of the genre"),
                                           DT::dataTableOutput(outputId = "songsreco")
                                         )
                                        
                                
                                ), #tabpanel1
                             
                                tabPanel("Based on Artist",
                                         sidebarPanel(
                                           selectInput(inputId = "singers", label = "Which singer do you like?",
                                                                  unique(songs$track_artist), multiple = FALSE),
                                           verbatimTextOutput("Ed Sheeran"),
                                           
                                           sliderInput(inputId = "range_2", label = "Range of ratings that you wish to read?",
                                                       min = min(songs$track_popularity), max = 100, value = c(55,100))),
                                         
                                         mainPanel(
                                           h2("Top songs of the artist"),
                                           DT::dataTableOutput(outputId = "songsreco_artist")
                                           
                                         ) #mainpanel
                                  ) #tabpanel
                                
                              ) #tabsetPanel2
                             
                    ),# tabPanel2
                    
                    tabPanel("Analysis",
                             tabsetPanel(
                               tabPanel("Genre Analysis",
                                        fixedRow(
                                          column(4, offset = 3,
                                                 br(),
                                                 h6("Which genre has the most released tracks in each year?"),
                                                 br(),
                                                 tags$img(src = "https://github.com/adarisa/Data-Science-group-project/blob/main/Which%20genre%20has%20the%20most%20released%20tracks%20in%20each%20year.jpg?raw=true",
                                                          height = 400, width = 400)),
                                          
                                          column(4, 
                                                 br(),
                                                 h6("When were the tracks released?"),
                                                 br(),
                                                 tags$img(src = "https://github.com/adarisa/Data-Science-group-project/blob/main/Release%20of%20songs%20across%20the%20years.jpg?raw=true",
                                                          height = 400, width = 400),
                                        
                                                
                                                 
                                                 )),
                                        fixedRow(
                                          column(4, offset = 5,
                                                 br(),
                                                 h6("How many tracks are released based on genre?"),
                                                 br(),
                                                 tags$img(src = "https://github.com/adarisa/Data-Science-group-project/blob/main/How%20many%20tracks%20are%20released%20based%20on%20genre.png.jpg?raw=true",
                                                          height = 400, width = 400)),
                                        ),
                                        br(),br(),
                                        ),
                                        
                               tabPanel("Artist Analysis", 
                                        fixedRow(
                                          column(4, offset = 5,
                                                 br(),
                                                 h6("Which artist has the most releases? "),
                                                 br(),
                                                 tags$img(src = "https://github.com/adarisa/Data-Science-group-project/blob/main/Which%20artist%20has%20the%20most%20releases.jpg?raw=true",
                                                          height = 400, width = 500))),
                                        fixedRow(
                                          column(4, offset = 5,
                                                 br(),
                                                 h6("How many artists that released tracks? "),
                                                 br(),
                                                 tags$img(src = "https://github.com/adarisa/Data-Science-group-project/blob/main/How%20many%20artists%20that%20released%20tracks%20based%20on%20genre.jpg?raw=true",
                                                          height = 400, width = 500))),
                                        br(),
                                        ),
                               
                               tabPanel("Popularity Analysis",
                                        fixedRow(
                                          column(4, offset = 4,
                                                        br(),
                                                        h6("What are the most popular words featuring in titles?"),
                                                        br(),
                                                        tags$img(src = "https://github.com/adarisa/Data-Science-group-project/blob/main/What%20are%20the%20popular%20words%20featuring%20in%20titles.jpg?raw=true",
                                                                 height = 400, width = 500))),
                                        
                                        fixedRow(
                                          column(4, offset = 3,
                                                 br(),
                                                 h6("Who are the top ten artists and what are their most famous tracks?"),
                                                 br(),
                                                 tags$img(src = "https://github.com/adarisa/Data-Science-group-project/blob/main/Who%20are%20the%20top%20ten%20artists%20and%20what%20are%20their%20most%20famous%20tracks.png.jpg?raw=true",
                                                          height = 500, width = 700)))
                               
                                        )
                                        
                               
                               ),
                                         

      
                             
                             
                    ), # tabPanel3
                    
                  ) #tabsetPanel1
                  
                )#navbar
                
        )# fluidPage

server <- function(input, output){
  datasetInput <- reactive({
    
    # Filtering the books based on genre and rating
    songs %>% filter(playlist_genre %in% as.vector(input$Columns)) %>%
      group_by(track_name) %>% filter(track_popularity >= as.numeric(input$range[1]), track_popularity <= as.numeric(input$range[2])) %>%
      arrange(desc(track_popularity)) %>%
      select(track_name, track_artist, track_popularity, playlist_genre) %>%
      rename(`song` = track_name, `Genre(s)` = playlist_genre)
    
    
  })
  
  datasetInput2 <- reactive({
    
    # Filtering the books based on genre and rating
    songs %>% filter(track_artist %in% as.vector(input$singers)) %>%
      group_by(track_name) %>% filter(track_popularity >= as.numeric(input$range_2[1]), track_popularity <= as.numeric(input$range_2[2])) %>%
      arrange(desc(track_popularity)) %>%
      select(track_name, track_artist, track_popularity, playlist_genre) %>%
      rename(`song` = track_name, `Genre(s)` = playlist_genre)
    
    
  })
  
  
  #Rendering the table
  output$songsreco <- DT::renderDataTable({
    
    DT::datatable(head(datasetInput(), n = 50), escape = FALSE, options = list(scrollX = '1000px'))
  })
  
  output$songsreco_artist <- DT::renderDataTable({
    
    DT::datatable(head(datasetInput2(), n = 100), escape = FALSE, options = list(scrollX = '1000px'))
  })
}


shinyApp(ui = ui, server = server)
