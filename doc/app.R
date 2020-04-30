#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

library(tm)
library(purrr)
library(tibble)
library(textdata)
library(data.table)
library(tidytext)
library(tidyverse)
library(DT)
library(vcd)
library(rstan)
library(dplyr)
library(stringr)
library(wordcloud2)
library(gridExtra)
library(ggplot2)
library(ngram)
library(tidyr)
library(PerformanceAnalytics)
library(wordcloud2)
library(RColorBrewer)
library(wordcloud)
library(reshape2)
library(scales)
library(plotly)
library(shiny) 
library(scales)
library(lattice)
library(htmltools)
library(maps)
library(plotly)
library(rsconnect)
library(devtools)
library(curl)
library(shinydashboard)
# load lyrics data

library(lobstr)
mem_used()
### build dashboard

header <- dashboardHeader(
    dropdownMenu(
        type = "notifications",
        notificationItem(
            text = "5 people are looking at the same website",
            icon("users")
        )
    )                                                     
)

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("wordcloud", tabName = "wordcloud", icon = icon("search-location")),
        menuItem("genre descriptive", tabName = "descriptive", icon = icon("search-location")),
        menuItem("overview", tabName = "overview", icon = icon("chart-line")),
        menuItem("References", tabName = "references", icon = icon("th"))
    )
)




body <- dashboardBody(
    ## set the color of header
    tags$head(tags$style(HTML('/* logo */
                                .skin-blue .main-header .logo {
                                background-color: #74d2e7;
                                }
                            /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: #74d2e7;
                                }
                            /* navbar (rest of the header) */
                            .skin-blue .main-header .navbar {
                            background-color: #74d2e7;
                            }
                            /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #74d2e7;
                            }
                            /* main sidebar */
                              .skin-blue .main-sidebar {
                              background-color: #f8f9f9;
                            }
                            /* body */
                                .content-wrapper, .right-side {
                                background-color: #f8f9f9;
                            }
                            /*    Move everything below the header */
                            .content-wrapper {
                            margin-top: 100px;
                            }
                            .content {
                            padding-top: 50px;
                            }
                            /*    Format the title/subtitle text */
                            .title-box {
                            position: absolute;
                            text-align: center;
                            top: 50%;
                            left: 50%;
                            transform:translate(-50%, -50%);
                            }
                            @media (max-width: 590px) {
                            .title-box {
                            position: absolute;
                            text-align: center;
                            top: 10%;
                            left: 10%;
                            transform:translate(-5%, -5%);
                            }
                            }
                            @media (max-width: 767px) {
                            .primary-title {
                            font-size: 1.1em;
                            }
                            .primary-subtitle {
                            font-size: 1em;
                            }
                            }
                            /*    Make the image taller */
                            .main-header .logo {
                             height: 190px;
                              }
                            /*    Override the default media-specific settings */
                            @media (max-width: 5000px) {
                            .main-header {
                            padding: 0 0;
                            position: relative;
                            }
                            .main-header .logo,
                            .main-header .navbar {
                            width: 100%;
                            float: none;
                            }
                           .main-header .navbar {
                           margin: 0;
                           }
                           .main-header .navbar-custom-menu {
                          float: right;
                          }
                          }
                          /*    Move the sidebar down */
                          .main-sidebar {
                          position: absolute;
                          }
                          .left-side, .main-sidebar {
                          padding-top: 250px;
                          }'
    ))),
    
    tabItems(
        tabItem(
            tabName = "home",
            fluidPage(
                tags$style(
                    HTML('
          .box.box-solid.box-primary>.box-header {
            color:#fff;
              background:#74d2e7
          }
          
          .box.box-solid.box-primary{
            border-bottom-color:#74d2e7;
              border-left-color:#74d2e7;
              border-right-color:#74d2e7;
              border-top-color:#74d2e7;
          }'
                    )
                ),
                fluidRow(
                    box(width = 15, title = "Introduction", status = "primary",
                        solidHeader = TRUE, 
                        h3("Help you have an understanding about the lyrics and genre"),
                        h4("Our shiny app is based on lyrics on different genres. "),
                        h4("This application aims to help users to discover and compare different genres and their sentiments in a more efficient manner,
                           and is created by Olivia Wang in February 2020, who is a Columbia student taking a tiny step here by designing this application."),
                        h4("Let's explore this app!"))),
                fluidRow(
                    box(width = 15, title = "User Guide", status = "primary",
                        solidHeader = TRUE,
                        h3("How to use this app?"),
                        tags$div(tags$ul(
                            tags$li("wordcloud: The tab has word cloud plot to give users a overview of words in different genres and ages"),
                            tags$li("genre descriptive: The tab has some plots to help users get a quick information of a special genre"),
                            tags$li("Overview: The tab has several graphs to give users a board overview of different sentiments among different genres or different ages")
                        ))
                    )
                ),
                fluidRow(
                    tags$img(
                        src = "music4.jpg",
                        width = "100%"
                    )
                )
            )
        ),
        tabItem(
            tabName = "wordcloud",
            fluidPage(
                fluidRow(
                    column(2,
                           
                           sliderInput(inputId = "nwords1",
                                       label = "Number of terms in the first word cloud:",
                                       min = 5, max = 100, value = 50),
                           selectInput('genre',label='genre',
                                       choices =  c("Folk", "R&B", "Electronic", "Jazz", "Indie", 
                                                    "Country", "Rock", "Metal", "Pop", "Hip-Hop"), 
                                       selected='Country'),
                           submitButton("search",width='100%')),
                    column(10, wordcloud2Output(outputId = "WC1", height = "200"),div())
                    
                    
                )
            )
        ),
        
    
        tabItem(
            tabName = "descriptive",
            fluidPage(
                fluidRow(
                    column(2,
                           selectInput("genre", label='genre',
                                       choices =  c("Folk", "R&B", "Electronic", "Jazz", "Indie", "Country", 
                                                    "Rock", "Metal", "Pop", "Hip-Hop"), 
                                      selected='Country'),
                           submitButton("search",width='100%'))),
                fluidRow(
                    column(10,
                           plotlyOutput("plot1"))),
                fluidRow(
                        column(10,
                           plotlyOutput("plot3"))),
                fluidRow(
                        column(10,
                           plotlyOutput("plot4")))
                
                )
            ),
        
        tabItem(
            tabName = "overview",
            tabsetPanel(type = "tabs",
                        tabPanel("total sentiment of genre",
                                 
                                 fluidRow(
                                     column(10, plotlyOutput('g2'), div()))),
            
                        
                                 
                        tabPanel("total sentiment along with ages",
                                 fluidRow(
                                     column(10, plotlyOutput('g3'), div()))))),
                        
        
        tabItem(
            tabName = "references",
            fluidPage(
                fluidRow(
                    box(width = 15, title = "Data Source", status = "primary",
                        solidHeader = TRUE,
                        "The data source of this shiny app is from",
                        tags$a(href = "https://github.com/olivia3395/Spring2020-Project1-olivia3395/tree/master/data", "Lyrics data"), 
                        ".")
                ),
                fluidRow(
                    box(width = 15, title = "Project Code", status = "primary",
                        solidHeader = TRUE, 
                        "The code of this project can be found at",
                        actionButton(inputId='code', label="GitHub", 
                                     icon = icon("github"), 
                                     onclick ="window.open('https://github.com/olivia3395')"),
                        ".")
                ),
                fluidRow(
                    box(width = 15, title = "Contact Us", status = "primary",
                        solidHeader = TRUE, 
                        h4("Feel free to contact me if you're interested in this app!"),
                        h5("Wang, Yuyao: yw3395@columbia.edu")
                    )
                ),
                fluidRow(
                    tags$img(
                        src = "music5.jpg",
                        width = "100%"
                    )
                )
            )
        )
    )
)




# Define UI for app that draws a histogram ----
ui <- dashboardPage(header, sidebar, body)



# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    meansentiment_genre=read.csv("meansentiment_genre.csv")
    sequence_genre_group=read.csv("sequence_genre_group.csv")
  
    sort_ages_genre=read.csv("sort_ages_genre.csv")
    
    sort_ages=read.csv("sort_ages.csv")
    s0= read.csv("s0.csv")
    word_df=read.csv("word_df.csv")
        
        
        
    total_sentiment_genre=meansentiment_genre%>%
        mutate(total=mean_genre_pos-mean_genre_neg)%>%
        select(genre,total)
    
    
    output$WC1 <- renderWordcloud2({
        count(filter(word_df, id %in% which(dt_lyrics$genre == input$genre)), 
              word, sort = TRUE) %>%
            slice(1:input$nwords1) %>%
            wordcloud2(size=0.6, rotateRatio=0.2)
    })
  
    # senti genre plot
    
    output$g2 <- renderPlotly({
        total_sentiment_genre=meansentiment_genre%>%
    mutate(total=mean_genre_pos-mean_genre_neg)%>%
        select(genre,total)
    
    senti_genre_plot=total_sentiment_genre%>%
        ggplot(aes(genre, total, fill = genre)) +
        geom_col(show.legend = FALSE) +
        labs(x = "genre",y="total sentiment(positive/negative)")
    
    ggplotly(senti_genre_plot) %>%
        layout(legend = list(bgcolor = "transparent",
                             bordercolor = "transparent")) %>%
        layout(plot_bgcolor='transparent') %>%
        layout(paper_bgcolor='transparent')
    })
    
    # overall sentiment age
    
    output$g3 <- renderPlotly({
        senti_age_plot=sort_ages%>%
        ggplot(aes(ages,mean,fill=ages)) +
        geom_col(show.legend = FALSE)+
        labs(x = NULL, 
             y = "Mean # of sentiment value(positive/negative)")
    
    ggplotly(senti_age_plot) %>%
        layout(legend = list(bgcolor = "transparent",
                             bordercolor = "transparent")) %>%
        layout(plot_bgcolor='transparent') %>%
        layout(paper_bgcolor='transparent')
    })
    
    # word cloud plot 1
    
  
    
    
    
    # sequence genre plot
    
    output$plot1 <- renderPlotly({
        t1=sequence_genre_group%>%
            filter(genre==input$genre)
        
        p1=ggplot(data=t1,aes(number,mean)) +
        geom_line(aes(color=genre)) +
        
        labs(x = NULL, 
             y = "Mean # of value on the track of time ")+
        theme_light()
        
        ggplotly(p1) %>%
            layout(legend = list(bgcolor = "transparent",
                                 bordercolor = "transparent")) %>%
            layout(plot_bgcolor='transparent') %>%
            layout(paper_bgcolor='transparent')
    })

    

    
    # words genre plot
    
    output$plot3 <- renderPlotly({
        s1=s0%>%
            group_by(genre,word)%>%
            filter(n==max(n))%>%
            mutate(number=ifelse(n==n,mean(number),number))%>%
            distinct()
        s1
        
        t3=s1%>%
            filter(genre==input$genre)
        
       p3= ggplot(data=t3,aes(word,number,fill=senti)) +
        geom_col(show.legend = FALSE) +
        coord_flip()
        
        ggplotly(p3) %>%
            layout(legend = list(bgcolor = "transparent",
                                 bordercolor = "transparent")) %>%
            layout(plot_bgcolor='transparent') %>%
            layout(paper_bgcolor='transparent')
        
        
    })
    
    # ages variation genre plot
    
    output$plot4 <- renderPlotly({
        t4=sort_ages_genre%>%
            filter(genre==input$genre)
        
        p4=ggplot(data=t4,aes(ages,mean,fill = ages)) +
            geom_col(show.legend = FALSE) +
            labs(x = NULL, 
             y = "Mean # of sentiment value(positive/negative)")
        
        ggplotly(p4) %>%
            layout(legend = list(bgcolor = "transparent",
                                 bordercolor = "transparent")) %>%
            layout(plot_bgcolor='transparent') %>%
            layout(paper_bgcolor='transparent')
    })
    
    
    
}
# Run the application 
shinyApp(ui = ui, server = server)
