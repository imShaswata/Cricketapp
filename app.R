##Calling libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(data.table)
library(leaflet)
library(DT)

##----UI codes------
ui <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidPage(theme = shinytheme("cerulean"),
              title = "Cricinfo",

              h3(strong("Summary of Indian cricketer"),style="text-align:center;
                       background-color:DodgerBlue;
                       color:white;
                       margin-top:0px;
                       padding:10px;
                       font-size:150%"),
              tags$style(HTML('table.dataTable tr:nth-child(even) {background-color: lightgrey !important;}')),
              tags$style(HTML('table.dataTable tr:nth-child(odd) {background-color: white !important;}')),
              tags$style(HTML('table.dataTable th {background-color: white !important;}')),
              fluidRow(column(4,selectInput(inputId = "format",
                                           label = "Cricket format",
                                           choices = c(Choose="","All","Test","ODI","	T20I"),
                                           selected = NULL
                                           )),
                       column(4,selectInput("st", "Place of birth", choices =c(Choose=""))),
                       column(4,sliderInput(inputId = "dec",
                                            label = "Choose a year range",
                                            min=1930,
                                            max = 2020,
                                            value = c(1960,2003),sep = ''))
                       
                       ),
              fluidRow(tabBox(width = "auto",
                              id = "tabset1", height = "250px",
                tabPanel("Map", 
                         leafletOutput("map",width = "auto",height = 600)
                         
                         ),
                tabPanel("Statistics", 
                         hr(),
                         box(width = "auto",title = "Batting Record", solidHeader = TRUE,
                             collapsible = TRUE,collapsed = T,
                             DT::dataTableOutput("table1")
                             ),
                         box(width = "auto",title = "Bowling Record", solidHeader = TRUE,
                             collapsible = TRUE,collapsed = T,
                             DT::dataTableOutput("table2")
                         )
                         ),
                tabPanel("Authors",
                         br(),
                         a(href="https://www.linkedin.com/in/shaswata-mukherjee-stat95","Shaswata Mukherjee"),"Statistician,Radix Ananlytics Pvt Ltd. Ahmedabad India",
                         br(),
                         a(href="https://www.linkedin.com/in/poulami-sarkar-b689b1144/","Poulami Sarkar"),"  Statistician,Radix Ananlytics Pvt Ltd. Ahmedabad India",
                         br(),
                         a(href="https://www.linkedin.com/in/gourab-saha-b0b865148","Gourab Saha"),"   Statistician,Radix Ananlytics Pvt Ltd. Ahmedabad India")
              ))
              
              
    )
    
  )
  )

server <- function(input,output,session){
  ## sourcing the function file
  source("src/functions.R")
  
  ## reading the data for three different format stored from web scrapping
  biodata_test_new <- data.table(readRDS("data/biodata_test_new.rds"))
  biodata_odi_new  <- data.table(readRDS("data/biodata_odi_new.rds"))
  biodata_t20_new  <- data.table(readRDS("data/biodata_t20_new.rds"))
  
  ## reading the data of statistic of cricketers 
  ## for three different format stored from web scrapping
  
  stat_table_odi   <- readRDS("data/stat_table_odi.rds")
  stat_table_test  <- readRDS("data/stat_table_test.rds")
  stat_table_t20   <- readRDS("data/stat_table_t20.rds")
  
  names(stat_table_odi) <- biodata_odi_new$Full_Name
  names(stat_table_test)<- biodata_test_new$Full_Name
  names(stat_table_t20) <- biodata_t20_new$Full_Name
  
  results_odi <-records(ls=stat_table_odi)
  results_test<-records(stat_table_test)
  results_t20<-records(stat_table_t20)
  
  batting_odi <-  results_odi[[1]]
  batting_odi <-  batting_odi[Type=="ODI"]
  setnames(batting_odi,
           c("M","Inns","NO","Runs","HS","Ave","BF","SR","X100","X50"),
           c("Matches_Played","Innings","NotOuts","Runs","Highest_Score","Batting_average","Balls_faced","Strike_rate","#100s","#50s"))
  
  bowling_odi <- results_odi[[2]]
  bowling_odi <- bowling_odi[Type=="ODI"]
  setnames(bowling_odi,
           c("M","Inns","Balls","Runs","Wkts","BBI","BBM","Ave","Eco","SR","X4W","X5W","X10W"),
           c("Matches_Played","Innings","Balls","Runs","Wickets","Best_Bowl_Inngs","Best_Bowl_Match","Bowling_average","Economy","Strike_rate","#4Wkcts","#5Wkcts","#10Wkcts"))
  
  
  
  batting_test <- results_test[[1]]
  batting_test <- batting_test[Type=="Test"]
  setnames(batting_test,
           c("M","Inns","NO","Runs","HS","Ave","BF","SR","X100","X50"),
           c("Matches_Played","Innings","NotOuts","Runs","Highest_Score","Batting_average","Balls_faced","Strike_rate","#100s","#50s"))
  
  bowling_test <- results_test[[2]]
  bowling_test <- bowling_test[Type=="Test"]
  setnames(bowling_test,
           c("M","Inns","Balls","Runs","Wkts","BBI","BBM","Ave","Eco","SR","X4W","X5W","X10W"),
           c("Matches_Played","Innings","Balls","Runs","Wickets","Best_Bowl_Inngs","Best_Bowl_Match","Bowling_average","Economy","Strike_rate","#4Wkcts","#5Wkcts","#10Wkcts"))
  
  
  
  batting_t20 <- results_t20[[1]]
  batting_t20 <- batting_t20[Type=="T20I"]
  setnames(batting_t20,
           c("M","Inns","NO","Runs","HS","Ave","BF","SR","X100","X50"),
           c("Matches_Played","Innings","NotOuts","Runs","Highest_Score","Batting_average","Balls_faced","Strike_rate","#100s","#50s"))
  
  bowling_t20 <- results_t20[[2]]
  bowling_t20 <- bowling_t20[Type=="T20I"]
  setnames(bowling_t20,
           c("M","Inns","Balls","Runs","Wkts","BBI","BBM","Ave","Eco","SR","X4W","X5W","X10W"),
           c("Matches_Played","Innings","Balls","Runs","Wickets","Best_Bowl_Inngs","Best_Bowl_Match","Bowling_average","Economy","Strike_rate","#4Wkcts","#5Wkcts","#10Wkcts"))
  
  all_data <- merge(biodata_test_new,
                    biodata_odi_new,
                    by=names(biodata_test_new),
                    all = T)
  
  all_data <- merge(all_data,
                    biodata_t20_new,
                    by=names(all_data),
                    all = T)
  
  all_data <- unique(all_data,by="Full_Name")
  
  all_data_batting <- merge(batting_test,
                            batting_odi,
                            by=names(batting_test),
                            all = T)
  all_data_batting <- merge(all_data_batting,
                            batting_t20,
                            by=names(all_data_batting),
                            all = T)
  all_data_bowling <- merge(bowling_test,
                            bowling_odi,
                            by=names(bowling_test),
                            all = T)
  all_data_bowling <- merge(all_data_bowling,
                            bowling_t20,
                            by=names(all_data_bowling),
                            all = T)
  
  data <- reactive({
    #observeEvent(input$format,{
    if(input$format=="All"){
      data <- all_data
      
    }else if(input$format=="Test"){
      data=biodata_test_new
    }else if(input$format=="ODI"){
      data=biodata_odi_new
    }else{
      data=biodata_t20_new
    }
    
    data <- data[,cntnt:=paste0('<div id="pop" style="text-align:center">
                                <img src="', Full_Name, '.jpg" width=100>',
                                '<br><strong>Name: </strong>',Full_Name,
                                '<br><strong>Major Team:</strong> ',Major_Team,
                                '<br><strong>Playing Role:</strong> ',Playing_Roll,
                                '<br><strong>Batting Style:</strong> ',Batting_Style,
                                '<br><strong>Bowling Style:</strong> ',Bowling_Style,
                                '<br><strong>International Debut:</strong> ',International_Debut,
                                '<br><strong>Place of Brith:</strong> ',place_of_birth,
                                '<br><strong>Date of Birth:</strong> ',DOB,
                                '</div')]
    
    return(data)
  })
  
  observeEvent(input$format,{
  updateSelectInput(session = session,"st", "Place of birth", choices =c(Choose='',unique(data()$state_of_birth)))
  updateSliderInput(session = session,inputId = "dec",
                   label = "Choose a year range",
                   min=min(data()$International_Debut,na.rm = T),
                   max = max(data()$International_Debut,na.rm = T),
                   value = c(quantile(data()$International_Debut,na.rm = T,names = F)[2],
                                                  quantile(data()$International_Debut,na.rm = T,names = F)[4]))
  })
  #data()$International_Debut<- as.character(data()$International_Debut)
  
  output$map <- renderLeaflet({
      leaflet(data()) %>%
        addTiles() %>%
        setView(lat= 20.71 ,lng = 77.67, zoom = 4.3) %>%
        addProviderTiles(provider = providers$Esri.WorldTopoMap) 
    })
  
  observeEvent(input$format,{
    
    if(input$format=="All"){
      dt  <- all_data_batting
      dt1 <- all_data_bowling
    }else{
      dt  <- all_data_batting[Type==input$format]
      dt1 <- all_data_bowling[Type==input$format]
    }
    
    observeEvent(c(input$st,input$dec),{
      #observeEvent(input$dec,{
      if(input$st!=""){
        data=data()
       
          data <- data[state_of_birth==input$st & International_Debut %in% input$dec[1]:input$dec[2] ]
          
          dt<- dt[Full_Name %in% c(data$Full_Name)]
          dt1 <- dt1[Full_Name %in% c(data$Full_Name)]
          
          output$table1 <- DT::renderDataTable({
            DT::datatable(dt,options = list(scrollX = TRUE))
          })
          output$table2 <- DT::renderDataTable({
            DT::datatable(dt1,options = list(scrollX = TRUE))
          })
          
          # create the leaflet map  
          if(nrow(data)>0){
          output$map <- renderLeaflet({
           
            leaflet(data,options = leafletOptions(minZoom = 0, maxZoom = 15,worldCopyJump = T)) %>%
              addTiles() %>%
              setView(lat=mean(data$lat1) ,lng = mean(data$lon1),zoom = 4) %>%
              addProviderTiles(provider = providers$Esri.WorldTopoMap) %>% 
              addMarkers(data = data,
                         lat =  ~lat1,
                         lng =  ~lon1,
                         label = as.character(data$Full_Name),
                         popup = as.character(data$cntnt))%>%
              addEasyButton(easyButton(
                icon="fa-crosshairs", title="ME",
                onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
          }) 
          }else{
            output$map <- renderLeaflet({
              leaflet(data()) %>%
                addTiles() %>%
                setView(lat= 20.71 ,lng = 77.67, zoom = 4.3) %>%
                addProviderTiles(provider = providers$Esri.WorldTopoMap) 
            })
              
            }
                                 
          
        
        }
    })
  })
  
}
shinyApp(ui, server)
