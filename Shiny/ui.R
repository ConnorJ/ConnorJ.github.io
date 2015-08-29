#library(ShinyDash)
library(shiny)
library(reshape2)
library(leaflet)

library(RMySQL)
library(psych)
connection <- dbConnect(MySQL(),
                        user = 'connorj_site',
                        password = 'mwWXsJ8Eot4_',
                        host = '108.167.180.87',
                        dbname='connorj_db')

rs <- dbSendQuery(connection,  statement = paste('select * from data'))
grad.merge <- fetch(rs, n = -1)   # extract all rows


#query <- parseQueryString(session$clientData$url_search)
query <- parseQueryString("?id=2")
if(is.null(query$id)){query$id=2}

rs <- dbSendQuery(connection,  statement = paste("select * from settings where id ='", query$id ,"'", sep = ""))
settings <- fetch(rs, n = -1) 
on.exit(dbDisconnect(connection))

head(settings$color[1])

inputData = grad.merge
College = c( "All", unique(inputData$College))
WorkType = c( "All", "Internship", "Full Time Employment", "Cooperative Education")
DegreeLevel = c( "All", levels(unique(inputData$Degree.Level)))


shinyUI(fluidPage(
  
  tags$head(
    tags$style(HTML(paste("

        body {
          background-color: #",settings$color[1],";
        }

    ", sep="")))
  ),
  
  
 tags$head(tags$link(rel='stylesheet', type='text/css', href='styles.css')),
  leafletMap(
   "map", "100%", 400,
    initialTileLayer = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
  initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
   options=list(
      center = c(37.45, -93.85),
      zoom = 4,
      maxBounds = list(list(17, -180), list(59, 180))
    )
  ),
  
  
  # Application title
  titlePanel(settings$title[1]),
  hr(),
  
  
  # Sidebar with controls to select a dataset and specify the
  # number of observations to view
  sidebarLayout(
    sidebarPanel(
      selectInput("College", "Choose a College", choices = College, selected= "All"),
      selectInput("Major", "Choose a Major", choices = NULL, selected="All" ),
      selectInput("DegreeLevel", "Degree Level", choices = DegreeLevel, selected="All" ),
      selectInput("WorkType", "Type of Position", choices = WorkType, selected="All" ),
      hr(),
      sliderInput("radius", "Choose Circle Radius:", min=0, max=10, value=5),
      hr(),
      h5("Top Hires in this Area"),
      tableOutput("companies1"),
      hr(),
      h5("Salaries in this Area"),
      plotOutput("plot1")
    ),
    
    
    mainPanel( 
      h4('Companies Hiring in this Area'),
      h5('Based on the latest five years of self-reported student data.'),
      dataTableOutput("people1")                
    )
  )
))
