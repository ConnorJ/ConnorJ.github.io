library(shiny)
library(ggplot2)
library(reshape2)
library(leaflet)
library(plyr)


library(RMySQL)
library(psych)
connection <- dbConnect(MySQL(),
                        user = 'connorj_site',
                        password = 'mwWXsJ8Eot4_',
                        host = '108.167.180.87',
                        dbname='connorj_db')

rs <- dbSendQuery(connection,  statement = paste('select * from data'))
grad.merge <- fetch(rs, n = -1)   # extract all rows
on.exit(dbDisconnect(connection))

inputData = grad.merge
#inputData = grad.merge
mapdata <- ddply(grad.merge,.(City,State),transform,count = NROW(piece))
mapstuff <- mapdata[ ! duplicated( mapdata[ c("City" , "State") ] ) , ]


# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output, session) {

   map <- createLeafletMap(session, 'map')

  citiesInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(inputData[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(inputData,
           Lat >= latRng[1] & Lat <= latRng[2] &
             Long >= lngRng[1] & Long <= lngRng[2])
  })
  
  
  filter <- reactive({  
    df <- citiesInBounds()
    
    if (input$WorkType != "All") {df <- subset(df, Work.Type == input$WorkType)}
    if (input$DegreeLevel != "All") {df <- subset(df, Degree.Level == input$DegreeLevel)}
    if (input$College == "All" & input$Major == "All"){df}
    if (input$College != "All" & input$Major == "All"){df <- subset(df, College == input$College)}
    if (input$College != "All" & input$Major != "All"){
      df <- subset(df, College == input$College & Major.1.at.Graduation == input$Major | Major.2.at.Graduation == input$Major )
    }
    df$Location <- paste(df$City, df$State)
    
    output$people1 = renderDataTable({
      dfTable <- df[, c(2, 7, 6, 3,4)]
      return(dfTable)
    })
     
    output$companies1 = renderTable({
      if (nrow(df)>0){
        df <- count(df,"Employer")
        df <- df[order(df$freq, decreasing = TRUE),]
        colnames(df) <- c("Employer","Employees")
        if (nrow(df)<5) {
          df<- df[1:nrow(df),]
          return(df)
        } else {
          df<- df[1:5,]
          return(df)
        }
      } 
    },include.rownames=FALSE)
    

 
    
    output$plot1 = renderPlot({
      if (nrow(df)>=5){
        print(qplot(data=(subset(na.omit(df), is.na(Work.Type) | df$Compensation <= 500000 )), x=as.numeric(Compensation), geom="density", bin = 5000, ylab = "Count", xlab = "Annual Compensation", fill="white") +
                xlim(0, 175000) + scale_size(guide = 'none') + guides(fill=FALSE) +
                theme(axis.title.y = element_text(size = rel(1)),
                      axis.title.x = element_text(size = rel(1)),
                      axis.text.y = element_blank())
        )
      } else {print("Not Enough Data")}
      
    })
    

    
    return(df)
  })
  
  radiusFactorcalc<- reactive({ radiusFactor <- 5000 * (as.numeric(input$radius)^2 +1) })
  
  observe({
    radiusFactor <- radiusFactorcalc()
    map$clearShapes() 
    mapdffilter <- filter()
    mapdata <- ddply(mapdffilter,.(City,State),transform,count = NROW(piece))
    mapstuff <- mapdata[ ! duplicated( mapdata[ c("City" , "State") ] ) , ]
    if (nrow(mapstuff) == 0)
      return()
    
    map$addCircle(
      mapstuff$Lat,
      mapstuff$Long,
      sqrt(mapstuff[["count"]]) * radiusFactor / max(5, input$map_zoom)^2,
      row.names(mapstuff),
      list(
        weight=1.2,
        fill=TRUE,
        color='#999933'
      )
    )
  })
  
  observe({
    dfmajor <- unique(subset(inputData, College == inputData$College))
    Major = c( "All", levels(factor(dfmajor$Major.1.at.Graduation)))
    updateSelectInput(session, "Major", choices = Major, selected="All")
  })
  

})



