library(shiny)
library(leaflet)
library(lubridate)
library(ggmap)

total_2015 <- read.csv("~/total_2015.csv")
total_2015$month <- month(total_2015$CST)
#Geocode the cities
cityList <- unique(total_2015$city)
cityList <- as.character(cityList)

lat=NULL
lon=NULL
for (city in cityList) {
  cityWithState = paste(city,"IL",sep=",")
  print(cityWithState)
  cityCoord <- geocode(cityWithState)
  print(cityCoord$lon)
  print(cityCoord$lat)
  lat=c(lat,cityCoord$lat)
  lon=c(lon,cityCoord$lon)
}

cityCoord <- as.data.frame(cbind(lon,lat,cityList))
cityCoord$lon<-as.numeric(levels(cityCoord$lon))[cityCoord$lon]
cityCoord$lat<- as.numeric(levels(cityCoord$lat))[cityCoord$lat]
colnames(cityCoord)[3] <- "city"

#UI Script
ui <- fluidPage(
#   leafletOutput("mymap",width="100%",height=500),
#   p(),
#   fluidRow(
#     column(6,plotOutput("plot1")),
#     column(6,plotOutput("plot2"))
#     
#   ),
#   fluidRow(
#     column(6,plotOutput("plot3")) 
#   )
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
     #   plotOutput("plot1"),
        plotOutput("plot2"),
        p(),
        p(),
        plotOutput("plot3")) 
    ) ,
    mainPanel(
      leafletOutput("mymap",width="100%",height=500),
      p(),
      plotOutput("plot1")
    )
    
  )
  
)

#Server Script
server <- function(input, output, session) {
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles(options = tileOptions(continuousWorld=FALSE)) %>% 
      setView(-87.681844, 40.41667, zoom = 6) %>% 
      addMarkers(lng=cityCoord$lon, lat=cityCoord$lat,layerId=cityCoord$city)
  })
  
  observe({
    leafletProxy("mymap") %>%  clearPopups()
    event <- input$mymap_marker_click
    if(is.null(event)) {
      return()
    }
    
    leafletProxy("mymap") %>% addPopups(event$lng, event$lat, toString(event$id))
    output$plot1 <- renderPlot({
      cityData <- subset(total_2015,city==event$id)
      calendarHeat(cityData$CST,cityData$CloudCover,varname = paste("CloudCover -",event$id,sep=" "),color = "ygb")
    })
    
    output$plot2 <- renderPlot({
      cityData <- subset(total_2015,city==event$id)
   #   calendarHeat(cityData$CST,cityData$MaxTempF,varname = paste("Max. Temperature F - ",event$id,sep=" "),color = "r2b")
      cityMAvgData <- aggregate(cityData[,2:21],list(cityData$month),"median")
      colnames(cityMAvgData)[1] <- "MonthNum"
      
      ggplot(data=cityMAvgData, aes(x=MonthNum,group=1)) +
        geom_line(aes(y=MaxTempF,col="MaxCol"),size=1.2 )+ 
        geom_line(aes(y=MinTempF,col="MinCol"),size=1.2)  + 
         scale_x_continuous(breaks=cityMAvgData$MonthNum,labels=month.abb)+
        labs(y=NULL,x=NULL)+ggtitle(paste("Monthly Min and Max (Median Values) Temperatures",event$id,sep="-"))+
        theme_bw()  +
     #guides(col=  guide_legend("right",c("Max Temp F","Min Temp F"),lty=c(1,1),col=c("blue","red")))
     scale_color_manual(name=NULL,labels=c("Max Temp F","Min Temp F"),values = c("MaxCol"="blue","MinCol"="red")) +
        expand_limits(y=c(0,80))
    })
    
    output$plot3 <- renderPlot({
      cityData <- subset(total_2015,city==event$id)
      cityData$CST <- as.Date(cityData$CST)
      cityData$quarter <- quarter(cityData$CST)
      cityData$quarter <- factor(cityData$quarter,levels=c(1,2,3,4),labels=c("Jan-Mar","Apr-Jun","Jul-Sep","Oct-Nov"))
      ggplot(data=cityData,aes(MeanTempF,Events,col=Events))+geom_jitter(aes(size=PrecipitationIn),alpha=0.6)+
        facet_wrap(~quarter)+ggtitle(paste("Overview of various weather events and Precipitation per quarter",event$id,sep="-"))
    })
    
  })
}

shinyApp(ui, server)
