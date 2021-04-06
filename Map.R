library(shiny)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(rgdal)
library(sp)
library(raster)
library(httr)
library(jsonlite) 
library(ROpenWeatherMap) #OpenWeather
library(RCurl)
library(owmr) #OpenWeather
library(js)
library(dplyr)
library(ggplot2)
library(ropenaq)
library(rworldmap)
#testMazamaScience/actually works
library(knitr)
library(rmarkdown)
library(AirSensor)
library(MazamaSpatialUtils)

#api_key = "c69997e98686ddc71077096dc80c5204"

owmr_settings("c69997e98686ddc71077096dc80c5204") #open weather
initializeMazamaSpatialUtils() #Mazama/ For interacting w/ data


#installSpatialData("NaturalEarthAdm1")




ui <- dashboardPage(
  dashboardHeader(title = "Project Geo"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Maps",
        tabName = "maps",
        icon = icon("globe"),
        menuSubItem("Earthquake OSM", tabName = "m_osm", icon = icon("map")),
        menuSubItem("Earthquake Dark", tabName = "m_dark", icon = icon("map")),
        menuSubItem("Earthquake Heat", tabName = "m_heat", icon = icon("map")),
        menuSubItem("WorldPop", tabName = "m_chor", icon = icon("map")),
        menuSubItem("WeatherMap", tabName = "m_test", icon = icon("air")),
        menuSubItem("AirQuality", tabName = "m_air", icon = icon("plus-square"))
      )
    )
  ),

  dashboardBody(
    tabItems(
      tabItem(
        tabName = "m_osm",
        tags$style(type = 'text/css', '#earthq_osm {height: calc(100vh - 200px) !important;}'),
        leafletOutput('earthq_osm')
      ),
      
      tabItem(
        tabName = "m_dark",
        tags$style(type = 'text/css', '#earthq_dark {height: calc(100vh - 200px) !important;}'),
        leafletOutput('earthq_dark')
      ),
      tabItem(
        tabName = "m_heat",
        tags$style(type = 'text/css', '#earthq_heat {height: calc(100vh - 200px) !important;}'),
        leafletOutput('earthq_heat')
      ),
      tabItem(
        tabName = "m_chor",
        tags$style(type = 'text/css', '#chor_pop {height: calc(100vh - 200px) !important;}'),
        leafletOutput('chor_pop')
      ),
      tabItem(
        textInput(inputId = "gorod", label = "Input your city", value = "Abakan"),
        ###actionButton(textInput(inputId = "gorod", label = "Input your city"), "Button!"),
        tabName = "m_test",
        tags$style(type = 'text/css', '#test_map {height: calc(100vh - 200px) !important;}'),
        leafletOutput('test_map')
      ),
      tabItem(
        tabName = "m_air",
        tags$style(type = 'text/css', '#air_map {height: calc(100vh - 200px) !important;}'),
        fluidRow(
          box(plotOutput("plot1", height = 250)),
          
          box(leafletOutput("airmap", height = 250))
        ),
        fluidRow(
          box(leafletOutput('mazamap', height = 500))
        )
      )
    )
  )
)


server <- function(input, output, session){
  
  #url <- "http://api.airvisual.com/v2/countries?key=a049425d-2cc6-49e6-9842-2285db3b26ba"
  
  data(quakes)
  
  output$earthq_osm <- renderLeaflet({
    
    pal <- colorNumeric("Blues", quakes$mag)
    
    leaflet(data = quakes) %>% addTiles(group = "OpenStreetMap") %>%
      
      addProviderTiles(providers$Esri.WorldStreetMap, options = tileOptions(minZoom = 0, maxZoom = 7), group = "Esri.WorldStreetMap") %>%
      
      addProviderTiles(providers$Esri.WorldImagery, options = tileOptions(minZoom = 7, maxZoom = 13), group = "Esri.WorldImagery") %>%
      
      addCircles(radius = ~10^mag/10, weight = 1, color = ~pal(mag), fillColor = ~pal(mag), fillOpacity = 0.6, 
                 popup = ~as.character(mag), label = ~as.character(mag), group = "Points") %>%
      
      #addMarkers(lng = ~long, lat = ~lat, popup = ~as.character(mag), label = ~as.character(mag), group = "Markers") %>%
      
      addLayersControl(
        baseGroups = c("OpenStreetMap", "Esri.WorldStreetMap", "Esri.WorldImagery"),
        overlayGroups = c("Markers", "Points"),
        
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
    
    
    
      addLegend(
        position = "topright",
        pal = pal,
        values = ~mag,
        group = "Points",
        title = "Магитуда землетрясений"
      )
      
  })
  
  output$earthq_dark <- renderLeaflet({
    
    pal <- colorNumeric("Reds", airquality$mag)
    leaflet(data = quakes) %>% addProviderTiles(providers$CartoDB.DarkMatter, options = tileOptions(minZoom = 0, maxZoom = 7)) %>%
      
      addCircles(radius = ~10^mag/10, weight = 1, color=~pal(mag), fillColor = ~pal(mag), fillOpacity = 0.7, 
                 popup = ~as.character(mag), label = ~as.character(mag), group = "Points")%>%
      
      addProviderTiles(providers$Esri.WorldImagery, options = tileOptions(minZoom = 7, maxZoom = 14)) %>%
    
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ~mag,
        group = "Points",
        title = "Магнитуда землетрясений"
      )
  })
  
  output$earthq_heat <-renderLeaflet ({
    
    #HEATMAP
    
    pal <- colorNumeric("RdYlBu", quakes$mag) 
    
    leaflet(data = quakes)%>% addProviderTiles(providers$Esri.WorldPhysical, options = tileOptions(minZoom = 0, maxZoom = 13)) %>%
    
    addHeatmap(
      lng = ~long, lat = ~lat, intensity = ~mag, blur = 20, max = 0.05, radius = 15
    )
  })
  
  output$chor_pop <- renderLeaflet({
        map <- leaflet() %>% addProviderTiles(providers$CartoDB.DarkMatter, 
                                              options = tileOptions(minZoom = 0, 
                                                                    maxZoom = 14)) 
        
      })

    
    
####################################
  
  observeEvent(input$gorod, {
    
    output$test_map <- renderLeaflet({
      
      
      
      #WEATHER
      owm_data <- find_city(city = input$gorod, units = "metric") %>%
        owmr_as_tibble()
      map <- leaflet() %>% addProviderTiles(providers$CartoDB.DarkMatter, 
                                            options = tileOptions(minZoom = 0, 
                                                                  maxZoom = 14)) %>%
        
        
        add_weather(
          owm_data,
          template = "<b>{{name}}</b>, {{temp}}°C",
          icon = owm_data$weather_icon
        )
    })
  })
  ##############################
  ###########AIR openAQ, In progress
  
  
  
  
  
  output$airmap <- renderLeaflet({
    
    dataGeo <- aq_locations()
    dataGeo <- filter(dataGeo, location != "Test Prueba", location != "PA")
    
    worldMap <- map_data(map="world")
    
    airmap <- ggplot() + geom_map(data=worldMap, map=worldMap,
                            aes(map_id=region, x=long, y=lat),
                             fill = "grey60")
    
  })  
  
  
  #Still air, but what if in tabs?
  
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(5)]
    hist(data)
  })
  
  #Mazama air sensor
  
  output$mazamap <- renderLeaflet({
    
    
    
    #setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1")%>%
    #initializeMazamaSpatialUtils() %>%
    
    #pas_load(
      #datestamp = "YYYYmmddHH"
      #baseUrl = "https://www.purpleair.com/json?all=true"
      #datestamp = NULL
      #pas_downloadParseRawData(baseUrl = "")
    #) %>%
    #    pas_leaflet()

      #setSpatialDataDir('~/Data/Spatial')%>%
    #initializeMazamaSpatialUtils()%>%
    
    
    pas <- pas_createNew(
      countryCodes = "RU",
      includePWFSL = TRUE,
      lookbackDays = 1,
      baseUrl = "https://www.purpleair.com/json?all=true"
      #hourlyData$temperature <- 5/9 * (temperature - 32) probably
    )
    
      if ( interactive() ) {
        pas %>%
          #pas_filter(stateCode == "CA") %>%
          pas_leaflet()
      }
    
      
  })
  
    
  
}

shinyApp(ui, server)
