rm(list=ls())
options(stringsAsFactors = FALSE)
options(mc.cores = parallel::detectCores())

library("geodata")
library(AOI)
library(climateR)
library(sf)
library(shiny)
library(bslib)
library(leaflet)
library(dplyr)
library(ggplot2)
library(leafpop)
library(terra)
setwd("Dan Vis app folder")

EF<-read_sf("data/reg1_eco_l3/reg1_eco_l3.shp")
EF<-dplyr::filter(EF,NA_L1NAME %in% c("EASTERN TEMPERATE FORESTS","NORTHERN FORESTS"))
#plot(EF)

new_england_spdf <- as(EF, "Spatial")

v <- vect(new_england_spdf)


cdatN<-getTerraClimNormals(
  v,
  c("tmax","ppt","def"),
  scenario = "19812010",
  month = 1:12,
  verbose = FALSE,
  ID = NULL,
  dryrun = FALSE
)

cdat2<-getTerraClimNormals(
  v,
  c("tmax","ppt","def"),
  scenario = "2C",
  month = 1:12,
  verbose = FALSE,
  ID = NULL,
  dryrun = FALSE
)

cdat4<-getTerraClimNormals(
  v,
  c("tmax","ppt","def"),
  scenario = "4C",
  month = 1:12,
  verbose = FALSE,
  ID = NULL,
  dryrun = FALSE
)


p<-read.csv("data/dater4tool.csv")

ui <- page_fluid(
  titlePanel("Climate Adjusted Provenancing Tool"),
  sidebarLayout(
    sidebarPanel=sidebarPanel(width = 3,
                              card(tags$p("1) Input the coordinates of your focal site here.", style = "font-family: 'Calibri'; font-size: 16px;"),  
                                   numericInput("Long", "longitude", value = -72.5,step=0.1),
                                   numericInput("Lat", "latitiude", value = 42.4,step=0.1)),
                              card(tags$p("2) Select the climate projection SCENARIO and climate variable FILTER you'd like to include in the reference site finder", style = "font-family: 'Calibri'; font-size: 16px;"),
                                   
                                   selectizeInput(inputId = 'scenario',label=" choose climate scenario",choices=c("contemporary","low (+2C)","med (+4C)"),selected="contemporary"),
                                   selectizeInput(inputId = 'filtr',label="choose a climate filter",choices=c("temperature","climatic water deficit"),selected="temperature")),
                              
                              
                              card(tags$p("5) Select the plants growth habits of interest.", style = "font-family: 'Calibri'; font-size: 16px;"),
                                   checkboxGroupInput("habit",label ="Growth Habit", choices = sort(unique(p$habit)),selected = unique(p$habit))),
                              card(downloadButton("downloadData", "Download data subset from map 1"),
                                   downloadButton("downloadData1", "Download table summary from plot 1"))#,
                                   #downloadButton("downloadData2", "Download data subset from map 2"))
    ),
    mainPanel = mainPanel(
      
      card(p("Welcome to the Climated Adjusted Provenancing Tool. You can use this tool to identify vegetation assembledges that correspond to current and future climate conditions at your location of interest and identify taxa that are widely distrubuted under these climate conditions. The data for underling this product come from Petri et al. 2022, and can be accessed in full at https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/ecy.3947. Follow the numeric guides below to explore the data. Metadata and additional information about this tool can be found here.")),                
      card(p("Climate projections for your focal site:"),
           tableOutput("tellum"),height = '20vh'),
      card(tags$p("3) View a map of climate-adjusted, potential provenancing locailities for your focal site:",style = "font-family: 'Calibri'; font-face: Bold; font-size: 16px;"),
           leafletOutput("mymap", height = '45vh'),
           p("Map 1: Click on any points to access additional information about the vegetation plot.")),
      card(
        tags$p("4) View the 30 most common species in your climate adjusted provenancing range.",style = "font-family: 'Calibri'; font-face: Bold; font-size: 16px;"),
        plotOutput("myplot",height='50vh',click = "plot_click"),
        tags$p("6) Click on the bars to see the occurence range of individual species on the map below.",style = "font-family: 'Calibri'; font-face: Bold; font-size: 16px;"),
        textOutput("variable_name"),
        leafletOutput("mymap2",height='30vh'),
        tags$p("Map 2: The green point depict every occurrence of the select species in the dataset, and the purple points represent those that are within the climate-adjusted provenancing climatic range",style = "font-family: 'Calibri'; font-face: Bold; font-size: 16px;"))
      
    ))
)


server <- function(input, output,session) {
  #####get home climate
  
  myval<-reactive({
    req(input$Lat, input$Long)
    coords1<- data.frame(Long=input$Long,Lat=input$Lat)
    
    goober<-terra::extract(cdatN$def,coords1)
    val = apply(goober[,5:9], 1, sum)
    
    goober2<-terra::extract(cdat2$def,coords1)
    val2 = apply(goober2[,5:9], 1, sum)
    
    goober4<-terra::extract(cdat4$def,coords1)
    val4 = apply(goober4[,5:9], 1, sum)
    
    myvalC<-data.frame(contemporary=val,low=val2,med=val4)
    
    gooberT<-terra::extract(cdatN$tmax,coords1)
    valT = apply(gooberT[,5:9], 1, max)
    
    gooberT2<-terra::extract(cdat2$tmax,coords1)
    valT2 = apply(gooberT2[,5:9], 1, max)
    
    gooberT4<-terra::extract(cdat4$tmax,coords1)
    valT4 = apply(gooberT4[,5:9], 1, max)
    
    myvalT<-data.frame(contemporary=valT,low=valT2,med=valT4)
    
    myvalo<-rbind(myvalT,myvalC)
    
    variable<-c("maximum temperature (C)", "climatic water deficit (mm)")
    
    myval<-cbind(variable,myvalo)
  })
  
  ####present climate info in a table
  output$tellum<-renderTable(myval())
  
  ###filter map 1 based on climate range
  filteredData <- reactive({
    if (input$scenario=="contemporary" & input$filtr=="climatic water deficit"){
      p %>% filter(def>=myval()$contemporary-2.5 &def<=myval()$contemporary+2.5 )  #### need to make this reactive to the climate scenario chose
      
    } else if (input$scenario=="low (+2C)" & input$filtr=="climatic water deficit"){
      p %>%filter(def>=myval()$low-2.5 &def<=myval()$low+2.5 ) 
      
    } else if (input$scenario=="med (+4C)"& input$filtr=="climatic water deficit") {
      p %>%filter(def>=myval()$med-2.5 &def<=myval()$med+2.5 ) 
      
    }else if (input$scenario=="contemporary" & input$filtr=="temperature"){
      p %>%filter(tmax>=myval()$contemporary-.5 &tmax<=myval()$contemporary+.5 ) 
      
    } else if (input$scenario=="low (+2C)" & input$filtr=="temperature"){
      p %>%filter(tmax>=myval()$low-.5 &tmax<=myval()$low+.5 ) 
      
    } else if (input$scenario=="med (+4C)"& input$filtr=="temperature") {
      p %>%filter(tmax>=myval()$med-.5 &tmax<=myval()$med+.5 ) 
      
    }
    
  }) 
  
  ####click on map to get species information
  
  #observeEvent(input$mymap_marker_click, {
  #clicked_marker <- input$mymap_marker_click
  #     if (!is.null(clicked_marker)) {
  #    # Access the clicked marker's data
  #   marker_info <- paste0("Plot: ", clicked_marker$AcceptedTaxonName, "\n")
  #  marker_info <- paste0(marker_info, "Latitude: ", clicked_marker$lat, "\n")
  # marker_info <- paste0(marker_info, "Longitude: ", clicked_marker$lng, "\n")
  # Update the text output
  #output$click_info <- renderText(marker_info)
  #  }
  #})
  
  
  ##filter plot based on selected growth habits
  filteredData2 <- reactive({
    filteredData()%>%
      filter(habit %in% input$habit) %>%
      group_by(AcceptedTaxonName) %>% count() %>% ungroup %>%
      arrange(n) %>%top_n(n=30)
    
  })  
  
  
  filteredDataSUM <- reactive({
    filteredData()%>%
      filter(habit %in% input$habit) %>%
      group_by(AcceptedTaxonName) %>% count() %>% ungroup %>%
      arrange(-n) %>%top_n(n=150)
    
  }) 
  
  
  ###make map1
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addScaleBar(position="bottomright") %>%
      setView(lng = -79, lat = 40, zoom = 4 )
  })
  
  
  
  observe({
    if (input$filtr=="climatic water deficit"){
      pal <- colorNumeric(
        palette = "inferno",
        domain = filteredData()$def
      )
      
      leafletProxy("mymap", data = filteredData()) %>%
        clearControls() %>%
        clearMarkers() %>%
        addMarkers(
          lng = ~input$Long,
          lat = ~input$Lat) %>%
        
        clearShapes()%>%
        addCircleMarkers(
          lng = ~Long,
          lat = ~Lat,
          layerId = ~Plot,
          color = ~ pal(def),
          popup = ~paste("<strong> PlotID: </strong>",Plot,"<br>",
                         "<strong> Latitude: </strong>",Lat,"<br>",
                         "<strong> Longitude: </strong>",Long,"<br>",
                         "<strong> Community: </strong>",sps,"<br>"),
          radius = 2) %>%
        addLegend(pal=pal,val=~def)
      
    }else if (input$filtr=="temperature"){ 
      
      pal <- colorNumeric(
        palette = "magma",
        domain = filteredData()$tmax
      )
      leafletProxy("mymap", data = filteredData()) %>%
        clearControls() %>%
        clearMarkers() %>%
        addMarkers(
          lng = ~input$Long,
          lat = ~input$Lat) %>%
        
        clearShapes()%>%
        addCircleMarkers(
          lng = ~Long,
          lat = ~Lat,
          layerId = ~Plot,
          color = ~ pal(tmax),
          popup = ~paste("<strong> PlotID: </strong>",Plot,"<br>",
                         "<strong> Latitude: </strong>",Lat,"<br>",
                         "<strong> Longitude: </strong>",Long,"<br>",
                         "<strong> Community: </strong>",sps,"<br>"),
          radius = 2)%>%
        addLegend(pal=pal,val=~tmax)
      
      
    }
    output$downloadData <- downloadHandler(
      filename = function(){"plotsinclimate.csv"}, 
      content = function(fname){
        write.csv(filteredData(), fname)
      }
    )
    
  })
  
  
  output$downloadData1 <- downloadHandler(
    filename = function(){"commonspecies.csv"}, 
    content = function(fname){
      write.csv(filteredDataSUM(), fname)
    }
  )
  
  
  
  
  
  ##make plot
  output$myplot<-renderPlot({ ggplot(filteredData2(),aes(y=reorder(AcceptedTaxonName,n),x=n))+geom_col()+theme_minimal(base_size = 18)+
      ylab("")+ theme(axis.text.y = element_text(face = "italic")) 
  })
  
  ##make it
  
  observeEvent(input$plot_click,{
    click_y <- input$plot_click$y
    clicked_variable <- round(click_y)
    output$variable_name <- renderText({
      paste("Focal species:", filteredData2()$AcceptedTaxonName[clicked_variable])
    })
    
    #make map 2
    output$mymap2 <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addScaleBar(position="bottomright") %>%
        setView(lng = -79, lat = 40, zoom = 4 )
    })
    
    
    filteredData3 <- reactive({
      p %>% filter(AcceptedTaxonName==filteredData2()$AcceptedTaxonName[clicked_variable])
    })
    
    filteredData4 <- reactive({
      filteredData3() %>% filter(Plot %in% filteredData()$Plot)
    })  
    
    leafletProxy("mymap2", data = filteredData3()) %>%
      clearShapes()%>%
      addCircles(color = "green",
                 
                 lng = ~Long,
                 lat = ~Lat,radius = 4,
                 popup = ~paste("<strong> PlotID: </strong>",Plot,"<br>",
                                "<strong> Latitude: </strong>",Lat,"<br>",
                                "<strong> Longitude: </strong>",Long,"<br>",
                                "<strong> Species </strong>",AcceptedTaxonName,"<br>",
                                "<strong> Relative Cover </strong>",paste(PctCov_100,"%"),"<br>" ))
    
    leafletProxy("mymap2", data = filteredData4()) %>%
      clearMarkers() %>%
      addCircleMarkers(color="purple",
                       lng = ~Long,
                       lat = ~Lat,radius= 3,
                       popup = ~paste("<strong> PlotID: </strong>",Plot,"<br>",
                                      "<strong> Latitude: </strong>",Lat,"<br>",
                                      "<strong> Longitude: </strong>",Long,"<br>",
                                      "<strong> Species </strong>",AcceptedTaxonName,"<br>",
                                      "<strong> Relative Cover </strong>",paste(PctCov_100,"%"),"<br>" ))
    
  })
  
  
  output$downloadData2 <- downloadHandler(
    filename = function(){"focalspeciesfull.csv"}, 
    content = function(fname){
      write.csv(filteredData3(), fname)
    }
  )
  
  
}

# Run the app ----
app <- shinyApp(ui = ui, server = server)