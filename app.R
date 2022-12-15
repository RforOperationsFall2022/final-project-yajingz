library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(shinythemes)
library(ggplot2)
library(DT)
library(stringr)
library(tools)
library(shinyWidgets)
library(tidyverse)
library(rsconnect)
library(rgdal)
library(leaflet)
library(leaflet.extras)
library(sp)
library(rgeos)
library(RColorBrewer)


# data source
# https://data.wprdc.org/dataset/hospitals
# https://data.wprdc.org/dataset/allegheny-county-primary-care-facilities
# https://data.wprdc.org/dataset/median-age-death
# https://data.wprdc.org/dataset/allegheny-county-municipal-boundaries

# Load and merge data ----------------------------------------------
hospital <- read.csv('hospitals.csv')
primary <- read.csv('primary_care.csv')
muShape<- readShapePoly("./deathage/MuniMedianDeathAge2011_15")
muShape@data <- cbind(muShape@data, rgeos::gCentroid(muShape, byid = TRUE)@coords)
facility <- rbind(hospital, primary)

# Avoid plotly issues ----------------------------------------------
pdf(NULL)

# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "Allegheny County Healthcare Access")

# Dashboard Sidebar ----------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    
    # Menu Items ----------------------------------------------
    menuItem("Map", icon = icon("map-marked-alt"), tabName = "map"),
    menuItem("Plot", icon = icon("bar-chart"), tabName = "plot"),    
    menuItem("Table", icon = icon("table"), tabName = "table"),
    
    # Inputs: select variables to plot -----------------------------------------
    # Municipality Selection ----------------------------------------------
    selectInput("muSelect",
                "Municipality:",
                choices = sort(unique(muShape$LABEL)),
                multiple = TRUE,
                selectize = TRUE,
                selected = c("Sewickley Borough", "Penn Hills Township")),
    
    # Facility Type Selection ----------------------------------------------
    selectInput("ftSelect",
                "Facility Type:",
                choices = c("Hospital", "Primary Care Facility"),
                multiple = TRUE,
                selectize = TRUE,
                selected = c("Hospital"))
  )
)

# Dashboard body ----------------------------------------------
body <- dashboardBody(tabItems(
  
  # Map Page ------------------------------------------
  tabItem("map",
          fluidRow(leafletOutput("Map", height = "100vh"))),
  
  # Plot Page----------------------------------
  tabItem("plot",
          fluidRow(
            tabBox(title = "Plot", # build two tabs in this exploration tab
                   width = 12,
                   tabPanel("Facility Number By Municipality", plotlyOutput("plot_facility")),
                   tabPanel("Median Death Age By Municiplity", plotlyOutput("plot_age")))
          )),
  
  # Data Table Page----------------------------------
  tabItem("table",
          fluidPage(
            box(title = "Data Table With Selected Features", 
                dataTableOutput("table"), width = 12)))
))

ui <- dashboardPage(header, sidebar, body)


# Define server function required to create plots and value boxes -----
server <- function(input, output) {
  
  # Input with municipality, facility, and race all selected ----------
  SelectedMuInput <- reactive({
    req(input$muSelect)
    req(input$ftSelect)
    filter(muShape@data, LABEL %in% input$muSelect)
  })

  SelectedFalInput <- reactive({
    req(input$muSelect)
    req(input$ftSelect)  
    filter(facility, Facility %in% input$ftSelect)
  })
  
  # set color -------------------------------------------
  pal_age <- colorNumeric(
    palette = "Oranges",
    domain = muShape$TOTAL_MD_A)
  
  pal_fal <- colorFactor(c("#DECBE4", "#D01C8B"), c("Hospital", "Primary Care"))
  
  # base leaflet map -------------------------------------------
  output$Map <- renderLeaflet({
    leaflet(muShape) %>%
      addTiles() %>%
      setView(-80, 40.5, 10)
  })
  
  # add polygons to the map ---------------------------------------------------
  observe({
    leafletProxy("Map", data = muShape) %>%
      clearMarkers() %>%
      addPolygons(popup = ~paste0("<b>", LABEL, "</b>"), fillOpacity = 0.8, 
                  weight = 0.5, color = ~pal_age(TOTAL_MD_A)) %>%
      addLegend(position = "topright", 
                pal = pal_age, 
                values = muShape$TOTAL_MD_A, 
                title = "Municipality")
  })
  
  # add points to the map ---------------------------------------------------
  observe({
    leafletProxy("Map", data = facility) %>%
      clearMarkers() %>%
      addCircleMarkers(lng = ~Longitude, lat = ~Latitude, 
                       radius = 0.2, 
                       color = ~pal_fal(Type), 
                       popup = ~paste0("<b>", Facility, "</b> (", Type, ")</b>"), 
                       clusterOptions = markerClusterOptions(),
                       fillOpacity = 1) %>%
      addLegend(position = "topright", 
                pal = pal_fal, 
                values = facility$Type, 
                title = "Median Age at Death")
  }) 
  
  # Plot comparing facility number -----------------------------
  output$plot_facility <- renderPlotly({
    dat <- SelectedFalInput()
    ggplot(data = dat, aes(x = LABEL, y = factor(Facility), fill = LABEL)) + 
      geom_bar(stat = "identity")+
      xlab("Municipality")+
      ylab("Number of Facilities")
  })
  
  # Plot comparing median death age -----------------------------
  output$plot_age <- renderPlotly({
    dat <- SelectedMuInput()
      ggplot(data = dat, aes(x = LABEL, y = TOTAL_MD_A, fill = LABEL)) + 
      geom_bar(stat = "identity")+
      xlab("Municipality")+
      ylab("Median Death Age")
  })
}

# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)