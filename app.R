library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)
library(shinythemes)
library(ggplot2)
library(DT)
library(stringr)
library(tools)
library(maps) 
library(maptools)
library(rgeos)
library(leaflet)


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
                selected = c("Pittsburgh", "Penn Hills Township")),
    
    # Facility Type Selection ----------------------------------------------
    selectInput("ftSelect",
                "Facility Type:",
                choices = c("Hospital", "Primary Care Facility"),
                multiple = TRUE,
                selectize = TRUE,
                selected = c("Hospital")),
    
    # Race Selection --------------------------------------------
    selectInput("rcSelect",
                "Race:",
                choices = c("Black", "White"),
                multiple = TRUE,
                selectize = TRUE,
                selected = c("White"))
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
    req(input$rcSelect)
    filter(municipality %in% input$muSelect & facility %in% input$ftSelect & 
           race %in% input$rcSelect)
  })
  
  #base leaflet map, centered on pa's centroid 
  output$Map <- renderLeaflet({
    leaflet() %>%
      addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = "Google", group = "Google") %>%
      addProviderTiles(provider = providers$Wikimedia, group = "Wiki") %>%        
      setView(-80, 40.5, 10) %>%
      addLayersControl(baseGroups = c("Google", "Wiki"))
  })
  
  #subsetting shapefile to plot 
  musubset <- reactive({
    musubset <- subset(muShape, muShape$LABEL == input$muSelect)
    musubset
  })
  
  #observe function for changes in user input and add polygon
  observe({
    musub <- musubset()
    
    leafletProxy("Map", data = musub) %>%
      clearGroup(group = "musub") %>%
      addPolygons(popup = ~paste0("<b>", LABEL, "</b>"), group = "musub", layerId = ~OBJECTID, fill = TRUE, color = "red") %>%
      setView(lng = musub$x[1], lat = musub$y[1], zoom = 6)
  })
}

# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)