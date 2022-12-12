library(shiny)
library(shinythemes)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(sf)
library(shinyjs)
library(dplyr)
library(plotly)
library(DT)
library(rgdal)


# data source
# https://data.wprdc.org/dataset/hospitals
# https://data.wprdc.org/dataset/allegheny-county-primary-care-facilities
# https://data.wprdc.org/dataset/median-age-death
# https://data.wprdc.org/dataset/allegheny-county-municipal-boundaries

# Load and merge data ----------------------------------------------
hospital <- read.csv('hospitals.csv')
age <- read.csv('md_age_death.csv')
primary <- read.csv('primary_care.csv')
muShape <- readOGR("./deathage/MuniMedianDeathAge2011_15.shp",
                   layer = "MuniMedianDeathAge2011_15")
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
                choices = sort(unique(age$municipality)),
                multiple = TRUE,
                selectize = TRUE,
                selected = c("PIISBURGH", "PENN HILLS TOWNSHIP")),
    
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
  
  # build a leaflet map ----------------------------------------
  output$leaflet <- renderLeaflet({
    
    map <- leaflet(data = muShape) %>%
      setView(-80, 40.5, 10) %>%
      addTiles() #%>%
    map
  })
  
  
  # update the map when municipality is selected ---------------------------
  
  observe({
    map0 <- leafletProxy("leaflet", data = muShape) 
    
})

}

# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)