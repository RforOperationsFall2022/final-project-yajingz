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
library(plotly)


# data source
# https://data.wprdc.org/dataset/hospitals
# https://data.wprdc.org/dataset/allegheny-county-primary-care-facilities
# https://data.wprdc.org/dataset/median-age-death

# Load, clean, and merge data ----------------------------------------------
hospital <- read.csv('hospitals.csv')
primary <- read.csv('primary_care.csv')
muShape <- readOGR("./deathage/MuniMedianDeathAge2011_15.shp", layer = "MuniMedianDeathAge2011_15")
muShape$TOTALdeath <- as.numeric(muShape$TOTALdeath)
muShape$BLACKdeath <- as.numeric(muShape$BLACKdeath)
muShape$WHITEdeath <- as.numeric(muShape$WHITEdeath)
facility <- rbind(hospital, primary)

# Avoid plotly issues ----------------------------------------------
pdf(NULL)

# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "Allegheny County Healthcare Access")

# Dashboard Sidebar ----------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    
    # Menu Items for Map ----------------------------------------------
    menuItem("Map", icon = icon("map-marked-alt"), tabName = "map"),
    # Map Variable Selection ----------------------------------------------
    selectInput("vbSelect",
                "Map Variable Display:",
                choices = c("Total Death Number", "Median Death Age"),
                multiple = FALSE,
                selectize = TRUE,
                selected = c("Median Death Age")),   
    # Facility Type Selection ----------------------------------------------
    selectInput("ftSelect",
                "Facility Type Display:",
                choices = c("Hospital", "Primary Care"),
                multiple = FALSE,
                selectize = TRUE,
                selected = c("Hospital")),   
    
    # Menu Items for Plot ----------------------------------------------
    menuItem("Plot", icon = icon("bar-chart"), tabName = "plot"), 
    # Municipality Selection -------------------------------------
    selectInput("muSelect",
                "Municipality:",
                choices = sort(unique(muShape$LABEL)),
                multiple = TRUE,
                selectize = TRUE,
                selected = c("Sewickley Borough", "Penn Hills Township")),
    
    # Menu Items for Table ----------------------------------------------
    menuItem("Table", icon = icon("table"), tabName = "table")

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
                   tabPanel("Total Death Number By Municiplity & Race", plotlyOutput("plot_number")),
                   tabPanel("Median Death Age By Municiplity & Race", plotlyOutput("plot_age"))
            )
          )),
  
  # Data Table Page----------------------------------
  tabItem("table",
          fluidPage(
            tabBox(title = "Data Table With Selected Features", 
                width = 12,
                tabPanel("Death Data Table", dataTableOutput("table_death")),
                tabPanel("Healthcare Data Table", dataTableOutput("table_health")))
          ))
))

ui <- dashboardPage(header, sidebar, body)


# Define server function required to create plots and value boxes -----
server <- function(input, output) {
  
  # Input with features selected ---------------------------------
  SelectedMuInput <- reactive({
    req(input$muSelect)
    filter(muShape@data, LABEL %in% input$muSelect)
  })
  
  SelectedFtInput <- reactive({
    req(input$ftSelect)
    filter(facility, Type %in% input$ftSelect)
  })

  
  # set color ---------------------------------------------
  pal_age <- colorNumeric(
    palette = "Oranges",
    domain = muShape$TOTAL_MD_A)
  
  bins <-c(0, 500, 2000, 5000, 10000, 50000)
  pal_number <- colorBin(
    palette = "Blues",
    bins = bins,
    na.color = "White",
    domain = muShape$TOTALdeath)
  
  pal_fal <- colorFactor(c("#800080", "#D01C8B"), c("Hospital", "Primary Care"))
  
  # base leaflet map -------------------------------------------
  output$Map <- renderLeaflet({
    leaflet(muShape) %>%
      addTiles() %>%
      setView(-80, 40.5, 10)
  })
  
  # add polygons to the map ---------------------------------------------------
  observe({
    Map <- leafletProxy("Map", data = muShape)
    
    if (input$vbSelect == "Median Death Age") {
      Map %>%
        clearMarkers() %>% clearShapes() %>% clearControls() %>%
        addPolygons(popup = ~paste0("<b>", LABEL, "</b>"), fillOpacity = 0.8, 
                    weight = 0.5, color = ~pal_age(TOTAL_MD_A)) %>%
        addLegend(position = "topright", 
                  pal = pal_age, 
                  values = muShape$TOTAL_MD_A, 
                  title = "Median Death Age")
    }
    else{
      Map %>%
        clearMarkers() %>% clearShapes() %>% clearControls() %>%
        addPolygons(popup = ~paste0("<b>", LABEL, "</b>"), fillOpacity = 0.8, 
                    weight = 0.5, color = ~pal_number(TOTALdeath)) %>%
        addLegend(position = "topright", 
                  pal = pal_number, 
                  values = muShape$TOTALdeath, 
                  title = "Total Death Number")
      
    }
  })
  
  # add points to the map ---------------------------------------------------
  observe({
    Map <- leafletProxy("Map", data = SelectedFtInput())
    
    if (length(input$ftSelect)>0) {
      Map %>%
        clearMarkers() %>% 
        addCircleMarkers(lng = ~Longitude, lat = ~Latitude, 
                         radius = 0.2, 
                         color = ~pal_fal(Type), 
                         popup = ~paste0("<b>", Facility, "</b> (", Type, ")</b>"),
                         fillOpacity = 1)
    }
    
    else{
      Map %>% clearMarkers()
      }
  }) 
  
  # Plot Total Death Number By Municiplity & Race -----------------------------
  output$plot_number <- renderPlotly({
    fig <- plot_ly(SelectedMuInput(),
                   x = ~LABEL,
                   y = ~BLACKdeath,
                   marker = list(color = "rgb(20, 20, 20)"),
                   type = 'bar',
                   name = 'Black Population')
    fig <- fig %>%
      add_trace(y = ~WHITEdeath,
                marker = list(color = "rgb(195, 195, 195)"),
                name = 'White Population')
    fig <- fig %>%
      layout(xaxis = list(title = 'Municipality'), 
             yaxis = list(title = 'Total Death Number'),
             barmode = 'group',
             paper_bgcolor = 'rgba(238, 251, 251, 1)',
             plot_bgcolor = 'rgba(238, 251, 251, 1)',
             legend=list(title=list(text='<b> Less than 5 deaths is shown as null. </b>',
                                                 font = list(size = 12))))
  })  
  
  # Plot Median Death Age By Municiplity & Race -----------------------------
  output$plot_age <- renderPlotly({
    fig <- plot_ly(SelectedMuInput(),
                   x = ~LABEL,
                   y = ~BLACK_MD_A,
                   marker = list(color = "rgb(20, 20, 20)"),
                   type = 'bar',
                   name = 'Black Population')
    fig <- fig %>%
      add_trace(y = ~WHITE_MD_A,
                marker = list(color = "rgb(195, 195, 195)"),
                name = 'White Population')
    fig <- fig %>%
      layout(xaxis = list(title = 'Municipality'), 
             yaxis = list(title = 'Median Death Age'),
             barmode = 'group',
             paper_bgcolor = 'rgba(255, 223, 191, 1)',
             plot_bgcolor = 'rgba(255, 223, 191, 1)',
             legend=list(title=list(text='<b> Less than 5 deaths is shown as null. </b>',
                                                 font = list(size = 12))))
  })  
  
  # Data table ----------------------------------------------
  output$table_death <- DT::renderDataTable({
    datatable(
      SelectedMuInput(), extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf')
      )
    )
  })
  
  output$table_health <- DT::renderDataTable({
    datatable(
      SelectedFtInput(), extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf')
      )
    )
  })
}

# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)