library(shiny)
library(shinythemes)
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
# https://data.wprdc.org/dataset/allegheny-county-municipal-boundaries

# Load and clean data ----------------------------------------------
hospital <- read.csv('hospitals.csv')
primary <- read.csv('primary_care.csv')
municipal <- readOGR("./municipal/LandRecords_LANDRECORDS_OWNER_Municipalities.shp")

# Avoid plotly issues ----------------------------------------------
pdf(NULL)

