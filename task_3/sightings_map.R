library(shiny)
library(leaflet)
library(tidyverse)

seabirds <- read_csv("clean_data/seabirds_clean.csv")

seabirds <- seabirds %>% 
  mutate(species_common_name = str_remove(species_common_name, "\\(unidentified\\)"))

species_options <- sort(unique(seabirds$species_common_name))

ui <- fluidPage(
  
  titlePanel("Bird Sightings"),
  
  sidebarLayout(
    sidebarPanel(
    selectInput("common_name", "Species Selection", species_options)
  ),
  
  mainPanel(
    leafletOutput("sightings_map")
  )
  )
  
  
  
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)