library(shiny)
library(leaflet)
library(tidyverse)
library(htmltools)
library(htmlwidgets)

seabirds <- read_csv("clean_data/seabirds_clean.csv")

seabirds <- seabirds %>% 
  filter(!is.na(lat) & !is.na(long)) %>% 
  mutate(species_common_name = str_to_title(str_remove(species_common_name, "\\(unidentified\\)|[A-Z]*$")),
         type_of_bird = str_extract(species_common_name, "[A-Z][a-z]+$"), .after = species_common_name)

bird_options <- sort(unique(seabirds$type_of_bird))

ui <- fluidPage(
  
  titlePanel("Bird Sightings"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("bird_input",
                  "Species Selection",
                  bird_options,
                  selected = "Penguin")
    ),
    
    mainPanel(
      leafletOutput("sightings_map")
    )
  )
)

server <- function(input, output, session) {
  
  output$sightings_map <- renderLeaflet({
    seabirds %>% 
      filter(type_of_bird == input$bird_input) %>% 
      leaflet() %>% 
      addProviderTiles(provider = providers$OpenStreetMap) %>% 
      addMarkers(lng = ~ long, lat = ~ lat,
                        label = ~species_common_name)
  })
  
}

shinyApp(ui, server)