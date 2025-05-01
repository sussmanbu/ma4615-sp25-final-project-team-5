#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(readr)
library(stringr)

options("readr.edition" = 1)

hiv_data <- read_rds("https://sussmanbu.github.io/ma4615-sp25-final-project-team-5/dataset_for_shiny/hiv_clean.rds") %>%
  mutate(
    neighborhood = str_remove(`Neighborhood (U.H.F)`, "\\s*\\d{3}$"),
    sex = SEX,
    race_ethnicity = `RACE/ETHNICITY`,
    year = YEAR,
    hiv_rate = `HIV DIAGNOSES PER 100,000 POPULATION`,
    aids_rate = `AIDS DIAGNOSES PER 100,000 POPULATION`
  )

nyc_geo <- st_read("https://sussmanbu.github.io/ma4615-sp25-final-project-team-5/dataset_for_shiny/UHF42.geo.json")

ui <- fluidPage(
  titlePanel("NYC HIV/AIDS Map Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Year", choices = unique(hiv_data$year)),
      selectInput("sex", "Sex", choices = unique(hiv_data$sex)),
      selectInput("race", "Race/Ethnicity", 
                  choices = unique(hiv_data$race_ethnicity[hiv_data$race_ethnicity != "Unknown"])),
      selectInput("metric", "Metric",
                  choices = c("HIV Diagnosis Rate" = "hiv_rate",
                              "AIDS Diagnosis Rate" = "aids_rate"))
    ),
    mainPanel(
      leafletOutput("map", height = 600)
    )
  )
)

server <- function(input, output, session) {
  filtered_data <- reactive({
    hiv_data %>%
      filter(
        year == input$year,
        sex == input$sex,
        race_ethnicity == input$race
      )
  })
  
  output$map <- renderLeaflet({
    data <- filtered_data()
    
    if (nrow(data) == 0 || all(is.na(data[[input$metric]]))) {
      leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        addControl("No data available for this selection.", position = "topright")
    } else {
      merged <- nyc_geo %>%
        left_join(data, by = c("GEONAME" = "neighborhood"))
      
      pal <- colorNumeric("YlOrRd", domain = merged[[input$metric]])
      
      leaflet(merged) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(
          fillColor = ~pal(get(input$metric)),
          color = "#444444",
          weight = 1,
          fillOpacity = 0.7,
          label = ~paste0(
            "<strong>", GEONAME, "</strong><br>",
            gsub("_", " ", input$metric), ": ",
            round(get(input$metric), 1)
          ) %>% lapply(htmltools::HTML),
          highlightOptions = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.9, bringToFront = TRUE)
        ) %>%
        addLegend(pal = pal, values = ~get(input$metric),
                  title = gsub("_", " ", input$metric),
                  position = "bottomright")
    }
  })
}

shinyApp(ui, server)
