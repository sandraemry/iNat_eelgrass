# app.R
library(shiny)
library(leaflet)
library(htmltools)

eel_sf <- readRDS("./data/eel_sf_clean.rds")

ui <- fluidPage(
  titlePanel("Eelgrass observations in BC (iNaturalist)"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year",
                  "Year",
                  min = 2010, max = 2025, # will be updated from data
                  value = c(2018, 2023),
                  sep = "",
                  step = 1),
      checkboxGroupInput(
        "species",
        "Species",
        choices = c("Zostera marina", "Zostera japonica"),
        selected = c("Zostera marina", "Zostera japonica")
      ),
      checkboxInput("cluster", "Cluster points (recommended if slow)", TRUE),
      helpText("Tip: Zoom in for dense areas (e.g., Vancouver Island).")
    ),
    mainPanel(
      leafletOutput("map", height = 700)
    )
  )
)

server <- function(input, output, session) {
  
  # ---- 1) Prepare sf once (no re-parsing every time) ----
  # Make sure observed_on is POSIXct; you already have it.
  base_sf <- reactive({
    req(inat_dat)
    
    eel <- inat_dat %>%
      filter(!is.na(longitude), !is.na(latitude)) %>%
      filter(between(longitude, -139, -114), between(latitude, 47, 61)) %>% # BC-ish bbox QA
      filter(scientific_name %in% c("Zostera marina", "Zostera japonica")) %>%
      mutate(
        observed_on_local = with_tz(observed_on, "America/Vancouver"),
        year = year(observed_on_local),
        observed_on_label = format(observed_on_local, "%Y-%m-%d %H:%M:%S %Z")
      )
    
    st_as_sf(eel, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  })
  
  # Update slider min/max from the data once
  observeEvent(base_sf(), {
    yrs <- sort(unique(base_sf()$year))
    yrs <- yrs[!is.na(yrs)]
    if (length(yrs) > 0) {
      updateSliderInput(
        session, "year",
        min = min(yrs), max = max(yrs),
        value = c(min(yrs), max(yrs))
      )
    }
  }, once = TRUE)
  
  # ---- 2) Filter reactively ----
  filtered_sf <- reactive({
    dat <- base_sf()
    req(input$species)
    
    dat %>%
      filter(scientific_name %in% input$species) %>%
      filter(!is.na(year)) %>%
      filter(year >= input$year[1], year <= input$year[2])
  })
  
  # ---- 3) Map ----
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
      addProviderTiles(providers$Esri.OceanBasemap, group = "Ocean") %>%
      fitBounds(lng1 = -139, lat1 = 47, lng2 = -114, lat2 = 61) %>%
      addLayersControl(
        baseGroups = c("Light", "Ocean"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  observe({
    dat <- filtered_sf()
    
    cols <- c("Zostera marina" = "#1b9e77",
              "Zostera japonica" = "#d95f02")
    
    popup <- with(
      dat,
      sprintf(
        "<b>%s</b><br/>Date: %s<br/>County: %s<br/>Lat/Lon: %.4f, %.4f<br/>Accuracy: %s m",
        htmlEscape(as.character(scientific_name)),
        ifelse(is.na(observed_on_label), "NA", observed_on_label),
        htmlEscape(place_county_name),
        latitude, longitude,
        ifelse(is.na(positional_accuracy), "NA", positional_accuracy)
      ) %>% lapply(HTML)
    )
    
    proxy <- leafletProxy("map", data = dat)
    
    proxy %>%
      clearMarkers() %>%
      clearControls()
    
    if (nrow(dat) == 0) {
      proxy %>% addControl("No points match your filters.", position = "topright")
      return()
    }
    
    if (isTRUE(input$cluster)) {
      proxy %>%
        addCircleMarkers(
          lng = ~longitude, lat = ~latitude,
          radius = 4, stroke = FALSE, fillOpacity = 0.6,
          color = ~cols[as.character(scientific_name)],
          popup = popup,
          clusterOptions = markerClusterOptions()
        )
    } else {
      proxy %>%
        addCircleMarkers(
          lng = ~longitude, lat = ~latitude,
          radius = 4, stroke = FALSE, fillOpacity = 0.6,
          color = ~cols[as.character(scientific_name)],
          popup = popup
        )
    }
    
    proxy %>%
      addLegend(
        "bottomright",
        colors = unname(cols[input$species]),
        labels = names(cols[input$species]),
        title = "Species",
        opacity = 1
      )
  })
}

shinyApp(ui, server)
