# app.R
library(shiny)
library(dplyr)
library(leaflet)
library(DT)
library(readr)

# ---- Load data ----
# Put your CSV at: data/populations.csv
pop <- readr::read_csv("data/populations.csv", show_col_types = FALSE) %>%
  mutate(
    species = as.character(species),
    taxon_group = as.character(taxon_group),
    study_id = as.character(study_id),
    marker = as.character(marker),
    metric = as.character(metric),
    value = as.numeric(value),
    lat = as.numeric(lat),
    lon = as.numeric(lon)
  ) %>%
  filter(!is.na(lat), !is.na(lon), !is.na(value))

ui <- navbarPage(
  title = "GenCoDE — Explore",
  tabPanel(
    "Map",
    sidebarLayout(
      sidebarPanel(
        width = 3,

        selectizeInput(
          "species", "Species",
          choices = sort(unique(pop$species)),
          selected = NULL,
          multiple = TRUE,
          options = list(placeholder = "All species")
        ),

        selectizeInput(
          "taxon_group", "Taxonomic group",
          choices = sort(unique(pop$taxon_group)),
          selected = NULL,
          multiple = TRUE,
          options = list(placeholder = "All groups")
        ),

        selectizeInput(
          "marker", "Genetic marker / data type",
          choices = sort(unique(pop$marker)),
          selected = NULL,
          multiple = TRUE,
          options = list(placeholder = "All marker types")
        ),

        selectInput(
          "metric", "Metric shown on map",
          choices = sort(unique(pop$metric)),
          selected = sort(unique(pop$metric))[1]
        ),

        selectizeInput(
          "study_id", "Study (DOI / ID)",
          choices = sort(unique(pop$study_id)),
          selected = NULL,
          multiple = TRUE,
          options = list(placeholder = "All studies")
        ),

        sliderInput(
          "value_range", "Metric range filter",
          min = floor(min(pop$value, na.rm = TRUE) * 100) / 100,
          max = ceiling(max(pop$value, na.rm = TRUE) * 100) / 100,
          value = c(
            floor(min(pop$value, na.rm = TRUE) * 100) / 100,
            ceiling(max(pop$value, na.rm = TRUE) * 100) / 100
          ),
          step = 0.01
        ),

        checkboxInput("show_legend", "Show legend", TRUE),
        helpText("Tip: Use filters to narrow data; map & table update together.")
      ),

      mainPanel(
        width = 9,
        leafletOutput("map", height = "650px"),
        br(),
        uiOutput("summary_line")
      )
    )
  ),

  tabPanel(
    "Table",
    fluidRow(
      column(
        12,
        DTOutput("tbl")
      )
    )
  )
)

server <- function(input, output, session) {

  filtered <- reactive({
    dat <- pop %>% filter(metric == input$metric)

    if (!is.null(input$species) && length(input$species) > 0)
      dat <- dat %>% filter(species %in% input$species)

    if (!is.null(input$taxon_group) && length(input$taxon_group) > 0)
      dat <- dat %>% filter(taxon_group %in% input$taxon_group)

    if (!is.null(input$marker) && length(input$marker) > 0)
      dat <- dat %>% filter(marker %in% input$marker)

    if (!is.null(input$study_id) && length(input$study_id) > 0)
      dat <- dat %>% filter(study_id %in% input$study_id)

    dat %>%
      filter(value >= input$value_range[1], value <= input$value_range[2])
  })

  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 0, lat = 20, zoom = 2)
  })

  observe({
    dat <- filtered()

    pal <- colorNumeric(
      palette = "viridis",
      domain = dat$value
    )

    leafletProxy("map") %>%
      clearMarkers() %>%
      clearControls() %>%
      addCircleMarkers(
        data = dat,
        lng = ~lon, lat = ~lat,
        radius = 5,
        stroke = TRUE, weight = 0.6,
        fillOpacity = 0.85,
        color = ~pal(value),
        popup = ~paste0(
          "<b>", species, "</b><br/>",
          "Group: ", taxon_group, "<br/>",
          "Metric: ", metric, " = ", round(value, 4), "<br/>",
          "Marker: ", marker, "<br/>",
          "Study: ", study_id,
          if ("site_name" %in% names(dat)) paste0("<br/>Site: ", site_name) else "",
          if ("year" %in% names(dat)) paste0("<br/>Year: ", year) else "",
          if ("n" %in% names(dat)) paste0("<br/>n: ", n) else ""
        )
      ) %>%
      {
        if (isTRUE(input$show_legend) && nrow(dat) > 0) {
          addLegend(
            .,
            position = "bottomright",
            pal = pal,
            values = dat$value,
            title = input$metric,
            opacity = 1
          )
        } else .
      }
  })

  output$tbl <- renderDT({
    dat <- filtered() %>%
      arrange(species, study_id)

    datatable(
      dat,
      filter = "top",
      rownames = FALSE,
      options = list(pageLength = 25, scrollX = TRUE)
    )
  })

  output$summary_line <- renderUI({
    dat <- filtered()
    tags$p(
      tags$b("Showing: "),
      nrow(dat), " populations | ",
      length(unique(dat$species)), " species | ",
      length(unique(dat$study_id)), " studies"
    )
  })
}

shinyApp(ui, server)