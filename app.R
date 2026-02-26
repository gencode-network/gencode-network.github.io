library(shiny)
library(dplyr)
library(leaflet)
library(DT)
library(readr)
library(htmltools)
library(htmlwidgets)
library(purrr)

# ---- Load data ----
dat0 <- readr::read_csv("data/gencode_points.csv", show_col_types = FALSE)

# Normalize column names (defensive)
names(dat0) <- trimws(tolower(names(dat0)))

required_cols <- c(
  "community_id","community_name","region","lat","lon",
  "taxon_group","species","study_id","contact","marker",
  "method","n_loci","genome_build","pipeline_version",
  "year","n_samples","ne","ne_ci_low","ne_ci_high"
)
missing <- setdiff(required_cols, names(dat0))
if (length(missing) > 0) {
  stop("Missing required columns in CSV: ", paste(missing, collapse = ", "))
}

dat0 <- dat0 %>%
  mutate(
    community_id      = as.character(community_id),
    community_name    = as.character(community_name),
    region            = as.character(region),
    taxon_group       = as.character(taxon_group),
    species           = as.character(species),
    study_id          = as.character(study_id),
    contact           = as.character(contact),
    marker            = as.character(marker),
    method            = as.character(method),
    year              = as.integer(year),
    n_samples         = as.integer(n_samples),
    ne                = as.numeric(ne),
    lat               = as.numeric(lat),
    lon               = as.numeric(lon),
    n_loci            = suppressWarnings(as.integer(n_loci)),
    genome_build      = as.character(genome_build),
    pipeline_version  = as.character(pipeline_version),
    ne_ci_low         = as.numeric(ne_ci_low),
    ne_ci_high        = as.numeric(ne_ci_high)
  ) %>%
  filter(!is.na(lat), !is.na(lon), !is.na(ne))

# ---- Popup builder: community summary + species dropdown that reveals species panels ----
build_community_popup <- function(df_comm) {

  comm_id   <- df_comm$community_id[1]
  comm_name <- df_comm$community_name[1]
  region    <- df_comm$region[1]

  n_species <- dplyr::n_distinct(df_comm$species)
  n_groups  <- dplyr::n_distinct(df_comm$taxon_group)
  mean_ne   <- mean(df_comm$ne, na.rm = TRUE)
  med_ne    <- median(df_comm$ne, na.rm = TRUE)

  safe_id <- gsub("[^A-Za-z0-9_]", "_", comm_id)
  select_id    <- paste0("sp_sel_", safe_id)
  container_id <- paste0("sp_container_", safe_id)

  species_list <- sort(unique(df_comm$species))

  # Use a safe token for option values / attribute selectors
  sp_token <- function(x) utils::URLencode(x, reserved = TRUE)

  options_html <- paste0(
    '<option value="">Select a species…</option>',
    paste0(
      '<option value="', sp_token(species_list), '">',
      htmlEscape(species_list),
      "</option>",
      collapse = ""
    )
  )

  panels <- lapply(species_list, function(sp) {
    sub <- df_comm %>% filter(species == sp)

    sp_taxon  <- paste(sort(unique(sub$taxon_group)), collapse = ", ")
    sp_marker <- paste(sort(unique(sub$marker)), collapse = ", ")
    sp_method <- paste(sort(unique(sub$method)), collapse = ", ")
    sp_years  <- paste(sort(unique(sub$year)), collapse = ", ")
    sp_nsamp  <- paste(sort(unique(sub$n_samples)), collapse = ", ")
    sp_loci   <- paste(sort(unique(na.omit(sub$n_loci))), collapse = ", ")
    sp_build  <- paste(sort(unique(na.omit(sub$genome_build[sub$genome_build != "NA"]))), collapse = ", ")
    sp_pipe   <- paste(sort(unique(na.omit(sub$pipeline_version[sub$pipeline_version != "NA"]))), collapse = ", ")
    sp_ne     <- round(mean(sub$ne, na.rm = TRUE), 2)

    ci_low  <- if (all(is.na(sub$ne_ci_low)))  NA else round(mean(sub$ne_ci_low,  na.rm = TRUE), 2)
    ci_high <- if (all(is.na(sub$ne_ci_high))) NA else round(mean(sub$ne_ci_high, na.rm = TRUE), 2)

    sp_studies  <- paste(sort(unique(sub$study_id)), collapse = ", ")
    sp_contacts <- paste(sort(unique(sub$contact)),  collapse = ", ")

    ci_line    <- if (!is.na(ci_low) && !is.na(ci_high)) paste0("Ne CI: ", ci_low, "–", ci_high, "<br/>") else ""
    loci_line  <- if (nzchar(sp_loci))  paste0("Loci / sites: ", htmlEscape(sp_loci), "<br/>") else ""
    build_line <- if (nzchar(sp_build)) paste0("Reference: ", htmlEscape(sp_build), "<br/>") else ""
    pipe_line  <- if (nzchar(sp_pipe))  paste0("Pipeline: ", htmlEscape(sp_pipe), "<br/>") else ""

    HTML(paste0(
      '<div class="sp-panel" data-token="', sp_token(sp), '" ',
      'style="display:none; margin-top:8px; padding-top:8px; border-top:1px solid #ddd;">',
        '<b>', htmlEscape(sp), '</b><br/>',
        'Taxon group: ', htmlEscape(sp_taxon), '<br/>',
        'Ne (mean): ', sp_ne, '<br/>',
        ci_line,
        'Marker(s): ', htmlEscape(sp_marker), '<br/>',
        'Method: ', htmlEscape(sp_method), '<br/>',
        loci_line, build_line, pipe_line,
        'Year(s): ', htmlEscape(sp_years), '<br/>',
        'Samples (n): ', htmlEscape(sp_nsamp), '<br/>',
        'Study ID(s): ', htmlEscape(sp_studies), '<br/>',
        'Contact: ', htmlEscape(sp_contacts),
      '</div>'
    ))
  })

  HTML(paste0(
    '<div style="min-width:260px;">',
      '<div style="font-size:14px;"><b>', htmlEscape(comm_name), '</b></div>',
      '<div style="color:#555;">', htmlEscape(region), ' (', htmlEscape(comm_id), ')</div>',

      '<div style="margin-top:6px;">',
        '<b>Community summary</b><br/>',
        'Species represented: ', n_species, '<br/>',
        'Taxonomic groups: ', n_groups, '<br/>',
        'Ne (mean): ', round(mean_ne, 2), '<br/>',
        'Ne (median): ', round(med_ne, 2),
      '</div>',

      '<div style="margin-top:10px;">',
        '<b>Species details</b><br/>',
        '<select id="', select_id,
          '" class="sp-select" data-container="', container_id,
          '" style="width:100%; margin-top:4px;">',
          options_html,
        '</select>',
        '<div id="', container_id, '">',
          paste0(vapply(panels, as.character, character(1)), collapse = ""),
        '</div>',
      '</div>',
    '</div>'
  ))
}

# ---- UI ----
ui <- navbarPage(
  title = tags$a(
    href = "https://gencode-network.github.io",
    target = "_self",
    style = "text-decoration:none;",
    "GenCoDE"
  ),

  tabPanel(
    "Map",
    sidebarLayout(
      sidebarPanel(
        width = 3,

        selectizeInput(
          "community", "Community",
          choices = sort(unique(dat0$community_name)),
          selected = NULL, multiple = TRUE,
          options = list(placeholder = "All communities")
        ),

        selectizeInput(
          "species", "Species",
          choices = sort(unique(dat0$species)),
          selected = NULL, multiple = TRUE,
          options = list(placeholder = "All species")
        ),

        selectizeInput(
          "taxon_group", "Taxonomic group",
          choices = sort(unique(dat0$taxon_group)),
          selected = NULL, multiple = TRUE,
          options = list(placeholder = "All groups")
        ),

        selectizeInput(
          "marker", "Genomic data type",
          choices = sort(unique(dat0$marker)),
          selected = NULL, multiple = TRUE,
          options = list(placeholder = "All marker types")
        ),

        selectizeInput(
          "study_id", "Study (DOI / ID)",
          choices = sort(unique(dat0$study_id)),
          selected = NULL, multiple = TRUE,
          options = list(placeholder = "All studies")
        ),

        sliderInput(
          "ne_range", "Ne range filter",
          min = floor(min(dat0$ne, na.rm = TRUE)),
          max = ceiling(max(dat0$ne, na.rm = TRUE)),
          value = c(
            floor(min(dat0$ne, na.rm = TRUE)),
            ceiling(max(dat0$ne, na.rm = TRUE))
          ),
          step = 1
        ),

        checkboxInput("show_legend", "Show legend", TRUE),
        helpText("Map points represent communities. Click a point to explore species-level Ne and metadata.")
      ),

      mainPanel(
        width = 9,
        leafletOutput("map", height = "650px"),
        br(),
        uiOutput("summary_line")
      )
    )
  ),

  tabPanel("Table", DTOutput("tbl"))
)

# ---- Server ----
server <- function(input, output, session) {

  filtered_long <- reactive({
    d <- dat0

    if (!is.null(input$community) && length(input$community) > 0)
      d <- d %>% filter(community_name %in% input$community)

    if (!is.null(input$species) && length(input$species) > 0)
      d <- d %>% filter(species %in% input$species)

    if (!is.null(input$taxon_group) && length(input$taxon_group) > 0)
      d <- d %>% filter(taxon_group %in% input$taxon_group)

    if (!is.null(input$marker) && length(input$marker) > 0)
      d <- d %>% filter(marker %in% input$marker)

    if (!is.null(input$study_id) && length(input$study_id) > 0)
      d <- d %>% filter(study_id %in% input$study_id)

    d %>% filter(ne >= input$ne_range[1], ne <= input$ne_range[2])
  })

  community_points <- reactive({
    d <- filtered_long()
    d %>%
      group_by(community_id, community_name, region, lat, lon) %>%
      summarise(
        n_species = n_distinct(species),
        n_groups  = n_distinct(taxon_group),
        ne_mean   = mean(ne, na.rm = TRUE),
        ne_median = median(ne, na.rm = TRUE),
        .groups = "drop"
      )
  })

  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      onRender("
        function(el, x) {
          var map = this;

          function bindPopupDropdown(popupEl) {
            if (!popupEl) return;

            var sel = popupEl.querySelector('select.sp-select');
            if (!sel) return;

            // Prevent rebinding if popup reopens
            if (sel.dataset.bound === '1') return;
            sel.dataset.bound = '1';

            var containerId = sel.getAttribute('data-container');
            // containerId is already safe (based on community_id), so CSS.escape is optional
            var cont = popupEl.querySelector('#' + containerId);
            if (!cont) return;

            function update() {
              var v = sel.value || '';
              var panels = cont.querySelectorAll('.sp-panel');
              panels.forEach(function(p){ p.style.display = 'none'; });

              if (v) {
                var match = cont.querySelector('.sp-panel[data-token=\"' + v.replace(/\"/g,'\\\\\"') + '\"]');
                if (match) match.style.display = 'block';
              }
            }

            sel.addEventListener('change', update);
            update();
          }

          map.on('popupopen', function(e) {
            bindPopupDropdown(e.popup && e.popup.getElement ? e.popup.getElement() : null);
          });
        }
      ")
  })

  observe({
  pts   <- community_points()
  dlong <- filtered_long()

  # Always clear first
  leafletProxy("map", session) %>% clearMarkers() %>% clearControls()

  # If nothing to show, stop here
  if (nrow(pts) == 0) return(NULL)

  pal <- colorNumeric(palette = "viridis", domain = pts$ne_mean)

  leafletProxy("map", session) %>%
    addCircleMarkers(
      data = pts,
      lng = ~lon, lat = ~lat,
      radius = 6,
      stroke = TRUE, weight = 0.7,
      fillOpacity = 0.9,
      color = ~pal(ne_mean),
      popup = purrr::map(pts$community_id, function(cid) {
        df_comm <- dlong %>% filter(community_id == cid)
        build_community_popup(df_comm)
      })
    ) %>%
    fitBounds(
      lng1 = min(pts$lon, na.rm = TRUE),
      lat1 = min(pts$lat, na.rm = TRUE),
      lng2 = max(pts$lon, na.rm = TRUE),
      lat2 = max(pts$lat, na.rm = TRUE)
    ) %>%
    {
      if (isTRUE(input$show_legend)) {
        addLegend(
          ., position = "bottomright",
          pal = pal, values = pts$ne_mean,
          title = "Community mean Ne", opacity = 1
        )
      } else .
    }
})

  output$tbl <- renderDT({
    d <- filtered_long() %>% arrange(community_name, taxon_group, species)
    datatable(
      d,
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = 25, scrollX = TRUE)
    )
  })

  output$summary_line <- renderUI({
    d <- filtered_long()
    pts <- community_points()
    tags$p(
      tags$b("Showing: "),
      nrow(pts), " communities | ",
      n_distinct(d$species), " species | ",
      n_distinct(d$taxon_group), " taxonomic groups | ",
      n_distinct(d$study_id), " studies"
    )
  })
}

shinyApp(ui, server)