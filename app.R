# =============================================================================
# Starter Shiny app: US map -> user picks a location -> map zooms there
#
# Packages needed:  install.packages(c("shiny", "leaflet"))
# Run it:           shiny::runApp("app.R")   (or click "Run App" in RStudio)
#
# The rest of the app should read the user's location from `user_location()`.
# Once your data is ready, look for the "ADD YOUR DATA HERE" comments.
# =============================================================================

library(shiny)
library(leaflet)

# -----------------------------------------------------------------------------
# Location lookup table (built from R's built-in `state.*` datasets, so it
# needs no downloads or extra packages)
# -----------------------------------------------------------------------------
states <- data.frame(
  name = state.name,
  abb = state.abb,
  lng = state.center$x,
  lat = state.center$y,
  area = state.area,
  stringsAsFactors = FALSE
)

# Zoom level based on state size: bigger states get zoomed out more
states$zoom <- pmin(pmax(round(14.8 - 0.5 * log2(states$area)), 4), 9)

# R's built-in centers for Alaska and Hawaii are moved for inset maps,
# so we replace them with their real locations
states[states$abb == "AK", c("lng", "lat", "zoom")] <- list(-152.0, 64.0, 4)
states[states$abb == "HI", c("lng", "lat", "zoom")] <- list(-157.5, 20.6, 7)

# Washington, DC isn't in state.name, so we add it ourselves
states <- rbind(
  states,
  data.frame(
    name = "District of Columbia",
    abb = "DC",
    lng = -77.03,
    lat = 38.90,
    area = 68,
    zoom = 11
  )
)
states <- states[order(states$name), ]

US_VIEW <- list(lng = -98.5, lat = 39.8, zoom = 4)

# -----------------------------------------------------------------------------
# JavaScript for the "Use my current location" button. It uses the browser's
# location access; the browser shows its own permission prompt. This works on
# localhost (RStudio) and on https sites like shinyapps.io.
# -----------------------------------------------------------------------------
geo_js <- "
function requestGeo() {
  if (!navigator.geolocation) {
    Shiny.setInputValue('geo_error', 'Your browser does not support location access.', {priority: 'event'});
    return;
  }
  navigator.geolocation.getCurrentPosition(
    function(pos) {
      Shiny.setInputValue('geo', {lat: pos.coords.latitude, lng: pos.coords.longitude},
                          {priority: 'event'});
    },
    function(err) {
      Shiny.setInputValue('geo_error', err.message, {priority: 'event'});
    },
    {timeout: 10000}
  );
}
"

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------
ui <- fillPage(
  tags$head(
    tags$script(HTML(geo_js)),
    tags$style(HTML(
      "
      #info-panel {
        background: rgba(255,255,255,0.95); padding: 12px 16px;
        border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.2);
        max-width: 280px; font-size: 14px;
      }
      #info-panel h4 { margin-top: 0; }
      .modal-body .btn { width: 100%; margin-top: 6px; }
      .or-divider { text-align: center; color: #888; margin: 10px 0; }
    "
    ))
  ),

  leafletOutput("map", width = "100%", height = "100%"),

  absolutePanel(
    id = "info-panel",
    top = 12,
    right = 12,
    draggable = TRUE,
    h4("Your location"),
    uiOutput("location_text"),
    actionButton("change_location", "Change location", class = "btn-sm"),
    actionButton("reset_view", "Show all of US", class = "btn-sm"),
    tags$p(
      tags$small(tags$em("Tip: you can also click anywhere on the map.")),
      style = "margin: 8px 0 0 0;"
    )
  )
)

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------
server <- function(input, output, session) {
  # The user's chosen location: list(lat, lng, label, state).
  # NULL until they pick one. Other parts of the app should use this.
  user_location <- reactiveVal(NULL)

  # ---- Starting map --------------------------------------------------------
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 3)) |>
      # Standard OpenStreetMap background (no API key needed)
      addTiles() |>
      setView(US_VIEW$lng, US_VIEW$lat, zoom = US_VIEW$zoom)
  })

  # ---- Location prompt (shown on startup and on "Change location") ---------
  location_modal <- function() {
    modalDialog(
      title = "Where are you located?",
      selectizeInput(
        "state_pick",
        "Pick your state",
        choices = c(
          "Choose a state..." = "",
          setNames(states$abb, states$name)
        ),
        width = "100%"
      ),
      actionButton("go_state", "Zoom to state", class = "btn-primary"),
      div(class = "or-divider", "— or —"),
      tags$button(
        id = "use_geo",
        type = "button",
        class = "btn btn-default",
        onclick = "requestGeo()",
        icon("location-crosshairs"),
        " Use my current location"
      ),
      footer = modalButton("Skip for now"),
      easyClose = TRUE
    )
  }

  # Show the prompt once when the app opens
  observeEvent(TRUE, showModal(location_modal()), once = TRUE)
  observeEvent(input$change_location, showModal(location_modal()))

  # ---- Ways the user can set a location ------------------------------------

  # 1) Picks a state from the dropdown
  observeEvent(input$go_state, {
    req(input$state_pick)
    s <- states[states$abb == input$state_pick, ]
    user_location(list(
      lat = s$lat,
      lng = s$lng,
      label = s$name,
      state = s$abb,
      zoom = s$zoom
    ))
    removeModal()
  })

  # 2) Uses the browser's current location
  observeEvent(input$geo, {
    user_location(list(
      lat = input$geo$lat,
      lng = input$geo$lng,
      label = "Your current location",
      state = NA,
      zoom = 10
    ))
    removeModal()
  })
  observeEvent(input$geo_error, {
    showNotification(
      paste(
        "Couldn't get your location:",
        input$geo_error,
        "Try picking a state instead."
      ),
      type = "warning",
      duration = 6
    )
  })

  # 3) Clicks on the map
  observeEvent(input$map_click, {
    click <- input$map_click
    user_location(list(
      lat = click$lat,
      lng = click$lng,
      label = sprintf("%.3f, %.3f", click$lat, click$lng),
      state = NA,
      zoom = max(input$map_zoom, 8)
    ))
  })

  # ---- Whenever the location changes: zoom in and drop a marker -------------
  observeEvent(user_location(), {
    loc <- user_location()
    leafletProxy("map") |>
      clearGroup("user_location") |>
      addMarkers(
        lng = loc$lng,
        lat = loc$lat,
        label = loc$label,
        group = "user_location"
      ) |>
      flyTo(loc$lng, loc$lat, zoom = loc$zoom)
  })

  observeEvent(input$reset_view, {
    leafletProxy("map") |>
      flyTo(US_VIEW$lng, US_VIEW$lat, zoom = US_VIEW$zoom)
  })

  output$location_text <- renderUI({
    loc <- user_location()
    if (is.null(loc)) {
      return(tags$p("No location selected yet."))
    }
    tags$p(
      tags$strong(loc$label),
      tags$br(),
      tags$small(sprintf("Lat %.3f, Lng %.3f", loc$lat, loc$lng))
    )
  })

  # ===========================================================================
  # ADD YOUR DATA HERE
  # Once your group settles on its data, add the map layers below. For example:
  #
  #   observeEvent(user_location(), {
  #     loc <- user_location()
  #     nearby <- my_data[my_data$state == loc$state, ]   # or filter by distance
  #     leafletProxy("map") |>
  #       clearGroup("data") |>
  #       addCircleMarkers(data = nearby, lng = ~lon, lat = ~lat,
  #                        popup = ~name, group = "data")
  #   })
  # ===========================================================================
}

shinyApp(ui, server)
