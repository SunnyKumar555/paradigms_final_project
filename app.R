# =============================================================================
# Choosing a Hospital: prototype Shiny app
#
# The user picks a condition and a location. The app pings nearby hospitals
# and ranks them by an overall score for that condition, which combines
# mortality (weighted most heavily) with readmission and complication
# measures. Clicking a ping shows each measure separately, plus distance and
# estimated drive time.
#
# Packages needed:  install.packages(c("shiny", "leaflet"))
# Run it:           shiny::runApp("app.R")   (or click "Run App" in RStudio)
#
# DATA: this version uses SYNTHETIC data from generate_synthetic_data.R.
# To switch to the real data, save it as a CSV with these columns and change
# DATA_FILE below:
#   facility_id, facility_name, citytown, state, lat, lng,
#   measure_id, measure_name, score
# (long format: one row per hospital per measure)
# =============================================================================

library(shiny)
library(leaflet)

DATA_FILE <- "synthetic_hospitals.csv"

# -----------------------------------------------------------------------------
# Which measures belong to which condition
#
# type: "mortality", "readmission" or "complication"
# higher_is_better:
#   TODO: CHECK THE DATA DICTIONARY. This prototype assumes a HIGHER score is
#   better for every measure. But all of these measures count bad outcomes
#   (deaths, readmissions, return days, complications), so for most or all
#   of them LOWER is probably better. Set each one to FALSE once confirmed.
# -----------------------------------------------------------------------------
MEASURES <- read.csv(
  text = "
condition|measure_id|type|higher_is_better
Heart attack|MORT_30_AMI|mortality|TRUE
Heart attack|READM_30_AMI|readmission|TRUE
Heart attack|EDAC_30_AMI|readmission|TRUE
Heart failure|MORT_30_HF|mortality|TRUE
Heart failure|READM_30_HF|readmission|TRUE
Heart failure|EDAC_30_HF|readmission|TRUE
Pneumonia|MORT_30_PN|mortality|TRUE
Pneumonia|READM_30_PN|readmission|TRUE
COPD|MORT_30_COPD|mortality|TRUE
COPD|READM_30_COPD|readmission|TRUE
Heart bypass surgery (CABG)|MORT_30_CABG|mortality|TRUE
Hip or knee replacement|COMP_HIP_KNEE|complication|TRUE
Hip or knee replacement|PSI_12|complication|TRUE
Cancer (outpatient chemotherapy)|OP_35_ADM|readmission|TRUE
General hospital stay|Hybrid_HWM|mortality|TRUE
General hospital stay|Hybrid_HWR|readmission|TRUE
General hospital stay|PSI_03|complication|TRUE
General hospital stay|PSI_12|complication|TRUE
",
  sep = "|",
  strip.white = TRUE,
  stringsAsFactors = FALSE
)

# How much of the overall score comes from mortality measures. The rest is
# split evenly among the condition's other measures. If a hospital doesn't
# report any mortality measure (or any other measure), the measures it does
# report make up the whole score.
MORTALITY_SHARE <- 0.6

# -----------------------------------------------------------------------------
# Load hospital data
# -----------------------------------------------------------------------------
raw <- read.csv(
  "data/synthetic_hospitals.csv",
  colClasses = c(facility_id = "character"),
  stringsAsFactors = FALSE
)

# Missing scores stay missing (NA), never zero. Text like "Not Available"
# becomes NA too. Rows without coordinates can't be mapped, so they're dropped.
raw$score <- suppressWarnings(as.numeric(raw$score))
raw <- raw[!is.na(raw$lat) & !is.na(raw$lng), ]

# One row per hospital (name and location)
hospitals <- unique(raw[, c(
  "facility_id",
  "facility_name",
  "citytown",
  "state",
  "lat",
  "lng"
)])
hospitals <- hospitals[!duplicated(hospitals$facility_id), ]

# Only keep conditions whose measures are actually in the data
MEASURES <- MEASURES[MEASURES$measure_id %in% raw$measure_id, ]
MEASURES$measure_name <- raw$measure_name[match(
  MEASURES$measure_id,
  raw$measure_id
)]
conditions <- unique(MEASURES$condition)

# -----------------------------------------------------------------------------
# Overall score for every hospital and condition (computed once at startup)
#
# 1. Put each measure on the same scale: how many standard deviations a
#    hospital is from the national average, flipped where needed so that
#    positive always means better.
# 2. Average a hospital's measures for the condition, weighting mortality by
#    MORTALITY_SHARE.
# 3. Turn that into a 0-100 percentile among all US hospitals with a score
#    for the condition (100 = best).
# -----------------------------------------------------------------------------
measure_stats <- do.call(
  rbind,
  lapply(unique(MEASURES$measure_id), function(id) {
    x <- raw$score[raw$measure_id == id]
    data.frame(
      measure_id = id,
      us_avg = mean(x, na.rm = TRUE),
      us_sd = sd(x, na.rm = TRUE)
    )
  })
)

compute_condition_scores <- function(cond) {
  m <- MEASURES[MEASURES$condition == cond, ]
  d <- raw[
    raw$measure_id %in% m$measure_id & !is.na(raw$score),
    c("facility_id", "measure_id", "score")
  ]
  if (nrow(d) == 0) {
    return(NULL)
  }

  info <- m[match(d$measure_id, m$measure_id), ]
  st <- measure_stats[match(d$measure_id, measure_stats$measure_id), ]
  d$z <- (d$score - st$us_avg) / st$us_sd
  d$z[is.na(d$z)] <- 0 # measure with no variation
  d$z <- ifelse(info$higher_is_better, d$z, -d$z)
  d$is_mort <- info$type == "mortality"

  per_hosp <- lapply(split(d, d$facility_id), function(h) {
    n_mort <- sum(h$is_mort)
    n_other <- sum(!h$is_mort)
    w <- if (n_mort > 0 && n_other > 0) {
      ifelse(
        h$is_mort,
        MORTALITY_SHARE / n_mort,
        (1 - MORTALITY_SHARE) / n_other
      )
    } else {
      rep(1 / nrow(h), nrow(h))
    }
    data.frame(
      facility_id = h$facility_id[1],
      combined = sum(w * h$z),
      n_reported = nrow(h),
      has_mortality = n_mort > 0
    )
  })
  out <- do.call(rbind, per_hosp)
  out$overall <- round(100 * (rank(out$combined) - 0.5) / nrow(out))
  out$condition <- cond
  out
}
condition_scores <- do.call(rbind, lapply(conditions, compute_condition_scores))

# -----------------------------------------------------------------------------
# Distance and drive time
# -----------------------------------------------------------------------------
miles_between <- function(lat1, lng1, lat2, lng2) {
  to_rad <- pi / 180
  dlat <- (lat2 - lat1) * to_rad
  dlng <- (lng2 - lng1) * to_rad
  a <- sin(dlat / 2)^2 +
    cos(lat1 * to_rad) * cos(lat2 * to_rad) * sin(dlng / 2)^2
  3958.8 * 2 * atan2(sqrt(a), sqrt(1 - a))
}

# ROUGH drive-time estimate: roads are ~30% longer than a straight line, at
# ~40 mph on average. For real driving times, the `osrm` package can query the
# free OSRM routing service (no API key), e.g. osrm::osrmTable(src, dst).
# It needs internet access and has rate limits, so only call it for the
# handful of hospitals being shown.
est_drive_minutes <- function(miles) round(miles * 1.3 / 40 * 60)

format_minutes <- function(m) {
  ifelse(m < 60, paste(m, "min"), sprintf("%d hr %d min", m %/% 60, m %% 60))
}

# -----------------------------------------------------------------------------
# Map pins: drawn as small SVG images so they're large, numbered and colored
# -----------------------------------------------------------------------------
svg_url <- function(svg) {
  paste0("data:image/svg+xml;charset=UTF-8,", URLencode(svg, reserved = TRUE))
}

pin_url <- function(fill, label, best = FALSE) {
  stroke <- if (best) "#000000" else "#ffffff"
  sw <- if (best) 3 else 2
  mapply(
    function(f, l) {
      svg_url(sprintf(
        "<svg xmlns='http://www.w3.org/2000/svg' width='36' height='48' viewBox='0 0 36 48'>
       <path d='M18 2C9.2 2 2 9.2 2 18c0 11.6 16 28 16 28s16-16.4 16-28C34 9.2 26.8 2 18 2z'
             fill='%s' stroke='%s' stroke-width='%s'/>
       <circle cx='18' cy='18' r='10.5' fill='white'/>
       <text x='18' y='22.5' font-family='Arial,Helvetica,sans-serif' font-size='%s'
             font-weight='bold' text-anchor='middle' fill='#222'>%s</text>
     </svg>",
        f,
        stroke,
        sw,
        if (nchar(l) > 1) 11 else 13,
        l
      ))
    },
    fill,
    label,
    USE.NAMES = FALSE
  )
}

you_are_here_url <- svg_url(
  "<svg xmlns='http://www.w3.org/2000/svg' width='32' height='32' viewBox='0 0 32 32'>
     <circle cx='16' cy='16' r='15' fill='#1a73e8' fill-opacity='0.25'/>
     <circle cx='16' cy='16' r='8' fill='#1a73e8' stroke='white' stroke-width='3'/>
   </svg>"
)

# Overall score colors: red (worse) -> green (better)
score_pal <- colorNumeric(
  c("#c62828", "#ef6c00", "#f9a825", "#7cb342", "#2e7d32"),
  domain = c(0, 100),
  na.color = "#9e9e9e"
)

# -----------------------------------------------------------------------------
# State lookup for the location picker (built from R's built-in datasets)
# -----------------------------------------------------------------------------
states <- data.frame(
  name = state.name,
  abb = state.abb,
  lng = state.center$x,
  lat = state.center$y,
  area = state.area,
  stringsAsFactors = FALSE
)
states$zoom <- pmin(pmax(round(14.8 - 0.5 * log2(states$area)), 4), 9)
# R's built-in centers for Alaska and Hawaii are moved for inset maps
states[states$abb == "AK", c("lng", "lat", "zoom")] <- list(-150.0, 61.5, 5)
states[states$abb == "HI", c("lng", "lat", "zoom")] <- list(-157.9, 21.4, 8)
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
# JavaScript for the "Use my current location" button
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
    function(err) { Shiny.setInputValue('geo_error', err.message, {priority: 'event'}); },
    {timeout: 10000}
  );
}
"

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------
ui <- fillPage(
  title = "Choosing a Hospital",
  tags$head(
    tags$script(HTML(geo_js)),
    tags$style(HTML(
      "
      #panel {
        background: rgba(255,255,255,0.97); padding: 14px 16px;
        border-radius: 8px; box-shadow: 0 2px 10px rgba(0,0,0,0.25);
        width: 380px; max-height: calc(100vh - 24px); overflow-y: auto;
        font-size: 13px;
      }
      #panel h4 { margin: 0 0 6px 0; }
      #panel h5 { margin: 12px 0 4px 0; font-weight: 600; }
      #panel .form-group { margin-bottom: 8px; }
      #panel table { font-size: 12px; width: 100%; }
      #panel .demo-note { color: #a33; font-size: 11px; margin-top: 8px; }
      .status { color: #555; margin: 6px 0; }
      .modal-body .btn { width: 100%; margin-top: 6px; }
      .or-divider { text-align: center; color: #888; margin: 10px 0; }
      .hosp-popup { font-size: 13px; min-width: 280px; }
      .hosp-popup table { width: 100%; border-collapse: collapse; margin-top: 4px; }
      .hosp-popup th { font-size: 11px; color: #666; font-weight: normal; text-align: left; }
      .hosp-popup td { padding: 3px 4px 3px 0; border-top: 1px solid #eee; vertical-align: top; }
      .hosp-popup .num { text-align: right; white-space: nowrap; padding-left: 10px; }
      .hosp-popup .tag { font-size: 10px; color: #777; }
      .overall-badge { display: inline-block; color: white; font-weight: bold;
                       border-radius: 4px; padding: 1px 7px; }
    "
    ))
  ),

  leafletOutput("map", width = "100%", height = "100%"),

  absolutePanel(
    id = "panel",
    top = 12,
    right = 12,
    h4("Choosing a Hospital"),

    h5("1. Your location"),
    uiOutput("location_text"),
    actionButton("change_location", "Change location", class = "btn-sm"),
    actionButton("reset_view", "Show all of US", class = "btn-sm"),
    tags$div(
      tags$small(tags$em("You can also click anywhere on the map.")),
      style = "margin-top: 4px;"
    ),

    h5("2. Condition"),
    selectInput("condition", NULL, choices = conditions, width = "100%"),
    tags$small(
      style = "color:#666",
      sprintf(
        "Overall score (0-100, higher = better) combines the measures below, with mortality counting for %d%%.",
        round(100 * MORTALITY_SHARE)
      )
    ),
    uiOutput("condition_measures"),

    h5("3. Which hospitals to show"),
    sliderInput(
      "max_miles",
      "Maximum distance (miles)",
      min = 5,
      max = 300,
      value = 50,
      step = 5,
      width = "100%"
    ),
    sliderInput(
      "n_show",
      "Number of hospitals",
      min = 1,
      max = 25,
      value = 5,
      step = 1,
      width = "100%"
    ),
    radioButtons(
      "sort_by",
      "Pick hospitals by",
      inline = TRUE,
      choices = c("Best overall score" = "score", "Closest" = "distance")
    ),

    h5("Results"),
    uiOutput("status"),
    tableOutput("results_table"),
    tags$div(
      class = "demo-note",
      "Prototype using SYNTHETIC data: hospitals and scores are not real."
    )
  )
)

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------
server <- function(input, output, session) {
  # The user's chosen location: list(lat, lng, label, zoom). NULL until picked.
  user_location <- reactiveVal(NULL)

  # ---- Starting map --------------------------------------------------------
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 3)) |>
      addTiles() |> # OpenStreetMap background (no API key needed)
      setView(US_VIEW$lng, US_VIEW$lat, zoom = US_VIEW$zoom) |>
      addLegend(
        "bottomleft",
        pal = score_pal,
        values = c(0, 100),
        opacity = 1,
        title = "Overall score<br><small>(higher = better)<br>Grey = not reported</small>"
      )
  })

  # ---- Location prompt (on startup and on "Change location") ---------------
  location_modal <- function() {
    modalDialog(
      title = "Find a hospital for your condition",
      selectInput(
        "modal_condition",
        "What condition are you looking for care for?",
        choices = conditions,
        selected = isolate(input$condition),
        width = "100%"
      ),
      tags$hr(),
      selectizeInput(
        "state_pick",
        "Where are you located? Pick your state",
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

  observeEvent(TRUE, showModal(location_modal()), once = TRUE)
  observeEvent(input$change_location, showModal(location_modal()))

  # Copy the condition chosen in the pop-up to the side panel
  observeEvent(input$modal_condition, {
    updateSelectInput(session, "condition", selected = input$modal_condition)
  })

  # ---- Ways the user can set a location ------------------------------------
  observeEvent(input$go_state, {
    req(input$state_pick)
    s <- states[states$abb == input$state_pick, ]
    user_location(list(lat = s$lat, lng = s$lng, label = s$name, zoom = s$zoom))
    removeModal()
  })

  observeEvent(input$geo, {
    user_location(list(
      lat = input$geo$lat,
      lng = input$geo$lng,
      label = "Your current location",
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

  observeEvent(input$map_click, {
    click <- input$map_click
    user_location(list(
      lat = click$lat,
      lng = click$lng,
      label = sprintf("Map point (%.3f, %.3f)", click$lat, click$lng),
      zoom = max(input$map_zoom, 8)
    ))
  })

  output$location_text <- renderUI({
    loc <- user_location()
    if (is.null(loc)) {
      return(tags$p("No location selected yet."))
    }
    tags$p(tags$strong(loc$label))
  })

  output$condition_measures <- renderUI({
    m <- MEASURES[MEASURES$condition == input$condition, ]
    tags$ul(
      style = "margin: 4px 0 0 0; padding-left: 18px; color: #555; font-size: 12px;",
      lapply(seq_len(nrow(m)), function(i) {
        tags$li(
          m$measure_name[i],
          tags$span(
            class = "tag",
            style = "color:#999",
            paste0(" (", m$type[i], ")")
          )
        )
      })
    )
  })

  # ---- Find and rank hospitals ---------------------------------------------

  # Hospitals within the distance limit, with their overall score for the
  # chosen condition (NA if they report none of its measures)
  hospitals_in_range <- reactive({
    loc <- user_location()
    req(loc, input$condition)
    d <- hospitals
    d$miles <- miles_between(loc$lat, loc$lng, d$lat, d$lng)
    d <- d[d$miles <= input$max_miles, ]
    cs <- condition_scores[condition_scores$condition == input$condition, ]
    i <- match(d$facility_id, cs$facility_id)
    d$overall <- cs$overall[i]
    d$n_reported <- ifelse(is.na(i), 0, cs$n_reported[i])
    d
  })

  # The hospitals that get a pin, ranked by overall score
  results <- reactive({
    d <- hospitals_in_range()
    if (nrow(d) == 0) {
      return(d)
    }
    d <- d[order(is.na(d$overall), -d$overall, d$miles), ] # best first, ties -> closer
    d$rank <- ifelse(is.na(d$overall), NA, cumsum(!is.na(d$overall)))
    if (input$sort_by == "distance") {
      d <- d[order(d$miles), ]
    }
    d <- head(d, input$n_show)
    d$drive_min <- est_drive_minutes(d$miles)
    d
  })

  # ---- Popup: overall score plus each measure separately --------------------
  make_popup <- function(h, loc, cond, n_ranked) {
    m <- MEASURES[MEASURES$condition == cond, ]
    rows <- vapply(
      seq_len(nrow(m)),
      function(i) {
        s <- raw$score[
          raw$facility_id == h$facility_id & raw$measure_id == m$measure_id[i]
        ]
        s <- if (length(s) == 0 || is.na(s[1])) {
          "Not reported"
        } else {
          format(s[1], nsmall = 1)
        }
        avg <- measure_stats$us_avg[measure_stats$measure_id == m$measure_id[i]]
        sprintf(
          "<tr><td>%s <span class='tag'>(%s)</span></td><td class='num'><b>%s</b></td><td class='num'>%.1f</td></tr>",
          htmltools::htmlEscape(m$measure_name[i]),
          m$type[i],
          s,
          avg
        )
      },
      character(1)
    )

    overall_html <- if (is.na(h$overall)) {
      "<span class='overall-badge' style='background:#9e9e9e'>No score</span> (no measures reported)"
    } else {
      sprintf(
        "<span class='overall-badge' style='background:%s'>%d / 100</span> &nbsp;#%d of %d nearby",
        score_pal(h$overall),
        as.integer(h$overall),
        as.integer(h$rank),
        n_ranked
      )
    }

    sprintf(
      "<div class='hosp-popup'>
         <strong style='font-size:14px'>%s</strong><br>
         <span style='color:#666'>%s, %s</span>
         <hr style='margin:6px 0'>
         <b>%s</b> overall score:<br>%s
         <table><tr><th>Measure</th><th class='num'>This hospital</th><th class='num'>US avg</th></tr>%s</table>
         <hr style='margin:6px 0'>
         Distance: %.1f miles (straight line)<br>
         Driving time: about %s <small>(estimate)</small><br>
         <a href='%s' target='_blank'>Get directions &#8599;</a>
       </div>",
      htmltools::htmlEscape(h$facility_name),
      tools::toTitleCase(tolower(h$citytown)),
      h$state,
      htmltools::htmlEscape(cond),
      overall_html,
      paste(rows, collapse = ""),
      h$miles,
      format_minutes(h$drive_min),
      sprintf(
        "https://www.google.com/maps/dir/?api=1&origin=%f,%f&destination=%f,%f",
        loc$lat,
        loc$lng,
        h$lat,
        h$lng
      )
    )
  }

  # ---- Map: your location and hospital pins --------------------------------

  # When the location changes: move the marker and zoom to fit the results
  observeEvent(user_location(), {
    loc <- user_location()
    proxy <- leafletProxy("map") |>
      clearGroup("user_location") |>
      addMarkers(
        lng = loc$lng,
        lat = loc$lat,
        label = paste("You:", loc$label),
        group = "user_location",
        icon = icons(
          you_are_here_url,
          iconWidth = 32,
          iconHeight = 32,
          iconAnchorX = 16,
          iconAnchorY = 16
        )
      )
    d <- results()
    if (nrow(d) > 0) {
      proxy |>
        flyToBounds(
          min(d$lng, loc$lng),
          min(d$lat, loc$lat),
          max(d$lng, loc$lng),
          max(d$lat, loc$lat),
          options = list(padding = c(60, 60))
        )
    } else {
      proxy |> flyTo(loc$lng, loc$lat, zoom = loc$zoom)
    }
  })

  # Redraw the pins whenever the location, condition or a filter changes
  observe({
    proxy <- leafletProxy("map") |> clearGroup("hospitals")
    req(user_location())
    d <- results()
    if (nrow(d) == 0) {
      return()
    }
    loc <- user_location()
    n_ranked <- sum(!is.na(hospitals_in_range()$overall))

    d$popup <- vapply(
      seq_len(nrow(d)),
      function(i) {
        make_popup(d[i, ], loc, input$condition, n_ranked)
      },
      character(1)
    )
    d$label <- ifelse(is.na(d$rank), "-", d$rank)
    d$is_best <- !is.na(d$rank) & d$rank == 1

    add_pins <- function(proxy, p, best) {
      if (nrow(p) == 0) {
        return(proxy)
      }
      w <- if (best) 46 else 36
      h <- if (best) 61 else 48
      proxy |>
        addMarkers(
          lng = p$lng,
          lat = p$lat,
          layerId = p$facility_id,
          group = "hospitals",
          icon = icons(
            pin_url(score_pal(p$overall), p$label, best),
            iconWidth = w,
            iconHeight = h,
            iconAnchorX = w / 2,
            iconAnchorY = h,
            popupAnchorX = 0,
            popupAnchorY = -h + 4
          ),
          label = paste0(
            ifelse(is.na(p$rank), "", paste0("#", p$rank, " ")),
            p$facility_name,
            ifelse(
              is.na(p$overall),
              " (no score)",
              paste0(" - ", p$overall, "/100")
            )
          ),
          popup = p$popup,
          # The best hospital is drawn on top of any overlapping pins
          options = markerOptions(zIndexOffset = if (best) 1000 else 0)
        )
    }
    proxy |> add_pins(d[!d$is_best, ], FALSE) |> add_pins(d[d$is_best, ], TRUE)
  })

  observeEvent(input$reset_view, {
    leafletProxy("map") |> flyTo(US_VIEW$lng, US_VIEW$lat, zoom = US_VIEW$zoom)
  })

  # ---- Results in the side panel -------------------------------------------
  output$status <- renderUI({
    if (is.null(user_location())) {
      return(div(
        class = "status",
        "Choose a location to see nearby hospitals."
      ))
    }
    n_range <- nrow(hospitals_in_range())
    if (n_range == 0) {
      return(div(
        class = "status",
        sprintf(
          "No hospitals within %d miles. Try a larger distance.",
          input$max_miles
        )
      ))
    }
    div(
      class = "status",
      sprintf(
        "Showing %d of %d hospitals within %d miles. Click a pin for details.",
        nrow(results()),
        n_range,
        input$max_miles
      )
    )
  })

  output$results_table <- renderTable(
    {
      req(user_location())
      d <- results()
      req(nrow(d) > 0)
      data.frame(
        Rank = ifelse(is.na(d$rank), "-", d$rank),
        Hospital = d$facility_name,
        Overall = ifelse(is.na(d$overall), "N/A", paste0(d$overall, "/100")),
        Miles = round(d$miles, 1),
        Drive = format_minutes(d$drive_min),
        check.names = FALSE
      )
    },
    striped = TRUE,
    spacing = "xs",
    align = "clllr",
    digits = 1
  )
}

shinyApp(ui, server)
