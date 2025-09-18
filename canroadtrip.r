# app.R
library(shiny)
library(mapgl)
library(sf)

# demo data (replace with your CSV later)
stops_tbl <- data.frame(
  stop_id = c(
    "montreal",
    "ottawa",
    "thunderbay",
    "winnipeg",
    "regina",
    "vancouver"
  ),
  title = c(
    "Montreal",
    "Ottawa",
    "Thunder Bay",
    "Winnipeg",
    "Regina",
    "Vancouver"
  ),
  lon = c(-73.5673, -75.6972, -89.2477, -97.1384, -104.6189, -123.1207),
  lat = c(45.5019, 45.4215, 48.3809, 49.8951, 50.4452, 49.2827),
  stringsAsFactors = FALSE
)
stops_sf <- st_as_sf(stops_tbl, coords = c("lon", "lat"), crs = 4326)

# route from ordered stops
route_line <- st_sfc(st_linestring(st_coordinates(stops_sf)), crs = 4326)
route_sf <- st_sf(geometry = route_line)

# UI: story sections auto-built from stops
ui <- fluidPage(
  story_maplibre(
    map_id = "map",
    sections = setNames(
      lapply(seq_len(nrow(stops_tbl)), function(i) {
        story_section(
          title = paste0(i, ". ", stops_tbl$title[i]),
          content = list(tags$p(paste("Stop", i)))
        )
      }),
      stops_tbl$stop_id
    )
  )
)

server <- function(input, output, session) {
  # base map + red route + blue stops + yellow highlight layer
  output$map <- renderMaplibre({
    maplibre() |>
      add_source("route", route_sf) |>
      add_line_layer(
        "route_line",
        "route",
        line_color = "#D22",
        line_width = 4
      ) |>
      add_source("stops", stops_sf) |>
      add_circle_layer(
        "stops_all",
        "stops",
        circle_radius = 6,
        circle_color = "#0066CC",
        circle_stroke_color = "#FFFFFF",
        circle_stroke_width = 2
      ) |>
      add_circle_layer(
        "stop_highlight",
        "stops",
        circle_radius = 10,
        circle_color = "#FFCC00",
        circle_stroke_color = "#333333",
        circle_stroke_width = 2
      ) |>
      fit_bounds(stops_sf, padding = 50)
  })

  # on scroll: fly to each stop and highlight it
  lapply(seq_len(nrow(stops_tbl)), function(i) {
    on_section("map", stops_tbl$stop_id[i], {
      xy <- as.numeric(st_coordinates(stops_sf[i, ])[1, 1:2])
      maplibre_proxy("map") |>
        set_filter(
          "stop_highlight",
          list("==", "stop_id", stops_tbl$stop_id[i])
        ) |>
        fly_to(
          center = xy,
          zoom = 8.5,
          pitch = 45,
          bearing = -15,
          duration = 2200
        )
    })
  }) |>
    invisible()
}

shinyApp(ui, server)
