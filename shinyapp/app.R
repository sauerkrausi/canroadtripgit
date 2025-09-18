library(shiny)
library(mapgl)
library(sf)
library(readr)
library(dplyr)

# --- data ---
stops_df <- read_csv(
  "/Users/felix/GoogleDrive/Canada/canroadtripgit/data/stops.csv",
  show_col_types = FALSE
) |>
  arrange(stop_order)
stops_df$section_id <- paste0(stops_df$stop_id, "_", stops_df$stop_order)
stops_sf <- st_as_sf(stops_df, coords = c("lon", "lat"), crs = 4326)

route_sf <- if (nrow(stops_sf) >= 2) {
  st_sf(geometry = st_sfc(st_linestring(st_coordinates(stops_sf)), crs = 4326))
} else st_sf(geometry = st_sfc(), crs = 4326)

# precompute coords for speed
coords_mat <- sf::st_coordinates(stops_sf)[, 1:2]

ui <- fluidPage(
  story_maplibre(
    map_id = "map",
    sections = setNames(
      lapply(seq_len(nrow(stops_df)), function(i) {
        story_section(
          title = paste0(
            stops_df$stop_order[i],
            ". ",
            stops_df$title[i],
            if (!is.na(stops_df$subtitle[i]) && nchar(stops_df$subtitle[i]) > 0)
              paste0(" — ", stops_df$subtitle[i]) else ""
          ),
          content = list(
            if (!is.na(stops_df$blurb_text[i])) tags$p(stops_df$blurb_text[i]),
            if (
              !is.na(stops_df$image_url[i]) && nchar(stops_df$image_url[i]) > 0
            )
              tags$img(
                src = stops_df$image_url[i],
                loading = "lazy",
                style = "width:100%;border-radius:8px;"
              )
          )
        )
      }),
      stops_df$section_id
    )
  )
)

server <- function(input, output, session) {
  output$map <- renderMaplibre({
    m <- maplibre(
      scrollZoom = FALSE,
      style = "https://basemaps.cartocdn.com/gl/positron-gl-style/style.json"
    )

    if (nrow(route_sf) > 0) {
      m <- add_source(m, "route", route_sf)
      m <- add_line_layer(
        m,
        "route_line",
        "route",
        line_color = "#D22",
        line_width = 4
      )
    }

    m <- add_source(m, "stops", stops_sf)
    m <- add_circle_layer(
      m,
      "stops_all",
      "stops",
      circle_radius = 5,
      circle_color = "#0066CC",
      circle_stroke_color = "#FFFFFF",
      circle_stroke_width = 2
    )
    m <- add_circle_layer(
      m,
      "stop_highlight",
      "stops",
      circle_radius = 10,
      circle_color = "#FFCC00",
      circle_stroke_color = "#333333",
      circle_stroke_width = 2
    )
    m <- set_filter(m, "stop_highlight", list("==", "section_id", "__none__"))
    m <- fit_bounds(m, stops_sf, padding = 40)
    m
  })

  invisible(lapply(seq_len(nrow(stops_df)), function(i) {
    on_section("map", stops_df$section_id[i], {
      maplibre_proxy("map") |>
        set_filter(
          "stop_highlight",
          list("==", "section_id", stops_df$section_id[i])
        ) |>
        fly_to(center = as.numeric(coords_mat[i, ]), zoom = 7.8, duration = 800)
    })
  }))
}

shinyApp(ui, server)
