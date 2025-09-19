library(shiny)
library(mapgl)
library(sf)
library(readr)
library(dplyr)

# --- data: stops + route ---
stops_df <- readr::read_csv("data/stops.csv", show_col_types = FALSE) |>
  dplyr::arrange(stop_order) |>
  dplyr::mutate(
    stop_id = as.character(stop_id) # ensure id is character
  ) |>
  dplyr::mutate(
    dplyr::across(
      dplyr::any_of(c(
        "subtitle",
        "blurb_text",
        "image_file",
        "image_url",
        "album_url"
      )),
      ~ as.character(.x)
    )
  )

# ensure types (optional but safer)
stops_df <- readr::read_csv("data/stops.csv", show_col_types = FALSE) |>
  dplyr::mutate(
    stop_order = as.numeric(stop_order),
    lon = as.numeric(lon),
    lat = as.numeric(lat)
  ) |>
  dplyr::arrange(stop_order) |>
  dplyr::mutate(
    stop_id = as.character(stop_id),
    dplyr::across(
      dplyr::any_of(c(
        "subtitle",
        "blurb_text",
        "image_file",
        "image_url",
        "album_url"
      )),
      ~ as.character(.x)
    )
  )

# strip accidental "www/" prefix; expect "photos/foo.jpeg"
stops_df$image_file <- ifelse(
  is.na(stops_df$image_file),
  "",
  sub("^www/", "", stops_df$image_file)
)

# correct missing-file warning (look under ./www)
missing_local <- nzchar(stops_df$image_file) &
  !file.exists(file.path("www", stops_df$image_file))
if (any(missing_local)) {
  warning(
    "Missing local images: ",
    paste(stops_df$image_file[missing_local], collapse = ", ")
  )
}

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
          content = htmltools::tagList(
            if (!is.na(stops_df$blurb_text[i])) tags$p(stops_df$blurb_text[i]),

            # prefer local file under www/  -> use "photos/..." here
            if (
              "image_file" %in%
                names(stops_df) &&
                !is.na(stops_df$image_file[i]) &&
                nchar(stops_df$image_file[i]) > 0
            )
              tags$img(
                src = stops_df$image_file[i],
                loading = "lazy",
                style = "width:100%;border-radius:8px;"
              ),

            # fallback to direct URL
            if (
              ("image_url" %in% names(stops_df)) &&
                !is.na(stops_df$image_url[i]) &&
                nchar(stops_df$image_url[i]) > 0
            )
              tags$img(
                src = stops_df$image_url[i],
                loading = "lazy",
                style = "width:100%;border-radius:8px;"
              ),

            # optional album link
            if (
              ("album_url" %in% names(stops_df)) &&
                !is.na(stops_df$album_url[i]) &&
                nchar(stops_df$album_url[i]) > 0
            )
              tags$p(tags$a(
                href = stops_df$album_url[i],
                target = "_blank",
                "Open album"
              ))
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

    # red route if present
    if (exists("route_sf") && nrow(route_sf) > 0) {
      m <- add_source(m, "route", route_sf)
      m <- add_line_layer(
        m,
        "route_line",
        "route",
        line_color = "#E53935",
        line_width = 4
      ) # richer red
    }

    # stops: larger dots + halo + labels
    m <- add_source(m, "stops", stops_sf)
    m <- add_circle_layer(
      m,
      "stops_all",
      "stops",
      circle_radius = 7, # bigger
      circle_color = "#1E88E5", # vivid blue
      circle_stroke_color = "#FFFFFF",
      circle_stroke_width = 2
    )
    m <- add_circle_layer(
      m,
      "stop_highlight",
      "stops",
      circle_radius = 11,
      circle_color = "#FFEB3B", # yellow
      circle_stroke_color = "#263238",
      circle_stroke_width = 2
    )

    # simple text labels from the 'title' column
    m <- add_symbol_layer(
      m,
      "stop_labels",
      "stops",
      text_field = "{title}", # uses stops_sf$title
      text_size = 12,
      text_offset = c(0, 1.2)
    ) # just above the dot

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
