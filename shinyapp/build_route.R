# build_route.R
library(readr)
library(dplyr)
library(sf)
library(osrm)

options(osrm.server = "https://router.project-osrm.org/", osrm.profile = "car")

stops <- read_csv("data/stops.csv", show_col_types = FALSE) |>
  arrange(stop_order)
stops_sf <- st_as_sf(stops, coords = c("lon", "lat"), crs = 4326)

# adjacent pairs
idx <- cbind(1:(nrow(stops_sf) - 1), 2:nrow(stops_sf))
pts <- data.frame(
  id = stops$stop_id,
  lon = st_coordinates(stops_sf)[, 1],
  lat = st_coordinates(stops_sf)[, 2]
)

segs <- lapply(seq_len(nrow(idx)), function(k) {
  i <- idx[k, 1]
  j <- idx[k, 2]
  Sys.sleep(0.3) # be kind to the public server
  osrmRoute(
    src = pts[i, c("id", "lon", "lat")],
    dst = pts[j, c("id", "lon", "lat")],
    overview = "full",
    returnclass = "sf"
  )
})

seg_sf <- do.call(rbind, segs)
route_geom <- st_line_merge(st_union(st_geometry(seg_sf)))
route_sf <- st_sf(geometry = route_geom, crs = 4326)

st_write(route_sf, "data/route_line.geojson", delete_dsn = TRUE)
message("Wrote data/route_line.geojson")
