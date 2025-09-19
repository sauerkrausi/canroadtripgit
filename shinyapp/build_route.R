# shinyapp/build_route.R
library(readr)
library(dplyr)
library(sf)
library(osrm)

options(osrm.server = "https://router.project-osrm.org/", osrm.profile = "car")

# --- paths (absolute to avoid WD confusion) ---
stops_csv <- "/Users/felix/GoogleDrive/Canada/canroadtripgit/data/stops.csv"
out_geojson <- "/Users/felix/GoogleDrive/Canada/canroadtripgit/data/route_line.geojson"

# --- load stops ---
stops <- read_csv(stops_csv, show_col_types = FALSE) |> arrange(stop_order)
stops_sf <- st_as_sf(stops, coords = c("lon", "lat"), crs = 4326)
stopifnot(nrow(stops_sf) >= 2)

# OSRM works with single-point sf rows; add an id
stops_sf$id <- as.character(stops$stop_id)

# adjacent pairs
idx <- cbind(1:(nrow(stops_sf) - 1), 2:nrow(stops_sf))

# --- fetch segments with error handling ---
seg_list <- lapply(seq_len(nrow(idx)), function(k) {
  i <- idx[k, 1]
  j <- idx[k, 2]
  Sys.sleep(0.3) # be kind to public server
  tryCatch(
    osrmRoute(
      src = stops_sf[i, ],
      dst = stops_sf[j, ],
      overview = "full",
      returnclass = "sf"
    ),
    error = function(e) {
      message("Segment failed: ", stops_sf$id[i], " → ", stops_sf$id[j])
      NULL
    }
  )
})

# keep only successful segments
seg_list <- Filter(function(x) inherits(x, "sf") && nrow(x) > 0, seg_list)
if (length(seg_list) == 0)
  stop("No OSRM segments succeeded. Try again later or check connectivity.")

# combine to one (multi)line
seg_sf <- do.call(rbind, seg_list)
route_geom <- st_line_merge(st_union(st_geometry(seg_sf)))
route_sf <- st_sf(geometry = route_geom, crs = 4326)

# --- write safely to GeoJSON ---
# ensure parent exists
dir.create(dirname(out_geojson), showWarnings = FALSE, recursive = TRUE)

# unlink old file (Google Drive can block delete_dsn)
if (file.exists(out_geojson)) unlink(out_geojson, force = TRUE)

# write to temp, then move (works around Drive locks)
tmp <- tempfile(fileext = ".geojson")
st_write(route_sf, tmp, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
ok <- file.copy(tmp, out_geojson, overwrite = TRUE)
if (!ok) stop("Failed to copy GeoJSON to target path: ", out_geojson)
unlink(tmp)

message("Wrote: ", out_geojson)
