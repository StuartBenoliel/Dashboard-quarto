library(DBI)
library(duckdb)
library(sf)

# -----------------------------
# 1. Création base DuckDB
# -----------------------------
con <- dbConnect(duckdb(), "test.duckdb")

dbExecute(con, "INSTALL spatial;")
dbExecute(con, "LOAD spatial;")

# -----------------------------
# 2. Génération données test
# -----------------------------
set.seed(123)

n <- 200000   # teste d'abord avec 200k

df <- data.frame(
  id = 1:n,
  lon = runif(n, -5, 8),
  lat = runif(n, 42, 51)
)

points_sf <- st_as_sf(df, coords = c("lon","lat"), crs = 4326)

dbWriteTable(con, "points_raw", df, overwrite = TRUE)

dbExecute(con, "
  CREATE TABLE points AS
  SELECT
    id,
    ST_Point(lon, lat) AS geom
  FROM points_raw;
")

# -----------------------------
# 3. Génération tuiles zoom 5
# -----------------------------
dir.create("tiles", showWarnings = FALSE)

z <- 5
n_tiles <- 2^z

for (x in 0:(n_tiles-1)) {
  for (y in 0:(n_tiles-1)) {
    
    query <- "
    WITH
    bounds AS (
      SELECT ST_TileEnvelope(
             CAST(? AS INTEGER),
             CAST(? AS INTEGER),
             CAST(? AS INTEGER)) AS geom
    ),
    mvtgeom AS (
      SELECT
        id,
        ST_AsMVTGeom(
          ST_Transform(p.geom, 'EPSG:4326', 'EPSG:3857'),
          ST_Extent(ST_Transform(bounds.geom, 'EPSG:4326', 'EPSG:3857')),
          4096,
          256,
          true
        ) AS geom
      FROM points p, bounds
      WHERE ST_Intersects(
        ST_Transform(p.geom, 'EPSG:4326', 'EPSG:3857'),
        ST_Transform(bounds.geom,  'EPSG:4326', 'EPSG:3857')
      )
    )
    SELECT ST_AsMVT(mvtgeom, 'points', 4096, 'geom')
    FROM mvtgeom;
    "
    
    tile <- dbGetQuery(con, query, params = list(z, x, y))[[1]]
    
    if (!is.null(tile) && length(tile) == 1) {
      
      tile_raw <- tile[[1]]
      
      if (is.raw(tile_raw) && length(tile_raw) > 0) {
        
        dir.create(file.path("tiles", z, x),
                   recursive = TRUE,
                   showWarnings = FALSE)
        
        writeBin(tile_raw,
                 file.path("tiles", z, x, paste0(y, ".pbf")))
      }
    }
  }
}

dbDisconnect(con)
