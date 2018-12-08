###--------------------------------------------------
### Canada: quick and dirty map outline
###--------------------------------------------------

library(geojsonio)
library(rmapshaper)
library(rgdal)
library(tidyverse)
library(broom)
library(socviz)

### Get data from:
### http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-2011-eng.cfm

### ArcGIS .shp file,
### Census Divisions
### Cartographic Boundary File

### Expand the zip file into a directory in your working folder named 'data'


## Import the shapefile into R as SpatialPolygon Dataframe.
canada_raw <- readOGR(dsn = "data/gcd_000b11a_e", layer = "gcd_000b11a_e",
                      use_iconv=TRUE, encoding="CP1250")

## Convert to GeoJSON format
canada_raw_json <- geojson_json(canada_raw)

## Simplify the polygons
canada_raw_sim <- ms_simplify(canada_raw_json)

## Save the GeoJSON version
geojson_write(canada_raw_sim, file = "data/canada_cd_sim.geojson")

## Read in the GeoJSON version
## (Use this version from the start in future)
canada_gj<- readOGR(dsn = "data/canada_cd_sim.geojson", layer="OGRGeoJSON")


### Transform to Lambert Conformal Conic Projection
### (I *think* these coordinates are correct)
### https://www.statcan.gc.ca/pub/92-195-x/2011001/other-autre/mapproj-projcarte/m-c-eng.htm
canada_gj <- spTransform(canada_gj, CRS("+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))


### Convert to a tidy data frame
canada_cd <- tidy(canada_sim_gj2)

## Map theme
theme_map <- function(base_size=9, base_family="") {
    require(grid)
    theme_bw(base_size=base_size, base_family=base_family) %+replace%
        theme(axis.line=element_blank(),
              axis.text=element_blank(),
              axis.ticks=element_blank(),
              axis.title=element_blank(),
              panel.background=element_blank(),
              panel.border=element_blank(),
              panel.grid=element_blank(),
              panel.spacing=unit(0, "lines"),
              plot.background=element_blank(),
              legend.justification = c(0,0),
              legend.position = c(0,0)
              )
}

theme_set(theme_map())

## Long vector of repeated colors---just to fill in the map,
## for decoration only
map_colors <-  RColorBrewer::brewer.pal(8, "Pastel1")
map_colors <- rep(map_colors, 37)


## Draw the map
p + geom_polygon(mapping = aes(fill = id),
                 color="gray60",
                 size=0.2) +
    scale_fill_manual(values = map_colors) +
    guides(fill = FALSE)
