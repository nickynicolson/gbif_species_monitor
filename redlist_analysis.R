
# compare previous_data and new occurrence points on a map
# check for difference in EOO, AOO

# load libraries
library(rCAT)
library(dplyr)
library(sf)
library(leaflet)
library(htmlwidgets)

# step 1 previous data ----
# read in current data
previous_data <- readRDS("data/Pachypodium_rosulatum/previous_data.rds")

# run cleaning protocols
flag_errors <- LCr::flag_occs(previous_data)
clean_points <- LCr::clean_occs(flag_errors$flagged_data)

clean_points <- clean_points$clean_occs %>%
  filter(country == "Madagascar")

#reformat points
thepoints <- clean_points %>%
  dplyr::select(decimalLatitude, decimalLongitude) %>%
  dplyr::rename("long" = "decimalLongitude",
                "lat" = "decimalLatitude") %>%
  dplyr::filter(complete.cases(lat))


# project the points for correct area measurements
thepoints <- simProjWiz(thepoints)

#get the EOO area in km2 for these points
eoo_poly <- eoo(thepoints, returnV = "SF")
eoo_poly_geog <- st_transform(eoo_poly, 4326) #set to geogrpahic to view on map
eoo_val <- eoo(thepoints)

#get the AOO area
aoo_val <- aoo(thepoints)
aoo_grid <- aoo(thepoints, returnV = "SF", cellsize = 20000)
aoo_poly_geog <- st_transform(aoo_grid, 4326) #set to geogrpahic to view on map

# step 2 new data ----
# do the same for the new data
new_recs <- readRDS("data/Pachypodium_rosulatum/new_records_20250515_102643.rds")

# run cleaning protocols
flag_errors <- LCr::flag_occs(new_recs)
clean_points <- LCr::clean_occs(flag_errors$flagged_data)

clean_points <- clean_points$clean_occs %>%
  filter(country == "Madagascar")

#reformat points
thepoints <- clean_points %>%
  dplyr::select(decimalLatitude, decimalLongitude) %>%
  dplyr::rename("long" = "decimalLongitude",
                "lat" = "decimalLatitude") %>%
  dplyr::filter(complete.cases(lat))


# project the points for correct area measurements
thepoints <- simProjWiz(thepoints)

#get the EOO area in km2 for these points
new_eoo_poly <- eoo(thepoints, returnV = "SF")
new_eoo_poly_geog <- st_transform(new_eoo_poly, 4326) #set to geogrpahic to view on map
new_eoo_val <- eoo(thepoints)

#get the AOO area
new_aoo_val <- aoo(thepoints)
new_aoo_grid <- aoo(thepoints, returnV = "SF", cellsize = 20000)
new_aoo_poly_geog <- st_transform(new_aoo_grid, 4326) #set to geogrpahic to view on map


m <- leaflet() %>%
  leaflet::addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
  leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite")

m <- leaflet() %>%
  addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%

  # EOO Polygon
  addPolygons(
    data = eoo_poly_geog,
    color = "red",
    weight = 1,
    opacity = 1,
    dashArray = "3",
    fillOpacity = 0,
    group = "EOO"
  ) %>%

  # Old AOO Polygon
  addPolygons(
    data = aoo_poly_geog,
    fillColor = "red",
    weight = 1,
    opacity = 1,
    color = "black",
    dashArray = "3",
    fillOpacity = 0.3,
    group = "AOO (old)"
  ) %>%

  # New AOO Polygon
  addPolygons(
    data = new_aoo_poly_geog,
    fillColor = "limegreen",
    color = "darkgreen",
    weight = 3,
    fillOpacity = 1,
    opacity = 1,
    group = "AOO (new)"
  ) %>%

  # Layers Control
  addLayersControl(
    baseGroups = c("OpenStreetMap", "Satellite"),
    overlayGroups = c("EOO", "AOO (old)", "AOO (new)"),
    options = layersControlOptions(collapsed = FALSE)
  )
m

htmlwidgets::saveWidget(m, file = "map_test.html", selfcontained = TRUE)
