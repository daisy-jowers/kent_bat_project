###########################################
######## BOUNDARIES AND GRID REFS #########
###########################################

# this script creates polygons to extract further data. First it reads in the
# county boundaries of Kent and Medway, before creating a 10km buffer,
# intersecting this with national grid references, and saving the county border,
# county border with buffer, and each grid square seperately (to allow for
# polygons to be uploaded to EDINA OS digimap portal)

source("./scripts/0_install_packages.R")

county_boundaries <- sf::st_read("./data/county_boundaries_2023_bfe/CTYUA_MAY_2023_UK_BFE.shp")

county_boundaries %<>%
  filter(CTYUA23NM %in% c("Kent", "Medway"))

# create spatial object with 10km buffer around Kent and Medway, to allow enough
# environmental data to be extracted

county_boundary_10km_buffer <- county_boundaries %>%
  sf::st_combine() %>%
  sf::st_buffer(10000)

# read in dataset of national grid references
grid_squares <- sf::st_read("./data/national_grid_squares/gb-grids_5564040/10km_grid_region.shp") %>%
  sf::st_transform(crs = 27700)
grid_squares %<>%
  mutate(intersects = sf::st_intersects(., county_boundary_10km_buffer, sparse = FALSE)) %>%
  filter(intersects == TRUE)

# clip each grid square to end at the 10km buffer (reduces data download
# quantity later)
grid_squares_buffer <- sf::st_intersection(grid_squares, county_boundary_10km_buffer)

# for each line of the 'grid_squares_buffer' save a new shapefile into the data
# folder. warning for dropping the 'intersects' column, but this is no longer 
# needed so okay.

for (i in 1:nrow(grid_squares_buffer)){
  tile <- grid_squares_buffer$TILE_NAME[i]
  tile_geometry <- grid_squares_buffer %>%
    filter(TILE_NAME == tile)
  outDir <- "./data/tile_boundaries/"
  dir.create(file.path(outDir, tile))
  file_name <- paste0(tile, ".shp")
  sf::st_write(tile_geometry, file.path(outDir, tile, file_name))
}


