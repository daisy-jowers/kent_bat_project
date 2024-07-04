############################################
#########     COMBINE OS DATA      #########
############################################

# NOTE: this script will throw an error unless a local working directory
# is specified on line 88. this is due to writing permissions to the UCL
# OneDrive

# this script reads in the tiled Ordnance Mastermap data, combines
# the layers into a whole of Kent (+10km buffer) file

dataDir <- "./data/os_data"
kent_grid_refs <- list.files(dataDir)

# first tile is read in to create base for appending rows
# 'Topographicarea' layer gives polygons of buildings, roads, etc.
# 'Topographicline' layer gives bounding lines of above polygons AND
# line only geometries. Bounding lines are filtered out before combining
# datasets, otherwise these geometries would be duplicated.


# 'Land' classes are removed from both as these add substantial size to the
# datasets and won't be used in the next stage. Combined themes (such as
# 'Land, Buildings') are retained as these need to be disaggregated at the next
# stage


tile <- kent_grid_refs[1]
files <- list.files(file.path(dataDir, tile))
gpkg <- str_subset(files, "mastermap")
combined_polygons <- sf::st_read(paste0(file.path(dataDir, tile, gpkg),
                                        "/", gpkg, ".gpkg"),
                                 layer = "Topographicarea") %>%
  filter(!(theme == "Land"))
combined_lines <- sf::st_read(paste0(file.path(dataDir, tile, gpkg),
                                     "/", gpkg, ".gpkg"),
                              layer = "Topographicline") %>%
  filter(nonboundingline == "true",
         !(theme == "Land"))

# for loop reads in each tile and adds to overall dataset

for (i in 2:length(kent_grid_refs)){
  tile <- kent_grid_refs[i]
  files <- list.files(file.path(dataDir, tile))
  gpkg <- str_subset(files, "mastermap")
  subset_polygons <- sf::st_read(paste0(file.path(dataDir, tile, gpkg),
                                        "/", gpkg, ".gpkg"),
                                 layer = "Topographicarea") %>%
    filter(!(theme == "Land"))
  combined_polygons <- bind_rows(combined_polygons, subset_polygons,
                                 .id = "tile")
  subset_lines <- sf::st_read(paste0(file.path(dataDir, tile, gpkg),
                                     "/", gpkg, ".gpkg"),
                              layer = "Topographicline") %>%
    filter(nonboundingline == "true",
           !(theme == "Land"))
  combined_lines <- bind_rows(combined_lines, subset_lines,
                              .id = "tile")
}


# duplicate entries are removed. these occur when a line/polygon goes over two
# tiles

combined_lines$duplicate <- duplicated(combined_lines$fid)
combined_polygons$duplicate <- duplicated(combined_polygons$fid)


# rename existing 'fid; column as 'toid' (to match OS documentation)
# add new 'fid' line as GeoPackage driver requires integer values.

combined_lines %<>%
  filter(duplicate == FALSE) %>%
  rename("toid" = fid) %>%
  mutate(fid = row_number(), .before = tile)


combined_polygons %<>%
  filter(duplicate == FALSE) %>%
  rename("toid" = fid) %>%
  mutate(fid = row_number(), .before = tile)


# working directory has to be changed for this to write as it will not write
# directly into UCL OneDrive. this was done in console.

# LOCAL_WD <- " "
setwd(LOCAL_WD)
st_write(combined_lines, "./combined_lines/combined_lines.gpkg")
st_write(combined_polygons, "./combined_polygons.gpkg")
