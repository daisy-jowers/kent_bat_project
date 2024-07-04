#######################################
########   STANDARDISE OS     #########
#######################################

# This script reads in the combined lines and polygons files, adds creation a
# creation data column, expands the 'theme' column into booleans for each type,
# and for polygons data splits these into seperate geopackages to increase
# file manageability.

source("./scripts/0_install_packages.R")

# POLYGONS

polygons <- sf::st_read("./data/os_data/0_combined_polygons/combined_polygons.gpkg")

# check number of versions

# add column to give reason for entry to database
polygons %<>%
  mutate(reasonforchange_1 = str_extract(reasonforchange, "^\\w{1,}\\b"),
         changedate_1 = as.Date(str_extract(changedate, "^\\d{4}-\\d{2}-\\d{2}\\b")))


polygons %<>%
  mutate(built_period = case_when(between(changedate_1, as.Date("1970-01-01"),
                                          as.Date("1999-12-31")) ~
                                    "pre-2000s",
                                 between(changedate_1, as.Date("2000-01-01"),
                                                          as.Date("2004-12-31")) ~
                                   "2000-2004",
                                 between(changedate_1, as.Date("2005-01-01"),
                                                          as.Date("2009-12-31")) ~
                                   "2005-2009",
                                 between(changedate_1, as.Date("2010-01-01"),
                                                          as.Date("2014-12-31")) ~
                                   "2010-2014",
                                 between(changedate_1, as.Date("2015-01-01"),
                                                          as.Date("2019-12-31")) ~
                                   "2015-2019",
                                 between(changedate_1, as.Date("2020-01-01"),
                                                          as.Date("2024-12-31")) ~
                                   "2020-2024",
                                 .default = NA))

# filter for just buildings
buildings_polygons <- polygons %>%
  filter(str_detect(theme, "Buildings"))

# LINES

lines <- sf::st_Read("./data/os_data/0_combined_lines/combined_lines.gpkg")