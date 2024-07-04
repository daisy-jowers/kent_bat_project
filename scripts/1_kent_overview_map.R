##########################################
########     KENT OVERVIEW MAP    ########
##########################################

# this script creates the overview map of Kent included in the introduction of
# the project.

source("./scripts/0_install_packages.R")
# load Kent, Medway, and district boundaries

kent_districts = c("Ashford", "Canterbury", "Dartford", "Dover", "Gravesham",
                   "Maidstone", "Sevenoaks", "Folkestone and Hythe", "Swale",
                   "Thanet", "Tonbridge and Malling", "Tunbridge Wells",
                   "Medway")

county_boundaries <- sf::st_read("./data/county_boundaries_2023_bfe/CTYUA_MAY_2023_UK_BFE.shp") %>%
  filter(CTYUA23NM %in% c("Kent", "Medway"))
district_boundaries <- sf::st_read("./data/kent_overview_map/district_boundaries_2023_BFE/LAD_MAY_2023_UK_BFE_V2.shp") %>%
  filter(LAD23NM %in% kent_districts)
greenbelt <- sf::st_read("./data/kent_overview_map/greenbelt_22-23/England_Green_Belt_2022_23_WGS84.shp") %>%
  sf::st_transform(crs = 27700) %>%
  filter(LAD_NM %in% kent_districts)
aonb <- sf::st_read("./data/kent_overview_map/aonb/Areas_of_Outstanding_Natural_Beauty_EnglandPolygon.shp")
kent_aonb <- sf::st_intersection(aonb, county_boundaries)


overview <- ggplot() +
  geom_sf(data = greenbelt, aes(), fill = "lightgreen", color = NA) +
  geom_sf(data = kent_aonb, aes(), fill = "darkgreen", color = NA, 
          alpha = 0.7) +
  geom_sf(data = district_boundaries, aes(), fill = NA, color = "grey",
          linetype = "dashed") +
  geom_sf(data = county_boundaries, aes(), fill = NA) +
  geom_sf(data = buildings_polygons, aes(fill = built_period), color = NA) +
  scale_fill_manual(values = c("pre-2000s" = "black", 
                               "2000-2004" = "darkslategray", 
                               "2005-2009" =  "darkslategray4", 
                               "2010-2014" = "darkslategray3", 
                               "2015-2019" = "darkslategray2", 
                               "2020-2024" = "darkslategray1")) +
  theme_classic()

ggplotly(overview)
