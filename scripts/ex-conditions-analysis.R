library(tidyverse)
library(tidycensus)
library(sf)
library(nntools)
library(leaflet)
library(tidytransit)
library(valhallr)
library(furrr)
library(tigris)
library(progressr)
library(scales)
library(rgdal)
library(units)
library(openxlsx)

future::plan(multisession)

# config =======================================================================
nn_valhalla_hostname <- "128.199.8.29"

#Coordinate systems
coord_global = 4326
coord_local = 2269 #see here: https://nelsonnygaard.shinyapps.io/coord-system-reference/

# Read data ====================================================================
# geodatabase directory
base_gdb <- "G:\\Current\\WOODBURN_Transit_Plan_2021.0328\\Analysis\\GDB\\WoodburnTDP.gdb"
# layer names in gdb
census_layer = "Blocks_Wdb_2020CensusPop_2019Jobs"
acs_wdb_layer = "BlockGroups_ACS2020_WdbArea"
acs_layer = "BlockGroups_ACS2020"

# database layer list
fc_list <- ogrListLayers(base_gdb)
print(fc_list)

# pop_job <- readOGR(dsn=base_gdb,
#                    layer=census_layer)

block_census_data <- sf::st_read(base_gdb, 
                            layer = census_layer)

bg_acs_wdb_data <- sf::st_read(base_gdb, 
                               layer = acs_wdb_layer)

bg_acs_data <- sf::st_read(base_gdb, 
                           layer = acs_layer)

ex_gtfs <- read_gtfs("input/gtfs/Woodburn_GTFS_Fall22_RtsStops.zip")

trips            <- ex_gtfs$trips  
stop_times       <- ex_gtfs$stop_times 
shapes           <- ex_gtfs$shapes 
routes           <- ex_gtfs$routes 
stops            <- ex_gtfs$stops
stops_geom       <- ex_gtfs$stops %>%
  filter(!is.na(stop_lon),!is.na(stop_lat)) %>%
  st_as_sf(coords = c("stop_lon", "stop_lat"),
           crs = coord_global) %>%
  st_transform(crs = coord_local)

# Isochrone ====================================================================

buff_dist_bus <- 5280/4 # Quarter mile
buff_dist_bus_km <- buff_dist_bus*0.0003048

# generate isochrone on bus stops
stop_iso <- stops %>%
  #Using purrr to enable iteration over each transit center
  mutate(isochrone_res = pmap(.l = list(stop_lon,stop_lat),
                              .f = function(lon,lat){
                                
                                isochrone(tibble(lon=lon,
                                                 lat=lat), 
                                          contours = c(buff_dist_bus_km),
                                          costing = "pedestrian",
                                          metric = "km",
                                          hostname = nn_valhalla_hostname)
                              
                              })) %>%
  unnest(isochrone_res) %>%
  st_as_sf()

# union isochrone
stop_iso_union = stop_iso %>% st_union()

# Population & Job =============================================================
## calculate population and job at block level which is covered by walkshed

# subset data from block census data
pop_job <- block_census_data %>% 
  select(GEOID20,
         F2020CensusPop,
         Total_emp_ESRI_BA) %>% 
  st_transform(coord_global) %>% 
  mutate(block_area = st_area(.), # calculate area of each block
         block_area_acre = units::set_units(block_area, acre)) # convert unit from m^2 to acre


# intersected area between blocks and walkshed
stop_iso_union_pop_job_inters <- st_intersection(pop_job,stop_iso_union) %>% 
  mutate(intersection_area = st_area(.), # area of each intersected block
         intersection_area_acre =units::set_units(intersection_area,acre),
         overlap_area = intersection_area_acre/block_area_acre, # % of overlapped area
         pop_adj = F2020CensusPop*overlap_area, # % of overlapped area * pop
         job_adj = Total_emp_ESRI_BA*overlap_area) %>%  # % of overlapped area * emp
  drop_units()


pop_job_stat = stop_iso_union_pop_job_inters %>% 
  st_drop_geometry() %>% 
  select(-block_area,
         -intersection_area)

write.xlsx(pop_job_stat, "output/pop_job_walkshed_quarter_mi_block.xlsx")


# Demographics =================================================================

demo_bg <- bg_acs_wdb_data %>% 
  select(geoid,
         age_65_plus,
         age_15_to_17,
         poverty_line_200_plus,
         TPI_SUM) %>% 
  st_transform(coord_global) %>% 
  mutate(bg_area = st_area(.), # calculate area of each block
         bg_area_acre = units::set_units(bg_area, acre)) # convert unit from m^2 to acre

stop_iso_union_demo_inters <- st_intersection(demo_bg,stop_iso_union) %>% 
  mutate(intersection_area = st_area(.), # area of each intersected block
         intersection_area_acre =units::set_units(intersection_area,acre),
         overlap_area = intersection_area_acre/bg_area_acre, # % of overlapped area
         age_65_plus_adj = age_65_plus*overlap_area, # % of overlapped area * pop
         age_15_to_17_adj = age_15_to_17*overlap_area,
         poverty_line_200_plus_adj= poverty_line_200_plus*overlap_area) %>% 
  drop_units() 


demo_stat <- stop_iso_union_demo_inters %>% 
  st_drop_geometry() %>% 
  select(-bg_area,
         -intersection_area)

write.xlsx(demo_stat, "output/demographic_walkshed_quarter_mi_bg.xlsx")





