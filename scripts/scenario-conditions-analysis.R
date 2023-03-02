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
activity_center_layer="Community_Development_DBO_School_Locations"
ridership_layer = 'Remix_Stops_Wdb_Ridership'

block_census_data <- sf::st_read(base_gdb, 
                                 layer = census_layer)

bg_acs_wdb_data <- sf::st_read(base_gdb, 
                               layer = acs_wdb_layer)

bg_acs_data <- sf::st_read(base_gdb, 
                           layer = acs_layer)

ac_data <- sf::st_read(base_gdb, 
                       layer = activity_center_layer)

ridership_data <- sf::st_read(base_gdb, 
                              layer = ridership_layer)


gtfs_scenario <- read_gtfs("input/gtfs/GTFS_Remix.zip")

trips            <- gtfs_scenario$trips  
stop_times       <- gtfs_scenario$stop_times 
shapes           <- gtfs_scenario$shapes 
routes           <- gtfs_scenario$routes 
stops            <- gtfs_scenario$stops
stops_geom       <- gtfs_scenario$stops %>%
  filter(!is.na(stop_lon),!is.na(stop_lat)) %>%
  st_as_sf(coords = c("stop_lon", "stop_lat"),
           crs = coord_global) %>%
  st_transform(crs = coord_local)

routes <- routes %>% 
  mutate(routes_scenario_name = paste(route_short_name,route_long_name, sep = "_"))

scenario_rt_lst = routes$routes_scenario_name

# Isochrone ====================================================================

buff_dist_bus <- 5280/4 # Quarter mile
buff_dist_bus_km <- buff_dist_bus*0.0003048

## Find associated stops with each route
rt_stops <- trips %>% 
  left_join(stop_times %>% 
              select(trip_id, stop_id)) %>% 
  left_join(routes)

for(i in scenario_rt_lst){
  
  rt_stop_sel <- rt_stops %>% 
    filter(routes_scenario_name == i) %>% 
    distinct(stop_id) %>% 
    pull(stop_id) 
  
  stops_sel <- stops %>% 
    filter(stop_id %in% rt_stop_sel)

  # generate isochrone on bus stops
  stop_iso <- stops_sel %>%
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
  
  st_write(stop_iso_union, 
           paste0("output/Scenarios/",paste(i,"quarter_mi.shp", sep = "_")))
  print(i)
  
}

# Metrics ======================================================================
## 1. Population & Job =========================================================
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

pop_job_func <- function(pop_job, walkshed_shp, scenario_num){
  stop_iso_union_pop_job_inters <- st_intersection(pop_job,walkshed_shp) %>% 
    mutate(intersection_area = st_area(.), # area of each intersected block
           intersection_area_acre =units::set_units(intersection_area,acre),
           overlap_area = intersection_area_acre/block_area_acre, # % of overlapped area
           pop_adj = F2020CensusPop*overlap_area, # % of overlapped area * pop
           job_adj = Total_emp_ESRI_BA*overlap_area) %>%  # % of overlapped area * emp
    drop_units()
  
  pop_job_stat = stop_iso_union_pop_job_inters %>% 
    st_drop_geometry() %>% 
    select(-block_area,
           -intersection_area) %>% 
    mutate(scenario = scenario_num)
  
  return(pop_job_stat)
  
}

## 2. Demographics =============================================================

demo_bg <- bg_acs_wdb_data %>% 
  select(geoid,
         age_65_plus,
         age_15_to_17,
         poverty_line_200_plus,
         TPI_SUM,
         race_poc_total) %>% 
  st_transform(coord_global) %>% 
  mutate(bg_area = st_area(.), # calculate area of each block
         bg_area_acre = units::set_units(bg_area, acre)) # convert unit from m^2 to acre

demographic_func <- function(demo_bg, walkshed_shp, scenario_num){
  stop_iso_union_demo_inters <- st_intersection(demo_bg,walkshed_shp) %>% 
    mutate(intersection_area = st_area(.), # area of each intersected block
           intersection_area_acre =units::set_units(intersection_area,acre),
           overlap_area = intersection_area_acre/bg_area_acre, # % of overlapped area
           age_65_plus_adj = age_65_plus*overlap_area, # % of overlapped area * pop
           age_15_to_17_adj = age_15_to_17*overlap_area,
           poverty_line_200_plus_adj= poverty_line_200_plus*overlap_area,
           race_poc_adj = race_poc_total*overlap_area) %>% 
    drop_units() 
  
  
  demo_stat <- stop_iso_union_demo_inters %>% 
    st_drop_geometry() %>% 
    select(-bg_area,
           -intersection_area) %>% 
    mutate(scenario = scenario_num)
  
  return(demo_stat)
}

## 3.  Ridership  ==============================================================
## calculate ridership within walkshed

ridership_data <- ridership_data %>% 
  st_transform(coord_global)

ridership_func <- function(ridership_data, walkshed_shp, scenario_num){
  stop_iso_union_ridership_inters <- st_intersection(ridership_data, walkshed_shp)
  
  
  ridership_stat <- data.frame(
    "FR_ons_sum_wdb" = NA,
    "ER_ons_sum_wdb" = NA,
    "Total_ons_sum_wdb" = NA,
    "FR_ons_sum_walkshed" = NA,
    "ER_ons_sum_walkshed" = NA,
    "Total_ons_sum_walkshed" = NA
  )
  
  ridership_stat$FR_ons_sum_wdb = sum(ridership_data$FR_Ons, na.rm = TRUE)
  ridership_stat$ER_ons_sum_wdb = sum(ridership_data$ER_Ons, na.rm = TRUE)
  ridership_stat$Total_ons_sum_wdb = sum(ridership_data$Total_Ons, na.rm = TRUE)
  ridership_stat$FR_ons_sum_walkshed = sum(stop_iso_union_ridership_inters$FR_Ons, na.rm = TRUE)
  ridership_stat$ER_ons_sum_walkshed = sum(stop_iso_union_ridership_inters$ER_Ons, na.rm = TRUE)
  ridership_stat$Total_ons_sum_walkshed = sum(stop_iso_union_ridership_inters$Total_Ons, na.rm = TRUE)
  
  ridership_stat <- ridership_stat %>% 
    mutate(FR_ons_perc = FR_ons_sum_walkshed/FR_ons_sum_wdb,
           ER_ons_perc = ER_ons_sum_walkshed/ER_ons_sum_wdb,
           ER_ons_perc = ER_ons_sum_walkshed/ER_ons_sum_wdb) %>% 
    mutate(scenario = scenario_num)
  
  return(ridership_stat)
}

## 4. Activity Center  =========================================================
## calculate Activity Center within walkshed

ac_data <- ac_data %>% 
  st_transform(coord_global) %>% 
  filter(Include_Eval == 1)

ac_func <- function(ac_data, walkshed_shp, scenario_num){
  
  stop_iso_union_ac_inters <- st_intersection(ac_data, walkshed_shp)
  
  
  ac_stat <- data.frame(
    activity_center_num_wdb = NA,
    activity_center_num_walkshed = NA
  )
  
  ac_stat$activity_center_num_wdb = nrow(ac_data)
  ac_stat$activity_center_num_walkshed = nrow(stop_iso_union_ac_inters)
  
  ac_stat <- ac_stat %>% 
    mutate(activity_center_perc = activity_center_num_walkshed/activity_center_num_wdb,
           scenario = scenario_num)
  
  return(ac_stat)
}

# Scenarios ====================================================================
## Scenario 1 ===================================================================
cur_scenario <- "scenario_1"
walkshed_dir <- "output/woodburn_scenario_walkshed_quarter_mi/"

dir.create(file.path("output/scenarios", cur_scenario))
cur_dir <- paste0("output/scenarios/",cur_scenario)

# Read GTFS Line walkshed for current scenario
## 1_3_Scen Express - Pattern A
scen_express_1_3 <- st_read(paste0(walkshed_dir,
                                   "1_3_Scen_Express_quarter_mi.shp"))
## 1_Scen Employer Shuttle - Pattern A
scen_1_employer_shuttle <- st_read(paste0(walkshed_dir,
                                          "1_Scen_Employer Shuttle_quarter_mi.shp"))
##  1_Scen Local B - Pattern A
scen_local_b <- st_read(paste0(walkshed_dir,
                                "1_Scen_Local B_quarter_mi.shp"))

# combine all walkshed and union them
scen1_shp <- rbind(scen_express_1_3,
                   scen_1_employer_shuttle,
                   scen_local_b)

scen1_union <- st_union(scen1_shp) %>% 
  st_transform(coord_global) %>% 
  st_make_valid() # resolve duplicated vertex issue


# save unioned walkshed for current scenario
st_write(scen1_union, paste0(cur_dir,"/scenario_1_walkshed_quarter_mi.shp"))

pop_job_scenario_1 <- pop_job_func(pop_job, scen1_union, 1)
demo_scenario_1 <- demographic_func(demo_bg, scen1_union, 1)
ridership_scenario_1 <- ridership_func(ridership_data, scen1_union, 1)
ac_scenario_1 <- ac_func(ac_data, scen1_union, 1)


## Scenario 2 ===================================================================

cur_scenario <- "scenario_2"
walkshed_dir <- "output/woodburn_scenario_walkshed_quarter_mi/"

dir.create(file.path("output/scenarios", cur_scenario))
cur_dir <- paste0("output/scenarios/",cur_scenario)

# Read GTFS Line walkshed for current scenario
## 1_3_Scen Express - Pattern A
scen_route_c_2_4 <- st_read(paste0(walkshed_dir,
                                   "2_4_Scen_Route C_quarter_mi.shp"))
## 1_Scen Employer Shuttle - Pattern A
scen_route_d <- st_read(paste0(walkshed_dir,
                                          "2_Scen_Route D_quarter_mi.shp"))
##  1_Scen Local B - Pattern A
scen_shopper_shuttle <- st_read(paste0(walkshed_dir,
                               "2_Scen_Shopper Shuttle_quarter_mi.shp"))

# combine all walkshed and union them
scen2_shp <- rbind(scen_route_c_2_4,
                   scen_route_d,
                   scen_shopper_shuttle)

scen2_union <- st_union(scen2_shp) %>% 
  st_transform(coord_global) %>% 
  st_make_valid() # resolve duplicated vertex issue

# save unioned walkshed for current scenario
st_write(scen2_union, paste0(cur_dir,"/scenario_2_walkshed_quarter_mi.shp"))

pop_job_scenario_2 <- pop_job_func(pop_job, scen2_union, 2)
demo_scenario_2 <- demographic_func(demo_bg, scen2_union, 2)
ridership_scenario_2 <- ridership_func(ridership_data, scen2_union, 2)
ac_scenario_2 <- ac_func(ac_data, scen2_union, 2)


## Scenario 3 ===================================================================

cur_scenario <- "scenario_3"
walkshed_dir <- "output/woodburn_scenario_walkshed_quarter_mi/"

dir.create(file.path("output/scenarios", cur_scenario))
cur_dir <- paste0("output/scenarios/",cur_scenario)

# Read GTFS Line walkshed for current scenario
## 1_3_Scen Express - Pattern A
scen_express_1_3 <- st_read(paste0(walkshed_dir,
                                   "1_3_Scen_Express_quarter_mi.shp"))
## 1_Scen Employer Shuttle - Pattern A
scen_flex_route <- st_read(paste0(walkshed_dir,
                               "3_Scen_Flex Route_quarter_mi.shp"))
##  Flex Zone
flex_zone_3 <- sf::st_read(base_gdb,
                         layer = "Flex_Zones_20230222") %>% 
  st_transform(coord_global) %>% 
  select(Zone_Name, Shape) %>% 
  rename(FID = Zone_Name,
         geometry = Shape)

# combine all walkshed and union them
scen3_shp <- rbind(scen_express_1_3,
                   scen_flex_route,
                   flex_zone_3)

scen3_union <- st_union(scen3_shp) %>% 
  st_transform(coord_global) %>% 
  st_make_valid() # resolve duplicated vertex issue

# save unioned walkshed for current scenario
st_write(scen3_union, paste0(cur_dir,"/scenario_3_walkshed_quarter_mi.shp"))


pop_job_scenario_3 <- pop_job_func(pop_job, scen3_union, 3)
demo_scenario_3 <- demographic_func(demo_bg, scen3_union, 3)
ridership_scenario_3 <- ridership_func(ridership_data, scen3_union, 3)
ac_scenario_3 <- ac_func(ac_data, scen3_union, 3)


## Scenario 4 ===================================================================

cur_scenario <- "scenario_4"
walkshed_dir <- "output/existing/woodburn_scenario_walkshed_quarter_mi/"

dir.create(file.path("output/scenarios", cur_scenario))
cur_dir <- paste0("output/scenarios/",cur_scenario)

# Read GTFS Line walkshed for current scenario
## 1_3_Scen Express - Pattern A
scen_route_c_2_4 <- st_read(paste0(walkshed_dir,
                                   "2_4_Scen_Route C_quarter_mi.shp"))

##  Flex Zone
flex_zone_4 <- sf::st_read(base_gdb,
                           layer = "T4_scen_citywide_microtransit") %>% 
  st_transform(coord_global) %>% 
  select(FID_1, Shape) %>% 
  rename(FID = FID_1,
         geometry = Shape)

# combine all walkshed and union them
scen4_shp <- rbind(scen_route_c_2_4,
                   flex_zone_4)

scen4_union <- st_union(scen4_shp) %>% 
  st_transform(coord_global) %>% 
  st_make_valid() # resolve duplicated vertex issue

# save unioned walkshed for current scenario
st_write(scen4_union, paste0(cur_dir,"/scenario_4_walkshed_quarter_mi.shp"))


pop_job_scenario_4 <- pop_job_func(pop_job, scen4_union, 4)
demo_scenario_4 <- demographic_func(demo_bg, scen4_union, 4)
ridership_scenario_4 <- ridership_func(ridership_data, scen4_union, 4)
ac_scenario_4 <- ac_func(ac_data, scen4_union, 4)

## Save Result ==================================================================
library("openxlsx")


pop_job_scenario <- rbind(pop_job_scenario_1,
                          pop_job_scenario_2,
                          pop_job_scenario_3,
                          pop_job_scenario_4)


demo_scenario <- rbind(demo_scenario_1,
                       demo_scenario_2,
                       demo_scenario_3,
                       demo_scenario_4)

ridership_scenario <- rbind(ridership_scenario_1,
                            ridership_scenario_2,
                            ridership_scenario_3,
                            ridership_scenario_4)

ac_scenario <- rbind(ac_scenario_1,
                     ac_scenario_2,
                     ac_scenario_3,
                     ac_scenario_4)

ridership_ac <- cbind(ridership_scenario %>% 
                        select(-scenario), 
                      ac_scenario)


list_of_datasets <- list("pop_job" = pop_job_scenario,
                         "demographic_bg" = demo_scenario,
                         "ridership_ac" = ridership_ac)
write.xlsx(list_of_datasets, file = "output/scenarios/scenario_result.xlsx")


# Individual GTFS line =========================================================

## output summary table by individual line

gtfs_scenario <- read_gtfs("input/gtfs/GTFS_Remix.zip")

trips            <- gtfs_scenario$trips  
stop_times       <- gtfs_scenario$stop_times 
shapes           <- gtfs_scenario$shapes 
routes           <- gtfs_scenario$routes 
stops            <- gtfs_scenario$stops
stops_geom       <- gtfs_scenario$stops %>%
  filter(!is.na(stop_lon),!is.na(stop_lat)) %>%
  st_as_sf(coords = c("stop_lon", "stop_lat"),
           crs = coord_global) %>%
  st_transform(crs = coord_local)

routes <- routes %>% 
  mutate(routes_scenario_name = paste(route_short_name,route_long_name, sep = "_"))

scenario_rt_lst = routes$routes_scenario_name

walkshed_shps_dir = "output/scenarios/woodburn_scenario_walkshed_quarter_mi/"

pop_job_lst <- list()
demo_lst <- list()
ridership_lst <- list()
ac_lst <- list()
for(i in scenario_rt_lst){
  
  curr_shps_name = paste(i,"quarter_mi.shp", sep = "_")
  curr_shps = st_read(paste0(walkshed_shps_dir,curr_shps_name))
  
  pop_job_curr <- pop_job_func(pop_job, curr_shps, i)
  demo_curr <- demographic_func(demo_bg, curr_shps, i)
  ridership_curr <- ridership_func(ridership_data, curr_shps, i)
  ac_curr <- ac_func(ac_data, curr_shps, i)
  
  pop_job_lst[[i]] <- pop_job_curr
  demo_lst[[i]] <- demo_curr
  ridership_lst[[i]] <- ridership_curr
  ac_lst[[i]] <- ac_curr
  
  print(i)

}

pop_job_df <- do.call("rbind", pop_job_lst)
demo_df <- do.call("rbind", demo_lst)
ridership_df <- do.call("rbind", ridership_lst)
ac_df <- do.call("rbind", ac_lst)

## Save Result =================================================================


ridership_ac <- cbind(ridership_df %>% 
                        select(-scenario), 
                      ac_df)


list_of_datasets <- list("pop_job" = pop_job_df,
                         "demographic_bg" = demo_df,
                         "ridership_ac" = ridership_ac)
write.xlsx(list_of_datasets, file = "output/scenarios/by_individual_GTFS_line_result.xlsx")

# Testing -------------------------------
leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = bg_acs_wdb_data %>% 
                st_transform(coord_global)) %>% 
  addPolygons(data = curr_shps,
              fillColor = 'red')


