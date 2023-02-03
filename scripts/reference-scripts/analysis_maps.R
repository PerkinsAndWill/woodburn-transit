## ---------------------------
##
## Script name: Pull in Census Data
##
## Purpose of script: 
##
## Author: Esther Needham
##
## Date Created: 2020-11-09
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

## set working directory here or make sure there is a .Rproj file

## ---------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation

## ---------------------------

## load up the packages we will need:  (uncomment as required)
library(tidyverse)
library(sf)
library(lwgeom)

source('reapportion_acs.R')

## ---------------------------

# Transit Shapes ----------------------------------------------------------
fr <- sf::st_read('../coic_dar_map/shapes/FR_concepts.shp') %>% st_make_valid() %>% st_transform(2922)
micro <- sf::st_read('../coic_dar_map/shapes/Microtransit_zones.shp') %>% st_make_valid() %>% st_transform(2922)

# Analysis shapes ---------------------------------------------------------
dar_trips <- sf::st_read('../coic_dar_map/shapes/Redmond_DAR_Dest_2019.shp') %>% st_make_valid() %>% st_transform(2922)
dar_dest <- sf::st_read('../coic_dar_map/shapes/Top_25_Destinations.shp') %>% st_make_valid() %>% st_transform(2922)
act_ctr <- sf::st_read('../coic_dar_map/shapes/activity_centers_edited_NN.shp') %>% 
  filter(City == 'Redmond') %>%# filter on city of Redmond
  st_make_valid() %>% st_transform(2922)
taz <<- sf::st_read(dsn = '../../Data/GDBs/PopEmpTAZ.gdb', layer = 'TAZ_2040') %>% st_transform(2922)
lehd <- sf::st_read(dsn = '../../Data/GDBs/RedmondDFR.gdb', layer = 'LEHD_block_points') %>% st_transform(2922)

# join lehd to blocks
lehdb <- bl %>% inner_join(lehd %>% st_drop_geometry(), by = c('GEOID10' = 'id'))

# Create Scenario Shapes --------------------------------------------------
hwy97 <- sf::st_read("../coic_dar_map/shapes/hwy97.shp") %>% st_zm() %>% st_transform(2922)

fl_a <- fr %>% filter(Name == 'NW CW') %>% st_transform(2922) %>% st_buffer(2640) %>% 
  st_intersection(taz) %>% st_union() %>% st_as_sf()
# cut fl_a by hwy 97
  x <- st_split(fl_a, hwy97)
  xpoly <- x %>% 
    st_collection_extract(c("POLYGON"))
  xpoly$ID <- 1:nrow(xpoly)
  xpoly$ID <- as.character(xpoly$ID)
  xpoly <- st_as_sf(xpoly)
  plot(xpoly)
fl_a <- xpoly %>% filter(!ID == "1") %>% select(-everything()) %>% st_as_sf()
st_write(fl_a, "output/shapes/flex_route_a.shp", append=FALSE)

fl_b <- fr %>% filter(Name == 'SE') %>% st_buffer(2640) %>% st_transform(2922) %>% 
  st_intersection(taz) %>% st_union()  %>% st_as_sf()
st_write(fl_b, "output/shapes/flex_route_b.shp", append=FALSE)
 
scen_1_buffer <- st_union(fl_a, fl_b) %>% st_as_sf() %>% st_make_valid()
st_write(scen_1_buffer, "output/shapes/scen_1_buffer.shp", append=FALSE)

fl_c <- fr %>% filter(Name == 'SW') %>% st_buffer(2640) %>% st_transform(2922) %>% 
  st_intersection(taz) %>% st_union() %>% st_as_sf()
st_write(fl_c, "output/shapes/flex_route_c.shp", append=FALSE)

scen_2_buffer <- st_union(scen_1_buffer, fl_c) %>% st_as_sf() %>% st_make_valid()
micro_union <- micro %>% st_union() %>% st_as_sf() %>% st_make_valid()
scen_2_buffer_micro <- st_union(scen_2_buffer, micro_union) %>% st_as_sf() %>% st_make_valid()
st_write(scen_2_buffer_micro, "output/shapes/scen_2_buffer.shp", append=FALSE)

fl_d <- fr %>% filter(Name == 'NE') %>% st_buffer(2640) %>% st_transform(2922) %>% 
  st_intersection(taz) %>% st_union() %>% st_as_sf()
st_write(fl_d, "output/shapes/flex_route_d.shp", append=FALSE)

fl_e <- fr %>% filter(Name == 'S') %>% st_buffer(2640) %>% st_transform(2922) %>% 
  st_intersection(taz) %>% st_union() %>% st_as_sf()
st_write(fl_d, "output/shapes/flex_route_d.shp", append=FALSE)

scen_3_buffer <- st_union(scen_2_buffer, fl_d) %>% 
                 st_union(fl_e) %>% st_as_sf() %>% st_make_valid()
scen_3_buffer_micro <- st_union(scen_3_buffer, micro %>% filter(Name == 'SW')) %>% 
  st_as_sf() %>% st_make_valid() %>% select(-everything())
st_write(scen_3_buffer, "output/shapes/scen_3_buffer.shp", append=FALSE)

fr_clean <- fr %>% filter(Name != 'NW CCW') %>%
  mutate(lettername = case_when(
    Name == 'NW CW' ~ 'A',
    Name == 'SE' ~ 'B',
    Name == 'SW' ~ 'C',
    Name == 'NE' ~ 'D',
    Name == 'S' ~ 'E'
  )) %>% st_transform(2922)

# buffer fixed routes - triming fixed route a buffer by highway 97
fr_a <- fr_clean %>% st_buffer(1320) %>% filter(lettername == 'A')
x <- st_split(fr_a, hwy97)
xpoly <- x %>% 
  st_collection_extract(c("POLYGON"))
xpoly$ID <- 1:nrow(xpoly)
xpoly$ID <- as.character(xpoly$ID)
xpoly <- st_as_sf(xpoly)
plot(xpoly['ID'])
fr_a <- xpoly %>% filter(ID == "1") %>% st_as_sf() %>% select(-everything()) %>% rename(x = geometry)
fr_buf <- fr_clean %>% filter(!lettername == 'A') %>% st_buffer(1320) %>% 
  st_intersection(taz) %>% st_union() %>% st_make_valid() %>% st_as_sf()
fr_buf <- rbind(fr_buf, fr_a) %>% st_union() %>% st_make_valid() %>% st_as_sf()

st_write(fr_buf, "output/shapes/fixed_route_qrtrmile_buffer.shp", append=FALSE)

scen_4_buffer_micro <- st_union(fr_buf, micro %>% filter(Name == 'SW')) %>% 
  st_as_sf() %>% st_make_valid() %>% select(-everything())
st_write(scen_4_buffer_micro, "output/shapes/scen_4_buffer.shp", append=FALSE)

fr_buf_ada <- fr_buf %>% st_buffer(2640) %>%  #3960 3/4 mile feet
  st_intersection(taz) %>% st_union() %>% st_make_valid() %>% st_as_sf()
st_write(fr_buf_ada, "output/shapes/fixed_route_ada_buffer.shp", append=FALSE)

# Analysis Metrics --------------------------------------------------------

# Metric 1 

m1a <- function(shape){
  shape$m1a_pop_served <- sf::st_interpolate_aw(tazinterp['pop'], shape, extensive=TRUE)$pop
  shape <- shape %>%
    mutate(
      m1a_pop_total = ugb_data$taz_pop2018,
      m1a_pop_srvd_pct = m1a_pop_served/m1a_pop_total) %>% st_drop_geometry()
  return(shape)
}

m1b <- function(shape){
  shape$m1b_pop_served_2040 <-  sf::st_interpolate_aw(taz['POPBASE'], shape, extensive=TRUE)$POPBASE
  ugb_2040_pop <- sf::st_interpolate_aw(taz['POPBASE'], ugb_data, extensive=TRUE)$POPBASE
  shape <- shape %>%
    mutate(
      m1b_pop_total_2040 = ugb_2040_pop,
      m1b_pop_srvd_pct_2040 = m1b_pop_served_2040/m1b_pop_total_2040) %>% st_drop_geometry()
  return(shape)
}

# Metric 2A, 2B, 3A, 3B 

m2a <- function(shape){
  shape$m2a_job_num <- sf::st_interpolate_aw(lehdb['jobs_work_in'], shape, extensive=TRUE)$jobs_work_in
  shape <- shape %>% mutate(
    m2a_job_total = sum(lehdb$jobs_work_in),
    m2a_job_pct = m2a_job_num/m2a_job_total
  ) %>% st_drop_geometry()
  return(shape)
}

m2b <- function(shape){
  shape$m2b_job_num_2040 <- sf::st_interpolate_aw(taz['EMPBASE'], shape, extensive=TRUE)$EMPBASE
  redmond_2040_jobs <- sf::st_interpolate_aw(taz['EMPBASE'], redmond_place, extensive=TRUE)$EMPBASE
  shape <- shape %>% mutate(
    m2b_job_total_2040 = redmond_2040_jobs,
    m2b_job_pct_2040 = m2b_job_num_2040/m2b_job_total_2040
  ) %>% st_drop_geometry()
  return(shape)
}

m3a <- function(shape){
  shape$m3a_job_num <- sf::st_interpolate_aw(lehdb['low_wage_live_in'], shape, extensive=TRUE)$low_wage_live_in
  shape <- shape %>% mutate(
    m3a_job_total = sum(lehdb$low_wage_live_in),
    m3a_job_pct = m3a_job_num/m3a_job_total
  ) %>% st_drop_geometry()
  return(shape)
}

m3b <- function(shape){
  shape$m3b_job_num <- sf::st_interpolate_aw(lehdb['low_wage_work_in'], shape, extensive=TRUE)$low_wage_work_in
  shape <- shape %>% mutate(
    m3b_job_total = sum(lehdb$low_wage_work_in),
    m3b_job_pct = m3b_job_num/m3b_job_total
  ) %>% st_drop_geometry()
  return(shape)
}

# Metric 4 
m4 <- function(shape){
  shape$m4_pov_served <-  sf::st_interpolate_aw(tazinterp['pov'], shape, extensive=TRUE)$pov
  shape <- shape %>%
    mutate(
      m4_pov_total = ugb_data$taz_pov2018,
      m4_pov_srvd_pct = m4_pov_served/m4_pov_total) %>% st_drop_geometry()
  return(shape)
}

# Metric 5 
m5 <- function(shape){
  shape$m5_over65_served <-  sf::st_interpolate_aw(tazinterp['over65'], shape, extensive=TRUE)$over65
  shape <- shape %>%
    mutate(
      m5_over65_total = ugb_data$taz_over652018,
      m5_over65_srvd_pct = m5_over65_served/m5_over65_total) %>% st_drop_geometry()
  return(shape)
}

# Metric 6 
m6 <- function(shape){
  shape$m6_yth_served <-  sf::st_interpolate_aw(tazinterp['youth_15_17'], shape, extensive=TRUE)$youth_15_17
  shape <- shape %>%
    mutate(
      m6_yth_total = ugb_data$taz_youth2018,
      m6_yth_srvd_pct = m6_yth_served/m6_yth_total) %>% st_drop_geometry()
  return(shape)
}

# Metric 7 

m7a <- function(shape){
  points <-  dar_trips %>% st_intersection(shape)
  shape <- shape %>% mutate(m7a_trips_num = sum(points$OW_trips_2),
                            m7a_trips_total = sum(dar_trips$OW_trips_2),
                            m7a_trips_pct = m7a_trips_num/m7a_trips_total) %>% 
    st_drop_geometry()
  return(shape)
}

m7b <- function(input_name){ # ADA 3/4 mile boundary only, for scenario 4 or ADA shape
  points <-  dar_trips %>% st_intersection(fr_buf_ada)
  fr_buf_ada <- fr_buf_ada %>% select(-everything()) %>%
    mutate(name = input_name) %>%
    mutate(m7b_trips_num = sum(points$OW_trips_2),
                            m7b_trips_total = sum(dar_trips$OW_trips_2),
                            m7b_trips_pct = m7b_trips_num/m7b_trips_total) %>% 
    st_drop_geometry()
  return(fr_buf_ada)
}

# Metric 8 

m8 <- function(shape, input_name){
  points <- dar_dest %>% st_intersection(shape)
  shape <- shape %>% mutate(m8_top_25_count = nrow(points)) %>% st_drop_geometry()
  
  points <- points %>% mutate(name = input_name) %>% st_drop_geometry()
  write.csv(points, paste0('output/top_25_destinations_', input_name, '.csv'), row.names = FALSE)
  return(shape)
}

# Metric 9 

m9 <- function(shape, input_name){
  points <- act_ctr %>% st_intersection(shape)
  shape <- shape %>% mutate(m9_act_ctr_count = nrow(points)) %>% st_drop_geometry()
  
  points2 <- act_ctr %>% mutate(name = input_name,
                                served_by = ifelse(Name %in% points$Name, TRUE, FALSE)) %>% 
    st_drop_geometry()
  write.csv(points2, paste0('output/activity_centers_', input_name, '.csv'), row.names = FALSE)
  return(shape)
}

# Metric 10 

m10 <- function(shape, input_name){
  hs <- act_ctr %>% filter(Category == 'High School' | Category == 'High Schools')
  points <- hs %>% st_intersection(shape)
  shape <- shape %>% mutate(m10_hs_count = nrow(points)) %>% st_drop_geometry()
  
  points <- points %>% mutate(name = input_name) %>% st_drop_geometry()
  write.csv(points, paste0('output/high_schools_', input_name, '.csv'), row.names = FALSE)
  return(shape)
}


# Run Metrics
# analysis shapes
scen_1_buffer <- scen_1_buffer %>% mutate(name = 'scenario_one')
scen_2_buffer_micro <- scen_2_buffer_micro %>% mutate(name = 'scenario_two')
scen_3_buffer_micro <- scen_3_buffer_micro %>% mutate(name = 'scenario_three')
scen_4_buffer_micro <- scen_4_buffer_micro %>% mutate(name = 'scenario_four')

scenario_shapes <- rbind(scen_1_buffer, scen_2_buffer_micro, scen_3_buffer_micro, scen_4_buffer_micro) %>% mutate(category = 'scenario')

fl_a <- fl_a %>% mutate(name = 'flex_route_A')
fl_b <- fl_b %>% mutate(name = 'flex_route_B')
fl_c <- fl_c %>% mutate(name = 'flex_route_C')
fl_d <- fl_d %>% mutate(name = 'flex_route_D')
fl_e <- fl_e %>% mutate(name = 'flex_route_E')
fr_a <- fr_a %>% mutate(name = 'fixed_route_A') %>% select(name) # special case because it was separated earlier and split by highway 97
fr_b <- fr_clean %>% rename(x = geometry) %>% filter(lettername == 'B') %>% st_buffer(1320) %>% mutate(name = 'fixed_route_B') %>% select(name)
fr_c <- fr_clean %>% rename(x = geometry) %>% filter(lettername == 'C') %>% st_buffer(1320) %>% mutate(name = 'fixed_route_C') %>% select(name)
fr_d <- fr_clean %>% rename(x = geometry) %>% filter(lettername == 'D') %>% st_buffer(1320) %>% mutate(name = 'fixed_route_D') %>% select(name)
fr_e <- fr_clean %>% rename(x = geometry) %>% filter(lettername == 'E') %>% st_buffer(1320) %>% mutate(name = 'fixed_route_E') %>% select(name)
micro1 <- micro %>% filter(Name == 'SW') %>% rename(x = geometry) %>% mutate(name = 'micro_transit_1_sw') %>% select(name)
micro2 <- micro %>% filter(Name == 'N/NE') %>% st_union() %>% st_as_sf() %>% mutate(name = 'micro_transit_2_n')
ada <- fr_buf_ada %>% mutate(name = 'ADA_service_area')

service_shapes <- rbind(fl_a, fl_b, fl_c, fl_d, fl_e, fr_a, fr_b, fr_c, fr_d, fr_e, micro1, micro2, ada) %>%
  mutate(category = 'service')

all_shapes <- rbind(scenario_shapes, service_shapes)

run_metrics <- function(){
  
  categories <- unique(all_shapes$category)
  
  for(i in categories){
    category_selected = i
    results_df <- data.frame()
    shapes <- all_shapes %>% filter(category == category_selected)
    for(j in shapes$name){
      shape_name <- j
      
      shape <- shapes %>% filter(name == shape_name)
      
      if(shape_name != 'scenario_four' & shape_name != 'ADA_service_area'){
        results <- shape %>% left_join(m1a(shape)) %>% left_join(m1b(shape)) %>% 
          left_join(m2a(shape)) %>% left_join(m2b(shape)) %>% left_join(m3a(shape)) %>% 
          left_join(m3b(shape)) %>% left_join(m4(shape)) %>% left_join(m5(shape)) %>% 
          left_join(m6(shape)) %>% left_join(m7a(shape)) %>% mutate(m7b_trips_num = NA, m7b_trips_total = NA, m7b_trips_pct = NA) %>%
          left_join(m8(shape, shape_name)) %>% left_join(m9(shape, shape_name)) %>% left_join(m10(shape, shape_name))  
      }
      else{
        results <- shape %>% left_join(m1a(shape)) %>% left_join(m1b(shape)) %>%
          left_join(m2a(shape)) %>% left_join(m2b(shape)) %>% left_join(m3a(shape)) %>%
          left_join(m3b(shape)) %>% left_join(m4(shape)) %>% left_join(m5(shape)) %>%
          left_join(m6(shape)) %>% left_join(m7a(shape)) %>% left_join(m7b(shape_name)) %>%
          left_join(m8(shape, shape_name)) %>% left_join(m9(shape, shape_name)) %>% left_join(m10(shape, shape_name))
      }
      
      results_df <- rbind(results_df, results)
    }
    
    export <- results_df %>% select(name) %>% rename(geometry = x)
    st_write(export, paste0('output/', category_selected, '_results_updated.shp'), append=FALSE)
    write.csv(results_df %>% st_drop_geometry(), paste0('output/', category_selected, '_results_updated.csv'), row.names=FALSE) 
  }
 
}

run_metrics()

# Maps --------------------------------------------------------------------
library(ggplot2)
library(ggthemes)
library(svglite)

# base shapes
citylim <- sf::st_read(dsn = '../../Data/GDBs/Base.gdb', layer = 'citylim_2018') %>%
  filter(CITY_NAME == "Redmond") %>% st_transform(2922)
ugb <- sf::st_read(dsn = '../../Data/GDBs/Base.gdb', layer = 'UGB_2018') %>%
  filter(InstName == "Redmond") %>% st_transform(2922)
hwys <- sf::st_read(dsn = '../../Data/GDBs/Base.gdb', layer = 'ESRI_streets_Hwy_only') %>% st_transform(2922) %>%
  st_intersection(ugb)
roads <- sf::st_read("../coic_dar_map/shapes/Redmond_maj_rds_2.shp") %>% st_zm() %>% st_transform(2922) %>%
  st_intersection(ugb)
parks <- sf::st_read(dsn = '../../Data/GDBs/Base.gdb', layer = 'Parks') %>% st_transform(2922) %>%
  st_intersection(ugb)

text_color <- '#27323d'
a_col <- '#007DB4'
b_col <- '#00A6B9'
c_col <- '#D3A809'
d_col <- '#E86228'
e_col <- '#595DA9'
micro_col <- '#7f13eb'
fr_col <- '#595959'

map_style_legend <- function(){
  theme_void() + theme(legend.position = "right",
                       legend.text=element_text(size=10, color = text_color),
                       legend.title=element_text(size=11, color = text_color),
                       panel.grid.major = element_line(colour = 'white'),
                       plot.margin = unit(c(0, 0, 0.5, 0), "cm"))#top, #right #bottom #left
}

map_style_without_legend <- function(){
  theme_void() + theme(legend.position = "none",
                       panel.grid.major = element_line(colour = 'white'))
}

# create coordinates for the map range, using the citylim
mapRange <- c(range(st_coordinates(citylim %>% st_transform(4326))[,1]),
              range(st_coordinates(citylim %>% st_transform(4326))[,2]))

base <- 
  ggplot() + geom_sf(data = ugb %>% st_transform(4326), aes(fill = 'a'), color = NA) +
  geom_sf(data = citylim %>% st_transform(4326), aes(fill = 'b'), color = NA) +
  geom_sf(data = parks %>% st_transform(4326), aes(fill = 'c'), color = NA) +
  geom_sf(data = hwys %>% st_transform(4326), color = '#6b6b6b', size = 1) +
  geom_sf(data = roads %>% st_transform(4326), color = '#6b6b6b', size = 0.5) +
  # geom_sf(data = hwy97 %>% st_transform(4326), color = '#6b6b6b', size = 2) +
  coord_sf(xlim = mapRange[c(1:2)], ylim = mapRange[c(3:4)]) +
  map_style_legend()

scen1 <-
  base + 
  geom_sf(data = scen_1_buffer, aes(fill = 'z'), 
          color = NA, alpha = 0.50) +
  geom_sf(data = fr_clean %>% filter(lettername == 'A'), aes(color = 'A'), size = 1.5) +
  geom_sf(data = fr_clean %>% filter(lettername == 'B'), aes(color = 'B'), size = 1.25) +
  scale_color_manual(labels = c('Flex Route A', 'Flex Route B'),
                     values = c(a_col, b_col),
                     name = 'Flex Routes') +
  scale_fill_manual(labels = c('UGB', 'City Limits', 'Parks', 'Flex-route deviation area (1/2 mile)'),
                    values = c('#ebe7df', '#d3d3d3', '#b3c293', fr_col)) +
  guides(fill=guide_legend(title="", override.aes= list(alpha = 0.15))) +
  coord_sf(xlim = mapRange[c(1:2)], ylim = mapRange[c(3:4)])
ggsave("../../Graphics/R_Outputs/scenario_1.png", plot = scen1, width = 9, height = 9)
ggsave("../../Graphics/R_Outputs/scenario_1.svg", plot = scen1, width = 9, height = 9)

scen2 <- 
  base + 
  geom_sf(data = scen_2_buffer, aes(fill = 'e'), 
          color = NA, alpha = 0.50) +
  geom_sf(data = micro, aes(fill = 'z'),  color = NA, alpha = 0.15) +
  geom_sf(data = fr_clean %>% filter(lettername == 'A'), aes(color = 'A'), size = 1.5) +
  geom_sf(data = fr_clean %>% filter(lettername == 'B'), aes(color = 'B'), size = 1.25) +
  geom_sf(data = fr_clean %>% filter(lettername == 'C'), aes(color = 'C'), size = 1.15) +
  scale_color_manual(labels = c('Flex Route A', 'Flex Route B', 'Flex Route C'),
                     values = c(a_col, b_col, c_col),
                     name = 'Flex Routes') +
  scale_fill_manual(labels = c('UGB', 'City Limits', 'Parks', 'Flex-route deviation area (1/2 mile)', 'Microtransit zone'),
                    values = c('#ebe7df', '#d3d3d3', '#b3c293', fr_col, micro_col)) +
  guides(fill=guide_legend(title="", override.aes= list(alpha = 0.15))) +
  coord_sf(xlim = mapRange[c(1:2)], ylim = mapRange[c(3:4)])
ggsave("../../Graphics/R_Outputs/scenario_2.png", plot = scen2, width = 9, height = 9)
ggsave("../../Graphics/R_Outputs/scenario_2.svg", plot = scen2, width = 9, height = 9)

scen3 <-
  base + 
  geom_sf(data = scen_3_buffer, aes(fill = 'e'), 
          color = NA, alpha = 0.50) +
  geom_sf(data = micro %>% filter(Name == 'SW'), aes(fill = 'z'),  color = NA, alpha = 0.15) +
  geom_sf(data = fr_clean %>% filter(lettername == 'A'), aes(color = 'A'), size = 1.5) +
  geom_sf(data = fr_clean %>% filter(lettername == 'B'), aes(color = 'B'), size = 1.25) +
  geom_sf(data = fr_clean %>% filter(lettername == 'C'), aes(color = 'C'), size = 1.15) +
  geom_sf(data = fr_clean %>% filter(lettername == 'D'), aes(color = 'D'), size = 1) +
  geom_sf(data = fr_clean %>% filter(lettername == 'E'), aes(color = 'e'), size = 1.15) +
  scale_color_manual(labels = c('Flex Route A', 'Flex Route B', 'Flex Route C', 'Flex Route D', 'Flex Route E'),
                     values = c(a_col, b_col, c_col, d_col, e_col),
                     name = 'Flex Routes') +
  scale_fill_manual(labels = c('UGB', 'City Limits', 'Parks', 'Flex-route deviation area (1/2 mile)', 'Microtransit zone SW'),
                    values = c('#ebe7df', '#d3d3d3', '#b3c293', fr_col, micro_col)) +
  guides(fill = guide_legend(title = "", order = 1, override.aes = list(alpha = 0.15))) +
  coord_sf(xlim = mapRange[c(1:2)], ylim = mapRange[c(3:4)])
ggsave("../../Graphics/R_Outputs/scenario_3.png", plot = scen3, width = 9, height = 9)
ggsave("../../Graphics/R_Outputs/scenario_3.svg", plot = scen3, width = 9, height = 9)

scen3_w_points <-
  base + 
  geom_sf(data = scen_3_buffer, aes(fill = 'e'), 
          color = NA, alpha = 0.50) +
  geom_sf(data = micro %>% filter(Name == 'SW'), aes(fill = 'z'),  color = NA, alpha = 0.15) +
  geom_sf(data = fr_clean %>% filter(lettername == 'A'), aes(color = 'A'), size = 1.5) +
  geom_sf(data = fr_clean %>% filter(lettername == 'B'), aes(color = 'B'), size = 1.25) +
  geom_sf(data = fr_clean %>% filter(lettername == 'C'), aes(color = 'C'), size = 1.15) +
  geom_sf(data = fr_clean %>% filter(lettername == 'D'), aes(color = 'D'), size = 1) +
  geom_sf(data = dar_trips, color = 'red') +
  geom_sf(data = dar_dest) +
  scale_color_manual(labels = c('Flex Route A', 'Flex Route B', 'Flex Route C', 'Flex Route D'),
                     values = c(a_col, b_col, c_col, d_col),
                     name = 'Flex Routes') +
  scale_fill_manual(labels = c('UGB', 'City Limits', 'Parks', 'Flex-route deviation area (1/2 mile)', 'Microtransit zone SW'),
                    values = c('#ebe7df', '#d3d3d3', '#b3c293', fr_col, micro_col)) +
  guides(fill = guide_legend(title = "", order = 1, override.aes = list(alpha = 0.15))) +
  coord_sf(xlim = mapRange[c(1:2)], ylim = mapRange[c(3:4)])
ggsave("../../Graphics/R_Outputs/scenario_3_w_points.svg", plot = scen3_w_points, width = 9, height = 9)

scen4 <-
  base + 
  geom_sf(data = fr_buf_ada, fill = NA, aes(linetype = 'a'), size = .75) +
  geom_sf(data = fr_buf, aes(fill = 'e'), color = NA, alpha = 0.50) +
  geom_sf(data = micro %>% filter(Name == 'SW'), aes(fill = 'z'),  color = NA, alpha = 0.15) +
  geom_sf(data = fr_clean %>% filter(lettername == 'A'), aes(color = 'A'), size = 1.5) +
  geom_sf(data = fr_clean %>% filter(lettername == 'B'), aes(color = 'B'), size = 1.25) +
  geom_sf(data = fr_clean %>% filter(lettername == 'C'), aes(color = 'C'), size = 1.15) +
  geom_sf(data = fr_clean %>% filter(lettername == 'D'), aes(color = 'D'), size = 1) +
  geom_sf(data = fr_clean %>% filter(lettername == 'E'), aes(color = 'E'), size = 1.25) +
  scale_linetype_manual(labels = c('ADA service area (3/4 mile)'), values = c(14)) +
  scale_color_manual(labels = c('Fixed Route A', 'Fixed Route B', 'Fixed Route C', 'Fixed Route D', 'Fixed Route E'),
                     values = c(a_col, b_col, c_col, d_col, e_col),
                     name = 'Fixed Routes') +
  scale_fill_manual(labels = c('UGB', 'City Limits', 'Parks', 'Fixed-route access area (1/4 mile)', 
                               'Microtransit zone SW'), 
                    values = c('#ebe7df', '#d3d3d3', '#b3c293', fr_col, micro_col)) +
  guides(fill = guide_legend(title = NULL, order = 1, override.aes = list(alpha = 0.15)),
         linetype = guide_legend(title = NULL, order = 2)) +
  coord_sf(xlim = mapRange[c(1:2)], ylim = mapRange[c(3:4)])
ggsave("../../Graphics/R_Outputs/scenario_4.png", plot = scen4, width = 9, height = 9)
ggsave("../../Graphics/R_Outputs/scenario_4.svg", plot = scen4, width = 9, height = 9) 

scen4_w_points <-
  base + 
  geom_sf(data = fr_buf_ada, fill = NA, aes(linetype = 'a'), size = .75) +
  geom_sf(data = fr_buf, aes(fill = 'e'), color = NA, alpha = 0.50) +
  geom_sf(data = micro %>% filter(Name == 'SW'), aes(fill = 'z'),  color = NA, alpha = 0.15) +
  geom_sf(data = fr_clean %>% filter(lettername == 'A'), aes(color = 'A'), size = 1.5) +
  geom_sf(data = fr_clean %>% filter(lettername == 'B'), aes(color = 'B'), size = 1.25) +
  geom_sf(data = fr_clean %>% filter(lettername == 'C'), aes(color = 'C'), size = 1.15) +
  geom_sf(data = fr_clean %>% filter(lettername == 'D'), aes(color = 'D'), size = 1) +
  geom_sf(data = fr_clean %>% filter(lettername == 'E'), aes(color = 'E'), size = 1.25) +
  geom_sf(data = dar_trips, color = 'red') +
  geom_sf(data = dar_dest) +
  scale_linetype_manual(labels = c('ADA service area (3/4 mile)'), values = c(14)) +
  scale_color_manual(labels = c('Fixed Route A', 'Fixed Route B', 'Fixed Route C', 'Fixed Route D', 'Fixed Route E'),
                     values = c(a_col, b_col, c_col, d_col, e_col),
                     name = 'Fixed Routes') +
  scale_fill_manual(labels = c('UGB', 'City Limits', 'Parks', 'Fixed-route access area (1/4 mile)', 
                               'Microtransit zone SW'), 
                    values = c('#ebe7df', '#d3d3d3', '#b3c293', fr_col, micro_col)) +
  guides(fill = guide_legend(title = NULL, order = 1, override.aes = list(alpha = 0.15)),
         linetype = guide_legend(title = NULL, order = 2)) +
  coord_sf(xlim = mapRange[c(1:2)], ylim = mapRange[c(3:4)])
ggsave("../../Graphics/R_Outputs/scenario_4_w_points.svg", plot = scen4_w_points, width = 9, height = 9) 



