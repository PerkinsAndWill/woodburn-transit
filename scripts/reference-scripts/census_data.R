# Get Census data for blocks and Redmond Place
# external scripts
source("census_funcs.R")

# block groups ------------------------------------------------------------
  pop <- get_acs_table(geography = 'block group', year = 2018, 
                       var_vector = c(pop = 'B01001_001'), state = 'OR', county = 'Deschutes') %>%
    select(GEOID, NAME, pop = popE)
  
  sr <- 
    get_acs_table(geography = 'block group', year = 2018, 
                  var_vector = c(m65 = 'B01001_020', 
                                 m67 = 'B01001_021',
                                 m70 = 'B01001_022', 
                                 m75 = 'B01001_023',
                                 m80 = 'B01001_024',
                                 m85 = 'B01001_025',
                                 f65 = 'B01001_044',
                                 f67 = 'B01001_045',
                                 f70 = 'B01001_046',
                                 f75 = 'B01001_047',
                                 f80 = 'B01001_048',
                                 f85 = 'B01001_049'), state = 'OR', county = 'Deschutes') %>%
    rowwise() %>%
    mutate(over65 = sum(m65E, m67E, m70E, m75E, m80E, m85E, f65E, f67E, f70E, f75E, f80E, f85E)) %>%
    select(GEOID, NAME, over65)
  
  yth <- 
    get_acs_table(geography = 'block group', year = 2018, 
                  var_vector = c(f = 'B01001_030', 
                                 m = 'B01001_006'), state = 'OR', county = 'Deschutes') %>%
    rowwise() %>%
    mutate(youth_15_17 = sum(fE, mE)) %>%
    select(GEOID, NAME, youth_15_17)
  
  pov <- 
    get_acs_table(geography = 'block group', year = 2018, 
                  var_vector = c(pov = 'C17002_008'), state = 'OR', county = 'Deschutes') %>%
    select(GEOID, NAME, pov = povE)
  
  # block group shapes
  bg <- get_census_shapes(geography = 'block_groups', year = 2018, state = 'OR', county = 'Deschutes') %>% st_transform(2922)
  # block shapes for LEHD data
  bl <- get_census_shapes(geography = 'blocks', year = 2018, state = 'OR', county = 'Deschutes') %>% st_transform(2922)
  # place redmond
  redmond_place <- places(year = 2018, state = 'OR') %>% filter(NAME == 'Redmond') %>% st_transform(2922)

# Place ------------------------------------------------------------
  pop_place <- get_acs_table(geography = 'place', year = 2018, 
                       var_vector = c(pop = 'B01001_001'), state = 'OR') %>%
    select(GEOID, NAME, pop = popE) %>%
    filter(NAME == 'Redmond city, Oregon')
  
  sr_place <- 
    get_acs_table(geography = 'place', year = 2018, 
                  var_vector = c(m65 = 'B01001_020', 
                                 m67 = 'B01001_021',
                                 m70 = 'B01001_022', 
                                 m75 = 'B01001_023',
                                 m80 = 'B01001_024',
                                 m85 = 'B01001_025',
                                 f65 = 'B01001_044',
                                 f67 = 'B01001_045',
                                 f70 = 'B01001_046',
                                 f75 = 'B01001_047',
                                 f80 = 'B01001_048',
                                 f85 = 'B01001_049'), state = 'OR') %>%
    rowwise() %>%
    mutate(over65 = sum(m65E, m67E, m70E, m75E, m80E, m85E, f65E, f67E, f70E, f75E, f80E, f85E)) %>%
    select(GEOID, NAME, over65) %>%
    filter(NAME == 'Redmond city, Oregon')
  
  yth_place <- 
    get_acs_table(geography = 'place', year = 2018, 
                  var_vector = c(f = 'B01001_030', 
                                 m = 'B01001_006'), state = 'OR') %>%
    rowwise() %>%
    mutate(youth_15_17 = sum(fE, mE)) %>%
    select(GEOID, NAME, youth_15_17) %>%
    filter(NAME == 'Redmond city, Oregon')
  
  pov_place <- 
    get_acs_table(geography = 'place', year = 2018, 
                  var_vector = c(pov = 'C17002_008'), state = 'OR') %>%
    select(GEOID, NAME, pov = povE) %>%
    filter(NAME == 'Redmond city, Oregon')
