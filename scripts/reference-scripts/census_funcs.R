## ---------------------------
##
## Script name: Census Data
##
## Purpose of script: Functions to pull census data
##
## Author: Esther Needham
##
## Date Created: 2020-02-03
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

require(tidyverse)
require(tidycensus)
require(tigris)
require(sf)

## ---------------------------

## load up our functions into memory -- for future use once we have organized re-usable custom functions

# source("functions/summarise_data.R") 

## ---------------------------

# helpful instructions for use
# https://walkerke.github.io/tidycensus/articles/basic-usage.html

#census_api_key('YOUR API KEY HERE')

# v18 <- load_variables(2018, "acs5", cache = TRUE)
# View(v18)

get_acs_table <- function(geography, year, var_vector, output = "wide", state = NULL, county = NULL){
    if((geography == 'county subdivision' | geography == 'tract' | geography == 'block group') & is.null(state)){
      print(paste0("ERROR: You must specify at least one state to get ", geography, " level data"))
    }else{
      if(output == "wide"){
        dat <- get_acs(geography = geography, 
                       variables = var_vector, 
                       state = state, 
                       county = county,
                       year = year, 
                       output = output)            
      }
      if(output == "long"){
        dat <- get_acs(geography = geography, 
                       variables = var_vector, 
                       state = state, 
                       county = county,
                       year = year)            
      }
    }
  return(dat)
  }

get_census_shapes <- function(geography, year, state = NULL, county = NULL, class = 'sf'){
  if(geography == 'county subdivision'){
    geography = 'county_subdivisions'
  }
  if(geography == 'tract'){
    geography = 'tracts'
  }
  if(geography == 'block group'){
    geography = 'block_groups'
  }
  if(geography == 'block'){
    geography = 'blocks'
  }
  if((geography == 'county_subdivisions' | geography == 'tracts' | geography == 'block_groups' | geography == 'blocks') & is.null(state)){
    print(paste0("ERROR: You must specify at least one state to get ", geography, " level shapes"))
  }
  else{
    if(geography == 'county_subdivisions'){
      shapes <- county_subdivisions(year = year, state = state, county = county, class = class)
    }
    if(geography == 'tracts'){
      shapes <- tracts(year = year, state = state, county = county, class = class)
    }
    if(geography == 'block_groups'){
      shapes <- block_groups(year = year, state = state, county = county, class = class)
    }
    if(geography == 'blocks'){
      shapes <- blocks(year = year, state = state, county = county, class = class)
    }
  }
  return(shapes)
}


get_census_map <- function(geography, year, var_vector, state = NULL, county = NULL){
    
  table <- get_acs_table(geography, year, var_vector, output = "wide", state, county)

  shapes <- get_census_shapes(geography, year, state, county)

  joined <- shapes %>% left_join(table, by = 'GEOID')
  
  return(joined)
}
