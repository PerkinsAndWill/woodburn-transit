# Re-apportion population and acs variables to taz boundaries

library(tidyverse)
library(sf)

# external scripts
source("census_data.R")

taz2040 <<- sf::st_read(dsn = '../../Data/GDBs/PopEmpTAZ.gdb', layer = 'TAZ_2040') %>% st_transform(2922)
taz2010 <<- sf::st_read(dsn = '../../Data/GDBs/PopEmpTAZ.gdb', layer = 'TAZ_2010') %>% st_transform(2922)

bg_dat <- bg %>% left_join(pop) %>% left_join(sr) %>% left_join(yth) %>% 
  left_join(pov)%>% st_transform(2922) %>%
  mutate(over65_pct = over65/pop, 
         youth_pct = youth_15_17/pop,
         pov_pct = pov/pop)
st_write(bg_dat, "output/shapes/block_groups_w_data.shp", append = FALSE)

# interpolate 2018 population for each taz based on incremental yearly change between 2010 and 2040
tazinterp <- taz2040 %>% select(TAZ, pop2040 = POPBASE) %>%
  left_join(taz2010 %>% select(TAZ, pop2010 = POPBASE) %>% st_drop_geometry()) %>%
  mutate(pop_chng = pop2040 - pop2010,
         chng_pr_yr = pop_chng/30,
         chng_2018 = chng_pr_yr*8,
         pop2018 = pop2010+chng_2018, 
         pop = pop2018) # assign pop 2018 to default pop

# create denominator population for Redmond - and check that this 2018 population is similar to ACS place population for Redmond
    # ACS place population for 2018 is 29,234 (see pop_place)
redmond_place$taz_pop2018 <- sf::st_interpolate_aw(tazinterp['pop2018'], redmond_place, extensive=TRUE)$pop2018


tazinterp$pov_pct <-  sf::st_interpolate_aw(bg_dat['pov_pct'], tazinterp, extensive=FALSE)$pov_pct
tazinterp$over65_pct <-  sf::st_interpolate_aw(bg_dat['over65_pct'], tazinterp, extensive=FALSE)$over65_pct
tazinterp$youth_pct <-  sf::st_interpolate_aw(bg_dat['youth_pct'], tazinterp, extensive=FALSE)$youth_pct

tazinterp <- tazinterp %>%
  mutate(pov = pov_pct*pop2018,
         over65 = over65_pct*pop2018,
         youth_15_17 = youth_pct*pop2018)

# Check pov, over65, and youth interpolation against the values for Redmond Place
# ACS place - see pov_place, yth_place, and sr_place
# Everything looks pretty close
redmond_place$taz_pov2018 <- sf::st_interpolate_aw(tazinterp['pov'], redmond_place, extensive=TRUE)$pov
redmond_place$taz_over652018 <- sf::st_interpolate_aw(tazinterp['over65'], redmond_place, extensive=TRUE)$over65
redmond_place$taz_youth2018 <- sf::st_interpolate_aw(tazinterp['youth_15_17'], redmond_place, extensive=TRUE)$youth_15_17
# add in the ACS values
redmond_place$acs_pop2018 <- pop_place$pop
redmond_place$acs_pov2018 <- pov_place$pov
redmond_place$acs_over652018 <- sr_place$over65
redmond_place$acs_youth2018 <- yth_place$youth_15_17
st_write(redmond_place, "output/shapes/redmond_place_w_data.shp", append=FALSE)

# And join taz variables to the ugb
ugb_data <- sf::st_read(dsn = '../../Data/GDBs/Base.gdb', layer = 'UGB_2018') %>%
  filter(InstName == "Redmond") %>% st_transform(2922)
ugb_data$taz_pop2018 <- sf::st_interpolate_aw(tazinterp['pop2018'], ugb_data, extensive=TRUE)$pop2018
ugb_data$taz_pov2018 <- sf::st_interpolate_aw(tazinterp['pov'], ugb_data, extensive=TRUE)$pov
ugb_data$taz_over652018 <- sf::st_interpolate_aw(tazinterp['over65'], ugb_data, extensive=TRUE)$over65
ugb_data$taz_youth2018 <- sf::st_interpolate_aw(tazinterp['youth_15_17'], ugb_data, extensive=TRUE)$youth_15_17
