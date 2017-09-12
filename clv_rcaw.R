library(tidycensus)
library(tidyverse)
library(tigris)
library(sf)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

#Census API Key
census_api_key("51bc73761c34ae0c240079cfc71fb4c50eaaec52", install = TRUE)

#View ACS variable list
v15 <- load_variables(2015, "acs5", cache = TRUE)
View(v15)

#Variable list
totpop   <- "B01003_001E"
nhwht    <- "B03002_003E"
nhblk    <- "B03002_004E" 
medinc   <- "B19013_001E"
pov_den  <- "C17002_001E"
povu50   <- "C17002_002E"
pov51_99 <- "C17002_003E"
var_list <- c(totpop, nhwht, nhblk, medinc, pov_den, povu50, pov51_99)

#Pull data for state of OH
oh_dat <- get_acs(geography = "tract", variables = var_list, state = "OH", geometry = TRUE)

#Subset Cleveland Metro Area - code adapted from https://walkerke.github.io/2017/05/tigris-metros/ 
cb <- core_based_statistical_areas(cb = TRUE)
clv_met <- filter(cb, grepl("Cleveland-Elyria", NAME))

in_clv <- st_within(oh_dat, clv_met)
in_clv2 <- map_lgl(in_clv, function(x) {
  if (length(x) == 1) {
    return(TRUE)
  } else {
    return(FALSE)
  }
})

#subset cleveland tracts
clv_trcts_tidy <- oh_dat[in_clv2,]

#Wide to long reshape
clv_untidy <- clv_trcts_tidy %>% 
  select(GEOID, variable, estimate) %>% 
  spread(variable, estimate) %>% 
  rename(
    totpop    = B01003_001,
    nhwht     = B03002_003,
    nhblk     = B03002_004,
    medinc    = B19013_001,
    pov_den   = C17002_001,
    povu50    = C17002_002,
    pov51_99  = C17002_003) %>%
  mutate(
    poc     = totpop - nhwht,
    pctp    = (poc/totpop)*100,
    pctw    = (nhwht/totpop)*100,
    pov     = povu50 + pov51_99,
    pctpov  = (pov/pov_den)*100,
    whtseg  = ifelse(pctw > 90 & totpop > 100, "Y", "N"),
    rcap    = ifelse(pctpov > 40 & pctp > 50 & totpop > 100, "Y", "N"),
    rcaa    = ifelse(pctw > 90 & medinc > 100000 & totpop > 100, "Y", "N"),
    segstat = ifelse(rcap == "Y", "RCAP",
                     ifelse(rcaa == "Y", "RCAA", NA))
  )

########## Tables #################
#White Seg Nhoods > 90% white
clv_untidy %>%
  group_by(whtseg) %>% 
  summarise(
    ntrcts = n(),
    totpop = sum(totpop),
    whtpop = sum(nhwht)
  )

#RCAP
clv_untidy %>%
  group_by(rcap) %>% 
  summarise(
    ntrcts = n(),
    totpop = sum(totpop)
  ) 

#RCAP
clv_untidy %>%
  group_by(rcaa) %>% 
  summarise(
    ntrcts = n(),
    totpop = sum(totpop)
  ) 

################
#Visualization
ggplot(clv_untidy, aes(fill = segstat)) + 
  geom_sf(alpha = 0.5) +
  scale_fill_discrete(na.value = NA, name = "Segregation Type") +
  # coord_sf(crs = 26917) + #UTM 17N Projection
  theme_void() +
  ggtitle("Racially Concentrated Areas of Affluence & Poverty \n Cleveland, OH")



#Export as shapefile
st_write(clv_untidy, "./clv_rcaa.shp")

