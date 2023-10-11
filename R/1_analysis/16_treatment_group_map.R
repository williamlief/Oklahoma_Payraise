library(tidyverse)
library(sf)
library(maps)
library(ggthemes)

# treatment group map -----------------------------------------------------
# Note - state labels manually added with photo editor

df_district <- readRDS("Data/clean_district.rds") %>%
  filter(year <= 2020,
         year > 2008) %>% 
  mutate(turnover = turnover * 100, 
         year = year - 2018)

# Creates figure 6. Requires raw data downloaded from OSF.
if (dir.exists("data-raw/EDGE_SCHOOLDISTRICT_TL18_SY1718")) {
  
  state_map_data <- 
    map('state', fill = TRUE, plot = FALSE) %>% 
    st_as_sf() %>% 
    filter(ID %in% c("oklahoma", "texas"))
  
  raw_edge <- st_read("data-raw/EDGE_SCHOOLDISTRICT_TL18_SY1718")
  edge <- raw_edge %>% filter(STATEFP %in% c("40", "48", "20"))
  
  sf <- df_district %>% filter(year == 0) %>% 
    select(state, NCES_leaid, border, psm_group) %>% 
    mutate(psm_group = if_else(is.na(psm_group), 0, 1)) %>% 
    inner_join(edge, by = c("NCES_leaid" = "GEOID"))
  
  sf2 <- 
    sf %>% 
    # sample_frac(sf, .01) %>% 
    mutate(fill = case_when( 
      psm_group == 1 & border == 1 ~ "Border & PSM", 
      psm_group == 1 ~ "PSM", 
      border == 1 ~ "Border County", 
      TRUE ~ "Other Districts"
    ))
  
  # NB: this plot takes forever to render in rstudio, better to save it as a png
  # and inspect that
  p <- ggplot(data = state_map_data) +
    geom_sf(aes(fill =  fill, geometry = geometry), lwd = 0.5, data = sf2) +
    geom_sf(lwd = 1.5, fill = "transparent") +
    labs(y = NULL, x = NULL, fill = "Sample", 
         title = "District Samples") +
    theme_map()
  
  ggsave("figures/sample_map.png", plot = p, bg = "white", width = 7, height = 7)
  
} else (warning("Shapefile not found, please download raw data from https://osf.io/7f6ms/"))
