library(gdata)
# note that readxl does not yet support reading from a URL: https://github.com/tidyverse/readxl/issues/278
library(tidyverse)

# Get Data ---------------------------------------------------------------------

if (!file.exists("data-raw/BLS_County_Unemployment.rds")) {
  
  # Direct download from the BLS website: https://www.bls.gov/lau/#cntyaa
  # Grabbing County level unemployment rates from 2006 - 2020
  
  years <- 6:20
  
  URLs <- paste0("http://www.bls.gov/lau/laucnty", 
                 stringr::str_pad(years, 2, "left", 0), 
                 ".xlsx")
  
  raw  <- URLs %>% map_dfr(function(x) read.xls(x, skip = 3, colClasses = "character"))
  saveRDS(raw, "data-raw/BLS_County_Unemployment.rds")
  
} else { 
  
  print("Raw data file has already been created, loading downloaded file")
  raw <- readRDS("data-raw/BLS_County_Unemployment.RDS")
  
}

# Clean Data -------------------------------------------------------------------

df <- raw %>% 
  rename(laus_code = Code, 
         state_code = Code.1, 
         county_code = Code.2,
         county_name = County.Name.State.Abbreviation, 
         year = Year, 
         labor_force = Force,
         employed = Employed,
         unemployed = Unemployed,
         unemployment_rate = X...) %>% 
  filter(
    state_code != "", # 2 footer rows per year
    #state_code %in% c("29", "48") # TX and OK - actually leave in all states
    ) %>% 
  mutate(across(c(year, labor_force, employed, unemployed, unemployment_rate),
                function(x) {(gsub(",", "", x))}), 
         across(c(year, labor_force, employed, unemployed, unemployment_rate),
                function(x) {if_else(x == "N.A.", NA_character_, x)}),
         across(c(year, labor_force, employed, unemployed, unemployment_rate),
                as.numeric)
         ) %>%  
  # remove commas and convert to double
  select(-X)

saveRDS(df, "Data/BLS_County_Unemployment.rds")
