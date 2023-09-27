# library(gdata)
# note that readxl does not yet support reading from a URL: https://github.com/tidyverse/readxl/issues/278
library(readxl)
library(tidyverse)

# Get Data ---------------------------------------------------------------------

# Direct download from the BLS website: https://www.bls.gov/lau/#cntyaa
# Grabbing County level unemployment rates from 2006 - 2020

# years <- 6:22
# 
# URLs <- paste0("https://www.bls.gov/lau/laucnty", 
#                stringr::str_pad(years, 2, "left", 0), 
#                ".xlsx")
# 

# NOTE: in Sept 2023 code above was broken on windows with R 4.3.1, instead of
# fixing I manually downloaded bls data from https://www.bls.gov/lau/tables.htm#mcounty
# for 2006 to 2022 to raw-data/BLS. This data is available in the OSF repository.

files <- list.files("data-raw/BLS/", full.names = TRUE)

raw  <- files %>% 
  map_dfr(function(x) read_xlsx(x, skip = 4, 
                                col_types = "text")) 

saveRDS(raw, "data-raw/BLS/BLS_County_Unemployment.rds")
  

# Clean Data -------------------------------------------------------------------

df <- raw %>% 
  rename(laus_code = Code...1, 
         state_code = Code...2, 
         county_code = Code...3,
         county_name = `County Name/State Abbreviation`, 
         year = Year, 
         labor_force = Force,
         employed = Employed,
         unemployed = Unemployed,
         unemployment_rate = `(%)`) %>% 
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
  select(-...6)

saveRDS(df, "Data/BLS_County_Unemployment.rds")
