library(readxl)
library(tidyverse)

# parse data --------------------------------------------------------------

raw <- read_excel("data-raw/Kansas/DR#741.xls")

# quality checks ----------------------------------------------------------

check_equal <- function(v1, v2) {
  sum(raw[v1] != raw[v2])
}

# duplicated columns?? - note never received response from state dept ed
check_equal("N_total_1", "N_total")
check_equal("experience_mean_1", "experience_mean")
check_equal("salary_mean_1", "salary_mean")

summary(raw$pct_turnover)

# pcts are all less than 100 and ge 0
raw %>% summarize(across(starts_with("pct"), max)) %>% pivot_longer(everything())
raw %>% summarize(across(starts_with("pct"), min)) %>% pivot_longer(everything())

# summaries look ok
summary(raw$N_total_1)
summary(raw$experience_mean_1)
summary(raw$salary_mean_1)

# check that sums of race = total n
countsum = apply((raw[grepl("sum", names(raw))][,1:8]), 1, sum)
sum(countsum != raw$N_total)


# rename vars -------------------------------------------------------------


df <- raw %>% 
  mutate( 
         year = program_yr + 1,
         across(starts_with("pct_"), function(x) x / 100),
         pct_white = WHT1_sum / N_total) %>% 
  select(year, 
         district_name = org_name, 
         localid = org_no,
         avg_sal = salary_mean, 
         avg_exp = experience_mean,
         p_black = pct_black,
         p_hispanic = pct_hispanic,
         p_white = pct_white, 
         n = N_total,
         turnover = pct_turnover)



# nces --------------------------------------------------------------------


nces <- readRDS("data/nces_data.rds") %>%
  filter(state == "KS") %>%
  select(localid, NCES_leaid) %>%
  mutate(localid = toupper(localid)) %>%
  distinct()

# nces <- readr::read_csv("data-raw/NCES_CCD.csv") %>% 
#   filter(state == "KS") %>% 
#   select(localid, NCES_leaid) %>% 
#   distinct()

df_nces <- df %>% 
  left_join(nces) %>% 
  tidylog::filter(!is.na(NCES_leaid)) %>% 
  select(-localid)
if (nrow(df_nces) != nrow(df)) warning("the nces merge inflated the observation count")


saveRDS(df_nces, "Data/KS_all.rds")
