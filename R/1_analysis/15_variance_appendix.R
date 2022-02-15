library(tidyverse)
library(stargazer)
library(starpolishr)
library(lfe)

sg_type = "latex"
sg_stars = c(.05,.01,.001)
sg_omit_stats = c("ser", "rsq")


df_district <- readRDS("Data/clean_district_2021-01-11_.rds") %>%
  filter(year <= 2020,
         year > 2008, 
         state %in% c("TX", "OK", "KS")) %>% 
  mutate(turnover = turnover * 100, 
         year = year - 2018)

df_district_border <- df_district %>% filter(border == 1)
df_district_psm <- df_district %>% filter(!is.na(psm_group))

df_state <- readRDS("Data/clean_state_2021-01-11_.rds") %>% 
  filter(year <= 2020,
         year > 2008, 
         state %in% c("TX", "OK", "KS", "PSMatched")) %>% 
  mutate(turnover = turnover * 100,
         year = year - 2018)


# Full State Models -------------------------------------------------------

# State unclustered
m1 <- lfe::felm(turnover ~ post_strike  | state + year_fac | 0 | 0, 
                data = df_state %>% filter(sample == "Full State"))

# State clustered
m2 <- lfe::felm(turnover ~ post_strike  | state + year_fac | 0 | state + year_fac, 
                data = df_state %>% filter(sample == "Full State"))

# state 2 year by 3 unit collapse
m3 <- lfe::felm(turnover ~ post_strike | state + year | 0 | 0,
                data = df_state %>% filter(sample == "Full State") %>% 
                  group_by(state, post_strike, year = year > 0) %>% 
                  summarize(turnover = mean(turnover))) 

# District unclustered                
m4 <- lfe::felm(turnover ~ post_strike  | state + year_fac | 0 | 0, 
                data = df_district, 
                weight = df_district$reg_weight_full)

# District with state year
m5 <- lfe::felm(turnover ~ post_strike  | state + year_fac | 0 | state + year_fac, 
                data = df_district, 
                weight = df_district$reg_weight_full)

# District with year and county
m6 <- lfe::felm(turnover ~ post_strike  | state + year_fac | 0 | county + year_fac, 
                data = df_district, 
                weight = df_district$reg_weight_full)

p1 <- stargazer(m1, m2, m3, m4, m5, m6,
          type = sg_type,
          omit.stat = sg_omit_stats,
          title = "Comparison of SE Clustering Strategies",
          star.cutoffs = sg_stars, 
          header = FALSE, 
          add.lines = list(c("Observation Level", rep("State X Year", 2),
                             "State X Post Strike", rep("District X Year", 3)), 
                           c("SE Clustering", "None", "State, Year", "None", 
                             "None", "State, Year", "County, Year"), 
                           c("Population Weights", "", "", "", "X", "X", "X")),
          label = "tab:cluster1")

# Border models -----------------------------------------------------------

# State unclustered
m1 <- lfe::felm(turnover ~ post_strike  | state + year_fac | 0 | 0, 
                data = df_state %>% filter(sample == "Border Counties"))

# State clustered
m2 <- lfe::felm(turnover ~ post_strike  | state + year_fac | 0 | state + year_fac, 
                data = df_state %>% filter(sample == "Border Counties"))

# state 2 year by 3 unit collapse
m3 <- lfe::felm(turnover ~ post_strike | state + year | 0 | 0,
                data = df_state %>% filter(sample == "Border Counties") %>% 
                  group_by(state, post_strike, year = year > 0) %>% 
                  summarize(turnover = mean(turnover))) 

# District unclustered                
m4 <- lfe::felm(turnover ~ post_strike  | state + year_fac | 0 | 0, 
                data = df_district_border, 
                weight = df_district_border$reg_weight_border)

# District with state year
m5 <- lfe::felm(turnover ~ post_strike  | state + year_fac | 0 | state + year_fac, 
                data = df_district_border, 
                weight = df_district_border$reg_weight_border)

# District with year and county
m6 <- lfe::felm(turnover ~ post_strike  | state + year_fac | 0 | county + year_fac, 
                data = df_district_border, 
                weight = df_district_border$reg_weight_border)


p2 <- stargazer(m1, m2, m3, m4, m5, m6,
                type = sg_type,
                omit.stat = sg_omit_stats,
                star.cutoffs = sg_stars, 
                header = FALSE)

# PSM Sample --------------------------------------------------------------

# State unclustered
m1 <- lfe::felm(turnover ~ post_strike  | state + year_fac | 0 | 0, 
                data = df_state %>% filter(sample == "PSM Matched Districts"))

# State clustered
m2 <- lfe::felm(turnover ~ post_strike  | state + year_fac | 0 | state + year_fac, 
                data = df_state %>% filter(sample == "PSM Matched Districts"))

# state 2 year by 2 unit collapse
m3 <- lfe::felm(turnover ~ post_strike | state + year  | 0 | 0,
                data = df_state %>% filter(sample == "PSM Matched Districts") %>% 
                  group_by(state, post_strike, year = year > 0) %>% 
                  summarize(turnover = mean(turnover))) 

# District unclustered                
m4 <- lfe::felm(turnover ~ post_strike  | psm_state + year_fac | 0 | 0, 
                data = df_district_psm, 
                weight = df_district_psm$reg_weight_psm)

# District with state year
m5 <- lfe::felm(turnover ~ post_strike  | psm_state + year_fac | 0 | psm_state + year_fac, 
                data = df_district_psm, 
                weight = df_district_psm$reg_weight_psm)

# District with year and county
m6 <- lfe::felm(turnover ~ post_strike  | psm_state + year_fac | 0 | psm_group + year_fac, 
                data = df_district_psm, 
                weight = df_district_psm$reg_weight_psm)

p3 <- stargazer(m1, m2, m3, m4, m5, m6,
                type = sg_type,
                omit.stat = sg_omit_stats,
                star.cutoffs = sg_stars, 
                header = FALSE)

# Stick em together -------------------------------------------------------


# Note - had to hand move the observation level, clustering, and pop weights lines 
# to the top of the table
star.panel.out <- star_panel(p1, p2, p3, same.summary.stats = FALSE,
                             panel.names = c("Full Sample", "Border Counties", "Propensity Score Matched Districts")
)

clipr::write_clip(star.panel.out)