## Defintions of variables in TX Data
## Mimic these in OK data aggregation from individual to district-year
## From Snapshot 2018: Item Definitions
## https://rptsvr1.tea.texas.gov/perfreport/snapshot/2018/itemdef.html

## n
# 43. Total Teacher FTE: The FTE count of personnel categorized as teachers,
# including special duty and permanent substitute teachers. Substitute teachers
# should not be confused with persons hired on a daily basis to substitute
# teach. (Source: TSDS PEIMS, 30050, 30090)

## avg_sal 
# 53. Average Teacher Salary: The sum of all the salaries of teachers divided by
# the total FTE count of teachers. The salary amount is pay for regular duties
# only; supplemental payments for activities such as coaching, band and
# orchestra assignments, and club sponsorships are excluded. (Source: TSDS
# PEIMS, 30050, 30060, 30090)


## avg_exp
# 58. Teacher Average Years of Experience: A weighted average obtained by
# multiplying each teacher's FTE count by his or her years of experience,
# summing for all weighted counts, and then dividing by total teacher FTEs.
# Adjustments are made so that teachers with zero years of experience are
# appropriately weighted in the formula. (Source: TSDS PEIMS, 30040, 30050,
# 30090)

# turnover
# 60. The FTE count of teachers not employed in the district in the fall of
# 2017-18, who were employed in the district in the fall of 2016-17, divided by
# the teacher FTE count for the fall of 2016-17. Teachers who continue
# employment with a district but in a role other than teaching are also
# included in the turnover rate. Social security numbers of reported teachers
# are checked to verify their employment status in the same district in the
# fall of 2016-17. (Source: TSDS PEIMS, 30050, 30090)

# p_black
# 61. Teacher % African American: The FTE count of teachers reported as African
# American expressed as a percentage of the total teacher FTE count. (Source:
# TSDS PEIMS, 30040, 30050, 30090)

# p_hispanic
# 62. Teacher % Hispanic: The FTE count of teachers reported as Hispanic
# expressed as a percentage of the total teacher FTE count. (Source: TSDS PEIMS,
# 30040, 30050, 30090)

# p_white
# 63. Teacher % White: The FTE count of teachers reported as white expressed as
# a percentage of the total teacher FTE count. (Source: TSDS PEIMS, 30040,
# 30050, 30090)


# Set up -----------------------------------------------------------------------

library(tidyverse)


# Clean and Aggregate OK Data --------------------------------------------------

OK <- readRDS("Data/OK_Payroll.rds") %>% 
  filter(group == "teacher")

# turnover is # not in a classroom the next year / total number, pushed up one year.
# That is turnover in 2018-19 is the number of teachers in 2017-18 who are not in the 
# classroom in 2018-19, divided by the total number from 2017-18. Non classroom jobs
# count as turnover
OK_turnover <- OK %>% 
  group_by(NCES_leaid, source_id) %>% 
  arrange(source_id, year) %>% 
  mutate(turnover = is.na(lead(year)) | lead(year) != year + 1) %>% 
  group_by(NCES_leaid, year) %>% 
  summarize(
    n = sum(fte),
    turnover = sum(fte * turnover) / n
    ) %>% 
  mutate(year = year + 1) %>% 
  filter(year != max(OK$year) + 1) %>%
  select(-n)

OK2 <- OK %>% 
  group_by(NCES_leaid, year) %>% 
  summarize(
    n = sum(fte),
    avg_sal = sum( salary_base * fte ) / sum(fte), 
    p_le5_yr_exp = sum((experience_total <= 5) * fte) / sum(fte),
    avg_exp = sum(experience_total * fte) / sum(fte),
    p_adv_deg = sum(fte * (degree_desc %in% c("Doctor", 
                                              "Doctor of Medicine",
                                              "Master"))) / sum(fte), 
    p_black = sum(fte * (race_code == 1)) / sum(fte), 
    p_white = sum(fte * (race_code == 5)) / sum(fte), 
    p_hispanic = sum(fte * (race_code == 3)) / sum(fte),
    p_other = sum(fte * (race_code %in% c(2, 4, 6, 7))) / sum(fte)
    ) %>% 
  left_join(OK_turnover) %>% 
  mutate(state = "OK")

# Get the TX data, rename ------------------------------------------------------

APR <- readRDS("Data/TX_APR.rds")
Snapshot <- readRDS("Data/TX_Snapshot.rds")

# Snapshot has more details, but is a year behind APR
TX <- APR %>% filter(year == 2022) %>%
  bind_rows(Snapshot)


TX2 <- TX %>% 
  select(
    NCES_leaid,
    year,
    n = tot_staff_fte,
    avg_sal = avgsal_teachers,
    avg_exp = tch_avg_yrs_exp, 
    turnover = tch_turnover_rate,
    p_black = tch_pct_afr_american,
    p_white = tch_pct_white, 
    p_hispanic = tch_pct_hispanic, 
  ) %>% 
  mutate_at(vars(turnover, p_black, p_white, p_hispanic), 
            list(~ . / 100)) %>% 
  mutate(state = "TX") %>% 
  filter(year >= 2007) 

# Stack  em up, filter out some NA require observed in all years ---------------

df <- 
  bind_rows(OK2, 
            TX2) %>% 
select(-c(p_le5_yr_exp, 
          p_adv_deg, 
          p_other)) %>% 
  filter(!is.na(NCES_leaid),
         !is.na(turnover),
         !is.na(avg_sal),
         !is.na(p_black),
         year >= 2008) %>% 
  rename(n_teachers = n) %>% 
  group_by(state) %>% 
  add_count(NCES_leaid, name = "count_obs") %>% 
  filter(count_obs == max(count_obs)) %>% 
  ungroup()

df %>% count(state, year) %>% 
  pivot_wider(names_from = state, values_from = n) %>% 
  arrange(year)

# Add on NCES Data -------------------------------------------------------------

nces <- readRDS("Data/nces_data.rds") %>% 
  mutate(nces = TRUE) %>% 
  select(year, state, NCES_leaid, agency_type, locale, starts_with("p_students"),
         students_total, student_teacher_ratio, teacher_fte, nces, county)

df2 <- df %>% 
  tidylog::left_join(nces, by = c("year", "state", "NCES_leaid")) 
# NCES data not yet available for 2023

# Add on BLS Data --------------------------------------------------------------

BLS_raw <- readRDS("Data/BLS_County_Unemployment.rds") 

BLS <- BLS_raw %>% 
  mutate(county = tolower(gsub(" County.*", "", county_name)),
         state = case_when(state_code == "48" ~ "TX", 
                           state_code == "40" ~ "OK",
                           state_code == "20" ~ "KS"),
         bls = TRUE) %>% 
  select(year, state, county, unemployment_rate, bls) 

# Match BLS calendar year to spring of school year
df3 <- df2 %>% 
  tidylog::left_join(BLS, by = c("year", "county", "state"))
# BLS data not yet available for 2023

# merge on border counties -----------------------------------------------------

county <- readxl::read_excel("data-raw/border_counties.xlsx") %>% 
  mutate(border = 1) %>% 
  filter(border_tx == 1 | border_ks == 1) %>% # only keep texas/kansas border
  select(county = County, state = State, starts_with("border"))

df4 <- df3 %>% 
  tidylog::left_join(county, by = c("state", "county")) %>% 
  mutate(border = ifelse(is.na(border), 0, border))

x <- county %>% anti_join(df3) # check non matches

# Define matched sample --------------------------------------------------------

df_to_match <- df4 %>% 
  filter(year == 2018, 
         state %in% c("OK", "KS", "TX")) %>% 
  mutate(OK = state == "OK") %>% 
  tidylog::filter(!is.na(n_teachers), !is.na(avg_exp), !is.na(turnover))

library(MatchIt)
match.it <- matchit(OK ~ n_teachers + avg_exp + turnover, 
                    data = df_to_match, method = "nearest", ratio = 1 )

a <- summary(match.it)
plot(match.it, type = 'jitter', interactive = FALSE) 
# Manually saved as dist_prop_scores.png

df_matched <- match.data(match.it)

df5 <- df4 %>% left_join(
  df_matched %>% select(NCES_leaid, psm_group = subclass)
)

# clean up data ----------------------------------------------------------------

df6 <- df5 %>% 
  mutate(post_strike = as.numeric(year > 2018 & state == "OK"),
         county = as.factor(county), 
         state = as.factor(state), 
         NCES_leaid = as.factor(NCES_leaid), 
         year_fac = as.factor(year) 
         ) %>% 
  select(state, year, year_fac, NCES_leaid, agency_type, locale, county, post_strike,
         border, psm_group, # district categorizing vars
         teacher_fte = n_teachers, turnover, avg_sal, avg_exp, # key teacher vars
         p_black, p_white, p_hispanic, # teacher workforce vars
         nces, teacher_fte_nces = teacher_fte, students_total, student_teacher_ratio, starts_with("p_students"), # nces vars
         bls, unemployment_rate
         ) 

# Create weight variables for regressions
weight_full <- df6 %>% 
  group_by(state, year) %>% 
  mutate(reg_weight_full = teacher_fte / sum(teacher_fte)) %>% 
  ungroup()

weight_border <- df6 %>% 
  filter(border == 1) %>% 
  group_by(state, year) %>% 
  mutate(reg_weight_border = teacher_fte / sum(teacher_fte)) %>% 
  ungroup()

weight_psm <- df6 %>% 
  mutate(psm_state = if_else(state == "OK", "OK", "Match")) %>% 
  filter(!is.na(psm_group)) %>% 
  group_by(psm_state, year) %>% 
  mutate(reg_weight_psm = teacher_fte / sum(teacher_fte)) %>% 
  ungroup()

district <- df6 %>% 
  tidylog::left_join(weight_full) %>% 
  tidylog::left_join(weight_border) %>% 
  tidylog::left_join(weight_psm)

# collapse to state
state <- district %>% 
  group_by(state, year, year_fac) %>% 
  summarize_at(vars(post_strike, turnover, avg_sal, avg_exp, starts_with("p_"), 
                    students_total, student_teacher_ratio, unemployment_rate), 
               list(~weighted.mean(., w = teacher_fte, na.rm = TRUE))) %>% 
  mutate(sample = "Full State") %>% 
  # subset to border
  bind_rows(district %>% 
              filter(border == 1, state %in% c("OK", "KS", "AR", "TX")) %>% 
              group_by(state, year, year_fac) %>% 
              summarize_at(vars(post_strike, turnover, avg_sal, avg_exp, starts_with("p_"), 
                                students_total, student_teacher_ratio, unemployment_rate), 
                           list(~weighted.mean(., w = teacher_fte, na.rm = TRUE))) %>% 
              mutate(sample = "Border Counties") 
  ) %>% 
  # subset to matched 
  bind_rows(district %>%
              filter(!is.na(psm_group)) %>%
              mutate(state = if_else(state == "OK", "OK", "PSMatched")) %>% 
              group_by(state, year, year_fac) %>%
              summarize_at(vars(post_strike, turnover, avg_sal, avg_exp, starts_with("p_"),
                                students_total, student_teacher_ratio, unemployment_rate),
                           list(~weighted.mean(., w = teacher_fte, na.rm = TRUE))) %>%
              mutate(sample = "PSM Matched Districts")
  ) %>%
  ungroup() 

# Save ------------------------------------------------------------------------

saveRDS(district, "Data/clean_district.rds")
saveRDS(state, "Data/clean_state.rds")