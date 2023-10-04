
library(tidyverse)

# Read in Files ----------------------------------------------------------------

list <- list.files(path = "data-raw/OK", 
                   pattern = ".xlsx", full.names=TRUE) 

readfun <- function(input_file) {
  print(input_file)
  df <- readxl::read_excel(input_file, skip = 1, col_types = "text") 
  df$year <- 2001 + as.numeric(substr(stringr::str_extract(input_file, regex("FY\\d\\d")), 3, 4))
  
  df <- df %>% 
    mutate(year = if_else(year == 2024, 2023, year)) # changed file naming convention, breaking year identification
  
  return(df)
}

files <- list %>% map_dfr(readfun)

# Combine files & variables-----------------------------------------------------

df <- files %>% 
  mutate(
    source_id = coalesce(TeacherNumber, `Teacher Number`),
    source_id_2 = coalesce(staff_id, `staff id`),
    name_first = tolower(coalesce(fname, firstname, `First Name`)),
    name_last = tolower(coalesce(lname, lastname, `Last Name`)),
    county_name = coalesce(county_name, `County Name`),
    district_name = coalesce(district_name, `District Name`),
    school_name = coalesce(school_name, `School Name`),
    degree_desc = coalesce(degree_desc, `degree desc`),
    race_desc = coalesce(race_desc, `race desc`),
    subject_desc = coalesce(subject_desc, `subject desc`),
    co = coalesce(co, County),
    dist = coalesce(dist, District),
    site = coalesce(site, Site),
    localid = paste("OK", co, dist, sep = "-"),
    school_id = paste(co, dist, site, sep = "-"),
    gender = tolower(gender),
    experience_total = coalesce(total_experience, tot_exper, `Total Experience`),
    fte = coalesce(fte, FTE, `Federal FTE`),
    salary_base = as.numeric(coalesce(base_salary, `base salary`)), 
    salary_fringe = as.numeric(coalesce(total_fringe, `total fringe`)),
    salary_other = as.numeric(coalesce(total_oth_salary, `Other pay`, `other pay`)),
    salary_extra_duty = as.numeric(coalesce(total_extra_duty, `extra_duty pay`, `extra duty pay`)),
    reason_for_leaving_desc = coalesce(reason_for_leaving, Reason_for_Leaving, `Reason for Leaving`),
    reason_for_leaving_code = coalesce(`Reason For Leaving  (RFL_code)`, rfl_code, `Reason For Leaving  (RFL_code)`)
    ) %>% 
  rename(county_id = co, 
         position = jobdesc,
         position_code = jobcode,
         race_code = race,
         degree_code = degree,
         subject_code = subject
         ) %>% 
  select(year, source_id, source_id_2, name_first, name_last, 
         county_name, county_id, localid, district_name, school_id, school_name,
         race_code, race_desc, gender, degree_code, degree_desc,
         position, position_code, subject_code, subject_desc, 
         experience_total, fte, starts_with("salary"), 
         reason_for_leaving_desc, reason_for_leaving_code) 
  
# Reduce to one record per employee --------------------------------------------
# keep highest FTE, highest salary, arbitrary
# count number of dists, schools, positions, subjects
# get rid of TOTAL OTHER PAY, TOTAL EXTRA DUTY PAY so dont double count

df2 <- df %>% 
  filter(!(position %in% c("TOTAL OTHER PAY", "TOTAL EXTRA DUTY PAY"))) %>% 
  arrange(source_id, year, desc(fte), desc(salary_base)) %>% 
  group_by(source_id, year) %>% 
  mutate(
    n_districts = n_distinct(localid, na.rm = TRUE),
    n_schools   = n_distinct(school_name, na.rm = TRUE),
    n_position  = n_distinct(position, na.rm = TRUE),
    n_subject   = n_distinct(subject_code, na.rm = TRUE),
    salary_base = sum(salary_base, na.rm = TRUE),
    salary_fringe = sum(salary_fringe, na.rm = TRUE),
    salary_other = sum(salary_other, na.rm = TRUE),
    salary_extra_duty = sum(salary_extra_duty, na.rm = TRUE)
  ) %>% 
  filter(row_number() == 1) %>% 
  ungroup()

# Merge on NCES ----------------------------------------------------------------

nces <- readRDS("data/nces_data.rds") %>%
  filter(state == "OK") %>%
  select(localid, NCES_leaid) %>%
  mutate(localid = toupper(localid)) %>%
  distinct() 

df3 <- df2 %>% 
  left_join(nces, by = "localid")
if (nrow(df2) != nrow(df3)) warning("the nces merge inflated the observation count")


# Finalize and save ------------------------------------------------------------

df3 <- df3 %>% 
  mutate(teacher = position %in% c("TEACHER", "RESOURCE TEACHER"),
         state = "OK",
         source = "OK_DOE")

# Manually map positions 
df3 %>% group_by(position) %>%
  count(sort = TRUE) %>% 
  write_csv("data-raw/OK/positions_raw.csv")

# merge on position mappings - `group` variable
pos <- read_csv("data-raw/OK/positions_map.csv")

df4 <- df3  %>% 
  mutate(
    source_id = case_when(
      year %in% c(2011, 2012) ~ stringr::str_pad(source_id, 6, pad = "0", side = "left"),
      TRUE ~ source_id)) %>% 
  left_join(pos, by = "position") %>% 
  rename(group = Group) %>% 
  mutate(across(c(race_code, degree_code, position_code, experience_total, fte,
                  source_id_2, NCES_leaid), as.numeric))

saveRDS(df4, "data/OK_Payroll.rds")
