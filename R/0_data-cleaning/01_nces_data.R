# Clean up the NCES data - side note, why is this data in such a terrible form?
# I manually downloaded from the table generator https://nces.ed.gov/ccd/elsi/tableGenerator.aspx
# Had to split into multiple downloads because it kept timing out

library(tidyverse)

files <- list.files("data-raw/nces", "*.csv", full.names = TRUE)

raw_all <- map(files, read_csv,
           skip = 6,
           col_types = cols(.default = 'c'),
           na = c("", "NA", "‡", "–", "†"))

raw <- reduce(raw_all, left_join)

all_names <- names(raw)
pivot_names <- all_names[grepl('\\d{4}-\\d{2}', all_names)]

df2 <- raw %>% 
  pivot_longer(
    cols = all_of(pivot_names),
    names_to = c(".value", "year"),
    names_pattern = "(.+)(\\d{4}-\\d{2})"
  ) 

df3 <- df2 %>% 
  select(
    year,
    "lea_name" = "Agency Name", 
    "localid" = "State Agency ID [District] ",
    "state" = "State Abbr [District] Latest available year", 
    "county" = "County Name [District] ",
    "NCES_ID" = "Agency ID - NCES Assigned [District] Latest available year",
    "agency_type" = "Agency Type [District] ",
    "locale" = "Locale [District] ",
    "students_total" = "Total Students All Grades (Excludes AE) [District] " ,
    "students_lep" = "Limited English Proficient (LEP) / English Language Learners (ELL) [District] ", 
    "students_iep" = "Individualized Education Program Students [District] ", 
    "students_male" = "Male Students [District] ", 
    "students_female" = "Female Students [District] ",
    "students_nativeAmerican" = "American Indian/Alaska Native Students [District] ",
    "students_asian" = "Asian or Asian/Pacific Islander Students [District] ",
    "students_hispanic" = "Hispanic Students [District] ",
    "students_black" = "Black or African American Students [District] ",
    "students_white" = "White Students [District] ",
    "students_hawaiin" = "Nat. Hawaiian or Other Pacific Isl. Students [District] ",
    "students_twoOrMore" = "Two or More Races Students [District] ",
    "teacher_fte" = "Full-Time Equivalent (FTE) Teachers [District] ",
    "student_teacher_ratio" = "Pupil/Teacher Ratio [District] "
  )


var_cleanup <- function(x) {
  x <- gsub('=\"', "", x)
  x <- gsub('\"', "", x)
  x <- gsub('"', "", x)
  return(x)
}

df4 <- df3 %>% 
  mutate_all(var_cleanup) %>% 
  mutate_all(tolower) %>% 
  mutate_at(vars(starts_with("student"),
                 starts_with("teacher"),
                 NCES_ID),
            as.numeric) %>% 
  mutate(year = as.numeric(substring(year,6,7)) + 2000,
         county = tolower(gsub(" county.*", "", county)), 
         state = toupper(state),
         p_students_lep = students_lep / students_total, 
         p_students_iep = students_iep / students_total, 
         p_students_white = students_white / students_total, 
         p_students_black = students_black / students_total, 
         p_students_hispanic = students_hispanic / students_total, 
         p_students_other = (students_nativeAmerican + students_asian + students_hawaiin +
                               students_twoOrMore) / students_total,
         agency_type = as.factor(agency_type),
         locale = as.factor(locale)) %>% 
  rename("NCES_leaid" = "NCES_ID") 

df5 <- df4 %>% 
  arrange(NCES_leaid, -year) %>% 
  group_by(NCES_leaid) %>% 
  fill(county) %>% 
  ungroup()

saveRDS(df5, "Data/nces_data.rds")


