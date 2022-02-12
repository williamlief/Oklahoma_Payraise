library(readxl)
library(tidyverse)

nces <- readRDS("data/nces_data.rds") %>%
  filter(state == "TX") %>%
  select(localid, NCES_leaid) %>%
  mutate(localid = toupper(localid)) %>%
  distinct() %>% 
  filter(!is.na(localid))

# TX_snapshot - Snapshot files -------------------------------------------------
files <- list.files(path = "data-raw/Texas/Snapshot", 
                    pattern = "district.xls", 
                    recursive = TRUE,
                    full.names = TRUE)

readfun <- function(input_file) {
  df <- read_excel(input_file, na = ".", col_types = "text") 
  
  year_variable <- grep("DA0AT", names(df), value = TRUE)
  year <- as.numeric(substr(year_variable, 6,7))
  df$year <- if_else(year > 90, year + 1901, year + 2001) # this will break in 2090
  
  return(df)
}

stack  <- files %>% map_dfr(readfun)
stack2 <- stack %>%
  select(
    year,
    distname = `DISTNAME`,
    district = `DISTRICT`,
    tot_staff_fte = `DPSATOFC`,
    tot_teacher_fte = `DPSTTOFC`,
    stf_pct_cent_admin = `DPSCTOFP`,
    stf_pct_school_admin = `DPSSTOFP`,
    stf_pct_prof_support = `DPSUTOFP`,
    stf_pct_teachers = `DPSTTOFP`,
    stf_pct_edu_aides = `DPSETOFP`,
    stf_pct_aux = `DPSXTOFP`,
    avgsal_cent_admin = `DPSCTOSA`,
    avgsal_school_admin = `DPSSTOSA`,
    avgsal_prof_support = `DPSUTOSA`,
    avgsal_teachers = `DPSTTOSA`,
    stf_pct_minority = `DPSAMIFP`,
    students_per_staff = `DPSAKIDR`,
    students_per_teacher = `DPSTKIDR`,
    tch_pct_under5yrsexp = `DPST05FP`,
    tch_avg_yrs_exp = `DPSTEXPA`,
    tch_pct_adv_deg = `DPSTADFP`,
    tch_turnover_rate = `DPSTURNR`,
    tch_pct_afr_american = `DPSTBLFP`,
    tch_pct_hispanic = `DPSTHIFP`,
    tch_pct_white = `DPSTWHFP`,
    tch_pct_other = `DPSTO2FP`,
    tch_pct_reg_ed = `DPSTREFP`,
    tch_pct_special_ed = `DPSTSPFP`,
    tch_pct_compensate_ed = `DPSTCOFP`,
    tch_pct_bil_esl_ed = `DPSTBIFP`,
    tch_pct_career_tech_ed = `DPSTVOFP`,
    tch_pct_other_ed = `DPSTGOFP`)

# Merge on NCES ids
stack3 <- stack2 %>% 
  mutate(district = str_pad(district, 6, "left", 0)) %>% 
  left_join(nces, by = c("district" = "localid"))

# very few missing, should be fine
test <- stack3 %>% filter(is.na(NCES_leaid), year >= 2007) 

# Clean up variables
stack4 <- stack3 %>% 
  mutate_at(vars(-distname), as.numeric)

saveRDS(stack4, "Data/TX_Snapshot.rds")

rm(files, stack, stack2, stack3, stack4, test, readfun)

# TX_APR - Academic Performance Reports ----------------------------------------

files <- list.files(path = "data-raw/Texas/AcademicPerformanceReports", 
                    pattern = "DSTAF.xls", 
                    recursive = TRUE,
                    full.names = TRUE)
# What the what? These files aren't excel files, instead they are html tables that 
# are given an xls extension. 
# year is not derived from data, assumes my file directory structure is correct.
readfun <- function(input_file) {
  print(input_file)
  options(stringsAsFactors = FALSE)
  df <-  XML::readHTMLTable(input_file, colClasses = "character", 
                            header = TRUE, 
                            as.data.frame = TRUE, 
                            which = 1)

  year_variable <- stringr::str_extract(input_file, "\\d{4}")
  df$year <- as.numeric(year_variable) + 1

  return(df)
}

stack  <- files %>% map_dfr(readfun)
stack2 <- stack %>%
  select(
    year,
    district = `DISTRICT`,
    tot_staff_fte = `DPSATOFC`,
    tot_teacher_fte = `DPSTTOFC`,
    stf_pct_cent_admin = `DPSCTOFP`,
    stf_pct_school_admin = `DPSSTOFP`,
    stf_pct_prof_support = `DPSUTOFP`,
    stf_pct_teachers = `DPSTTOFP`,
    stf_pct_edu_aides = `DPSETOFP`,
    stf_pct_aux = `DPSXTOFP`,
    avgsal_cent_admin = `DPSCTOSA`,
    avgsal_school_admin = `DPSSTOSA`,
    avgsal_prof_support = `DPSUTOSA`,
    avgsal_teachers = `DPSTTOSA`,
    stf_pct_minority = `DPSAMIFP`,
#   students_per_staff = `DPSAKIDR`,
    students_per_teacher = `DPSTKIDR`,
#   tch_pct_under5yrsexp = `DPST05FP`,
    tch_avg_yrs_exp = `DPSTEXPA`,
#   tch_pct_adv_deg = `DPSTADFP`, 
    tch_turnover_rate = `DPSTURNR`,
    tch_pct_afr_american = `DPSTBLFP`,
    tch_pct_hispanic = `DPSTHIFP`,
    tch_pct_white = `DPSTWHFP`,
#   tch_pct_other = `DPSTO2FP`,
    tch_pct_reg_ed = `DPSTREFP`,
    tch_pct_special_ed = `DPSTSPFP`,
    tch_pct_compensate_ed = `DPSTCOFP`,
    tch_pct_bil_esl_ed = `DPSTBIFP`,
    tch_pct_career_tech_ed = `DPSTVOFP`,
#   tch_pct_other_ed = `DPSTGOFP`
)

# Merge on NCES ids
stack3 <- stack2 %>% 
  mutate(district = gsub("'", "", district)) %>% 
  left_join(nces, by = c("district" = "localid"))

# very few missing, should be fine
test <- stack3 %>% filter(is.na(NCES_leaid), year >= 2007) 

# Clean up variables
stack4 <- stack3 %>%
  mutate_all(list(~na_if(., "."))) %>% 
  mutate_all(as.numeric)

saveRDS(stack4, "Data/TX_APR.rds")