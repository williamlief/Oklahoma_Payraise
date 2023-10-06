# Table 1

library(tidyverse)

df_district <- readRDS("Data/clean_district.rds") %>%
  filter(year <= 2023,
         year > 2008, 
         state %in% c("OK")) %>% 
  mutate(turnover = turnover * 100, 
         year = year - 2018)

tab <- function(dat) {
  
  dat %>% 
    summarize(across(c(teacher_fte, turnover, avg_sal, avg_exp, p_white, 
                       students_total, p_students_lep, p_students_white, 
                       students_total, student_teacher_ratio, unemployment_rate), 
                     c("mean" = mean,  "sd" = sd, "median" = median, 
                       "min" = min, "max" = max), na.rm = T, 
                     .names = "{.fn}__{.col}")
    ) %>% 
    pivot_longer(everything(), names_to = c("fun", "var"), names_sep = "__") %>% 
    pivot_wider(names_from = "fun", values_from = "value") %>% 
    bind_rows(
      dat %>% summarize(mean = min(year + 2018),
                        sd = max(year + 2018)) %>% 
        mutate(var = "Observed Years"),
      dat %>% summarize(mean = n()) %>% 
        mutate(var = "Number of District-Years")
    )
}

taba <- df_district %>% 
  filter(state == "OK", year <= 0) %>% 
  tab()

tabb <- df_district %>% 
  filter(state == "OK", year > 0) %>% 
  tab()

options(knitr.kable.NA = '')

knitr::kable(
  bind_rows(
    data.frame(var = "Panel A: Pre-Strike Data"), 
    taba, 
    data.frame(var = ""), 
    data.frame(var = "Panel B: Post-Strike Data"),
    tabb), 
  format = 'latex',
  digits = 2,
  caption = "Oklahoma teacher statistics, pre-strike Panel A, post-strike Panel B",
  label = "tab:ok_desc"
) %>% clipr::write_clip()