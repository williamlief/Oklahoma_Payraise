library(readxl)
library(tidyverse)


# parse data --------------------------------------------------------------



raw <- read_excel("data-raw/Arkansas/20231019_231142_comparison_FY_24_34.xlsx",
                  skip = 2, col_types = "text", na = "N/A")

long <- raw %>% 
  pivot_longer(-c(LEA, `District Name`), 
               names_to = c("year", "var"),
               names_pattern = "(20\\d\\d-\\d\\d) (.*)", 
               values_to = "value")

wide <- long %>% 
  filter(var %in% c("Black/African American", 
                    "Hispanic/Latino", 
                    "White",
                    "Percent Attrition",
                    "Total",
                    "Average Salary/Non-Federal Licensed Classroom FTE's",
                    "Average Years of Teacher Experience"
                    )
  ) %>% 
  pivot_wider(names_from = var, values_from = value)

df <- wide %>% 
  rename(district_name = `District Name`, 
         salary = `Average Salary/Non-Federal Licensed Classroom FTE's`, 
         experience = `Average Years of Teacher Experience`,
         black = `Black/African American`,
         hispanic = `Hispanic/Latino`,
         white = White, 
         total = Total,
         turnover = `Percent Attrition`) %>% 
  mutate(black = as.numeric(black), 
         hispanic = as.numeric(hispanic), 
         white = as.numeric(white),
         total = as.numeric(total),
         year = as.numeric(substr(year,1,4)) + 1,
         salary = as.numeric(salary),
         experience = as.numeric(experience),
         turnover = as.numeric(turnover)
         ) %>% 
  mutate(p_black = black / total, 
         p_white = white / total, 
         p_hispanic = hispanic / total)
  
# Quick check of availability

data = df %>%
  group_by(year) %>% 
  summarize(turnover = weighted.mean(turnover, total, na.rm = T),
            salary = weighted.mean(salary, total, na.rm = T)) %>% 
  pivot_longer(-year, names_to = "var", values_to = "val")

ggplot(data = data, aes(year, val)) + geom_line() + facet_wrap(~var, scales = "free_y")

# Only 4 years of turnover, not that helpful. Don't include.
