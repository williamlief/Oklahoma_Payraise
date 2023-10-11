# Figures 1-4

library(tidyverse)
library(ggrepel)  
library(scales)
library(ggridges)

# note - this is a now archived package used to format scales separately by
# facet, not strictly needed. See facet_grid_sc calls and swap for commented
# out calls to facet_grid if you don't want to install it.
# devtools::install_github("zeehio/facetscales", ref = "1bd739b34b64f4ae8efe45dcb07911b9c0490d8e")
library(facetscales)

WIDTH = 6
HEIGHT = 4

df_payroll <- readRDS("Data/OK_Payroll.rds") %>% 
  mutate(salary_total = salary_base + salary_fringe + 
           salary_other + salary_extra_duty)



# Turnover definitions ----------------------------------------------------

turn_pipe <- function(dat, ...) {
  micro <- dat %>% 
    select(group, year, source_id, fte, ...) %>% 
    group_by(..., source_id) %>% 
    arrange(source_id, year) %>% 
    mutate(turnover = is.na(lead(year)) | lead(year) != year + 1,
           year = year + 1) %>% 
    ungroup() 
  
  
  micro %>%
    filter(group == "teacher") %>% 
    group_by(year) %>% 
    summarize(
      turnover = weighted.mean(turnover, w = fte, na.rm = TRUE),
      .groups = "drop"
    )
}

turns <- list()

# teacher-state
turns[["Teaching job, state"]] <- df_payroll %>% 
  filter(group == "teacher") %>% 
  turn_pipe()

# State
turns[["Any job, state"]] <- df_payroll %>% 
  turn_pipe()

# teacher-district !! standard
turns[["Teaching job, district"]] <- df_payroll %>% 
  filter(group == "teacher") %>%
  turn_pipe(NCES_leaid)

# district
turns[["Any job, district"]] <- df_payroll %>% 
  turn_pipe(NCES_leaid)

# teacher-school 
turns[["Teaching job, school"]] <- df_payroll %>% 
  filter(group == "teacher") %>% 
  turn_pipe(school_id)

# school 
turns[["Any job, school"]] <- df_payroll %>% 
  turn_pipe(school_id)

turn_dat <- bind_rows(turns, .id = "turnover_type")

ggplot(data = turn_dat %>% filter(year != 2024) %>%
         mutate(year = paste0(year-1, "-", year-2000),
                label = if_else(year == max(year), turnover_type, NA_character_)), 
       aes(x = factor(year), y = turnover, 
           color = turnover_type, group = turnover_type)) +
  geom_line() +
  geom_vline(xintercept = 11, linetype = "dashed", color = "black") +
  labs(title = "Teacher turnover: school, district, and state", 
       x = "", 
       y = "Teacher turnover rate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  coord_cartesian(clip = 'off') +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_text(aes(label = label), hjust = 0, nudge_x = 0.1) +
  theme(legend.position = 'none',
        plot.margin = margin(0.1, 3.6, 0.1, 0.1, "cm")) 
    
ggsave("figures/turn_comparison.png", bg = "white", width = WIDTH, height = HEIGHT)

# Subset to just teachers -------------------------------------------------

df_payroll <- df_payroll %>% 
  filter(group == "teacher") 

# State vs District Aggregate Turnover Rates ------------------------------

turn <- df_payroll %>% 
  group_by(NCES_leaid, source_id) %>% 
  arrange(source_id, year) %>% 
  mutate(dist_turnover = is.na(lead(year)) | lead(year) != year + 1) %>% # calc whether left dist
  group_by(source_id) %>% 
  mutate(state_turnover = is.na(lead(year)) | lead(year) != year + 1) # calc whether left state

turn_rates_dist <- turn %>% 
  group_by(NCES_leaid, year) %>% 
  summarize(dist_turnover = mean(dist_turnover), 
            dist_salary = mean(salary_total)) 

turn_rates_dist <- 
  full_join(turn_rates_dist %>% select(-dist_turnover), 
            turn_rates_dist %>% select(-dist_salary) %>% 
              mutate(year = year + 1)) %>% # hack the year up for the turnover rate!
  group_by(NCES_leaid) %>% 
  filter(year != min(year), year != max(year), 
         !is.na(dist_turnover),
         !is.na(dist_salary)) 

turn_rates_state <- turn %>%
  group_by(year) %>% 
  summarize(State_turnover = mean(state_turnover), 
            State_salary = mean(salary_total)) 

turn_rates_state <- 
  full_join(turn_rates_state %>% select(-State_turnover), 
            turn_rates_state %>% select(-State_salary) %>% 
              mutate(year = year + 1)) %>% # hack the year up for the turnover rate!
  filter(year != 2007, year != 2024) 

turn_rates <- turn_rates_state %>% 
  tidylog::full_join(turn_rates_dist %>% 
                       group_by(year) %>% 
                       summarize(
                         District_turn_25th_pctile = quantile(dist_turnover, .25),
                         District_turn_50th_pctile = quantile(dist_turnover, .5),
                         District_turn_75th_pctile = quantile(dist_turnover, .75),
                         District_salary_25th_pctile = quantile(dist_salary, .25),
                         District_salary_50th_pctile = quantile(dist_salary, .5),
                         District_salary_75th_pctile = quantile(dist_salary, .75)))

ggplot(data = turn_rates %>% 
         pivot_longer(-year) %>% 
         mutate(x2 = as.numeric(as.factor(year)),
                year = paste0(year-1, "-", year-2000), 
                type = ifelse(str_detect(name, "salary"), 
                              "Salary", "Turnover"),
                color = str_remove(name, "_salary|_turnover|_turn"),
                color = str_replace_all(color, "_", " "),
                label = if_else(year == max(year), color, NA_character_)), 
       aes(year, value, color = color, group = name)) + 
  geom_line() +
  # facet_grid(rows = vars(type), scales = "free_y", switch = "y") +
  facet_grid_sc(rows = vars(type), switch = "y",
                scales = list(y = list("Salary" = scale_y_continuous(labels = scales::label_dollar()),
                                       "Turnover" = scale_y_continuous(labels = scales::percent_format())))) +
  geom_vline(xintercept = "2017-18", linetype = "dashed", color = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Salaries and turnover rates, state average and district quartiles", 
       x = NULL, y = NULL) +
  coord_cartesian(clip = 'off') +
  geom_text_repel(aes(x = x2, label = label),hjust = 0, direction = "y", na.rm = T, 
                  segment.size = 0, xlim = c(0, 19), nudge_x = 0) +
  theme(legend.position = 'none',
        plot.margin = margin(0.1, 2.6, 0.1, 0.1, "cm"), 
        strip.placement = "outside", panel.spacing = unit(2, "lines"),
        strip.text.y = element_text(size = 13)) 

# Labels got screwed up
# Manually saved width = 825 height = 550
# ggsave("figures/dist_turn.png", bg = "white", width = WIDTH, height = HEIGHT)


# Turnover by Subject -----------------------------------------------------

subj <- df_payroll %>% 
  group_by(subject_desc, year) %>% 
  summarize(n = n(), salary = mean(salary_total)) %>% 
  summarize(n = mean(n), salary = mean(salary)) 
write.csv(subj, "data-raw/OK/subject_positions_raw.csv")
# manually mapped subjects
subj_map <- read.csv("data-raw/OK/subject_positions_map.csv")

df_subject <- df_payroll %>% 
  left_join(
    subj_map %>% select(subject_desc, subject)) %>% 
  mutate(subject_full = factor(subject), 
         subject = fct_lump_n(subject_full, 7)) %>% 
  group_by(source_id) %>% # NOT SUBJECT
  arrange(source_id, year) %>% 
  mutate(subject_turnover = is.na(lead(year)) | lead(year) != year + 1) 

turn_subject <- df_subject %>% 
  group_by(subject, year) %>% 
  summarize(subject_turnover = weighted.mean(subject_turnover, w = fte, na.rm = TRUE),
            subject_salary = weighted.mean(salary_total, w = fte, na.rm = TRUE))

turn_subject <- 
  full_join(turn_subject %>% select(-subject_turnover), 
            turn_subject %>% select(-subject_salary) %>% 
              mutate(year = year + 1)) %>% # hack the year up for the turnover rate!
  filter(year != 2007, year != 2024) 

ggplot(data = turn_subject %>% 
         pivot_longer(-c(subject, year)) %>% 
         mutate(year = paste0(year-1, "-", year-2000), 
                name = if_else(name == "subject_salary", "Salary", "Turnover")), 
       aes(year, value, color = subject, group = subject)) + 
  geom_line() +
  # facet_grid(rows = vars(name), scales = "free_y", switch = "y") +
  facet_grid_sc(rows = vars(name), switch = "y",
                scales = list(y = list("Salary" = scale_y_continuous(labels = scales::label_dollar()),
                                       "Turnover" = scale_y_continuous(labels = scales::percent_format())))) +
  geom_vline(xintercept = "2017-18", linetype = "dashed", color = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90), 
        strip.placement = "outside", panel.spacing = unit(2, "lines"), 
        strip.text.y = element_text(size = 13)) +
  labs(title = "Salaries and turnover rates by subject area", color = "Subject",
       x = NULL, y = NULL)

ggsave("figures/subject_turn.png", bg = "white", width = WIDTH, height = HEIGHT)

# Turnover by Experience --------------------------------------------------

#' new teachers (0 to 1 years of experience), 
#' early career teachers (2-5 year), 
#' mid career teachers (5-10) years, 
#' experienced teachers (10-20 years)
#' senior teachers (21 years or more). 

turn_rate_exp <- turn %>%  
  mutate(experience = cut(experience_total,
                          breaks = c(-Inf, 1, 5, 10, 20, Inf),  
                          labels = c("New (0-1)", "Early career (2-5)", "Mid career (6-10)", "Experienced (11-20)", "Senior (21+)"), 
                          ordered_result = TRUE)) %>% 
  group_by(experience, year) %>% 
  summarize(experience_turnover = mean(state_turnover),
            experience_salary = mean(salary_total))

turn_rate_exp <- 
  full_join(turn_rate_exp %>% select(-experience_turnover), 
            turn_rate_exp %>% select(-experience_salary) %>% 
              mutate(year = year + 1)) %>% # hack the year up for the turnover rate!
  filter(year != 2007, year != 2024) 

ggplot(data = turn_rate_exp %>% 
         pivot_longer(-c(experience, year)) %>% 
         mutate(x2 = as.numeric(as.factor(year)),
                year = paste0(year-1, "-", year-2000),
                name = if_else(name == "experience_salary", "Salary", "Turnover"),
                label = ifelse(year == max(year), as.character(experience), NA)), 
       aes(year, value, color = experience, group = experience)) + 
  geom_line() +
  # facet_grid(rows = vars(name), scales = "free_y", switch = "y") +
  facet_grid_sc(rows = vars(name), switch = "y",
                scales = list(y = list("Salary" = scale_y_continuous(labels = scales::label_dollar()),
                                       "Turnover" = scale_y_continuous(labels = scales::percent_format())))) +
  geom_vline(xintercept = "2017-18", linetype = "dashed", color = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Salary and turnover rates by years of teaching experience", 
       x = NULL, y = NULL) +
  coord_cartesian(clip = 'off') +
  geom_text_repel(aes(x = x2, label = label), hjust = 0, direction = "y", na.rm = T, 
                  segment.size = 0,  xlim = c(0, 19)) +
  theme(legend.position = 'none',
        plot.margin = margin(0.1, 2.7, 0.1, 0.1, "cm"),
        strip.placement = "outside", panel.spacing = unit(2, "lines"),
        strip.text.y = element_text(size = 13))  +
  scale_color_brewer(palette = "Set1")

# Labels got screwed up
# Manually saved width = 825 height = 550
# ggsave("figures/exp_turn.png", bg = "white", width = WIDTH, height = HEIGHT)


# Salary distribution figure ----------------------------------------------

# Note, technically redundant, but put here in case you just want to replicate this 
# figure and missed line 86 

df_payroll <- df_payroll %>% 
  filter(group == "teacher") 

p_excl <- nrow(df_payroll %>% filter(salary_total > 80000 | salary_total < 20000)) / nrow(df_payroll)

ggplot(data = df_payroll,
       aes(y = paste0(year-1, "-", year-2000),
           x = salary_total)) +
  geom_density_ridges() +
  coord_cartesian(xlim = c(20000, 80000)) +
  theme_minimal() +
  scale_x_continuous(labels = label_number(prefix = "$", suffix = "K", scale = 1e-3)) +
  labs(x = "Total Salary", y = NULL, 
       caption = str_wrap("Figure excludes 2.4% of records with salaries < $20K or > $80K. Figure includes all teachers, regardless of full time equivalency level."),
       title = "Distribution of Oklahoma teacher total salaries")

ggsave("figures/salary_distribution_ridge.png", width = WIDTH, heigh = HEIGHT, bg = "white")
