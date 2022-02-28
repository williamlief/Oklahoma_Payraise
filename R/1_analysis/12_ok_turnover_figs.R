# Figures 1-4

library(tidyverse)
library(ggrepel)  

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
turns[["Teacher State"]] <- df_payroll %>% 
  filter(group == "teacher") %>% 
  turn_pipe()

# State
turns[["Any Job State"]] <- df_payroll %>% 
  turn_pipe()

# teacher-district !! standard
turns[["Teacher District"]] <- df_payroll %>% 
  filter(group == "teacher") %>%
  turn_pipe(NCES_leaid)

# district
turns[["Any Job District"]] <- df_payroll %>% 
  turn_pipe(NCES_leaid)

# teacher-school 
turns[["Teacher School"]] <- df_payroll %>% 
  filter(group == "teacher") %>% 
  turn_pipe(school_id)

# school 
turns[["Any Job School"]] <- df_payroll %>% 
  turn_pipe(school_id)

turn_dat <- bind_rows(turns, .id = "turnover_type")

ggplot(data = turn_dat %>% filter(year != 2021) %>%
         mutate(year = paste0(year-1, "-", year),
                label = if_else(year == max(year), turnover_type, NA_character_)), 
       aes(x = factor(year), y = turnover, 
           color = turnover_type, group = turnover_type)) +
  geom_line() +
  geom_vline(xintercept = 11, linetype = "dashed", color = "black") +
  labs(title = "Teacher Turnover: School, District, and State", 
       x = "", 
       y = "Teacher Turnover Rate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  coord_cartesian(clip = 'off') +
  geom_text(aes(label = label), hjust = 0, nudge_x = 0.1) +
  theme(legend.position = 'none',
        plot.margin = margin(0.1, 2.6, 0.1, 0.1, "cm")) 
    
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
  summarize(state_turnover = mean(state_turnover), 
            state_salary = mean(salary_total)) 

turn_rates_state <- 
  full_join(turn_rates_state %>% select(-state_turnover), 
            turn_rates_state %>% select(-state_salary) %>% 
              mutate(year = year + 1)) %>% # hack the year up for the turnover rate!
  filter(year != 2007, year != 2021) 

turn_rates <- turn_rates_state %>% 
  tidylog::full_join(turn_rates_dist %>% 
                       group_by(year) %>% 
                       summarize(
                         dist_turn_p25 = quantile(dist_turnover, .25),
                         dist_turn_p50 = quantile(dist_turnover, .5),
                         dist_turn_p75 = quantile(dist_turnover, .75),
                         dist_salary_p25 = quantile(dist_salary, .25),
                         dist_salary_p50 = quantile(dist_salary, .5),
                         dist_salary_p75 = quantile(dist_salary, .75)))

ggplot(data = turn_rates %>% 
         pivot_longer(-year) %>% 
         mutate(x2 = as.numeric(as.factor(year)) + 1.2,
                year = paste0(year-1, "-", year), 
                type = ifelse(str_detect(name, "salary"), 
                              "salary", "turnover"), 
                color = str_remove(name, "_salary|_turnover|_turn"), 
                label = if_else(year == max(year), color, NA_character_)), 
       aes(year, value, color = color, group = name)) + 
  geom_line() +
  facet_wrap(~type, nrow = 2, scales = "free_y") +
  geom_vline(xintercept = "2017-2018", linetype = "dashed", color = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "State and District Turnover Rates by Year", 
       x = NULL, y = NULL) +
  coord_cartesian(clip = 'off') +
  geom_text_repel(aes(x = x2, label = label), direction = "y", na.rm = T, 
                  segment.size = 0) +
  theme(legend.position = 'none',
        plot.margin = margin(0.1, 2.6, 0.1, 0.1, "cm")) 

# Labels got screwed up
# Manually saved width = 811 height = 545
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
  filter(year != 2007, year != 2021) 

ggplot(data = turn_subject %>% 
         pivot_longer(-c(subject, year)) %>% 
         mutate(year = paste0(year-1, "-", year)), 
       aes(year, value, color = subject, group = subject)) + 
  geom_line() +
  facet_wrap(~name, nrow = 2, scales = "free_y") +
  geom_vline(xintercept = "2017-2018", linetype = "dashed", color = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Turnover Rates by Subject Area", 
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
                          labels = c("New (0-1)", "Early (2-5)", "Mid (6-10)", "Experienced (11-20)", "Senior (21+)"), 
                          ordered_result = TRUE)) %>% 
  group_by(experience, year) %>% 
  summarize(experience_turnover = mean(state_turnover),
            experience_salary = mean(salary_total))

turn_rate_exp <- 
  full_join(turn_rate_exp %>% select(-experience_turnover), 
            turn_rate_exp %>% select(-experience_salary) %>% 
              mutate(year = year + 1)) %>% # hack the year up for the turnover rate!
  filter(year != 2007, year != 2021) 
  
ggplot(data = turn_rate_exp %>% 
         pivot_longer(-c(experience, year)) %>% 
         mutate(x2 = as.numeric(as.factor(year)) + 2,
           year = paste0(year-1, "-", year),
                label = ifelse(year == max(year), as.character(experience), NA)), 
       aes(year, value, color = experience, group = experience)) + 
  geom_line() +
  facet_wrap(~name, nrow = 2, scales = "free_y") +
  geom_vline(xintercept = "2017-2018", linetype = "dashed", color = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "State Turnover Rates by Experience Level", 
       x = NULL, y = NULL) +
  coord_cartesian(clip = 'off') +
  geom_text_repel(aes(x = x2, label = label), direction = "y", na.rm = T, 
                  segment.size = 0, hjust = 0) +
  theme(legend.position = 'none',
        plot.margin = margin(0.1, 2.6, 0.1, 0.1, "cm")) +
  scale_color_brewer(palette = "Set1")

# Labels got screwed up
# Manually saved width = 811 height = 545
# ggsave("figures/exp_turn.png", bg = "white", width = WIDTH, height = HEIGHT)