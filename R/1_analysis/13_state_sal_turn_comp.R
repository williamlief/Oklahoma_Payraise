# Figure 5

library(tidyverse)
library(ggpubr)

WIDTH = 6
HEIGHT = 4

df_state <- readRDS("Data/clean_state_2021-01-11_.rds") %>% 
  filter(year <= 2020,
         year > 2008, 
         state %in% c("TX", "OK", "KS", "PSMatched")) %>% 
  mutate(turnover = turnover * 100,
         year = year - 2018)

p_sal <- ggplot(data = df_state %>% filter(sample == "Full State") %>% 
                  mutate(label = if_else(year == max(year), state, NA_character_)), 
                aes(x= factor(year), y = avg_sal, color = state, group = state)) + 
  geom_line() +
  geom_vline(xintercept = 10, linetype = "dashed", color = "black") +
  labs(title = "Panel A: Teacher Salaries", x = "", y = "Nominal Salary") +
  theme_bw() +
  scale_color_brewer(palette = "Set2") +
  coord_cartesian(clip = 'off', ylim = c(35000, 60000)) +
  geom_text(aes(label = label), hjust = 0, nudge_x = 0.1, na.rm = T) +
  theme(legend.position = 'none',
        plot.margin = margin(0.1, 2.6, 0.1, 0.1, "cm")) 


p_turn <- ggplot(data = df_state %>%
                   filter(sample == "Full State") %>% 
                   mutate(label = if_else(year == max(year), state, NA_character_)), 
                 aes(x= factor(year), y = turnover, color = state, group = state)) + 
  geom_line() +
  geom_vline(xintercept = 10, linetype = "dashed", color = "black") +
  labs(title = "Panel B: Teacher Turnover", x = "", y = "Teacher Turnover Rate") +
  theme_bw() +
  scale_color_brewer(palette = "Set2") +
  coord_cartesian(clip = 'off', ylim = c(0, 17)) +
  geom_text(aes(label = label), hjust = 0, nudge_x = 0.1, na.rm = T) +
  theme(legend.position = 'none',
        plot.margin = margin(0.1, 2.6, 0.1, 0.1, "cm")) 

ggpubr::ggarrange(p_sal, p_turn, 
                  nrow = 2)
# Labels got screwed up
# Manually saved width = 811 height = 545
# ggsave("figures/state_sal_turn_comp.png", bg = "white", width = WIDTH, height = HEIGHT)
