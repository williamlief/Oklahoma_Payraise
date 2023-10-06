# Figure 5

library(tidyverse)
library(ggpubr)

# note - this is a now archived package used to format scales separately by
# facet, not strictly needed. See facet_grid_sc calls and swap for commented
# out calls to facet_grid if you don't want to install it.
# devtools::install_github("zeehio/facetscales", ref = "1bd739b34b64f4ae8efe45dcb07911b9c0490d8e")
library(facetscales)

WIDTH = 6
HEIGHT = 4

df_state <- readRDS("Data/clean_state.rds") %>% 
  filter(year > 2008, 
         year < 2023,
         state %in% c("TX", "OK", "PSMatched")) %>% 
  mutate(year_c = year - 2018, 
         year_l = if_else(year_c == min(year_c) | year_c == max(year_c), 
                          paste0(year_c, "\n", paste0(year-1, "-", year-2000)),
                          as.character(year_c)))

df_state %>% filter(sample == "Full State") %>% 
  mutate(label = if_else(year == max(year), state, NA_character_)) %>% 
  pivot_longer(c(avg_sal, turnover), names_to = "name", values_to = "value") %>% 
  mutate(name = if_else(name == "avg_sal", "Salary", "Turnover")) %>% 
  ggplot(., 
         aes(x= factor(year_c), y = value, color = state, group = state)) +
  geom_line() +
  #facet_grid(rows = vars(name), scales = "free_y", switch = "y") +
  facet_grid_sc(rows = vars(name), switch = "y",
                scales = list(y = list("Salary" = scale_y_continuous(labels = scales::label_dollar()),
                                       "Turnover" = scale_y_continuous(labels = scales::percent_format())))) +
  geom_vline(xintercept = 10, linetype = "dashed", color = "black") +
  theme_minimal() +
  labs(title = "Salaries and turnover rates, by state", 
       x = NULL, y = NULL) +
  coord_cartesian(clip = 'off') +
  geom_text(aes(label = label) ,hjust = 0,  na.rm = T, 
                nudge_x = 0) +
  theme(legend.position = 'none',
        strip.placement = "outside", panel.spacing = unit(2, "lines"),
        strip.text.y = element_text(size = 13)) +
  scale_color_brewer(palette = "Set2") +
  scale_x_discrete(name = "", breaks = sort(unique(df_state$year_c)), labels = unique(df_state$year_l))
  
ggsave("figures/state_sal_turn_comp.png", bg = "white", width = WIDTH, height = HEIGHT)
