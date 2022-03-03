library(tidyverse)
library(stargazer)
library(starpolishr)
library(lfe)

df_district <- readRDS("Data/clean_district.rds") %>%
  filter(year <= 2020,
         year > 2008) %>% 
  mutate(turnover = turnover * 100, 
         year = year - 2018)

df_district_border <- df_district %>% filter(border == 1)
df_district_psm <- df_district %>% filter(!is.na(psm_group))

df_state <- readRDS("Data/clean_state.rds") %>% 
  filter(year <= 2020,
         year > 2008) %>% 
  mutate(turnover = turnover * 100,
         year = year - 2018)

sg_type = "latex"
sg_stars = c(.05,.01,.001)
sg_omit_stats = c("ser", "rsq")


# Table 2 DD -------------------------------------------------------------------


formula <- as.formula("turnover ~ post_strike  | state + year_fac | 0 | state + year_fac")

m1 <- lfe::felm(formula, 
                data = df_state %>% filter(sample == "Full State"))
m2 <- lfe::felm(formula, 
                data = df_state %>% filter(sample == "Border Counties"))
m3 <- lfe::felm(formula, 
                data = df_state %>% filter(sample == "PSM Matched Districts"))
m4 <- lfe::felm(formula, 
                data = df_district, 
                weight = df_district$reg_weight_full)
m5 <- lfe::felm(formula, 
                data = df_district_border, 
                weight = df_district_border$reg_weight_border)
m6 <- lfe::felm(turnover ~ post_strike  | psm_state + year_fac | 0 | psm_state + year_fac, 
                data = df_district_psm, 
                weight = df_district_psm$reg_weight_psm)


stargazer(m1, m4, m2, m5, m3, m6,
          # ci = TRUE,
          type = sg_type,
          omit.stat = sg_omit_stats,
          title = "Difference in Difference Estimates of Teacher Turnover",
          star.cutoffs = sg_stars, 
          header = FALSE, 
          add.lines = list(c("State Fixed Effects", rep("X", 6)), 
                           c("Year Fixed Effects", rep("X", 6)), 
                           c("Sample", "Full", "Full", "Border", "Border", "PSM", "PSM", "PSM"),
                           c("Population Weights", rep(c("", "X"), 3))),
          notes = c("All models include state and year clustered standard errors.",
                    "Odd numbered models use state macro data, even numbered models use district micro data.",
                    "In models 5, 6 the 'state' is recoded as an indicator for Oklahoma or a counterfactual state."),
          notes.align = "l",
          label = "tab:samples")

finalm_psm <- m6



# Table 3 Covariate Models -----------------------------------------------------
# Note - student demographics not available for 2008-9 or 2009-10

covs <- c("avg_exp", "p_white")
covs2 <- c(covs, "p_students_white", "students_total", "student_teacher_ratio", "unemployment_rate")

formula <- as.formula("turnover ~ post_strike | year_fac + state | 0 | state + year_fac")
cov_formula <- as.formula(paste0(
  "turnover ~ post_strike +", paste(covs, collapse = "+"),
  "| year_fac + state | 0 | state + year_fac"))
cov_formula2 <- as.formula(paste0(
  "turnover ~ post_strike +", paste(covs2, collapse = "+"),
  "| year_fac + state | 0 | state + year_fac"))


m1 <- lfe::felm(cov_formula , data = df_district, weight = df_district$reg_weight_full)
m2 <- lfe::felm(cov_formula2, data = df_district, weight = df_district$reg_weight_full)
m3 <- lfe::felm(cov_formula , data = df_district_border, weight = df_district_border$reg_weight_border)
m4 <- lfe::felm(cov_formula2, data = df_district_border, weight = df_district_border$reg_weight_border)

psm_cov <- as.formula(paste0(
  "turnover ~ post_strike +", paste(covs, collapse = "+"),
  "| year_fac + psm_state | 0 | psm_state + year_fac"))
psm_cov2 <- as.formula(paste0(
  "turnover ~ post_strike +", paste(covs2, collapse = "+"),
  "| year_fac + state | 0 | psm_state + year_fac"))

m5 <- lfe::felm(psm_cov , data = df_district_psm, weight = df_district_psm$reg_weight_psm)
m6 <- lfe::felm(psm_cov2, data = df_district_psm, weight = df_district_psm$reg_weight_psm)


stargazer(m1, m2, m3, m4, m5, m6,
          title = "DD with Covariate Controls",
          type = sg_type,
          omit.stat = sg_omit_stats,
          star.cutoffs = sg_stars, 
          header = FALSE, 
          add.lines = list(c("State Fixed Effects", rep("X", 6)), 
                           c("Year Fixed Effects", rep("X", 6)), 
                           c("Staff Share Weights", rep("X", 6)),
                           c("Sample", "Full", "Full", "Border", "Border", "PSM", "PSM")),
          notes = c("All models include state and year clustered standard errors."), 
          notes.align = "l", 
          label = "tab:covs")

finalm_psmcov2 <- m6


# Table 4 Time Series Models ---------------------------------------------------

add_year_diffs <- function(dat) {
  dat %>% 
    mutate(year_difs = (as.numeric(as.character(year_fac)) * (state == "OK")),
           year_difs = if_else(year_difs %in% 2009:2015, 0, year_difs),
           year_difs = if_else(year_difs %in% 2009:2012, NA_real_, year_difs),
           year_difs = relevel(as.factor(year_difs), ref = "0"))
} 

df_district <- df_district %>% add_year_diffs()
df_district_border <- df_district_border %>%  add_year_diffs()
df_district_psm <- df_district_psm %>% add_year_diffs()

formula <- as.formula("turnover ~ year_difs | year_fac + state | 0 | state + year_fac")
cov_formula <- as.formula(paste0(
  "turnover ~ year_difs +", paste(covs2, collapse = "+"),
  "| year_fac + state | 0 | state + year_fac"))
psm_form <- as.formula("turnover ~ year_difs | year_fac + psm_state | 0 | psm_state + year_fac")
psm_cov <- as.formula(paste0(
  "turnover ~ year_difs +", paste(covs, collapse = "+"),
  "| year_fac + psm_state | 0 | psm_state + year_fac"))


m1 <- lfe::felm(formula , data = df_district %>% filter(year >= -5), weight = df_district %>% filter(year >= -5) %>% pull(reg_weight_full))
m2 <- lfe::felm(cov_formula, data = df_district %>% filter(year >= -5), weight = df_district %>% filter(year >= -5) %>% pull(reg_weight_full))
m3 <- lfe::felm(formula , data = df_district_border %>% filter(year >= -5), weight = df_district_border %>% filter(year >= -5) %>% pull(reg_weight_border))
m4 <- lfe::felm(cov_formula, data = df_district_border %>% filter(year >= -5), weight = df_district_border %>% filter(year >= -5) %>% pull(reg_weight_border))
m5 <- lfe::felm(psm_form , data = df_district_psm %>% filter(year >= -5), weight = df_district_psm %>% filter(year >= -5) %>% pull(reg_weight_psm))
m6 <- lfe::felm(psm_cov, data = df_district_psm %>% filter(year >= -5), weight = df_district_psm %>% filter(year >= -5) %>% pull(reg_weight_psm))

stargazer(m1, m2, m3, m4, m5, m6,
          title = "Time Series Difference in Difference Estimates of Teacher Turnover",
          type = sg_type,
          omit.stat = sg_omit_stats,
          star.cutoffs = sg_stars, 
          header = FALSE, 
          add.lines = list(c("State Fixed Effects", rep("X", 6)), 
                           c("Year Fixed Effects", rep("X", 6)), 
                           c("Staff Share Weights", rep("X", 6)),
                           c("Sample", "Full", "Full", "Border", "Border", "PSM", "PSM"),
                           c("Restricted Covariates", rep(c("", "X"), 3))),
          notes = c("All models include state and year clustered standard errors.",
                    "Covariates include percent of teachers who are white and average experience level in the district.", 
                    "Model includes data from 2013 through 2020, 2013 through 2015 are the baseline years."), 
          notes.align = "l", 
          label = "tab:ddts")


# Heterogeneity analysis --------------------------------------------------


# add groupings 
thresh <- df_district %>% filter(year_fac == 2018, state == "OK") %>% 
  summarize(across(c(avg_sal, avg_exp, turnover), quantile, probs = .33, .names = "{.col}_low"),
            across(c(avg_sal, avg_exp, turnover), quantile, probs = .67, .names = "{.col}_high"))

add_levels <- function(dat) {
  mutate(dat, 
         third_experience = case_when(avg_exp < thresh$avg_exp_low ~ "low", 
                                      avg_exp > thresh$avg_exp_high ~ "high"),
         third_pay = case_when(avg_sal < thresh$avg_sal_low ~ "low", 
                               avg_sal > thresh$avg_sal_high ~ "high"),
         third_turnover = case_when(turnover < thresh$turnover_low ~ "low", 
                                    turnover > thresh$turnover_high ~ "high"),)
}
df_district <- df_district %>% add_levels()
df_district_border <- df_district_border %>% add_levels()
df_district_psm <- df_district_psm %>% add_levels()


formula <- as.formula("turnover ~ post_strike | year_fac + state | 0 | state + year_fac")

cov_formula2 <- as.formula(paste0(
  "turnover ~ post_strike +", paste(covs2, collapse = "+"),
  "| year_fac + state | 0 | state + year_fac"))

psm <- as.formula("turnover ~ post_strike | year_fac + psm_state| 0 | psm_state + year_fac")

psm_cov2 <- as.formula(paste0(
  "turnover ~ post_strike +", paste(covs2, collapse = "+"),
  "| year_fac + state | 0 | psm_state + year_fac"))

het_models <- function(var, cond) {
  
  df_district <- df_district %>% 
    filter(str_detect(.data[[var]], cond)) %>% 
    group_by(state, year_fac) %>% 
    mutate(reg_weight_full = teacher_fte / sum(teacher_fte)) %>% 
    ungroup()
  
  df_district_border <- df_district_border %>%
    filter(str_detect(.data[[var]], cond)) %>% 
    group_by(state, year_fac) %>% 
    mutate(reg_weight_border = teacher_fte / sum(teacher_fte)) %>% 
    ungroup()
  
  df_district_psm <- df_district_psm %>% 
    filter(str_detect(.data[[var]], cond)) %>% 
    group_by(psm_state, year_fac) %>% 
    mutate(reg_weight_psm = teacher_fte / sum(teacher_fte)) %>% 
    ungroup()
  
  list(
    m1 = lfe::felm(formula     , data = df_district, weight = df_district$reg_weight_full),
    m2 = lfe::felm(cov_formula2, data = df_district, weight = df_district$reg_weight_full),
    m3 = lfe::felm(formula     , data = df_district_border, weight = df_district_border$reg_weight_border),
    m4 = lfe::felm(cov_formula2, data = df_district_border, weight = df_district_border$reg_weight_border),
    m5 = lfe::felm(psm         , data = df_district_psm, weight = df_district_psm$reg_weight_psm),
    m6 = lfe::felm(psm_cov2    , data = df_district_psm, weight = df_district_psm$reg_weight_psm)
  )
  
}

het_sg_lines <- list(c("State Fixed Effects", rep("X", 6)), 
                     c("Year Fixed Effects", rep("X", 6)), 
                     c("Staff Share Weights", rep("X", 6)),
                     c("Covariates", rep(c("", "X"), 3)),
                     c("Sample", "Full", "Full", "Border", "Border", "PSM", "PSM"))



# Table 5 Rural/Urban ----------------------------------------------------------

ms_rural <- het_models("locale", "rural")
ms_non_rural <- het_models("locale", "city|suburb|town")

finalm_psm_rural <- ms_rural[["m6"]]

s_rural <- stargazer(ms_rural, 
                     title = "Rural vs Non Rural Districts",
                     type = sg_type,
                     omit.stat = sg_omit_stats,
                     star.cutoffs = sg_stars, 
                     header = FALSE, 
                     keep = "post_strike",
                     add.lines = het_sg_lines)

s_nonrural <- stargazer(ms_non_rural, 
                        type = sg_type,
                        omit.stat = sg_omit_stats,
                        star.cutoffs = sg_stars, 
                        header = FALSE, 
                        keep = "post_strike",
                        add.lines = het_sg_lines)


star_panel(s_rural, s_nonrural, same.summary.stats = FALSE,
           panel.names = c("Rural Districts", "Non-Rural Districts")) %>% 
  star_notes_tex(note.type = "caption", note = "All models include state and year clustered standard errors.") %>% cat


# Table 6 Experience -----------------------------------------------------------

ms_lowxp  <- het_models("third_experience", "low")
ms_highxp <- het_models("third_experience", "high")

finalm_psm_lowxp <- ms_lowxp[["m6"]]


s_lowxp <- stargazer(ms_lowxp, 
                     title = "Less Experienced vs More Experienced Districs",
                     type = sg_type,
                     omit.stat = sg_omit_stats,
                     star.cutoffs = sg_stars, 
                     header = FALSE, 
                     keep = "post_strike",
                     add.lines = het_sg_lines)

s_highxp <- stargazer(ms_highxp, 
                      type = sg_type,
                      omit.stat = sg_omit_stats,
                      star.cutoffs = sg_stars, 
                      header = FALSE, 
                      keep = "post_strike",
                      add.lines = het_sg_lines)


star_panel(s_lowxp, s_highxp, same.summary.stats = FALSE,
           panel.names = c("Low Experience Districts", "High Experience Districts")) %>% 
  star_notes_tex(note.type = "caption", note = "Low experience districts are districts in the bottom third of Oklahoma districts' average teacher experience in 2018 (< 12.4 years). High experience districts are districts in the top third in 2018 (> 14.7 years). All models include state and year clustered standard errors.") %>% cat

