
---
  ## Please provide your name and the date below. This quiz is due September 17 at 11:59 PM.
  
  title: "Assignment 2"
output: html_document
date: 10/25/25
name: Diamond Beckom
---
# ---- Libraries ----
library(tidyverse)
library(readxl)
library(janitor)
library(readr)
library(broom)

# ---- Load & clean ----
path <- "C:/Users/diamo/OneDrive/Documents/income_gender.xlsm"

# Read without headers, promote the first row to headers, then clean
income <- read_excel(path, sheet = 1, col_names = FALSE) %>%
  row_to_names(row_number = 1) %>%
  clean_names()

# At this point your first column should be occupations (you saw all_occupations earlier)
# Make sure that column exists; if not, it’s probably the first column.
if (!"all_occupations" %in% names(income)) {
  names(income)[1] <- "all_occupations"
}

# Parse all non-occupation columns to numeric (handles $, commas)
income_num <- income %>%
  mutate(across(-all_occupations, ~ parse_number(as.character(.x))))

# Pick the FIRST numeric column that looks like an income measure
num_cols <- names(income_num)[sapply(income_num, is.numeric)]
stopifnot(length(num_cols) > 0)  # fail early if truly no numeric columns

weekly_col <- num_cols[1]  # you can change to a different numeric column if you prefer

# Build tidy frame with the chosen income column
df <- income_num %>%
  select(all_occupations, all_of(weekly_col)) %>%
  rename(occupation = all_occupations, weekly_income = all_of(weekly_col)) %>%
  filter(!is.na(weekly_income)) %>%
  mutate(
    occupation = as.factor(occupation),
    weekly_income = as.numeric(weekly_income)
  )

# (Optional) focus analysis on the top N most common occupations to keep ANOVA readable
top_n_occ <- df %>%
  count(occupation, sort = TRUE) %>%
  slice_head(n = 8) %>%
  pull(occupation)

df_top <- df %>% filter(occupation %in% top_n_occ)

# ---- Descriptives + 95% CI by occupation ----
group_summary <- df_top %>%
  group_by(occupation) %>%
  summarise(
    mean_income = mean(weekly_income, na.rm = TRUE),
    sd_income   = sd(weekly_income, na.rm = TRUE),
    n           = n(),
    se          = sd_income / sqrt(n),
    ci_low      = mean_income - qt(0.975, df = n - 1) * se,
    ci_high     = mean_income + qt(0.975, df = n - 1) * se,
    .groups = "drop"
  )
group_summary

# ---- One-way ANOVA ----
fit <- aov(weekly_income ~ occupation, data = df_top)
summary(fit)

# Tidy ANOVA table for your write-up
tidy(fit)

# ---- Post-hoc (Tukey) to see which occupations differ ----
posthoc <- TukeyHSD(fit)
posthoc

# ---- Plot ----
ggplot(df_top, aes(x = occupation, y = weekly_income)) +
  geom_boxplot() +
  coord_flip() +
  labs(
    title = "Weekly Income by Occupation",
    x = "Occupation",
    y = "Weekly Income"
  ) +
  theme_minimal()


