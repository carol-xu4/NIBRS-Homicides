## Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr)

# Set working directory
setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/NIBRS Homicides")

## Shared age bins -------------------------------------------------------------

age_levels_5yr = c(paste0(seq(0, 75, by = 5), "-", seq(4, 79, by = 5)), "80+", "Unknown")

## victims data ----------------------------------------------------------------

victims = readRDS("data/output/victims_homicide.rds")

victims = victims %>%
  mutate(
    age_group_5yr = case_when(
      is.na(V4018) ~ "Unknown",
      V4018 >= 80 ~ "80+",
      TRUE ~ paste0(floor(V4018 / 5) * 5, "-", floor(V4018 / 5) * 5 + 4)
    ) %>% factor(levels = age_levels_5yr),
    ethnicity = case_when(
      str_detect(V4021, "^\\(0\\)") ~ "Not Hispanic/Latino",
      str_detect(V4021, "^\\(1\\)") ~ "Hispanic/Latino",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Not Hispanic/Latino", "Hispanic/Latino"))
  )

victim_table = victims %>%
  group_by(V4020, ethnicity, age_group_5yr, V4019) %>%
  summarise(n = n(), .groups = "drop") %>%
  rename(race = V4020, sex = V4019)

print(victim_table, n = Inf)

## offenders data ----------------------------------------------------------------

offenders = readRDS("data/output/offenders_homicide.rds")

# NOTE: V5007 = 0 means "Unknown" per NIBRS coding (01-98 = years old,
# 99 = over 98, 00 = unknown) -- not a real age of zero. 2,270 offenders
# had age = 0 ("Unknown") and 3,117 had NA (missing) as of this data pull.

offenders = offenders %>%
  mutate(
    age_group_5yr = case_when(
      V5007 <= 0 ~ "Unknown",
      is.na(V5007) ~ "Unknown",
      V5007 >= 80 ~ "80+",
      TRUE ~ paste0(floor(V5007 / 5) * 5, "-", floor(V5007 / 5) * 5 + 4)
    ) %>% factor(levels = age_levels_5yr),
    ethnicity = case_when(
      str_detect(V5011, "^\\(0\\)") ~ "Not Hispanic/Latino",
      str_detect(V5011, "^\\(1\\)") ~ "Hispanic/Latino",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Not Hispanic/Latino", "Hispanic/Latino"))
  )

offender_table = offenders %>%
  group_by(V5009, ethnicity, age_group_5yr, V5008) %>%
  summarise(n = n(), .groups = "drop") %>%
  rename(race = V5009, sex = V5008)

print(offender_table, n = Inf)

## Combine victims + offenders, clean labels, collapse Asian + NHPI ----------

combined_table = full_join(
  victim_table %>% rename(n_victims = n),
  offender_table %>% rename(n_offenders = n),
  by = c("race", "ethnicity", "age_group_5yr", "sex")
) %>%
  mutate(
    n_victims = replace_na(n_victims, 0),
    n_offenders = replace_na(n_offenders, 0),
    race = str_remove(race, "^\\(\\d+\\)\\s*"),
    sex  = str_remove(sex, "^\\(\\d+\\)\\s*"),
    race = case_when(
      race %in% c("Asian", "Native Hawaiian or Other Pacific Islander") ~ "Asian or Pacific Islander",
      TRUE ~ race
    )
  ) %>%
  group_by(race, ethnicity, age_group_5yr, sex) %>%
  summarise(
    n_victims = sum(n_victims, na.rm = TRUE),
    n_offenders = sum(n_offenders, na.rm = TRUE),
    .groups = "drop"
  )

print(combined_table, n = Inf)

## ACS 2023 (IPUMS) data ------------------------------------------------------

acs = read_ipums_micro(ddi = "data/input/usa_00028.xml")

# raw-code table (kept for reference / other uses)
acs = acs %>%
  mutate(
    age_group_5yr = case_when(
      is.na(age) ~ "Unknown",
      age >= 80 ~ "80+",
      TRUE ~ paste0(floor(age / 5) * 5, "-", floor(age / 5) * 5 + 4)
    ) %>% factor(levels = age_levels_5yr)
  )

acs_table = acs %>%
  group_by(race, hispan, age_group_5yr, sex) %>%
  summarise(n = n(), weighted = sum(perwt, na.rm = TRUE), .groups = "drop") %>%
  arrange(race, hispan, age_group_5yr, sex)

write_csv(acs_table, "results/acs_race_age_sex.csv")

# recode to match NIBRS categories
acs = acs %>%
  mutate(
    sex_nibrs = case_when(
      sex == 1 ~ "Male",
      sex == 2 ~ "Female",
      TRUE ~ NA_character_
    ),
    ethnicity_nibrs = case_when(
      hispan == 0 ~ "Not Hispanic/Latino",
      hispan %in% 1:4 ~ "Hispanic/Latino",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Not Hispanic/Latino", "Hispanic/Latino")),
    race_nibrs = case_when(
      race == 1 ~ "White",
      race == 2 ~ "Black or African American",
      race == 3 ~ "American Indian or Alaska Native",
      race %in% 4:6 ~ "Asian or Pacific Islander",
      race %in% 7:9 ~ "Other/Two or More Races"  # no NIBRS equivalent -- excluded before rate calc
    )
  )

acs_table_nibrs = acs %>%
  group_by(race_nibrs, ethnicity_nibrs, age_group_5yr, sex_nibrs) %>%
  summarise(n = n(), weighted = sum(perwt, na.rm = TRUE), .groups = "drop") %>%
  rename(race = race_nibrs, ethnicity = ethnicity_nibrs, sex = sex_nibrs) %>%
  arrange(race, ethnicity, age_group_5yr, sex)

write_csv(acs_table_nibrs, "results/acs_race_age_sex_nibrs.csv")

## Join NIBRS (victims + offenders) with ACS population denominator ---------

rates_table = combined_table %>%
  left_join(
    acs_table_nibrs %>% filter(race != "Other/Two or More Races"),
    by = c("race", "ethnicity", "age_group_5yr", "sex")
  ) %>%
  mutate(
    victim_rate_per_100k   = n_victims   / weighted * 100000,
    offender_rate_per_100k = n_offenders / weighted * 100000
  ) %>%
  arrange(race, ethnicity, age_group_5yr, sex)

print(rates_table, n = Inf)

write_csv(rates_table, "results/victim_offender_rates.csv")

## Plot: victim & offender rate by age, faceted by race ----------------------

colors_role = c("Offenders" = "#3043B4", "Victims" = "#C97703")

rates_by_race_age = rates_table %>%
  filter(!is.na(race), age_group_5yr != "Unknown") %>%
  group_by(race, age_group_5yr) %>%
  summarise(
    n_victims = sum(n_victims, na.rm = TRUE),
    n_offenders = sum(n_offenders, na.rm = TRUE),
    weighted  = sum(weighted, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    victim_rate_per_100k   = n_victims / weighted * 100000,
    offender_rate_per_100k = n_offenders / weighted * 100000
  ) %>%
  pivot_longer(
    cols = c(victim_rate_per_100k, offender_rate_per_100k),
    names_to = "role", values_to = "rate_per_100k"
  ) %>%
  mutate(role = case_when(
    role == "victim_rate_per_100k" ~ "Victims",
    role == "offender_rate_per_100k" ~ "Offenders"
  ))

ggplot(rates_by_race_age, aes(x = age_group_5yr, y = rate_per_100k, color = role, group = role)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 2) +
  scale_color_manual(values = colors_role) +
  facet_wrap(~ race) +
  labs(
    title = "Homicide Victim & Offender Rate by Age and Race",
    subtitle = "rates_table; per 100,000 population, NIBRS 2023 / ACS 2023",
    x = NULL, y = NULL, color = NULL,
    caption = "Source: NIBRS 2023 via ICPSR; ACS 2023 via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 20, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 18),
    strip.text = element_text(size = 18, color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor.y = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 12, color = "gray40", angle = 40, hjust = 1),
    axis.text.y = element_text(size = 18, color = "gray40"),
    plot.caption = element_text(size = 12, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/victim_offender_rate_by_age_race.png", width = 15, height = 10)

## Plot: victim & offender rate by age, faceted by ethnicity -----------------

rates_by_ethnicity_age = rates_table %>%
  filter(!is.na(ethnicity), age_group_5yr != "Unknown") %>%
  group_by(ethnicity, age_group_5yr) %>%
  summarise(
    n_victims = sum(n_victims, na.rm = TRUE),
    n_offenders = sum(n_offenders, na.rm = TRUE),
    weighted  = sum(weighted, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    victim_rate_per_100k   = n_victims / weighted * 100000,
    offender_rate_per_100k = n_offenders / weighted * 100000
  ) %>%
  pivot_longer(
    cols = c(victim_rate_per_100k, offender_rate_per_100k),
    names_to = "role", values_to = "rate_per_100k"
  ) %>%
  mutate(role = case_when(
    role == "victim_rate_per_100k" ~ "Victims",
    role == "offender_rate_per_100k" ~ "Offenders"
  ))

ggplot(rates_by_ethnicity_age, aes(x = age_group_5yr, y = rate_per_100k, color = role, group = role)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 2) +
  scale_color_manual(values = colors_role) +
  facet_wrap(~ ethnicity) +
  labs(
    title = "Homicide Victim & Offender Rate by Age and Ethnicity",
    subtitle = "rates_table; per 100,000 population, NIBRS 2023 / ACS 2023",
    x = NULL, y = NULL, color = NULL,
    caption = "Source: NIBRS 2023 via ICPSR; ACS 2023 via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 20, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 18),
    strip.text = element_text(size = 18, color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor.y = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 12, color = "gray40", angle = 40, hjust = 1),
    axis.text.y = element_text(size = 18, color = "gray40"),
    plot.caption = element_text(size = 12, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/victim_offender_rate_by_age_ethnicity.png", width = 15, height = 10)
