## Combine into one table: n_offenders and n_victims -------------------------
combined_table = full_join(
  victim_table %>% rename(n_victims = n),
  offender_table %>% rename(n_offenders = n),
  by = c("race", "ethnicity", "age_group_5yr", "sex")
) %>%
  mutate(
    n_victims = replace_na(n_victims, 0),
    n_offenders = replace_na(n_offenders, 0)
  ) %>%
  arrange(race, ethnicity, age_group_5yr, sex)

print(combined_table, n = Inf)

write_csv(combined_table, "results/victims_offenders_race_age_sex.csv")

# read in ACS 2023 
ddi_acs = read_ipums_ddi("data/input/usa_00029.xml")
acs = read_ipums_micro(ddi_acs)

acs = acs %>% rename_with(tolower) %>%
  select(year, perwt, sex, age, race, hispan,
       racamind, racasian, racblk, racpacis, racwht)


# ACS population estimates, by original race, age, sex, ethnicity variables
age_levels_5yr = c(paste0(seq(0, 75, by = 5), "-", seq(4, 79, by = 5)), "80+", "Unknown")

acs = acs %>%
  mutate(
    age_group_5yr = case_when(
      is.na(age) ~ "Unknown",
      age >= 80 ~ "80+",
      TRUE ~ paste0(floor(age / 5) * 5, "-", floor(age / 5) * 5 + 4)
    ) %>% factor(levels = age_levels_5yr))

acs_table = acs %>%
  group_by(race, hispan, age_group_5yr, sex) %>%
  summarise(
    n = n(),
    weighted = sum(perwt, na.rm = TRUE),
    .groups = "drop") %>%
  arrange(race, hispan, age_group_5yr, sex)

print(acs_table, n = Inf)
print(acs_table, n = 100)

write_csv(acs_table, "results/acs_race_age_sex.csv")

# recode ACS demographics to match NIBRS coding
    # Hierarchical single-race assignment (NCHS bridged-race convention) to
    # resolve multiracial ACS respondents into NIBRS's single-race field.
    # Priority order: Black > AIAN > Asian > NHPI > White.
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
  racblk == 2 ~ "Black or African American",
  racamind == 2 ~ "American Indian or Alaska Native",
  racasian == 2 ~ "Asian",
  racpacis == 2 ~ "Native Hawaiian or Other Pacific Islander",
  racwht == 2 ~ "White",
  TRUE ~ NA_character_
) %>% factor(levels = c(
  "White",
  "Black or African American",
  "American Indian or Alaska Native",
  "Asian",
  "Native Hawaiian or Other Pacific Islander"
)))

## Group: n and weighted population, NIBRS-comparable categories -------------
acs_table_nibrs = acs %>%
  group_by(race_nibrs, ethnicity_nibrs, age_group_5yr, sex_nibrs) %>%
  summarise(
    n = n(),
    weighted = sum(perwt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(race = race_nibrs, ethnicity = ethnicity_nibrs, sex = sex_nibrs) %>%
  arrange(race, ethnicity, age_group_5yr, sex)

print(acs_table_nibrs, n = Inf)

write_csv(acs_table_nibrs, "results/acs_race_age_sex_nibrs.csv")

# update combined table with rates (NIBRS/ACS)
combined_table = combined_table %>%
  mutate(
    race = str_remove(race, "^\\(\\d+\\)\\s*"),
    sex  = str_remove(sex, "^\\(\\d+\\)\\s*"))

rates_table = combined_table %>%
  left_join(
    acs_table_nibrs,
    by = c("race", "ethnicity", "age_group_5yr", "sex")) %>%
  mutate(
    victim_rate_per_100k   = n_victims   / weighted * 100000,
    offender_rate_per_100k = n_offenders / weighted * 100000) %>%
  arrange(race, ethnicity, age_group_5yr, sex)

print(rates_table, n = Inf, width = Inf)

write_csv(rates_table, "results/nibrs_acs_rates.csv")

# plotting rates
rates_by_race_age = rates_table %>%
  filter(!is.na(race)) %>%
  group_by(race, age_group_5yr) %>%
  summarise(
    n_victims = sum(n_victims, na.rm = TRUE),
    n_offenders = sum(n_offenders, na.rm = TRUE),
    weighted  = sum(weighted, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(
    victim_rate_per_100k   = n_victims / weighted * 100000,
    offender_rate_per_100k = n_offenders / weighted * 100000) %>%
  pivot_longer(
    cols = c(victim_rate_per_100k, offender_rate_per_100k),
    names_to = "role", values_to = "rate_per_100k"
  ) %>%
  mutate(role = case_when(
    role == "victim_rate_per_100k" ~ "Victims",
    role == "offender_rate_per_100k" ~ "Offenders"))

colors_role = c("Offenders" = "#3043B4", "Victims" = "#C97703")

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

## Victim & offender rate by age, faceted by ethnicity -----------------------

rates_by_ethnicity_age = rates_table %>%
  filter(!is.na(ethnicity)) %>%
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

# new single race categories




rates_table %>%
  filter(is.finite(victim_rate_per_100k) | is.finite(offender_rate_per_100k)) %>%
  filter(victim_rate_per_100k > 1000 | offender_rate_per_100k > 1000) %>%
  select(race, ethnicity, age_group_5yr, sex, n_victims, n_offenders, weighted, victim_rate_per_100k, offender_rate_per_100k) %>%
  arrange(desc(victim_rate_per_100k))
