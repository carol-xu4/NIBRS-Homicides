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
ddi_acs = read_ipums_ddi("data/input/usa_00028.xml")
acs = read_ipums_micro(ddi_acs)

acs = acs %>% rename_with(tolower) %>%
  select(year, perwt, sex, age, race, hispan)


# ACS population estimates, by original race, age, sex, ethnicity variables
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

write_csv(acs_table, "results/acs_race_age_sex.csv")

# recode ACS demographics to match NIBRS coding
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
      race %in% 7:9 ~ "Other/Two or More Races"))  # no NIBRS equivalent -- exclude

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
    acs_table_nibrs %>% filter(race != "Other/Two or More Races"),
    by = c("race", "ethnicity", "age_group_5yr", "sex")) %>%
  mutate(
    victim_rate_per_100k   = n_victims   / weighted * 100000,
    offender_rate_per_100k = n_offenders / weighted * 100000) %>%
  arrange(race, ethnicity, age_group_5yr, sex)

print(rates_table, n = Inf, width = Inf)

write_csv(rates_table, "results/nibrs_acs_rates.csv")
