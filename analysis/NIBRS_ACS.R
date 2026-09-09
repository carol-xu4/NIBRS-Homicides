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
combined_table = read_csv("results/victims_offenders_race_age_sex.csv")

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
print(rates_table, n = 100, width = Inf)

write_csv(rates_table, "results/nibrs_acs_rates.csv")

# plotting rates
rates_by_race_age = rates_table %>%
  filter(!is.na(race), age_group_5yr != "Unknown") %>%
  droplevels() %>%  
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
  coord_cartesian(ylim = c(0, 60)) +
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

ggsave("results/all_rate_by_age_race.png", width = 15, height = 10)

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

# offending rate vs. victimization
rates_scatter = rates_table %>%
  filter(!is.na(victim_rate_per_100k), !is.na(offender_rate_per_100k))

ggplot(rates_scatter, aes(x = victim_rate_per_100k, y = offender_rate_per_100k)) +
  geom_abline(intercept = 0, slope = 1, color = "gray70", linetype = "dashed", linewidth = 0.8) +
  geom_point(color = "#3043B4", alpha = 0.5, size = 2.5) +
  labs(
    title = "Offender Rate vs. Victimization Rate",
    subtitle = "rates_table; per 100,000 population, by race x ethnicity x age x sex; NIBRS 2023 / ACS 2023",
    x = "Victim Rate per 100,000", y = "Offender Rate per 100,000",
    caption = "Source: NIBRS 2023 via ICPSR; ACS 2023 via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 18, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "none",
    panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_text(size = 20, color = "gray40"),
    axis.text.x = element_text(size = 18, color = "gray40"),
    axis.text.y = element_text(size = 18, color = "gray40"),
    plot.caption = element_text(size = 12, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/offender_vs_victim_rate_scatter.png", width = 12, height = 12)

ggplot(rates_scatter, aes(x = victim_rate_per_100k, y = offender_rate_per_100k)) +
  geom_abline(intercept = 0, slope = 1, color = "gray70", linetype = "dashed", linewidth = 0.8) +
  geom_point(color = "#3043B4", alpha = 0.5, size = 2.5) +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
  labs(
    title = "Offender Rate vs. Victimization Rate (Zoomed)",
    subtitle = "rates_table; per 100,000 population, by race x ethnicity x age x sex; NIBRS 2023 / ACS 2023",
    x = "Victim Rate per 100,000", y = "Offender Rate per 100,000",
    caption = "Source: NIBRS 2023 via ICPSR; ACS 2023 via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 18, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "none",
    panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_text(size = 20, color = "gray40"),
    axis.text.x = element_text(size = 18, color = "gray40"),
    axis.text.y = element_text(size = 18, color = "gray40"),
    plot.caption = element_text(size = 12, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/offender_vs_victim_rate_scatter_zoom.png", width = 12, height = 12)

rates_scatter = rates_table %>%
  filter(!is.na(victim_rate_per_100k), !is.na(offender_rate_per_100k),
         !is.na(race), !is.na(ethnicity), !is.na(sex),
         age_group_5yr != "Unknown") %>%
  mutate(age_mid = case_when(
    age_group_5yr == "80+" ~ 85,
    TRUE ~ as.numeric(str_extract(as.character(age_group_5yr), "^\\d+")) + 2
  ))

race_colors = c(
  "White" = "#3043B4",
  "Black or African American" = "#C97703",
  "American Indian or Alaska Native" = "#2CA58D",
  "Asian" = "#A64AC9",
  "Native Hawaiian or Other Pacific Islander" = "#D64550"
)

ggplot(rates_scatter, aes(x = victim_rate_per_100k, y = offender_rate_per_100k,
                          color = race, shape = sex, size = age_mid)) +
  geom_abline(intercept = 0, slope = 1, color = "gray70", linetype = "dashed", linewidth = 0.8) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values = race_colors) +
  scale_shape_manual(values = c(Male = 16, Female = 17)) +
  scale_size_continuous(range = c(1.5, 7), name = "Age (bin midpoint)") +
  facet_wrap(~ ethnicity) +
  labs(
    title = "Offender Rate vs. Victimization Rate, by Race, Sex, Age, and Ethnicity",
    subtitle = "rates_table; per 100,000 population; NIBRS 2023 / ACS 2023",
    x = "Victim Rate per 100,000", y = "Offender Rate per 100,000",
    color = "Race", shape = "Sex",
    caption = "Source: NIBRS 2023 via ICPSR; ACS 2023 via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 26, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 16, color = "gray40", hjust = 0, margin = margin(b = 12)),
    strip.text = element_text(size = 16, color = "black"),
    legend.position = "right",
    legend.text = element_text(size = 12),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_text(size = 16, color = "gray40"),
    axis.text = element_text(size = 14, color = "gray40"),
    plot.caption = element_text(size = 12, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/offender_vs_victim_rate_all_dims.png", width = 16, height = 10)

ggplot(rates_scatter, aes(x = victim_rate_per_100k, y = offender_rate_per_100k,
                          color = race, shape = sex, size = age_mid)) +
  geom_abline(intercept = 0, slope = 1, color = "gray70", linetype = "dashed", linewidth = 0.8) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values = race_colors) +
  scale_shape_manual(values = c(Male = 16, Female = 17)) +
  scale_size_continuous(range = c(1.5, 7), name = "Age (bin midpoint)") +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
  facet_wrap(~ ethnicity) +
  labs(
    title = "Offender Rate vs. Victimization Rate, by Race, Sex, Age, and Ethnicity (Zoomed)",
    subtitle = "rates_table; per 100,000 population; NIBRS 2023 / ACS 2023",
    x = "Victim Rate per 100,000", y = "Offender Rate per 100,000",
    color = "Race", shape = "Sex",
    caption = "Source: NIBRS 2023 via ICPSR; ACS 2023 via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 26, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 16, color = "gray40", hjust = 0, margin = margin(b = 12)),
    strip.text = element_text(size = 16, color = "black"),
    legend.position = "right",
    legend.text = element_text(size = 12),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_text(size = 16, color = "gray40"),
    axis.text = element_text(size = 14, color = "gray40"),
    plot.caption = element_text(size = 12, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/offender_vs_victim_rate_all_dims_zoom.png", width = 16, height = 10)

model = lm(offender_rate_per_100k ~ victim_rate_per_100k, data = rates_scatter)
summary(model)
summary(model)$r.squared


model_weighted = lm(offender_rate_per_100k ~ victim_rate_per_100k,
                     data = rates_scatter, weights = n_victims + n_offenders)
summary(model_weighted)$r.squared

rates_scatter_no_kids = rates_scatter %>%
  filter(!age_group_5yr %in% c("0-4", "5-9"))

nrow(rates_scatter)
nrow(rates_scatter_no_kids)   

model_no_kids = lm(offender_rate_per_100k ~ victim_rate_per_100k, data = rates_scatter_no_kids)
summary(model_no_kids)$r.squared

model_weighted_no_kids = lm(offender_rate_per_100k ~ victim_rate_per_100k,
                             data = rates_scatter_no_kids, weights = n_victims + n_offenders)
summary(model_weighted_no_kids)$r.squared
