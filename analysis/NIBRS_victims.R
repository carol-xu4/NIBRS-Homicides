## Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr)

# Set working directory 
setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/NIBRS Homicides")

# victims data
victims = readRDS("data/output/victims_homicide.rds")

dim(victims)
glimpse(victims)

table(victims$V4018, useNA = "always")  # age
table(victims$V4019, useNA = "always")  # sex
table(victims$V4020, useNA = "always")  # race
table(victims$V4021, useNA = "always")  # ethnicity

# age distribution of victims
ggplot(victims, aes(x = V4018)) +
  geom_histogram(binwidth = 1, fill = "#C97703", alpha = 0.8, color = NA) +
  scale_x_continuous(breaks = seq(0, 100, by = 10), expand = c(0.02, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 600)) +
  labs(
    title = "Age Distribution of Homicide Victims",
    subtitle = "victims_homicide; NIBRS 2023, individual victims of murder/nonnegligent and negligent manslaughter",
    x = NULL, y = NULL,
    caption = "Source: NIBRS 2023 via ICPSR") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 20, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor.y = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 25, color = "gray40"),
    axis.text.y = element_text(size = 25, color = "gray40"),
    plot.caption = element_text(size = 12, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/victim_age_histogram.png", width = 15, height = 10)

# race
race_tbl = victims %>%
  count(V4020) %>%
  mutate(pct = n / sum(n) * 100)

print(race_tbl)
write_csv(race_tbl, "results/victim_race.csv")

ggplot(race_tbl, aes(x = reorder(V4020, -n), y = n)) +
  geom_col(fill = "#C97703", width = 0.6) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 9500), breaks = seq(0, 10000, by = 1000)) +
  labs(
    title = "Race of Homicide Victims",
    subtitle = "victims_homicide; NIBRS 2023",
    x = NULL, y = NULL,
    caption = "Source: NIBRS 2023 via ICPSR") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 20, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor.y = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 16, color = "gray40", angle = 20, hjust = 1),
    axis.text.y = element_text(size = 25, color = "gray40"),
    plot.caption = element_text(size = 12, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/victim_race.png", width = 15, height = 10)

# ethnicity
ethnicity_tbl = victims %>%
  count(V4021) %>%
  mutate(pct = n / sum(n) * 100)

print(ethnicity_tbl)
write_csv(ethnicity_tbl, "results/victim_ethnicity.csv")

ggplot(ethnicity_tbl, aes(x = reorder(V4021, -n), y = n)) +
  geom_col(fill = "#C97703", width = 0.5) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 12000), breaks = seq(0, 12000, by = 1000)) +
  labs(
    title = "Ethnicity of Homicide Victims",
    subtitle = "victims_homicide; NIBRS 2023",
    x = NULL, y = NULL,
    caption = "Source: NIBRS 2023 via ICPSR") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 20, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor.y = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 20, color = "gray40"),
    axis.text.y = element_text(size = 25, color = "gray40"),
    plot.caption = element_text(size = 12, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/victim_ethnicity.png", width = 15, height = 10)

# race, 5 year age bins, and sex
victims = victims %>%
  mutate(age_group_5yr = case_when(
    is.na(V4018) ~ "Unknown",
    V4018 >= 80 ~ "80+",
    TRUE ~ paste0(floor(V4018 / 5) * 5, "-", floor(V4018 / 5) * 5 + 4)))

age_levels_5yr = c(paste0(seq(0, 75, by = 5), "-", seq(4, 79, by = 5)), "80+", "Unknown")

victims = victims %>%
  mutate(ethnicity = case_when(
    str_detect(V4021, "^\\(0\\)") ~ "Not Hispanic/Latino",
    str_detect(V4021, "^\\(1\\)") ~ "Hispanic/Latino",
    TRUE ~ NA_character_
  ) %>% factor(levels = c("Not Hispanic/Latino", "Hispanic/Latino")))

victims = victims %>%
  mutate(age_group_5yr = factor(age_group_5yr, levels = age_levels_5yr))

victim_table = victims %>%
  group_by(V4020, ethnicity, age_group_5yr, V4019) %>%
  summarise(n = n(), .groups = "drop") %>%
  rename(race = V4020, sex = V4019)

print(victim_table, n = Inf)

write_csv(victim_table, "results/victim_race_age_sex.csv")

# new single race categories (ACS) ----------------------------------------------------------------------------
theme_nibrs_scatter = theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 20, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "none",
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 20, color = "gray40"),
    axis.text.y = element_text(size = 18, color = "gray40"),
    plot.caption = element_text(size = 12, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

# age x race
ggplot(victims, aes(x = V4018, y = V4020)) +
  geom_jitter(color = "#C97703", alpha = 0.15, height = 0.2, size = 1.5) +
  scale_x_continuous(breaks = seq(0, 100, by = 10), expand = c(0.02, 0)) +
  labs(
    title = "Homicide Victims by Age and Race",
    subtitle = "victims_homicide; NIBRS 2023, individual victims of murder/nonnegligent and negligent manslaughter",
    x = "Age", y = NULL,
    caption = "Source: NIBRS 2023 via ICPSR") +
  theme_nibrs_scatter

ggsave("results/victim_age_race_scatter.png", width = 15, height = 10)

# age x ethnicity
ggplot(victims, aes(x = V4018, y = V4021)) +
  geom_jitter(color = "#C97703", alpha = 0.15, height = 0.2, size = 1.5) +
  scale_x_continuous(breaks = seq(0, 100, by = 10), expand = c(0.02, 0)) +
  labs(
    title = "Homicide Victims by Age and Ethnicity",
    subtitle = "victims_homicide; NIBRS 2023, individual victims of murder/nonnegligent and negligent manslaughter",
    x = "Age", y = NULL,
    caption = "Source: NIBRS 2023 via ICPSR") +
  theme_nibrs_scatter

ggsave("results/victim_age_ethnicity_scatter.png", width = 15, height = 10)

# age x sex
ggplot(victims, aes(x = V4018, y = V4019)) +
  geom_jitter(color = "#C97703", alpha = 0.15, height = 0.2, size = 1.5) +
  scale_x_continuous(breaks = seq(0, 100, by = 10), expand = c(0.02, 0)) +
  labs(
    title = "Homicide Victims by Age and Sex",
    subtitle = "victims_homicide; NIBRS 2023, individual victims of murder/nonnegligent and negligent manslaughter",
    x = "Age", y = NULL,
    caption = "Source: NIBRS 2023 via ICPSR") +
  theme_nibrs_scatter

ggsave("results/victim_age_sex_scatter.png", width = 15, height = 10)

# check how many incidents involve >3 victims
victims %>% count(incident_key, name = "n_victims") %>% count(n_victims > 3, name = "n_incidents")

victims %>% count(incident_key) %>% summarise(max(n))
