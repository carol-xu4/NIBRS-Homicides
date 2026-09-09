## Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr)

# Set working directory 
setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/NIBRS Homicides")

# NIBRS offenders
offenders = readRDS("data/output/offenders_homicide.rds")

dim(offenders)
glimpse(offenders)

table(offenders$V5007, useNA = "always")  # age
table(offenders$V5008, useNA = "always")  # sex
table(offenders$V5009, useNA = "always")  # race
table(offenders$V5011, useNA = "always")  # ethnicity

# age distribution of offenders
# both 00 and NA unknown
ggplot(offenders %>% filter(V5007 > 0), aes(x = V5007)) +
  geom_histogram(binwidth = 1, fill = "#3043B4", alpha = 0.8, color = NA) +
  scale_x_continuous(breaks = seq(0, 100, by = 10), expand = c(0.02, 0)) +
  scale_y_continuous(expand = c(0, 0)) +  # set limits/breaks once you see the plot
  labs(
    title = "Age Distribution of Homicide Offenders",
    subtitle = "offenders_homicide; NIBRS 2023, excludes 2,270 offenders coded age = 0 (Unknown) and 3,117 with missing age",
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

ggsave("results/offender_age_histogram.png", width = 15, height = 10)

# race
race_tbl_off = offenders %>%
  count(V5009) %>%
  mutate(pct = n / sum(n) * 100)

print(race_tbl_off)
write_csv(race_tbl_off, "results/offender_race.csv")

ggplot(race_tbl_off, aes(x = reorder(V5009, -n), y = n)) +
  geom_col(fill = "#3043B4", width = 0.6) +
  scale_y_continuous(expand = c(0, 0)) +  # set limits/breaks once you see race_tbl_off
  labs(
    title = "Race of Homicide Offenders",
    subtitle = "offenders_homicide; NIBRS 2023",
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

ggsave("results/offender_race.png", width = 15, height = 10)

# ethnicity
ethnicity_tbl_off = offenders %>%
  count(V5011) %>%
  mutate(pct = n / sum(n) * 100)

print(ethnicity_tbl_off)
write_csv(ethnicity_tbl_off, "results/offender_ethnicity.csv")

ggplot(ethnicity_tbl_off, aes(x = reorder(V5011, -n), y = n)) +
  geom_col(fill = "#3043B4", width = 0.5) +
  scale_y_continuous(expand = c(0, 0)) +  # set limits/breaks once you see ethnicity_tbl_off
  labs(
    title = "Ethnicity of Homicide Offenders",
    subtitle = "offenders_homicide; NIBRS 2023",
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

ggsave("results/offender_ethnicity.png", width = 15, height = 10)

# race, 5 year age bins, and sex
offenders = offenders %>%
  mutate(age_group_5yr = case_when(
    V5007 <= 0 ~ "Unknown",  
    is.na(V5007) ~ "Unknown",
    V5007 >= 80 ~ "80+",
    TRUE ~ paste0(floor(V5007 / 5) * 5, "-", floor(V5007 / 5) * 5 + 4)))

age_levels_5yr = c(paste0(seq(0, 75, by = 5), "-", seq(4, 79, by = 5)), "80+", "Unknown")

offenders = offenders %>%
  mutate(ethnicity = case_when(
    str_detect(V5011, "^\\(0\\)") ~ "Not Hispanic/Latino",
    str_detect(V5011, "^\\(1\\)") ~ "Hispanic/Latino",
    TRUE ~ NA_character_) %>% 
    factor(levels = c("Not Hispanic/Latino", "Hispanic/Latino")))

offenders = offenders %>%
  mutate(age_group_5yr = factor(age_group_5yr, levels = age_levels_5yr))

offender_table = offenders %>%
  group_by(V5009, ethnicity,age_group_5yr, V5008) %>%
  summarise(n = n(), .groups = "drop") %>%
  rename(race = V5009, sex = V5008)

print(offender_table, n = Inf)

write_csv(offender_table, "results/offender_race_age_sex.csv")

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
ggplot(offenders %>% filter(V5007 > 0), aes(x = V5007, y = V5009)) +
  geom_jitter(color = "#3043B4", alpha = 0.15, height = 0.2, size = 1.5) +
  scale_x_continuous(breaks = seq(0, 100, by = 10), expand = c(0.02, 0)) +
  labs(
    title = "Homicide Offenders by Age and Race",
    subtitle = "offenders_homicide; NIBRS 2023, excludes offenders coded age = 0 (Unknown) or missing age",
    x = "Age", y = NULL,
    caption = "Source: NIBRS 2023 via ICPSR") +
  theme_nibrs_scatter

ggsave("results/offender_age_race_scatter.png", width = 15, height = 10)

# age x ethnicity
ggplot(offenders %>% filter(V5007 > 0), aes(x = V5007, y = V5011)) +
  geom_jitter(color = "#3043B4", alpha = 0.15, height = 0.2, size = 1.5) +
  scale_x_continuous(breaks = seq(0, 100, by = 10), expand = c(0.02, 0)) +
  labs(
    title = "Homicide Offenders by Age and Ethnicity",
    subtitle = "offenders_homicide; NIBRS 2023, excludes offenders coded age = 0 (Unknown) or missing age",
    x = "Age", y = NULL,
    caption = "Source: NIBRS 2023 via ICPSR") +
  theme_nibrs_scatter

ggsave("results/offender_age_ethnicity_scatter.png", width = 15, height = 10)

# age x sex
ggplot(offenders %>% filter(V5007 > 0), aes(x = V5007, y = V5008)) +
  geom_jitter(color = "#3043B4", alpha = 0.15, height = 0.2, size = 1.5) +
  scale_x_continuous(breaks = seq(0, 100, by = 10), expand = c(0.02, 0)) +
  labs(
    title = "Homicide Offenders by Age and Sex",
    subtitle = "offenders_homicide; NIBRS 2023, excludes offenders coded age = 0 (Unknown) or missing age",
    x = "Age", y = NULL,
    caption = "Source: NIBRS 2023 via ICPSR") +
  theme_nibrs_scatter

ggsave("results/offender_age_sex_scatter.png", width = 15, height = 10)


