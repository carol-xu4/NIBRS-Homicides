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