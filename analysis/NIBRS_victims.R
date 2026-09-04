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

