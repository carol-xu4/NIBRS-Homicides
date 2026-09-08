victims_long = victims %>%
  transmute(age = V4018, Race = V4020, Ethnicity = V4021, Sex = V4019) %>%
  pivot_longer(c(Race, Ethnicity, Sex), names_to = "variable", values_to = "category") %>%
  filter(!is.na(age), !is.na(category))

ggplot(victims_long, aes(x = age, y = category)) +
  geom_jitter(color = "#C97703", alpha = 0.1, height = 0.2, size = 1) +
  facet_wrap(~variable, scales = "free_y", ncol = 1) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  theme_nibrs_scatter

combined = bind_rows(
  victims %>% filter(!is.na(V4018)) %>%
    transmute(age = V4018, role = "Victim", Race = V4020, Ethnicity = V4021, Sex = V4019),
  offenders %>% filter(V5007 > 0) %>%
    transmute(age = V5007, role = "Offender", Race = V5009, Ethnicity = V5011, Sex = V5008)
) %>%
  pivot_longer(c(Race, Ethnicity, Sex), names_to = "variable", values_to = "category") %>%
  filter(!is.na(category))

ggplot(combined, aes(x = age, y = category, color = role)) +
  geom_jitter(alpha = 0.12, height = 0.2, size = 1) +
  scale_color_manual(values = c(Victim = "#C97703", Offender = "#3043B4")) +
  facet_wrap(~variable, scales = "free_y", ncol = 1) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  theme_nibrs_scatter +
  theme(strip.text = element_text(size = 18, face = "bold"))

pacman::p_load(ggridges)

ggplot(combined, aes(x = age, y = category, fill = role)) +
  geom_density_ridges(alpha = 0.6, color = "white", scale = 1.2) +
  scale_fill_manual(values = c(Victim = "#C97703", Offender = "#3043B4")) +
  facet_wrap(~variable, scales = "free_y", ncol = 1) +
  theme_nibrs_scatter

ggsave("results/nibrs_alldemographics_age.png", width = 15, height = 10)
