## Combine into one table: n_offenders and n_victims -------------------------
combined_table = full_join(
  victim_table %>% rename(n_victims = n),
  offender_table %>% rename(n_offenders = n),
  by = c("race", "age_group_5yr", "sex")
) %>%
  mutate(
    n_victims = replace_na(n_victims, 0),
    n_offenders = replace_na(n_offenders, 0)
  ) %>%
  arrange(race, age_group_5yr, sex)

print(combined_table, n = Inf)

write_csv(combined_table, "results/victims_offenders_race_age_sex.csv")
