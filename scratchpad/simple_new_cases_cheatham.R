new_cases_county <-
  age_by_county_df %>% 
  filter(COUNTY == "Cheatham") %>% 
  filter(AGE_GROUP != "Pending") %>%
  select(-COUNTY) %>% 
  pivot_wider(id_cols = "DATE", 
              names_from = "AGE_GROUP", 
              values_from = "CASE_COUNT") %>%
  mutate(across(!starts_with("DATE"),
                .fns = list(new = ~ c(0, diff(.))),
                .names = "{fn}_{col}"
  )) %>%
  select(DATE, starts_with("new_")) %>%
  rename_at(vars(starts_with("new_")),
            ~ str_replace(., "new_", "")) %>%
  pivot_longer(-DATE, names_to = "AGE_GROUP", values_to = "CASE_COUNT") %>%
  mutate(
    avg_age = case_when(
      AGE_GROUP == "0-10 years"  ~ 5,
      AGE_GROUP == "11-20 years" ~ 15,
      AGE_GROUP == "21-30 years" ~ 25,
      AGE_GROUP == "31-40 years" ~ 35,
      AGE_GROUP == "41-50 years" ~ 45,
      AGE_GROUP == "51-60 years" ~ 55,
      AGE_GROUP == "61-70 years" ~ 65,
      AGE_GROUP == "71-80 years" ~ 75,
      AGE_GROUP == "81+ years"   ~ 85,
  ))

new_cases_county %>% pivot_wider(id_cols = "DATE", names_from = "AGE_GROUP", values_from = "CASE_COUNT") %>% tail(n = 10)



county_new_avg_age <-
  new_cases_county %>%
  left_join(new_cases_county %>% 
              select(DATE, CASE_COUNT) %>% 
              group_by(DATE) %>% 
              summarize(TOTAL_CASE_COUNT = sum(CASE_COUNT)), by = c("DATE" = "DATE")) %>%
  group_by(DATE) %>% 
  summarize(mean_age = sum(avg_age * CASE_COUNT) / sum(CASE_COUNT)) %>% 
  unique()


county_new_avg_age %>% 
  filter(!is.na(mean_age)) %>% 
  filter(!is.infinite(mean_age)) %>% 
  ggplot(aes(x = as.Date(DATE))) + 
  geom_point(aes(y = mean_age), shape = 20) + 
  geom_line(aes(y = SMA(mean_age, n = 14)), color = "darkseagreen4")
