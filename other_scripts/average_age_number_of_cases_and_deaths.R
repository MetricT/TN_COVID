################################################################################
### Take age data for cases/deaths, and compute an average age so we can see
### how it changes over time.   We also want to compare it to number of 
### cases/deaths over time.
###
### You'll need to run TN_COVID.R first to allow it to load the necessary data, 
### then run this script.
################################################################################

### Get a tibble of the number of new cases/deaths
new_cases_num <-
  age_df %>%
  select(DATE, NEW_CASES, NEW_DEATHS) %>%
  rename(date = DATE) %>%
  group_by(date) %>%
  summarize(cases  = sum(NEW_CASES, na.rm = TRUE),
            deaths = sum(NEW_DEATHS, na.rm = TRUE)) 

model_cases_num <-
  new_cases_num %>%
  as_tsibble() %>%
  model(STL(cases ~ trend())) %>%
  components() %>%
  select(date, trend) %>%
  rename(trend_cases = trend) %>%
  as_tibble()

model_deaths_num <-
  new_cases_num %>%
  as_tsibble() %>%
  model(STL(deaths ~ trend())) %>%
  components() %>%
  select(date, trend) %>%
  rename(trend_deaths = trend) %>%
  as_tibble()

new_cases_num <-
  new_cases_num %>%
  left_join(model_cases_num,  by = "date") %>%
  left_join(model_deaths_num, by = "date") %>%
  rename(value_cases  = cases,
         value_deaths = deaths) %>%
  pivot_longer(-date, names_to = c("value_trend", 
                                   "cases_deaths"), 
               names_sep = "_", 
               values_to = "value") %>%
  filter(!(cases_deaths == "deaths" & date < as.Date("2020-04-10"))) %>%
  mutate(num_age = "num") %>%
  pivot_wider(id_cols = c("date", "num_age", "cases_deaths"),
               names_from = "value_trend",
               values_from = "value")

### Output should have fields:
### date, num_age, cases_deaths, value, trend
### num_age = num or age
### cases_deaths = cases or deaths
# NEW_DEATHS starts on 2020-04-10

new_cases_age <-
  age_df %>%
  unique() %>% 
  select(DATE, AGE, TOTAL_CASES) %>%
  mutate(AGE = gsub(" years", "", AGE)) %>%
  filter(AGE != "Pending") %>%
  arrange(DATE) %>%
  pivot_wider(id_cols = "DATE", names_from = "AGE", values_from = "TOTAL_CASES") %>%
  mutate_if(is.numeric, ~ replace_na(., 0)) %>%
  mutate_at(vars(-"DATE"), ~ . - lag(.)) %>%
  mutate(Total = rowSums(.[2:10])) %>%
  filter(!is.na(Total)) %>%
  select(-Total) %>%
  pivot_longer(cols = -DATE) %>%
  mutate(value = if_else(value < 0, 0, value)) %>%
  mutate(
    avg_age = case_when(
                         name == "0-10"  ~ 5,
                         name == "11-20" ~ 15,
                         name == "21-30" ~ 25,
                         name == "31-40" ~ 35,
                         name == "41-50" ~ 45,
                         name == "51-60" ~ 55,
                         name == "61-70" ~ 65,
                         name == "71-80" ~ 75,
                         name == "81+"   ~ 85,
    ))

new_cases_tib <- tibble(date = as.Date(character()), avg_age = double())
for (date in as.Date(new_cases_age$DATE) %>% unique()) {

  ### Pluck out the entries for this data
  new_cases_ss <-
    new_cases_age %>%
    filter(DATE == as.Date(date, origin = "1970-01-01")) %>%
    mutate(avg_age_contr = avg_age * value ) %>%
    mutate()

  this_avg_age <- sum(new_cases_ss$avg_age_contr) / sum(new_cases_ss$value)

  new_cases_tib <-
    new_cases_tib %>%
    add_row(date = as.Date(date, origin = "1970-01-01"), avg_age=this_avg_age)
}

new_cases_tib <-
  new_cases_tib %>%
  mutate(date = as.Date(date)) %>%
  as_tsibble(index = "date") %>%
  tsibble::fill_gaps() %>%
  mutate(avg_age = na.aggregate(avg_age))

new_deaths_age <-
  age_df %>% 
  unique() %>%
  filter(DATE >= as.Date("2020-04-10")) %>%
  select(DATE, AGE, TOTAL_DEATHS) %>%
  mutate(AGE = gsub(" years", "", AGE)) %>%
  filter(AGE != "Pending") %>%
  arrange(DATE) %>%
  pivot_wider(id_cols = c("DATE"), names_from = c("AGE"), values_from = c("TOTAL_DEATHS")) %>%
  mutate_at(vars(-"DATE"), ~ . - lag(.)) %>%
  mutate_if(is.numeric, ~ replace_na(., 0)) %>%
  filter(DATE > as.Date("2020-04-10")) %>%
  mutate(Total = rowSums(.[2:10])) %>%
  filter(!is.na(Total)) %>%
  select(-Total) %>%
  pivot_longer(cols = -DATE) %>%
  mutate(value = if_else(value < 0, 0, value)) %>%
  mutate(
    avg_age = case_when(
      name == "0-10"  ~ 5,
      name == "11-20" ~ 15,
      name == "21-30" ~ 25,
      name == "31-40" ~ 35,
      name == "41-50" ~ 45,
      name == "51-60" ~ 55,
      name == "61-70" ~ 65,
      name == "71-80" ~ 75,
      name == "81+"   ~ 85,
    )) %>%
  arrange(DATE)

new_deaths_tib <-
  tibble(date = as.Date(character()),
         avg_age = double())

for (date in as.Date(new_deaths_age$DATE) %>% unique()) {

  ### Pluck out the entries for this data
  new_deaths_ss <-
    new_deaths_age %>%
    filter(DATE == as.Date(date, origin = "1970-01-01")) %>%
    mutate(avg_age_contr = avg_age * value ) %>%
    mutate()

  this_avg_age <- sum(new_deaths_ss$avg_age_contr) / sum(new_deaths_ss$value)

  new_deaths_tib <-
    new_deaths_tib %>%
    add_row(date = as.Date(date, origin = "1970-01-01"), avg_age=this_avg_age)

}

new_deaths_tib <-
  new_deaths_tib %>%
  mutate(date = as.Date(date)) %>%
  as_tsibble(index = "date") %>%
  tsibble::fill_gaps() %>%
  mutate(avg_age = na.aggregate(avg_age))

model_new_cases_age <-
  new_cases_tib %>%
  model(STL(avg_age ~ trend())) %>%
  components()

model_new_cases_age %>% autoplot()

model_new_deaths_age <-
  new_deaths_tib %>%
#  model(STL(avg_age ~ trend())) %>%
  model(STL(avg_age ~ trend(window = 21))) %>%
  components()

model_new_deaths_age %>% autoplot()

cases_trend  <- model_new_cases_age  %>% select(date, trend) %>% rename(cases_trend = trend)
deaths_trend <- model_new_deaths_age %>% select(date, trend) %>% rename(deaths_trend = trend)

cases_deaths_tsib <-
  (new_cases_tib %>% rename(cases = avg_age)) %>%
  full_join((new_deaths_tib %>% rename(deaths = avg_age)), by = "date") %>%
  full_join(cases_trend, by = "date") %>%
  full_join(deaths_trend, by = "date") %>%
  as_tibble()

cases_deaths_tib <- 
  cases_deaths_tsib %>% 
  rename(cases_value = cases, 
         deaths_value = deaths) %>% 
  pivot_longer(-date, 
               names_to = c("cases_deaths", "value_trend"), 
               names_sep = "_", 
               values_to = "values") %>%
  pivot_wider(id_cols = c("date", "cases_deaths"),
              names_from = "value_trend",
              values_from = "values") %>%
  mutate(num_age = "age") %>%
  filter(!(cases_deaths == "deaths" & date < as.Date("2020-04-10"))) %>%
  select(date, num_age, cases_deaths, value, trend) %>%
  bind_rows(new_cases_num)

data <-
  cases_deaths_tib %>%
  mutate(descr = case_when(
         num_age == "num" & cases_deaths == "cases"  ~ "New Cases",
         num_age == "num" & cases_deaths == "deaths" ~ "New Deaths",
         num_age == "age" & cases_deaths == "cases"  ~ "Average Age of New Cases",
         num_age == "age" & cases_deaths == "deaths" ~ "Average Age of New Deaths",
         ))

g_cases_and_deaths_facet <-
  ggplot(data = data, aes(x = as.Date(date))) +
  theme_bw() +
  theme(legend.position = "none") +
  
  geom_point(aes(y = value), size = 1, shape = 19, alpha = 0.2) +
  
  geom_line(aes(y = trend), color = "firebrick3", size = 1.3) +
  
  geom_hline(data = subset(data, descr == "Average Age of New Cases") %>% tail(n = 1), 
             aes(yintercept = 38.7), linetype = "dotted") + 
  
  geom_text(data = subset(data, descr == "Average Age of New Cases") %>% tail(n = 1), 
            aes(x = as.Date("2020-06-15"), y = 39),
            size = 4, color = "#333333",
            label = "Median Age of TN = 38.7 yrs") +

  facet_wrap(~ descr, scales = "free_y") +
  
  labs(title = "Average Age of TN New Cases and New Deaths", x = "Date", y = "Average Age")

print(g_cases_and_deaths_facet)
