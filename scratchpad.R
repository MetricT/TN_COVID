### Just some ideas I'm working on.   Run TN_COVID.R to load the data, and then
### you can play with these.

################################################################################
###  What percent of new cases are males?
###############################################################################
males <-
  race_ethnicity_sex_df %>% 
  filter(CAT_DETAIL != "Pending") %>% 
  filter(Category == "SEX") %>% 
  select(Date, CAT_DETAIL, Cat_CaseCount) %>% 
  pivot_wider(id_cols = "Date", 
              names_from = "CAT_DETAIL", 
              values_from = Cat_CaseCount) %>% 
  mutate(across(!starts_with("Date"),
              .fns = list(new = ~ c(0, diff(.))),
              .names = "{fn}_{col}")) %>%
  filter(Date >= as.Date("2020-04-09")) %>%
  mutate(male_frac = new_Male / (new_Male + new_Female)) %>%
  select(Date, male_frac) 

males_trend <- 
  males %>%
  as_tsibble(index = "Date") %>%
  tsibble::fill_gaps() %>%
  na.locf() %>%
  model(STL(male_frac ~ trend())) %>%
  components() %>%
  select(Date, trend)

males <-
  males %>%
  full_join(males_trend, by = "Date") 

g_males <-
  ggplot(data = males, aes(x = as.Date(Date))) +
  theme_classic() +
  geom_point(aes(y = male_frac)) +
  geom_line(aes(y = trend), color = "darkgreen") +
  geom_hline(yintercept = 0.5, color = "darkred", linetype = "dotted") +
  labs(title = "Males as % of New Cases", x = "Date", y = "Percent")

print(g_males)


################################################################################
###  What percent of new deaths are males?
###############################################################################
males <-
  race_ethnicity_sex_df %>% 
  filter(CAT_DETAIL != "Pending") %>% 
  filter(Category == "SEX") %>% 
  select(Date, CAT_DETAIL, CAT_DEATHCOUNT) %>% 
  pivot_wider(id_cols = "Date", 
              names_from = "CAT_DETAIL", 
              values_from = CAT_DEATHCOUNT) %>% 
  mutate(across(!starts_with("Date"),
                .fns = list(new = ~ c(0, diff(.))),
                .names = "{fn}_{col}")) %>%
  filter(Date >= as.Date("2020-06-26")) %>%
  mutate(male_frac = new_Male / (new_Male + new_Female)) %>%
  select(Date, male_frac)

### Filter out the huge outlier on 7/26
males <-
  males %>%
  anomalize(male_frac) %>%
  filter(anomaly == "No") %>%
  select(Date, male_frac)

males_trend <- 
  males %>%
  as_tsibble(index = "Date") %>%
  tsibble::fill_gaps() %>%
  na.locf() %>%
  model(STL(male_frac ~ trend())) %>%
  components() %>%
  select(Date, trend)

males <-
  males %>%
  full_join(males_trend, by = "Date") 

g_males <-
  ggplot(data = males, aes(x = as.Date(Date))) +
  theme_classic() +
  geom_point(aes(y = male_frac)) +
  geom_line(aes(y = trend), color = "darkgreen") +
  geom_hline(yintercept = 0.5, color = "darkred", linetype = "dotted") +
  labs(title = "Males as % of New Deaths", x = "Date", y = "Percent")

print(g_males)

################################################################################
###
################################################################################


race_new_cases_deaths <-
  race_ethnicity_sex_df %>% 
  filter(CAT_DETAIL != "Pending") %>% 
  mutate(CAT_DETAIL = if_else(CAT_DETAIL == "Other/ Multiracial", "Other/Multiracial", CAT_DETAIL)) %>%
  filter(Category == "RACE") %>% 
  select(Date, CAT_DETAIL, Cat_CaseCount, CAT_DEATHCOUNT) %>% 
  rename(date = Date, 
         detail = CAT_DETAIL, 
         cases = Cat_CaseCount,
         deaths = CAT_DEATHCOUNT) %>%
  pivot_longer(cols = c(cases, deaths), names_to = "cases_deaths", values_to = "values") %>%
  pivot_wider(id_cols = "date", 
              names_from = c("cases_deaths", "detail"),
              names_sep = ":", 
              values_from = "values") %>%
  mutate(across(!starts_with("date"),
                .fns = list(new = ~ c(0, diff(.))),
                .names = "{fn}_{col}")) %>%
  filter(date >= Sys.Date() - 15) %>%
  select(date, starts_with("new_")) %>%
  rename_at(vars(starts_with("new_")),
            ~ gsub("new_", "", (.))) %>%
#  mutate(across(-date, ~ if_else(. < 0, 0, .))
  pivot_longer(-date, 
               names_to = c("cases_deaths", "detail"), 
               names_sep = ":", 
               values_to = "values") %>%
  select(-date) %>%
  group_by(cases_deaths, detail) %>%
  summarize(count = sum(values)) %>%
  mutate(total = if_else(cases_deaths == "cases", 26831, 256)) %>%
  mutate(frac = count / total)





g_facet_race <-
  ggplot(data = race_new_cases_deaths, 
         aes(x = as.Date(date), y = values)) +
  theme_bw() +
  theme(legend.position = "none") +
  
  geom_point(size = 1, shape = 19, alpha = 0.2) +
  
#  geom_line(cases_deaths_tib, color = "firebrick3",
#            mapping = aes(y = trend), size = 1.3) +

  facet_wrap(~ cases_deaths + detail) +
  
  labs(title = "Facet of Cases/Deaths by Race", x = "Date", y = "Average Age")

print(g_facet_race)

################################################################################
### Looking at MMWR data that was just released.
################################################################################

mmwr_data <- 
  mortality_df %>% 
  select(DATE, COUNTY_CASE_COUNT) %>% 
  group_by(DATE) %>% 
  summarize(CASES = sum(COUNTY_CASE_COUNT)) %>%
  as_tsibble(index = "DATE") %>% 
  tsibble::fill_gaps() %>% 
  mutate(CASES = if_else(is.na(CASES), 0, CASES))
  
mmwr_model <-
  mmwr_data %>%
  model(STL(CASES ~ trend() + season(period = "week") + season(period = "month"))) %>%
  components() 
  
  ### Create a new tibble using the decomposed components
mmwr_tib <-
  tibble(date      = mmwr_data  %>% pull("DATE"),
         values    = mmwr_data  %>% pull("CASES"),
         trend     = mmwr_model %>% pull("trend"),
         s_week    = mmwr_model %>% pull("season_week"),
         s_month   = mmwr_model %>% pull("season_month"),
         s_adjust  = mmwr_model %>% pull("season_adjust"),
         remainder = mmwr_model %>% pull("remainder"),
    ) %>% as_tsibble(index = "date")
  
### Let's plot the weekly pattern
g_week_mortality <-
  mmwr_tib %>%
  gg_season(s_week, period = "1 week") +
  xlab("Day of Week") +
  ylab("New cases") +
  ggtitle("Weekly Seasonal Component of MMWR")
print(g_week_mortality)

g_month_mortality <-
  mmwr_tib %>%
  gg_season(s_month, period = "month") +
  xlab("Day of Month") +
  ylab("New cases") +
  ggtitle("Weekly Seasonal Component of MMWR")
print(g_month_mortality)