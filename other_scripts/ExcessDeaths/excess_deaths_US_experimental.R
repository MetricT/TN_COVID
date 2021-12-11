################################################################################
### Pull mortality data from the CDC and graph excess death - /u/MetricT
################################################################################

library(tidyverse)
library(janitor)
library(cowplot)
library(lubridate)
library(tidycensus)
library(tsibble)
library(geofacet)
library(zoo)
library(forecast)
library(broom)


# Install your Census API key
# You can request one at https://api.census.gov/data/key_signup.html
# census_api_key("<YOUR_CENSUS_API_KEY_HERE>", install = TRUE)


# Load data on finalized (2014-2018) and provisional (2019-2020) mortality
finalized_ss   <- read_csv("https://data.cdc.gov/api/views/3yf8-kanr/rows.csv?accessType=DOWNLOAD")
provisional_ss <- read_csv("https://data.cdc.gov/api/views/muzy-jte6/rows.csv?accessType=DOWNLOAD")


# Take the data and clean it up a bit
finalized   <- finalized_ss   %>% clean_names() %>% select("jurisdiction_of_occurrence", "mmwr_year", "mmwr_week", "all_cause")
provisional <- provisional_ss %>% clean_names() %>% select("jurisdiction_of_occurrence", "mmwr_year", "mmwr_week", "all_cause")


# Combine both datasets and filter the subset of states we want to look at
deaths <-
  finalized %>%
  bind_rows(provisional) %>%
  rename(year = mmwr_year,
         week = mmwr_week,
         deaths = all_cause,
         state = jurisdiction_of_occurrence) %>%

  # Quick hack to remove "week 53" in 2014 because yearweek() can't handle it
  filter(week != 53) %>%

  # Add New York + New York City together (don't know why they do it this way)
  pivot_wider(id_cols = c("year", "week"), names_from = "state", values_from = "deaths") %>%
  mutate(NewYorkAll = `New York` + `New York City`) %>%
  select(-`New York`, -`New York City`) %>%
  rename(`New York` = NewYorkAll) %>%
  pivot_longer(-c("year", "week"), names_to = "state", values_to = "deaths") %>%

  # Turn the year and week into a yearweek() datatype
  mutate(yearweek = yearweek(date_decimal(year + (week - 1) / 52))) %>%

  # Uncomment the line appropriate for the states you want to analyze
  # I may need to combine New York + New York City, but won't know until done
  # Also may need to remove the last two weeks of data for some state since
  # they're late with their mortality data
  #   filter(row_number() <= n() - 2) %>%

  #filter(state %in% c("Tennessee")) # <- Just Tennessee
  filter(!state %in% c("United States", "Puerto Rico")) # <- The entire US


# What is the latest date available
last_date <-
  deaths %>%
  arrange(yearweek) %>%
  tail(n = 1) %>%
  pull(yearweek) %>%
  as.Date()

# Model deaths and extract the seasonally-adjusted deaths
deaths_mod <-
  deaths %>%
  select(yearweek, state, deaths) %>%
  mutate(state = paste("state:", state, sep = "")) %>%
  pivot_wider(id_cols = "yearweek", names_from = "state", values_from = "deaths") %>%

  # Convert to a tsibble and fill in gaps by interpolation
  as_tsibble() %>%
  fill_gaps() %>%
  mutate_if(is.numeric, ~ na.locf(.)) %>%

  # Take the raw data and compute the seasonally-adjusted value
  mutate(across(starts_with("state:"),
                   .fns = list(trend = ~ (.) %>% ts() %>% mstl() %>% seasadj()),
                   .names = "{fn}_{col}")) %>%
  select("yearweek", starts_with("trend_state")) %>%
  rename_at(vars(starts_with("trend_state:")), ~ str_replace(., "trend_state:", "")) %>%
  pivot_longer(-yearweek, names_to = "state", values_to = "deaths_seas_adj")


# Compute a least-squares regression of the seasonally-adjusted data from
# 2014-2019 so we can subtract off the population growth term
fit_deaths <-
  deaths_mod %>%
  filter(yearweek < yearweek("2020-01-01")) %>%
  mutate(state = paste("state:", state, sep = "")) %>%
  pivot_wider(id_cols = "yearweek", names_from = "state", values_from = "deaths_seas_adj") %>%
  mutate(decimal_date = year(yearweek) + (week(yearweek) - 1) / 52) %>%

  # Compute a linear regression so we can subtract out deaths due to growing/falling population
  mutate(across(starts_with("state:"),
                .fns = list(intercept  = ~ lm((.) ~ decimal_date) %>% tidy() %>% filter(term == "(Intercept)")  %>% pull("estimate")),
                .names = "{fn}_{col}")) %>%
  mutate(across(starts_with("state:"),
                .fns = list(coefficient = ~ lm((.) ~ decimal_date) %>% tidy() %>% filter(term == "decimal_date") %>% pull("estimate")),
                .names = "{fn}_{col}")) %>%
  select(yearweek, starts_with("intercept_state"), starts_with("coefficient_state")) %>%
  head(n = 1) %>%
  pivot_longer(-yearweek, names_to = c("type", "state"), names_sep = "_", values_to = "value") %>%
  as_tibble() %>%
  select(state, type, value) %>%
  mutate(state = gsub("^state:", "", state)) %>%
  pivot_wider(id_cols = "state", names_from = "type", values_from = "value")


# Take the regression coefficients above, subtract off population growth, and
# get the detrended Excess Deaths
deaths_mod <-
  deaths_mod %>%
  left_join(fit_deaths, by = "state") %>%
  mutate(decimal_date = year(yearweek) + (week(yearweek) - 1) / 52) %>%
  mutate(pop_deaths = intercept + coefficient * decimal_date) %>%
  mutate(detrend = deaths_seas_adj - pop_deaths)


# Fetch county population so we can compute per capita
pop <-
  get_acs(geography   = "state",
          variables   = c("B01003_001"),
          year        = 2019,
          geometry    = FALSE,
          cache_table = TRUE) %>%
  rename(population = estimate,
         state   = NAME) %>%
  select(state, population)

# Only show states we have data for
facet_map <-
  us_state_grid1 %>%
  filter(name %in% (deaths %>% pull(state) %>% unique())) %>%
  mutate(row = row - min(row) + 1,
         col = col - min(col) + 1)


# Compute Excess Deaths per capita and graph
deaths_mod %>%
  left_join(pop, by = "state") %>%
  mutate(detrend_capita = detrend / population) %>%
  select(yearweek, state, detrend_capita) %>%
  filter(yearweek >= as.Date("2020-01-01"), yearweek < as.Date("2021-07-01")) %>%
  ggplot() +
  theme_bw() +
  geom_area(aes(x = as.Date(yearweek), y = detrend_capita), fill = "darkorange") +
  scale_x_date(breaks = pretty_breaks(3)) +
  scale_y_continuous(
    limits = c(0, NA),
    labels = scales::percent_format(accuracy = 0.001)
    ) +
  labs(x = "",
       y = "",
       title = "Excess Deaths per Capita - 2020-01-01 to 2021-06-30",
       caption = "Mortality Data from the Centers for Disease Control"
       ) +
  facet_geo(~ state, grid = facet_map)