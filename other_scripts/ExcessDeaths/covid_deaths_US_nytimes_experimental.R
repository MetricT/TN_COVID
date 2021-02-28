# Largely based on technique from:  https://robjhyndman.com/hyndsight/excess-deaths/

library(dplyr)
library(tidyr)
library(ggplot2)
library(janitor)
library(cowplot)
library(lubridate)
library(tidycensus)

### Get state population
pop <-
  get_acs(geography   = "state",
          variables   = c("B01003_001"),
          year        = 2018,
          geometry    = FALSE,
          cache_table = TRUE) %>%
  rename(POP2018 = estimate,
         state   = NAME) %>%
  select(state, POP2018) %>%
  add_row(state   = "United States",
          POP2018 = 326289971)

data <- 
  read_csv("../Datasets/nytimes/covid-19-data/us-states.csv")

us_data <- 
  data %>% 
  select(date, cases, deaths) %>% 
  group_by(date) %>% 
  summarize(cases  = sum(cases), 
            deaths = sum(deaths)) %>% 
  mutate(state = "United States", fips = "99") %>% 
  select(date, state, fips, cases, deaths)

data <- data %>% bind_rows(us_data)

data <-
  data %>%
  select(date, state, cases, deaths) %>% 
  pivot_wider(id_cols = "date",
            names_from = "state",
            values_from = c("cases", "deaths")) %>%
  mutate_if(is.numeric, ~ (replace_na(., 0))) %>%

  # Use diff() to turn total cases/deaths into new cases/deaths
  mutate(across(-date, ~ . - lag(.))) %>%
  mutate_if(is.numeric, ~ (replace_na(., 0))) %>% 
  mutate(`cases_Washington` = if_else(date == as.Date("2020-01-21"), 1, `cases_Washington`)) %>%
  
  # Take the new count and do a 7-day SMA to smooth it
  mutate(across(!starts_with("date"), .fns = list(sma = ~ SMA(., n = 7)), .names = "{fn}_{col}")) %>%
  rename_at(vars(starts_with("sma_")), ~ str_replace(., "sma_", "sma")) %>% 
  
  pivot_longer(-date, names_to = c("type", "state"), names_sep = "_", values_to = "values") %>% 
  pivot_wider(id_cols = c("date", "state"), names_from = "type", values_from = "values") %>%
  rename(sma_cases = smacases,
         sma_deaths = smadeaths) %>%
  left_join(pop, by = c("state")) %>%
  mutate(percapita_cases  = cases / POP2018,
         percapita_deaths = deaths / POP2018,
         sma_percapita_cases = sma_cases / POP2018,
         sma_percapita_deaths = sma_deaths / POP2018) %>%
  filter(date >= as.Date("2020-09-01")) %>% 
  filter(!state %in% c("Guam", "Northern Mariana Islands", "Virgin Islands", "Puerto Rico"))# %>%
  #filter(state %in% c("Tennessee"))

### This currently graphs by states showing the highest number of cases throughout
### the history of the pandemic.   Come back and write a smarter order function that
### looks at just the last two weeks, as well as the trend growth (+/-) during that time.
order_cases <-
  data %>% 
  filter(date == data %>% arrange(date) %>% tail(n = 1) %>% pull(date)) %>%
  select(state, sma_percapita_cases) %>% 
  group_by(state) %>% 
  summarize(max = max(sma_percapita_cases)) %>% 
  arrange(desc(max)) %>%
  pull("state")

order_deaths <-
  data %>% 
  filter(date == data %>% arrange(date) %>% tail(n = 1) %>% pull(date)) %>%
  select(state, sma_percapita_deaths) %>% 
  group_by(state) %>% 
  summarize(max = max(sma_percapita_deaths)) %>% 
  arrange(desc(max)) %>%
  pull("state")

data_cases <-
  data %>% 
  select(date, state, cases, percapita_cases, sma_percapita_cases) %>%
  mutate(state = fct_relevel(state, order_cases))

data_deaths <-
  data %>% 
  select(date, state, deaths, percapita_deaths, sma_percapita_deaths) %>%
  mutate(state = fct_relevel(state, order_deaths))

us_deaths_avg <- data_deaths %>% filter(date == data %>% arrange(date) %>% tail(n = 1) %>% pull(date)) %>% filter(state == "United States") %>% pull("sma_percapita_deaths")
us_cases_avg  <- data_cases  %>% filter(date == data %>% arrange(date) %>% tail(n = 1) %>% pull(date)) %>% filter(state == "United States") %>% pull("sma_percapita_cases")

color_cases <-
  data_cases %>% 
  filter(date == data %>% arrange(date) %>% tail(n = 1) %>% pull(date)) %>% 
  select(state, sma_percapita_cases) %>%
  mutate(fillcolor = if_else(sma_percapita_cases < us_cases_avg, "green", "red")) %>%
  mutate(fillcolor = if_else(state == "United States", "blue", fillcolor)) %>%
  select(state, fillcolor)

color_deaths <-
  data_deaths %>% 
  filter(date == data %>% arrange(date) %>% tail(n = 1) %>% pull(date)) %>% 
  select(state, sma_percapita_deaths) %>%
  mutate(fillcolor = if_else(sma_percapita_deaths < us_deaths_avg, "green", "red")) %>%
  mutate(fillcolor = if_else(state == "United States", "blue", fillcolor)) %>%
  select(state, fillcolor)

data_cases  <- data_cases  %>% left_join(color_cases,  by = "state")
data_deaths <- data_deaths %>% left_join(color_deaths, by = "state")

g_excess_cases <-
  ggplot(data = data_cases %>% filter(!state %in% c("United States", "District of Columbia"))) + 
  theme_bw() +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, col = "gray") +
  geom_area(aes(x = as.Date(date) - days(3), y = sma_percapita_cases, fill = fillcolor), color = "black", size = 0.1) + 
  #geom_area(aes(x = date, y = percapita_cases), fill = "orange", color = NA)  +
  #geom_line(aes(x = as.Date(date) - days(3), y = sma_percapita_cases), linetype = "dotted") + 
  facet_wrap(~ state, ncol = 10) +
  scale_fill_manual(values = c("red" = "firebrick2", "green" = "seagreen4", "blue" = "steelblue2")) + 
  labs(title = paste("New Cases per Capita - ", data %>% arrange(date) %>% tail(n = 1) %>% pull(date)), x = "Date", y = "") +
  scale_y_continuous(labels = scales::percent)
print(g_excess_cases)

g_excess_deaths <-
  ggplot(data = data_deaths %>% filter(!state %in% c("United States", "District of Columbia"))) + 
  theme_bw() +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, col = "gray") +
  geom_area(aes(x = as.Date(date) - days(3), y = sma_percapita_deaths, fill = fillcolor), color = "black", size = 0.1) + 
  scale_fill_manual(values = c("red" = "firebrick2", "green" = "seagreen4", "blue" = "steelblue2")) + 
  facet_wrap(~ state, ncol = 10) +
  labs(title = paste("New Deaths per Capita - ", data %>% arrange(date) %>% tail(n = 1) %>% pull(date)), x = "Date", y = "") +
  scale_y_continuous(labels = scales::percent)
print(g_excess_deaths)

