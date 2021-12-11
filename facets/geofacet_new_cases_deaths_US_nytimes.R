library(dplyr)
library(tidyr)
library(ggplot2)
library(janitor)
library(cowplot)
library(lubridate)
library(tidycensus)

# Get state population
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

# Read COVID cases/deaths data from NYTimes
data <- 
  read_csv("../Datasets/nytimes/covid-19-data/us-states.csv")

# Summarize individual state data to make US-wide data
us_data <- 
  data %>% 
  select(date, cases, deaths) %>% 
  group_by(date) %>% 
  summarize(cases  = sum(cases), 
            deaths = sum(deaths)) %>% 
  mutate(state = "United States", fips = "99") %>% 
  select(date, state, fips, cases, deaths)

data <- data %>% bind_rows(us_data)

# Process data (turn total cases/deaths -> new cases/deaths, 7-day MA)
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
  
  # Filter out "blips" in the data where states suddenly revise the number of cases/deaths
  mutate(`deaths_Delaware`  = ifelse(date == as.Date("2021-07-30"), 0, `deaths_Delaware`)) %>%
  mutate(`deaths_Kentucky`  = ifelse(date == as.Date("2021-06-01"), 0, `deaths_Kentucky`)) %>%
  mutate(`deaths_Nebraska`  = ifelse(date == as.Date("2021-05-28"), 0, `deaths_Nebraska`)) %>%
  mutate(`deaths_Maryland`  = ifelse(date == as.Date("2021-05-27"), 0, `deaths_Maryland`)) %>%
  mutate(`deaths_Oklahoma`  = ifelse(date == as.Date("2021-05-26"), 0, `deaths_Oklahoma`)) %>%
  mutate(`cases_Washington` = ifelse(date == as.Date("2020-01-21"), 1, `cases_Washington`)) %>%
    
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
  filter(date >= as.Date("2021-06-01")) %>% 
  filter(!state %in% c("Guam", "Northern Mariana Islands", "Virgin Islands", "Puerto Rico"))# %>%
  #filter(state %in% c("Tennessee"))

us_deaths_avg <- data %>% filter(date == data %>% arrange(date) %>% tail(n = 1) %>% pull(date)) %>% filter(state == "United States") %>% pull("sma_percapita_deaths")
us_cases_avg  <- data %>% filter(date == data %>% arrange(date) %>% tail(n = 1) %>% pull(date)) %>% filter(state == "United States") %>% pull("sma_percapita_cases")

color_cases <-
  data %>% 
  filter(date == data %>% arrange(date) %>% tail(n = 1) %>% pull(date)) %>% 
  select(state, sma_percapita_cases) %>%
  mutate(fillcolor_cases = if_else(sma_percapita_cases < us_cases_avg, "green", "red")) %>%
  mutate(fillcolor_cases = if_else(state == "United States", "blue", fillcolor_cases)) %>%
  select(state, fillcolor_cases)

color_deaths <-
  data %>% 
  filter(date == data %>% arrange(date) %>% tail(n = 1) %>% pull(date)) %>% 
  select(state, sma_percapita_deaths) %>%
  mutate(fillcolor_deaths = if_else(sma_percapita_deaths < us_deaths_avg, "green", "red")) %>%
  mutate(fillcolor_deaths = if_else(state == "United States", "blue", fillcolor_deaths)) %>%
  select(state, fillcolor_deaths)

data <- 
  data %>%
  left_join(color_cases, by = "state") %>% 
  left_join(color_deaths, by = "state")

us_cases  <- data %>% filter(state == "United States") %>% tail(n = 1) %>% pull("sma_percapita_cases")
us_deaths <- data %>% filter(state == "United States") %>% tail(n = 1) %>% pull("sma_percapita_deaths")

g_excess_cases <-
  ggplot(data = data %>% filter(!state %in% c("United States", "District of Columbia"))) + 
  theme_bw() +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, col = "gray") +
  geom_area(aes(x = as.Date(date) - days(3), y = sma_percapita_cases, fill = fillcolor_cases), color = "black", size = 0.1) + 
  facet_geo(~ state, grid = us_state_grid1 %>% filter(!name == "District of Columbia")) + 
  scale_fill_manual(values = c("red" = "firebrick2", "green" = "seagreen4", "blue" = "steelblue2")) + 
  labs(title = paste("New Cases per Capita - ", data %>% arrange(date) %>% tail(n = 1) %>% pull(date)), x = "Date", y = "") +
  scale_y_continuous(labels = scales::percent)
print(g_excess_cases)

g_excess_deaths <-
  ggplot(data = data %>% filter(!state %in% c("United States", "District of Columbia"))) + 
  theme_bw() +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, col = "gray") +
  geom_area(aes(x = as.Date(date) - days(3), y = sma_percapita_deaths, fill = fillcolor_deaths), color = "black", size = 0.1) + 
  scale_fill_manual(values = c("red" = "firebrick2", "green" = "seagreen4", "blue" = "steelblue2")) + 
  facet_geo(~ state, grid = us_state_grid1 %>% filter(!name == "District of Columbia")) + 
  labs(title = paste("New Deaths per Capita - ", data %>% arrange(date) %>% tail(n = 1) %>% pull(date)), x = "Date", y = "") +
  scale_y_continuous(labels = scales::percent)
#print(g_excess_deaths)
