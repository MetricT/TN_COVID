### Make a facet map of US states with the number of new cases over time

library(tidyverse)
library(tidycensus)
library(cowplot)
library(TTR)
library(geofacet)
library(broom)

### Pull cases data from the NY Times repo below
###
### https://github.com/nytimes/covid-19-data
###
spreadsheet <-
  "../Datasets/nytimes/covid-19-data/us-states.csv" %>%
  read_csv(col_names = TRUE, col_types = "Dccdd")

### How big should our SMA window be?
SMA_n <- 7

### Pull state population data from the Census
pop2018 <-
  get_acs(geography   = "state",
          variables   = c("B01003_001"),
          year        = 2018,
          geometry    = FALSE,
          cache_table = TRUE) %>%
  rename(POP2018 = estimate) %>%
  select(NAME, POP2018) %>%
  left_join(fips_codes %>%
              select(state_name, state) %>%
              unique() %>%
              as_tibble(),
            by = c("NAME" = "state_name")) %>%
  rename(state_abbr = state,
         state = NAME) %>%
  select(state, state_abbr, POP2018)

### Take the data and convert it to a 7-day SMA of new cases per capita
data <-
  spreadsheet %>%
  select(date, state, cases) %>%
  arrange(date) %>%
  pivot_wider(id_cols = "date", names_from = "state", values_from = "cases") %>%
  mutate(across(!starts_with("date"),
                .fns = list(new = ~ c(.[1], diff(.))),
                .names = "{fn}_{col}"
  )) %>%
  select(date, contains("new_"))  %>%
  rename_at(vars(contains("new_")),
            ~ str_replace(., "new_", "")) %>%
  mutate(across(!starts_with("date"),
                .fns = list(sma = ~ SMA(., n = SMA_n)),
                .names = "{fn}_{col}"
  )) %>%
  select(date, contains("sma_"))  %>%
  rename_at(vars(contains("sma_")),
            ~ str_replace(., "sma_", "")) %>%
  pivot_longer(-date, names_to = "state", values_to = "new_cases") %>%
  left_join(pop2018, by = c("state" = "state")) %>%
  mutate(new_cases_percapita = 100000 * new_cases / POP2018) %>%
  select(date, state, state_abbr, new_cases_percapita)


### Add dot to the end of the line for reference
dots <-
  data %>%
  select(date, state, new_cases_percapita) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "new_cases_percapita") %>%
  tail(n = 1) %>%
  pivot_longer(-date, names_to = "state", values_to = "new_cases_percapita") %>%
  rename(dots = new_cases_percapita)

data <- data %>% left_join(dots, by = c("date" = "date", "state" = "state"))

### Compute the growth rate over the last 2 weeks
growth <-
  data %>%
  select(date, state, new_cases_percapita) %>%
  filter(date >= as.Date(Sys.Date()) - 14) %>%
  filter(!state %in% c("Virgin Islands",
                       "Guam",
                       "Northern Mariana Islands")) %>%
  group_by(state) %>%
  do(tidy(lm(new_cases_percapita ~ date, .))) %>%
  filter(term == "date") %>%
  select(state, estimate) %>%
  rename(growth = estimate) %>%
  arrange(growth)

#data <- data %>% left_join(dots, by = c("date" = "date", "state" = "state"))

this_title <-
  paste("New Cases per 100k (", SMA_n, "-day moving average)", sep = "")

g_facet <-
  ggplot(data = data) +
  theme_bw() +
  geom_line(aes(x = as.Date(date), y = new_cases_percapita)) +
  geom_point(aes(x = as.Date(date), y = dots), color = "blue") +
  facet_geo(~ state) +
  labs(title = this_title,
       caption = "", x = "", y = "")
print(g_facet)
