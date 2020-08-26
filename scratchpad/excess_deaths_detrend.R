# Largely based on technique from:  https://robjhyndman.com/hyndsight/excess-deaths/

library(dplyr)
library(tidyr)
library(ggplot2)
library(janitor)
library(cowplot)

### Data on finalized (2014-2018) and provisional (2019-2020) all-causes death counts
finalized_ss   <- readr::read_csv("https://data.cdc.gov/api/views/3yf8-kanr/rows.csv?accessType=DOWNLOAD")
provisional_ss <- readr::read_csv("https://data.cdc.gov/api/views/muzy-jte6/rows.csv?accessType=DOWNLOAD")

### Take the data, clean the names up a bit, and pick the columns we want
finalized <-
  finalized_ss %>%
  janitor::clean_names() %>%
  select("jurisdiction_of_occurrence", "mmwr_year", "mmwr_week", "all_cause")

provisional <-
  provisional_ss %>%
  janitor::clean_names() %>%
  select("jurisdiction_of_occurrence", "mmwr_year", "mmwr_week", "all_cause")


### Take the finalized and provisional data, stack them together, 
### and rename the rows to shorter names
deaths <-
  finalized %>%
  bind_rows(provisional) %>%
  rename(year = mmwr_year,
         week = mmwr_week,
         deaths = all_cause,
         state = jurisdiction_of_occurrence) %>%
#  mutate(year = as.integer(year)) %>%
#  mutate(week = as.integer(week)) %>%
  
  ### 2014 has 53 months for unknown reasons, so remove the first and renumber
  mutate(week = if_else(year == 2014, week - 1, week)) %>% 
  filter(week != 0) %>%
   
  ### Comment out the line below to get graphs for all 50 states
  #filter(state %in% c("United States", "Tennessee")) %>%
  #filter(state %in% c("Tennessee")) %>%
  filter(state %in% c("United States")) %>%
  
  mutate(thisyear = (year == 2020)) %>%
  group_by(state, year) %>%

  ### Filter out the most recent two weeks worth of data as the provisional
  ### data it is based on as:
  ###
  ### "Last available weeks (not just lastone) are incomplete, and therefore,
  ###  the user need to be extremely cautious about it. The share of
  ###  incompleteness changes slightly every week. "
  ###
  ### https://www.mortality.org/Public/STMF_DOC/STMFmetadata.pdf
  filter(!(year == 2020 & week %in% seq(max(week) - 1, max(week)))) %>%
  ungroup() %>%
  mutate(yearweek = paste(year, " W", week, sep = "")) %>%
  mutate(yearweek = yearweek(yearweek)) %>%
  as_tsibble(index = "yearweek") %>%
  tsibble::fill_gaps() %>%
  na.locf() #%>%
  #as_tibble()
    

model <-
  deaths %>%
  select(yearweek, deaths) %>%
  model(STL(deaths ~ trend(window = 365) + season(period = "year"))) %>%
  components()

model_trend <- (max(model$trend) - model$trend) %>% as_tibble() %>% rename(model_trend = value)

deaths <- 
  deaths %>% 
  bind_cols(model_trend) %>% 
  as_tibble() %>%
  mutate(adj_deaths = deaths + model_trend) %>%
  select(-deaths) %>%
  rename(deaths = adj_deaths) %>%
  mutate(nweek = week(yearweek),
         nyear = year(yearweek)) %>%
  filter(yearweek != yearweek("2014 W01")) %>%
  select(-year, - week) %>%
  rename(week = nweek,
         year = nyear) %>%
  filter(!(year == 2020 & week == 52))
 
### Create a graph of weekly deaths by state
g_weekly_deaths <-
  ggplot(data = deaths, aes(x = week, y = deaths, group = year)) +
  theme_bw() +
  geom_line(aes(col = thisyear)) +
  facet_wrap(~ state, scales = "free_y") +
  scale_color_manual(values = c("FALSE" = "gray", "TRUE" = "red")) +
  guides(col = FALSE) +
  ggtitle("Weekly deaths") +
  labs(x = "Week", y = "") +
  geom_hline(yintercept = 0, col = "gray") +
  scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 52)) + 
  scale_y_continuous(labels = scales::comma, 
                     limits = c(50000, 80000))
print(g_weekly_deaths)


### How current is the data?   Shows the lag in weeks
deaths %>%
  filter(year == 2020) %>%
  group_by(state) %>%
  summarise(last_week = max(week)) %>%
  ungroup() %>%
  mutate(
    current_week = lubridate::week(Sys.Date()),
    lag = current_week - last_week
  ) %>%
  data.frame()

### Estimate excess deaths
recent_deaths <- deaths %>%
  filter(year >= 2015 & year <= 2019) %>%
  group_by(state, week) %>%
  summarise(median_deaths = median(deaths)) %>%
  ungroup()
excess_deaths <- deaths %>%
  filter(year >= 2015) %>%
  left_join(recent_deaths) %>%
  mutate(excess = deaths - median_deaths) %>%
  mutate(thisyear = (year == 2020))

### Create a graph of weekly excess deaths by state
g_excess_deaths <-
  ggplot(data = excess_deaths, aes(x = week, y = excess, group = year)) +
  theme_bw() +
  geom_hline(yintercept = 0, col = "gray") +
  geom_line(aes(col = thisyear)) +
  facet_wrap(~ state, scales = "free_y") +
  scale_color_manual(values = c("FALSE" = "gray", "TRUE" = "red")) +
  guides(col = FALSE) +
  ggtitle("Excess deaths") +
  labs(x = "Week", y = "") +
  scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 52)) + 
  scale_y_continuous(labels = scales::comma)
print(g_excess_deaths)

### Summarize excess deaths
excess_deaths %>%
  filter(year == 2020) %>%
  filter(excess >= 0) %>%
  group_by(state) %>%
  summarise(
    excess = sum(excess),
    last_week = max(week),
    as_at = as.Date("2020-01-01") + 7 * (last_week - 1)
  ) %>%
  ungroup() %>%
  select(state, excess, as_at) %>%
  data.frame()
plot_grid(g_weekly_deaths,
          g_excess_deaths,
          nrow = 1, ncol = 2, align = "hv")