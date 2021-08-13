# Largely based on technique from:  https://robjhyndman.com/hyndsight/excess-deaths/

library(dplyr)
library(tidyr)
library(ggplot2)
library(janitor)
library(cowplot)
library(lubridate)
library(tidycensus)

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
  mutate(yearweek = yearweek(date_decimal(year + (week - 1)/52))) %>%
  
  ### Comment out the line below to get graphs for all 50 states
  filter(state %in% c("United States", "Tennessee")) %>%
  #filter(state %in% c("United States", "Tennessee", "Kentucky", "North Carolina", "Georgia", "Alabama", "Mississipi", "Arkansas")) %>%
  #filter(state %in% c("Tennessee")) %>%
  #filter(state %in% c("United States")) %>%
  
  mutate(thisyear = (year >= 2020)) %>%
  group_by(state, year) %>%

  ### Filter out the first week in 2014, as it appears to be a huge outlier
  filter(!(year == 2014 & week == 1)) %>%

  ### Also filter out the most recent two weeks worth of data as the provisional
  ### data it is based on as:
  ###
  ### "Last available weeks (not just lastone) are incomplete, and therefore,
  ###  the user need to be extremely cautious about it. The share of
  ###  incompleteness changes slightly every week. "
  ###
  ### https://www.mortality.org/Public/STMF_DOC/STMFmetadata.pdf
  filter(!(year >= 2020 & week %in% seq(max(week) - 2, max(week)))) #%>%
  
  ### West Virginia and North Carolina are especially slow about reporting mortality
  ### numbers, so knock off an additional two weeks for those states

  ### Add this when you have time

deaths_mod_us <-
  deaths %>%
  filter(state == "United States") %>%
  select(yearweek, deaths) %>%
  as_tsibble(index = "yearweek") %>%
  model(STL(deaths ~ season(period = "1 year"))) %>%
  components()


ggplot(data = deaths, aes(x = yearweek)) + 
  theme_bw() + 
  geom_line(aes(y = deaths)) +
  facet_wrap(~state, scales = "free_y")



### Recombine NY & NYC
deaths_non_ny <-
  deaths %>%
  filter(!grepl("New York", state))

deaths_ny <-
  deaths %>%
  filter(grepl("New York", state)) %>%
  group_by(year, week, thisyear) %>%
  summarize(deaths = sum(deaths)) %>%
  mutate(state = "New York") %>%
  select(state, year, week, deaths, thisyear)

deaths <-
  deaths_non_ny %>%
  bind_rows(deaths_ny) %>%
  arrange(state)

### Create a graph of weekly deaths by state
g_weekly_deaths <-
  ggplot(data = deaths, aes(x = week, y = deaths, group = year)) +
  theme_bw() +
  geom_line(aes(col = thisyear)) +
  facet_wrap(~ state, scales = "free_y") +
  scale_color_manual(values = c("FALSE" = "gray", "TRUE" = "red")) +
  guides(col = "none") +
  ggtitle("Weekly deaths") +
  labs(x = "Week", y = "") +
  geom_hline(yintercept = 0, col = "gray") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 80000))
print(g_weekly_deaths)


### How current is the data?   Shows the lag in weeks
deaths %>%
  filter(year >= 2020) %>%
  group_by(state) %>%
  summarise(last_week = max(week)) %>%
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
  mutate(thisyear = (year >= 2020))

### Get county population
pop <-
  get_acs(geography   = "state",
          variables   = c("B01003_001"),
          year        = 2019,
          geometry    = FALSE,
          cache_table = TRUE) %>%
  rename(POP2018 = estimate,
         state   = NAME) %>%
  select(state, POP2018) %>%
  add_row(state   = "United States",
          POP2018 = 326289971)

# United States - 326,289,971
# New York City - 8,399,000
# New York State - 11,051,000

excess_deaths <-
  excess_deaths %>% 
  left_join(pop, by = "state") %>% 
  mutate(excess = excess / POP2018)

excess_deaths_2020 <-
  excess_deaths %>%
  filter(year >= 2020) #%>%
#  filter(week %in% c(37, 36))
  #filter(week > 25)

### Order graphs by the state seeing the worst impact
us_order <- 
  excess_deaths_2020 %>% 
  ungroup() %>% 
  select(state, excess) %>% 
  group_by(state) %>%
  summarize(max = max(excess)) %>%
  arrange(desc(max)) %>% 
  pull(state)

excess_deaths$state <- factor(excess_deaths$state, levels = us_order)

### Create a graph of weekly excess deaths by state
g_excess_deaths <-
  ggplot(data = excess_deaths, aes(x = week, y = excess, group = year)) +
  theme_bw() +
  geom_hline(yintercept = 0, col = "gray") +
  geom_line(aes(col = thisyear)) +
  geom_vline(xintercept = week(Sys.Date()), linetype = "dotted", color = "darkred") + 
  facet_wrap(~ state) + #, scales = "free_y") +
  scale_color_manual(values = c("FALSE" = "gray", "TRUE" = "red")) +
  guides(col = FALSE) +
  ggtitle("Excess deaths") +
  labs(x = "Week", y = "") +
  scale_y_continuous(labels = scales::percent)
print(g_excess_deaths)

g_excess_deaths <-
  ggplot(data = excess_deaths %>% filter(year >= 2020), aes(x = week, y = excess)) +
  theme_bw() +
  geom_hline(yintercept = 0, col = "gray") +
  geom_area(fill = "orange", color = "black")  +
  geom_vline(xintercept = week(Sys.Date()), linetype = "dotted", color = "darkred") + 
  facet_wrap(~ state) + #, scales = "free_y") +
  scale_color_manual(values = c("FALSE" = "gray", "TRUE" = "red")) +
  guides(col = FALSE) +
  ggtitle("Excess deaths by week as % of population") +
  labs(subtitle = "Vertical dotted line represents current week.  Mortality data lags ~1 month") +
  labs(x = "Week", y = "") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.0001))
print(g_excess_deaths)

### Summarize excess deaths
excess_deaths %>%
  filter(year >= 2020) %>%
  group_by(state) %>%
  summarise(
    excess = sum(excess),
    last_week = max(week),
    as_at = as.Date("2020-01-01") + 7 * (last_week - 1)
  ) %>%
  select(state, excess, as_at) %>%
  data.frame()

plot_grid(g_weekly_deaths,
          g_excess_deaths,
          nrow = 1, ncol = 2, align = "hv")

