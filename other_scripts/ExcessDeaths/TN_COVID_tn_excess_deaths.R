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
  filter(state %in% c("Tennessee")) %>%
  mutate(thisyear = (year == 2020)) %>%
  group_by(state, year) %>%
  filter(!(year == 2014 & week == 1)) %>%
  mutate(yearweek = paste(year, " W", week, sep = ""))
         
         

  ### filter out the most recent week worth of data as the provisional
  ### data it is based on as:
  ###
  ### "Last available weeks (not just lastone) are incomplete, and therefore,
  ###  the user need to be extremely cautious about it. The share of
  ###  incompleteness changes slightly every week. "
  ###
  ### https://www.mortality.org/Public/STMF_DOC/STMFmetadata.pdf
  filter(!(year == 2020 & week %in% seq(max(week) - 1, max(week))))

### Estimate excess deaths
recent_deaths <- deaths %>%
  filter(year >= 2015 & year <= 2019) %>%
  group_by(state, week) %>%
  summarise(median_deaths = median(deaths),
            sd_deaths     = sd(deaths)) %>%
  ungroup()

excess_deaths <- deaths %>%
  filter(year >= 2015) %>%
  left_join(recent_deaths) %>%
  mutate(excess = deaths - (median_deaths + 0.5 * sd_deaths)) %>%
  mutate(thisyear = (year == 2020))

### Summarize excess deaths
excess_txt <-
  excess_deaths %>%
  filter(year == 2020) %>%
  group_by(state) %>%
  summarise(
    excess = sum(excess),
    last_week = max(week),
    as_at = as.Date("2020-01-01") + 7 * (last_week - 1)
  ) %>%
  select(state, excess, as_at) %>%
  data.frame()

confirmed_deaths <- 
  deaths_data %>%
  mutate(week = week(date)) %>%
  select(week, new_deaths) %>%
  group_by(week) %>%
  summarise(new_deaths = sum(new_deaths))

official_deaths <-
  tribble(
    ~week_of_year,  ~official_covid_deaths,
    14,                           48,
    15,                           52,
    16,                           33,
    17,                           31,
    18,                           38,
    19,                           38,
    20,                           41,
    21,                           38,
    22,                           38,
    23,                           54,
    24,                           58,
    25,                           49,
    26,                           62,
    27,                           61,
    28,                          102,
    29,                          104,
    30,                           96,
  )

official_deaths <- official_deaths %>% mutate(year = 2020)

excess_deaths <-
  excess_deaths %>%
  left_join(official_deaths, by = c("year" = "year", "week" = "week_of_year"))


### Create a graph of weekly excess deaths by state
g_excess_deaths_tn <-
  ggplot(data = excess_deaths, aes(group = year)) +
  theme_bw() +
  geom_hline(yintercept = 0, col = "gray") +

  geom_line(aes(x = week, y = excess, col = thisyear)) +
  
  geom_line(aes(x = week, y = official_covid_deaths), color = "steelblue2", size = 1.0) +
  
  scale_color_manual(values = c("FALSE" = "gray", "TRUE" = "red")) +
  guides(col = FALSE) +
  ggtitle(paste("Excess deaths in Tennesseee in 2020: ", 
                excess_txt$excess %>% format(big.mark = ",", scientific = FALSE), 
                " as of ", as.Date(excess_txt$as_at), sep = "")) +
  labs(x = "Week", y = "") +
  scale_y_continuous(labels = scales::comma)
print(g_excess_deaths_tn)
