### Pull new confirmed cases and analyze data to find days or times of month
### with relatively lower/higher risk of infection.

library(tidyverse)
library(tsibble)
library(feasts)
library(cowplot)

### What state are we interested in?
my_state <- "Tennessee"

### What county are we interested in?
my_county <- "Davidson"

### How many days between being infected and being confirmed.   According
### to Harvard, it's 4-5 days, according to WHO it's 5-6 days, so I choose 5.
### We have "date of confirmed infection", so if we subtract 5 days, it should
### give us a good estimate about the date the infection occurs.
lag <- 5

### Pull cases data from the NY Times repo below and change the path
### as needed
###
### https://github.com/nytimes/covid-19-data
###
spreadsheet <- "../Datasets/nytimes/covid-19-data/us-counties.csv"


### Take the data in the spreadsheet, which is total new cases, and convert
### it to new cases per day
data <-
  read_csv(spreadsheet, col_names = TRUE, col_types = "Dccddd") %>%
  filter(state == my_state) %>%
  filter(county == my_county) %>%
  arrange(date) %>%
  mutate(new_cases  = cases - lag(cases)) %>%
  select(-state, -county, -fips, -deaths, -cases) %>%
  filter(!is.na(new_cases)) %>%
  as_tsibble(index = "date")

### Decompose new cases using the STL() function from the "feasts"
### package, using a 7 day window for the trend, and seasonal periods of
### one week and one month.
model_cases <-
  data %>%
  model(
    STL(new_cases ~ trend(window = 7) +
                    season(period = "week") +
                    season(period = "month"), iterations = 100)
  ) %>% components()

### Graph the model.   Note that monthly
g_stl <- model_cases %>% autoplot()
print(g_stl)

### Create a new tibble using the decomposed components
new_cases_tib <-
  tibble(date      = data %>% pull("date"),
         values    = data %>% pull("new_cases"),
         trend     = model_cases %>% pull("trend"),
         s_week    = model_cases %>% pull("season_week"),
         s_month   = model_cases %>% pull("season_month"),
         s_adjust  = model_cases %>% pull("season_adjust"),
         remainder = model_cases %>% pull("remainder"),
  ) %>% as_tsibble(index = "date")

### Let's plot the weekly pattern
g_week <-
  new_cases_tib %>%
  mutate(date = as.Date(date) - lag) %>%
  gg_season(s_week,  period = "1 week") +
  xlab("Day of Week") +
  ylab("New cases") +
  ggtitle("Weekly Seasonal Component of New Infected")

### Let's plot the monthly pattern
g_month <-
  new_cases_tib %>%
  mutate(date = as.Date(date) - lag) %>%
  gg_season(s_month,  period = "1 month") +
  xlab("Day of Month") +
  ylab("New cases") +
  ggtitle("Monthly Seasonal Component of New Infected")

### Header/footer strings
title_string <- paste("COVID-19 Infection Risk assuming ", lag, " day lag between infection and symptoms\n", my_county, " County, ", my_state, 
                      " [", as.Date(data %>% tail(n = 1) %>% pull(date)), "]", sep = "")
footer_string <- "Data Source:  https://github.com/nytimes/covid-19-data"

title  <- ggdraw() + draw_label(title_string, fontface = "bold")
footer <- ggdraw() + draw_label(footer_string, size = 10)


### Let's smoosh it all together in one graph
p_period <- plot_grid(g_week, g_month, nrow = 1, ncol = 2, align = "hv", axis = "lbrt")
p_final <- 
  plot_grid(title,
          g_stl,
          p_period,
          footer,
          axis = "lbrt", nrow = 4, ncol = 1, rel_heights = c(0.1, 1, 0.5, 0.05),
          align = "hv")
print(p_final)
