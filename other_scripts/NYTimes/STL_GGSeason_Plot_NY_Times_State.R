### Pull new confirmed cases and analyze data to find days or times of month
### with relatively lower/higher risk of infection.

library(tidyverse)
library(tsibble)
library(feasts)
library(cowplot)

### What state are we interested in?
my_state <- "Tennessee"

### How many days between being infected and being confirmed.   According
### to Harvard, it's 4-5 days, according to WHO it's 5-6 days, so I choose 5.
### We have "date of confirmed infection", so if we subtract 5 days, it should
### give us a good estimate about the date the infection occurs.
lag <- 0

### Pull cases data from the NY Times repo below and change the path
### as needed
###
### https://github.com/nytimes/covid-19-data
###
spreadsheet <- "../Datasets/nytimes/covid-19-data/us-states.csv"


### Take the data in the spreadsheet, which is total new cases, and convert
### it to new cases per day
data <-
  read_csv(spreadsheet, col_names = TRUE, col_types = "Dccdd") %>%
  filter(state == my_state) %>%
  arrange(date) %>%
  mutate(new_cases  = cases - lag(cases)) %>%
  select(-state, -fips, -deaths, -cases) %>%
  filter(!is.na(new_cases)) %>%
  as_tsibble(index = "date") %>%
  filter(date >= as.Date("2020-05-01"))

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
g_stl <- model_cases %>% autoplot() + theme_bw()
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
  theme_bw() + 
  xlab("Day of Week") +
  ylab("New cases") +
  ggtitle("Weekly Seasonal Component of New Infections")
print(g_week)

### Let's plot the monthly pattern
g_month <-
  new_cases_tib %>%
  mutate(date = as.Date(date) - lag) %>%
  gg_season(s_month, period = "1 month") +
  theme_bw() + 
  xlab("Day of Month") +
  ylab("New cases") +
  ggtitle("Monthly Seasonal Component of New Infections")
print(g_month)

### Header/footer strings
title_string <- paste("COVID-19 Infection Risk assuming ", lag,
                      " day lag between infection and symptoms\n", my_state,
                      " [", as.Date(data %>% tail(n = 1) %>% pull(date)), "]", sep = "")
footer_string <- "Data Source:  https://github.com/nytimes/covid-19-data"

title  <- ggdraw() + draw_label(title_string, fontface = "bold")
footer <- ggdraw() + draw_label(footer_string, size = 10)

### Let's smoosh it all together in one graph
p_period <- plot_grid(g_week, g_month, nrow = 2, ncol = 1, align = "hv", axis = "lbrt")
p_final <- 
  plot_grid(title,
            plot_grid(g_stl, p_period, nrow = 1, ncol = 2, rel_widths = c(1, 0.4)),
            #          p_period,
            footer,
            axis = "lbrt", nrow = 3, ncol = 1, rel_heights = c(0.1, 1, 0.05),
            align = "hv")
print(p_final)

