library(tidyverse)
library(forecast)
library(naniar)
library(useful)

### What state are we interested in?
my_state <- "Tennessee"

### How much do we scale up deaths to make it visually similar in size to cases
scale <- 110

### What is the lag in days between cases and deaths
lag_st <- 7

# Download this Github repo and add the path to the spreadsheet
# https://github.com/nytimes/covid-19-data
data <-
  read_csv("../Datasets/nytimes/covid-19-data/us-states.csv", col_names = TRUE, col_types = "Dccdd") %>%
  filter(state == my_state) %>%
  select(-state, -fips) %>%
  arrange(date) %>%
  replace_with_na(replace = list(deaths = 0)) %>%
  mutate_at("deaths", ~lead(., lag_st)) %>%
  mutate(new_cases  = cases - lag(cases)) %>%
  mutate(new_deaths = deaths - lag(deaths)) %>%
  mutate(new_cases_trend  = new_cases  %>% ts(frequency = 7) %>% mstl() %>% trendcycle()) %>%
  mutate(new_deaths_trend = new_deaths %>% ts(frequency = 7) %>% mstl() %>% trendcycle()) %>%
  filter(date >= as.Date("2020-06-01"))
data$new_deaths_trend[is.na(data$new_deaths)] <- NA

current_date <- data %>% tail(n = 1) %>% pull("date")

g_cases_and_deaths <-
  ggplot(data = data) +
  theme_bw() +
  theme(legend.position = "none") +
  
  geom_point(aes(x = as.Date(date), y = new_cases, color = "Cases"), size = 1, shape = 19, alpha = 0.4) +
  geom_line(aes(x = as.Date(date), y = new_cases_trend, color = "Cases"), size = 1.3) +

  geom_point(aes(x = as.Date(date), y = new_deaths * scale, color = "Deaths"), size = 1, shape = 19, alpha = 0.4) +
  geom_line(aes(x = as.Date(date), y = new_deaths_trend * scale, color = "Deaths"), size = 1.3) +
  
  scale_color_manual(values =  c("Cases" = "steelblue2",
                                 "Deaths" = "firebrick2")) +
  
  geom_vline(xintercept = as.Date(current_date) - lag_st, linetype = "dotted") + 
  
  labs(title = paste(my_state, " New Cases and New Deaths data from ", current_date, " lagged by ", lag_st, " days", sep = ""), x = "Date", y = "New Cases (blue)") + 
  scale_y_continuous(labels = scales::comma, limits = c(400, 2300),
                     sec.axis = sec_axis(~ . / scale,
                                         name = "New Deaths (red)",
                                         labels = scales::comma))
  
print(g_cases_and_deaths)

### Now let's do CFR...
#data <-
#  data %>%
#  mutate(cfr = new_deaths / new_cases) %>%
#  mutate(cfr_trend = new_deaths_trend / new_cases_trend) #cfr %>% ts(frequency = 7) %>% mstl() %>% trendcycle()) 
#
#data$cfr_trend[is.na(data$cfr)] <- NA
#
#g_cfr <-
#  ggplot(data = data, aes(x = as.Date(date))) +
#  theme_bw() +
#  theme(legend.position = "none") +
#  
#  geom_point(aes(y = cfr, color = "Cases"), size = 1, shape = 19, alpha = 0.4) +
#  geom_line(aes(y = cfr_trend, color = "Cases"), size = 1.3) +
#
#  labs(title = paste(my_state, " Case Fatality Rate as of ", current_date, " lagged by ", lag_st, " days", sep = ""), 
#       x = "Date", 
#       y = "Case Fatality Rate") + 
#  scale_y_continuous(labels = scales::percent, limits = c(0.01, 0.03))
#
#print(g_cfr)

