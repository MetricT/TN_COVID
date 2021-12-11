library(tidyverse)
library(forecast)
library(naniar)
library(useful)

### What state are we interested in?
my_state <- "Tennessee"

### What county are we interested in?
my_county <- "Davidson"

### How much do we scale up deaths to make it visually similar in size to cases
scale <- 80

### What is the lag in days between cases and deaths
lag_st <- 14

# Download this Github repo and add the path to the spreadsheet
# https://github.com/nytimes/covid-19-data
data <-
  read_csv("../Datasets/nytimes/covid-19-data/us-counties.csv", col_names = TRUE, col_types = "Dccddd") %>%
  filter(state == my_state) %>%
  filter(county == my_county) %>%
  select(-state, -county, -fips) %>%
  arrange(date) %>%
  replace_with_na(replace = list(deaths = 0)) %>%
  mutate_at("deaths", ~lead(., lag_st)) %>%
  mutate(new_cases  = cases - lag(cases)) %>%
  mutate(new_deaths = deaths - lag(deaths)) %>%
  mutate(new_cases_trend  = new_cases  %>% ts(frequency = 7) %>% mstl() %>% trendcycle()) %>%
  mutate(new_deaths_trend = new_deaths %>% ts(frequency = 7) %>% mstl() %>% trendcycle()) 
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
  
  labs(title = paste(my_county, ", ", my_state, " New Cases and New Deaths data from ", current_date, " lagged by ", lag_st, " days", sep = ""), x = "Date", y = "New Cases (blue)") + 
  scale_y_continuous(labels = scales::comma,
                     sec.axis = sec_axis(~ . / scale,
                                         name = "New Deaths (red)",
                                         labels = scales::comma))
  
print(g_cases_and_deaths)
