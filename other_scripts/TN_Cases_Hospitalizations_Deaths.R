library(tidyverse)
library(forecast)
library(naniar)
library(useful)
library(zoo)

### What state are we interested in?
my_state <- "Tennessee"


### Function to read Excel spreadsheets from URL's since the "readxl" package is
### behind the times
read_excel_url <- function(url, ...) {
  tf <- tempfile(fileext = ".xlsx")
  curl::curl_download(url, tf)
  return(readxl::read_excel(tf, ...))
}

### Read in data from the TN Dept. of Health's website
url_root <- "https://www.tn.gov/content/dam/tn/health/documents/cedep/novel-coronavirus/datasets/"
county_new_url <- paste(url_root, "Public-Dataset-County-New.XLSX", sep = "")
county_new_data <-
  read_excel_url(county_new_url, col_types = c("date", "text", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric")) 

county_new_df <- 
  county_new_data %>%
  select(DATE, COUNTY, NEW_CASES, NEW_TESTS, NEW_DEATHS, NEW_RECOVERED, NEW_ACTIVE, NEW_HOSPITALIZED) %>%
  mutate(Date = as.Date(DATE)) %>%
  select(-DATE)

new_cases <-
  county_new_df %>%
  select(Date, COUNTY, NEW_CASES) %>%
  pivot_wider(id_cols = "Date", names_from = "COUNTY", values_from = "NEW_CASES") %>%
  mutate_if(is.numeric, ~ (replace_na(., 0))) %>%
  mutate(Total = rowSums(select(., !starts_with("Date")))) %>%
  select(Date, Total, sort(current_vars()))

new_tests <-
  county_new_df %>%
  select(Date, COUNTY, NEW_TESTS) %>%
  pivot_wider(id_cols = "Date", names_from = "COUNTY", values_from = "NEW_TESTS") %>%
  mutate_if(is.numeric, ~ (replace_na(., 0))) %>%
  mutate(Total = rowSums(select(., !starts_with("Date")))) %>%
  select(Date, Total, sort(current_vars()))

new_deaths <-
  county_new_df %>%
  select(Date, COUNTY, NEW_DEATHS) %>%
  pivot_wider(id_cols = "Date", names_from = "COUNTY", values_from = "NEW_DEATHS") %>%
  mutate_if(is.numeric, ~ (replace_na(., 0))) %>%
  mutate(Total = rowSums(select(., !starts_with("Date")))) %>%
  select(Date, Total, sort(current_vars()))

new_recovered <-
  county_new_df %>%
  select(Date, COUNTY, NEW_RECOVERED) %>%
  pivot_wider(id_cols = "Date", names_from = "COUNTY", values_from = "NEW_RECOVERED") %>%
  mutate_if(is.numeric, ~ (replace_na(., 0))) %>%
  mutate(Total = rowSums(select(., !starts_with("Date")))) %>%
  select(Date, Total, sort(current_vars()))

new_active <-
  county_new_df %>%
  select(Date, COUNTY, NEW_ACTIVE) %>%
  pivot_wider(id_cols = "Date", names_from = "COUNTY", values_from = "NEW_ACTIVE") %>%
  mutate_if(is.numeric, ~ (replace_na(., 0))) %>%
  mutate(Total = rowSums(select(., !starts_with("Date")))) %>%
  select(Date, Total, sort(current_vars()))

new_hospitalized <-
  county_new_df %>%
  select(Date, COUNTY, NEW_HOSPITALIZED) %>%
  pivot_wider(id_cols = "Date", names_from = "COUNTY", values_from = "NEW_HOSPITALIZED") %>%
  mutate_if(is.numeric, ~ (replace_na(., 0))) %>%
  mutate(Total = rowSums(select(., !starts_with("Date")))) %>%
  select(Date, Total, sort(current_vars()))

data <-
           (new_cases         %>% select(Date, Total) %>% rename(new_cases = Total)) %>%
  left_join(new_tests         %>% select(Date, Total) %>% rename(new_tests = Total), by = "Date") %>%
  left_join(new_deaths        %>% select(Date, Total) %>% rename(new_deaths = Total), by = "Date") %>%
  left_join(new_active        %>% select(Date, Total) %>% rename(new_active = Total), by = "Date") %>%
  left_join(new_recovered     %>% select(Date, Total) %>% rename(new_recovered = Total), by = "Date") %>%
  left_join(new_hospitalized  %>% select(Date, Total) %>% rename(new_hospitalized = Total), by = "Date") %>%

  mutate(new_cases_trend        = new_cases        %>% ts(frequency = 7) %>% mstl() %>% trendcycle()) %>%
  mutate(new_tests_trend        = new_tests        %>% ts(frequency = 7) %>% mstl() %>% trendcycle()) %>%
  mutate(new_deaths_trend       = new_deaths       %>% ts(frequency = 7) %>% mstl() %>% trendcycle()) %>%
  mutate(new_active_trend       = new_active       %>% ts(frequency = 7) %>% mstl() %>% trendcycle()) %>%
  mutate(new_recovered_trend    = new_recovered    %>% ts(frequency = 7) %>% mstl() %>% trendcycle()) %>%
  mutate(new_hospitalized_trend = new_hospitalized %>% ts(frequency = 7) %>% mstl() %>% trendcycle()) %>%
  
  
  as_tsibble(index = "Date") %>%
  rename(date = Date) %>%
  filter(date >= as.Date("2020-03-10"))

### How much do we scale up deaths to make it visually similar in size to cases
scale <- 80

### What is the lag in days between cases and deaths
lag_st <- 0

current_date <- data %>% tail(n = 1) %>% pull("date")

g_cases_and_deaths <-
  ggplot(data = data) +
  theme_bw() +
  theme(legend.position = "none") +
  
  geom_point(aes(x = as.Date(date), y = new_cases, color = "Cases"), size = 1, shape = 19, alpha = 0.4) +
  geom_line(aes(x = as.Date(date), y = new_cases_trend, color = "Cases"), size = 1.3) +

  #geom_point(aes(x = as.Date(date), y = new_deaths * scale, color = "Deaths"), size = 1, shape = 19, alpha = 0.4) +
  #geom_line(aes(x = as.Date(date), y = new_deaths_trend * scale, color = "Deaths"), size = 1.3) +
  
  #geom_point(aes(x = as.Date(date), y = new_hospitalized * 15.5, color = "Hospitalized"), size = 1, shape = 19, alpha = 0.4) +
  #geom_line(aes(x = as.Date(date), y = new_hospitalized_trend * 15.5, color = "Hospitalized"), size = 1.3) +
  
  #geom_point(aes(x = as.Date(date), y = new_tests * 0.05, color = "Tests"), size = 1, shape = 19, alpha = 0.4) +
  #geom_line(aes(x = as.Date(date), y = new_tests_trend * 0.05, color = "Tests"), size = 1.3) +
  
  geom_point(aes(x = as.Date(date), y = new_active * 1, color = "Tests"), size = 1, shape = 19, alpha = 0.4) +
  geom_line(aes(x = as.Date(date), y = new_active_trend * 1, color = "Tests"), size = 1.3) +  

  
  scale_color_manual(values =  c("Cases" = "steelblue2",
                                 "Deaths" = "firebrick2",
                                 "Hospitalized" = "goldenrod2",
                                 "Tests" =  "green",
                                 "Recovered" = "orange",
                                 "Active"    = "pink")) +
  
  geom_vline(xintercept = as.Date(current_date) - lag_st, linetype = "dotted") + 
  
  labs(title = paste(my_state, " New Cases and New Deaths data from ", current_date, " lagged by ", lag_st, " days", sep = ""), x = "Date", y = "New Cases (blue)") + 
  scale_y_continuous(labels = scales::comma,
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

