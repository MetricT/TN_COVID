library(tsibble)
library(feasts)
library(zoo)
library(forecast)

################################################################################
### COVID-19 related mobile data
################################################################################
###
### Apple data available for direct download at:
###
### https://www.apple.com/covid19/mobility  
###
### Google data available for direct download at:
### 
### https://www.google.com/covid19/mobility/
###
### I download the data from a Github aggregator (makes updating easier).  
### You can download both datasets at:
### https://github.com/ActiveConclusion/COVID19_mobility

################################################################################
### Load the data...
################################################################################
apple_mobility <-
  "../Datasets/ActiveConclusion/COVID19_mobility/apple_reports/apple_mobility_report_US.csv" %>%
  read_csv()

google_mobility <-
  "../Datasets/ActiveConclusion/COVID19_mobility/google_reports/mobility_report_US.csv" %>%
  read_csv()

################################################################################
### The data is subdivided by state and county, so you can select a particular
### state or county.
################################################################################
my_state  <- c("Tennessee")
my_county <- c("Cheatham County", "Davidson County", "Williamson County", "Dickson County", "Sevier County")

### Doing it this way so you can specify counties in multiple states and compare
my_locations <- paste(my_county, ", ", my_state, sep = "")

################################################################################
### Take Apple data and draw a facet graph of locations showing change in activity
################################################################################

### Pull out the data and use mstl() to pluck out a trend line
apple_model <-
  apple_mobility %>% 
  filter(geo_type == "county") %>%
  rename(county = county_and_city) %>%
  mutate(location = paste(county, ", ", state, sep = "")) %>%
  filter(location %in% my_locations) %>%
  select(date, location, driving) %>%
  rename(values = driving, dates = date) %>%
  mutate(location = paste("values:", location, sep = "")) %>%
  select(dates, location, values) %>%
  pivot_wider(id_cols = "dates", names_from = "location", values_from = "values") %>%
  arrange(dates) %>%
  as_tsibble() %>% 
  tsibble::fill_gaps() %>% 
  na.locf() %>% 
  mutate(across(starts_with("values:"),
                .fns = list(trend = ~ (.) %>% ts() %>% mstl() %>% trendcycle()),
                .names = "{fn}_{col}")) %>%
  rename_at(vars(starts_with("trend_values")),
          ~ str_replace(., "trend_values", "trend")) %>%
  as_tibble() %>%
  pivot_longer(-dates, names_to = c("type", "location"), names_sep = ":", values_to = "values") %>%
  pivot_wider(id_cols = c("dates", "location"), names_from = "type", values_from = "values")


### Create a facet map showing the data along with the trend for each location
g_apple <-
  ggplot(data = apple_model, aes(x = as.Date(dates))) +
  theme_linedraw() +

  geom_point(aes(y = values), alpha = 0.5) +
   geom_line(aes(y = trend), size = 1, color = "firebrick3") +
  
  facet_wrap(~ location) +
  
  labs(
       x = "Date", 
       y = "Change in requests since January 13, 2020")
print(g_apple)

################################################################################
### Take Google data and draw a facet graph of locations showing change in activity
################################################################################
google_tn <-
  google_mobility %>% 
  mutate(location = paste(county, ", ", state, sep = "")) %>%
  filter(location %in% my_locations) %>%
  select(-state, -county) %>%

  # Pivot longer so we can turn it into a proper tsibble and interpolate missing data
  pivot_longer(-c("date", "location")) %>%
  pivot_wider(id_cols = "date", names_from = c("location", "name"), names_sep = ":", values_from = "value") %>%
  as_tsibble(index = date) %>%
  tsibble::fill_gaps() %>% 
  as_tibble() %>%
  pivot_longer(-date, names_to = c("location", "name"), names_sep = ":", values_to = "value") %>%
  
  
  # Now do a SMA of the data...
  mutate(location = paste("values:", location, sep = "")) %>%
  pivot_wider(id_cols = "date", names_from = c("location", "name"), names_sep = ":", values_from = "value") %>%
  mutate_if(is.numeric, ~ (replace_na(., 0))) %>%
  mutate(across(starts_with("values:"),
                .fns = list(trend = ~ (.) %>% ts() %>% mstl() %>% trendcycle()),
                .names = "{fn}_{col}")) %>%
  rename_at(vars(starts_with("trend_values")),
            ~ str_replace(., "trend_values", "trend")) %>%
  pivot_longer(-date, names_to = c("type", "location", "name"), names_sep = ":", values_to = "value") %>%
  pivot_wider(id_cols = c("date", "location", "name"), names_from = "type", values_from = "value")
  

g_google <-
  ggplot(data = google_tn %>% subset(name == "workplaces"), aes(x = as.Date(date))) +
  theme_linedraw() +
  geom_line(aes(y = trend), color = "darkseagreen4", size = 1.0) +
  labs(title = "% Change from baseline") +
  facet_wrap(~ location + name, scales = "free_y")

print(g_google)

tn_unemploy <-
  tribble(
    ~date,        ~name,                 ~value,     ~g_workplace_avg,
    "2020-02-01",	"0 - TN unemployment", -3.4,       -11.0,
    "2020-03-01",	"0 - TN unemployment", -3.3,       -39.7,
    "2020-04-01",	"0 - TN unemployment", -15.5,      -32.6,
    "2020-05-01",	"0 - TN unemployment", -11,        -23.5,
    "2020-06-01",	"0 - TN unemployment", -9.7,       -27.7) %>%
  mutate(date = as.Date(date))

g_tn <-
  google_tn %>% 
  as_tibble() %>%
  filter(name == "workplaces") %>% 
  mutate(date = as.Date(date) - 8) %>%
  mutate(name = "Google change in visits to workplace in TN,\n% change from baseline") %>%
  bind_rows(tn_unemploy)

g_google_unemploy <-
  ggplot(data = g_tn, aes(x = as.Date(date))) +
  theme_linedraw() +
  theme(strip.text = element_text(size = 20)) + 
  geom_line(aes(y = value), color = "darkseagreen4", size = 1.0) +
  geom_vline(xintercept = as.Date("2020-02-01"), linetype = "dotted") +
  geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dotted") +
  geom_vline(xintercept = as.Date("2020-04-01"), linetype = "dotted") +
  geom_vline(xintercept = as.Date("2020-05-01"), linetype = "dotted") +
  geom_vline(xintercept = as.Date("2020-06-01"), linetype = "dotted") +
  geom_vline(xintercept = as.Date("2020-07-01"), linetype = "dotted") +
  facet_wrap(~ name, scales = "free_y")
print(g_google_unemploy)




google_us <-
  google_mobility %>% 
  filter(state == "Total") %>%
  filter(county == "Total") %>%
  select(date, workplaces) %>%
  group_by(date) %>%
  summarize(workplaces_values = sum(workplaces, na.rm = TRUE)) %>%
  ungroup() %>%
  as_tsibble(index = date) %>%
  tsibble::fill_gaps() %>% 
  na.locf() %>% 
  mutate(workplaces = workplaces_values %>% ts() %>% mstl() %>% trendcycle()) %>% 
  select(-workplaces_values) %>%
  pivot_longer(-date)

unemploy_us <-
  fredr(series_id = "TNURN",
        frequency = "m",
        observation_start = as.Date("2020-02-01")) %>%
  rename(name = series_id) %>%
  mutate(name = "0 - US Unemployment [UNRATE]") %>%
  mutate(value = - value)

g_us <-
  google_us %>% 
  as_tibble() %>%
  mutate(date = as.Date(date) - 8) %>%
  mutate(name = "Google change in visits to workplace in US,\n% change from baseline") %>%
  bind_rows(unemploy_us)

g_google_unemploy_us <-
  ggplot(data = g_us, aes(x = as.Date(date))) +
  theme_linedraw() +
  theme(strip.text = element_text(size = 20)) + 
  geom_line(aes(y = value), color = "darkseagreen4", size = 1.0) +
  geom_vline(xintercept = as.Date("2020-02-01"), linetype = "dotted") +
  geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dotted") +
  geom_vline(xintercept = as.Date("2020-04-01"), linetype = "dotted") +
  geom_vline(xintercept = as.Date("2020-05-01"), linetype = "dotted") +
  geom_vline(xintercept = as.Date("2020-06-01"), linetype = "dotted") +
  geom_vline(xintercept = as.Date("2020-07-01"), linetype = "dotted") +
  facet_wrap(~ name, scales = "free_y") +
  labs(x = "Date")
print(g_google_unemploy_us)
