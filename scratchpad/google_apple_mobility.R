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
my_county <- c("Cheatham County", "Davidson County", "Williamson County")

### Pull data for the locations listed
apple_county <-
  apple_mobility %>% 
  filter(geo_type == "county") %>%
  rename(county = county_and_city) %>%
  filter(state %in% my_state) %>%
  filter(county %in% my_county) %>%
  select(-geo_type, -transit, -walking) %>%
  rename(values = driving, dates = date) %>%
  mutate(location = paste("values:", county, ", ", state, sep = "")) %>%
  select(dates, location, values) %>%
  pivot_wider(id_cols = "dates", names_from = "location", values_from = "values") %>%
  janitor::clean_names() %>%
  arrange(dates)


### Use mstl() to pluck out a trend line
apple_model <-
  apple_county %>%
  as_tsibble() %>% 
  tsibble::fill_gaps() %>% 
  na.locf() %>% 
  mutate(across(starts_with("values_"),
                .fns = list(trend = ~ (.) %>% ts() %>% mstl() %>% trendcycle()),
                .names = "{fn}_{col}")) %>%
  rename_at(vars(starts_with("trend_values_")),
          ~ str_replace(., "trend_values_", "trend:")) %>%
  rename_at(vars(starts_with("values_")),
            ~ str_replace(., "values_", "values:")) %>%
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

google_tn <-
  google_mobility %>% 
  janitor::clean_names() %>%
  filter(state == "Tennessee") %>%
  filter(county == "Cheatham County") %>%
  select(-state, -county) %>%
  group_by(date) %>%
  summarize(retail_and_recreation = sum(retail_and_recreation, na.rm = TRUE), 
            grocery_and_pharmacy  = sum(grocery_and_pharmacy, na.rm = TRUE),
            parks                 = sum(parks, na.rm = TRUE),
            transit_stations      = sum(transit_stations, na.rm = TRUE),
            workplaces            = sum(workplaces, na.rm = TRUE),
            residential           = sum(residential, na.rm = TRUE)) %>%
  ungroup() %>%
  as_tsibble(index = date) %>%
  tsibble::fill_gaps() %>% 
  na.locf() %>% 
  mutate(trend_retail_and_recreation = retail_and_recreation %>% SMA(n = 7)) %>%
  mutate(trend_grocery_and_pharmacy  = grocery_and_pharmacy  %>% SMA(n = 7)) %>%
  mutate(trend_parks                 = parks                 %>% SMA(n = 7)) %>%
  mutate(trend_transit_stations      = transit_stations      %>% SMA(n = 7)) %>%
  mutate(trend_workplaces            = workplaces            %>% SMA(n = 7)) %>%
  mutate(trend_residential           = residential           %>% SMA(n = 7)) %>%
  
  select(date, starts_with("trend_")) %>%
  rename_at(vars(starts_with("trend_")),
            ~ str_replace(., "trend_", "")) %>%
  pivot_longer(-date) %>%
  mutate(name = gsub("", "", name))

g_google <-
  ggplot(data = google_tn, aes(x = as.Date(date))) +
  theme_linedraw() +
  geom_line(aes(y = value), color = "darkseagreen4", size = 1.0) +
  labs(title = "% Change from baseline") +
  facet_wrap(~ name, scales = "free_y")

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
