library(tsibble)
library(feasts)
library(zoo)

###
### From: https://www.apple.com/covid19/mobility  
###
apple_mobility <-
  "../Datasets/Mobility/applemobilitytrends-2020-08-01.csv" %>%
  read_csv()

###
### From:  https://www.google.com/covid19/mobility/
###
google_mobility <-
  "../Datasets/Mobility/Global_Mobility_Report.csv" %>%
  read_csv()

apple_county <-
  apple_mobility %>% 
  filter(geo_type == "county") %>%
  select(-geo_type, -alternative_name, -country) %>%
  rename(state = "sub-region", county = region) %>%
  pivot_longer(-c("county", "transportation_type", "state"), names_to = "dates", values_to = "values") %>%
  filter(state  == "Tennessee") %>%
  filter(county == "Cheatham County") %>%
  filter(transportation_type == "driving") %>%
  select(dates, values) %>%
  mutate(dates = as.Date(dates)) %>%
  arrange(dates) 

apple_model <-
  apple_county %>%
  as_tsibble() %>% 
  tsibble::fill_gaps() %>% 
  na.locf() %>% 
  model(STL(values ~ trend())) %>%
  components()

g_apple <-
  ggplot(data = apple_county, aes(x = as.Date(dates))) +
  theme_linedraw() +
  geom_point(aes(y = values), alpha = 0.5) +
  geom_line(data = apple_model, aes(y = trend), size = 1, color = "firebrick3")
print(g_apple)

google_tn <-
  google_mobility %>% 
  mutate(date = as.Date(date)) %>%
  filter(country_region_code == "US") %>%
  select(-country_region_code, -country_region, -sub_region_2, - iso_3166_2_code, -metro_area, -census_fips_code) %>%
  rename(state = sub_region_1) %>%
  filter(state == "Tennessee") %>%
  select(-state) %>% 
  group_by(date) %>%
  summarize(retail_and_recreation_percent_change_from_baseline = sum(retail_and_recreation_percent_change_from_baseline, na.rm = TRUE), 
            grocery_and_pharmacy_percent_change_from_baseline  = sum(grocery_and_pharmacy_percent_change_from_baseline, na.rm = TRUE),
            parks_percent_change_from_baseline                 = sum(parks_percent_change_from_baseline, na.rm = TRUE),
            transit_stations_percent_change_from_baseline      = sum(transit_stations_percent_change_from_baseline, na.rm = TRUE),
            workplaces_percent_change_from_baseline            = sum(workplaces_percent_change_from_baseline, na.rm = TRUE),
            residential_percent_change_from_baseline           = sum(residential_percent_change_from_baseline, na.rm = TRUE)) %>%
  ungroup() %>%
  as_tsibble(index = date) %>%
  tsibble::fill_gaps() %>% 
  na.locf() %>% 
  mutate(trend_retail_and_recreation_percent_change_from_baseline = retail_and_recreation_percent_change_from_baseline %>% SMA(n = 7)) %>%
  mutate(trend_grocery_and_pharmacy_percent_change_from_baseline  = grocery_and_pharmacy_percent_change_from_baseline  %>% ts() %>% mstl() %>% trendcycle()) %>%
  mutate(trend_parks_percent_change_from_baseline                 = parks_percent_change_from_baseline                 %>% ts() %>% mstl() %>% trendcycle()) %>%
  mutate(trend_transit_stations_percent_change_from_baseline      = transit_stations_percent_change_from_baseline      %>% ts() %>% mstl() %>% trendcycle()) %>%
  mutate(trend_workplaces_percent_change_from_baseline            = workplaces_percent_change_from_baseline            %>% ts() %>% mstl() %>% trendcycle()) %>%
  mutate(trend_residential_percent_change_from_baseline           = residential_percent_change_from_baseline           %>% ts() %>% mstl() %>% trendcycle()) %>%
  select(date, starts_with("trend_")) %>%
  rename_at(vars(starts_with("trend_")),
            ~ str_replace(., "trend_", "")) %>%
  pivot_longer(-date) %>%
  mutate(name = gsub("_percent_change_from_baseline", "", name))

g_google <-
  ggplot(data = google_tn, aes(x = as.Date(date))) +
  theme_linedraw() +
  geom_line(aes(y = value), color = "darkseagreen4", size = 1.0) +
  labs(title = "% Change from baseline") +
  facet_wrap(~ name, scales = "free_y")

print(g_google)

tn_unemploy <-
  tribble(
    ~date,        ~name,         ~value,
    "2020-02-01",	"0 - TN unemployment", -3.4,
    "2020-03-01",	"0 - TN unemployment", -3.3,
    "2020-04-01",	"0 - TN unemployment", -15.5,
    "2020-05-01",	"0 - TN unemployment", -11,
    "2020-06-01",	"0 - TN unemployment", -9.7) %>%
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
  mutate(date = as.Date(date)) %>%
  filter(country_region_code == "US") %>%
  select(date, workplaces_percent_change_from_baseline) %>%
  group_by(date) %>%
  summarize(workplaces_values = sum(workplaces_percent_change_from_baseline, na.rm = TRUE)) %>%
  ungroup() %>%
  as_tsibble(index = date) %>%
  tsibble::fill_gaps() %>% 
  na.locf() %>% 
  mutate(workplaces = workplaces_values %>% ts() %>% mstl() %>% trendcycle()) %>% 
  select(-workplaces_values) %>%
  pivot_longer(-date)

unemploy_us <-
  fredr(series_id = "UNRATE",
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
  facet_wrap(~ name, scales = "free_y")
print(g_google_unemploy_us)
