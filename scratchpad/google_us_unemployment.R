library(tsibble)
library(feasts)
library(zoo)
library(forecast)

################################################################################
### COVID-19 related mobile data
################################################################################
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
google_mobility <-
  "../Datasets/ActiveConclusion/COVID19_mobility/google_reports/mobility_report_US.csv" %>%
  read_csv()

google_mobility <-
  "../Google/Global_Mobility_Report.csv" %>%
  read_csv()

google_us <-
  google_mobility %>% 
  filter(country_region_code == "US") %>% 
  rename(country = country_region, 
         state = sub_region_1,
         value = workplaces_percent_change_from_baseline) %>%
  select(date, country, state, value) %>%
  filter(is.na(state)) %>%
  mutate(name = "Google change in visits to workplace in US,\n% change from baseline") %>%
  select(date, name, value) %>%
  mutate(value = value %>% ts() %>% mstl(frequency = 7) %>% trendcycle())

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
#  mutate(name = "Google change in visits to workplace in US,\n% change from baseline") %>%
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
  geom_vline(xintercept = as.Date("2020-08-01"), linetype = "dotted") +
  facet_wrap(~ name, scales = "free_y") +
  labs(x = "Date")
print(g_google_unemploy_us)
