### Idea from:  https://www.reddit.com/r/dataisbeautiful/comments/i1sedr/oc_united_states_covid19_deaths_curve_with/
### Implementation by /u/MetricT

library(tidyverse)
library(tidycensus)
library(tigris)
library(forecast)
library(naniar)
library(useful)
library(TTR)
library(sf)

bins <-
  get_acs(geography   = "county",
          variables   = c("B01003_001"),
          year        = 2018,
          geometry    = FALSE,
          cache_table = TRUE) %>%
  rename(POP2018 = estimate) 

bins <-
  bins %>%
  mutate(NAME = gsub(" County, Tennessee", "", NAME)) %>%
  arrange(desc(POP2018)) %>%
  mutate(cum_pop = cumsum(POP2018)) %>%
  mutate(cum_pop_frac = cum_pop / sum(bins$POP2018)) %>%
  mutate(bin = case_when(
    cum_pop_frac <  0.251    ~ "Highest Population",
    cum_pop_frac >= 0.251 &
      cum_pop_frac <  0.50  ~ "High Population",
    cum_pop_frac >= 0.50 &
      cum_pop_frac <  0.75  ~ "Low Population",
    cum_pop_frac >= 0.75    ~ "Lowest Population",
  )) %>%
  select(GEOID, POP2018, bin)

spreadsheet <- 
  "../Datasets/nytimes/covid-19-data/us-counties.csv" %>%
  read_csv(col_names = TRUE, col_types = "Dcccdd") %>%
  mutate(state_fips = substr(fips, 1, 2)) %>%
  filter(!state_fips %in% c(69, 72, 78)) %>%
  filter(!is.na(state_fips)) %>%
  select(date, fips, cases, deaths) %>%
  arrange(date) %>%
  pivot_wider(id_cols = "date",
              names_from = "fips",
              values_from = c("cases", "deaths")) %>%
  mutate_if(is.numeric, ~ (replace_na(., 0))) %>%
  mutate(across(-date, ~ . - lag(.))) %>%
  mutate_if(is.numeric, ~ (replace_na(., 0))) %>% 
  mutate("cases_53061" = if_else(date == as.Date("2020-01-21"), 1, `cases_53061`)) %>%
  pivot_longer(-date, names_to = c("type", "fips"), names_sep = "_", values_to = "values") %>% 
  pivot_wider(id_cols = c("date", "fips"), names_from = "type", values_from = "values")
  
# Just pluck out TN
#spreadsheet <- 
#  spreadsheet %>% 
#  filter(substr(fips, 1, 2) == "47") %>% 
#  filter(date >= as.Date("2020-03-05")) %>%
#  left_join((fips_codes %>% 
#               mutate(fips = paste(state_code, county_code, sep = "")) %>% 
#               mutate(county = gsub(" County", "", county)) %>% 
#               filter(state_code == "47") %>% 
#               select(county, fips)), by = "fips")

### Pluck the date out to include in our graph
current_date <- spreadsheet %>% arrange(date) %>% tail(n = 1) %>% pull("date")


################################################################################
### Take our spreadsheet data and compute a 7-day simple moving average of the data
################################################################################
data <-
  spreadsheet %>%
  filter(!is.na(deaths)) %>%
  filter(deaths != 0) %>%
  arrange(date) %>% 
  left_join(bins, by = c("fips" = "GEOID")) %>%
  group_by(date, bin) %>%
  summarize(deaths = sum(deaths, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(id_cols = "date", names_from = "bin", names_prefix = "bin_", values_from = "deaths") %>%
  mutate_if(is.numeric, ~ (replace_na(., 0))) %>%

  ### Take a 7-day SMA of values to smooth them out a bit
  mutate(across(starts_with("bin_"),
                .fns = list(sma   = ~ SMA(., n = 7)),
                .names = "{fn}_{col}"
  )) %>%
  select(-starts_with("bin_")) %>%
  rename_at(vars(starts_with("sma_bin_")), ~ str_replace(., "sma_bin_", "")) %>%
  pivot_longer(-date, names_to = "type", values_to = "values")


################################################################################
### Draw the inset "Regional Curves" graph
################################################################################
data$type <- 
  factor(data$type,
         levels = rev(c("Highest Population", "High Population", "Low Population", "Lowest Population")))

g_regional_curves_deaths <-
  ggplot(data = data, aes(x = as.Date(date), y = values)) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(strip.background = element_blank()) +

  geom_line(aes(y = values), color = "black", size = 1) +
  geom_area(aes(fill = as.factor(type))) +
  
  labs(title = "", x = "", y = "") +
  scale_y_continuous(labels = scales::comma) +
  
  scale_fill_manual(
    values = c("Highest Population" = "#56106E",
               "High Population"    = "#BB3754",
               "Low Population"     = "#F98C0A",
               "Lowest Population"  = "#FCFFA4")) +
    
  facet_wrap(~ type, nrow = 4, ncol = 1, strip.position = "right")
print(g_regional_curves_deaths)

################################################################################
### Draw the inset USA map 
################################################################################
county_map <-
  read_sf("../Shapefiles/us_county/us_county.shp") %>% 
  filter(!STATEFP %in% c("60", "66", "69", "72", "78")) %>%
  left_join(bins, by = "GEOID")

g_map_us_regions_deaths <-
  ggplot(data = county_map) +
  theme_void() + 
  theme(legend.position = "none") +
  geom_sf(aes(fill = as.factor(bin)), color = "black", size = 0.03) +
  
  scale_fill_manual(
    values = c("Highest Population" = "#56106E",
               "High Population"    = "#BB3754",
               "Low Population"     = "#F98C0A",
               "Lowest Population"  = "#FCFFA4"))
  
print(g_map_us_regions_deaths)

################################################################################
### Draw the main graph (stacked line graph of cases)
################################################################################
caption <-
  paste("Data Source:  The New York Times\n",
        "Current as of ", 
        data %>% tail(n = 1) %>% pull("date") %>% format("%B %d, %Y"),
        sep = "")

total_deaths <- spreadsheet %>% select(deaths) %>% sum()

growing_at <- 
  spreadsheet %>% 
  select(date, deaths) %>% 
  group_by(date) %>% 
  summarize(deaths = sum(deaths)) %>% 
  tail(n = 7) %>% 
  pull(deaths) %>% 
  mean() %>% 
  round() %>%
  format(big.mark = ",", scientific = FALSE)

up_rate <- 
  spreadsheet %>% 
  select(date, deaths) %>% 
  group_by(date) %>% 
  summarize(deaths = sum(deaths)) %>%
  ungroup() %>% 
  mutate(new_deaths_sma = SMA(deaths, n = 7)) %>%
  tail(n = 7) %>% 
  arrange(date) %>% 
  filter(row_number() %in% c(1, n())) %>% 
  pull(new_deaths_sma)

up_rate <- round(100 * (up_rate[2] - up_rate[1]) / up_rate[1], 2)

up_rate_txt <- "Up"
if (up_rate < 0) {
  up_rate_txt <- "Down"}

subtitle <-
  paste(total_deaths %>% format(big.mark = ",", scientific = FALSE), 
        " Total Deaths (",
        round(100000 * total_deaths / sum(bins$POP2018), 2),
        " per 100k)\n",
        "Average ", growing_at, " new deaths/day over the last 7 days\n",
        up_rate_txt, " ", abs(up_rate), "% from 7 days ago",
        sep = "")

g_deaths_stacked <-
  ggplot(data = data, aes(x = as.Date(date), y = values, fill = type)) + 
  theme_linedraw() + 
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  geom_area(color="black", size = 0.4, alpha = 0.8) +
  scale_fill_manual(
    values = c("Highest Population" = "#56106E",
               "High Population"    = "#BB3754",
               "Low Population"     = "#F98C0A",
               "Lowest Population"  = "#FCFFA4")) +
  scale_y_continuous(labels = scales::comma) + 
  labs(title = "Daily COVID-19 Deaths in US binned by county population\nEach color represents 25% of the US Population",
       subtitle = subtitle,
       x = "Date", 
       y = "Daily Deaths",
       caption = caption)
print(g_deaths_stacked)

################################################################################
### Draw the inset stacked graph percent chart in the upper-right
################################################################################

per_data <-
  data %>%
  group_by(date, type) %>%
  summarise(n = sum(values, na.rm = TRUE)) %>%
  mutate(percentage = n / sum(n)) %>%
  select(-n) %>%
  filter(date >= as.Date("2020-03-16"))
  
g_deaths_stacked_per <-
  ggplot(data = per_data, aes(x = as.Date(date), y = percentage, fill = type)) + 
  theme_linedraw() + 
  theme(legend.title = element_blank(),
        legend.position = "none",
        plot.background = element_rect(fill = "transparent",colour = NA)) +
  geom_area(color="black", size = 0.4, alpha = 0.8) +
  ### Assuming the bins were divided perfectly
  #geom_hline(yintercept = 0.25, linetype = "dotted") + 
  #geom_hline(yintercept = 0.50, linetype = "dotted") + 
  #geom_hline(yintercept = 0.75, linetype = "dotted") + 
  
  ### How the bins are *actually* divided...  
  geom_hline(yintercept = 0.244, linetype = "dotted") + 
  geom_hline(yintercept = 0.502, linetype = "dotted") + 
  geom_hline(yintercept = 0.746, linetype = "dotted") + 
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_manual(
    values = c("Highest Population" = "#56106E",
               "High Population"    = "#BB3754",
               "Low Population"     = "#F98C0A",
               "Lowest Population"  = "#FCFFA4")) +
  labs(title = "Proportion of Daily Deaths", x = "", y = "")
print(g_deaths_stacked_per)


################################################################################
### Put it all together
################################################################################

g_final_deaths <-
  g_deaths_stacked + 
  annotation_custom(ggplotGrob(g_deaths_stacked_per), 
                    xmin = as.Date(data %>% head(n = 1) %>% pull("date")) + 85, 
                    xmax = as.Date(data %>% head(n = 1) %>% pull("date")) + 85 + 60, 
                    ymin = 1000,
                    ymax = 3200) +
  
  annotation_custom(ggplotGrob(g_map_us_regions_deaths), 
                    xmin = as.Date(data %>% head(n = 1) %>% pull("date")) + 175,
                    xmax = as.Date(data %>% head(n = 1) %>% pull("date")) + 175 + 75, 
                    ymin = 1000,
                    ymax = 3250) + 

  annotation_custom(ggplotGrob(g_regional_curves_deaths), 
                    xmin = as.Date(data %>% head(n = 1) %>% pull("date")) - 5,
                    xmax = as.Date(data %>% head(n = 1) %>% pull("date")) - 5 + 33, 
                    ymin = 150,
                    ymax = 3200)
print(g_final_deaths)

