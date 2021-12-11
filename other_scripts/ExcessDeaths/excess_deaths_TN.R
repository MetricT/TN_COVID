################################################################################
### Pull mortality data from the CDC and graph excess death - /u/MetricT
################################################################################

library(tidyverse)
library(janitor)
library(cowplot)
library(lubridate)
library(tsibble)
library(geofacet)
library(zoo)
library(forecast)
library(feasts)
library(scales)
library(fabletools)
library(broom)


# Install your Census API key
# You can request one at https://api.census.gov/data/key_signup.html
# census_api_key("<YOUR_CENSUS_API_KEY_HERE>", install = TRUE)

# Fetch total/new official deaths from TN DoH

read_excel_url <- function(url, ...) {
  tf <- tempfile(fileext = ".xlsx")
  curl::curl_download(url, tf)
  return(readxl::read_excel(tf, ...))
}

official_deaths <- 
  "https://www.tn.gov/content/dam/tn/health/documents/cedep/novel-coronavirus/datasets/Public-Dataset-Daily-Case-Info.XLSX" %>%
  read_excel_url(col_types = NULL) %>%
  select(DATE, NEW_DEATHS, TOTAL_DEATHS) %>%
  rename(date                  = DATE,
         official_new_deaths   = NEW_DEATHS,
         official_total_deaths = TOTAL_DEATHS) %>%
  mutate(date = as.Date(date),
         yearweek = yearweek(date)) %>%
  select(yearweek, official_new_deaths, official_total_deaths) %>%
  group_by(yearweek) %>%
  summarize(official_new_deaths   = sum(official_new_deaths), 
            official_total_deaths = max(official_total_deaths)) %>%
  ungroup() %>%
  mutate(date = as.Date(yearweek)) %>%
  select(yearweek, date, official_new_deaths, official_total_deaths)



# Load data on finalized (2014-2018) and provisional (2019-2020) mortality
finalized_ss   <- read_csv("https://data.cdc.gov/api/views/3yf8-kanr/rows.csv?accessType=DOWNLOAD")
provisional_ss <- read_csv("https://data.cdc.gov/api/views/muzy-jte6/rows.csv?accessType=DOWNLOAD")


# Take the data and clean it up a bit
finalized   <- finalized_ss   %>% clean_names() %>% select("jurisdiction_of_occurrence", "mmwr_year", "mmwr_week", "all_cause")
provisional <- provisional_ss %>% clean_names() %>% select("jurisdiction_of_occurrence", "mmwr_year", "mmwr_week", "all_cause")


# Combine both datasets and filter the subset of states we want to look at
deaths <-
  finalized %>%
  bind_rows(provisional) %>%
  rename(year = mmwr_year,
         week = mmwr_week,
         deaths = all_cause,
         state = jurisdiction_of_occurrence) %>%
  filter(state %in% c("Tennessee")) %>%

  # Quick hack to remove "week 53" in 2014 because yearweek() can't handle it
  # This wouldn't be necessary if the spreadsheet included dates, it's an aliasing problem
  filter(week != 53) %>%

  # Turn the year and week into a yearweek() datatype
  mutate(yearweek = yearweek(date_decimal(year + (week - 1) / 52))) %>%
  select(yearweek, deaths) %>%
  arrange(yearweek) %>%
  
  # Remove the last week's worth of data since TN is slow to send mortality to CDC
  filter(row_number() <= n() - 1)

# Use a feasts STL() model to seasonally-adjust the deaths data
deaths_mod <-
  deaths %>%
  as_tsibble(index = "yearweek") %>%
  fill_gaps() %>%
  mutate_if(is.numeric, ~ na.locf(.)) %>%
  model(STL(deaths ~ trend() + season(window = "periodic"))) %>%
  components()


# Add the STL data values to our original death tibble
deaths <- 
  deaths %>% 
  left_join(deaths_mod %>% as_tibble() %>% select(-.model, -deaths), by = "yearweek") %>%
  
  # Ditch the "yearweek" at this point by converting it into a regular date.
  # "yearweek" was necessary for the tsibble model, but it's a weird way of 
  # tracking date that causes weird glitches.  For example, week("2014 W01") = 52.
  mutate(date = as.Date(yearweek)) %>%
  
  # Add a decimal date to make linear regression in the next step easier
  mutate(decimal_date = decimal_date(date)) 


# Compute a linear regression using data from 2014-2019 (before COVID hit) so
# we can subtract off deaths due to population/population growth.
lin_reg <-
  deaths %>%
  filter(date < as.Date("2020-01-01")) %>%
  lm(season_adjust ~ decimal_date, data = .) %>% 
  tidy() %>%
  pull("estimate")


# Add a "growth" term, so we can subtract off and get excess deaths
deaths <-
  deaths %>%
  mutate(growth = lin_reg[1] + lin_reg[2] * decimal_date) %>%
  mutate(excess_deaths = season_adjust - growth) %>%
  select(yearweek, date, decimal_date, deaths, trend, season_year, remainder, season_adjust, growth, excess_deaths) %>%
  
  # Add official deaths
  full_join(official_deaths %>% select(-date), by = "yearweek") %>%
  mutate(date = as.Date(yearweek)) %>%

  # Pivot longer and add a descriptive name for graphing
  pivot_longer(-c(yearweek, date, decimal_date), names_to = "series", values_to = "value") %>%
  filter(!series %in% c("trend", "remainder")) %>%
  mutate(description = case_when(
    series == "deaths"              ~ "All-Cause Deaths from CDC",
    series == "season_year"         ~ "Seasonal Deaths",
    #series == "trend"              ~ "Seasonally-adjusted trend",
    #series == "remainder"          ~ "Seasonally-adjusted remainder",
    series == "season_adjust"       ~ "Seasonally-adjusted All-Cause Deaths",
    series == "growth"              ~ "Deaths due to Population/Population Growth",
    series == "excess_deaths"       ~ "Excess Deaths",
    series == "official_new_deaths" ~ "Official COVID New Deaths/Week",
    series == "total_new_deaths"    ~ "Cumulative Official COVID Deaths"
  ))

# Now start making graphs for my talk
g_all_cause_deaths <- 
  deaths %>% 
  filter(series == "deaths") %>% 
  ggplot() + 
  theme_bw() + 
  geom_line(aes(x = date, y = value)) + 
  scale_x_date(breaks = pretty_breaks(10)) + 
  scale_y_continuous(breaks = pretty_breaks(10), labels = scales::comma) + 
  labs(x = "", y = "All-Cause Deaths", 
       title = "All-Cause Deaths in Tennessee, 2014 - 2021", 
       caption = "Mortality data taken from CDC.gov")
print(g_all_cause_deaths)

# Decompose all-cause deaths into seasonal + seasonal-adjusted deaths
g_all_cause_deaths_decompose <-
  deaths_mod %>% 
  autoplot() + 
  theme_bw() + 
  labs(x = "")
print(g_all_cause_deaths_decompose)

# Seasonally-adjusted deaths
g_seas_adj_deaths <- 
  deaths %>% 
  filter(series %in% c("season_adjust", "growth")) %>% 
  select(date, series, value) %>% 
  pivot_wider(id_cols = "date", names_from = "series", values_from = "value") %>%
  ggplot() + 
  theme_bw() + 
  #geom_point(aes(x = date, y = season_adjust), alpha = 0.2) +
  geom_line(aes(x = date, y = growth), size = 1.5, color = "blue", linetype = "dashed") +
  geom_line(aes(x = date, y = season_adjust),  alpha = 1.0) + 
  scale_x_date(breaks = pretty_breaks(10)) + 
  scale_y_continuous(breaks = pretty_breaks(10), labels = scales::comma) + 
  labs(x = "", y = "Seasonally-Adjusted Deaths", 
       title = "Seasonally-Adjusted Deaths in Tennessee, 2014 - 2021", 
       caption = "Mortality data taken from CDC.gov")
print(g_seas_adj_deaths)

# Excess weekly deaths
g_excess_deaths <- 
  deaths %>% 
  filter(series %in% c("excess_deaths")) %>% 
  ggplot() + 
  theme_bw() + 
  geom_line(aes(x = date, y = value)) + 
  geom_hline(yintercept = 0, color = "blue", linetype = "dashed") +
  scale_x_date(breaks = pretty_breaks(10)) + 
  scale_y_continuous(breaks = pretty_breaks(10), labels = scales::comma) + 
  labs(x = "", y = "Excess Deaths per Week", 
       title = "Weekly Excess Deaths in Tennessee, 2014 - 2021", 
       caption = "Mortality data taken from CDC.gov")
print(g_excess_deaths)

# Total Excess deaths
g_excess_deaths <- 
  deaths %>% 
  filter(series %in% c("excess_deaths")) %>% 
  ggplot() + 
  theme_bw() + 
  geom_line(aes(x = date, y = cumsum(value))) + 
  geom_hline(yintercept = 0, color = "blue", linetype = "dashed") +
  geom_vline(xintercept = as.Date("2020-03-20"), color = "darkred") + #, linetype = "dotted") + 
  annotate("text", size = 4, label = "1st Official TN COVID Death - 2020-03-10", x = as.Date("2020-02-15"), y = 10000, angle = "90", color = "darkred") +
  scale_x_date(breaks = pretty_breaks(10)) + 
  scale_y_continuous(breaks = pretty_breaks(10), labels = scales::comma) + 
  labs(x = "", y = "Excess Deaths", 
       title = "Total Excess Deaths in Tennessee, 2014 - 2021", 
       subtitle = "Mortality data taken from CDC.gov")
print(g_excess_deaths)


# Excess deaths since COVID arrived
g_excess_deaths_covid_era <- 
  deaths %>% 
  filter(series %in% c("excess_deaths", "official_new_deaths")) %>% 
  #filter(date >= as.Date("2020-01-01"),
  #       date <  as.Date("2021-07-01")) %>%
  pivot_wider(id_cols = "date", names_from = "series", values_from = "value") %>%
  ggplot() + 
  theme_bw() + 
  geom_line(aes(x = date, y = excess_deaths)) + 
  geom_line(aes(x = date, y = official_new_deaths), color = "firebrick3") + #, linetype = "dashed") +  
  geom_hline(yintercept = 0) + #, linetype = "dashed") +
  geom_vline(xintercept = as.Date("2020-03-20"), color = "darkred") + #, linetype = "dotted") + 
  annotate("text", size = 4, label = "1st Official TN COVID Death - 2020-03-10", x = as.Date("2020-02-15"), y = 500, angle = "90", color = "darkred") +
  scale_x_date(breaks = pretty_breaks(10)) + 
  scale_y_continuous(breaks = pretty_breaks(10), labels = scales::comma) + 
  labs(x = "", y = "Weekly Excess Deaths", 
       title = "Weekly Official COVID deaths (red) vs Weekly Excess Deaths in Tennessee", #, Jan 2020 - Jun 2021 (black)", 
       #caption = "Mortality data to compute Excess Deaths from CDC.gov\nOfficial COVID Deaths from Tennessee Dept. of Health"
       )
print(g_excess_deaths_covid_era)


# Excess deaths since COVID arrived
g_tot_excess_deaths_covid_era <- 
  deaths %>% 
  filter(series %in% c("excess_deaths", "official_total_deaths")) %>% 
  #filter(date >= as.Date("2020-01-01")) %>%
  #filter(date <  as.Date("2021-07-01")) %>%
  pivot_wider(id_cols = "date", names_from = "series", values_from = "value") %>%
  ggplot() + 
  theme_bw() + 
  geom_line(aes(x = date, y = cumsum(excess_deaths))) + 
  geom_line(aes(x = date, y = official_total_deaths), color = "firebrick3") + #, linetype = "dashed") +  

  geom_vline(xintercept = deaths %>% filter(series == "excess_deaths") %>% filter(!is.na(value)) %>% tail(n = 1) %>% pull(date) %>% as.Date(), linetype ="dashed") + 
  
  #geom_point(x = as.Date("2021-08-02"), y = 16076, shape = 10, size = 4, color = "black") +   
  #geom_point(x = as.Date("2021-08-02"), y = 12836, shape = 10, size = 4, color = "firebrick3") + 
  
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = as.Date("2020-03-20"), color = "darkred", linetype = "dotted") + 
  annotate("text", size = 4, label = "1st Official TN COVID Death - 2020-03-20", x = as.Date("2020-03-12"), y = 10000, angle = "90", color = "darkred") +
  scale_x_date(breaks = pretty_breaks(10)) + 
  scale_y_continuous(breaks = pretty_breaks(10), labels = scales::comma) + 
  labs(x = "", y = "Deaths", 
       title = paste("Total Official Weekly COVID deaths (red) vs Total Excess Deaths in Tennessee as of ", 
                     deaths %>% filter(series == "excess_deaths") %>% filter(!is.na(value)) %>% tail(n = 1) %>% pull(date), 
                     " (black)", sep = ""), 
       caption = "Mortality data to compute Excess Deaths from CDC.gov\nOfficial COVID Deaths from Tennessee Dept. of Health")
print(g_tot_excess_deaths_covid_era)

plot_grid(g_excess_deaths_covid_era, g_tot_excess_deaths_covid_era, nrow = 2, ncol = 1, align = "hv")

g_total_frac_official <-
  deaths %>% 
  select(date, series, value) %>% 
  filter(series %in% c("excess_deaths", "official_total_deaths")) %>% 
  filter(date >= as.Date("2020-03-09"), 
         date < as.Date("2021-09-20")) %>% 
  pivot_wider(id_cols = "date", names_from = "series", values_from = "value") %>% 
  mutate(cum_excess_deaths = cumsum(excess_deaths)) %>% 
  mutate(frac = official_total_deaths / cum_excess_deaths) %>% 
  ggplot() + 
  theme_bw() + 
  geom_line(aes(x = date, y = frac)) +
  scale_x_date(breaks = pretty_breaks(8)) + 
  scale_y_continuous(breaks = pretty_breaks(8), labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "", title = "Official Total COVID Deaths as a % of Cumulative Excess Deaths")
print(g_total_frac_official)

g_new_frac_official <-
  deaths %>% 
  select(date, series, value) %>% 
  filter(series %in% c("excess_deaths", "official_new_deaths")) %>% 
  filter(date >= as.Date("2020-03-09"), 
         date < as.Date("2021-09-20")) %>% 
  pivot_wider(id_cols = "date", names_from = "series", values_from = "value") %>% 
  mutate(frac = official_new_deaths / excess_deaths) %>% 
  ggplot() + 
  theme_bw() + 
  geom_line(aes(x = date - 3 , y = SMA(frac, n = 7)))
print(g_new_frac_official)

