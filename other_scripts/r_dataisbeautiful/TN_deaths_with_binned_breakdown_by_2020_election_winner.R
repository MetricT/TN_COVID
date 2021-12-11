### Idea from:  https://www.reddit.com/r/dataisbeautiful/comments/i1sedr/oc_united_states_covid19_deaths_curve_with/
### Implementation by /u/MetricT

library(tidyverse)
library(tidycensus)
library(tigris)
library(forecast)
library(naniar)
library(useful)
library(TTR)

### Load outcome data from 2020 US Presidential election
pres_2020 <-
  "../Datasets/US_County_Level_Election_Results_08-16/2020_US_County_Level_Presidential_Results.csv" %>%
  read_csv(col_names = TRUE, col_types = "cdcdddnddd") %>%
  rename(fips = county_fips,
         dem = per_dem,
         rep = per_gop) %>%
  mutate(fips = str_pad(fips, 5, pad = "0"),
         year = 2020,
         other = 1 - dem - rep) %>%
  filter(substr(fips, 1, 2) == "47") %>%
  mutate(fips = if_else(fips == "46113", "46102", fips)) %>%
  mutate(fips = if_else(fips == "02270", "02158", fips)) %>%
  rowwise() %>%
  mutate(winner = max(dem, rep, other)) %>%
  ungroup() %>%
  mutate(winner = case_when(
    winner == dem ~ "Democrat",
    winner == rep ~ "Republican",
    winner == other ~ "Other")) %>%
  mutate(margin = rep - dem) %>%
  mutate(county_name = gsub(" County", "", county_name)) %>%
  select(fips, county_name, margin)

### Get county population
pop <-
  get_acs(geography   = "county",
          variables   = c("B01003_001"),
          state       = c("47"),
          year        = 2019,
          geometry    = FALSE,
          cache_table = TRUE) %>%
  rename(POP2019 = estimate) %>%
  mutate(NAME = gsub(" County, Tennessee", "", NAME)) %>%
  rename(county = NAME) %>%
  select(county, POP2019)

bins <- 
  pop %>% 
  left_join(pres_2020, by = c("county" = "county_name")) %>%
  arrange(margin)  %>%
  mutate(cum_pop = cumsum(POP2019)) %>%
  mutate(cum_pop_frac = cum_pop / 6709356) %>%
  mutate(bin = case_when(
    cum_pop_frac <  0.251    ~ "Democratic",
    cum_pop_frac >= 0.251 &
      cum_pop_frac <  0.501  ~ "Somewhat Republican",
    cum_pop_frac >= 0.501 &
      cum_pop_frac <  0.762  ~ "Republican",
    cum_pop_frac >= 0.762    ~ "Very Republican",
  )) #%>%
  #mutate(county = ifelse(county == "DeKalb", "Dekalb", county)) 

################################################################################
### Pull death data from county_new_df (TN DoH spreadsheet)
################################################################################

# Map county names to FIPS code
fips_map <-
  fips_codes %>% 
  mutate(fips_code = paste(state_code, county_code, sep = "")) %>% 
  mutate(county = gsub(" County", "", county)) %>% 
  filter(state_code == "47") %>% 
  select(county, fips_code) %>% 
  #mutate(county = ifelse(county == "DeKalb", "Dekalb", county)) %>%
  as_tibble()

# Function to read Excel spreadsheets from URL's since the "readxl" package is
# behind the times
read_excel_url <- function(url, ...) {
  tf <- tempfile(fileext = ".xlsx")
  curl::curl_download(url, tf)
  return(readxl::read_excel(tf, ...))
}

# URL's for the spreadsheets
url_root <- "https://www.tn.gov/content/dam/tn/health/documents/cedep/novel-coronavirus/datasets/Public-Dataset-"

county_new_url         <- paste(url_root, "County-New.XLSX", sep = "")

county_new_df <-
  read_excel_url(county_new_url,
                 col_types = c("date", "text", "numeric", "numeric",
                               "numeric", "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric")) %>%
  mutate(DATE = as.Date(DATE)) %>%
  mutate(COUNTY = ifelse(COUNTY == "Dekalb", "DeKalb", COUNTY))

spreadsheet <-
  county_new_df %>%
  select(DATE, COUNTY, NEW_CASES, NEW_DEATHS) %>%
  filter(!COUNTY %in% c("Out of State", "Pending")) %>%
  left_join(fips_map, by = c("COUNTY" = "county")) %>%
  select(DATE, fips_code, COUNTY, NEW_CASES, NEW_DEATHS) %>%
  rename(date = DATE, fips = fips_code, county = COUNTY, cases = NEW_CASES, deaths = NEW_DEATHS) %>%
  mutate(deaths = ifelse(deaths < 0, 0, deaths))

#spreadsheet <- 
#  "../Datasets/nytimes/covid-19-data/us-counties.csv" %>%
#  read_csv(col_names = TRUE, col_types = "Dcccdd") %>%
#  mutate(state_fips = substr(fips, 1, 2)) %>%
#  filter(!state_fips %in% c(69, 72, 78)) %>%
#  filter(!is.na(state_fips)) %>%
#  select(date, fips, cases, deaths) %>%
#  arrange(date) %>%
#  pivot_wider(id_cols = "date",
#              names_from = "fips",
#              values_from = c("cases", "deaths")) %>%
#  mutate_if(is.numeric, ~ (replace_na(., 0))) %>%
#  mutate(across(-date, ~ . - lag(.))) %>%
#  mutate_if(is.numeric, ~ (replace_na(., 0))) %>% 
#  mutate("cases_53061" = if_else(date == as.Date("2020-01-21"), 1, `cases_53061`)) %>%
#  pivot_longer(-date, names_to = c("type", "fips"), names_sep = "_", values_to = "values") %>% 
#  pivot_wider(id_cols = c("date", "fips"), names_from = "type", values_from = "values")
  
# Just pluck out TN
#spreadsheet <- 
#  spreadsheet %>% 
#  filter(substr(fips, 1, 2) == "47") %>% 
#  filter(date >= as.Date("2020-03-05")) %>%
# left_join((fips_codes %>% 
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
  left_join(bins, by = c("county" = "county", "fips" = "fips")) %>%
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
         levels = rev(c("Democratic", "Somewhat Republican", "Republican", "Very Republican")))
#         levels = rev(c("Very Republican", "Republican", "Somewhat Republican", "Democratic")))

g_regional_curves_deaths <-
  ggplot(data = data, aes(x = as.Date(date), y = values)) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(strip.background = element_blank()) +

  geom_line(aes(y = values), color = "black", size = 1) +
  geom_area(aes(fill = as.factor(type))) +
  
  labs(title = "Regional Curves", x = "", y = "") +
  scale_y_continuous(labels = scales::comma) +
  
  scale_fill_manual(
    values = c("Democratic"          = "#0015BC",
               "Somewhat Republican" = "#f8b1b4",
               "Republican"          = "#f06268",
               "Very Republican"     = "#e9141d")) +
    
  facet_wrap(~ type, nrow = 4, ncol = 1, strip.position = "right")
print(g_regional_curves_deaths)

################################################################################
### Draw the inset USA map 
################################################################################
county_map <-
  read_sf("../Shapefiles/us_county/us_county.shp") %>% 
  filter(STATEFP == "47") %>%
  st_transform(crs = "+proj=aea +lat_1=25 +lat_2=50 +lon_0=-86") %>%
  left_join(bins, by = c("NAME" = "county"))

g_map_tn_regions_deaths <-
  ggplot(data = county_map) +
  theme_void() + 
  theme(legend.position = "none") +
  geom_sf(aes(fill = as.factor(bin)), color = "black", size = 0.4) +
  
  scale_fill_manual(
    values = c("Democratic"          = "#0015BC",
               "Somewhat Republican" = "#f8b1b4",
               "Republican"          = "#f06268",
               "Very Republican"     = "#e9141d"))
  
print(g_map_tn_regions_deaths)

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
        round(100000 * total_deaths / 6651089, 2),
        " per 100k)\n",
        "Growing at ", growing_at, " new deaths/day\n",
        up_rate_txt, " ", abs(up_rate), "% from 7 days ago",
        sep = "")

g_deaths_stacked <-
  ggplot(data = data, aes(x = as.Date(date), y = values, fill = type)) + 
  theme_linedraw() + 
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  geom_area(color="black", size = 0.4, alpha = 0.8) +
  scale_fill_manual(
    values = c("Democratic"          = "#0015BC",
               "Somewhat Republican" = "#f8b1b4",
               "Republican"          = "#f06268",
               "Very Republican"     = "#e9141d")) +
  scale_y_continuous(labels = scales::comma) + 
  labs(title = "Daily COVID-19 Deaths in Tennessee",
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
  geom_hline(yintercept = 0.250, linetype = "dotted") + 
  geom_hline(yintercept = 0.513, linetype = "dotted") + 
  geom_hline(yintercept = 0.749, linetype = "dotted") + 
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_manual(
    values = c("Democratic"          = "#0015BC",
               "Somewhat Republican" = "#f8b1b4",
               "Republican"          = "#f06268",
               "Very Republican"     = "#e9141d")) +
  labs(title = "Proportion of Statewide Daily Deaths", x = "", y = "")
print(g_deaths_stacked_per)


################################################################################
### Put it all together
################################################################################

### What's the highest value in the data
y_max <- data %>% select(date, values) %>% filter(!is.na(values)) %>% group_by(date) %>% summarize(values = sum(values)) %>% pull("values") %>% max()

g_final_deaths <-
  g_deaths_stacked + 
  annotation_custom(ggplotGrob(g_deaths_stacked_per), 
                    xmin = as.Date(data %>% head(n = 1) %>% pull("date")) + 340,
                    xmax = as.Date(data %>% head(n = 1) %>% pull("date")) + 340 + 180,
                    ymin = 60,
                    ymax = y_max) +
  annotation_custom(ggplotGrob(g_map_tn_regions_deaths), 
                    xmin = as.Date(data %>% head(n = 1) %>% pull("date")) + 100,
                    xmax = as.Date(data %>% head(n = 1) %>% pull("date")) + 100 + 185, 
                    ymin = 85,
                    ymax = y_max) + 
  annotation_custom(ggplotGrob(g_regional_curves_deaths), 
                    xmin = as.Date(data %>% head(n = 1) %>% pull("date")) - 0,
                    xmax = as.Date(data %>% head(n = 1) %>% pull("date")) - 0 + 100, 
                    ymin = 11,
                    ymax = y_max)
print(g_final_deaths)

