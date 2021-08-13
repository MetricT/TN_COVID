###############################################################################
### Render a map of Tennessee using TN Dept. of Health data on COVID-19
###
### TN Dept. of Health COVID-19 website:
###     https://www.tn.gov/health/cedep/ncov.html
###
### Note: You will need a Census API key in order to access their data.  You 
###       can get a key by going to this website:
###
###       https://api.census.gov/data/key_signup.html
###
### Once you have the key, just add it below.
###
### Data also available at:
### https://www.tn.gov/health/cedep/ncov/data/downloadable-datasets.html
################################################################################

### Set the Census API key.   Get your API key at https://api.census.gov/data/key_signup.html
# api_key_census <- "PUT_YOUR_CENSUS_API_KEY_HERE"

### Let's start by cleaning the environment, it currently causes the graphs
### some problems if the environment is already populated
###
### We don't want to erase these, it speeds things up considerably...
#keepme <- c("covid_data", "cases_df", "deaths_df", "hospital_df",
#            "testres_df", "testrates_df", "recover_df", "age_df",
#            "demographics_df", "data_loaded", "api_key_census")
#rm(list = ls()[!ls() %in% keepme])

# If I want to generate charts for an earlier day, uncomment this
#last_date <- as.Date("2020-04-18")

### Load the necessary libraries
packages <- c("tidyverse", "dplyr", "readxl", "ggplot2", "sf", "rgeos", "TTR",
              "scales", "cowplot", "viridis", "gridExtra", "tidycensus", "zoo",
              "RColorBrewer", "reshape2", "tidyselect", "feasts", "fable", 
              "lubridate", "tsibble", "keyring")

new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages, quiet = TRUE)
invisible(lapply(packages, "library", quietly = TRUE,
                 character.only = TRUE, warn.conflicts = FALSE))

### Or I can pull my api key from keyring
#api_key_census <- key_get("api_key_census")

### Set the Census API key
census_api_key(api_key_census)

### Enable map caching
options(tigris_use_cache = TRUE)

################################################################################
### Read data from my spreadsheet
################################################################################

### Function to read Excel spreadsheets from URL's since the "readxl" package is
### behind the times
read_excel_url <- function(url, ...) {
  tf <- tempfile(fileext = ".xlsx")
  curl::curl_download(url, tf)
  return(readxl::read_excel(tf, ...))
}

vac_url_root <- "https://www.tn.gov/content/dam/tn/health/documents/cedep/novel-coronavirus/datasets/"
vaccine_age_url <- paste(vac_url_root, "COVID_VACCINE_COUNTY_AGE_GROUPS_CENSUS.XLSX", sep = "")
vaccine_age_df <- 
  read_excel_url(vaccine_age_url) %>%
  mutate(DATE = as.Date(DATE)) %>%
  pivot_wider(id_cols = c("DATE", "PATIENT_COUNTY"), 
              names_from = "AGE_GROUP_CENSUS", 
              values_from = "RECIPIENT_COUNT") %>% 
  janitor::clean_names() %>% 
  mutate_if(is.numeric, ~ (replace_na(., 0))) %>%
  mutate(`x85+` = x85_89 + x90_94 + x95_99 + x100) %>%
  select(-x100, -x85_89, -x90_94, -x95_99, -pending) %>%
  pivot_longer(-c(date, patient_county), 
               names_to = "Age", 
               values_to = "Count") %>%
  filter(!patient_county %in% c("OUT OF STATE")) %>%
  mutate(Age = gsub("x", "", Age), Age = gsub("_", "-", Age)) %>%
  rename(County = patient_county,
         Date = date)


latest_date <- 
  vaccine_age_df %>% 
  select(Date) %>% 
  unique() %>% 
  arrange(Date) %>% 
  tail(n = 1) %>% 
  pull()

### County Current Total
vac_county_total <-
  vaccine_age_df %>%
  filter(Date == latest_date) %>%
  select(County, Count) %>% 
  group_by(County) %>% 
  summarize(Count = sum(Count)) %>% 
  ungroup()


### New Vaccines/Day
g_new_vaccine <- 
  vaccine_age_df %>% 
  select(Date, Count) %>%
  group_by(Date) %>% 
  summarize(Count = sum(Count)) %>% 
  ungroup() %>% 
  mutate(New_Count = c(0, diff(Count))) %>% 
  select(Date, New_Count) %>% 
  ggplot() +
  theme_bw() +
  geom_point(aes(x = Date, y = New_Count)) +
  geom_smooth(aes(x = Date, y = New_Count), method = "loess", formula = "y ~ x") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "", y  = "Vaccines/Day", title = "TN - New Vaccines/Day")
print(g_new_vaccine)

### Total Vaccines/Day
g_total_vaccine <-
  vaccine_age_df %>% 
  select(Date, Count) %>%
  group_by(Date) %>% 
  summarize(Count = sum(Count)) %>% 
  ungroup() %>% 
  select(Date, Count) %>% 
  ggplot() +
  theme_bw() +
  geom_point(aes(x = Date, y = Count / 1000000)) +
  #geom_smooth(aes(x = Date, y = Count / 1000000), method = "loess", formula = "y ~ x") +
  geom_hline(yintercept = 6.829, color = "darkgreen", linetype = "dashed") + 
  scale_y_continuous(labels = scales::comma_format(suffix = "M"), name = "Population", breaks = pretty_breaks(7),
                     sec.axis = sec_axis( trans = ~ . / 6.829, name="% of Pop", breaks = pretty_breaks(10), labels = scales::percent_format(accuracy = 1))
                     ) +
  labs(x = "", y  = "Vaccines/Day", title = "TN - Total Vaccines")
print(g_total_vaccine)


g_count_vaccine <- plot_grid(g_new_vaccine, g_total_vaccine, nrow = 1, ncol = 2)

plot_grid(map_tn_vaccine_per_capita, g_count_vaccine, nrow = 2, ncol = 1, rel_heights = c(0.7, 1))
