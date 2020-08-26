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
keepme <- c("covid_data", "cases_df", "deaths_df", "hospital_df",
            "testres_df", "testrates_df", "recover_df", "age_df",
            "demographics_df", "data_loaded", "api_key_census")
rm(list = ls()[!ls() %in% keepme])

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

### Cache this because loading the spreadsheets is slowwwwww.....
if (!exists("data_loaded")) {

  ### URL's for the spreadsheets
  url_root <- "https://www.tn.gov/content/dam/tn/health/documents/cedep/novel-coronavirus/datasets/Public-Dataset-"

  age_ss_url             <- paste(url_root, "Age.XLSX", sep = "")
  age_by_county_url      <- paste(url_root, "Daily-County-Age-Group.XLSX", sep = "")
  daily_case_url         <- paste(url_root, "Daily-Case-Info.XLSX", sep = "")
  county_new_url         <- paste(url_root, "County-New.XLSX", sep = "")
  race_ethnicity_sex_url <- paste(url_root, "RaceEthSex.XLSX", sep = "")
  county_school_url      <- paste(url_root, "Daily-County-Cases-5-18-Years.XLSX", sep = "")
  mmwr_url               <- paste(url_root, "MMWR-Week-Case-Count.XLSX", sep = "")

  
  ### Read 'em in...
  age_ss_df <-
    read_excel_url(age_ss_url) %>%
    mutate(DATE = as.Date(DATE))

  ### Test:  If the data is old, it means TN hasn't updated their spreadsheets
  ###        yet.   Test the data, and die if it's yesterday's data.   Obviously
  ###        comment this out if you *want* yesterday's data
  data_date <- age_ss_df  %>% select(DATE) %>% arrange(DATE) %>% tail(n = 1) %>% pull()
  if (data_date < Sys.Date() & (Sys.time() %>% hour()) >= 14) {
    stop("TN Spreadsheets haven't been updated with today's data yet.  Last data from ", data_date, ". Exiting...")
  }

  age_by_county_df  <-
    read_excel_url(age_by_county_url) %>%
    mutate(DATE = as.Date(DATE))

  daily_case_df <-
    read_excel_url(daily_case_url) %>%
    mutate(DATE = as.Date(DATE))

  county_new_df <-
    read_excel_url(county_new_url,
                   col_types = c("date", "text", "numeric", "numeric",
                                 "numeric", "numeric", "numeric", "numeric",
                                 "numeric", "numeric", "numeric", "numeric",
                                 "numeric", "numeric", "numeric", "numeric",
                                 "numeric", "numeric", "numeric", "numeric",
                                 "numeric", "numeric")) %>%
    mutate(DATE = as.Date(DATE))

  race_ethnicity_sex_df <-
    read_excel_url(race_ethnicity_sex_url) %>%
    mutate(Date = as.Date(Date))

  county_school_df <-
    read_excel_url(county_school_url) %>%
    mutate(DATE = as.Date(DATE))
  
  mmwr_df <-
    read_excel_url(mmwr_url) %>%
    mutate(DATE = as.Date(DATE))
  
  ################################################################################
  ### Cleaning the state's data...
  ################################################################################
  ###
  ### The State of TN missed one day's worth of data on 6/28.  That missing date
  ### precludes several forms of analysis (particularly STL) which are useful.
  ###
  ### The script below interpolates the data for the missing data by taking the
  ### values for the day before/after and splitting the difference.
  ###
  ### Comment out the line below if you want to use the "pure" data without my
  ### fix
  source("patches/2020-06-28/patch_injector.R")
  #source("patch.R")
  
  ### Todo:  Add a patch for data earlier than the state's DB.   Just straight
  ### data, no fancy patch
  
}


################################################################################
### Subset the the data here (if desired)
################################################################################

### For the map, we only want the latest total for each county.
non_county_fields <- c("Date", "Total", "Other_State_Country", "Out of State",
                       "Pending", "Unknown", "ERROR!!!")

### All counties
county_names <-
  county_new_df %>%
  filter(!COUNTY %in% non_county_fields) %>%
  pull(COUNTY) %>%
  unique() %>%
  sort()


################################################################################
### Select the map you want
################################################################################

graph_counties <- c("Total")
map_counties <- county_names
location <- "Tennessee"

### Or if you would like to do some other subset, uncomment the "source"
### below and then add the lines for your desired subset

source("geography.R")

#graph_counties <- nashville_msa
#map_counties <- nashville_msa
#location <- "Nashville MSA"

#graph_counties <- c("Davidson")
#map_counties <- c("Davidson")
#location <- "Davidson County"

#graph_counties <- c("Cheatham")
#map_counties <- c(nashville_msa, cheatham_superset)
#location <- "Cheatham County"



################################################################################
### A few visual configuration settings
################################################################################

map_palette <- c("#7f7f7f", viridis(n = 1000, direction = -1))
map_fontsize <- 3
graph_color <- "darkseagreen4"
geom_thickness <- 0.25
line_thickness <- 1.0
geom_age_orientation <- coord_flip()
ENABLE_LOG_SCALE <- FALSE

### If TRUE, graph total cases, new cases, and total deaths on a log scale
### If FALSE, graph them normally

if (isTRUE(ENABLE_LOG_SCALE)) {
     graph_log10_opts1 <- scale_y_log10(breaks = c(1, 10, 100, 1000))
     graph_log10_opts2 <- annotation_logticks()
} else {
     graph_log10_opts1 <- geom_blank()
     graph_log10_opts2 <- geom_blank()
}


################################################################################
### Adjusting the geometry of our map
################################################################################

map_counties_fips <-
  map_counties %>%
  as_tibble() %>%
  mutate(value = tolower(value)) %>%
  left_join(fips_codes %>%
              as_tibble() %>%
              filter(state_code == "47") %>%
              select(county_code, county) %>%
              mutate(county = gsub(" County", "", county)) %>%
              mutate(county = tolower(county)),
            by = c("value" = "county"))

### Use TidyCensus to pull population data as well as map geometry
### from the Census bureau
### Note: Spews red "errors" because I'm feeding it county names instead of 
### FIPS codes.   Fix later.
county_acs <-
  get_acs(geography   = "county",
          variables   = c("B01003_001"),
          state       = c("47"),
          county      = map_counties_fips %>% as_vector(),
          year        = 2018,
          geometry    = TRUE,
          cache_table = TRUE) %>%
  rename(POP2018 = estimate)

### Split the Census data apart into a map...
tn_map_df <-
  county_acs %>%
  select(GEOID, geometry)

### And a population dataframe
tn_pop_df <-
  county_acs %>%
  st_set_geometry(NULL) %>%
  mutate(NAME = str_replace(NAME, " County, Tennessee", "")) %>%
  select(GEOID, NAME, POP2018) %>%
  rename(County = NAME) %>%
  mutate(County = if_else(County == "DeKalb", "Dekalb", County))

################################################################################
### Take our imported data from the spreadsheets, and split it into some useful
### structures to make manipulation simpler.    The maps/graphs pull data from
### these structures, so you should be able to "port" this script to other
### states or localities by loading the data and munging it into a compatible 
### tibble.
################################################################################

### Now build data tibbles compatible with the rest of the script
total_cases_tib <-
  county_new_df %>%
  select(DATE, COUNTY, TOTAL_CASES) %>%
  mutate(Date = as.Date(DATE)) %>%
  select(-DATE) %>%
  #filter(!COUNTY %in% c("Pending", "Out of State")) %>%
  arrange(Date, COUNTY) %>%
  pivot_wider(names_from = c("COUNTY"),
              values_from = c("TOTAL_CASES")) %>%
  select("Date", sort(peek_vars())) %>%
  mutate_if(is.numeric, ~ (replace_na(., 0))) %>%
  mutate(Total = rowSums(select(., !starts_with("Date")))) %>%
  select(Date, Total, sort(peek_vars()))

new_cases_tib <-
  county_new_df %>%
  select(DATE, COUNTY, NEW_CASES) %>%
  mutate(Date = as.Date(DATE)) %>%
  select(-DATE) %>%
  #filter(!COUNTY %in% c("Pending", "Out of State")) %>%
  arrange(Date, COUNTY) %>%
  pivot_wider(names_from = c("COUNTY"),
              values_from = c("NEW_CASES")) %>%
  select("Date", sort(peek_vars())) %>%
  mutate_if(is.numeric, ~ (replace_na(., 0))) %>%
  mutate(Total = rowSums(select(., !starts_with("Date")))) %>%
  select(Date, Total, sort(peek_vars()))

total_deaths_tib <-
  county_new_df %>%
  select(DATE, COUNTY, TOTAL_DEATHS) %>%
  mutate(Date = as.Date(DATE)) %>%
  select(-DATE) %>%
  filter(Date >= as.Date("2020-04-01")) %>%
  #filter(!COUNTY %in% c("Pending", "Out of State")) %>%
  arrange(Date, COUNTY) %>%
  pivot_wider(names_from = c("COUNTY"),
              values_from = c("TOTAL_DEATHS")) %>%
  select("Date", sort(peek_vars())) %>%
  mutate_if(is.numeric, ~ (replace_na(., 0))) %>%
  mutate(Total = rowSums(select(., !starts_with("Date")))) %>%
  select(Date, Total, sort(peek_vars()))

new_deaths_tib <-
  county_new_df %>%
  select(DATE, COUNTY, NEW_DEATHS) %>%
  mutate(Date = as.Date(DATE)) %>%
  select(-DATE) %>%
  filter(Date >= as.Date("2020-04-01")) %>%
  #filter(!COUNTY %in% c("Pending", "Out of State")) %>%
  arrange(Date, COUNTY) %>%
  pivot_wider(names_from = c("COUNTY"),
              values_from = c("NEW_DEATHS")) %>%
  select("Date", sort(peek_vars())) %>%
  mutate_if(is.numeric, ~ (replace_na(., 0))) %>%
  mutate(Total = rowSums(select(., !starts_with("Date")))) %>%
  select(Date, Total, sort(peek_vars()))

total_recovered_tib <-
  county_new_df %>%
  select(DATE, COUNTY, TOTAL_RECOVERED) %>%
  mutate(Date = as.Date(DATE)) %>%
  select(-DATE) %>%
  filter(Date >= as.Date("2020-04-10")) %>%
  #filter(!COUNTY %in% c("Pending", "Out of State")) %>%
  arrange(Date, COUNTY) %>%
  pivot_wider(names_from = c("COUNTY"),
              values_from = c("TOTAL_RECOVERED")) %>%
  select("Date", sort(peek_vars())) %>%
  mutate_if(is.numeric, ~ (replace_na(., 0))) %>%
  mutate(Total = rowSums(select(., !starts_with("Date")))) %>%
  select(Date, Total, sort(peek_vars()))

new_recovered_tib <-
  county_new_df %>%
  select(DATE, COUNTY, NEW_RECOVERED) %>%
  mutate(Date = as.Date(DATE)) %>%
  select(-DATE) %>%
  filter(Date >= as.Date("2020-04-10")) %>%
  #filter(!COUNTY %in% c("Pending", "Out of State")) %>%
  arrange(Date, COUNTY) %>%
  pivot_wider(names_from = c("COUNTY"),
              values_from = c("NEW_RECOVERED")) %>%
  select("Date", sort(peek_vars())) %>%
  mutate_if(is.numeric, ~ (replace_na(., 0))) %>%
  mutate(Total = rowSums(select(., !starts_with("Date")))) %>%
  select(Date, Total, sort(peek_vars()))

total_active_tib <-
  county_new_df %>%
  select(DATE, COUNTY, TOTAL_ACTIVE) %>%
  mutate(Date = as.Date(DATE)) %>%
  select(-DATE) %>%
  filter(Date >= as.Date("2020-03-31")) %>%
  #filter(!COUNTY %in% c("Pending", "Out of State")) %>%
  arrange(Date, COUNTY) %>%
  pivot_wider(names_from = c("COUNTY"),
              values_from = c("TOTAL_ACTIVE")) %>%
  select("Date", sort(peek_vars())) %>%
  mutate_if(is.numeric, ~ (replace_na(., 0))) %>%
  mutate(Total = rowSums(select(., !starts_with("Date")))) %>%
  select(Date, Total, sort(peek_vars()))

new_active_tib <-
  county_new_df %>%
  select(DATE, COUNTY, NEW_ACTIVE) %>%
  mutate(Date = as.Date(DATE)) %>%
  select(-DATE) %>%
  filter(Date >= as.Date("2020-03-31")) %>%
  #filter(!COUNTY %in% c("Pending", "Out of State")) %>%
  arrange(Date, COUNTY) %>%
  pivot_wider(names_from = c("COUNTY"),
              values_from = c("NEW_ACTIVE")) %>%
  select("Date", sort(peek_vars())) %>%
  mutate_if(is.numeric, ~ (replace_na(., 0))) %>%
  mutate(Total = rowSums(select(., !starts_with("Date")))) %>%
  select(Date, Total, sort(peek_vars()))

total_hospitalized_tib <-
  county_new_df %>%
  select(DATE, COUNTY, TOTAL_HOSPITALIZED) %>%
  mutate(Date = as.Date(DATE)) %>%
  select(-DATE) %>%
  filter(Date >= as.Date("2020-04-27")) %>%
  #filter(!COUNTY %in% c("Pending", "Out of State")) %>%
  arrange(Date, COUNTY) %>%
  pivot_wider(names_from = c("COUNTY"),
              values_from = c("TOTAL_HOSPITALIZED")) %>%
  select("Date", sort(peek_vars())) %>%
  mutate_if(is.numeric, ~ (replace_na(., 0))) %>%
  mutate(Total = rowSums(select(., !starts_with("Date")))) %>%
  select(Date, Total, sort(peek_vars()))

new_hospitalized_tib <-
  county_new_df %>%
  select(DATE, COUNTY, NEW_HOSPITALIZED) %>%
  mutate(Date = as.Date(DATE)) %>%
  select(-DATE) %>%
  filter(Date >= as.Date("2020-04-27")) %>%
  #filter(!COUNTY %in% c("Pending", "Out of State")) %>%
  arrange(Date, COUNTY) %>%
  pivot_wider(names_from = c("COUNTY"),
              values_from = c("NEW_HOSPITALIZED")) %>%
  select("Date", sort(peek_vars())) %>%
  mutate_if(is.numeric, ~ (replace_na(., 0))) %>%
  mutate(Total = rowSums(select(., !starts_with("Date")))) %>%
  select(Date, Total, sort(peek_vars()))

################################################################################
### Build tertiary data structures.  These use the *_tib structures above and
### should be "abstracted" from whatever the actual data source is, so you
### shouldn't have to change them if you port it to a new source.
################################################################################

###################################################
# Total confirmed cases by county
###################################################
total_cases_by_county <-
  total_cases_tib %>%
  pivot_longer(-Date) %>%
  filter(!name %in% c("Pending", "Out of State")) %>%
  pivot_wider(id_cols = "Date", names_from = "name", values_from = "value") %>%
  tail(n = 1) %>%
  select(-Total) %>%
  pivot_longer(-Date) %>%
  select(name, value) %>%
  rename(County = name,
         total_cases = value)

###################################################
# New confirmed cases by county
###################################################
new_cases_by_county <-
  new_cases_tib %>%
  pivot_longer(-Date) %>%
  filter(!name %in% c("Pending", "Out of State")) %>%
  pivot_wider(id_cols = "Date", names_from = "name", values_from = "value") %>%
  tail(n = 1) %>%
  select(-Total) %>%
  pivot_longer(-Date) %>%
  select(name, value) %>%
  rename(County = name,
         new_cases = value)

###################################################
# Total deaths by county
###################################################
total_deaths_by_county <-
  total_deaths_tib %>%
  pivot_longer(-Date) %>%
  filter(!name %in% c("Pending", "Out of State")) %>%
  pivot_wider(id_cols = "Date", names_from = "name", values_from = "value") %>%
  tail(n = 1) %>%
  select(-Total) %>%
  pivot_longer(-Date) %>%
  select(name, value) %>%
  rename(County = name,
         total_deaths = value)

###################################################
# New deaths by county
###################################################
new_deaths_by_county <-
  new_deaths_tib %>%
  pivot_longer(-Date) %>%
  filter(!name %in% c("Pending", "Out of State")) %>%
  pivot_wider(id_cols = "Date", names_from = "name", values_from = "value") %>%
  tail(n = 1) %>%
  select(-Total) %>%
  pivot_longer(-Date) %>%
  select(name, value) %>%
  rename(County = name,
         new_deaths = value)


###################################################
# Total recovered by county
###################################################
total_recovered_by_county <-
  total_recovered_tib %>%
  pivot_longer(-Date) %>%
  filter(!name %in% c("Pending", "Out of State")) %>%
  pivot_wider(id_cols = "Date", names_from = "name", values_from = "value") %>%
  tail(n = 1) %>%
  select(-Total) %>%
  pivot_longer(-Date) %>%
  select(name, value) %>%
  rename(County = name,
         total_recovered = value)


###################################################
# New recovered by county
###################################################
new_recovered_by_county <-
  new_recovered_tib %>%
  pivot_longer(-Date) %>%
  filter(!name %in% c("Pending", "Out of State")) %>%
  pivot_wider(id_cols = "Date", names_from = "name", values_from = "value") %>%
  tail(n = 1) %>%
  select(-Total) %>%
  pivot_longer(-Date) %>%
  select(name, value) %>%
  rename(County = name,
         new_recovered = value)


###################################################
# Total active by county
###################################################
total_active_by_county <-
  total_active_tib %>%
  pivot_longer(-Date) %>%
  filter(!name %in% c("Pending", "Out of State")) %>%
  pivot_wider(id_cols = "Date", names_from = "name", values_from = "value") %>%
  tail(n = 1) %>%
  select(-Total) %>%
  pivot_longer(-Date) %>%
  select(name, value) %>%
  rename(County = name,
         total_active = value)


###################################################
# New active by county
###################################################
new_active_by_county <-
  new_active_tib %>%
  pivot_longer(-Date) %>%
  filter(!name %in% c("Pending", "Out of State")) %>%
  pivot_wider(id_cols = "Date", names_from = "name", values_from = "value") %>%
  tail(n = 1) %>%
  select(-Total) %>%
  pivot_longer(-Date) %>%
  select(name, value) %>%
  rename(County = name,
         new_active = value)


###################################################
# Combine the data above and add to the map
###################################################
stats_df <-
  total_cases_by_county %>%
  left_join(new_cases_by_county,       by = "County") %>%
  left_join(total_deaths_by_county,    by = "County") %>%
  left_join(new_deaths_by_county,      by = "County") %>%
  left_join(total_active_by_county,    by = "County") %>%
  left_join(new_active_by_county,      by = "County") %>%
  left_join(total_recovered_by_county, by = "County") %>%
  left_join(new_recovered_by_county,   by = "County")


################################################################################
### Math section for charts
################################################################################
cases_data <-
  total_cases_tib %>%
  select(Date, all_of(graph_counties)) %>%
  mutate(Total = rowSums(select(., !starts_with("Date")))) %>%
  rename(total_cases = Total) %>%
  rename(date = Date) %>%
  filter(total_cases != 0) %>%
  mutate(new_cases = c(total_cases[1], diff(total_cases)))

recovered_data <-
  total_recovered_tib %>%
  select(Date, all_of(graph_counties)) %>%
  mutate(Total = rowSums(select(., !starts_with("Date")))) %>%
  rename(total_recovered = Total) %>%
  rename(date = Date) %>%
  filter(total_recovered != 0) %>%
  mutate(new_recovered = c(total_recovered[1], diff(total_recovered)))

deaths_data <-
  total_deaths_tib %>%
  select(Date, all_of(graph_counties)) %>%
  mutate(Total = rowSums(select(., !starts_with("Date")))) %>%
  rename(total_deaths = Total) %>%
  rename(date = Date) %>%
  filter(total_deaths != 0) %>%
  mutate(new_deaths = c(total_deaths[1], diff(total_deaths)))

active_data <-
  total_active_tib %>%
  select(Date, all_of(graph_counties)) %>%
  mutate(Total = rowSums(select(., !starts_with("Date")))) %>%
  rename(total_active = Total) %>%
  rename(date = Date) %>%
  mutate(new_active = c(total_active[1], diff(total_active)))

hospital_data <-
  total_hospitalized_tib %>%
  select(Date, all_of(graph_counties)) %>%
  mutate(Total = rowSums(select(., !starts_with("Date")))) %>%
  rename(total_hospitalized = Total) %>%
  rename(date = Date) %>%
  mutate(new_hospitalized = c(total_hospitalized[1], diff(total_hospitalized)))

merge_data <-
  cases_data %>%
  full_join(recovered_data, by = "date") %>%
  full_join(deaths_data,    by = "date") %>%
  full_join(active_data,    by = "date") %>%
  full_join(hospital_data,  by = "date") %>%
  filter(date >= as.Date("2020-04-10")) %>%
  mutate(cfr = total_deaths / total_cases)

age_df <-
  age_ss_df %>%
  select(DATE, AGE_RANGE, AR_CASECOUNT, NEW_ARCASES,
         AR_TOTALDEATHS, AR_NEWDEATHS) %>%
  rename(AGE = AGE_RANGE,
         TOTAL_CASES = AR_CASECOUNT,
         NEW_CASES = NEW_ARCASES,
         TOTAL_DEATHS = AR_TOTALDEATHS,
         NEW_DEATHS = AR_NEWDEATHS)

################################################################################
### The code below takes my script and renders the infographic.   It should be 
### possible to split off the top part and use it as a "data loader" for other
### scripts
################################################################################

### Make a working copy of the state geometry
counties <- tn_pop_df

counties <-
  counties %>%
  left_join(tn_map_df, by = "GEOID") %>%
  left_join(stats_df,  by = "County")

### Find the center of each county so we can add the number of cases
county_centers <-
  tn_map_df$geometry %>%
  as_Spatial() %>%
  gCentroid(byid = TRUE)

### The centroid function is very good, but occasionally can give less-than-
### perfect results, often due to counties with extreme concave shape.   We
### can use "nudges" to adjust the location of the number to make it look good.

# Default nudge is 0
counties$nudge_y <- 0
counties$nudge_x <- 0

counties$nudge_y[counties$County == "Chester"]   <-  0.035
counties$nudge_y[counties$County == "Houston"]   <-  0.015
counties$nudge_y[counties$County == "Putnam"]    <-  0.025
counties$nudge_y[counties$County == "Loudon"]    <-  0.015
counties$nudge_y[counties$County == "Anderson"]  <- -0.015
counties$nudge_x[counties$County == "Pickett"]   <- -0.065
counties$nudge_x[counties$County == "Unicoi"]    <- -0.070
counties$nudge_y[counties$County == "Unicoi"]    <- -0.035
counties$nudge_x[counties$County == "Hancock"]   <- -0.035
counties$nudge_x[counties$County == "Trousdale"] <- -0.020
counties$nudge_x[counties$County == "Lake"]      <-  0.030
counties$nudge_x[counties$County == "Moore"]     <-  0.005
counties$nudge_y[counties$County == "Moore"]     <-  0.015




################################################################################
### Load maps
################################################################################
#source("maps/new_active_percapita_last7.R")   # New active per capita last 7
#source("maps/new_cases_percapita_last7.R")    # New cases per capita last 7
#source("maps/new_deaths_percapita.R")         # New deaths per capita
#source("maps/total_active_percapita_last7.R") # Total active per capita last 7
#source("maps/total_recovered_percapita.R")    # Total recoverd per capita last 7
#source("maps/total_cases.R")                  # Total confirmed cases
#source("maps/total_deaths.R")                 # Total deaths
#source("maps/new_deaths.R")                   # New deaths
#source("maps/new_active.R")                   # New active
#source("maps/total_active.R")                 # Total active
#source("maps/total_recovered.R")              # Total recovered
#source("maps/new_recovered.R")                # New recovered
#source("maps/case_fatality_rate.R")           # Case Fatality Rate
#source("maps/total_cases_percapita.R")        # Total confirmed cases
source("maps/total_deaths_percapita.R")        # Total deaths per 100k
source("maps/total_active_percapita.R")        # Total active per 100k
source("maps/new_cases.R")                     # New confirmed cases
source("maps/new_deaths_last7.R")              # New deaths last 7 days
source("maps/new_recovered_last7.R")           # New recovered last 7 days
source("maps/new_active_last7.R")              # New active last 7 days


################################################################################
### Load graphs
################################################################################
#source("graphs/total_cases.R")              # Tot. confirmed cases
#source("graphs/log_total_vs_new.R")         # Log(total vs New)
#source("graphs/total_hospitalized.R")       # Total hospitalized
#source("graphs/total_recovered.R")          # Total recovered
#source("graphs/total_deaths.R")             # Total deaths
#source("graphs/case_fatality_rate.R")       # Case Fatality Rate
#source("graphs/total_positive_test_rate.R") # Tot. Pos. Rate
source("graphs/new_recovered.R")             # New recovered
source("graphs/total_active.R")              # Total active
source("graphs/new_active.R")                # New active
source("graphs/new_deaths.R")                # New deaths
source("graphs/new_cases.R")                 # New confirmed cases
source("graphs/barchart_ages.R")             # Bar chart for age
source("graphs/new_hospitalized.R")          # New hospitalized
source("graphs/new_tests_perday.R")          # New tests per day
source("graphs/new_positive_test_rate.R")    # Positive test rate


################################################################################
### Arrange the graphs into the final infographic
################################################################################

# What's the date of the last data point
data_date <- new_cases_tib %>% arrange(Date) %>% tail(n = 1) %>% pull("Date")

# Title
title_string <- paste("COVID-19 Confirmed Cases in ", location,
                      " [", as.Date(data_date), "]\n",
                      "from TN Dept of Health spreadsheets", sep = "")

# Footer
footer_string <- paste("SMA = 7-day Simple Moving Average\n",
                       "Data Source:  Tennessee Department of Health\n",
                       "https://www.tn.gov/health/cedep/ncov.html", sep = "")

grob_charts <-
 plot_grid(graph_new_positive_test_rate, graph_new_tests_perday,  graph_new_hospital,  graph_age_cases,    graph_age_deaths,
           graph_new_cases,              graph_new_deaths,        graph_new_active,    graph_total_active, graph_new_recover,
           ncol = 5, nrow = 2, align = "hv")

grob_maps <-
 plot_grid(map_new_recovered_last7, map_new_cases,
           map_new_deaths_last7,    map_total_active_percapita,
           map_new_active_last7,    map_total_deaths_percapita,
           ncol = 2, nrow = 3, align = "hv")
   
charts <- plot_grid(grob_maps,
                    grob_charts,
                    ncol = 1, nrow = 2,
                    rel_widths  = c(1, 1),
                    rel_heights = c(1, 1))

title  <- ggdraw() + draw_label(title_string, fontface = "bold")

footer <- ggdraw() + draw_label(footer_string, size = 10)

final <- plot_grid(title, charts, footer,
                   nrow = 3, rel_heights = c(0.05, 1, 0.05))

Sys.sleep(3)
print(final)

### Save image to file and we're done!
scale <- 1.3
width <- round(1300 * scale)
height <- round(900 * scale)
png("TN_COVID_Viridis.png", width = width, height = height)
plot(final)
dev.off()
