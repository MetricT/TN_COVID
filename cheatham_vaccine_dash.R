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
  read_excel_url(vaccine_age_url, col_types = c("date", "text", "text", "numeric", "numeric", "numeric")) %>%
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
         Date = date) %>%
  mutate(Age = ifelse(Age == "50-55", "50-54", Age)) #%>%
  #filter(County == "CHEATHAM") 


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
  as_tsibble(index = "Date") %>% 
  model(STL(New_Count ~ season(period = "1 week"))) %>% 
  components() %>% 
  select(Date, season_adjust) %>%
  rename(New_Count = season_adjust) %>%
  ggplot() +
  theme_bw() +
  geom_point(aes(x = Date, y = New_Count), alpha = 0.2) +
  geom_line(aes(x = Date - 3, y = SMA(New_Count, n = 7))) +
  #geom_smooth(aes(x = Date, y = New_Count), method = "loess", formula = "y ~ x") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 220)) +
  labs(x = "", y  = "Vaccines/Day", title = "Cheatham Co. - New Vaccines/Day")
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
  geom_point(aes(x = Date, y = Count)) +
  #geom_smooth(aes(x = Date, y = Count), method = "loess", formula = "y ~ x") +
  geom_hline(yintercept = 40667, color = "darkgreen", linetype = "dashed") + 
  scale_y_continuous(labels = scales::comma, name = "Population", breaks = pretty_breaks(7),
                     sec.axis = sec_axis( trans = ~ . / 40667, name="% of Pop", breaks = pretty_breaks(10), labels = scales::percent_format(accuracy = 1))
                     ) +
  labs(x = "", y  = "Vaccines/Day", title = "Cheatham Co. - Total Vaccines")
print(g_total_vaccine)


g_count_vaccine <- plot_grid(g_new_vaccine, g_total_vaccine, nrow = 1, ncol = 2)

last_date <- vaccine_age_df %>% filter(Date == vaccine_age_df %>% arrange(Date) %>% tail(n = 1) %>% pull("Date"))

num_vac <- last_date %>% pull(Count) %>% sum() %>% format(big.mark = ",", scientific = FALSE)


########################################################################################

### Use this if you want to split the data into 5-year bins.   It looks a bit
### weird (you'll notice multiple 20-24, for instance) because the Census
### data is split weird (21 year olds, 22 year olds, 23-25 year olds) and has
### to combine them
agegroups <- c("0-4", "5-9", "10-14", "15-19", "15-19", "20-24", "20-24",
               "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54",
               "55-59", "60-64", "60-64", "65-69", "65-69", "70-74", "75-79",
               "80-84", "85+")

### Now create age groups for both genders
agesex <- c(paste("Male",   agegroups),
            paste("Female", agegroups))

### Fetch data from the Census
age_acs <- get_acs(geography   = "county",
                   state       = "47",
                   county      = "021",
                   year        = 2019,
                   table       = "B01001",
                   cache_table = TRUE)

### Extract the name of the location so we can print it in the title
loc_name <- age_acs %>% select(NAME) %>% unique() %>% pull()

### Take the age data, slice and dice it
age <- age_acs %>%
  mutate(variable = str_replace(variable, "B01001_0", "")) %>%
  filter(!variable %in% c("01", "02", "26")) %>%
  arrange(NAME, variable) %>%
  mutate(group = rep(agesex, length(unique(NAME)))) %>%
  group_by(NAME, group) %>%
  mutate(Population = sum(estimate)) %>%
  distinct(NAME, group, .keep_all = TRUE) %>%
  ungroup() %>%
  select(group, Population) %>%
  separate(group, into = c("Gender", "Ages"), sep = " ") %>%
  mutate(Ages = factor(Ages, levels = unique(Ages))) %>%
  mutate(Population = ifelse(Gender == "Female", Population, -Population)) %>%
  mutate(pop = abs(Population)) %>% 
  group_by(Ages) %>% 
  summarize(pop = sum(pop)) %>% 
  ungroup()


last_date <-
  last_date %>%
  mutate(Age = fct_relevel(Age, c(
    "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", 
    "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", 
    "65-69", "70-74", "75-79", "80-84", "85+"
  ))) %>%
  left_join(age, by = c("Age" = "Ages")) %>%
  mutate(Count_Frac = Count / pop)


########################################################################################


graph_age_cases <-
  ggplot(data = last_date, aes(x = Age, y = Count)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.7)) +
  theme(axis.text.x = element_text(hjust = 0.8)) +
  geom_bar(stat = "identity", fill = "darkred") +
  geom_text(nudge_y = 0.05 * max(last_date$Count),
            label = last_date$Count %>% format(big.mark = ",", scientific = FALSE)) +
  scale_y_continuous(labels = scales::comma,
                     sec.axis = sec_axis(~ . / sum(last_date$Count), labels = scales::percent_format(accuracy = 1))) +
  labs(title = paste("Cheatham Co. Vaccinations by Age: ", num_vac, " out of 40,997", sep = ""), x = "", y = "")
print(graph_age_cases)

graph_age_cases_per <-
  ggplot(data = last_date, aes(x = Age, y = Count_Frac)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.7)) +
  theme(axis.text.x = element_text(hjust = 0.8)) +
  geom_bar(stat = "identity", fill = "darkred") +
  geom_text(nudge_y = 0.05 * max(last_date$Count_Frac),
            label = paste((last_date$Count_Frac * 100) %>% round(digits = 0), "%", sep = "")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = pretty_breaks(5),
                     sec.axis = sec_axis(~ . / sum(last_date$Count_Frac), labels = scales::percent_format(accuracy = 1))) +
  labs(title = paste("Cheatham Co. Vaccination Percentage by Age: ", num_vac, " out of 40,997 vaccinated as of ", 
                     last_date %>% tail(n = 1) %>% pull("Date"), sep = ""), x = "", y = "")
print(graph_age_cases_per)

plot_grid(graph_age_cases_per, 
          g_count_vaccine, 
          g_rt_combined,
          #map_tn_vaccine_per_capita, 
          nrow = 3, ncol = 1, rel_heights = c(0.7, 1, 1))