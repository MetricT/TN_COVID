### Idea from:  https://www.reddit.com/r/dataisbeautiful/comments/i1sedr/oc_united_states_covid19_deaths_curve_with/
### Implementation by /u/MetricT

library(tidyverse)
library(tidycensus)
library(tigris)
library(forecast)
library(naniar)
library(useful)
library(TTR)
library(politicaldata)

# Load the spreadsheet (which contains total cases/deaths) and compute new cases/deaths
spreadsheet <- 
  "../Datasets/nytimes/covid-19-data/us-counties.csv" %>%
  read_csv(col_names = TRUE, col_types = "Dcccdd") %>%
  filter(date >= as.Date("2021-10-25")) %>%
  mutate(state_fips = substr(fips, 1, 2)) %>%
  filter(state_fips %in% c("47")) %>%
  select(date, fips, cases, deaths) %>%
  arrange(date) %>%
  pivot_wider(id_cols = "date",
              names_from = "fips",
              values_from = c("cases", "deaths")) %>%
  mutate_if(is.numeric, ~ (replace_na(., 0))) %>%
  mutate(across(-date, ~ . - lag(.))) %>%
  mutate_if(is.numeric, ~ (replace_na(., 0))) %>% 
  pivot_longer(-date, names_to = c("type", "fips"), names_sep = "_", values_to = "values") %>% 
  pivot_wider(id_cols = c("date", "fips"), names_from = "type", values_from = "values")

spreadsheet <- spreadsheet %>% group_by(fips) %>% summarize(total_cases = sum(cases), total_deaths = sum(deaths)) %>% ungroup()

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

fip_map <- 
  fips_codes %>% 
  as_tibble() %>% 
  filter(state_code == "47") %>% 
  mutate(fips = paste(state_code, county_code, sep = "")) %>% 
  select(county, fips) %>% 
  mutate(county = gsub(" County", "", county)) %>%
  mutate(county = str_to_title(county))

vac_df <- 
  vaccine_age_df %>% 
  filter(Date == vaccine_age_df %>% arrange(Date) %>% tail(n = 1) %>% pull(Date)) %>%
  group_by(County) %>%
  summarize(Count = sum(Count)) %>%
  ungroup() %>%
  mutate(County = str_to_title(County)) %>%
  left_join(fip_map, by = c("County" = "county"))


population <-
  get_acs(geography   = "county",
          variables   = c("B01003_001"),
          year        = 2019,
          state       = "47",
          geometry    = FALSE,
          cache_table = TRUE) %>%
  rename(population = estimate) %>%
  select(GEOID, population)

pres_2020 <- 
  "../Datasets/tonmcg/US_County_Level_Election_Results_08-20/2020_US_County_Level_Presidential_Results.csv" %>%
  read_csv(col_names = TRUE, col_types = "cccddddddd") %>%
  rename(fips = county_fips, dem = per_dem, rep = per_gop) %>%
  mutate(fips = str_pad(fips, 5, pad = "0"),
         year = 2020,
         other = 1 - dem - rep) %>%
  filter(substr(fips, 1, 2) %in% c("47")) %>%
  rowwise() %>%
  mutate(winner = max(dem, rep, other)) %>%
  ungroup() %>%
  mutate(winner = case_when(
    winner == dem ~ "Democrat",
    winner == rep ~ "Republican",
    winner == other ~ "Other")) %>%
  mutate(margin = rep - dem) %>%
  select(year, total_votes, dem, rep, other, winner, margin, fips)

data <-
  vac_df %>%
  left_join(spreadsheet, by = "fips") %>%
  rename(vaccinated = Count) %>%
  left_join(population, by = c("fips" = "GEOID")) %>%
  mutate(vaccinated   = vaccinated / population,
         total_cases  = total_cases / population,
         total_deaths = total_deaths / population) %>%
  left_join(pres_2020, by = "fips") %>%
  select(fips, County, population, total_cases, total_deaths, vaccinated, rep, margin) %>%
  rename(county = County, cases = total_cases, deaths = total_deaths)

data_mean <- data %>% pull(population) %>% mean()
data_sd   <- data %>% pull(population) %>% sd()

data <- data %>% 
  mutate(pop_z = (population - data_mean) / data_sd, 
         pop_z = pop_z + 0.4936601 + 0.25)

g_cases_vaccination <-
  data %>% 
  ggplot() + 
  theme_bw() + 
  #geom_point(data = data %>% filter(county == "Cheatham"), aes(x = vaccinated, y = cases), color = "black", shape = 10, size = 6) + 
  geom_point(aes(x = vaccinated, y = cases, color = rep, size = pop_z)) + 
  geom_line(aes(x = vaccinated, y = 0.0864 - 0.0132 * vaccinated), size = 1.5) +
  #geom_smooth(aes(x = vaccinated, y = cases), method = "lm") +
  scale_colour_gradient2(name = "GOP %\nof Vote", high = "#E9141D", mid = "#BA93D4", low = "#0015BC", midpoint = 0.5, labels = scales::percent_format(accuracy = 1)) +
  scale_size(guide = "none") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) + #, limits = c(0.1, 0.2)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Vaccination Rate", y = "Cases per Capita", title = "Tennessee Counties: Cases per Capita vs Vaccination Rate")
print(g_cases_vaccination)


g_deaths_vaccination <-
  data %>% 
  ggplot() + 
  theme_bw() + 
  geom_point(data = data %>% filter(county == "Davidson"), aes(x = vaccinated, y = deaths), color = "black", shape = 10, size = 8) + 
  geom_point(aes(x = vaccinated, y = deaths, color = rep, size = pop_z)) + 
  geom_line(aes(x = vaccinated, y = 0.001520535 - 0.0007078620 * vaccinated), size = 1.5) +
  scale_colour_gradient2(name = "GOP %\nof Vote", high = "#E9141D", mid = "#BA93D4", low = "#0015BC", midpoint = 0.5, labels = scales::percent_format(accuracy = 1)) +
  scale_size(guide = "none") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) + #, limits = c(0.0004, 0.002)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Vaccination Rate", y = "Deaths per Capita", title = "Tennessee Counties: Deaths per Capita in 2021 vs Vaccination Rate")
print(g_deaths_vaccination)

plot_grid(g_cases_vaccination,
          g_deaths_vaccination,
          nrow = 2, ncol = 1, align = "hv")

#plot_grid(g_cases, g_deaths, nrow = 2, ncol = 1, align = "hv")

