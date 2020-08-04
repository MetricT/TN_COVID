### THIS DOESN"T WORK YET!!!   WORK IN PROGRESS

### Idea from:  https://www.reddit.com/r/dataisbeautiful/comments/i1sedr/oc_united_states_covid19_deaths_curve_with/

library(tidyverse)
library(forecast)
library(naniar)
library(useful)
library(sf)
library(viridis)

################################################################################
### Load a few interesting variables
################################################################################

### Population
tn_pop_df <-
  get_acs(geography   = "county",
          variables   = c("B01003_001"),
          state       = c("47"),
          year        = 2018,
          geometry    = FALSE,
          cache_table = TRUE) %>%
  rename(POP2018 = estimate) %>%
  mutate(NAME = str_replace(NAME, " County, Tennessee", "")) %>%
  select(GEOID, NAME, POP2018) %>%
  rename(County = NAME) %>%
  mutate(County = if_else(County == "DeKalb", "Dekalb", County))

### Land area of county (so we can compute population density)
area <- 
  counties(year = 2010,
         state = "47",
         class = "sf",
         progress_bar = TRUE) %>%
  st_drop_geometry() %>%
  as_tibble() %>%
  mutate(area = ALAND10 / 2589988) %>%
  select(GEOID10, area) %>%
  rename(GEOID = GEOID10)

tn_vars <-
  tn_pop_df %>%
  left_join(area, by = c("GEOID" = "GEOID")) %>%
  mutate(pop_density = POP2018 / area) %>%
  arrange(desc(pop_density)) %>%
  mutate(cum_pop = cumsum(POP2018)) %>%
  mutate(cum_pop_frac = cum_pop / sum(POP2018))


tn_dem_per <-
  read_csv("tn_dem_per.csv", col_names = TRUE, col_type = "cd") %>%
  mutate(county = if_else(county == "DeKalb", "Dekalb", county)) %>%
  left_join(tn_pop_df %>% select(County, POP2018), by = c("county" = "County")) %>%
  mutate(cum_pop = cumsum(POP2018)) %>% 
  mutate(cum_pop_frac = cum_pop / sum(POP2018)) %>%
  mutate(pop_group = case_when(
    cum_pop_frac <= 0.25 ~ "a",
    cum_pop_frac <= 0.50 ~ "b",
    cum_pop_frac <= 0.75 ~ "c",
    cum_pop_frac <= 1.00 ~ "d",
  ))

region_1 <- tn_dem_per %>% filter(pop_group == "a") %>% pull("county") %>% rev()
region_2 <- tn_dem_per %>% filter(pop_group == "b") %>% pull("county") %>% rev()
region_3 <- tn_dem_per %>% filter(pop_group == "c") %>% pull("county") %>% rev()
region_4 <- tn_dem_per %>% filter(pop_group == "d") %>% pull("county") %>% rev()

region_df <-
  tribble(
    ~region, ~color,    ~name,
    "region_1",     "#41b6c4", "Highest Democratic vote for POTUS 2016",
    "region_2",     "#225ea8", "2nd highest Dem vote",  
    "region_3",     "#a1dab4", "3rd highest Dem vote",
    "region_4",     "#ffffcc", "Lowest Democratic vote for POTUS 2016",
  )

################################################################################
### If we want to group counties by population (25% in each)
################################################################################

tn_pop_df <-
  get_acs(geography   = "county",
          variables   = c("B01003_001"),
          state       = c("47"),
          year        = 2018,
          geometry    = FALSE,
          cache_table = TRUE) %>%
  rename(POP2018 = estimate) %>%
  mutate(NAME = str_replace(NAME, " County, Tennessee", "")) %>%
  select(GEOID, NAME, POP2018) %>%
  rename(County = NAME) %>%
  mutate(County = if_else(County == "DeKalb", "Dekalb", County))


tn_pop_info <-
  tn_pop_df %>% 
  as_tibble() %>% 
  arrange(POP2018) %>%
  mutate(cum_pop = cumsum(POP2018)) %>% 
  mutate(cum_pop_frac = cum_pop / sum(POP2018)) %>%
  mutate(pop_group = case_when(
    cum_pop_frac <= 0.25 ~ "a",
    cum_pop_frac <= 0.50 ~ "b",
    cum_pop_frac <= 0.75 ~ "c",
    cum_pop_frac <= 1.00 ~ "d"
  )) %>%
  # Fix Knox, as it gets erroneously stuck in group 
  mutate(pop_group = if_else(County == "Knox", "c", pop_group))

region_1 <- tn_pop_info %>% filter(pop_group == "d") %>% pull("County") %>% rev()
region_2 <- tn_pop_info %>% filter(pop_group == "c") %>% pull("County") %>% rev()
region_3 <- tn_pop_info %>% filter(pop_group == "b") %>% pull("County") %>% rev()
region_4 <- tn_pop_info %>% filter(pop_group == "a") %>% pull("County") %>% rev()

county_reg_df <- 
  tibble(
    county = c(region_1, region_2, region_3, region_4),
    region = ""
  )

county_reg_df$region[county_reg_df$county %in% region_1] <- "region_1"
county_reg_df$region[county_reg_df$county %in% region_2] <- "region_2"
county_reg_df$region[county_reg_df$county %in% region_3] <- "region_3"
county_reg_df$region[county_reg_df$county %in% region_4] <- "region_4"

region_df <-
  tribble(
    ~region,    ~name,
    "region_1", "1st tier",
    "region_2", "2nd tier",
    "region_3", "3rd tier",
    "region_4", "4th tier",
  )
region_df <-
  region_df %>%
  bind_cols(
    inferno(n = 4) %>%
#    colorRampPalette(c("#ffffcc", "#41b6c4"))(nrow(region_df)) %>% 
      as_tibble() %>% 
      rename(color = value)
  )


################################################################################
### Draw the inset county map 
################################################################################
county_map <- 
  county_laea %>%
  st_transform(crs = "+proj=aea +lat_1=25 +lat_2=50 +lon_0=-86") %>%
  left_join(fips_codes %>% 
              mutate(fips_code = paste(state_code, county_code, sep = "")) %>%
              as_tibble(), by = c("GEOID" = "fips_code")) %>%
  mutate(county = gsub(" County", "", county)) %>%
  mutate(county = if_else(county == "DeKalb", "Dekalb", county)) %>%
  filter(state == "TN") %>%
  select(-state, -state_code, -state_name) %>%
  left_join(county_reg_df, by = "county") %>%
  left_join(region_df, by = "region")

g_map_state_regions <-
  ggplot(data = county_map) +
  theme_void() + 
  geom_sf(fill = county_map$color, color="black", size = 0.4, alpha = 0.8)
print(g_map_state_regions)


# Download this Github repo and add the path to the spreadsheet
# https://github.com/nytimes/covid-19-data
# Note that this data is *total* deaths, so I need to convert it to *new* deaths
spreadsheet <- 
  "../Datasets/nytimes/covid-19-data/us-counties.csv" %>%
  read_csv(col_names = TRUE, col_types = "Dcccdd")

data <-
  spreadsheet %>%
  filter(state == "Tennessee") %>%
  filter(!county %in% c("Unknown")) %>%
  mutate(county = if_else(county == "DeKalb", "Dekalb", county)) %>%
  select(date, county, deaths) %>%
  filter(deaths != 0) %>%
  arrange(date) %>% 
  mutate(region = case_when(
    county %in% region_1 ~ "region_1",
    county %in% region_2 ~ "region_2",
    county %in% region_3 ~ "region_3",
    county %in% region_4 ~ "region_4"
    )) %>%
  group_by(date, region) %>%
  summarize(deaths = sum(deaths, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(id_cols = "date", names_from = "region", values_from = "deaths") %>%
  mutate(across(-"date", ~ . - lag(.))) %>%
  filter(date >= as.Date("2020-03-21")) %>%
  mutate(trend_1 = region_1 %>% ts() %>% mstl() %>% trendcycle()) %>%
  mutate(trend_2 = region_2 %>% ts() %>% mstl() %>% trendcycle()) %>%
  mutate(trend_3 = region_3 %>% ts() %>% mstl() %>% trendcycle()) %>%
  mutate(trend_4 = region_4 %>% ts() %>% mstl() %>% trendcycle()) %>%
  pivot_longer(-date, names_to = c("type", "region"), names_sep = "_", values_to = "values") %>%
  mutate(region_sep = paste("region_", region, sep = "")) %>%
  select(-region) %>%
  pivot_wider(id_cols = c("date", "region_sep"), names_from = c("type"), values_from = c("values")) %>%
  rename(values = region) %>%
  filter(date >= as.Date("2020-05-01"))

### Join the data to the table about regions
data <-
  data %>%
  left_join(region_df, by = c("region_sep" = "region"))

 current_date <- data %>% tail(n = 1) %>% pull("date")

g_new_deaths <-
  ggplot(data = data) +
  theme_bw() +
  theme(legend.position = "none") +
  
  #geom_point(aes(x = as.Date(date), y = values, color = color), size = 1, shape = 19, alpha = 0.4) +

  geom_line(aes(x = as.Date(date), y = trend), color = "#222222", size = 1, alpha = 0.4) +
  
  
  labs(title = paste("Deaths data from ", current_date, sep = ""), x = "Date", y = "Deaths") + 
  scale_y_continuous(labels = scales::comma) + 
  facet_wrap(~ name)
  
print(g_new_deaths)

per_data <- 
  data %>%
  group_by(date, region_sep) %>%
  summarise(n = sum(trend)) %>%
  mutate(percentage = n / sum(n)) %>%
  left_join(region_df, by = c("region_sep" = "region"))

g_new_deaths_stack <-
  ggplot(data = per_data, 
         aes(x = date, 
             y = percentage,
             fill = region_sep)) +
  theme_classic() +
  geom_area() + 
  labs(x = "Date", y = "Deaths (moving average)")
print(g_new_deaths_stack)
  

