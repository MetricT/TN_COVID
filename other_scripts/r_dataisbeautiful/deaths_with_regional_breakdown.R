### Idea from:  https://www.reddit.com/r/dataisbeautiful/comments/i1sedr/oc_united_states_covid19_deaths_curve_with/
### Implementation by /u/MetricT

library(tidyverse)
library(tidycensus)
library(tigris)
library(forecast)
library(naniar)
library(useful)
library(TTR)


### Download this Github repo and add the path to the spreadsheet
### https://github.com/nytimes/covid-19-data
### Note that this data is *total* deaths, so I need to convert it to *new* deaths
if (!exists("data_loaded")) {

  # Remove these territories from our data
  remove_states <- 
    c("District of Columbia", "Puerto Rico", "Guam", 
      "Virgin Islands", "Northern Mariana Islands")

  # Load the spreadsheet and only leave states
  spreadsheet <- 
    "../Datasets/nytimes/covid-19-data/us-states.csv" %>%
    read_csv(col_names = TRUE, col_types = "Dccdd") %>%
    filter(!state %in% remove_states) %>%

    ### A special one for New Jersey...
    mutate(deaths = if_else(state == "New Jersey" & date >= as.Date("2020-06-25"), deaths - 1877, deaths)) %>%
  
    ### And two for New York...
    mutate(deaths = if_else(state == "New York" & date >= as.Date("2020-05-06"), deaths - 694, deaths)) %>%
    mutate(deaths = if_else(state == "New York" & date >= as.Date("2020-06-30"), deaths - 610, deaths))
  
    data_loaded <- 1
}

### Pluck the date out to include in our graph
current_date <- spreadsheet %>% arrange(date) %>% tail(n = 1) %>% pull("date")

### Define a list of regions and assign state to them.  Regions take from:
### https://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf

region_df <-
  tribble(
    ~region,    ~name,
    "region_1", "Northeast",
    "region_2", "Midwest",
    "region_3", "South",
    "region_4", "West",
  )
region_df <-
  region_df %>%
  bind_cols(
    colorRampPalette(c("#ffffcc", "#41b6c4"))(nrow(region_df)) %>% 
      as_tibble() %>% 
      rename(color = value)
  )

region_1 <- c("Pennsylvania", "New York", "New Jersey", "Vermont", "New Hampshire", "Maine", "Massachusetts", "Connecticut", "Rhode Island")
region_2 <- c("Ohio", "Indiana", "Michigan", "Illinois", "Wisconsin", "Minnesota", "Iowa", "Missouri", "Kansas", "Nebraska", "South Dakota", "North Dakota")
region_3 <- c("Texas", "Oklahoma", "Arkansas", "Louisiana", "Mississippi", "Alabama", "Georgia", "Florida", "Tennessee", "North Carolina", "South Carolina", "Virginia", "Kentucky", "West Virginia", "Maryland", "Delaware")
region_4 <- c("Alaska", "Hawaii", "California", "Oregon", "Washington", "Idaho", "Montana", "Wyoming", "Nevada", "Utah", "Colorado", "Arizona", "New Mexico")

state_df <- 
  tibble(
  state = c(region_1, region_2, region_3, region_4),
  region = ""
)

state_df$region[state_df$state %in% region_1] <- "region_1"
state_df$region[state_df$state %in% region_2] <- "region_2"
state_df$region[state_df$state %in% region_3] <- "region_3"
state_df$region[state_df$state %in% region_4] <- "region_4"

################################################################################
### Take our spreadsheet data, filter it a bit (New Jersey uglied up the graph 
### a few weeks ago), and compute a 7-day simple moving average of the data
################################################################################
data <-
  spreadsheet %>%
  filter(!is.na(deaths)) %>%
  filter(deaths != 0) %>%
  
  ### A special one for New Jersey...
  #mutate(deaths = if_else(state == "New Jersey" & date >= as.Date("2020-06-25"), deaths - 1877, deaths)) %>%
  
  arrange(date) %>% 
  left_join(state_df, by = "state") %>%
  group_by(date, region) %>%
  summarize(deaths = sum(deaths, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(id_cols = "date", names_from = "region", values_from = "deaths") %>%
  mutate(across(starts_with("region"), ~ . - lag(.))) %>%
  
  ### Take a 7-day SMA of values to smooth them out a bit
  mutate(across(starts_with("region"),
                .fns = list(sma   = ~ SMA(., n = 7)),
                .names = "{fn}_{col}"
  )) %>%
  select(-starts_with("region")) %>%
  rename_at(vars(starts_with("sma_")), ~ str_replace(., "sma_", "")) %>%
  filter(date >= as.Date("2020-03-01")) %>%
  pivot_longer(-date, names_to = c("type", "region"), names_sep = "_", values_to = "values") %>%
  mutate(region_sep = paste("region_", region, sep = "")) %>%
  select(-region) %>%
  pivot_wider(id_cols = c("date", "region_sep"), names_from = c("type"), values_from = c("values")) %>%
  rename(values = region) %>%
  mutate_if(is.numeric, ~ (replace_na(., 0))) %>%
  left_join(region_df, by = c("region_sep" = "region"))


################################################################################
### Draw the inset "Regional Curves" graph
################################################################################
data$region_sep <- 
  factor(data$region_sep,
         levels = c("region_3", "region_4", "region_2", "region_1"))

g_regional_curves_deaths <-
  ggplot(data = data, aes(x = as.Date(date), y = values)) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(strip.background = element_blank()) +

  geom_line(aes(y = values), color = "black", size = 1) +
  geom_area(aes(fill = as.factor(region_sep))) +
  
  labs(title = "Regional Curves", x = "", y = "") +
  scale_y_continuous(labels = scales::comma) +
  
  scale_fill_manual(values = c("region_1" = "#ffffcc",
                               "region_2" = "#a1dab4",
                               "region_3" = "#225ea8",
                               "region_4" = "#41b6c4")) +
    
  facet_wrap(~ name, nrow = 4, ncol = 1, strip.position = "right")
#print(g_regional_curves_deaths)

################################################################################
### Draw the inset USA map 
################################################################################
state_map <- 
  state_laea %>%
  filter(!GEOID %in% c(11)) %>%
  left_join(fips_codes %>% 
              select(state, state_name, state_code) %>% 
              unique() %>% 
              as_tibble(), by = c("GEOID" = "state_code")) %>%
  left_join(state_df, by = c("state_name" = "state")) %>%
  left_join(region_df, by = c("region" = "region"))

g_map_usa_regions_deaths <-
  ggplot(data = state_map) +
  theme_void() + 
  geom_sf(fill = state_map$color, color="black", size = 0.4, alpha = 0.8)
#print(g_map_usa_regions_deaths)

################################################################################
### Draw the main graph (stacked line graph of deaths)
################################################################################
caption <-
  paste("Data Source:  The New York Times\n",
        "Current as of ", 
        data %>% tail(n = 1) %>% pull("date") %>% format("%B %d, %Y"),
        sep = "")

total_deaths <- spreadsheet %>% filter(date == (spreadsheet %>% tail(n = 1) %>% pull("date"))) %>% pull("deaths") %>% sum()

#growing_at <- spreadsheet %>% select(date, deaths) %>% group_by(date) %>% summarize(deaths = sum(deaths)) %>% tail(n = 2) %>% pull(deaths) %>% diff()

growing_at <- 
  spreadsheet %>% 
  select(date, deaths) %>% 
  group_by(date) %>% 
  summarize(deaths = sum(deaths)) %>% 
  tail(n = 10) %>% 
  mutate(new_deaths = c(0, diff(deaths))) %>% 
  tail(n = 7) %>% 
  pull(new_deaths) %>% 
  mean() %>% 
  round() %>%
  format(big.mark = ",", scientific = FALSE)

up_rate <- 
  spreadsheet %>% 
  select(date, deaths) %>% 
  group_by(date) %>% 
  summarize(deaths = sum(deaths)) %>% 
  mutate(new_deaths = c(0, diff(deaths))) %>% 
  mutate(new_deaths_sma = SMA(new_deaths, n = 7)) %>%
  tail(n = 7) %>% 
  arrange(date) %>% 
  filter(row_number() %in% c(1, n())) %>% 
  pull(new_deaths_sma)

up_rate <- round(100 * (up_rate[2] - up_rate[1]) / up_rate[1], 2)


subtitle <-
  paste(total_deaths %>% format(big.mark = ",", scientific = FALSE), 
        " Total Deaths (",
        round(10000 * total_deaths / 327533795, 1),
        " per 10,000 people)\n",
        "Growing at ", growing_at, " new deaths/day\n",
        "Up ", up_rate, "% from 7 days ago",
        sep = "")

g_deaths_stacked <-
  ggplot(data = data, aes(x = as.Date(date), y = values, fill = region_sep)) + 
  theme_linedraw() + 
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  geom_area(color="black", size = 0.4, alpha = 0.8) +
  geom_vline(xintercept = as.Date("2020-07-25"), linetype = "dashed") +
  scale_fill_manual(values = c("region_1" = "#ffffcc",
                               "region_2" = "#a1dab4",
                               "region_3" = "#225ea8",
                               "region_4" = "#41b6c4")) +
  scale_y_continuous(labels = scales::comma) + 
  labs(title = "Daily COVID-19 Deaths",
       subtitle = subtitle,
       x = "Date", 
       y = "Daily Deaths",
       caption = caption)
#print(g_deaths_stacked)

################################################################################
### Draw the inset stacked graph percent chart in the upper-right
################################################################################

per_data <-
  data %>%
  group_by(date, region_sep) %>%
  summarise(n = sum(values)) %>%
  mutate(percentage = n / sum(n)) %>%
  select(-n) %>%
  filter(date >= as.Date("2020-03-16"))
  
g_deaths_stacked_per <-
  ggplot(data = per_data, aes(x = as.Date(date), y = percentage, fill = region_sep)) + 
  theme_linedraw() + 
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  geom_area(color="black", size = 0.4, alpha=.8) +
  scale_fill_manual(values = c("region_1" = "#ffffcc",
                               "region_2" = "#a1dab4",
                               "region_3" = "#225ea8",
                               "region_4" = "#41b6c4")) +
  labs(title = "Proportion of National Daily Deaths", x = "", y = "")
#print(g_deaths_stacked_per)


################################################################################
### Put it all together
################################################################################
g_final_deaths <-
  g_deaths_stacked + 
  annotation_custom(ggplotGrob(g_deaths_stacked_per), 
                               xmin = as.Date(data %>% tail(n = 1) %>% pull("date")) - 60, 
                               xmax = as.Date(data %>% tail(n = 1) %>% pull("date")), 
                               ymin = 1300,
                               ymax = 2300) +
  annotation_custom(ggplotGrob(g_map_usa_regions_deaths), 
                    xmin = as.Date(data %>% head(n = 1) %>% pull("date")) - 5,
                    xmax = as.Date(data %>% head(n = 1) %>% pull("date")) - 5 + 45, 
                    ymin = 1300,
                    ymax = 2300) + 

  annotation_custom(ggplotGrob(g_regional_curves_deaths), 
                  xmin = as.Date(data %>% head(n = 1) %>% pull("date")),
                  xmax = as.Date(data %>% head(n = 1) %>% pull("date")) + 23, 
                  ymin = 250,
                  ymax = 1250)
print(g_final_deaths)

