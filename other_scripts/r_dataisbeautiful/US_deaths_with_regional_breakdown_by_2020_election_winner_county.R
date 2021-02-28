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

### Download this Github repo and add the path to the spreadsheet
### https://github.com/nytimes/covid-19-data

# Load the spreadsheet (which contains total cases/deaths) and compute
# new cases/deaths
spreadsheet <- 
  "../Datasets/nytimes/covid-19-data/us-counties.csv" %>%
  read_csv(col_names = TRUE, col_types = "Dcccdd") %>%
  mutate(state_fips = substr(fips, 1, 2)) %>%
  
  ### Filter out Alaska because they return cases by county but election results by house district, because <Alaska>
  ### Filter out Hawaii to be consistent and just use the continental US
  filter(!state_fips %in% c("02", "15")) %>%
  
  ### Also filter out US Territories
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

# Output date, state, new cases (NY Times doesn't include death counts so we can't do that...)

### Pluck the date out to include in our graph
current_date <- spreadsheet %>% arrange(date) %>% tail(n = 1) %>% pull("date")

pres_2020 <- 
  "../Datasets/tonmcg/US_County_Level_Election_Results_08-20/2020_US_County_Level_Presidential_Results.csv" %>%
  read_csv(col_names = TRUE, col_types = "cccddddddd") %>%
  rename(fips = county_fips, dem = per_dem, rep = per_gop) %>%
  mutate(fips = str_pad(fips, 5, pad = "0"),
         year = 2020,
         other = 1 - dem - rep) %>%
  filter(!substr(fips, 1, 2) %in% c("02", "15")) %>%
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
  select(year, total_votes, dem, rep, other, winner, margin, fips)

population <-
  get_acs(geography   = "county",
          variables   = c("B01003_001"),
          year        = 2019,
          geometry    = FALSE,
          cache_table = TRUE) %>%
  rename(population = estimate) %>%
  select(GEOID, population) %>%
  filter(!substr(GEOID, 1, 2) %in% c("02", "15", "72")) 

total_pop <- population$population %>% sum()

pres_2020 <- 
  pres_2020 %>% 
  left_join(population, by = c("fips"= "GEOID")) %>%
  mutate(voter_turnout = total_votes / population) 

pres_2020 <-
  pres_2020 %>% 
  arrange(desc(margin)) %>% 
  mutate(cum_population = cumsum(population), 
         percent = population / total_pop, 
         cum_percent = cum_population / total_pop) %>% 
  mutate(subtype = case_when(
    cum_percent < 0.2 ~ "Very\nGOP", 
    cum_percent < 0.4 ~ "GOP", 
    cum_percent < 0.6 ~ "Slightly\nDem", 
    cum_percent < 0.8 ~ "Dem", 
    cum_percent <= 1  ~ "Very\nDem"))

################################################################################
### Take our spreadsheet data, filter it a bit (New Jersey uglied up the graph 
### a few weeks ago), and compute a 7-day simple moving average of the data
################################################################################
data <-
  spreadsheet %>%
  left_join(pres_2020, by = "fips") %>%
  group_by(date, subtype) %>%
  summarize(deaths = sum(deaths, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(id_cols = c("date"), names_from = "subtype", values_from = "deaths") %>%
  ### Take a 7-day SMA of values to smooth them out a bit
  mutate(across(-date,
                .fns = list(sma   = ~ SMA(., n = 7)),
                .names = "{fn}_{col}"
  )) %>%
  select("date", starts_with("sma_")) %>%
  rename_at(vars(starts_with("sma_")), ~ str_replace(., "sma_", "")) %>%
  filter(date >= as.Date("2020-03-01")) %>%
  pivot_longer(-date, names_to = "subtype", values_to = "values") %>%
  mutate_if(is.numeric, ~ (replace_na(., 0))) %>%
  filter(!is.na(subtype))

################################################################################
### Draw the inset "Regional Curves" graph
################################################################################
#data$winner  <- factor(data$winner,  levels = c("Democrat", "Republican"))
data$subtype <- factor(data$subtype, levels = c("Very\nGOP", "GOP", "Slightly\nDem", "Dem", "Very\nDem"))

g_regional_curves_deaths <-
  ggplot(data = data, aes(x = as.Date(date), y = values)) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(strip.background = element_blank()) +

  geom_line(aes(y = values), color = "black", size = 1) +
  geom_area(aes(fill = as.factor(subtype))) +
  
  labs(title = "Deaths by Political Lean", x = "", y = "") +
  scale_y_continuous(labels = scales::comma) +
  
  scale_fill_manual(values = c("Very\nDem"       = "#0015BC",
                               "Dem"             = "#8bb1ff",
                               "Slightly\nDem"   = "lightblue1",
                               "GOP"             = "#ff8b98",
                               "Very\nGOP"       = "#E9141D")) +
    
  facet_wrap(~ subtype, nrow = 5, ncol = 1, strip.position = "right")
print(g_regional_curves_deaths)

################################################################################
### Draw the inset USA map 
################################################################################
county_map <- 
  county_laea %>%
  rename(fips = GEOID) %>%
  mutate(state_fips = substr(fips, 1, 2)) %>%
  filter(!state_fips %in% c("02", "15")) %>%
  mutate(fips = if_else(fips == "46113", "46102", fips)) %>%
  mutate(fips = if_else(fips == "02270", "02158", fips)) %>%
  left_join(pres_2020, by = "fips")

g_map_usa_regions_deaths <-
  ggplot(data = county_map) +
  theme_void() + 
  theme(legend.position = "none") +
  geom_sf(aes(fill = subtype), color = "black", size = 0) +
  
  scale_fill_manual(values = c("Very\nDem"     = "#0015BC",
                               "Dem"           = "#8bb1ff",
                               "Slightly\nDem" = "lightblue1",
                               "GOP"           = "#ff8b98",
                               "Very\nGOP"     = "#E9141D")) +
  geom_sf(data = state_laea %>% filter(!GEOID %in% c("02", "15")), size = 0.1, color = "black", fill = NA)
print(g_map_usa_regions_deaths)

################################################################################
### Draw the main graph (stacked line graph of cases)
################################################################################
caption <-
  paste("Data Source:  The New York Times\n",
        "Current as of ", 
        data %>% tail(n = 1) %>% pull("date") %>% format("%B %d, %Y"),
        sep = "")

total_deaths <- spreadsheet %>% filter(date == (spreadsheet %>% tail(n = 1) %>% pull("date"))) %>% pull("deaths") %>% sum()

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
        round(10000 * total_deaths / 327533795, 1),
        " per 10,000 people)\n",
        "Average ", growing_at, " new deaths/day last 7 days\n",
        up_rate_txt, " ", abs(up_rate), "% from 7 days ago",
        sep = "")

g_deaths_stacked <-
  ggplot(data = data, aes(x = as.Date(date), y = values, fill = subtype)) + 
  theme_linedraw() + 
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  geom_area(color="black", size = 0.4, alpha = 0.8) +
  scale_fill_manual(values = c("Very\nDem"       = "#0015BC",
                               "Dem"             = "#8bb1ff",
                               "Slightly\nDem"   = "lightblue1",
                               "GOP"             = "#ff8b98",
                               "Very\nGOP"       = "#E9141D")) +
  scale_y_continuous(labels = scales::comma) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(title = "Daily COVID-19 Deaths by winner of 2020 US Presidential Election binned by population quintile",
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
  group_by(date, subtype) %>%
  summarise(n = sum(values)) %>%
  mutate(percentage = n / sum(n)) %>%
  select(-n) %>%
  filter(date >= as.Date("2020-03-16"))
  
g_deaths_stacked_per <-
  ggplot(data = per_data, aes(x = as.Date(date), y = percentage, fill = subtype)) + 
  theme_linedraw() + 
  theme(legend.title = element_blank(),
        legend.position = "none",
        plot.background = element_rect(fill = "transparent",colour = NA)
  ) +
  
  geom_area(color="black", size = 0.4, alpha = .8) +
  scale_y_continuous(labels = scales::percent) + 
  geom_hline(yintercept = 0.5, linetype = "dashed") + 
  scale_fill_manual(values = c("Very\nDem"     = "#0015BC",
                               "Dem"           = "#8bb1ff",
                               "Slightly\nDem" = "lightblue1",
                               "GOP"           = "#ff8b98",
                               "Very\nGOP"     = "#E9141D")) +
  labs(title = "Proportion of National Daily Deaths", 
       subtitle = "Dotted line represents fraction of voters in respective parties",
       x = "", y = "")
print(g_deaths_stacked_per)

################################################################################
### Put it all together
################################################################################

g_final_deaths <-
  g_deaths_stacked + 
  annotation_custom(ggplotGrob(g_deaths_stacked_per), 
                    xmin = as.Date(data %>% head(n = 1) %>% pull("date")) + 170, 
                    xmax = as.Date(data %>% head(n = 1) %>% pull("date")) + 170 + 70, 
                    ymin = 1200,
                    ymax = 2200) +
  annotation_custom(ggplotGrob(g_map_usa_regions_deaths), 
                    xmin = as.Date(data %>% head(n = 1) %>% pull("date")) + 80,
                    xmax = as.Date(data %>% head(n = 1) %>% pull("date")) + 80 + 85, 
                    ymin = 1300,
                    ymax = 2300) + 
  
  annotation_custom(ggplotGrob(g_regional_curves_deaths), 
                    xmin = as.Date(data %>% head(n = 1) %>% pull("date")) - 10,
                    xmax = as.Date(data %>% head(n = 1) %>% pull("date")) - 10 + 33, 
                    ymin = 2200,
                    ymax = 150)
print(g_final_deaths)

