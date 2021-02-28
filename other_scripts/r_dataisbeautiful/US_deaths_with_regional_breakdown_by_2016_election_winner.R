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

# Remove these territories from our data
remove_states <- 
  c("District of Columbia", 
    "Puerto Rico", 
    "Guam", 
    "Virgin Islands", 
    "Northern Mariana Islands")

# Load the spreadsheet (which contains total cases/deaths) and compute
# new cases/deaths
spreadsheet <- 
  "../Datasets/nytimes/covid-19-data/us-states.csv" %>%
  read_csv(col_names = TRUE, col_types = "Dccdd") %>%
  filter(!state %in% remove_states) %>%

  ### A special one for New Jersey...
  mutate(cases = if_else(state == "New Jersey" & date >= as.Date("2020-06-25"), cases - 1877, cases)) %>%
  mutate(deaths = if_else(state == "New Jersey" & date >= as.Date("2020-06-25"), deaths - 1877, deaths)) %>%
    
  ### And two for New York...
  mutate(cases = if_else(state == "New York" & date >= as.Date("2020-05-06"), cases - 694, cases)) %>%
  mutate(cases = if_else(state == "New York" & date >= as.Date("2020-06-30"), cases - 610, cases)) %>%
  mutate(deaths = if_else(state == "New York" & date >= as.Date("2020-05-06"), deaths - 694, deaths)) %>%
  mutate(deaths = if_else(state == "New York" & date >= as.Date("2020-06-30"), deaths - 610, deaths)) %>%  

  ### Gotta mess with Texas...
  mutate(deaths = if_else(state == "Texas"    & date >= as.Date("2020-07-27"), deaths - 1007, deaths)) %>%
  
  select(date, state, cases, deaths) %>%
  arrange(date) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              names_sep = ":",
              values_from = c(cases, deaths)) %>% 
  mutate_if(is.numeric, ~ (replace_na(., 0))) %>%
  mutate(across(-date, ~ . - lag(.))) %>%
  mutate_if(is.numeric, ~ (replace_na(., 0))) %>%
  mutate("cases:Washington" = if_else(date == as.Date("2020-01-21"), 1, `cases:Washington`)) %>%
  pivot_longer(-date, names_to = c("type", "state"), names_sep = ":", values_to = "values") %>%
  pivot_wider(id_cols = c("date", "state"), names_from = "type", values_from = "values")

### Pluck the date out to include in our graph
current_date <- spreadsheet %>% arrange(date) %>% tail(n = 1) %>% pull("date")

### Load a map of the 2016 Presidential election results
pres_2016 <- 
  politicaldata::pres_results %>%
  filter(year %in% c(2016)) %>%
  rowwise() %>%
  mutate(winner = max(dem, rep, other)) %>%
  ungroup() %>%
  mutate(winner = case_when(
    winner == dem ~ "Democrat",
    winner == rep ~ "Republican",
    winner == other ~ "Other"
  )) %>%
  mutate(margin = rep - dem) %>%
  mutate(color = if_else(winner == "Democrat", "#0015BC", "#E9141D")) %>%
  left_join((fips_codes %>% select(state, state_name) %>% unique() %>% as_tibble()), by = c("state" = "state")) %>%
  select(-state) %>%
  rename(state = state_name)

population <-
  get_acs(geography   = "state",
          variables   = c("B01003_001"),
          year        = 2016,
          geometry    = FALSE,
          cache_table = TRUE) %>%
  rename(population = estimate) %>%
  select(NAME, population)

pres_2016 <- 
  pres_2016 %>% 
  left_join(population, by = c("state"= "NAME")) %>%
  mutate(voter_turnout = total_votes / population) 

pres_2016 <-
  pres_2016 %>% 
  arrange(desc(margin)) %>% 
  mutate(cum_population = cumsum(population), 
         percent = population / 318558162, 
         cum_percent = cum_population / 318558162) %>% 
  mutate(subtype = case_when(
    cum_percent < 0.2 ~ "Very\nGOP", 
    cum_percent < 0.4 ~ "GOP", 
    cum_percent < 0.6 ~ "Swing\nStates", 
    cum_percent < 0.8 ~ "Dem", 
    cum_percent <= 1  ~ "Very\nDem"))

################################################################################
### Take our spreadsheet data, filter it a bit (New Jersey uglied up the graph 
### a few weeks ago), and compute a 7-day simple moving average of the data
################################################################################
data <-
  spreadsheet %>%
  left_join(pres_2016, by = "state") %>%
  group_by(date, subtype) %>%
  summarize(cases = sum(cases, na.rm = TRUE),
            deaths = sum(deaths, na.rm = TRUE)) %>%
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
  mutate_if(is.numeric, ~ (replace_na(., 0))) 

################################################################################
### Draw the inset "Regional Curves" graph
################################################################################
#data$winner  <- factor(data$winner,  levels = c("Democrat", "Republican"))
data$subtype <- factor(data$subtype, levels = c("Very\nGOP", "GOP", "Swing\nStates", "Dem", "Very\nDem"))

g_regional_curves_deaths <-
  ggplot(data = data, aes(x = as.Date(date), y = values)) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(strip.background = element_blank()) +

  geom_line(aes(y = values), color = "black", size = 1) +
  geom_area(aes(fill = as.factor(subtype))) +
  
  labs(title = "Deaths by Political Lean", x = "", y = "") +
  scale_y_continuous(labels = scales::comma) +
  
  scale_fill_manual(values = c("Very\nDem"   = "#0015BC",
                               "Dem"   = "#8bb1ff",
                               "Swing\nStates" = "#FFFFFF",
                               "GOP" = "#ff8b98",
                               "Very\nGOP" = "#E9141D")) +
    
  facet_wrap(~ subtype, nrow = 5, ncol = 1, strip.position = "right")
print(g_regional_curves_deaths)

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
  left_join(pres_2016, by = c("state_name" = "state"))

g_map_usa_regions_deaths <-
  ggplot(data = state_map) +
  theme_void() + 
  theme(legend.position = "none") +
  
  
  geom_sf(data = state_map %>% filter(subtype == "Swing\nStates"),
          aes(color = winner), size = 1.5, fill = "NA") + 
  
  scale_color_manual(values = c("Democrat"   = "#0015BC",
                                "Republican" = "#E9141D")) +
  
  geom_sf(aes(fill = subtype), color = "black", size = 0.4) +
  
  scale_fill_manual(values = c("Very\nDem"   = "#0015BC",
                               "Dem"   = "#8bb1ff",
                               "Swing\nStates" = NA, #"goldenrod2",
                               "GOP" = "#ff8b98",
                               "Very\nGOP" = "#E9141D"))
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
  tail(n = 10) %>% 
  mutate(new_values = c(0, diff(deaths))) %>% 
  tail(n = 7) %>% 
  pull(new_values) %>% 
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
  scale_fill_manual(values = c("Very\nDem"   = "#0015BC",
                               "Dem"   = "#8bb1ff",
                               "Swing\nStates" = NA, #"goldenrod2",
                               "GOP" = "#ff8b98",
                               "Very\nGOP" = "#E9141D")) +
  scale_y_continuous(labels = scales::comma) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(title = "Daily COVID-19 Deaths by winner of 2016 US Presidential Election binned by population quintile",
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
  scale_fill_manual(values = c("Very\nDem"   = "#0015BC",
                               "Dem"   = "#8bb1ff",
                               "Swing\nStates" = NA, #"goldenrod2",
                               "GOP" = "#ff8b98",
                               "Very\nGOP" = "#E9141D")) +
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
                    xmin = as.Date(data %>% tail(n = 1) %>% pull("date")) -70 - 60, 
                    xmax = as.Date(data %>% tail(n = 1) %>% pull("date")) -70, 
                    ymin = 1100,
                    ymax = 2100) +
  annotation_custom(ggplotGrob(g_map_usa_regions_deaths), 
                    xmin = as.Date(data %>% head(n = 1) %>% pull("date")) - 10,
                    xmax = as.Date(data %>% head(n = 1) %>% pull("date")) - 10 + 45, 
                    ymin = 1400,
                    ymax = 2100) + 
  
  annotation_custom(ggplotGrob(g_regional_curves_deaths), 
                    xmin = as.Date(data %>% head(n = 1) %>% pull("date")) - 10,
                    xmax = as.Date(data %>% head(n = 1) %>% pull("date")) - 10 + 33, 
                    ymin = 100,
                    ymax = 1050)
print(g_final_deaths)

#plot_grid(g_final_cases,
#        g_final_deaths,
#        nrow = 2, ncol = 1 , align = "hv")

