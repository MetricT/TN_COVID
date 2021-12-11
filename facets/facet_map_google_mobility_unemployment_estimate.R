### Make a facet map of US states with the number of new cases over time

library(tidyverse)
library(tidycensus)
library(cowplot)
library(TTR)
library(geofacet)
library(broom)

################################################################################
### Load the data...
################################################################################
google_mobility <- 
  "https://raw.githubusercontent.com/ActiveConclusion/COVID19_mobility/master/google_reports/mobility_report_US.csv" %>%
  read_csv()

google_us <-
  google_mobility %>% 
  filter(state != "Total" & county == "Total") %>% 
  select(state, date, workplaces) #%>% 
#  pivot_wider(id_cols = "date", names_from = "state", values_from = "workplaces")
#  mutate(value = value %>% ts() %>% mstl(frequency = 7) %>% trendcycle())
  
google_us_trend <-
  tribble(
    ~state, ~date, ~workplaces
  ) %>% 
  mutate(state = as.character(state),
         date = as.Date(date),
         workplaces = as.numeric(workplaces))

all_states <- google_us %>% select(state) %>% unique() %>% pull(state)

for (this_state in all_states) {
  
  print(paste("INFO:  Computing trend for", this_state))
  
  trend_model <-
    google_us %>% 
    filter(state == this_state) %>%
    select(date, workplaces) %>%
    arrange(date) %>%
    unique() %>%
    group_by(date) %>%
    summarize(workplaces = max(workplaces)) %>%
    ungroup() %>%
    unique() %>%
    na.locf() %>%
    as_tsibble(index = "date") %>%
    na.locf() %>%
    model(STL(workplaces ~ trend() + season(period = "1 week"))) %>%
    components() %>%
    select(date, trend) %>%
    as_tibble() %>%
    mutate(state = this_state) %>%
    rename(workplaces = trend) %>%
    select(state, date, workplaces)
  
  google_us_trend <-
    google_us_trend %>%
    bind_rows(trend_model)

}

g_facet <-
  ggplot(data = google_us_trend %>% filter(date >= as.Date("2021-08-01"))) +
  theme_bw() +
  geom_line(aes(x = as.Date(date), y = workplaces)) +
  facet_geo(~ state,scales = "free_y") +
  labs(title = "Google: visits to workplaces",
       caption = "", x = "", y = "")
print(g_facet)
