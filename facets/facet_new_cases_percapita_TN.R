### EpiEstim and other useful epidemic analysis packages available from:
###
### # https://www.repidemicsconsortium.org/

library(tidyverse)
library(tsibble)
library(feasts)
library(EpiEstim)
library(forecast)

### If you want to do every county in the state
my_county <- 
  new_cases_tib %>% 
  pivot_longer(-Date, names_to = "County", values_to = "Value") %>% 
  select(County) %>% 
  filter(!County %in% c("Total", "Pending", "Out of State")) %>% 
  mutate(County = ifelse(County == "Dekalb", "DeKalb", County)) %>%
  unique() %>% 
  pull()

facet_map <- 
  us_tn_counties_grid1 %>% 
  mutate(name = if_else(name == "Dekalb", "DeKalb", name),
         code = if_else(code == "Dekalb", "DeKalb", code))

data <-
  new_cases_tib %>%
  pivot_longer(-Date, names_to = "County", values_to = "New_Cases") %>%
  left_join(tn_pop_df, by = "County") %>%
  select(-GEOID) %>%
  mutate(County = ifelse(County == "Dekalb", "DeKalb", County)) %>%
  mutate(newcases_percapita = New_Cases / POP2018) %>%
  filter(!County %in% c("Total", "Pending", "Out of State")) %>% 
  filter(Date >= as.Date("2021-06-01"))

### Take the Rt_tib estimates and compute mstl() trends
trend_tib <-
  data %>%
  select(Date, County, newcases_percapita) %>%
  arrange(Date, County) %>%
  unique() %>%
  mutate(County = paste("values:", County, sep = "")) %>%
  pivot_wider(id_cols = "Date", 
              names_from = "County", 
              values_from = "newcases_percapita") %>%
  mutate_if(is.numeric, ~ (replace_na(., 0))) %>%
  mutate(across(starts_with("values:"),
                #.fns = list(sma = ~ SMA(., n = 7)),
                #.fns = list(sma = ~ c(rep(0,6), SMA(., n = 7))),
                .fns = list(trend = ~ (.) %>% ts() %>% mstl() %>% trendcycle()),
                .names = "{fn}_{col}")) %>%
  select("Date", starts_with("trend_values")) %>%
  rename_at(vars(starts_with("trend_values:")),
            ~ str_replace(., "trend_values:", "")) %>%
  pivot_longer(-Date, names_to = "County", values_to = "newcases_percapita_sma")

#scratchpad/scratchpad.R:                .fns = list(new = ~ c(0, diff(.))),
#Vandy/Vandy_Barchart.R:                .fns = list(sma = ~ SMA(., n = 7)),
# seasadj


### Add our trend data back into the original data
data <- data %>% left_join(trend_tib, by = c("Date" = "Date", "County" = "County")) 

### Title for our graph
title <- "New cases per capita, 7-day moving average"

### Render the graph and done!
g_new_cases_counties <-
  ggplot(data = data, aes(x = as.Date(Date))) +
  theme_linedraw() +
  theme(strip.text.x = element_text(size = 12)) +
  geom_area(aes(y = newcases_percapita_sma), color = "darkseagreen4", size = 1.2) +
  facet_geo(~ County, grid = facet_map) + 
  labs(title = title, x = "", y = "Rt")
print(g_new_cases_counties)
