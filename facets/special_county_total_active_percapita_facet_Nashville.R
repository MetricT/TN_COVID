################################################################################
### Graph:  COVID-19 Total active
################################################################################

library(tidyverse)
library(geofacet)

counties <- c("Montgomery", "Robertson",  "Sumner",
              "Cheatham",   "Davidson",   "Wilson",
              "Dickson",    "Williamson", "Rutherford")

#counties <- county_names

mandate_df <- 
  read_csv("data/mandates.csv", col_names = TRUE, col_types = "cDc") %>%
  mutate(woy = week(effective_date)) %>%
  filter(county %in% counties) %>%
  filter(county != "Robertson")

#graph_color <- "darkred"
graph_color <- "darkseagreen4"

county_pop <-
  tn_pop_df %>%
  filter(County %in% counties) %>%
  select(County, POP2018)

total_active <-
  total_active_tib %>%
  select("Date", counties) %>%
  pivot_longer(-Date) %>%
  left_join(county_pop, by = c("name" = "County")) %>%
  left_join(mandate_df, by = c("name" = "county")) %>%
  mutate(active_rate = 100 * value / POP2018) %>%
  rename(County = name) %>%
#  select(Date, County, active_rate) %>%
  filter(Date >= as.Date("2020-06-01"))

totact_title <- "Active COVID-19 Cases as % of County Population"
#####\nwith dotted line showing date mask mandate for general public (if any) comes into effect"

combined <-
  total_active# %>%
#  full_join(forecast_1_per, by = c("Date" = "date"))


#my_grid <- data.frame(
#  row = c( 1, 1, 1,
#           2, 2, 2,
#           3, 3, 3),
#  col = c( 1, 2, 3,
#           1, 2, 3,
#           1, 2, 3),
#  code = counties,
#  name = counties,
#  stringsAsFactors = FALSE
#)

#combined <-
#  combined %>%
#  mutate(County = paste(County, " Co., Tennessee", sep = ""))

graph_total_active_county_percapita <-
  ggplot(data = combined, aes(x = as.Date(Date), y = active_rate / 100)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.7)) +
  theme(axis.text.x = element_text(hjust = 0.8)) +
  theme(strip.text.x = element_text(size = 16)) +
  theme(legend.title = element_blank()) +
  theme(strip.text.x = element_text(size = 16, colour = "white")) +
  theme(strip.background = element_rect(fill = "black"))+
  theme(legend.position = "none") +
  
  geom_vline(mapping = aes(xintercept = as.Date(effective_date)), linetype = "dotted") + 
  geom_vline(xintercept = as.Date("2020-07-19"), linetype = "dashed") + 
  
  
  geom_line(color = graph_color, size = line_thickness) +

  facet_wrap(~ County) +#, grid = my_grid) +

  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous(labels = scales::percent, breaks = c(0.000, 0.002, 0.004, 0.006, 0.008, 0.010), limits = c(0, 0.015)) +
  labs(title = totact_title, x = "", y = "")
print(graph_total_active_county_percapita)
