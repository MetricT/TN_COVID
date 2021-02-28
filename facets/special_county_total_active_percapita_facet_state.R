################################################################################
### Graph:  COVID-19 Total active
################################################################################

library(geofacet)

this_counties <- 
  tn_pop_df %>% 
  as_tibble() %>% 
  #filter(!County %in% c("Lake")) %>%
  arrange("County") %>% 
  pull("County")

graph_color <- "steelblue2"

county_pop <-
  tn_pop_df %>%
  filter(County %in% this_counties) %>%
  select(County, POP2018)

total_active <-
  total_active_tib %>%
  select("Date", all_of(this_counties)) %>%
  pivot_longer(-Date) %>%
  left_join(county_pop, by = c("name" = "County")) %>%
  mutate(active_rate = 100 * value / POP2018) %>%
  rename(County = name) %>%
  select(Date, County, active_rate) %>%
  filter(Date >= as.Date("2020-09-07"))

totact_title <- "Active COVID-19 Cases as % of County Population"

combined <-
  total_active %>%
  ### So it matches the name on the geofacet grid...
  mutate(County = if_else(County == "Dekalb", "DeKalb", County))

graph_total_active_county_percapita <-
  ggplot(data = combined, aes(x = as.Date(Date), y = active_rate / 100)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.7)) +
  theme(axis.text.x = element_text(hjust = 0.8)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +

  geom_line(color = "darkseagreen4", size = line_thickness) +

  facet_geo(~ County, grid = "us_tn_counties_grid1") +

  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.015), breaks = c(0.000, 0.002, 0.004, 0.006, 0.008, 0.010, 0.012, 0.014)) +
  labs(title = totact_title, x = "", y = "")
print(graph_total_active_county_percapita)
