################################################################################
### Graph:  COVID-19 Total active
################################################################################

library(geofacet)

this_counties <- c("Montgomery", "Robertson",  "Sumner",
                   "Cheatham",   "Davidson",   "Wilson",
                   "Dickson",    "Williamson", "Rutherford")

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
  filter(Date >= as.Date("2020-06-01"))

total_active <-
  total_active %>% 
  pivot_wider(id_cols = "Date",
              names_from = "County",
              values_from = "active_rate")

totact_title <- "Active COVID-19 Cases as % of County Population"

combined <-
  total_active# %>%
#  full_join(forecast_1_per, by = c("Date" = "date"))


my_grid <- data.frame(
  row = c( 1, 1, 1,
           2, 2, 2,
           3, 3, 3),
  col = c( 1, 2, 3,
           1, 2, 3,
           1, 2, 3),
  code = counties,
  name = counties,
  stringsAsFactors = FALSE
)

graph_total_active_county_percapita <-
  ggplot(data = combined, aes(x = as.Date(Date))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.7)) +
  theme(axis.text.x = element_text(hjust = 0.8)) +
  theme(legend.title = element_blank()) +

  geom_line(aes(y = Cheatham,   color = "Cheatham"), size = line_thickness) +
  geom_line(aes(y = Davidson,   color = "Davidson"), size = line_thickness) +
  geom_line(aes(y = Williamson, color = "Williamson"), size = line_thickness) +
  geom_line(aes(y = Rutherford, color = "Rutherford"), size = line_thickness) +
  geom_line(aes(y = Wilson,     color = "Wilson"), size = line_thickness) +
  geom_line(aes(y = Sumner,     color = "Sumner"), size = line_thickness) +

  labs(title = totact_title, x = "", y = "")
print(graph_total_active_county_percapita)
