################################################################################
### Map of new cases per capita the last <num_days> days by county
################################################################################

num_days <- 7

scale     <- 100000
scale_txt <- "100k"

new_cases_percapita_last_n_days <-
  new_cases_tib %>%
  tail(n = num_days) %>%
  select(-Date, -Total) %>%
  gather() %>%
  group_by(key) %>%
  summarize(new_cases_last_n_days = mean(value)) %>%
  rename(County = key) %>%
  mutate(
    color_code = case_when(
      new_cases_last_n_days <= 1.0 ~ "#3e87b5",
      new_cases_last_n_days >  1.0 ~ "#ce703a",
    ))

this_map <-
  county_acs %>%
  mutate(NAME = str_replace(NAME, " County, Tennessee", "")) %>%
  mutate(NAME = if_else(NAME == "DeKalb", "Dekalb", NAME)) %>%
  select(GEOID, NAME, POP2018) %>%
  rename(County = NAME) %>%
  left_join(new_cases_percapita_last_n_days, by = "County") %>%
  mutate(new_cases_percapita_last_n_days = new_cases_last_n_days / POP2018)

new_cases_percapita_last_n_days_label <-
  (scale * this_map$new_cases_percapita_last_n_days) %>% round(2)
new_cases_percapita_last_n_days_label[new_cases_percapita_last_n_days_label == 0] <- ""

### By default use black for the font, but if the value is over 1/2's of the way
### to the max, use the white font instead as it looks nicer
frac <- 0.5 * (this_map %>% st_drop_geometry() %>% select(new_cases_percapita_last_n_days) %>% filter(!is.na(new_cases_percapita_last_n_days)) %>% pull() %>% max())
this_map$textcolor = if_else(this_map$new_cases_percapita_last_n_days > frac, "white", "black")

map_new_cases_percapita_last_n_days <-
  ggplot(this_map) +
  theme_void() +
  theme(legend.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") +
  geom_sf(data = this_map$geometry, aes(fill = this_map$new_cases_percapita_last_n_days), size = geom_thickness) +
  geom_text(data = this_map, size = map_fontsize, fontface = "bold",
            color = this_map$textcolor,
            aes(x     = county_centers$x,
                y     = county_centers$y,
                label = new_cases_percapita_last_n_days_label),
            nudge_x = this_map$nudge_x,
            nudge_y = this_map$nudge_y) +
  labs(title = paste("New cases Per ", scale_txt, " Last ", num_days, " Days", sep = "")) +
  scale_fill_gradient2(midpoint = 0,
                       trans = "pseudo_log",
                       low  = "darkgreen",
                       mid  = "white",
                       high = "darkred")
print(map_new_cases_percapita_last_n_days)