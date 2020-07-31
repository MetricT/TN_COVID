################################################################################
### Map of new cases per capita the last 14 days by county
################################################################################
scale     <- 100000
scale_txt <- "100k"

new_cases_percapita_last14 <-
  new_cases_tib %>%
  tail(n = 14) %>%
  select(-Date, -Total) %>%
  gather() %>%
  group_by(key) %>%
  summarize(new_cases_last14 = mean(value)) %>%
  rename(County = key) %>%
  mutate(
    color_code = case_when(
      new_cases_last14 <= 1.0 ~ "#3e87b5",
      new_cases_last14 >  1.0 ~ "#ce703a",
    ))

this_map <-
  county_acs %>%
  mutate(NAME = str_replace(NAME, " County, Tennessee", "")) %>%
  select(GEOID, NAME, POP2018) %>%
  rename(County = NAME) %>%
  left_join(new_cases_percapita_last14, by = "County") %>%
  mutate(new_cases_percapita_last14 = new_cases_last14 / POP2018)

new_cases_percapita_last14_label <-
  (scale * this_map$new_cases_percapita_last14) %>% round(2)
new_cases_percapita_last14_label[new_cases_percapita_last14_label == 0] <- ""

### By default use black for the font, but if the value is over 1/2's of the way
### to the max, use the white font instead as it looks nicer
frac <- 0.5 * max(this_map$new_cases_percapita_last14)
this_map$textcolor = if_else(this_map$new_cases_percapita_last14 > frac, "white", "black")

map_new_cases_percapita_last14 <-
  ggplot(this_map) +
  theme_void() +
  theme(legend.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") +
  geom_sf(data = this_map$geometry, aes(fill = this_map$new_cases_percapita_last14), size = geom_thickness) +
  geom_text(data = this_map, size = map_fontsize, fontface = "bold",
            color = this_map$textcolor,
            aes(x     = county_centers$x,
                y     = county_centers$y,
                label = new_cases_percapita_last14_label),
            nudge_x = this_map$nudge_x,
            nudge_y = this_map$nudge_y) +
  labs(title = paste("New cases Per ", scale_txt, " Last 14 Days", sep = "")) +
  scale_fill_gradient2(midpoint = 0,
                       trans = "pseudo_log",
                       low  = "darkgreen",
                       mid  = "white",
                       high = "darkred")
#print(map_new_cases_percapita_last14)
