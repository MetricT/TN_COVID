################################################################################
### Map of total active per capita the last 7 days by county
################################################################################
scale     <- 100
scale_txt <- "%"

total_active_percapita_last7 <-
  total_active_tib %>%
  tail(n = 1) %>%
  select(-Date, -Total) %>%
  gather() %>%
  group_by(key) %>%
  summarize(total_active_last7 = sum(value)) %>%
  rename(County = key)

this_map <-
  county_acs %>%
  mutate(NAME = str_replace(NAME, " County, Tennessee", "")) %>%
  select(GEOID, NAME, POP2018) %>%
  rename(County = NAME) %>%
  left_join(total_active_percapita_last7, by = "County") %>%
  mutate(total_active_percapita_last7 = total_active_last7 / POP2018)

total_active_percapita_last7_label <-
  (scale * this_map$total_active_percapita_last7) %>% round(2)
total_active_percapita_last7_label[total_active_percapita_last7_label == 0] <- ""

### By default use black for the font, but if the value is over 1/2's of the way
### to the max, use the white font instead as it looks nicer
frac <- 0.5 * max(this_map$total_active_percapita_last7)
this_map$textcolor = if_else(this_map$total_active_percapita_last7 > frac, "white", "black")

map_total_active_percapita_last7 <-
  ggplot(this_map) +
  theme_void() +
  theme(legend.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") +
  geom_sf(data = this_map$geometry, aes(fill = this_map$total_active_percapita_last7), size = geom_thickness) +
  geom_text(data = this_map, size = map_fontsize, fontface = "bold",
            color = this_map$textcolor,
            aes(x     = county_centers$x,
                y     = county_centers$y,
                label = total_active_percapita_last7_label),
            nudge_x = this_map$nudge_x,
            nudge_y = this_map$nudge_y) +
  labs(title = "Active Cases as a % of population") +
#  labs(title = paste("total Active Per ", scale_txt, " Last 7 Days", sep = "")) +
  scale_fill_gradient2(midpoint = 0,
                       trans = "pseudo_log",
                       low  = "darkgreen",
                       mid  = "white",
                       high = "darkred")
print(map_total_active_percapita_last7)
