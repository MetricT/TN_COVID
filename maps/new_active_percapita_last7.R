################################################################################
### Map of new active per capita the last 7 days by county
################################################################################
scale     <- 1000
scale_txt <- "1k"

counties <- 
  counties %>% 
  select(-starts_with("new_active_percapita_last7"))

new_active_percapita_last7 <-
  new_active_tib %>%
  tail(n = 7) %>%
  select(-Date, -Total) %>%
  gather() %>%
  group_by(key) %>%
  summarize(new_active_last7 = sum(value)) %>%
  rename(County = key)

this_map <-
  county_acs %>%
  mutate(NAME = str_replace(NAME, " County, Tennessee", "")) %>%
  select(GEOID, NAME, POP2018) %>%
  rename(County = NAME) %>%
  mutate(County = if_else(County == "DeKalb", "Dekalb", County)) %>%
  left_join(new_active_percapita_last7, by = "County") %>%
  mutate(new_active_percapita_last7 = new_active_last7 / POP2018)

new_active_percapita_last7_label <-
  (scale * this_map$new_active_percapita_last7) %>% round(2)
new_active_percapita_last7_label[new_active_percapita_last7_label == 0] <- "0"

### By default use black for the font, but if the value is over 1/2's of the way
### to the max, use the white font instead as it looks nicer
frac <- 0.5 * (this_map$new_active_percapita_last7 %>% na.omit() %>% max())
this_map$textcolor = if_else(this_map$new_active_percapita_last7 > frac, "white", "black")

### Grey out counties that haven't shown any new active in the 7 day period
null_counties <- counties %>% filter(new_active_last7 == 0)

map_new_active_percapita_last7 <-
  ggplot(this_map) +
  theme_void() +
  theme(legend.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") +
  geom_sf(data = this_map$geometry, aes(fill = this_map$new_active_percapita_last7), size = geom_thickness) +
  geom_sf(data = null_counties$geometry, fill = "#7f7f7f") +
  geom_text(data = this_map, size = map_fontsize, fontface = "bold",
            color = this_map$textcolor,
            aes(x     = county_centers$x,
                y     = county_centers$y,
                label = new_active_percapita_last7_label),
            nudge_x = this_map$nudge_x,
            nudge_y = this_map$nudge_y) +
  labs(title = paste("New Active Per ", scale_txt, " Last 7 Days, [ ",
                     new_active_tib %>% tail(n = 1) %>% pull("Date"), " ]", sep = "")) +
  scale_fill_gradient2(midpoint = 0,
#                       trans = "pseudo_log",
                       low  = "darkgreen",
                       mid  = "white",
                       high = "darkred")
print(map_new_active_percapita_last7)
