################################################################################
### Map of total active per capita the last 7 days by county
################################################################################
scale     <- 100
scale_txt <- "%"

counties <- 
  counties %>% 
  select(-starts_with("total_active_percapita_last7"))

total_active_tn <- total_active_tib %>% tail(n = 7) %>% pull("Total") %>% mean()
total_pop_tn    <- county_acs %>% pull("POP2018") %>% sum()

total_active_percapita <- total_active_tn / total_pop_tn

total_active_percapita_last7 <-
  total_active_tib %>%
  tail(n = 7) %>%
  select(-Date, -Total) %>%
  gather() %>%
  group_by(key) %>%
  summarize(total_active_last7 = mean(value)) %>%
  rename(County = key)

this_map <-
  county_acs %>%
  mutate(NAME = str_replace(NAME, " County, Tennessee", "")) %>%
  select(GEOID, NAME, POP2018) %>%
  rename(County = NAME) %>%
  mutate(County = if_else(County == "DeKalb", "Dekalb", County)) %>%
  left_join(total_active_percapita_last7, by = "County") %>%
  mutate(total_active_percapita_last7 = total_active_last7 / POP2018)

total_active_percapita_last7_label <-
  (scale * this_map$total_active_percapita_last7) %>% round(2)
total_active_percapita_last7_label[total_active_percapita_last7_label == 0] <- "0"

### By default use black for the font, but if the value is more than 1.5 standard deviations
### from the mean, use white instead
mean <- this_map$total_active_percapita_last7 %>% na.omit() %>% mean()
sd   <- this_map$total_active_percapita_last7 %>% na.omit() %>% sd()
frac_max <- mean + 1.5 * sd
frac_min <- mean - 1.5 * sd
this_map$textcolor = if_else(this_map$total_active_percapita_last7 > frac_min & this_map$total_active_percapita_last7 < frac_max, "black", "white")

map_total_active_percapita_last7 <-
  ggplot(data = this_map) +
  theme_void() +
  theme(legend.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") +
  geom_sf(aes(fill = total_active_percapita_last7), size = geom_thickness) +
  geom_text(size = map_fontsize, fontface = "bold",
            color = this_map$textcolor,
            aes(x     = county_centers$x,
                y     = county_centers$y,
                label = total_active_percapita_last7_label),
            nudge_x = this_map$nudge_x,
            nudge_y = this_map$nudge_y) +
  labs(title = paste("Average Total Active as a % of population Last 7 Days - ", round(100 * total_active_percapita, digits = 2), "%")) +
  scale_fill_gradient2(midpoint = total_active_percapita,
                       low  = "darkgreen",
                       mid  = "white",
                       high = "darkred")
print(map_total_active_percapita_last7)