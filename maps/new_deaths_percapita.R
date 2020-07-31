################################################################################
### Map of new deaths per capita
################################################################################

counties$new_deaths_percapita <- round(100000 * counties$new_deaths / counties$POP2018, 3)
new_deaths_percapita_label <- round(counties$new_deaths_percapita, 1)
new_deaths_percapita_label[new_deaths_percapita_label == 0] <- ""

### By default use black for the font, but if the value is over 1/2's of the way
### to the max, use the white font instead as it looks nicer
frac <- 0.5 * (counties$new_deaths_percapita %>% na.omit() %>% max())
counties$textcolor = if_else(counties$new_deaths_percapita >= frac, "white", "black")

map_new_deaths_percapita <- ggplot(counties) +
  theme_void() +
  theme(legend.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") +
  geom_sf(data = counties$geometry, aes(fill = counties$new_deaths_percapita), size = geom_thickness) +
  geom_text(data = counties, size = map_fontsize, fontface = "bold",
            color = counties$textcolor,
            aes(x     = county_centers$x,
                y     = county_centers$y,
                label = new_deaths_percapita_label),
            nudge_x = counties$nudge_x,
            nudge_y = counties$nudge_y) +
  labs(title = "New Deaths per 100k") +
  scale_fill_gradientn(colours = map_palette)
#print(map_new_deaths_percapita)
