################################################################################
### Map of total deaths by county
################################################################################

total_deaths_label <- counties$total_deaths
total_deaths_label[total_deaths_label == 0] <- ""

### By default use black for the font, but if the value is over 1/2's of the way
### to the max, use the white font instead as it looks nicer
frac <- 0.5 * max(counties$total_deaths)
counties$textcolor = if_else(counties$total_deaths >= frac, "white", "black")

map_total_deaths <- ggplot(counties) +
  theme_void() +
  theme(legend.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") +
  geom_sf(data = counties$geometry, aes(fill = counties$total_deaths), size = geom_thickness) +
  geom_text(data = counties, size = map_fontsize, fontface = "bold",
            color = counties$textcolor,
            aes(x     = county_centers$x,
                y     = county_centers$y,
                label = total_deaths_label),
            nudge_x = counties$nudge_x,
            nudge_y = counties$nudge_y) +
  labs(title = "Total Deaths") +
  scale_fill_gradientn(colours = map_palette)
print(map_total_deaths)