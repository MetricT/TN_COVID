################################################################################
### Map of total COVID-19 cases
################################################################################

### We want to add text with the number of cases in each county to that
### county on the map.   But we *don't* want to label counties that don't yet
### have any confirmed cases.
total_cases_label <- counties$total_cases
total_cases_label[total_cases_label == 0] <- ""

### By default use black for the font, but if the value is over 1/2's of the way
### to the max, use the white font instead as it looks nicer
frac <- 0.5 * max(counties$total_cases)
counties$textcolor = if_else(counties$total_cases >= frac, "white", "black")

### Map:  COVID-19 confirmed cases per county
map_total_cases <-
  ggplot(counties) +
  theme_void() +
  theme(legend.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") +
  geom_sf(data = counties$geometry, aes(fill = counties$total_cases), size = geom_thickness) +
  geom_text(data = counties, size = map_fontsize, fontface = "bold",
            color = counties$textcolor,
            aes(x     = county_centers$x,
                y     = county_centers$y,
                label = total_cases_label),
            nudge_x = counties$nudge_x,
            nudge_y = counties$nudge_y) +
  labs(title = "Total Cases") +
  scale_fill_gradientn(colours = map_palette)
print(map_total_cases)
