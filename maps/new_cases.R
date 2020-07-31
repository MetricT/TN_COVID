################################################################################
### Map of new COVID-19 cases
################################################################################

num <- 
  new_cases_tib %>% 
  tail(n = 1) %>% 
  pull("Total") %>%
  format(big.mark = ",", scientific = FALSE)

### We want to add text with the number of cases in each county to that
### county on the map.   But we *don't* want to label counties that don't yet
### have any confirmed cases.
new_cases_label <- counties$new_cases
new_cases_label[new_cases_label <= 0] <- ""

### By default use black for the font, but if the value is over 1/2's of the way
### to the max, use the white font instead as it looks nicer
frac <- 0.1 * (counties$new_cases %>% na.omit() %>% max())
counties$textcolor = if_else(counties$new_cases >= frac, "white", "black")
counties$new_cases[counties$new_cases < 0] <- 0

### Map:  COVID-19 confirmed new cases per county
map_new_cases <-
  ggplot(counties) +
  theme_void() +
  theme(legend.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") +
  geom_sf(data = counties$geometry, aes(fill = counties$new_cases), size = geom_thickness) +
  geom_text(data = counties, size = map_fontsize, fontface = "bold",
            color = counties$textcolor,
            aes(x     = county_centers$x,
                y     = county_centers$y,
                label = new_cases_label),
            nudge_x = counties$nudge_x,
            nudge_y = counties$nudge_y) +
  labs(title = paste("New Cases: ", num, sep = "")) +
  scale_fill_gradientn(colours = map_palette, trans = "pseudo_log")
print(map_new_cases)
