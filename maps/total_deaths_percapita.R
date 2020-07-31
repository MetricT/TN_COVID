################################################################################
### Map of total deaths per 100k by county
################################################################################
scale     <- 100000
scale_txt <- "100k"

counties$total_deaths_percapita <- round(scale * counties$total_deaths / counties$POP2018, 3)
total_deaths_percapita_label <- round(counties$total_deaths_percapita, 1)
total_deaths_percapita_label[total_deaths_percapita_label == 0] <- ""

num <- ((counties$total_deaths %>% na.omit() %>% sum()) / (counties$POP2018 %>% na.omit() %>% sum()) * scale) %>% round(1)

### By default use black for the font, but if the value is over 1/2's of the way
### to the max, use the white font instead as it looks nicer
frac <- 0.5 * (counties$total_deaths_percapita %>% na.omit() %>% max())
counties$textcolor = if_else(counties$total_deaths_percapita >= frac, "white", "black")

map_total_deaths_percapita <- ggplot(counties) +
  theme_void() +
  theme(legend.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") +
  geom_sf(data = counties$geometry, aes(fill = counties$total_deaths_percapita), size = geom_thickness) +
  geom_text(data = counties, size = map_fontsize, fontface = "bold",
            color = counties$textcolor,
            aes(x     = county_centers$x,
                y     = county_centers$y,
                label = total_deaths_percapita_label),
            nudge_x = counties$nudge_x,
            nudge_y = counties$nudge_y) +
  labs(title = paste("Total Deaths per ", scale_txt, ": ", num, sep = "")) +
  scale_fill_gradientn(colours = map_palette)
print(map_total_deaths_percapita)