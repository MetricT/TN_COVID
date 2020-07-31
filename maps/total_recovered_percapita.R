################################################################################
### Map of total people recovered by county
################################################################################
counties$total_recovered_percapita <- round(100 * counties$total_recovered / counties$POP2018, 2)

frac <- 0.5 * max(counties$total_recovered_percapita)
counties$textcolor = if_else(counties$total_recovered_percapita >= frac, "white", "black")

total_recovered_percapita_label  <- counties$total_recovered_percapita
total_recovered_percapita_label[total_recovered_percapita_label == 0] <- ""

map_total_recovered_percapita <- 
  ggplot(counties) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.caption = element_text(hjust = 0.5)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  geom_sf(data = counties$geometry, size = geom_thickness,
          aes(fill = counties$total_recovered_percapita)) +
  geom_text(data = counties, size = map_fontsize, fontface = "bold",
            color = counties$textcolor,
            aes(x     = county_centers$x,
                y     = county_centers$y,
                label = total_recovered_percapita_label),
            nudge_x = counties$nudge_x,
            nudge_y = counties$nudge_y) +
  labs(title = "Total Recovered Percent of Population") +
  scale_fill_gradientn(colours = map_palette, trans = "pseudo_log")
print(map_total_recovered_percapita)