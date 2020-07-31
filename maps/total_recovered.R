################################################################################
### Map of total people recovered by county
################################################################################
frac <- 0.5 * max(counties$total_recovered)
counties$textcolor = if_else(counties$total_recovered >= frac, "white", "black")

total_recovered_label  <- counties$total_recovered
total_recovered_label[total_recovered_label == 0] <- ""

map_total_recovered <- ggplot(counties) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.caption = element_text(hjust = 0.5)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  geom_sf(data = counties$geometry, size = geom_thickness,
          aes(fill = counties$total_recovered)) +
  geom_text(data = counties, size = map_fontsize, fontface = "bold",
            color = counties$textcolor,
            aes(x     = county_centers$x,
                y     = county_centers$y,
                label = total_recovered_label),
            nudge_x = counties$nudge_x,
            nudge_y = counties$nudge_y) +
  labs(title = "Total Recovered") +
  scale_fill_gradientn(colours = map_palette, trans = "pseudo_log")
#print(map_total_recovered)
