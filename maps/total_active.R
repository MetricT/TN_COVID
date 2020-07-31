################################################################################
### Map of total people active (infected but not dead or recovered) by county
################################################################################
frac <- 0.5 * max(counties$total_active)
counties$textcolor = if_else(counties$total_active >= frac, "white", "black")

total_active_label_per  <- counties$total_active
total_active_label_per[total_active_label_per == 0] <- ""

tps_title <- "Total Active Cases:"

map_total_active <-
  ggplot(counties) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.caption = element_text(hjust = 0.5)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  geom_sf(data = counties$geometry, size = geom_thickness,
          aes(fill = counties$total_active)) +
  geom_text(data = counties, size = map_fontsize, fontface = "bold",
            color = counties$textcolor,
            aes(x     = county_centers$x,
                y     = county_centers$y,
                label = total_active_label_per),
            nudge_x = counties$nudge_x,
            nudge_y = counties$nudge_y) +
  labs(title = tps_title) +
  scale_fill_gradientn(colours = map_palette, trans = "pseudo_log")
#print(map_total_active)
