################################################################################
### Map of new people recovered by county
################################################################################
counties <-
  counties %>%
  mutate(new_recovered = if_else(new_recovered < 0, 0, new_recovered))

frac <- 0.5 * max(counties$new_recovered)
counties$textcolor = if_else(counties$new_recovered >= frac, "white", "black")

new_recovered_label  <- counties$new_recovered
new_recovered_label[new_recovered_label == 0] <- ""

map_new_recovered <-
  ggplot(counties) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.caption = element_text(hjust = 0.5)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  geom_sf(data = counties$geometry, size = geom_thickness,
          aes(fill = counties$new_recovered)) +
  geom_text(data = counties, size = map_fontsize, fontface = "bold",
            color = counties$textcolor,
            aes(x     = county_centers$x,
                y     = county_centers$y,
                label = new_recovered_label),
            nudge_x = counties$nudge_x,
            nudge_y = counties$nudge_y) +
  labs(title = "New Recovered") +
  scale_fill_gradientn(colours = map_palette, trans = "pseudo_log")
#print(map_new_recovered)
