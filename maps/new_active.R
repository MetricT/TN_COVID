################################################################################
### Map of new people active (infected but not dead or recovered) by county
################################################################################
frac <- 0.5 * max(counties$new_active)
counties$textcolor = if_else(counties$new_active >= frac, "white", "black")

new_active_label_per  <- counties$new_active
new_active_label_per[new_active_label_per == 0] <- ""

tps_title <- "New Active Cases:"

### Fix a rendering issue, we want "0 active counties" to have the grey
### color fill and not green/white/red
null_counties <-
  counties %>%
  filter(new_active == 0)

map_new_active <-
  ggplot(counties) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.caption = element_text(hjust = 0.5)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  geom_sf(data = counties$geometry, size = geom_thickness,
          aes(fill = counties$new_active)) +
  geom_sf(data = null_counties$geometry, size = geom_thickness, fill = "#7f7f7f") +

  geom_text(data = counties, size = map_fontsize, fontface = "bold",
            color = counties$textcolor,
            aes(x     = county_centers$x,
                y     = county_centers$y,
                label = new_active_label_per),
            nudge_x = counties$nudge_x,
            nudge_y = counties$nudge_y) +
  labs(title = tps_title) +
  #scale_fill_gradientn(colours = map_palette)

  scale_fill_gradient2(midpoint = 0,
                       trans = "pseudo_log",
                       low  = "darkgreen",
                       mid  = "white",
                       high = "darkred")
#print(map_new_active)
