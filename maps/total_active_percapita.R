################################################################################
### Map of total active cases per capita
################################################################################
scale     <- 100
scale_txt <- "%"

counties <-
  counties %>%
  mutate(total_active_percapita =
           round(scale * total_active / POP2018, 2)) %>%
  mutate(
    color_code = case_when(
      total_active_percapita <  0.5 ~ "green",
      total_active_percapita >= 0.5 & total_active_percapita <  1.0 ~ "yellow",
      total_active_percapita >= 1.0 ~ "red",
    ))

num <- ((counties$total_active %>% na.omit() %>% sum()) / (counties$POP2018 %>% na.omit() %>% sum()) * scale) %>% round(2)

frac <- 0.5 * (counties$total_active_percapita %>% na.omit() %>% max())
counties$textcolor = if_else(counties$total_active_percapita >= frac, "white", "black")

total_active_percapita_label  <- counties$total_active_percapita
total_active_percapita_label[total_active_percapita_label == 0] <- ""

map_total_active_percapita <-
  ggplot(counties) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.caption = element_text(hjust = 0.5)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  geom_sf(data = counties$geometry, size = geom_thickness,
          aes(fill = counties$total_active_percapita)) +

#  geom_sf(data = counties$geometry, size = geom_thickness, fill = counties$color_code) +
#          aes(fill = counties$color_code)) +

  geom_text(data = counties, size = map_fontsize, fontface = "bold",
            color = counties$textcolor,
            aes(x     = county_centers$x,
                y     = county_centers$y,
                label = total_active_percapita_label),
            nudge_x = counties$nudge_x,
            nudge_y = counties$nudge_y) +
  labs(title = paste("Total active cases as % of population: ", num, "%", sep = "")) +
  #labs(title = paste("Total Active per ", scale_txt, ": ", num, sep = "")) +
  scale_fill_gradientn(colours = map_palette[map_palette != "#7f7f7f"], trans = "pseudo_log")
print(map_total_active_percapita)

