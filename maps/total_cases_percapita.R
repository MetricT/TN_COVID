################################################################################
### Map of total COVID-19 cases per capita
################################################################################
scale     <- 1000
scale_txt <- "1k"

counties$total_cases_percapita <-
  round(scale * counties$total_cases / counties$POP2018, 1)

frac <- 0.2 * max(counties$total_cases_percapita)
counties$textcolor = if_else(counties$total_cases_percapita >= frac, "white", "black")

total_cases_label_per  <- round(counties$total_cases_percapita, 1)
total_cases_label_per[total_cases_label_per == 0] <- ""

map_total_cases_percapita <- ggplot(counties) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.caption = element_text(hjust = 0.5)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  geom_sf(data = counties$geometry, size = geom_thickness,
          aes(fill = counties$total_cases_percapita)) +
  geom_text(data = counties, size = map_fontsize, fontface = "bold",
            color = counties$textcolor,
            aes(x     = county_centers$x,
                y     = county_centers$y,
                label = total_cases_label_per),
            nudge_x = counties$nudge_x,
            nudge_y = counties$nudge_y) +
  labs(title = paste("Total Cases per ", scale_txt, sep = "")) +
  scale_fill_gradientn(colours = map_palette, trans = "pseudo_log")
print(map_total_cases_percapita)


