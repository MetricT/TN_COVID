################################################################################
### Map of new active the last 7 days by county
################################################################################
counties <- counties %>% select(-starts_with("new_active_last7"))

new_active_last7 <-
  new_active_tib %>%
  tail(n = 7) %>%
  select(-Date, -Total, -Pending, -"Out of State") %>%
  gather() %>%
  group_by(key) %>%
  summarize(new_active_last7 = sum(value)) %>%
  rename(County = key)

counties <-
  counties %>%
  left_join(new_active_last7, by = "County")

num <-
  new_active_last7$new_active_last7 %>%
  sum() %>%
  format(big.mark = ",", scientific = FALSE)

new_active_last7_label <- counties$new_active_last7
new_active_last7_label[new_active_last7_label == 0] <- "0"

### By default use black for the font, but if the value is over 1/2's of the way
### to the max, use the white font instead as it looks nicer
frac <- 0.2 * (counties$new_active_last7 %>% na.omit() %>% max())
counties$textcolor = if_else(counties$new_active_last7 > frac, "white", "black")

map_new_active_last7 <-
  ggplot(counties) +
  theme_void() +
  theme(legend.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") +
  geom_sf(data = counties$geometry, aes(fill = counties$new_active_last7), size = geom_thickness) +
  geom_text(data = counties, size = map_fontsize, fontface = "bold",
            color = counties$textcolor,
            aes(x     = county_centers$x,
                y     = county_centers$y,
                label = new_active_last7_label),
            nudge_x = counties$nudge_x,
            nudge_y = counties$nudge_y) +
  labs(title = paste("New Active Last 7 Days: ", num, sep = "")) +
#  scale_fill_gradientn(colours = map_palette)
  scale_fill_gradient2(midpoint = 0,
                       trans = "pseudo_log",
                       low  = "darkgreen",
                       mid  = "white",
                       high = "darkred")
print(map_new_active_last7)
