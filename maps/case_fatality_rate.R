################################################################################
### Map of Case Fatality Rate by county
################################################################################

cfr <-
  (total_cases_tib %>% filter(Date >= as.Date("2020-03-20")) %>% pivot_longer(-Date) %>% rename(total_cases = value)) %>%
  left_join((total_deaths_tib %>% filter(Date >= as.Date("2020-03-20")) %>% pivot_longer(-Date) %>% rename(total_deaths = value)), by = c("Date" = "Date", "name" = "name")) %>%
  mutate(cfr = total_deaths / total_cases) %>%
  select(Date, name, cfr) %>%
  pivot_wider(id_cols = c("Date"), names_from = c("name"), values_from = c("cfr")) %>%
  filter(Date >= as.Date("2020-04-01"))


d_cfr <-
  (new_cases_tib %>% filter(Date >= as.Date("2020-03-20")) %>% pivot_longer(-Date) %>% rename(new_cases = value)) %>%
  left_join((new_deaths_tib %>% filter(Date >= as.Date("2020-03-20")) %>% pivot_longer(-Date) %>% rename(new_deaths = value)), by = c("Date" = "Date", "name" = "name")) %>%
  mutate(d_cfr = new_deaths / new_cases) %>%
  mutate(d_cfr = if_else(is.finite(d_cfr), d_cfr, 0)) %>%
  select(Date, name, d_cfr) %>%
  pivot_wider(id_cols = c("Date"), names_from = c("name"), values_from = c("d_cfr")) %>%
  filter(Date >= as.Date("2020-04-01"))


case_fatality_rate <-
  d_cfr %>%
  tail(n = 7) %>%
  select(-Date, -Total) %>%
  gather() %>%
  group_by(key) %>%
  summarize(case_fatality_rate = mean(value)) %>%
  rename(County = key)

counties <-
  counties %>%
  left_join(case_fatality_rate, by = "County")

counties$case_fatality_rate <- ifelse(counties$total_cases != 0,
                                      counties$total_deaths / counties$total_cases,
                                      NA)

avg_cfr = (100 * (counties$total_deaths %>% na.omit() %>% sum()) /
                 (counties$total_cases %>% na.omit() %>% sum())) %>% round(2)

counties$case_fatality_rate[is.nan(counties$case_fatality_rate)] <- 0
cfr_title <- paste("Case Fatality Rate (State Avg = ", avg_cfr, "%)", sep = "")

case_fatality_rate_label <- 100*round(counties$case_fatality_rate, 4)
case_fatality_rate_label[case_fatality_rate_label == 0 & counties$total_cases != 0] <- ""

### By default use black for the font, but if the value is over 1/2's of the way
### to the max, use the white font instead as it looks nicer
frac <- 0.5 * max(counties$case_fatality_rate)
counties$textcolor = if_else(counties$case_fatality_rate >= frac, "white", "black")

map_case_fatality_rate <- ggplot(counties) +
  theme_void() +
  theme(legend.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") +
  geom_sf(data = counties$geometry, aes(fill = counties$case_fatality_rate), size = geom_thickness) +
  geom_text(data = counties, size = map_fontsize, fontface = "bold",
            color = counties$textcolor,
            aes(x     = county_centers$x,
                y     = county_centers$y,
                label = case_fatality_rate_label),
            nudge_x = counties$nudge_x,
            nudge_y = counties$nudge_y) +
  labs(title = cfr_title) +
  scale_fill_gradientn(colours = map_palette, trans = "pseudo_log")
print(map_case_fatality_rate)
