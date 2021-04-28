################################################################################
### Map of new cases over past 14 days as a % of total cases
################################################################################
this_stat <- total_cases_tib
this_stat_txt <- "Cases"

### Assume 14 days, though you can change the constant if you need to
num_days <- 7

now <- 
  this_stat %>% 
  tail(n = 1) %>% 
  select(-Date) %>%
  pivot_longer(cols = everything()) %>%
  filter(!name %in% c("Out of State", "Pending")) %>%
  rename(now = value)

past <-
  this_stat %>% 
  tail(n = num_days) %>% 
  head(n = 1) %>%
  select(-Date) %>% 
  pivot_longer(cols = everything()) %>%
  filter(!name %in% c("Out of State", "Pending")) %>%
  rename(past = value)

data <- 
  now %>% 
  left_join(past, by = "name") %>% 
  rename(county = name) %>%
  mutate(cases_recent = (now - past) / now) %>%
  mutate(cases_recent = if_else(cases_recent < 0, 0.0001, cases_recent)) %>%
  select(county, cases_recent)

mid <- data %>% filter(county == "Total") %>% pull(cases_recent)

counties <- counties %>% left_join(data, by = c("County" = "county"))

frac <- 0.7 * max(counties$cases_recent)
counties$textcolor = if_else(counties$cases_recent >= frac, "white", "black")

recent_cases_label  <- round(100 * counties$cases_recent, digits = 1)
#recent_cases_label[recent_cases_label == 0] <- ""

map_recent_cases_percent <-
  ggplot(counties) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.caption = element_text(hjust = 0.5)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  geom_sf(data = counties$geometry, size = geom_thickness,
          aes(fill = counties$cases_recent)) +
  geom_text(data = counties, size = map_fontsize, fontface = "bold",
            color = counties$textcolor,
            aes(x     = county_centers$x,
                y     = county_centers$y,
                label = recent_cases_label),
            nudge_x = counties$nudge_x,
            nudge_y = counties$nudge_y) +
  labs(title = paste("New", this_stat_txt, "Last", num_days, "Days as a % of Total", this_stat_txt, "- TN Avg =", round(100 * mid, digits = 1), "%")) +
  
  scale_fill_gradient2(midpoint = mid, low   = "darkgreen", mid   = "white", high  = "darkred")
  
#  scale_fill_gradientn(colours = map_palette, trans = "pseudo_log")
print(map_recent_cases_percent)

################################################################################
### Map of new hospitalized over past 14 days as a % of total cases
################################################################################
this_stat <- total_hospitalized_tib
this_stat_txt <- "Hospitalized"

now <- 
  this_stat %>% 
  tail(n = 1) %>% 
  select(-Date) %>%
  pivot_longer(cols = everything()) %>%
  filter(!name %in% c("Out of State", "Pending")) %>%
  rename(now = value)

past <-
  this_stat %>% 
  tail(n = num_days) %>% 
  head(n = 1) %>%
  select(-Date) %>% 
  pivot_longer(cols = everything()) %>%
  filter(!name %in% c("Out of State", "Pending")) %>%
  rename(past = value)

data <- 
  now %>% 
  left_join(past, by = "name") %>% 
  rename(county = name) %>%
  mutate(hosp_recent = (now - past) / now) %>%
  mutate(hosp_recent = if_else(hosp_recent < 0, 0.0001, hosp_recent)) #%>%
  #select(county, hosp_recent)

mid <- data %>% filter(county == "Total") %>% pull(hosp_recent)

counties <- counties %>% left_join(data, by = c("County" = "county"))

frac <- 0.7 * max(counties$hosp_recent)
counties$textcolor = if_else(counties$hosp_recent >= frac, "white", "black")

recent_hosp_label  <- round(100 * counties$hosp_recent, digits = 1)
#recent_hosp_label[recent_hosp_label == 0] <- ""

map_recent_hosp_percent <-
  ggplot(counties) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.caption = element_text(hjust = 0.5)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  geom_sf(data = counties$geometry, size = geom_thickness,
          aes(fill = counties$hosp_recent)) +
  geom_text(data = counties, size = map_fontsize, fontface = "bold",
            color = counties$textcolor,
            aes(x     = county_centers$x,
                y     = county_centers$y,
                label = recent_hosp_label),
            nudge_x = counties$nudge_x,
            nudge_y = counties$nudge_y) +
  labs(title = paste("New", this_stat_txt, "Last", num_days, "Days as a % of Total", this_stat_txt, "- TN Avg =", round(100 * mid, digits = 1), "%")) +
  scale_fill_gradient2(midpoint = mid, low   = "darkgreen", mid   = "white", high  = "darkred")
#  scale_fill_gradientn(colours = map_palette, trans = "pseudo_log")
print(map_recent_hosp_percent)

################################################################################
### Map of new deaths over past 14 days as a % of total cases
################################################################################
this_stat <- total_deaths_tib
this_stat_txt <- "Deaths"

now <- 
  this_stat %>% 
  tail(n = 1) %>% 
  select(-Date) %>%
  pivot_longer(cols = everything()) %>%
  filter(!name %in% c("Out of State", "Pending")) %>%
  rename(now = value)

past <-
  this_stat %>% 
  tail(n = num_days) %>% 
  head(n = 1) %>%
  select(-Date) %>% 
  pivot_longer(cols = everything()) %>%
  filter(!name %in% c("Out of State", "Pending")) %>%
  rename(past = value)

data <- 
  now %>% 
  left_join(past, by = "name") %>% 
  rename(county = name) %>%
  mutate(deaths_recent = (now - past) / now) %>%
  mutate(deaths_recent = if_else(deaths_recent < 0, 0.0001, deaths_recent)) %>%
  select(county, deaths_recent)

mid <- data %>% filter(county == "Total") %>% pull(deaths_recent)

counties <- counties %>% left_join(data, by = c("County" = "county"))

frac <- 0.7 * max(counties$deaths_recent)
counties$textcolor = if_else(counties$deaths_recent >= frac, "white", "black")

recent_deaths_label  <- round(100 * counties$deaths_recent, digits = 1)
#recent_deaths_label[recent_deaths_label == 0] <- ""

map_recent_deaths_percent <-
  ggplot(counties) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.caption = element_text(hjust = 0.5)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  geom_sf(data = counties$geometry, size = geom_thickness,
          aes(fill = counties$deaths_recent)) +
  geom_text(data = counties, size = map_fontsize, fontface = "bold",
            color = counties$textcolor,
            aes(x     = county_centers$x,
                y     = county_centers$y,
                label = recent_deaths_label),
            nudge_x = counties$nudge_x,
            nudge_y = counties$nudge_y) +
  labs(title = paste("New", this_stat_txt, "Last", num_days, "Days as a % of Total", this_stat_txt, "- TN Avg =", round(100 * mid, digits = 1), "%")) +
  scale_fill_gradient2(midpoint = mid, low   = "darkgreen", mid   = "white", high  = "darkred")
#  scale_fill_gradientn(colours = map_palette, trans = "pseudo_log")
print(map_recent_deaths_percent)

final <-
  plot_grid(map_recent_cases_percent,
            map_recent_hosp_percent,
            map_recent_deaths_percent,
            nrow = 3, ncol = 1, align = "hv")
print(final)

