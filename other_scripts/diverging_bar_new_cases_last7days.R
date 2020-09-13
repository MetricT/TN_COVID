# Original Phil Williams post - https://www.facebook.com/PhilWilliamsNC5/posts/10157183186395824

new_cases_percapita_last7 <-
  new_cases_tib %>%
  tail(n = 7) %>%
  select(-Date) %>%
  gather() %>%
  group_by(key) %>%
  summarize(new_cases_last7 = mean(value)) %>%
  ungroup() %>%
  rename(County = key) %>%
  filter(!County %in% c("Out of State", "Pending"))

pop_data <-
  county_acs %>%
  st_drop_geometry() %>%
  mutate(NAME = str_replace(NAME, " County, Tennessee", "")) %>%
  mutate(NAME = if_else(NAME == "DeKalb", "Dekalb", NAME)) %>%
  select(NAME, POP2018) %>%
  rename(County = NAME) 

total_pop <-
  pop_data$POP2018 %>% sum()

new_cases_percapita_last7 <-
  new_cases_percapita_last7 %>%
  full_join(pop_data, by = c("County" = "County")) %>%
  mutate(POP2018 = if_else(County == "Total", total_pop, POP2018)) %>%
  mutate(new_cases_percapita_last7 = 100000 * new_cases_last7 / POP2018) %>%
  mutate(County = if_else(County == "Total", "Tennessee", County))

tn_avg <-
  new_cases_percapita_last7 %>%
  filter(County == "Tennessee") %>%
  pull("new_cases_percapita_last7")

new_cases_percapita_last7 <-
  new_cases_percapita_last7 %>%
  filter(County != "Tennessee") %>%
  mutate(nc_diff = new_cases_percapita_last7 - tn_avg) %>%
  mutate(color = case_when(
    new_cases_percapita_last7 <  tn_avg ~ "darkseagreen3",
    new_cases_percapita_last7 >  tn_avg ~ "firebrick2"
  )) %>%
  mutate(string = County)

new_cases_percapita_last7 <-
  new_cases_percapita_last7[order(new_cases_percapita_last7$new_cases_percapita_last7),]

new_cases_percapita_last7$string <- factor(new_cases_percapita_last7$string, levels = new_cases_percapita_last7$string)

labels_shift_mean <- function(x) { x + round(tn_avg, 1) }

# Diverging Barcharts
g_diverging <-
   ggplot(data = new_cases_percapita_last7,
          aes(x = string, 
              y = nc_diff,
              label = new_cases_percapita_last7)) +
  geom_bar(stat = "identity",
           width = 0.5,
           aes(fill = color)) +
  scale_y_continuous(labels = labels_shift_mean) +

  scale_fill_manual(name = "New Cases per 100k",
                    labels = c("Below Average", "Above Average"),
                    values = c("darkseagreen3", "firebrick2")) +
  labs(title = "7-day COVID-19 Infection Rate, Tennessee Counties",
       subtitle = "(New cases/daily/100k Residents)",
       x = "County",
       y = "New Cases Per Capita Last 7 days") + 
  coord_flip()
print(g_diverging)

county_colors <-
  new_cases_percapita_last7 %>%
  select(County, color)

map <-
  county_acs %>%
  mutate(NAME = str_replace(NAME, " County, Tennessee", "")) %>%
  mutate(NAME = if_else(NAME == "DeKalb", "Dekalb", NAME)) %>%
  left_join(county_colors, by = c("NAME" = "County"))

g_map <-
  ggplot(data = map) +
  theme_void() + 
  theme(legend.position = "none") +
  geom_sf(aes(fill = map$color), color = "black", size = 0.2) +
  scale_fill_manual(values = c("darkseagreen3" = "darkseagreen3",
                               "firebrick2"   = "firebrick2"))
print(g_map)


library(cartogram)
crs_tn <- "+proj=aea +lat_1=25 +lat_2=50 +lon_0=-86"

cartogram <-
  map %>%
  st_transform(crs = crs_tn) %>%
  cartogram_cont("POP2018", itermax = 50)

g_cartogram <-
  ggplot(data = cartogram) +
  theme_void() + 
  theme(legend.position = "none") +
  geom_sf(aes(fill = map$color), color = "black", size = 0.2) +
  scale_fill_manual(values = c("darkseagreen3" = "darkseagreen3",
                               "firebrick2"   = "firebrick2"))
print(g_cartogram)

g_final <-
  g_diverging +
  annotation_custom(ggplotGrob(g_cartogram), 
                    ymin = 1.5,
                    ymax = 49.5)
print(g_final)
