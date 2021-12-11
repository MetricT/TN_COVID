library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)

latest_date <- vaccine_age_df %>% arrange(desc(Date)) %>% head(n = 1) %>% pull(Date)

counties <- counties %>% arrange(County)

vaccine_df <-
  vaccine_age_df %>% 
  filter(Date == latest_date) %>%
  select(County, RECIPIENT_COUNT) %>% 
  group_by(County) %>% 
  summarize(Vaccine_Count = sum(RECIPIENT_COUNT, na.rm = TRUE)) %>% 
  ungroup() %>%
  mutate(County = str_to_lower(County))

### Pull population data from the Census
pop <- tn_pop_df %>%
  rename(NAME = County,
         pop2019 = POP2018)%>%
  mutate(NAME = str_to_lower(NAME))

# Merge vaccine data with population data and compute fraction of population
# that has received vaccine
vaccine_data <-
  vaccine_df %>%
  left_join(pop, by = c("County" = "NAME")) %>%
  mutate(vaccine_frac = Vaccine_Count / pop2019)

### Find the center of each county so we can add the number of cases
county_centers <-
  counties$geometry %>%
  as_Spatial() %>%
  gCentroid(byid = TRUE)

counties$vaccine_frac <- round(100 * vaccine_data$vaccine_frac, 1)

vaccine_frac_label <- counties$vaccine_frac
#vaccine_frac_label <- paste(counties$vaccine_frac, "%", sep = "")
#vaccine_frac_label <- counties$vaccine_frac %>% round(digits = 3)

md <- counties$vaccine_frac %>% median()
sd <- counties$vaccine_frac %>% sd()

### By default use black for the font, but use white if it's in a dark range
counties$vac_textcolor = if_else(counties$vaccine_frac > md + 1.4*sd, "white", "black")
counties$vac_textcolor = if_else(counties$vaccine_frac < md - 1.4*sd, "white", counties$vac_textcolor)

map_tn_vaccine_per_capita <-
  ggplot(data = counties) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5),#, size = 16),
    legend.title = element_blank(),
    legend.position = "none"
    ) +
  geom_sf(data = counties, size = geom_thickness, aes(fill = vaccine_frac, geometry = geometry)) + 
  
  geom_text(size = map_fontsize, fontface = "bold",
            color = counties$vac_textcolor,
            aes(x     = county_centers$x,
                y     = county_centers$y,
                label = vaccine_frac_label),
            nudge_x = counties$nudge_x,
            nudge_y = counties$nudge_y) +
  
  scale_fill_gradient2(low = "darkred", high = "darkgreen", mid = "lightyellow", midpoint = counties$vaccine_frac %>% median(), labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Percent of Population receiving at least one vaccine dose")
print(map_tn_vaccine_per_capita)
