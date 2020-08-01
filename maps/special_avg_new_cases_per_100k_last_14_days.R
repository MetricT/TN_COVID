################################################################################
### Map of new cases per capita the last 14 days by county.  This map can be
### found at:  https://www.tn.gov/health/cedep/ncov/data/epi-curves.html
### I've seen Phil Williams @ NC5 use it several times so I thought it was worth
### replicating
################################################################################
scale     <- 100000
scale_txt <- "100k"

new_cases_last14 <-
  new_cases_tib %>%
  tail(n = 15) %>%
  head(n = 14) %>%
  select(-Date, -Total) %>%
  gather() %>%
  group_by(key) %>%
  summarize(new_cases_last14 = mean(value)) %>%
  rename(County = key) #%>%

this_map <- 
  county_acs %>%
  mutate(NAME = str_replace(NAME, " County, Tennessee", "")) %>%
  mutate(NAME = if_else(NAME == "DeKalb", "Dekalb", NAME)) %>%
  select(GEOID, NAME, POP2018) %>%
  rename(County = NAME) %>%
  left_join(new_cases_last14, by = "County") %>%
  mutate(new_cases_percapita_last14 = scale * new_cases_last14 / POP2018) %>%
  mutate(
    color_code = case_when(
      new_cases_percapita_last14 <= 10 ~ "Below Threshold <= 10",
      new_cases_percapita_last14 >  10 ~ "Above Threshold >  10",
      #new_cases_percapita_last14 <= 10 ~ "#3e87b5",
      #new_cases_percapita_last14 >  10 ~ "#ce703a",
    ))



new_cases_percapita_last14_label <- 
  (this_map$new_cases_percapita_last14) %>% round(1)
new_cases_percapita_last14_label[new_cases_percapita_last14_label == 0] <- ""

### By default use black for the font, but if the value is over 1/2's of the way
### to the max, use the white font instead as it looks nicer
frac <- 0.5 * max(this_map$new_cases_percapita_last14)
this_map$textcolor = if_else(this_map$new_cases_percapita_last14 > frac, "white", "black")

this_map$textcolor = "black"

map_new_cases_percapita_last14 <- 
  ggplot(this_map) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom") +
  geom_sf(data = this_map$geometry, aes(fill = this_map$color_code),
          size = geom_thickness, color = "white") +
  geom_text(data = this_map, size = 3, 
            color = "black", #this_map$textcolor,
            aes(x     = county_centers$x,
                y     = county_centers$y,
                label = new_cases_percapita_last14_label),
            nudge_x = this_map$nudge_x,
            nudge_y = this_map$nudge_y) +
  
  # Set the color scale manually
  scale_fill_manual(values =  c("Below Threshold <= 10" = "#3e87b5",
                                "Above Threshold >  10" = "#ce703a")) +
  
  labs(title = paste("Average New Cases Per ", scale_txt, " Last 14 Days - ", Sys.Date(), sep = "")) 
print(map_new_cases_percapita_last14)
  
