################################################################################
### Map of new cases per capita the last 7 days by county.  This particular map
### is based on one from Harvard Univ:
###
### https://globalepidemics.org/key-metrics-for-covid-suppression
################################################################################
scale     <- 100000
scale_txt <- "100k"

new_cases_last7 <-
  new_cases_tib %>%
  tail(n = 7) %>%
  select(-Date, -Total) %>%
  gather() %>%
  group_by(key) %>%
  summarize(new_cases_last7 = mean(value)) %>%
  rename(County = key) #%>%

this_map <- 
  county_acs %>%
  mutate(NAME = str_replace(NAME, " County, Tennessee", "")) %>%
  mutate(NAME = if_else(NAME == "DeKalb", "Dekalb", NAME)) %>%
  select(GEOID, NAME, POP2018) %>%
  rename(County = NAME) %>%
  left_join(new_cases_last7, by = "County") %>%
  mutate(new_cases_percapita_last7 = scale * new_cases_last7 / POP2018) %>%
  mutate(
    color_code = case_when(
      new_cases_percapita_last7 >= 25   ~ "> 25 daily new cases per 100k people",

      new_cases_percapita_last7 <  25 &
      new_cases_percapita_last7 >= 10   ~ "> 10 daily new cases per 100k people",

      new_cases_percapita_last7 <  10 &
      new_cases_percapita_last7 >= 1    ~ ">  1 daily new case  per 100k people",
      
      new_cases_percapita_last7 <   1  ~ "<  1 daily new case  per 100k people",
    ))

new_cases_percapita_last7_label <- 
  (this_map$new_cases_percapita_last7) %>% round(1)
new_cases_percapita_last7_label[new_cases_percapita_last7_label == 0] <- ""

### By default use black for the font, but if the value is over 1/2's of the way
### to the max, use the white font instead as it looks nicer
frac <- 0.5 * max(this_map$new_cases_percapita_last7)
this_map$textcolor = if_else(this_map$new_cases_percapita_last7 > frac, "white", "black")

this_map$textcolor = "black"

map_new_cases_percapita_last7 <- 
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
                label = new_cases_percapita_last7_label),
            nudge_x = this_map$nudge_x,
            nudge_y = this_map$nudge_y) +
  
  # Set the color scale manually
  scale_fill_manual(values = 
                      c("> 25 daily new cases per 100k people" = "red",
                        "> 10 daily new cases per 100k people" = "orange",
                        ">  1 daily new case  per 100k people" = "yellow",
                        "<  1 daily new case  per 100k people" = "green")) +
  
  labs(title = paste("Average New Cases Per ", scale_txt, " Last 7 Days - ", 
                     new_cases_tib %>% tail(n = 1) %>% pull("Date"), sep = "")) 
print(map_new_cases_percapita_last7)

