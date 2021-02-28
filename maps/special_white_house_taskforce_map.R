################################################################################
### Show the "White House Taskforce" map of the state.   
###
### More info (such as there is) here:
### https://www.tn.gov/health/cedep/ncov/data/maps.html
################################################################################

case_data_from <- as.Date("2020-10-03")
lab_data_from  <- as.Date("2020-10-01")

### Pluck out info about new cases over the last 7 days   Doing it this way
### instead of using the new_cases_tib directly because some of the new_cases
### have negative results, and this method eliminates them.
new_cases_last7 <-
  total_cases_tib %>%
  filter(Date >= as.Date(case_data_from)) %>% 
  filter(Date <= as.Date(case_data_from) + 6) %>%
  filter(row_number()==1 | row_number()==n()) %>%
  mutate(across(!starts_with("DATE"), ~ . - lag(.))) %>%
  tail(n = 1) %>%
  pivot_longer(-Date, names_to = "County", values_to = "new_cases_last7") %>%
  select(County, new_cases_last7) %>%
  filter(!County %in% c("Out of State", "Pending", "Total"))
  
### Pluck out info about new positivity results over the last week.
positivity_results <- 
  county_new_df %>% 

  select(DATE, COUNTY, NEW_POS_TESTS, NEW_NEG_TESTS) %>% 
  filter(COUNTY %in% county_names) %>%
  rename(POS_TESTS = NEW_POS_TESTS,
         NEG_TESTS = NEW_NEG_TESTS) %>%
    
  filter(!(is.na(NEG_TESTS) & is.na(POS_TESTS))) %>%
  filter(DATE >= as.Date(lab_data_from)) %>%
  filter(DATE <= as.Date(lab_data_from) + 6) %>% 
  select(DATE, COUNTY, POS_TESTS, NEG_TESTS) %>%
  pivot_wider(id_cols = "COUNTY", names_from = "DATE", values_from = c("POS_TESTS", "NEG_TESTS"))

pos_count <-
  positivity_results %>%
  select(COUNTY, starts_with("POS_TESTS")) %>% 
  mutate(TOT_POS_TESTS = rowSums(select(., !starts_with("COUNTY")))) %>%
  select(COUNTY, TOT_POS_TESTS)

neg_count <-
  positivity_results %>%
  select(COUNTY, starts_with("NEG_TESTS")) %>% 
  mutate(TOT_NEG_TESTS = rowSums(select(., !starts_with("COUNTY")))) %>%
  select(COUNTY, TOT_NEG_TESTS)
  
positivity_results <-
  pos_count %>%
  left_join(neg_count, by = "COUNTY") %>%
  mutate(positivity = round(TOT_POS_TESTS / (TOT_POS_TESTS + TOT_NEG_TESTS), 3)) %>%
  rename(county = COUNTY) %>%
  select(county, positivity)

### Get the population so we can compute per capita data
pop_2018 <-
  county_acs %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  rename(county = NAME) %>%
  select(county, POP2018) %>%
  mutate(county = gsub(" County, Tennessee", "", county)) %>%
  mutate(county = if_else(county == "DeKalb", "Dekalb", county))

### Combine them all together
combined <- 
  new_cases_last7 %>%
  left_join(positivity_results, by = c("County" = "county")) %>%
  left_join(pop_2018, by = c("County" = "county")) %>%
  filter(!County %in% c("Out of State", "Pending")) %>%
  mutate(new_cases_percapita = 100000 * new_cases_last7 / POP2018) %>%
  mutate(positivity = positivity * 100) %>%

  # Red zones are jurisdictions that, during the previous week, "reported both new
  # cases above 100 per 100,000 per population and a diagnostic test positivity
  # result above 10 percent," task force documents show.
  
  # Yellow zones are those areas that "reported both new cases between 10-100 per 
  # 100,000 population, and a diagnostic test positivity result between 5-10%, or 
  # one of those two conditions and one condition qualifying as being in the 
  # 'Red Zone.'"
    
  mutate(level_pop = case_when(
    new_cases_percapita >  100   ~ "RED",
    
    new_cases_percapita >  50 &
    new_cases_percapita <= 100   ~ "ORANGE",
    
    new_cases_percapita >= 10  &
    new_cases_percapita <= 50   ~ "YELLOW",
    
    new_cases_percapita <  10    ~ "WHITE",  

  )) %>%

  mutate(level_pos = case_when(
    positivity <  5    ~ "WHITE",
      
    positivity >=  5 &
    positivity <   8   ~ "YELLOW",
    
    positivity >= 8 &
    positivity <= 10   ~ "ORANGE",
      
    positivity >  10   ~ "RED",
  )) %>%

  mutate(level = case_when(
    level_pop == "RED"    & level_pos == "RED"    ~ "RED",
    
    level_pop == "ORANGE" &
      level_pos %in% c("ORANGE", "RED")           ~ "ORANGE",
    
    level_pos == "ORANGE" &
      level_pop %in% c("ORANGE", "RED")           ~ "ORANGE",
    
    level_pop == "YELLOW" & level_pos == "YELLOW" ~ "YELLOW",
    
    level_pop == "YELLOW" & 
      level_pos %in% c("YELLOW", "ORANGE", "RED") ~ "YELLOW",
    
    level_pos == "YELLOW" & 
      level_pop %in% c("YELLOW", "ORANGE", "RED") ~ "YELLOW",
    
    TRUE                                          ~ "WHITE"
  )) 
  
  wh_df <-
    combined %>%
    select(County, level) %>%
    rename(county = County,
           color_level = level)


this_map <- 
  county_acs %>%
  mutate(NAME = str_replace(NAME, " County, Tennessee", "")) %>%
  mutate(NAME = if_else(NAME == "DeKalb", "Dekalb", NAME)) %>%
  select(GEOID, NAME) %>%
  rename(County = NAME) %>%
  left_join(wh_df, by = c("County" = "county"))


this_map$color_level <-
  factor(this_map$color_level,
         levels = c("WHITE", "YELLOW", "ORANGE", "RED"))

map_title <- "White House Task Force Map"

map_subtitle <- paste("Case Data is from ", as.Date(case_data_from) %>% format("%m/%d"), 
                                    " to ", as.Date(case_data_from + 6) %>% format("%m/%d"), ".\n",
                       "Lab Data is from ", as.Date(lab_data_from)  %>% format("%m/%d"),
                                    " to ", as.Date(lab_data_from + 6) %>% format("%m/%d"), ".", sep = "")

map_caption <- "White House Coronavirus Task Force"

map_white_house <- 
  ggplot(this_map) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(hjust = 0.5, size = 12),
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    legend.position = "bottom"
  ) +
  geom_sf(data = this_map$geometry, aes(fill = this_map$color_level),
          size = geom_thickness, color = "black") +

  # Set the color scale manually
  scale_fill_manual(values = 
                      c("RED"     = "firebrick2",
                        "ORANGE"  = "orange",
                        "YELLOW"  = "goldenrod2",
                        "WHITE"   = "white")) +
  
  labs(title = map_title, subtitle = map_subtitle)
print(map_white_house)

