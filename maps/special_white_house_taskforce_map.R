################################################################################
### Show the "White House Taskforce" map of the state.   
###
### More info (such as there is) here:
### https://www.tn.gov/health/cedep/ncov/data/maps.html
################################################################################

### To just use the spreadsheet, select this.   Otherwise, it will attempt to
### render the graph using the current data
use_spreadsheet <- 0

if (use_spreadsheet == 1) {
  
  wh_df_date <- as.Date("2020-07-31")
  
  wh_df <-
    "data/WH-Task-Force-TN-State-Report.xlsx" %>%
    # Skip the "White House Task Force TN Report" header at top
    read_xlsx(skip = 1) %>%
    
    # Stack the columns and rework to make them easier to join to the map
      pivot_longer(c("RED", "YELLOW", "WHITE"), 
                   names_to = "names", 
                   values_to = "values") %>% 
    filter(values != "<NA>") %>% 
    rename(county = values, color_level = names) %>% 
    mutate(county = if_else(county == "DeKalb", "Dekalb", county)) %>%
    mutate(county = if_else(county == "Green",  "Greene", county)) %>%
    select(county, color_level, color_level)

} else {


  
  ### Use this to filter data after this date, so I can try to figure out what 
  ### date the White House is using to get their results.
  
  last_date <- as.Date("2020-08-01")
#  last_date <- as.Date(Sys.Date()) + 1  # Effectively disable it 
  
  wh_df_date <- last_date
  
  #wh_df_date <- new_cases_tib %>% tail(n = 1) %>% pull("Date")
  
  # From https://www.newschannel5.com/news/newschannel-5-investigates/white-house-task-force-warns-about-covid-19-spread-in-75-tennessee-counties
  #
  # Red zones are jurisdictions that, during the previous week, "reported both new
  # cases above 100 per 100,000 population and a diagnostic test positivity result above
  # 10 percent," task force documents show.
  #
  # Yellow zones are those areas that "reported both new cases between 10-100 per 
  # 100,000 population, and a diagnostic test positivity result between 5-10%, or 
  # one of those two conditions and one condition qualifying as being in the 
  # 'Red Zone.'"


  ### Pluck out info about new cases over the last 7 days   Doing it this way
  ### instead of using the new_cases_tib directly because some of the new_cases
  ### have negative results, and this method eliminates them.
  new_cases_last7 <-
    total_cases_tib %>%
    filter(Date >= as.Date("2020-04-01")) %>% 
    filter(Date <= last_date) %>%
    tail(n = 7) %>%
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
    filter(DATE >= as.Date("2020-04-01")) %>% 
    filter(DATE <= last_date) %>%
    select(DATE, COUNTY, POS_TESTS, NEG_TESTS) %>%
    pivot_wider(id_cols = "DATE", 
                names_from = "COUNTY", 
                names_sep = ":", 
                values_from = c("POS_TESTS", "NEG_TESTS")) %>%
    tail(n = 7) %>%
    filter(row_number()==1 | row_number()==n()) %>%
    mutate(across(!starts_with("DATE"), ~ . - lag(.))) %>%
    tail(n = 1) %>%
    pivot_longer(-DATE, names_to = c("type", "county"), names_sep = ":", values_to = "value") %>%
    pivot_wider(id_cols = c("DATE", "county"), names_from = "type", values_from = "value") %>%
    mutate(positivity = POS_TESTS / (POS_TESTS + NEG_TESTS)) %>%
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
      new_cases_percapita <  10    ~ "WHITE",
      
      new_cases_percapita >= 10 &
        new_cases_percapita < 100  ~ "YELLOW",
      
      new_cases_percapita >= 100   ~ "RED",
    )) %>%

    mutate(level_pos = case_when(
      positivity <  5    ~ "WHITE",
      
      positivity >= 5  &
        positivity < 10  ~ "YELLOW",
      
      positivity >= 10   ~ "RED",
    )) %>%

    mutate(level = case_when(
      level_pop == "RED"    & level_pos == "RED"    ~ "RED",
      level_pop == "YELLOW" & level_pos == "YELLOW" ~ "YELLOW",
      level_pop == "RED"    & level_pos == "YELLOW" ~ "YELLOW",
      level_pop == "YELLOW" & level_pos == "RED"    ~ "YELLOW",
      TRUE                                          ~ "WHITE"
    )) 
  
    wh_df <-
      combined %>%
      select(County, level) %>%
      rename(county = County,
             color_level = level)
}

this_map <- 
  county_acs %>%
  mutate(NAME = str_replace(NAME, " County, Tennessee", "")) %>%
  mutate(NAME = if_else(NAME == "DeKalb", "Dekalb", NAME)) %>%
  select(GEOID, NAME) %>%
  rename(County = NAME) %>%
  left_join(wh_df, by = c("County" = "county"))


this_map$color_level <-
  factor(this_map$color_level,
         levels = c("WHITE", "YELLOW", "RED"))

map_title <- paste("White House Task Force Map - [", wh_df_date, "]", sep = "")

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
                        "YELLOW"  = "goldenrod2",
                        "WHITE"   = "white")) +
  
  labs(title = map_title)
print(map_white_house)
