################################################################################
### Compute Rt (the reproduction number) for the given counties and populations
################################################################################

### Load the necessary libraries
packages <- 
  c("tidyverse", "lubridate", "tsibble", "EpiEstim", "curl", "forecast", "TTR")

new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages, quiet = TRUE)
invisible(lapply(packages, "library", quietly = TRUE,
                 character.only = TRUE, warn.conflicts = FALSE))

### Choose the counties you want to add to the facet graph.  They will also graph
### in this order, so arrange them how you want.

### If you want to do every county in the state
my_county <- 
  total_cases_tib %>% 
  pivot_longer(-Date, names_to = "County", values_to = "Value") %>% 
  select(County) %>% 
  filter(!County %in% c("Total", "Pending", "Out of State")) %>% 
  mutate(County = ifelse(County == "Dekalb", "DeKalb", County)) %>%
  unique() %>% 
  pull()

facet_map <- 
  us_tn_counties_grid1 %>% 
  mutate(name = if_else(name == "Dekalb", "DeKalb", name),
         code = if_else(code == "Dekalb", "DeKalb", code))

### Counties centered on Davidson
my_county <- c("Montgomery", "Robertson", "Sumner", 
               "Cheatham",   "Davidson",  "Wilson",
               "Dickson",    "Williamson", "Rutherford")


facet_map <- 
  tribble(
    ~name,        ~row, ~col,  ~code,
    "Montgomery",   1,    1,   "Montgomery",
    "Robertson",    1,    2,   "Robertson",
    "Sumner",       1,    3,   "Sumner",
    "Cheatham",     2,    1,   "Cheatham",
    "Davidson",     2,    2,   "Davidson",
    "Wilson",       2,    3,   "Wilson",
    "Dickson",      3,    1,   "Dickson",
    "Williamson",   3,    2,   "Williamson",
    "Rutherford",   3,    3,   "Rutherford",
  )

#my_county <- c("Cheatham")

#facet_map <- 
#  tribble(
#    ~name,        ~row, ~col,  ~code,
#    "Cheatham",     1,    1,   "Cheatham",
#  )



################################################################################
### Fetching data.   The EpiEstim::estimate_R() function expects to be handed a
### data.frame or tibble with two columns:   "dates" (date of confirmed case)
### and "I" (number of new incidents/cases).   I add a third column "county"
### so I can make facet graphs of multiple counties at once.
################################################################################

### The TN Dept of Health maintains a separate spreadsheet of confirmed cases for
### school-age children (aged 5-18 years) for each county.    Load that and use
### it as our population.

### Function to read Excel spreadsheet from URL's
read_excel_url <- function(url, ...) {
  tf <- tempfile(fileext = ".xlsx")
  curl::curl_download(url, tf)
  return(readxl::read_excel(tf, ...))
}

### Load case data for schools
data <-
  "https://www.tn.gov/content/dam/tn/health/documents/cedep/novel-coronavirus/datasets/Public-Dataset-Daily-County-Cases-5-18-Years.XLSX" %>%
  read_excel_url() %>%
  mutate(DATE = as.Date(DATE)) %>%
  select(DATE, NEW_CASES, COUNTY) %>%
  filter(DATE >= as.Date("2021-06-01")) %>%
  rename(dates    = DATE,
         I        = NEW_CASES,
         county = COUNTY) %>%
  
  arrange(dates) %>%
  unique() %>%
  
  # Filter it to just the counties specified
  mutate(county = ifelse(county == "Dekalb", "DeKalb", county)) %>%
  filter(county %in% my_county) %>%
  
  # Filter out cases wh#ere "I" is a NA
  filter(!is.na(I)) %>%

  # Also, estimate_R can't handle negative numbers, so set negative values to 0.
  # These are usually due to suspected cases being lumped in and subsequently 
  # found to not be COVID.
  mutate(I = if_else(I < 0, 0, I))


################################################################################
### The model EpiEstim uses requires the "serial interval", ie the lag in time
### between infection and symptoms appearing.    I use values from the CDC paper
### below.
### https://wwwnc.cdc.gov/eid/article/26/6/20-0357_article
################################################################################

si_mean <- 3.96
si_std  <- 4.75


### Create a blank tibble to hold our EpiEstim output
Rt_tib <-
  tribble(
    ~dates, ~mean_r, ~std_r, ~county,
  )

### Iterate over all counties and generate the EpiEstim estimate for Rt
for (this_county in data %>% pull("county") %>% unique()) {
  
  print(this_county)
  
  data_subset <-
    data %>%
    filter(county == this_county) %>%
    select(dates, I)
  
  estimate <- 
    estimate_R(data_subset,
               method = "parametric_si", 
               config = make_config(list(mean_si = si_mean,
                                          std_si = si_std)))
  
  e_dates  <- estimate$dates %>% as_tibble() %>% filter(row_number() > 7)
  e_values <- estimate$R %>% as_tibble() %>% select("Mean(R)", "Std(R)")
  
  r_data <- 
    e_dates %>% 
    bind_cols(e_values) %>% 
    janitor::clean_names() %>% 
    rename(dates = value) %>%
    mutate(county = this_county)
  
  Rt_tib <-
    Rt_tib %>%
    bind_rows(r_data)
  
}

### Take the Rt_tib estimates and compute mstl() trends
trend_tib <-
  Rt_tib %>%
  select(dates, county, mean_r) %>% 
  arrange(dates, county) %>%
  unique() %>%
  mutate(county = paste("values:", county, sep = "")) %>%
  pivot_wider(id_cols = "dates", 
              names_from = "county", 
              values_from = "mean_r") %>%
  mutate(across(starts_with("values:"),
                .fns = list(trend = ~ (.) %>% ts() %>% mstl() %>% trendcycle()),
                .names = "{fn}_{col}")) %>%
  select("dates", starts_with("trend_values")) %>%
  rename_at(vars(starts_with("trend_values:")),
            ~ str_replace(., "trend_values:", "")) %>%
  pivot_longer(-dates, names_to = "county", values_to = "trend")


### Add our trend data to the Rt tibble
Rt_tib <-
  Rt_tib %>%
  left_join(trend_tib, by = c("dates" = "dates", "county" = "county")) 


################################################################################
### Render the graph and done
################################################################################
### Title for our graph
title <- paste("Estimated Rt among children ages 5-18 - ", 
               Rt_tib %>% tail(n = 1) %>% pull("dates"), sep = "")

subtitle <- paste("assuming mean(serial interval) = ", si_mean, 
                  " days and std(serial interval) = ", si_std, " days", sep = "")

g_rt_schools <-
  ggplot(data = Rt_tib, aes(x = as.Date(dates))) +
  theme_linedraw() +
  theme(strip.text.x = element_text(size = 12)) +
  geom_point(aes(y = mean_r), size = 1.0) + 
  geom_ribbon(aes(ymin = mean_r - std_r, 
                  ymax = mean_r + std_r),
              color = NA, fill = "darkgrey", alpha = 0.2) +
  geom_line(aes(y = trend), color = "darkseagreen4", size = 1.2) +
  geom_hline(yintercept = 1, linetype = "dashed") + 
  facet_wrap(~county) + 
  facet_geo(~ county, grid = facet_map) +
   
  scale_y_continuous(limits = c(0, 2)) +
  
  labs(title = title, x = "", y = "Rt")
print(g_rt_schools)

