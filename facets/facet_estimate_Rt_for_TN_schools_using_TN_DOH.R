################################################################################
### Compute Rt (the reproduction number) for the given locations and populations
################################################################################

### Load the necessary libraries
packages <- 
  c("tidyverse", "lubridate", "tsibble", "EpiEstim", "curl", "geofacet", 
    "forecast", "TTR")

new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages, quiet = TRUE)
invisible(lapply(packages, "library", quietly = TRUE,
                 character.only = TRUE, warn.conflicts = FALSE))

### Choose the counties you want to add to the facet graph
my_location <- 
  c("Montgomery", "Robertson", "Sumner", "Cheatham",   "Davidson",  "Wilson",
    "Dickson",    "Williamson", "Rutherford")

start_dates <- 
  tribble(
  ~county,       ~first_day,
  "Montgomery",  "2020-08-31",
  "Robertson",   "2020-08-10",
  "Sumner",      "2020-08-03",
  "Cheatham",    "2020-08-13",
  "Davidson",    "2020-08-04",
  "Wilson",      "2020-08-03",
  "Dickson",     "2020-08-03",
  "Williamson",  "2020-08-07",
  "Rutherford",  "2020-08-13",
  ) %>% mutate(first_day = as.Date(first_day))


################################################################################
### Fetching data.   The EpiEstim::estimate_R() function expects to be handed a
### data.frame or tibble with two columns:   "dates" (date of confirmed case)
### and "I" (number of new incidents/cases).   I add a third column "location"
### so I can make facet graphs of multiple locations at once.
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
  "https://www.tn.gov/content/dam/tn/health/documents/cedep/novel-coronavirus/datasets/Public-Dataset-Daily-County-School.XLSX" %>%
  read_excel_url() %>%
  mutate(DATE = as.Date(DATE)) %>%
  select(DATE, NEW_CASES, COUNTY) %>%
  rename(dates    = DATE,
         I        = NEW_CASES,
         location = COUNTY) %>%
  
  # Filter it to just the locations specified
  filter(location %in% my_location) %>%
  
  # Filter out cases where "I" is a NA
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
    ~dates, ~mean_r, ~std_r, ~location,
  )

### Iterate over all locations and generate the EpiEstim estimate for Rt
for (this_location in data %>% pull("location") %>% unique()) {
  
  print(this_location)
  
  data_subset <-
    data %>%
    filter(location == this_location) %>%
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
    mutate(location = this_location)
  
  Rt_tib <-
    Rt_tib %>%
    bind_rows(r_data)
  
}

### Take the Rt_tib estimates and compute the 7-day moving average
trend_tib <-
  Rt_tib %>%
  select(dates, location, mean_r) %>% 
  mutate(location = paste("values:", location, sep = "")) %>%
  pivot_wider(id_cols = "dates", 
              names_from = "location", 
              values_from = "mean_r") %>%
  
  # Use mstl() %>% trendcyle()
  mutate(across(starts_with("values:"),
                .fns = list(trend = ~ (.) %>% ts() %>% mstl() %>% trendcycle()),
                .names = "{fn}_{col}")) %>%

  # Use SMA
  #mutate(across(starts_with("values:"),
  #              .fns = list(trend = ~ (.) %>% SMA(n = 7)),
  #              .names = "{fn}_{col}")) %>%
  #mutate(dates = as.Date(dates) - 3) %>%
  
  
  select("dates", starts_with("trend_values")) %>%
  rename_at(vars(starts_with("trend_values:")),
            ~ str_replace(., "trend_values:", "")) %>%
  pivot_longer(-dates, names_to = "location", values_to = "trend")


### Add our trend and mask mandate data to the Rt tibble
Rt_tib <-
  Rt_tib %>%
  left_join(trend_tib, by = c("dates" = "dates", "location" = "location")) %>%
  left_join(start_dates, by = c("location" = "county"))



### I'm using geofacet to render the facets for middle TN.   For other locations,
### either find a more suitable facet grid, create your own, or just ignore it
### and use factor()/facet_wrap() to arrange it how you wish.
my_grid <- data.frame(
  row = c( 1, 1, 1,
           2, 2, 2,
           3, 3, 3),
  col = c( 1, 2, 3,
           1, 2, 3,
           1, 2, 3),
  code = my_location,
  name = my_location,
  stringsAsFactors = FALSE
)

################################################################################
### Render the graph and done
################################################################################

### Title for our graph
title <- paste("Estimated Rt ", 
               "assuming mean(serial interval) = ", si_mean, 
               " days and std(serial interval) = ", si_std, " days", sep = "")

g <-
  ggplot(data = Rt_tib, aes(x = as.Date(dates))) +
  theme_linedraw() +
  theme(strip.text.x = element_text(size = 16)) +
  geom_point(aes(y = mean_r), size = 1.0) + 
  geom_ribbon(aes(ymin = mean_r - std_r, 
                  ymax = mean_r + std_r),
              color = NA, fill = "darkgrey", alpha = 0.2) +
  
  geom_line(aes(y = trend), color = "darkseagreen4", size = 1.2) +
  
  geom_hline(yintercept = 1, linetype = "dashed") + 
  
  geom_vline(aes(xintercept = as.Date(first_day)), linetype = "dotted") +
  
  facet_geo(~ location, grid = my_grid) +
  # facet_wrap(~ location) + # Uncomment this if you don't want to use a geofacet grid
  labs(title = title, x = "", y = "Rt")
print(g)