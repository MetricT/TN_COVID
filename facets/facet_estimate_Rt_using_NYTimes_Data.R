### EpiEstim and other useful epidemic analysis packages available from:
###
### # https://www.repidemicsconsortium.org/

library(tidyverse)
library(tsibble)
library(feasts)
library(cowplot)
library(EpiEstim)
library(plotrix)
library(geofacet)
library(forecast)

### Choose the locations you want to add to the facet graph
my_state <- "Tennessee"
my_county <- c("Montgomery", "Robertson", "Sumner",
               "Cheatham",   "Davidson",  "Wilson",
               "Dickson",    "Williamson", "Rutherford")

my_county <- c("Cheatham", "Davidson")

my_locations <- paste(my_county, " Co., ", my_state, sep = "")

### Pull data on local mask mandates from a spreadsheet 
mask_mandates_df <- 
  read_csv("data/mandates.csv", col_names = TRUE, col_types = "cDc") %>%
  filter(county %in% my_county) %>%
  mutate(location = paste(county, " Co., Tennessee", sep = ""))


### Serial Interval estimates from:
### https://wwwnc.cdc.gov/eid/article/26/6/20-0357_article
si_mean <- 3.96
si_std  <- 4.75


### Load COVID data from the NY Times.   You can check out your own from Github
### or if you want you can access it directly at:
### https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv
spreadsheet <- 
  "../Datasets/nytimes/covid-19-data/us-counties.csv" %>%
  read_csv(col_names = TRUE, col_types = "Dccddd") %>%
  mutate(location = paste(county, " Co., ", state, sep = "")) %>%
  filter(location %in% my_locations) %>%
  select(date, location, cases)

### The NY Times gives *total* cases.   Convert that to new cases,
### and fill in the occasional negative number with zeros.  The EpiEstim
### estimate_R() function expects the data to be labeled "dates" and "I" so
### do that as well.
data <-
  spreadsheet %>%
  pivot_wider(id_cols = "date", names_from = "location", values_from = "cases") %>%
  mutate(across(!starts_with("date"),
                .fns = list(new = ~ c((.)[1], diff(.))),
                .names = "{fn}_{col}"
  )) %>%
  select("date", starts_with("new_")) %>%
  rename_at(vars(starts_with("new_")),
          ~ str_replace(., "new_", "")) %>%
  pivot_longer(-date, names_to = "location", values_to = "new_cases") %>%
  mutate(new_cases = if_else(new_cases < 0, 0, new_cases)) %>%
  select(date, location, new_cases) %>%
  rename(dates = date,
         I = new_cases) %>%
  filter(dates >= as.Date("2020-06-01"))


### Create a blank tibble to hold our EpiEstim output
r0_tib <-
  tribble(
    ~dates, ~mean_r, ~std_r, ~location,
  )

### Iterate over all locations and generate the EpiEstim estimate for R0
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
  
  r0_tib <-
    r0_tib %>%
    bind_rows(r_data)
    
}

### Take the r0_tib estimates and compute mstl() trends
trend_tib <-
  r0_tib %>%
  select(dates, location, mean_r) %>% 
  mutate(location = paste("values:", location, sep = "")) %>%
  pivot_wider(id_cols = "dates", 
              names_from = "location", 
              values_from = "mean_r") %>%
  mutate(across(starts_with("values:"),
              .fns = list(trend = ~ (.) %>% ts() %>% mstl() %>% trendcycle()),
              .names = "{fn}_{col}")) %>%
  select("dates", starts_with("trend_values")) %>%
  rename_at(vars(starts_with("trend_values:")),
            ~ str_replace(., "trend_values:", "")) %>%
  pivot_longer(-dates, names_to = "location", values_to = "trend")


### Add our trend and mask mandate data to the r0 tibble
r0_tib <-
  r0_tib %>%
  left_join(trend_tib, by = c("dates" = "dates", "location" = "location")) %>%
  left_join(mask_mandates_df, by = "location")

### Title for our graph
title <- paste("Estimated R0 ", 
               "assuming mean(serial interval) = ", si_mean, 
               " days and std(serial interval) = ", si_std, " days", sep = "")

### I'm using geofacet to render the facets, so it'll be easy to stick in a 
### grid for the entire state at a later date.
#my_grid <- data.frame(
#  row = c( 1, 1, 1,
#           2, 2, 2,
#           3, 3, 3),
#  col = c( 1, 2, 3,
#           1, 2, 3,
#          1, 2, 3),
#  code = my_locations,
#  name = my_locations,
#  stringsAsFactors = FALSE
#)

### Render the graph and done!
g <-
  ggplot(data = r0_tib, aes(x = as.Date(dates))) +
  theme_linedraw() +
  theme(strip.text.x = element_text(size = 16)) +
  geom_point(aes(y = mean_r), size = 1.0) + 
  geom_ribbon(aes(ymin = mean_r - std_r, 
                  ymax = mean_r + std_r),
              color = NA, fill = "darkgrey", alpha = 0.2) +
  
  geom_line(aes(y = trend), color = "darkseagreen4", size = 1.2) +
  
  geom_vline(mapping = aes(xintercept = as.Date(effective_date)), linetype = "dashed", color = "darkred") + 
  geom_hline(yintercept = 1, linetype = "dashed") + 
#  facet_geo(~ location, grid = my_grid) +
  facet_wrap(~ location) +
  labs(title = title, x = "", y = "R0")
print(g)
