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

### Choose the countys you want to add to the facet graph
my_county <- c("Montgomery", "Robertson", "Sumner",
               "Cheatham",   "Davidson",  "Wilson",
               "Dickson",    "Williamson", "Rutherford")

data <-
  county_new_df %>% 
  select(DATE, COUNTY, NEW_CASES) %>% 
  filter(COUNTY %in% my_county) %>%
  filter(!is.na(NEW_CASES)) %>%
  rename(dates = DATE, county = COUNTY, I = NEW_CASES) %>%
  mutate(I = if_else(I < 0, 0, I)) 

mask_mandates_df <-
  read_csv("data/mandates.csv")

### Serial Interval estimates from:
### https://wwwnc.cdc.gov/eid/article/26/6/20-0357_article
si_mean <- 3.96
si_std  <- 4.75

### Create a blank tibble to hold our EpiEstim output
r0_tib <-
  tribble(
    ~dates, ~mean_r, ~std_r, ~county,
  )

### Iterate over all countys and generate the EpiEstim estimate for R0
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
  
  r0_tib <-
    r0_tib %>%
    bind_rows(r_data)
  
}

### Take the r0_tib estimates and compute mstl() trends
trend_tib <-
  r0_tib %>%
  select(dates, county, mean_r) %>% 
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


### Add our trend and mask mandate data to the r0 tibble
r0_tib <-
  r0_tib %>%
  left_join(trend_tib, by = c("dates" = "dates", "county" = "county")) %>%
  left_join(mask_mandates_df, by = "county")

### Title for our graph
title <- paste("Estimated R0 ", 
               "assuming mean(serial interval) = ", si_mean, 
               " days and std(serial interval) = ", si_std, " days", sep = "")

### I'm using geofacet to render the facets, so it'll be easy to stick in a 
### grid for the entire state at a later date.
my_grid <- data.frame(
  row = c( 1, 1, 1,
           2, 2, 2,
           3, 3, 3),
  col = c( 1, 2, 3,
           1, 2, 3,
           1, 2, 3),
  code = my_county,
  name = my_county,
  stringsAsFactors = FALSE
)

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
  scale_y_continuous(limits = c(0, 2)) +
  #facet_wrap(~ county) +
  facet_geo(~ county, grid = my_grid) +
  labs(title = title, x = "", y = "R0")
print(g)