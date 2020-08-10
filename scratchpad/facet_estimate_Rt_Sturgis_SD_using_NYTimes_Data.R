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
my_state <- "South Dakota"
my_county <- c("Meade")

my_locations <- paste(my_county, " Co., ", my_state, sep = "")

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
  filter(dates >= as.Date("2020-06-01")) %>%
  mutate(location = paste(location, " [Sturgis Bike Rally 2020]", sep =))


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

### Take the Rt_tib estimates and compute mstl() trends
trend_tib <-
  Rt_tib %>%
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


### Add our trend and mask mandate data to the Rt tibble
Rt_tib <-
  Rt_tib %>%
  left_join(trend_tib, by = c("dates" = "dates", "location" = "location"))

### Title for our graph
title <- paste("Estimated Rt ", 
               "assuming mean(serial interval) = ", si_mean, 
               " days and std(serial interval) = ", si_std, " days", sep = "")


################################################################################
### Add a "forecast" using linear regression
################################################################################
subset <-
  Rt_tib %>%
  mutate(dates = decimal_date(dates)) %>%
  filter(dates >= decimal_date(as.Date("2020-07-30"))) %>%
  filter(dates <= decimal_date(as.Date("2020-08-05")))

fit_subset <-
  glm(trend ~ dates, data = subset)

forecast_Rt <-
  tibble(
    date = as.Date("2020-08-06") + 1:10,
    forecast_r = fit_subset$coefficients[1] +
                 fit_subset$coefficients[2] * decimal_date(date)
  ) 

### Add our forecast to the tibble
Rt_combined <-
  Rt_tib %>%
  full_join(forecast_Rt, by = c("dates" = "date")) %>%
  mutate(location = "Meade Co., South Dakota  [Sturgis Bike Rally 2020]")

### Render the graph and done!
g <-
  ggplot(data = Rt_combined, aes(x = as.Date(dates))) +
  theme_linedraw() +
  theme(strip.text.x = element_text(size = 16)) +
  geom_point(aes(y = mean_r), size = 1.0) + 
  geom_ribbon(aes(ymin = mean_r - std_r, 
                  ymax = mean_r + std_r),
              color = NA, fill = "darkgrey", alpha = 0.2) +
  
  # Add a trend line
  geom_line(aes(y = trend), color = "darkseagreen4", size = 1.2) +
  
  # Add a forecast line based on linear regression
  geom_line(aes(y = forecast_r), color = "firebrick2", size = 1.2, linetype = "dotted") +

  geom_hline(yintercept = 1, linetype = "dashed") + 
  
  annotate("text", size = 4, label = "Rally Begins", x = as.Date("2020-08-06"), y = 0.5, angle = "90") +
  geom_vline(xintercept = as.Date("2020-08-07"), linetype = "dashed") +
  
  annotate("text", size = 4, label = "Rally Ends", x = as.Date("2020-08-15"), y = 0.5, angle = "90") +
  geom_vline(xintercept = as.Date("2020-08-16"), linetype = "dashed") +
  
  scale_y_continuous(limits = c(0, 3)) + 
  
  facet_wrap(~ location) +
  labs(title = "Rt estimate", x = "", y = "Rt")
print(g)

