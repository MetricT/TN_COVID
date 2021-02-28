### EpiEstim and other useful epidemic analysis packages available from:
###
### # https://www.repidemicsconsortium.org/

library(tidyverse)
library(tsibble)
library(feasts)
library(EpiEstim)
library(forecast)

### Choose the countys you want to add to the facet graph

### If you want to do every county in the state
my_county <- 
  total_cases_tib %>% 
  pivot_longer(-Date, names_to = "County", values_to = "Value") %>% 
  select(County) %>% 
  filter(!County %in% c("Total", "Pending", "Out of State")) %>% 
  unique() %>% 
  pull()

my_county <- c("Montgomery", "Robertson", "Sumner",
               "Cheatham",   "Davidson",  "Wilson",
               "Dickson",    "Williamson", "Rutherford")

#my_county <- c("Montgomery", "Cheatham")

my_county <- c("Cheatham")

data <-
  county_new_df %>% 
  select(DATE, COUNTY, NEW_CASES) %>% 
  filter(DATE >= as.Date("2020-10-01")) %>%
  filter(COUNTY %in% my_county) %>%
  filter(!is.na(NEW_CASES)) %>%
  rename(dates = DATE, county = COUNTY, I = NEW_CASES) %>%
  mutate(I = if_else(I < 0, 0, I)) %>%
  filter(dates >= as.Date("2020-06-01"))

mask_mandates_df <-  read_csv("data/mandates.csv")

### Serial Interval estimates from:
### https://wwwnc.cdc.gov/eid/article/26/6/20-0357_article
si_mean <- 3.96
si_std  <- 4.75

### Create a blank tibble to hold our EpiEstim output
Rt_tib <-
  tribble(
    ~dates, ~mean_r, ~std_r, ~county,
  )

### Iterate over all countys and generate the EpiEstim estimate for Rt
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


### Add our trend and mask mandate data to the Rt tibble
Rt_tib <-
  Rt_tib %>%
  left_join(trend_tib, by = c("dates" = "dates", "county" = "county")) %>%
  left_join(mask_mandates_df, by = "county")

### Title for our graph
title <- "Estimate for Rt among the general population"
subtitle <- paste("assuming mean(serial interval) = ", si_mean, 
                  " days and std(serial interval) = ", si_std, " days", sep = "")

### Pick the top N worst counties
last_date <-
  Rt_tib %>% arrange(dates) %>% tail(n = 1) %>% pull(dates)

worst_counties <-
  Rt_tib %>% 
  filter(dates >= last_date - 3)  %>% 
  select(county, mean_r) %>% 
  group_by(county) %>% 
  summarize(mean_r = mean(mean_r)) %>% 
  ungroup() %>% 
  arrange(desc(mean_r)) %>% 
  head(n = 20) %>%
  pull("county")

Rt_tib <- Rt_tib %>% filter(county %in% worst_counties)

### Order counties in the order given in my_location at the top of the script
Rt_tib$county <- factor(Rt_tib$county, levels = worst_counties)

#Rt_tib <-
#  Rt_tib %>%
#  filter(dates >= as.Date("2020-07-26"))

Rt_tib$county <- factor(Rt_tib$county, levels = my_county)

### Render the graph and done!
g_rt_counties <-
  ggplot(data = Rt_tib, aes(x = as.Date(dates))) +
  theme_linedraw() +
  theme(strip.text.x = element_text(size = 16)) +
  geom_point(aes(y = mean_r), size = 1.0) + 
  geom_ribbon(aes(ymin = mean_r - std_r, 
                  ymax = mean_r + std_r),
              color = NA, fill = "darkgrey", alpha = 0.2) +
  
  geom_line(aes(y = trend), color = "firebrick4", size = 1.2) +
  
  #geom_vline(xintercept = as.Date("2020-07-19"), linetype = "dashed", color = "darkseagreen4") +
  #geom_vline(xintercept = as.Date("2020-07-21"), linetype = "dashed", color = "firebrick4") +
  #geom_vline(mapping = aes(xintercept = as.Date(effective_date)), linetype = "dashed", color = "darkred") + 
  geom_hline(yintercept = 1, linetype = "dashed") + 
  scale_y_continuous(limits = c(0, 2)) +
  facet_wrap(~ county) +
  #facet_wrap(~ county, scales = "free_y") +
  labs(title = title, x = "", y = "Rt")
print(g_rt_counties)

plot_grid(g_rt_counties,
          g_rt_schools,
          nrow = 1, ncol = 2, align = "hv")

