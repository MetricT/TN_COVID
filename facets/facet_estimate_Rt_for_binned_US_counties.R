### EpiEstim and other useful epidemic analysis packages available from:
###
### # https://www.repidemicsconsortium.org/

library(tidyverse)
library(tsibble)
library(feasts)
library(EpiEstim)
library(forecast)

### Get county population and divide them into bins according to population
pop_bins <-
  county_acs %>%
  select(NAME, POP2018) %>%
  mutate(NAME = gsub(" County, Tennessee", "", NAME)) %>%
  arrange(desc(POP2018)) %>%
  mutate(cum_pop = cumsum(POP2018)) %>%
  mutate(cum_pop_frac = cum_pop / 6651089) %>%
  mutate(pop_bin = case_when(
    cum_pop_frac <  0.25   ~ "Highest Population",
    cum_pop_frac >= 0.25 &
    cum_pop_frac <  0.50   ~ "High Population",
    cum_pop_frac >= 0.50 &
    cum_pop_frac <  0.75   ~ "Low Population",
    cum_pop_frac >= 0.75   ~ "Lowest Population",
  ))

pop_bins$pop_bin <- factor(pop_bins$pop_bin, levels = c("Highest Population", "High Population", "Low Population", "Lowest Population"
                                                        ))

ggplot(data = pop_bins, aes(fill = factor(pop_bin))) + theme_void() + geom_sf() + theme(plot.title = element_text(hjust = 0.5)) + theme(legend.title = element_blank()) + labs(title = "Tennessee Counties binned by population") + scale_fill_viridis(discrete = TRUE, option = "inferno")

data <-
  county_new_df %>% 
  select(DATE, COUNTY, NEW_CASES) %>% 
  filter(COUNTY %in% my_county) %>%
  filter(!is.na(NEW_CASES)) %>%
  rename(dates = DATE, county = COUNTY, I = NEW_CASES) %>%
  mutate(I = if_else(I < 0, 0, I)) %>%
  filter(dates >= as.Date("2020-06-01"))

mask_mandates_df <-
  read_csv("data/mandates.csv")

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

### Order counties in the order given in my_location at the top of the script
Rt_tib$county <- factor(Rt_tib$county, levels = my_county)

Rt_tib <-
  Rt_tib %>%
  filter(dates >= as.Date("2020-07-26"))

### Render the graph and done!
g_rt_counties <-
  ggplot(data = Rt_tib, aes(x = as.Date(dates))) +
  theme_linedraw() +
  theme(strip.text.x = element_text(size = 16)) +
  geom_point(aes(y = mean_r), size = 1.0) + 
  geom_ribbon(aes(ymin = mean_r - std_r, 
                  ymax = mean_r + std_r),
              color = NA, fill = "darkgrey", alpha = 0.2) +
  
  geom_line(aes(y = trend), color = "darkseagreen4", size = 1.2) +
  
  geom_vline(xintercept = as.Date("2020-07-19"), linetype = "dashed", color = "darkseagreen4") +
  
#  geom_vline(mapping = aes(xintercept = as.Date(effective_date)), linetype = "dashed", color = "darkred") + 
  geom_hline(yintercept = 1, linetype = "dashed") + 
  scale_y_continuous(limits = c(0.5, 2.0)) +
  facet_wrap(~ county) +
  labs(title = title, x = "", y = "Rt")
print(g_rt_counties)

plot_grid(g_rt_schools,
          g_rt_counties,
          nrow = 2, ncol = 1, align = "hv")
