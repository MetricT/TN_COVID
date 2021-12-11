### EpiEstim and other useful epidemic analysis packages available from:
###
### # https://www.repidemicsconsortium.org/

library(tidyverse)
library(tsibble)
library(feasts)
library(EpiEstim)
library(forecast)

### Choose the countys you want to add to the facet graph
my_county <- c("Cheatham")

data <-
  county_new_df %>% 
  select(DATE, COUNTY, NEW_CASES) %>% 
  filter(DATE >= as.Date("2021-03-01")) %>%
  filter(COUNTY %in% my_county) %>%
  filter(!is.na(NEW_CASES)) %>%
  rename(dates = DATE, county = COUNTY, I = NEW_CASES) %>%
  mutate(I = if_else(I < 0, 0, I)) %>%
  filter(dates >= as.Date("2021-03-01")) %>%
  arrange(dates)

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
  left_join(data %>% select(-county), by = "dates") %>%
  mutate(county = paste("Rt estimate - Cheatham Co. All Ages -", Rt_tib %>% tail(n = 1) %>% pull("dates")))


### Render the graph and done!
g_rt_counties <-
  ggplot(data = Rt_tib, aes(x = as.Date(dates))) +
  theme_linedraw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(strip.text.x = element_text(size = 16)) +
  geom_point(aes(y = mean_r), size = 1.0) + 
  geom_ribbon(aes(ymin = mean_r - std_r, 
                  ymax = mean_r + std_r),
              color = NA, fill = "darkgrey", alpha = 0.2) +
  
  geom_line(aes(y = trend), color = "firebrick4", size = 1.2) +
  geom_hline(yintercept = 1, linetype = "dashed") + 
  scale_y_continuous(limits = c(0, 2)) +
  facet_wrap(~ county) +
  labs(title = "", x = "", y = "Rt")
print(g_rt_counties)

g_num_counties <- 
  g_num <-
  ggplot(data = data %>% filter(dates >= as.Date("2021-06-08")), aes(x = as.Date(dates))) +
  theme_linedraw() +
  theme(strip.text.x = element_text(size = 12)) +
  geom_bar(stat = "identity", aes(y = I), size = 1.0, color = "firebrick4", fill = "firebrick4") + 
  labs(title = "", x = "", y = "New Cases/Day")
print(g_num_schools)

g_county <- plot_grid(g_rt_counties, g_num_counties, nrow = 2, ncol = 1, align = "hv", rel_heights = c(1, 0.4))
print(g_county)

plot_grid(g_county, g_schools, nrow = 1, ncol = 2, align = "hv")
