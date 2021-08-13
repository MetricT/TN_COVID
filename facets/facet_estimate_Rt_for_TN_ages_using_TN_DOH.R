### EpiEstim and other useful epidemic analysis packages available from:
###
### # https://www.repidemicsconsortium.org/

library(tidyverse)
library(tsibble)
library(feasts)
library(EpiEstim)
library(forecast)

### Function to read Excel spreadsheets from URL's since the "readxl" package is
### behind the times
read_excel_url <- function(url, ...) {
  tf <- tempfile(fileext = ".xlsx")
  curl::curl_download(url, tf)
  return(readxl::read_excel(tf, ...))
}

spreadsheet <- 
  read_excel_url("https://www.tn.gov/content/dam/tn/health/documents/cedep/novel-coronavirus/datasets/Public-Dataset-Age.XLSX")
  
data <-
  spreadsheet %>%
  select(DATE, AR_NEWCASES, AGE_RANGE) %>%
  rename(dates = DATE,
         I = AR_NEWCASES,
         age_range = AGE_RANGE) %>%
  mutate(dates = as.Date(dates)) %>%
  filter(age_range != "Pending") %>%
  pivot_wider(id_cols = "dates", names_from = "age_range", values_from = "I") %>%
  as_tsibble(index = "dates") %>%
  tsibble::fill_gaps() %>%
#  filter(!dates %in% c(as.Date("2020-06-28"), as.Date("2020-06-29"))) %>%
#  add_row(dates = as.Date("2020-06-28"),
#          "0-10 years"  = 61,
#          "11-20 years" = 117,
#          "21-30 years" = 294,
#          "31-40 years" = 195,
#          "41-50 years" = 180,
#          "51-60 years" = 117,
#          "61-70 years" = 76,
#          "71-80 years" = 42,
#          "81+ years"   = 18
#          ) %>%
#  add_row(dates = as.Date("2020-06-29"),
#          "0-10 years"  = 62,
#          "11-20 years" = 117,
#          "21-30 years" = 295,
#          "31-40 years" = 195,
#          "41-50 years" = 181,
#          "51-60 years" = 118,
#          "61-70 years" = 76,
#          "71-80 years" = 43,
#          "81+ years"   = 19  ) %>%
  arrange(dates) %>%
  
  # Fix negative values by averaging them with the previous day's value
  mutate("81+ years" = if_else(dates == as.Date("2020-04-15"), 3, `81+ years`)) %>%
  mutate("81+ years" = if_else(dates == as.Date("2020-04-14"), 4, `81+ years`)) %>%

  mutate("81+ years" = if_else(dates == as.Date("2020-04-12"), 8, `81+ years`)) %>%
  mutate("81+ years" = if_else(dates == as.Date("2020-04-11"), 8, `81+ years`)) %>%
  
  mutate("0-10 years" = if_else(dates == as.Date("2020-04-09"), 2, `0-10 years`)) %>%
  mutate("0-10 years" = if_else(dates == as.Date("2020-04-08"), 2, `0-10 years`)) %>%
  mutate("0-10 years" = if_else(dates == as.Date("2020-04-07"), 2, `0-10 years`)) %>%
  
  mutate("0-10 years" = if_else(dates == as.Date("2020-04-05"), 1, `0-10 years`)) %>%
  mutate("0-10 years" = if_else(dates == as.Date("2020-04-04"), 2, `0-10 years`)) %>%
  
  mutate("41-50 years" = if_else(dates == as.Date("2020-04-02"), 43, `41-50 years`)) %>%
  mutate("41-50 years" = if_else(dates == as.Date("2020-04-01"), 43, `41-50 years`)) %>%
  
  mutate("81+ years" = if_else(dates == as.Date("2020-03-20"), 2, `81+ years`)) %>%
  mutate("81+ years" = if_else(dates == as.Date("2020-03-19"), 2, `81+ years`)) %>%
  
  as_tibble() %>%
  pivot_longer(-dates, names_to = "age_group", values_to = "I")


foo <-
  age_by_county_df %>% 
  filter(COUNTY == "Cheatham") %>% 
  filter(AGE_GROUP != "Pending") %>%
  select(-COUNTY) %>% 
  pivot_wider(id_cols = "DATE", names_from = "AGE_GROUP", values_from = "TOTAL_CASES") %>%
  mutate(across(!starts_with("DATE"),
              .fns = list(new = ~ c(0, diff(.))),
              .names = "{fn}_{col}")) %>%
  select(DATE, starts_with("new_")) %>%
  rename_at(vars(starts_with("new_")),
            ~ str_replace(., "new_", "")) #%>%
  #pivot_longer(cols = !starts_with("DATE"),
  #             names_to = "AGE_GROUP",
  #             values_to = "CASE_COUNT")
  
  


### Serial Interval estimates from:
### https://wwwnc.cdc.gov/eid/article/26/6/20-0357_article
si_mean <- 3.96
si_std  <- 4.75

### Get a list of unique counties and age groups so we can iterate over thhem
#all_counties   <- data %>% pull("county") %>% unique()
all_age_groups <- data %>% pull("age_group") %>% unique()

### Create a blank tibble to hold our EpiEstim output
Rt_tib <-
  tribble(
    ~dates, ~mean_r, ~std_r, ~age_group,
  )

### Iterate over all countys and generate the EpiEstim estimate for Rt
for (this_age_group in all_age_groups) {
  
  print(this_age_group)
  
  data_subset <-
    data %>%
    filter(age_group == this_age_group) %>%
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
    mutate(age_group = this_age_group)
  
  Rt_tib <-
    Rt_tib %>%
    bind_rows(r_data)

}

### Take the Rt_tib estimates and compute mstl() trends
trend_tib <-
  Rt_tib %>%
  select(dates, age_group, mean_r) %>% 
  mutate(age_group = paste("values:", age_group, sep = "")) %>%
  pivot_wider(id_cols = "dates", 
              names_from = "age_group", 
              values_from = "mean_r") %>%
  mutate(across(starts_with("values:"),
                .fns = list(trend = ~ (.) %>% ts() %>% mstl() %>% trendcycle()),
                .names = "{fn}_{col}")) %>%
  select("dates", starts_with("trend_values")) %>%
  rename_at(vars(starts_with("trend_values:")),
            ~ str_replace(., "trend_values:", "")) %>%
  pivot_longer(-dates, names_to = "age_group", values_to = "trend")


### Add our trend and mask mandate data to the Rt tibble
Rt_tib <-
  Rt_tib %>%
  left_join(trend_tib, by = c("dates" = "dates", "age_group" = "age_group"))

### Title for our graph
title <- "Estimate for Rt in Tennessee by age group"
subtitle <- paste("assuming mean(serial interval) = ", si_mean, 
                  " days and std(serial interval) = ", si_std, " days", sep = "")

### Order counties in the order given in my_location at the top of the script
Rt_tib$age_group <- factor(Rt_tib$age_group, levels = all_age_groups)

Rt_tib <-
  Rt_tib %>%
  filter(dates >= as.Date("2021-05-01"))

### Render the graph and done!
g_rt_age_group <-
  ggplot(data = Rt_tib, aes(x = as.Date(dates))) +
  theme_linedraw() +
  theme(strip.text.x = element_text(size = 16)) +
  geom_point(aes(y = mean_r), size = 1.0) + 
  geom_line(aes(y = mean_r), size = 1.0) + 
  #geom_smooth(aes(y = mean_r)) + 
  geom_ribbon(aes(ymin = mean_r - std_r, 
                  ymax = mean_r + std_r),
              color = NA, fill = "darkgrey", alpha = 0.2) +
  
  #geom_line(aes(y = trend), color = "firebrick4", size = 1.2) +
  
  geom_hline(yintercept = 1, linetype = "dashed") + 
  #scale_y_continuous(limits = c(0.75, 1.25)) +
  facet_wrap(~ age_group, scales = "fixed") +
  labs(title = title, x = "", y = "Rt")
print(g_rt_age_group)
