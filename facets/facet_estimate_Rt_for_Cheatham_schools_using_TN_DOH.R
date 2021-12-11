################################################################################
### Compute Rt (the reproduction number) for the given locations and populations
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
my_location <- c("Cheatham")

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

###  Alternative way of collecting data, not used right now
#data <- 
#  age_by_county_df %>%
#  filter(AGE_GROUP %in% c("0-10 years", 
#                          "11-20 years")) %>% 
#  group_by(DATE, COUNTY) %>% 
#  summarize(I = sum(CASE_COUNT)) %>%
#  ungroup() %>%
#  rename(dates = DATE,
#         location = COUNTY) %>%
#  
#  ### The data above is total case count.   Pivot and compute new case count
#  pivot_wider(id_cols = "dates", names_from = "location", values_from = "I") %>%
#  mutate(across(!starts_with("dates"),
#                .fns = list(new = ~ c(0, diff(.))),
#                .names = "{fn}_{col}"
#  )) %>%
#  select(dates, starts_with("new_")) %>%
#  rename_at(vars(starts_with("new_")),
#            ~ str_replace(., "new_", "")) %>%
#  pivot_longer(-dates, names_to = "location", values_to = "I") %>%
#  
#  # Filter it to just the locations specified
#  filter(location %in% my_location) %>%
#  
#  # Filter out cases where "I" is a NA
#  filter(!is.na(I)) %>%
#  
#  # Also, estimate_R can't handle negative numbers, so set negative values to 0.
#  # These are usually due to suspected cases being lumped in and subsequently 
#  # found to not be COVID.
#  mutate(I = if_else(I < 0, 0, I)) %>%
#  filter(date >= as.Date("2020-07-01"))


### Load case data for schools
data <-
  "https://www.tn.gov/content/dam/tn/health/documents/cedep/novel-coronavirus/datasets/Public-Dataset-Daily-County-Cases-5-18-Years.XLSX" %>%
  read_excel_url() %>%
  mutate(DATE = as.Date(DATE)) %>%
  select(DATE, NEW_CASES, COUNTY) %>%
  filter(DATE >= as.Date("2021-06-01")) %>%
  rename(dates    = DATE,
         I        = NEW_CASES,
         location = COUNTY) %>%
  
  arrange(dates) %>%
  unique() %>%
  
  # Filter it to just the locations specified
  filter(location %in% my_location) %>%
  
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
  left_join(trend_tib, by = c("dates" = "dates", "location" = "location")) #%>%
  #left_join(start_dates, by = c("location" = "county"))


################################################################################
### Render the graph and done
################################################################################
Rt_tib <- 
  Rt_tib %>% 
  left_join(data %>% select(-location), by = "dates") %>%
  mutate(location = paste("Rt estimate - ", my_location, " Co. Ages 5-18 -", Rt_tib %>% tail(n = 1) %>% pull("dates")))

g_rt_schools <-
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
  
  #geom_vline(aes(xintercept = as.Date(first_day)), linetype = "dotted") +
 
  facet_wrap(~location) + 
   
  scale_y_continuous(limits = c(0, 2)) +
  
  labs(title = "", x = "", y = "Rt")
print(g_rt_schools)

g_num_schools <- 
  ggplot(data = data %>% filter(dates >= as.Date("2021-06-08")), aes(x = as.Date(dates))) +
  theme_linedraw() +
  theme(strip.text.x = element_text(size = 12)) +
  geom_bar(stat = "identity", aes(y = I), size = 1.0, color = "firebrick4", fill = "firebrick4") + 

  labs(title = "", x = "", y = "New Cases/Day")
print(g_num_schools)

g_schools <- plot_grid(g_rt_schools, g_num_schools, nrow = 2, ncol = 1, align = "hv", rel_heights = c(1, 0.4))
print(g_schools)
