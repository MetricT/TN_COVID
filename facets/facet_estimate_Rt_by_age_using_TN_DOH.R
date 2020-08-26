################################################################################
### Compute Rt (the reproduction number) for the given location and age groups
################################################################################

### Load the necessary libraries
packages <- 
  c("tidyverse", "lubridate", "tsibble", "EpiEstim", "curl", "forecast", "TTR")

new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages, quiet = TRUE)
invisible(lapply(packages, "library", quietly = TRUE,
                 character.only = TRUE, warn.conflicts = FALSE))

### Choose the counties you want to to view.
my_location <- c("Davidson")

################################################################################
### Fetching data.   The EpiEstim::estimate_R() function expects to be handed a
### data.frame or tibble with two columns:   "dates" (date of confirmed case)
### and "I" (number of new incidents/cases).   I add a third column "location"
### so I can make facet graphs of multiple locations at once.
################################################################################

### Function to read Excel spreadsheet from URL's
read_excel_url <- function(url, ...) {
  tf <- tempfile(fileext = ".xlsx")
  curl::curl_download(url, tf)
  return(readxl::read_excel(tf, ...))
}

### URL's for the spreadsheets
url <- "https://www.tn.gov/content/dam/tn/health/documents/cedep/novel-coronavirus/datasets/Public-Dataset-Daily-County-Age-Group.XLSX"

spreadsheet  <-
  read_excel_url(url) %>%
  mutate(DATE = as.Date(DATE))

### Now I need to patch in the missing date (6/28) when the state's computers went down
patch_csv <-
  "patches/2020-06-28/age_by_county.csv" %>%
  read_csv(col_names = TRUE, col_types = "Dccd")

spreadsheet <-
  spreadsheet %>%
  bind_rows(patch_csv) %>%
  unique()

data <- 
  spreadsheet %>%
  filter(AGE_GROUP != "Pending") %>%
  filter(COUNTY == my_location) %>%
  rename(dates = DATE,
         I     = CASE_COUNT,
         facet_by = AGE_GROUP) %>%
  
  ### The data above is total case count.   Pivot and compute new case count
  pivot_wider(id_cols = "dates", names_from = "facet_by", values_from = "I") %>%
  mutate(across(!starts_with("dates"),
                .fns = list(new = ~ c(0, diff(.))),
                .names = "{fn}_{col}"
  )) %>%
  select(dates, starts_with("new_")) %>%
  rename_at(vars(starts_with("new_")),
            ~ str_replace(., "new_", "")) %>%
  pivot_longer(-dates, names_to = "facet_by", values_to = "I") %>%
  
  # Filter out cases where "I" is a NA
  filter(!is.na(I)) %>%
  
  # Also, estimate_R can't handle negative numbers, so set negative values to 0.
  # These are usually due to suspected cases being lumped in and subsequently 
  # found to not be COVID.
  mutate(I = if_else(I < 0, 0, I)) %>%
  filter(dates >= as.Date("2020-06-25"))

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

### Iterate over all facets and generate the Rt estimatefor that group
for (this_facet in data %>% pull("facet_by") %>% unique()) {
  
  print(this_facet)
  
  data_subset <-
    data %>%
    filter(facet_by == this_facet) %>%
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
    mutate(facet_by = this_facet)
  
  Rt_tib <-
    Rt_tib %>%
    bind_rows(r_data)
  
}

### Take the Rt_tib estimates and compute the 7-day moving average
trend_tib <-
  Rt_tib %>%
  select(dates, facet_by, mean_r) %>% 
  mutate(facet_by = paste("values:", facet_by, sep = "")) %>%
  pivot_wider(id_cols = "dates", 
              names_from = "facet_by", 
              values_from = "mean_r") %>%
  
  mutate(across(starts_with("values_"),
                .fns = list(trend = ~ model(STL(. ~ trend())) %>% components() %>% pull("trend")),
                .names = "{fn}_{col}")) #%>%
  
  # Use mstl() %>% trendcyle()
#  mutate(across(starts_with("values:"),
#                .fns = list(trend = ~ (.) %>% ts() %>% mstl() %>% trendcycle()),
#                .names = "{fn}_{col}")) %>%

  select("dates", starts_with("trend_values")) %>%
  rename_at(vars(starts_with("trend_values:")),
            ~ str_replace(., "trend_values:", "")) %>%
  pivot_longer(-dates, names_to = "facet_by", values_to = "trend")


### Add our trend and mask mandate data to the Rt tibble
Rt_tib <-
  Rt_tib %>%
  left_join(trend_tib, by = c("dates" = "dates", "facet_by" = "facet_by"))


################################################################################
### Render the graph and done
################################################################################
### Title for our graph
title <- "Estimated Rt"

subtitle <- paste("assuming mean(serial interval) = ", si_mean, 
                  " days and std(serial interval) = ", si_std, " days", sep = "")

### Order counties in the order given in my_location at the top of the script
#Rt_tib$facet_by <- factor(Rt_tib$facet_by, levels = my_location)

g_rt_schools <-
  ggplot(data = Rt_tib, aes(x = as.Date(dates))) +
  theme_linedraw() +
  theme(strip.text.x = element_text(size = 16)) +
  geom_point(aes(y = mean_r), size = 1.0) + 
  geom_ribbon(aes(ymin = mean_r - std_r, 
                  ymax = mean_r + std_r),
              color = NA, fill = "darkgrey", alpha = 0.2) +
  
  geom_line(aes(y = trend), color = "darkseagreen4", size = 1.2) +
  
  geom_hline(yintercept = 1, linetype = "dashed") + 
  
  facet_wrap(~facet_by) + 
   
  scale_y_continuous(limits = c(0, 2)) +
  
  labs(title = title, x = "", y = "Rt")
print(g_rt_schools)
