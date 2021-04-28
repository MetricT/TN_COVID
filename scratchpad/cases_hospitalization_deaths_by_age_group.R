### Function to read Excel spreadsheets from URL's since the "readxl" package is
### behind the times
read_excel_url <- function(url, ...) {
  tf <- tempfile(fileext = ".xlsx")
  curl::curl_download(url, tf)
  return(readxl::read_excel(tf, ...))
}

this_url <- "https://www.tn.gov/content/dam/tn/health/documents/cedep/novel-coronavirus/datasets/Public-Dataset-Daily-Age-Group-Outcomes.XLSX"

outcome_df <-
  read_excel_url(this_url) %>%
  mutate(DATE = as.Date(DATE)) %>%
  mutate(AGE_GROUP = if_else(grepl("21­30 years", AGE_GROUP), "21-30 years", AGE_GROUP)) %>%
  mutate(AGE_GROUP = if_else(AGE_GROUP == "61­70 years", "61-70 years", AGE_GROUP)) %>%
  mutate(AGE_GROUP = if_else(AGE_GROUP == "71-80 yearó", "71-80 years", AGE_GROUP))

this_name <- "Cases"
#this_name <- "Hospitalizations"
#this_name <- "Deaths"

hosp_df <-
  outcome_df %>%
  
  select(DATE, AGE_GROUP, HOSP_COUNT) %>%
  rename(COUNT = HOSP_COUNT) %>%
  
  filter(AGE_GROUP != "Pending") %>%
  group_by(DATE, AGE_GROUP) %>%
  summarize(COUNT = sum(COUNT)) %>%
  ungroup() %>%
  pivot_wider(id_cols = "DATE", names_from = "AGE_GROUP", values_from = "COUNT") %>% 
  
  # Take the total count and derive new count
  mutate(across(!starts_with("DATE"), .fns = list(new = ~ c(0, diff(.))), .names = "{fn}_{col}")) %>% 
  select("DATE", starts_with("new")) %>% 
  rename_at(vars(starts_with("new_")), ~ str_replace(., "new_", "")) %>% 
  
  # Take the new count and do a 7-day SMA to smooth it
  mutate(across(!starts_with("DATE"), .fns = list(sma = ~ SMA(., n = 7)), .names = "{fn}_{col}")) %>%
  select("DATE", starts_with("sma")) %>% 
  rename_at(vars(starts_with("sma_")), ~ str_replace(., "sma_", "")) %>% 
  
  pivot_longer(-DATE, names_to = "AGE_GROUP", values_to = "COUNT") %>%
  filter(!is.na(COUNT)) %>%
  mutate(COUNT = if_else(COUNT < 0, 0, COUNT))

case_df <-
  outcome_df %>%
  
  select(DATE, AGE_GROUP, CASE_COUNT) %>%
  rename(COUNT = CASE_COUNT) %>%
  filter(AGE_GROUP != "Pending") %>%
  group_by(DATE, AGE_GROUP) %>%
  summarize(COUNT = sum(COUNT)) %>%
  ungroup() %>%
  pivot_wider(id_cols = "DATE", names_from = "AGE_GROUP", values_from = "COUNT") %>% 
  
  # Take the total count and derive new count
  mutate(across(!starts_with("DATE"), .fns = list(new = ~ c(0, diff(.))), .names = "{fn}_{col}")) %>% 
  select("DATE", starts_with("new")) %>% 
  rename_at(vars(starts_with("new_")), ~ str_replace(., "new_", "")) %>% 
  
  # Take the new count and do a 7-day SMA to smooth it
  mutate(across(!starts_with("DATE"), .fns = list(sma = ~ SMA(., n = 7)), .names = "{fn}_{col}")) %>%
  select("DATE", starts_with("sma")) %>% 
  rename_at(vars(starts_with("sma_")), ~ str_replace(., "sma_", "")) %>% 
  
  pivot_longer(-DATE, names_to = "AGE_GROUP", values_to = "COUNT") %>%
  filter(!is.na(COUNT)) %>%
  mutate(COUNT = if_else(COUNT < 0, 0, COUNT))

death_df <-
  outcome_df %>%
  
  select(DATE, AGE_GROUP, DEATH_COUNT) %>%
  rename(COUNT = DEATH_COUNT) %>%
  filter(AGE_GROUP != "Pending") %>%
  group_by(DATE, AGE_GROUP) %>%
  summarize(COUNT = sum(COUNT)) %>%
  ungroup() %>%
  pivot_wider(id_cols = "DATE", names_from = "AGE_GROUP", values_from = "COUNT") %>% 
  
  # Take the total count and derive new count
  mutate(across(!starts_with("DATE"), .fns = list(new = ~ c(0, diff(.))), .names = "{fn}_{col}")) %>% 
  select("DATE", starts_with("new")) %>% 
  rename_at(vars(starts_with("new_")), ~ str_replace(., "new_", "")) %>% 
  
  # Take the new count and do a 7-day SMA to smooth it
  mutate(across(!starts_with("DATE"), .fns = list(sma = ~ SMA(., n = 7)), .names = "{fn}_{col}")) %>%
  select("DATE", starts_with("sma")) %>% 
  rename_at(vars(starts_with("sma_")), ~ str_replace(., "sma_", "")) %>% 
  
  pivot_longer(-DATE, names_to = "AGE_GROUP", values_to = "COUNT") %>%
  filter(!is.na(COUNT)) %>%
  mutate(COUNT = if_else(COUNT < 0, 0, COUNT))




my_palette <- viridis(n = 9, direction = 1, option = "inferno")

################################################################################
### Draw cases by age
################################################################################
g_cases_by_age <-
  ggplot(data = case_df, aes(x = as.Date(DATE), y = COUNT)) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(strip.background = element_blank()) +
  geom_line(aes(y = COUNT), color = "black", size = 0.2) +
  geom_area(aes(fill = as.factor(AGE_GROUP))) +
  labs(title = "Breakdown by Age", x = "", y = "") +
  scale_fill_manual(values = my_palette) + 
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~ AGE_GROUP, nrow = 9, ncol = 1, strip.position = "right", scale = "free")
print(g_cases_by_age)


g_cases_stacked <-
  ggplot(data = case_df, aes(x = as.Date(DATE), y = COUNT, fill = AGE_GROUP)) + 
  theme_linedraw() + 
  geom_area(color="black", size = 0.2, alpha = 0.8) +
  scale_fill_manual(name = "Age Group", values = my_palette) + 
  scale_y_continuous(labels = scales::comma) + 
  labs(title = paste("New COVID-19 Cases in Tennessee by Age", sep = ""), x = "",
       y = this_name)
print(g_cases_stacked)

cases_per_age_data <-
  case_df %>%
  group_by(DATE, AGE_GROUP) %>%
  summarise(n = sum(COUNT, na.rm = TRUE)) %>%
  mutate(percentage = n / sum(n)) %>%
  select(-n)

g_cases_stacked_per <-
  ggplot(data = cases_per_age_data, aes(x = as.Date(DATE), y = percentage, fill = AGE_GROUP)) + 
  theme_linedraw() + 
  theme(#legend.title = element_blank(),
    #legend.position = "none",
    plot.background = element_rect(fill = "transparent",colour = NA)) +
  geom_area(color="black", size = 0.2, alpha = 0.8) +
  scale_fill_manual(name = "Age Group", values = my_palette) + 
  scale_y_continuous(labels = scales::percent) + #, limits = c(0, 0.4)) + 
  labs(title = "TN Proportion of New Cases", x = "Date", y = "")
print(g_cases_stacked_per)


################################################################################
### Draw hospitalized by age
################################################################################
g_hosp_by_age <-
  ggplot(data = hosp_df, aes(x = as.Date(DATE), y = COUNT)) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(strip.background = element_blank()) +
  geom_line(aes(y = COUNT), color = "black", size = 0.2) +
  geom_area(aes(fill = as.factor(AGE_GROUP))) +
  labs(title = "Breakdown by Age", x = "", y = "") +
  scale_fill_manual(values = my_palette) + 
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~ AGE_GROUP, nrow = 9, ncol = 1, strip.position = "right", scale = "free")
print(g_hosp_by_age)


g_hosp_stacked <-
  ggplot(data = hosp_df, aes(x = as.Date(DATE), y = COUNT, fill = AGE_GROUP)) + 
  theme_linedraw() + 
  geom_area(color="black", size = 0.2, alpha = 0.8) +
  scale_fill_manual(name = "Age Group", values = my_palette) + 
  scale_y_continuous(labels = scales::comma) + 
  labs(title = paste("New COVID-19 Hospitalized in Tennessee by Age", sep = ""), x = "",
       y = this_name)
print(g_hosp_stacked)


hosp_per_age_data <-
  hosp_df %>%
  group_by(DATE, AGE_GROUP) %>%
  summarise(n = sum(COUNT, na.rm = TRUE)) %>%
  mutate(percentage = n / sum(n)) %>%
  select(-n)

g_hosp_stacked_per <-
  ggplot(data = hosp_per_age_data, aes(x = as.Date(DATE), y = percentage, fill = AGE_GROUP)) + 
  theme_linedraw() + 
  theme(#legend.title = element_blank(),
    #legend.position = "none",
    plot.background = element_rect(fill = "transparent",colour = NA)) +
  geom_area(color="black", size = 0.2, alpha = 0.8) +
  scale_fill_manual(name = "Age Group", values = my_palette) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(title = "TN Proportion of New Hospitalizations", x = "Date", y = "")
print(g_hosp_stacked_per)

################################################################################
### Draw deaths by age
################################################################################
g_deaths_by_age <-
  ggplot(data = death_df, aes(x = as.Date(DATE), y = COUNT)) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(strip.background = element_blank()) +
  geom_line(aes(y = COUNT), color = "black", size = 0.2) +
  geom_area(aes(fill = as.factor(AGE_GROUP))) +
  labs(title = "Breakdown by Age", x = "", y = "") +
  scale_fill_manual(values = my_palette) + 
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~ AGE_GROUP, nrow = 9, ncol = 1, strip.position = "right", scale = "free")
print(g_deaths_by_age)


g_deaths_stacked <-
  ggplot(data = case_df, aes(x = as.Date(DATE), y = COUNT, fill = AGE_GROUP)) + 
  theme_linedraw() + 
  geom_area(color="black", size = 0.2, alpha = 0.8) +
  scale_fill_manual(name = "Age Group", values = my_palette) + 
  scale_y_continuous(labels = scales::comma) + 
  labs(title = paste("New COVID-19 Deaths in Tennessee by Age", sep = ""), x = "",
       y = this_name)
print(g_deaths_stacked)

deaths_per_age_data <-
  death_df %>%
  group_by(DATE, AGE_GROUP) %>%
  summarise(n = sum(COUNT, na.rm = TRUE)) %>%
  mutate(percentage = n / sum(n)) %>%
  select(-n)

g_deaths_stacked_per <-
  ggplot(data = deaths_per_age_data, aes(x = as.Date(DATE), y = percentage, fill = AGE_GROUP)) + 
  theme_linedraw() + 
  theme(#legend.title = element_blank(),
    #legend.position = "none",
    plot.background = element_rect(fill = "transparent",colour = NA)) +
  geom_area(color="black", size = 0.2, alpha = 0.8) +
  scale_fill_manual(name = "Age Group", values = my_palette) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(title = "TN Proportion of New Deaths", x = "Date", y = "")
print(g_deaths_stacked_per)


################################################################################
### Draw the inset stacked graph percent chart in the upper-right
################################################################################

stacked <- plot_grid(g_hosp_stacked,
                     g_hosp_stacked_per,
                     nrow = 2, ncol = 1, align = "hv")
print(stacked)

final <- plot_grid(stacked, g_hosp_by_age, nrow = 1, ncol = 2, align = "hv", rel_widths = c(1, 0.2))
print(final)

stacked <- plot_grid(g_cases_stacked_per,
                     g_hosp_stacked_per,
                     g_deaths_stacked_per,
                     nrow = 3, ncol = 1, align = "hv")
print(stacked)
  

