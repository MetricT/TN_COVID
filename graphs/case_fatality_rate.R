################################################################################
### Graph:  Case Fatality Rate by Date
################################################################################

### Compute CFR as total_deaths / total_cases
cfr_data <-
  cases_data %>%
  full_join(deaths_data, by = "date") %>%
  mutate(cfr_total = total_deaths / total_cases) %>%
  select(date, cfr_total) %>%
  filter(!is.na(cfr_total))


### What's the current number of total cases
new_cfr_num <- cfr_data %>% pull("cfr_total")                %>% tail(n = 1) %>% round(digits = 4)
SMA         <- cfr_data %>% pull("cfr_total") %>% SMA(n = 7) %>% tail(n = 1) %>% round(digits = 4)

cfr_title <- paste("Case Fatality Rate: ", new_cfr_num * 100, "%, SMA: ", SMA * 100, "%", sep = "")

### Graph:  new COVID-19 Cases
graph_case_fatality_rate <- ggplot(data = cfr_data) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.7)) +
  theme(axis.text.x = element_text(hjust = 0.8)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  geom_point(data = cfr_data, shape = 19, size = 0.5,
             aes(x = as.Date(date), y = cfr_total), color = "black") +
  geom_line(data = cfr_data, color = graph_color, size = 1.0,
            aes(x = as.Date(date) - 3, y = SMA(cfr_total, n = 7))) +
  scale_x_date(#date_breaks = "3 days",
    date_labels = "%m/%d") +
  scale_y_continuous(labels = scales::percent) +
  graph_log10_opts1 +
  graph_log10_opts2 +
  labs(title = cfr_title, x = "", y = "")
#print(graph_case_fatality_rate)
