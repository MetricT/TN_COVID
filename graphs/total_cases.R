################################################################################
### Graph:  COVID-19 Total Cases
################################################################################

### The current number of total cases
total_num <-
  cases_data %>%
  tail(n = 1) %>%
  pull("total_cases") %>%
  format(big.mark = ",", scientific = FALSE)

# What's the date of the last data point
data_date <- cases_data  %>% select("date") %>% tail(n = 1) %>% pull()

title_string <- paste("COVID-19 Cases Reported - ", location,
                      " [", as.Date(data_date), "]", sep = "")

graph_total_cases <- ggplot(data = cases_data) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.7)) +
  theme(axis.text.x = element_text(hjust = 0.8)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  geom_point(data = cases_data, shape = 19, size = 0.5,
             aes(x = as.Date(date), y = total_cases), color = "black") +
  geom_line(data = cases_data, color = graph_color, size = 1.0,
            aes(x = as.Date(date) - 3, y = SMA(total_cases, n = 7))) +
  scale_x_date(date_breaks = "3 days", date_labels = "%m/%d") +
  #  geom_vline(xintercept = as.Date("2020-03-15")) +
  graph_log10_opts1 +
  graph_log10_opts2 +
  labs(title = paste("Total Cases: ", total_num, sep = ""), x = "", y = "")
print(graph_total_cases)
