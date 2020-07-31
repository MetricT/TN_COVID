################################################################################
### Graph:  COVID-19 New Confirmed Cases
################################################################################

### Set any reversions (ie, when TN DOH revises cases downwards due to error) to 0
cases_data <-
  cases_data %>%
  mutate(new_cases = if_else(new_cases < 0, 0, new_cases))

### What's the current number of total cases cases
new_num <- cases_data %>% tail(n = 1) %>% select("new_cases") %>% pull() %>% format(big.mark = ",", scientific = FALSE)
SMA <- cases_data$new_cases %>% SMA(n = 7) %>% tail(n = 1) %>% round() %>% format(big.mark = ",", scientific = FALSE)

newinf_title <- paste("New Cases: ", new_num, ", SMA: ", SMA, sep = "")

### Graph:  new COVID-19 Cases
graph_new_cases <- ggplot(data = cases_data) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.7)) +
  theme(axis.text.x = element_text(hjust = 0.8)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  geom_point(data = cases_data, shape = 19, size = 0.5,
             aes(x = as.Date(date), y = new_cases), color = "black") +
  geom_line(data = cases_data, color = graph_color, size = 1.0,
            aes(x = as.Date(date) - 3, y = SMA(new_cases, n = 7))) +
  scale_x_date(#date_breaks = "3 days",
               date_labels = "%m/%d") +
  scale_y_continuous(labels = scales::comma) +
  #  scale_y_continuous(limits = c(100, 650)) +
  graph_log10_opts1 +
  graph_log10_opts2 +
  labs(title = newinf_title, x = "", y = "")
#print(graph_new_cases)
