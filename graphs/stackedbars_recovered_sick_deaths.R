################################################################################
### Stacked Bars:  Recovered, Sick, Deaths
################################################################################

rsd_df <-
  merge_data %>%
  select(date, totaldeaths, totalrecovered, current_sick) %>%
  rename(Deaths = totaldeaths) %>%
  rename(Recovered = totalrecovered) %>%
  rename(Sick = current_sick) %>%
  gather(key, value, -date) %>%
  mutate(key = as.factor(key)) %>%
  mutate(key = factor(key, levels = c("Recovered", "Sick", "Deaths")))

palette <- c("springgreen4", "goldenrod2", "firebrick2")

p_sum_stack <-
  ggplot(data = rsd_df, aes(fill = key, x = as.Date(date), y = value)) +
  scale_fill_manual(values = palette) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_breaks = "3 days", date_labels = "%m/%d") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.7)) +
  theme(axis.text.x = element_text(hjust = 0.8)) +
  labs(title = "TN COVID-19:  Recovered, Sick, Deaths",
       x = "Date", y = "Cases") +
  geom_bar(position = "stack", stat = "identity")
plot(p_sum_stack)

p_sum_fill <-
  ggplot(data = rsd_df, aes(fill = key, x = as.Date(date), y = value)) +
  scale_fill_manual(values = palette) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(date_breaks = "3 days", date_labels = "%m/%d") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.7)) +
  theme(axis.text.x = element_text(hjust = 0.8)) +
  labs(title = "TN COVID-19:  Recovered, Sick, Deaths as % of Total",
       x = "Date", y = "Percent of all cases") +
  geom_bar(position = "fill", stat = "identity")
plot(p_sum_fill)

sum <- plot_grid(p_sum_stack,
                 p_sum_fill,
                 ncol = 1, nrow = 2)
plot(sum)