################################################################################
### Graph:  COVID-19 New active
################################################################################

new_active_df <-
  new_active_tib %>%
  select(Date, Total) %>%
  filter(!is.na(Total)) %>%
  mutate(Date = as.Date(Date)) %>%
  rename(new_active = Total)

new_num <-  new_active_df %>%  tail(n = 1) %>% select("new_active") %>% pull() %>% format(big.mark = ",", scientific = FALSE)
SMA <- new_active_df$new_active %>% SMA(n = 7) %>% tail(n = 1) %>% round() %>% format(big.mark = ",", scientific = FALSE)

newact_title <- paste("Change in Active Cases: ", new_num, ", SMA: ", SMA, sep = "")

graph_new_active <- ggplot(data = new_active_df, aes(x = as.Date(Date), y = new_active)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.7)) +
  theme(axis.text.x = element_text(hjust = 0.8)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  #geom_bar(stat = "identity", fill = graph_color) +
  geom_point(shape = 19, size = 0.5,
             aes(x = as.Date(Date), y = new_active), color = "black") +

  geom_hline(yintercept = 0, alpha = 0.5) +

  geom_line(data = active_data, color = graph_color, size = line_thickness,
            aes(x = as.Date(date) - 3, y = SMA(new_active, n = 7))) +

  scale_x_date(#date_breaks = "3 days",
    date_labels = "%m/%d") +
  scale_y_continuous(labels = scales::comma) +
  #  graph_log10_opts1 +
  #  graph_log10_opts2 +
  labs(title = newact_title, x = "", y = "")
#print(graph_new_active)
