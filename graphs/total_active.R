################################################################################
### Graph:  COVID-19 Total active
################################################################################

new_num <-  active_data %>%  tail(n = 1) %>% select("total_active") %>% pull() %>% format(big.mark = ",", scientific = FALSE)

totact_title <- paste("Active Cases: ", new_num, sep = "")

graph_total_active <- ggplot(data = active_data, aes(x = as.Date(date), y = total_active)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.7)) +
  theme(axis.text.x = element_text(hjust = 0.8)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  #geom_bar(stat = "identity", fill = graph_color) +
  geom_point(data = active_data, shape = 19, size = 0.5,
             aes(x = as.Date(date), y = total_active), color = "black") +

  geom_line(data = active_data, color = graph_color, size = line_thickness,
            aes(x = as.Date(date) - 3, y = SMA(total_active, n = 7))) +

  scale_x_date(#date_breaks = "3 days",
               date_labels = "%m/%d") +
  scale_y_continuous(labels = scales::comma) +
  #  graph_log10_opts1 +
  #  graph_log10_opts2 +
  labs(title = totact_title, x = "", y = "")
#print(graph_total_active)
