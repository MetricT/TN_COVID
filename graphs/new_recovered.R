################################################################################
### Graph:  COVID-19 New Recovered
################################################################################

new_rec_num <- recovered_data %>% tail(n = 1) %>% select("new_recovered") %>% pull() %>% format(big.mark = ",", scientific = FALSE)

SMA <- recovered_data$new_recovered %>% SMA(n = 7) %>% tail(n = 1) %>% round() %>% format(big.mark = ",", scientific = FALSE)

newrec_title <- paste("New Recovered: ", new_rec_num, ", SMA: ", SMA, sep = "")

graph_new_recover <- ggplot(data = recovered_data, aes(x = as.Date(date), y = new_recovered)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.7)) +
  theme(axis.text.x = element_text(hjust = 0.8)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  geom_point(data = recovered_data, shape = 19, size = 0.5,
             aes(x = as.Date(date), y = new_recovered), color = "black") +
  
  geom_line(data = recovered_data, color = graph_color, size = line_thickness,
            aes(x = as.Date(date) - 3, 
                y = SMA(new_recovered, n = 7))) + 
  scale_y_continuous(labels = scales::comma) + 
  
  scale_x_date(#date_breaks = "3 days", 
               date_labels = "%m/%d") +
  #  graph_log10_opts1 +
  #  graph_log10_opts2 +
  labs(title = newrec_title, x = "", y = "")
print(graph_new_recover)