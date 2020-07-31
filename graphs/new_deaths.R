################################################################################
### Graph:  COVID-19 New Deaths
################################################################################

new_deaths_num <- deaths_data %>% tail(n = 1) %>% select("new_deaths") %>% pull() %>% format(big.mark = ",", scientific = FALSE)
SMA <- deaths_data$new_deaths %>% SMA(n = 7) %>% tail(n = 1) %>% round() %>% format(big.mark = ",", scientific = FALSE)

  newinf_title <- paste("New Deaths: ", new_deaths_num, ", SMA: ", SMA, sep = "") 

graph_new_deaths <- ggplot(data = deaths_data) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.7)) +
  theme(axis.text.x = element_text(hjust = 0.8)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  geom_point(shape = 19, size = 0.5, 
             aes(x = as.Date(date), y = new_deaths), color = "black") +
  
  geom_line(data = deaths_data, color = graph_color, size = line_thickness,
            aes(x = as.Date(date) - 3, y = SMA(new_deaths, n = 7))) + 
  scale_x_date(#date_breaks = "3 days", 
               date_labels = "%m/%d") +
  graph_log10_opts1 +
  graph_log10_opts2 +
  labs(title = newinf_title, x = "", y = "")
print(graph_new_deaths)
