################################################################################
### Graph:  COVID-19 Total Deaths
################################################################################

graph_total_deaths <- ggplot(data = deaths_data) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.7)) +
  theme(axis.text.x = element_text(hjust = 0.8)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  geom_point(shape = 19, size = 0.5,
             aes(x = as.Date(date), y = total_deaths), color = "black") +
  scale_x_date(date_breaks = "3 days", date_labels = "%m/%d") +
  graph_log10_opts1 +
  graph_log10_opts2 +
  labs(title = paste("Total Deaths: ", tail(deaths_data$total_deaths, n = 1),  sep = ""),
       x = "", y = "")
print(graph_total_deaths)