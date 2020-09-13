################################################################################
### Graph:  COVID-19 Cases [ log(Total) vs log(New) ]
### Reference:  https://aatishb.com/covidtrends/
################################################################################

graph_total_new <- ggplot(data = cases_data, aes(x = total_cases, y = new_cases)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.7)) +
  theme(axis.text.x = element_text(hjust = 0.8)) +
  theme(legend.position = "none") +
  geom_point(data = cases_data, shape = 19, size = 0.5,
             aes(x = total_cases, y = new_cases), color = "black") +
  geom_smooth(method = "loess", formula = y ~ x,
              se = FALSE, size = line_thickness, color = graph_color) +
  scale_x_log10(labels = scales::comma_format(accuracy = 1), breaks = c(1, 10, 100, 1000)) +
  scale_y_log10(labels = scales::comma_format(accuracy = 1), breaks = c(1, 10, 100, 1000)) +
  annotation_logticks() +
  labs(title = "Log(New vs Total)", x = "", y = "")
print(graph_total_new)
