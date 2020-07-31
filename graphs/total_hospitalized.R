################################################################################
### Graph:  COVID-19 Total Hospitalizations
################################################################################

hospital_num <- hospital_data %>% arrange(date) %>% tail(n = 1) %>% select("total_hospitalized") %>% pull() %>% format(big.mark = ",", scientific = FALSE)

graph_total_hospital <- ggplot(data = hospital_data, aes(x = as.Date(date), y = total_hospitalized)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.7)) +
  theme(axis.text.x = element_text(hjust = 0.8)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  geom_bar(stat = "identity", fill = graph_color) +
  scale_x_date(#date_breaks = "3 days",
               date_labels = "%m/%d") +
  labs(title = paste("TN Hospitalizations: ", hospital_num, sep = ""), x = "", y = "")
#print(graph_total_hospital)

### Filter out the first data point
hospital_data <-
  hospital_data %>%
  filter(date >= as.Date("2020-04-28"))

hospital_num <- hospital_data %>% arrange(date) %>% tail(n = 1) %>% select("total_hospitalized") %>% pull() %>% format(big.mark = ",", scientific = FALSE)

SMA <- hospital_data$total_hospitalized %>% SMA(n = 7) %>% tail(n = 1) %>% round() %>% format(big.mark = ",", scientific = FALSE)

totalhos_title <- paste("Total Hospitalizations: ", hospital_num, ", SMA: ", SMA, sep = "")

graph_total_hospital <-
  ggplot(data = hospital_data, aes(x = as.Date(date), y = total_hospitalized)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.7)) +
  theme(axis.text.x = element_text(hjust = 0.8)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +

  geom_point(data = hospital_data, shape = 19, size = 0.5,
             aes(x = as.Date(date), y = total_hospitalized), color = "black") +

  geom_line(data = hospital_data, color = graph_color, size = line_thickness,
            aes(x = as.Date(date) - 3, y = SMA(total_hospitalized, n = 7))) +

  scale_x_date(#date_breaks = "3 days",
    date_labels = "%m/%d") +
  labs(title = totalhos_title, x = "", y = "")
#print(graph_total_hospital)
