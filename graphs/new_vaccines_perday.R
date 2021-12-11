latest_date <- 
  vaccine_age_df %>% 
  select(Date) %>% 
  unique() %>% 
  arrange(Date) %>% 
  tail(n = 1) %>% 
  pull()

new_vaccine_data <-
  vaccine_age_df %>%
  select(Date, RECIPIENT_COUNT) %>%
  group_by(Date) %>% 
  summarize(Count = sum(RECIPIENT_COUNT, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(New_Count = c(0, diff(Count))) %>% 
  select(Date, New_Count) %>%
  arrange(Date)

new_vac <- new_vaccine_data %>% tail(n = 1) %>% select("New_Count") %>% pull() %>% format(big.mark = ",", scientific = FALSE)
SMA_vac <- new_vaccine_data %>% tail(n = 7) %>% pull("New_Count") %>% mean() %>% round() %>% format(big.mark = ",", scientific = FALSE)

newvac_title <- paste("New Vaccines: ", new_vac, ", SMA: ", SMA_vac, sep = "")


### New Vaccines/Day
graph_new_vaccine <-
  new_vaccine_data %>%
  ggplot() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.7)) +
  theme(axis.text.x = element_text(hjust = 0.8)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  geom_point(aes(x = Date, y = New_Count), shape = 19, size = 0.5) +
  geom_line(aes(x = Date - 3, y = SMA(New_Count, n = 7)), color = graph_color, size = line_thickness) +
  #geom_smooth(aes(x = Date, y = New_Count), method = "loess", formula = "y ~ x", color = graph_color, size = line_thickness, se = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_labels = "%m/%d") +
  labs(x = "", y  = "", title = newvac_title)
print(graph_new_vaccine)

