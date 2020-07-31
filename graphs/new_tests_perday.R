################################################################################
### Graph:  COVID-19 Number of New Tests Per Day, with 7-day SMA
################################################################################

ntpd_df <-
  county_new_df %>% 
  select(DATE, COUNTY, NEW_TESTS) %>%
  arrange(COUNTY) %>%
  pivot_wider(id_cols = "DATE", names_from = "COUNTY", values_from = "NEW_TESTS") %>%
  filter(DATE >= as.Date("2020-04-01")) %>%
  mutate(Total = rowSums(select(., !starts_with("DATE")))) %>%
  select(DATE, Total) %>%
  rename(Date = DATE, D_TN = Total) %>%
  mutate(D_TN = if_else(D_TN == 59864, 10000, D_TN))

#ntpd_df <- testres_df %>% filter(Date >= as.Date("2020-03-26"))

new_ntpd_num <- ntpd_df %>% arrange(Date) %>% tail(n = 1) %>% select("D_TN") %>% pull() %>% format(big.mark = ",", scientific = FALSE)
SMA <- ntpd_df %>% arrange(Date) %>% pull("D_TN") %>% SMA(n = 7) %>% tail(n = 1) %>% round() %>% format(big.mark = ",", scientific = FALSE)

newntpd_title <- paste("Tests/Day: ", new_ntpd_num, ", SMA: ", SMA, sep = "")

graph_new_tests_perday <- ggplot(data = ntpd_df, aes(x = as.Date(Date), y = D_TN)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.7)) +
  theme(axis.text.x = element_text(hjust = 0.8)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  geom_point(data = ntpd_df, shape = 19, size = 0.5,
             aes(x = as.Date(Date), y = D_TN), color = "black") +
  
  geom_line(data = ntpd_df, color = graph_color, size = line_thickness,
            aes(x = as.Date(Date) - 3, y = SMA(D_TN, n = 7))) +
  
  scale_x_date(#date_breaks = "3 days", 
    date_labels = "%m/%d") +
  scale_y_continuous(labels = scales::comma) + 
  #scale_y_continuous(limits = c(100, 650)) +
  #  graph_log10_opts1 +
  #  graph_log10_opts2 +
  labs(title = newntpd_title, x = "", y = "")
print(graph_new_tests_perday)
