################################################################################
### Graph:  COVID-19 Positive test rate
################################################################################

ntpd_df <-
  county_new_df %>%
  filter(DATE >= as.Date("2020-03-31")) %>%
  select(DATE, POS_TESTS, NEG_TESTS) %>%
  group_by(DATE) %>%
  summarize(total_pos_tests = sum(POS_TESTS),
            total_neg_tests = sum(NEG_TESTS)) %>%
  mutate(Positive_Rate = total_pos_tests / (total_pos_tests + total_neg_tests)) %>%
  mutate(Date = as.Date(DATE)) %>%
  select(-DATE)

total_pos_num <- ntpd_df %>% arrange(Date) %>% tail(n = 1) %>% select("Positive_Rate") %>% pull() %>% format(big.mark = ",", scientific = FALSE)

posrate_title <- paste("Total Positive Test Rate: ", round(100 * as.double(total_pos_num), 1), "%", sep = "")

graph_total_positive_test_rate <-
  ggplot(data = ntpd_df, aes(x = as.Date(Date), y = D_TN)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.7)) +
  theme(axis.text.x = element_text(hjust = 0.8)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  geom_point(data = ntpd_df, shape = 19, size = line_thickness,
             aes(x = as.Date(Date), y = Positive_Rate), color = "black") +

  geom_vline(xintercept = as.Date("2020-06-12"), linetype = "dotted") +

  annotate("text", size = 4, angle = 90,
           label = "Change in State Reporting", x = as.Date("2020-06-12"), y = 0.07) +

  scale_x_date(#date_breaks = "3 days",
    date_labels = "%m/%d") +
  scale_y_continuous(labels = scales::comma) +
  #scale_y_continuous(limits = c(100, 650)) +
  #  graph_log10_opts1 +
  #  graph_log10_opts2 +
  labs(title = posrate_title, x = "", y = "")
#print(graph_total_positive_test_rate)
