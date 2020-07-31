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
  mutate(new_pos_tests = total_pos_tests - lag(total_pos_tests)) %>%
  mutate(new_neg_tests = total_neg_tests - lag(total_neg_tests)) %>%
  mutate(Positive_Rate = new_pos_tests / (new_pos_tests + new_neg_tests)) %>%
  rename(Date = DATE) %>%
  mutate(Date = as.Date(Date))

ntpd_trend <-
  ntpd_df %>%
  select(Date, Positive_Rate) %>%
  filter(!is.na(Positive_Rate)) %>%
  as_tsibble(index = "Date") %>%
  tsibble::fill_gaps() %>%
  na.locf() %>%
  model(STL(Positive_Rate ~ trend() + season(window = "periodic"))) %>%
  components() %>%
  select(Date, trend)

ntpd_df <-  ntpd_df %>% left_join(ntpd_trend, by = "Date")

new_pos_num <- (ntpd_df %>% arrange(Date) %>% tail(n = 1) %>% pull("Positive_Rate") * 100) %>% round(2)
SMA         <- (ntpd_df %>% arrange(Date) %>% tail(n = 1) %>% pull("trend") * 100) %>% round(2)
#SMA <- (ntpd_df %>% arrange(Date) %>% select(Positive_Rate) %>% SMA(n = 7) %>% tail(n = 1) * 100) %>% round(2)

posrate_title <- paste("Positive New Test Rate: ", new_pos_num, "%, SMA: ", SMA, "%", sep = "")

graph_new_positive_test_rate <-
  ggplot(data = ntpd_df, aes(x = as.Date(Date), y = D_TN)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.7)) +
  theme(axis.text.x = element_text(hjust = 0.8)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  geom_point(data = ntpd_df, shape = 19, size = 0.5,
             aes(x = as.Date(Date), y = Positive_Rate), color = "black") +

#  geom_vline(xintercept = as.Date("2020-06-12"), linetype = "dotted") +

  geom_line(data = ntpd_df, size = line_thickness, color = graph_color,
            aes(x = as.Date(Date), y = trend)) +

  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0.025, 0.125)) +
  labs(title = posrate_title, x = "", y = "")

#print(graph_new_positive_test_rate)
