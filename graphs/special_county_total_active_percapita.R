################################################################################
### Graph:  COVID-19 Total active
################################################################################

library(lubridate)

county <- c("Cheatham")

total_active <-
  total_active_tib %>%
  select("Date", all_of(county)) %>%
  rename(County = county) %>%
  mutate(County_per = 100 * County / (tn_pop_df %>%
                                        filter(County == county) %>%
                                        pull(POP2018)))
total_active_per_num <-
  total_active %>%
  tail(n = 1)

subset <-
  total_active %>%
  select(Date, County_per) %>%
  mutate(Decimal_Date = decimal_date(Date)) %>%
  filter(Date >= as.Date("2020-07-01")) #%>%
  #filter(Date <= as.Date("2020-07-15"))

fit_subset <-
  lm(County_per ~ Decimal_Date, data = subset)

date_1_per <-
  ((1 - fit_subset$coefficients[1]) / fit_subset$coefficients[2]) %>%
  date_decimal()

forecast_1_per <-
  tibble(
    date = as.Date("2020-07-01") + 0:100,
    forecast = fit_subset$coefficients[1] +
               fit_subset$coefficients[2] * decimal_date(date)
  ) %>%
  filter(forecast <= 1.0)

intercept_date <- as.Date(forecast_1_per %>% tail(n = 1) %>% pull("date")) + 1

totact_title <-
  paste("Active Cases in ", county, " Co. as % of county population: ",
        round(total_active_per_num$County_per, 2), "% as of ",
        total_active_per_num$Date, "\nCurrent cases hit 1% approximately ",
        intercept_date, sep = "")

combined <-
  total_active %>%
  full_join(forecast_1_per, by = c("Date" = "date"))

graph_total_active_county_percapita <-
  ggplot(data = combined, aes(x = as.Date(Date), y = County_per / 100)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.7)) +
  theme(axis.text.x = element_text(hjust = 0.8)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +

  geom_point(shape = 19, size = 0.5, color = "black") +

  geom_line(color = graph_color, size = line_thickness) +

  geom_line(color = "firebrick2", size = 1.0, linetype = "dashed",
            aes(y = forecast / 100)) +

  #geom_vline(xintercept = as.Date(intercept_date), linetype = "dashed") +

  geom_hline(yintercept = 0.010, linetype = "dotted") +
  annotate("text", size = 4, label = "Virtual", x = as.Date("2020-05-01"), y = 0.0103) +

  geom_hline(yintercept = 0.005, linetype = "dotted") +
  annotate("text", size = 4, label = "Hybrid", x = as.Date("2020-05-01"), y = 0.0053) +

  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous(labels = scales::percent, breaks = c(0.000, 0.002, 0.004, 0.006, 0.008, 0.010)) +
  labs(title = totact_title, x = "", y = "")
print(graph_total_active_county_percapita)
