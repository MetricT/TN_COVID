model_new_cases <-
  new_cases_tib %>% 
  select(Date, Total) %>% 
  as_tsibble() %>%
  #model(STL(Total ~ trend(window = 7) + season(period = "1 week") + season(period = "1 month"))) %>%
  model(STL(Total ~ trend(window = 7) + season(period = "1 week"))) %>%
  components() %>%
  filter(Date >= as.Date("2020-12-01"))

#model_new_cases %>% autoplot()  

g_c <-
  ggplot(data = model_new_cases) +  #%>% filter(Date >= as.Date("2021-07-01"))) + 
  theme_bw() + 
  #scale_y_continuous(labels = scales::comma) + 
  scale_y_continuous(labels = scales::comma, trans = "log2") + 
  geom_smooth(aes(x = as.Date(Date), y = Total), method = "gam", formula = y ~ s(x, bs = "cs")) +
  #geom_line(aes(x = as.Date(Date) - 3, y = SMA(Total, n = 7)), size = 1.3, color = "steelblue4")+
  geom_point(aes(x = as.Date(Date), y = Total), color = "black") + 
  #geom_point(aes(x = as.Date(Date), y = season_adjust), color = "darkseagreen4", size = 1) + 
  #geom_point( data = data_new_cases %>% filter(Date > as.Date(latest_date)), aes(x = as.Date(Date), y = Total), color = "blue") +
  #geom_vline(xintercept = as.Date(latest_date), linetype = "dashed") + 
  labs(x = "", y = "", title = "TN New Cases - Log Scale")
print(g_c)

g_c_lin <-
  ggplot(data = model_new_cases) +  #%>% filter(Date >= as.Date("2021-07-01"))) + 
  theme_bw() + 
  #scale_y_continuous(labels = scales::comma) + 
  scale_y_continuous(labels = scales::comma) + 
  geom_point(aes(x = as.Date(Date), y = Total), color = "black") + 
  geom_line(aes(x = as.Date(Date) - 3, y = SMA(Total, n = 7)), size = 1.3, color = "steelblue4")+
  #geom_point(aes(x = as.Date(Date), y = season_adjust), color = "darkseagreen4", size = 1) + 
  #geom_point( data = data_new_cases %>% filter(Date > as.Date(latest_date)), aes(x = as.Date(Date), y = Total), color = "blue") +
  #geom_vline(xintercept = as.Date(latest_date), linetype = "dashed") + 
  labs(x = "", y = "", title = "TN New Cases - Linear Scale")
print(g_c_lin)

plot_grid(g_c_lin, g_c, nrow = 1, ncol = 2, align = "hv")

################################################################################

model_new_deaths <-
  new_deaths_tib %>% 
  select(Date, Total) %>% 
  as_tsibble() %>%
  model(STL(Total ~ trend(window = 7) + season(period = "1 week") + season(period = "1 month"))) %>%
  components() %>%
  filter(Date >= as.Date("2020-12-01"))

model_new_deaths %>% autoplot()  

g_d <-
  ggplot(data = model_new_deaths) + 
  theme_bw() + 
  scale_y_continuous(labels = scales::comma, trans = "log2") + 
  geom_smooth(aes(x = as.Date(Date), y = Total), method = "gam", formula = y ~ s(x, bs = "cs")) + 
  geom_point(aes(x = as.Date(Date), y = Total), color = "black") + 
  #geom_line(aes(x = as.Date(Date), y = trend), color = "darkseagreen4", size = 1) + 
  #geom_point( data = data_new_cases %>% filter(Date > as.Date(latest_date)), aes(x = as.Date(Date), y = Total), color = "blue") +
  #geom_vline(xintercept = as.Date(latest_date), linetype = "dashed") + 
  labs(x = "", y = "", title = "TN New Deaths")
print(g_d)


print(plot_grid(g_c, g_d, nrow = 1, ncol = 2, align = "hv"))


combined <- 
  model_new_cases %>% 
  select(Date, Total, trend, season_adjust) %>% 
  rename(date = Date, 
         new_cases = Total,
         new_cases_trend = trend,
         new_cases_adj = season_adjust) %>%
  left_join(
    model_new_deaths %>% 
      select(Date, Total, trend, season_adjust) %>% 
      rename(date = Date, 
             new_deaths = Total,
             new_deaths_trend = trend,
             new_deaths_adj = season_adjust), by = "date")

combined %>% 
  filter(date >= as.Date("2021-04-01")) %>%
  ggplot() +
  theme_bw() +
  geom_line(aes(x = date - 3 + 17, y = SMA(new_cases_adj, n = 7)), color = "darkgreen") +
  geom_line(aes(x = date - 3, y = SMA(new_deaths_adj, n = 7) * 82.56928), color = "darkblue") +
  scale_y_continuous(labels = scales::comma, 
                     sec.axis = sec_axis(~ . / 82.56928, name = "New Deaths (blue)", labels = scales::comma)) +
  labs(x = "", y = "New Cases (green)", title = "TN New Cases and New Deaths trends (7MA) lagged by 18 days", caption = "Data taken from TN Dept of Health")

  

