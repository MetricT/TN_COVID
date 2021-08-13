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
  ggplot(data = model_new_cases) + # %>% filter(Date >= as.Date("2020-12-01"))) + 
  theme_bw() + 
  #scale_y_continuous(labels = scales::comma) + 
  scale_y_continuous(labels = scales::comma, trans = "log2") + 
  geom_smooth(aes(x = as.Date(Date), y = Total), method = "gam", formula = y ~ s(x, bs = "cs")) + 
  geom_point(aes(x = as.Date(Date), y = Total), color = "black") + 
  #geom_point(aes(x = as.Date(Date), y = season_adjust), color = "darkseagreen4", size = 1) + 
  #geom_point( data = data_new_cases %>% filter(Date > as.Date(latest_date)), aes(x = as.Date(Date), y = Total), color = "blue") +
  #geom_vline(xintercept = as.Date(latest_date), linetype = "dashed") + 
  labs(x = "", y = "", title = "TN New Cases")
print(g_c)

################################################################################


model_new_hosp <-
  new_hospitalized_tib %>% 
  select(Date, Total) %>% 
  as_tsibble() %>%
  model(STL(Total ~ trend(window = 7) + season(period = "1 week") + season(period = "1 month"))) %>%
  components() %>%
  filter(Date >= as.Date("2020-12-01"))

model_new_hosp %>% autoplot()  

g_h <-
  ggplot(data = model_new_hosp) + 
  theme_bw() + 
  scale_y_continuous(labels = scales::comma, trans = "log2") + 
  geom_smooth(aes(x = as.Date(Date), y = Total), method = "gam", formula = y ~ s(x, bs = "cs")) + 
  geom_point(aes(x = as.Date(Date), y = Total), color = "black") + 
  #geom_line(aes(x = as.Date(Date), y = trend), color = "darkseagreen4", size = 1) + 
  #geom_point( data = data_new_cases %>% filter(Date > as.Date(latest_date)), aes(x = as.Date(Date), y = Total), color = "blue") +
  #geom_vline(xintercept = as.Date(latest_date), linetype = "dashed") + 
  labs(x = "", y = "", title = "TN New Hospitalized")
print(g_h)

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
  labs(x = "", y = "", title = "TN New Hospitalized")
print(g_d)

################################################################################


model_new_hosp <-
  new_hospitalized_tib %>%
  select(Date, Total) %>% 
  as_tsibble() %>%
  model(
    #    snaive = SNAIVE(Total),
    #nnetar = NNETAR(Total),
    ETS = ETS(Total),
    #stl = STL(Total ~ season(window = "periodic"))
  ) %>%
  forecast(h = "2 weeks") %>% 
  as_tibble() %>% 
  select(Date, .mean) %>% 
  rename(Total = .mean)

model_new_deaths <-
  new_deaths_tib %>%
  select(Date, Total) %>% 
  as_tsibble() %>%
  model(
    #    snaive = SNAIVE(Total),
    #nnetar = NNETAR(Total),
    ETS = ETS(Total),
    #stl = STL(Total ~ season(window = "periodic"))
  ) %>%
  forecast(h = "2 weeks") %>% 
  as_tibble() %>% 
  select(Date, .mean) %>% 
  rename(Total = .mean)



data_new_hosp <-
  (new_hospitalized_tib %>% 
     select(Date, Total)) %>%
  bind_rows(model_new_hosp)

data_new_deaths <-
  (new_deaths_tib %>% 
     select(Date, Total)) %>%
  bind_rows(model_new_deaths)


g_h <-
  ggplot() + 
  theme_bw() + 
  scale_y_continuous(labels = scales::comma, trans = "log2") + 
  geom_smooth(data = data_new_hosp %>% filter(Date >= as.Date("2020-12-01") & Date <= as.Date(latest_date)),
              aes(x = as.Date(Date), y = Total), method = "gam", formula = y ~ s(x, bs = "cs")) + 
  geom_point(data = data_new_hosp %>% filter(Date >= as.Date("2020-12-01") & Date <= as.Date(latest_date)),
             aes(x = as.Date(Date), y = Total), color = "black") + 
  #geom_point( data = data_new_hosp %>% filter(Date > as.Date(latest_date)), aes(x = as.Date(Date), y = Total), color = "blue") +
  #geom_vline(xintercept = as.Date(latest_date), linetype = "dashed") + 
  labs(x = "", y = "", title = "TN New Hospitalized")
print(g_h)

g_d <-
  ggplot() + 
  theme_bw() + 
  scale_y_continuous(labels = scales::comma, trans = "log2") + 
  geom_smooth(data = data_new_deaths %>% filter(Date >= as.Date("2020-12-01") & Date <= as.Date(latest_date)),
              aes(x = as.Date(Date), y = Total), method = "gam", formula = y ~ s(x, bs = "cs")) +
  geom_point(data = data_new_deaths %>% filter(Date >= as.Date("2020-12-01") & Date <= as.Date(latest_date)),
             aes(x = as.Date(Date), y = Total), color = "black") + 
  #geom_point( data = data_new_deaths %>% filter(Date > as.Date(latest_date)), aes(x = as.Date(Date), y = Total), color = "blue") +
  #geom_vline(xintercept = as.Date(latest_date), linetype = "dashed") + 
  labs(x = "", y = "", title = "TN New Deaths")
print(g_d)

#print(plot_grid(g_c, g_h, g_d, nrow = 3, ncol = 1, align = "hv"))
print(plot_grid(g_c, g_d, nrow = 2, ncol = 1, align = "hv"))

