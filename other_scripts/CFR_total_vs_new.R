total <-
  (total_deaths_tib %>% select(Date, Total) %>% rename(Deaths = Total)) %>%
  left_join((total_cases_tib %>% select(Date, Total) %>% rename(Cases = Total)), by= "Date") %>%
  mutate(DeathRate_Total = Deaths / Cases)

new <-
  (new_deaths_tib %>% select(Date, Total) %>% rename(Deaths = Total)) %>%
  left_join((new_cases_tib %>% select(Date, Total) %>% rename(Cases = Total)), by= "Date") %>%
  mutate(DeathRate_New = if_else(Deaths != 0, Deaths / Cases, 0))

new_sma <-
  new %>% 
  select(Date, DeathRate_New) %>% 
  mutate(Date = Date - 3, 
         DeathRate_New_SMA = SMA(DeathRate_New, n = 7)) %>% 
  filter(!is.na(DeathRate_New_SMA)) %>%
  select(Date, DeathRate_New_SMA)

combined <-
  (total %>% select(Date, DeathRate_Total)) %>%
  left_join((new     %>% select(Date, DeathRate_New)),     by = "Date") %>%
  left_join((new_sma %>% select(Date, DeathRate_New_SMA)), by = "Date") %>%
  rename(Total = DeathRate_Total,
         New   = DeathRate_New,
         New_SMA = DeathRate_New_SMA)

g <- 
  ggplot(data = combined) +
  theme_bw() +
  theme(legend.title = element_blank()) +
#  geom_point(aes(y = New), color = "grey", shape = 20) + 
  #geom_smooth(aes(x = as.Date(Date), y = New_SMA), method = "loess", formula = "y ~ x") + 
  geom_line(aes(x = as.Date(Date), y = Total,   color = "Total Cases/Deaths")          , size = 1.2) +
  geom_line(aes(x = as.Date(Date), y = New_SMA, color = "New Cases/Deaths\n(7 day average)"), size = 1.2) +

  geom_point(data = combined  %>% filter(!is.na(New_SMA)) %>% tail(n = 1), aes(x = as.Date(Date), y = New_SMA, color = "New Cases/Deaths\n(7 day average)"), size = 3) +
  
  geom_hline(yintercept = combined %>% filter(!is.na(New_SMA)) %>% tail(n = 1) %>% pull(New_SMA), linetype = "dotted") + 
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),  breaks = pretty_breaks(8)) +
  scale_color_manual(name = "CFR",
                     values = c("New Cases/Deaths\n(7 day average)" = "darkred",
                                "Total Cases/Deaths"                = "darkseagreen4")) +
  labs(title = "Tennessee COVID-19 Case Fatality Rate",
       x = "Month", 
       y = "Case Fatality Rate")
print(g)

