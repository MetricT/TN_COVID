total <-
  (total_deaths_tib %>% select(Date, Cheatham) %>% rename(Deaths = Cheatham)) %>%
  left_join((total_cases_tib %>% select(Date, Cheatham) %>% rename(Cases = Cheatham)), by= "Date") %>%
  mutate(DeathRate_Total = Deaths / Cases)

new <-
  (new_deaths_tib %>% select(Date, Cheatham) %>% rename(Deaths = Cheatham)) %>% mutate(Deaths = if_else(Deaths < 0, 0, Deaths)) %>%
  left_join((new_cases_tib %>% select(Date, Cheatham) %>% rename(Cases = Cheatham) %>% mutate(Cases = if_else(Cases < 0, 0, Cases))), by= "Date") 

sma_date   <- new$Date - 3
sma_deaths <- new$Deaths %>% SMA(n = 7)
sma_cases  <- new$Cases  %>% SMA(n = 7)
sma_rate   <- sma_deaths / sma_cases

new <- 
  tibble(sma_date, sma_deaths, sma_cases, sma_rate) %>% 
  filter(!is.na(sma_cases)) %>%
  rename(Date = sma_date, Deaths = sma_deaths, Cases = sma_cases, DeathRate_New = sma_rate)

combined <-
  (total %>% select(Date, DeathRate_Total)) %>%
  left_join((new     %>% select(Date, DeathRate_New)),     by = "Date") %>%
  #left_join((new_sma %>% select(Date, DeathRate_New_SMA)), by = "Date") %>%
  rename(Total = DeathRate_Total,
         New   = DeathRate_New)
#         New_SMA = DeathRate_New_SMA)

g <- 
  ggplot(data = combined, aes(x = as.Date(Date))) +
  theme_bw() +
  theme(legend.title = element_blank()) +
#  geom_point(aes(y = New), color = "grey", shape = 20) + 
  geom_line(aes(y = Total,   color = "Total Cases/Deaths")          , size = 1.2) +
  geom_line(aes(y = New, color = "New Cases/Deaths\n(7 day average)"), size = 1.2) +
  
  geom_point(data = combined  %>% filter(!is.na(New)) %>% tail(n = 1), aes(y = New, color = "New Cases/Deaths\n(7 day average)"), size = 3) +
  
  geom_hline(yintercept = combined %>% filter(!is.na(New)) %>% tail(n = 1) %>% pull(New), linetype = "dotted") + 
  
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.05)) +
  scale_color_manual(name = "CFR",
                     values = c("New Cases/Deaths\n(7 day average)" = "darkred",
                                "Total Cases/Deaths"                = "darkseagreen4")) +
  labs(title = "Cheatham COVID-19 Case Fatality Rate",
       x = "Month", 
       y = "Case Fatality Rate")
print(g)

