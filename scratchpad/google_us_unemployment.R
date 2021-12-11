library(tsibble)
library(feasts)
library(zoo)
library(forecast)
library(fredr)

################################################################################
### COVID-19 related mobile data
################################################################################
###
### Google data available for direct download at:
### 
### https://www.google.com/covid19/mobility/
###
### I download the data from a Github aggregator (makes updating easier).  
### You can download both datasets at:
### https://github.com/ActiveConclusion/COVID19_mobility

################################################################################
### Load the data...
################################################################################
google_mobility <- 
  "https://raw.githubusercontent.com/ActiveConclusion/COVID19_mobility/master/google_reports/mobility_report_US.csv" %>%
  read_csv()

#google_mobility %>% 
#  filter(state != "Total", county == "Total") %>% 
#  select(state, date, workplaces) %>% 
#  pivot_wider(id_cols = "date", names_from = "state", values_from = "workplaces") %>%
#  as_tsibble(index = "date") %>% 
#  #model(STL(workplaces ~ trend() + season(period = "1 week"))) %>%
#  #components()
#  mutate(workplaces = workplaces %>% ts() %>% mstl(frequency = 7) %>% trendcycle()) %>% 
#  ggplot() + theme_bw() + geom_line(aes(x = as.Date(date), y = workplaces)) + facet_geo(~state)


google_us <-
  google_mobility %>% 
  #filter(state == "Tennessee" & county == "Total") %>%
  #filter(state == "Tennessee" & county == "Cheatham County") %>%
  filter((state == "Total" & county == "Total")) %>% #| (state == "Tennessee" & county == "Total")) %>%
  mutate(state = if_else(state == "Total", "US", state)) %>%
  mutate(name = paste("Google change in visits to workplace in ", state, ",\n% change from baseline", sep = "")) %>%
  rename(value = workplaces) %>%
  select(date, name, value) %>%
  #pivot_wider(id_cols = "date", names_from = "state", values_from = "value") %>%
  mutate(value = value %>% ts() %>% mstl(frequency = 7) %>% trendcycle()) #%>%
  #mutate(value_tn = Tennessee %>% ts() %>% mstl(frequency = 7) %>% trendcycle())


civpart <-
  fredr(
        #series_id = "LBSSA47", # For Tennessee
        series_id = "CIVPART", # For US
        frequency = "m",
        observation_start = as.Date("2020-02-01")) %>%
  rename(name = series_id) %>%
  mutate(name = "Labor Force Participation Rate [CIVPART]") %>%
  select(date, name, value)

g_us <-
  google_us %>% 
  as_tibble() %>%
  mutate(date = as.Date(date) - 8) %>%
  bind_rows(civpart) %>%
  mutate(type = case_when(
    grepl("Google", name)   ~ "Google",
    grepl("Labor", name)    ~ "CIVPART",
  ))

g_google_unemploy_us <-
  ggplot(data = g_us, aes(x = as.Date(date))) +
  theme_linedraw() +
  theme(strip.text = element_text(size = 12)) + 
  geom_line(aes(y = value), color = "darkseagreen4", size = 1.0) +
  scale_x_date(breaks = pretty_breaks(6)) + 
  facet_wrap(~ name, scales = "free_y") +
  labs(x = "", y = "")
print(g_google_unemploy_us)

g_civpart_fit <- 
  g_us %>% 
  select(date, type, value) %>% 
  pivot_wider(id_cols = "date", names_from = "type", values_from = "value") %>% 
  mutate(CIVPART_fit = 63.44103566 + 0.06624173 * Google) %>% 
  filter(!is.na(CIVPART), !is.na(Google)) %>% 
  ggplot() + 
  theme_bw() + 
  geom_line(aes(x = as.Date(date), y = CIVPART)) + 
  geom_line(aes(x = as.Date(date), y = CIVPART_fit), color = "darkred", linetype = "dashed") +
  labs(x = "", y = "CIVPART estimate", title = "CIVPART estimate using Google workplace data")
print(g_civpart_fit)

#plot_grid(g_google_unemploy_us, g_civpart_fit, nrow = 2, ncol = 1, align = "hv")
plot_grid(g_google_unemploy_us, g_civpart_fit, nrow = 2, ncol = 1, align = "hv")

