### EpiEstim and other useful epidemic analysis packages available from:
###
### # https://www.repidemicsconsortium.org/

library(tidyverse)
library(tsibble)
library(feasts)
library(EpiEstim)
library(forecast)

data <-
  new_cases_tib %>%
  select(Date, Total) %>%
  rename(dates = Date, I = Total)

### Serial Interval estimates from:
### https://wwwnc.cdc.gov/eid/article/26/6/20-0357_article
si_mean <- 3.96
si_std  <- 4.75

### Create a blank tibble to hold our EpiEstim output
Rt_tib <-
  tribble(
    ~dates, ~mean_r, ~std_r,
  )

estimate <- 
  estimate_R(data,
             method = "parametric_si", 
             config = make_config(list(mean_si = si_mean,
                                        std_si = si_std)))
  
e_dates  <- estimate$dates %>% as_tibble() %>% filter(row_number() > 7)
e_values <- estimate$R     %>% as_tibble() %>% select("Mean(R)", "Std(R)")
  
Rt_tib <- 
  e_dates %>% 
  bind_cols(e_values) %>% 
  janitor::clean_names() %>% 
  rename(dates = value) #%>%
#  mutate(county = "Tennessee")

Rt_dates  <- Rt_tib %>% tail(n = 1) %>% pull("dates")
Rt_mean_r <- Rt_tib %>% tail(n = 7) %>% pull("mean_r") %>% mean() %>% round(digits = 2)

### Render the graph and done!
g_rt_counties <-
  ggplot(data = Rt_tib) +
  theme_linedraw() +
  theme(strip.text.x = element_text(size = 16)) +
  geom_point(aes(x = as.Date(dates), y = mean_r), size = 1.0) + 
  geom_ribbon(aes(x = as.Date(dates),
                  ymin = mean_r - std_r, 
                  ymax = mean_r + std_r),
              color = NA, fill = "darkgrey", alpha = 0.2) +
  geom_line(aes(x = as.Date(dates) - 3, y = SMA(mean_r, n = 7)), color = "firebrick4", size = 1.2) +
  geom_hline(yintercept = 1, linetype = "dashed") + 
  scale_y_continuous(limits = c(0.5, 1.5)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(title = paste("Rt estimate for Tennessee, ", Rt_dates, ": ", Rt_mean_r, sep = ""), 
       x = "", y = "Rt")
print(g_rt_counties)
