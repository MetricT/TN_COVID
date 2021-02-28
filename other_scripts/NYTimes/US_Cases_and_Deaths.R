library(tidyverse)
library(forecast)
library(naniar)

### How much do we scale up deaths to make it visually similar in size to cases
scale <- 60.16231
#scale <- 70

### What is the lag in days between cases and deaths
lag_us <- 28
#lag_us <- 6

# Download this Github repo and add the path to the spreadsheet
# https://github.com/nytimes/covid-19-data
spreadsheet <-
  read_csv("../Datasets/nytimes/covid-19-data/us.csv", col_names = TRUE, col_types = "Ddd")
  

data <-
  spreadsheet %>%
  replace_with_na(replace = list(deaths = 0)) %>%
  mutate_at("deaths", ~lead(., lag_us)) %>%
  mutate(new_cases  = cases - lag(cases)) %>%
  mutate(new_deaths = deaths - lag(deaths)) %>%
  mutate(new_cases_trend  = new_cases  %>% ts(frequency = 7) %>% mstl() %>% trendcycle()) %>%
  mutate(new_deaths_trend = new_deaths %>% ts(frequency = 7) %>% mstl() %>% trendcycle()) 
data$new_deaths_trend[is.na(data$new_deaths)] <- NA

current_date <- data %>% tail(n = 1) %>% pull("date")

g_cases_and_deaths <-
  ggplot(data = data) +
  theme_bw() +
  theme(legend.position = "none") +
  
  theme(axis.text.y.left = element_text(color = "blue")) +
  theme(axis.text.y.right = element_text(color = "red")) +
  
  geom_point(aes(x = as.Date(date), y = new_cases, color = "Cases"), size = 1, shape = 19, alpha = 0.2) +
  geom_line(aes(x = as.Date(date), y = new_cases_trend, color = "Cases"), size = 1.3) +

  geom_point(aes(x = as.Date(date), y = new_deaths * scale, color = "Deaths"), size = 1, shape = 19, alpha = 0.2) +
  geom_line(aes(x = as.Date(date), y = new_deaths_trend * scale, color = "Deaths"), size = 1.3) +

  #geom_vline(xintercept = as.Date("2020-06-03"), linetype = "dashed") + 
  
  scale_y_continuous(labels = scales::comma,
                     sec.axis = sec_axis(~ . / scale)
                     )+  
  
  scale_color_manual(values =  c("Cases" = "steelblue2",
                                 "Deaths" = "firebrick2")) +
  
  labs(title = paste("US New Cases (blue) and New Deaths (red) with ", lag_us, " day lag as of ", current_date, sep = ""), 
       x = "Date", 
       y = "")
  

print(g_cases_and_deaths)

