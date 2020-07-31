# From:  https://robjhyndman.com/hyndsight/excess-deaths/

library(dplyr)
library(tidyr)
library(ggplot2)
library(janitor)

stmf <- 
  readr::read_csv("https://www.mortality.org/Public/STMF/Outputs/stmf.csv", skip = 1) 

deaths <- 
  stmf %>%
  janitor::clean_names() %>%
  filter(country_code == "USA") %>%
  select(country_code:d_total) %>%
  pivot_longer(5:10,
               names_to = "age", values_to = "deaths",
               names_pattern = "[d_]*([a-z0-9_p]*)"
  ) %>%
  filter(age == "total", sex == "b") %>%
  mutate(
    country = recode(country_code,
                     AUT = "Austria",
                     BEL = "Belgium",
                     BGR = "Bulgaria",
                     CHE = "Switzerland",
                     CZE = "Czech Republic",
                     DEUTNP = "Germany",
                     DNK = "Denmark",
                     ESP = "Spain",
                     EST = "Estonia",
                     FIN = "Finland",
                     FRATNP = "France",
                     GBRTENW = "England & Wales",
                     GBR_SCO = "Scotland",
                     HUN = "Hungary",
                     ISL = "Iceland",
                     ISR = "Isreal",
                     ITA = "Italy",
                     LTU = "Lithuania",
                     LUX = "Luxembourg",
                     NLD = "Netherlands",
                     NOR = "Norway",
                     PRT = "Portugal",
                     SWE = "Sweden",
                     SVK = "Slovakia",
                     USA = "United States")
  ) %>% 
  select(year, week, country, deaths) %>%
  mutate(thisyear = (year == 2020)) %>% 
  group_by(country, year) %>%
  filter(!(year==2020 & week==max(week))) %>%
  filter(!(year==2020 & week==max(week)))
  


g_weekly_deaths <-
  ggplot(data = deaths %>% select(-country), aes(x=week, y=deaths, group=year)) +
  theme_classic() + 
  geom_line(aes(col=thisyear)) +
#  facet_wrap(~ country, scales='free_y') +
  scale_color_manual(values=c("FALSE"='gray',"TRUE"='red')) +
  guides(col=FALSE) +
  ggtitle("Weekly deaths in US") + 
  labs(x = "Week", y = "") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 80000))
print(g_weekly_deaths)


### How current is the data?   Shows the lag in weeks
deaths %>%
  filter(year==2020) %>%
  group_by(country) %>%
  summarise(last_week = max(week)) %>%
  mutate(
    current_week = lubridate::week(Sys.Date()),
    lag = current_week - last_week
  ) %>% data.frame()

### Estimate excess deaths
recent_deaths <- deaths %>%
  filter(year >= 2015 & year <= 2019) %>%
  group_by(country,week) %>%
  summarise(median_deaths = median(deaths)) %>%
  ungroup()

excess_deaths <- deaths %>%
  filter(year >= 2015) %>%
  left_join(recent_deaths) %>%
  mutate(excess = deaths - median_deaths) %>%
  mutate(thisyear = (year == 2020))

g_excess_deaths <- 
  ggplot(data = excess_deaths %>% select(-country), aes(x=week, y=excess, group=year)) +
  theme_classic() + 
  geom_hline(yintercept=0, col='gray') +
  geom_line(aes(col=thisyear)) +
  #facet_wrap(~ country, scales='free_y') +
  scale_color_manual(values=c("FALSE"='gray',"TRUE"='red')) +
  guides(col=FALSE) +
  ggtitle("Excess deaths in US") + 
  labs(x = "Week", y = "") +
  scale_y_continuous(labels = scales::comma)

### Summarize excess deaths
excess_deaths %>%
  filter(year==2020) %>%
  group_by(country) %>%
  summarise(
    excess = sum(excess),
    last_week = max(week),
    as_at = as.Date("2020-01-01") + 7*(last_week-1)
  ) %>%
  select(country, excess, as_at) %>% data.frame()

  plot_grid(g_weekly_deaths, g_excess_deaths, nrow = 1, ncol = 2, align = "hv")

plot_grid(g_deaths, g_cases_and_deaths, nrow = 2, ncol = 1, align = "hv")
