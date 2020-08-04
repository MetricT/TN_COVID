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
                     ISR = "Israel",
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
  filter(country != "Israel") %>%
  select(year, week, country, deaths)

### Add up the EU so we can compare mortality numbers against the US
deaths_us <-
  deaths %>%
  filter(country %in% c("United States"))

deaths_eu <-
  deaths %>%
  filter(!country %in% c("United States")) %>%
  pivot_wider(id_cols = c("year", "week"), 
              names_from = c("country"), 
              values_from = c("deaths")) %>% 
  janitor::clean_names() %>% 
  mutate_if(is.numeric, ~ replace_na(., 0)) #
  mutate(deaths = england_wales + scotland) %>% 
  add_column(country = "United Kingdom") %>%
  select(year, week, country, deaths)
  


deaths <-
  deaths_us %>%
  #bind_rows(deaths_non_uk) %>%
  mutate(thisyear = (year == 2020)) %>% 
  group_by(country, year) %>%

  filter(!(year==2020 & week==max(week))) %>%
  filter(!(year==2020 & week==max(week)))

deaths %>%
  ggplot(aes(x=week, y=deaths, group=year)) +
  theme_bw() + 
  geom_line(aes(col=thisyear)) +
  facet_wrap(~ country, scales='free_y') +
  scale_color_manual(values=c("FALSE"='gray',"TRUE"='red')) +
  guides(col=FALSE) +
  ggtitle("Weekly deaths") + 
  labs(x = "Week", y = "") +
  scale_y_continuous(labels = scales::comma)


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

ggplot(data = excess_deaths, aes(x=week, y=excess, group=year)) +
  theme_bw() + 
  geom_hline(yintercept=0, col='gray') +
  geom_line(aes(col=thisyear)) +
  facet_wrap(~ country, scales='free_y') +
  scale_color_manual(values=c("FALSE"='gray',"TRUE"='red')) +
  guides(col=FALSE) +
  ggtitle("Excess deaths") + 
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


world_pop <- 
  read_csv("Data/world_country_population.csv") %>% 
  select(country, population_2020)
