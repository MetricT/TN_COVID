# From:  https://robjhyndman.com/hyndsight/excess-deaths/

library(dplyr)
library(tidyr)
library(ggplot2)
library(janitor)

eu_pop <- read_csv("../Datasets/Data/EU_Population.csv")

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
                     GBR_NIR = "Northern Ireland",
                     GRC = "Greece",
                     HRV = "Croatia",
                     HUN = "Hungary",
                     ISL = "Iceland",
                     ISR = "Israel",
                     ITA = "Italy",
                     KOR = "South Korea",
                     LTU = "Lithuania",
                     LUX = "Luxembourg",
                     LVA = "Latvia",
                     NLD = "Netherlands",
                     NOR = "Norway",
                     POL = "Poland",
                     PRT = "Portugal",
                     RUS = "Russia",
                     SWE = "Sweden",
                     SVK = "Slovakia",
                     SVN = "Slovenia",
                     USA = "United States")
) %>% 
  select(year, week, country, deaths) %>%
  filter(!country %in% c("United States", "Israel", "Russia", "South Korea"))

deaths_uk <-
  deaths %>%
  filter(country %in% c("Scotland", "England & Wales", "Northern Ireland")) %>%
  pivot_wider(id_cols = c("year", "week"), 
              names_from = c("country"), 
              values_from = c("deaths")) %>% 
  janitor::clean_names() %>% 
  mutate(deaths = england_wales + scotland) %>% 
  add_column(country = "United Kingdom") %>%
  select(year, week, country, deaths)
  
deaths_non_uk <-
  deaths %>%
  filter(!country %in% c("Scotland", "England & Wales", "Northern Ireland"))

deaths <-
  deaths_uk %>%
  bind_rows(deaths_non_uk) %>%
  mutate(thisyear = (year == 2020)) %>% 
  group_by(country, year) %>%
 #filter(country %in% c("Greece", "Czech Republic", "Sweden", "Portugal", "Hungary")) %>%
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

excess_deaths <- 
  deaths %>%
  filter(year >= 2015) %>%
  left_join(recent_deaths) %>%
  mutate(excess = deaths - median_deaths) %>%
  mutate(thisyear = (year == 2020))

excess_deaths %>%
  ggplot(aes(x=week, y=excess, group=year)) +
  theme_bw() + 
  geom_hline(yintercept=0, col='gray') +
  geom_line(aes(col=thisyear)) +
  facet_wrap(~ country) + # scales='free_y') +
  scale_color_manual(values=c("FALSE"='gray',"TRUE"='red')) +
  guides(col=FALSE) +
  ggtitle("Excess deaths") + 
  labs(x = "Week", y = "") +
  scale_y_continuous(labels = scales::comma)

excess_deaths <-
  excess_deaths %>% 
  left_join(eu_pop, by = c("country" = "Country")) %>% 
  mutate(excess = excess / Population) 

eu_order <- c(
  "Spain",      "United Kingdom", "Belgium",     "Italy",    "Netherlands", 
  "France",     "Sweden",         "Switzerland", "Portugal", "Lithuania", 
  "Luxumbourg", "Bulgaria",       "Germany",     "Croatia",  "Czech Republic",
  "Estonia",    "Greece",         "Hungary",     "Iceland",  "Latvia", 
  "Slovenia",   "Denmark",        "Slovakia",    "Austria",  "Poland", 
  "Finland",    "Norway"
)

excess_deaths$country <-
  factor(excess_deaths$country, 
         levels = eu_order)

excess_deaths %>%
  filter(!is.na(country)) %>%
  ggplot(aes(x = week, 
             y = excess, 
             group = year)) +
  theme_bw() + 
  geom_hline(yintercept = 0, col = "gray") +
  geom_line(aes(col = thisyear)) +
  facet_wrap(~ country) + # scales='free_y') +
  scale_color_manual(values = c("FALSE" = "gray", 
                                "TRUE"  = "red")) +
  guides(col = FALSE) +
  ggtitle("Excess deaths per capita per week") + 
  labs(x = "Week", y = "") +
  scale_y_continuous(labels = scales::percent)


### Summarize excess deaths
summary <-
  excess_deaths %>%
  filter(year==2020) %>%
  group_by(country) %>%
  summarise(
    excess = sum(excess),
    last_week = max(week),
    as_at = as.Date("2020-01-01") + 7*(last_week-1)
  ) %>%
  select(country, excess, as_at) %>% data.frame() %>%
  left_join(read_csv("Data/world_country_population.csv", col_type = "dcdcndddc") %>% select(country, population_2020), by = "country") %>%
  mutate(excess_per = 100000 * excess / population_2020) %>%
  arrange(excess_per)

print(summary)



uk <- 
  deaths %>% 
  filter(country %in% c("Scotland", "England & Wales")) %>%
  select(-thisyear) %>% 
  pivot_wider(id_cols = c("year", "week"), 
              names_from = c("country"), 
              values_from = c("deaths")) %>% 
  janitor::clean_names() %>% 
  mutate(deaths = england_wales + scotland) %>% 
  add_column(country = "United Kingdom") %>%
  select(year, week, country, deaths) 
  
