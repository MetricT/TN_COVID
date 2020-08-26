################################################################################
### This script pulls mortality data for Tennessee to estimate the number of 
### excess deaths that have occured in 2020 due to COVID-19  - /u/MetricT
################################################################################

library(forcats)
library(tsibble)
library(tidyverse)


### Pull mortality data by age group from the CDC and do some basic clean-ups
mortality_data <- 
  "https://data.cdc.gov/api/views/y5bj-9g5w/rows.csv?accessType=DOWNLOAD&bom=true&format=true" %>%
  read_csv(col_names = TRUE, col_types = "ccccccccccc")


### Do some basic clean-ups on the mortality data
md <-
  mortality_data %>%
  janitor::clean_names() %>%
  mutate(week_ending_date = as.Date(week_ending_date,
                                    format = "%m/%d/%Y")) %>%
  mutate(yearweek = yearweek(week_ending_date)) %>%
  mutate(age_group = factor(age_group)) %>%
  filter(is.na(suppress)) %>%
  select(-state_abbreviation, -suppress, -week_ending_date, 
         -year, -week, -time_period) %>%
  mutate(year = year(yearweek) %>% as.integer()) %>%
  mutate(week = week(yearweek) %>% as.integer()) 


### Now filter out mortality data for Tennessee
deaths <- 
  md %>%
  filter(jurisdiction == "Tennessee") %>%
  select(-jurisdiction, -note) %>%
  mutate(number_of_deaths = as.integer(number_of_deaths)) %>%
  select(yearweek, year, week, age_group, number_of_deaths) %>%
  mutate(thisyear = if_else(year(yearweek) == 2020, TRUE, FALSE)) %>%
  group_by(year) %>%
  filter(!(year == 2020 & week %in% seq(max(week) - 2, max(week)))) %>%
  ungroup()

### The deaths data is broken down into age groups.   I originally intended to
### use this to show excess deaths by age group, and just kind of ran with it.
###
### Since each age group shows a different growth rate (Boomers are dying at 
### older rates than younger people because they're older, and similar reasons)
### So I detrend each individual age group, and them sum them up later to find
### excess deaths across all age groups
model_1 <-
  deaths %>%
  filter(age_group == "Under 25 years") %>%
  filter(!are_duplicated(., index=yearweek)) %>%
  select(yearweek, number_of_deaths) %>%
  as_tsibble(index = "yearweek") %>%
  tsibble::fill_gaps() %>%
  na.locf() #%>%

model_2 <-
  deaths %>%
  filter(age_group == "25-44 years") %>%
  filter(!are_duplicated(., index=yearweek)) %>%
  select(yearweek, number_of_deaths) %>%
  as_tsibble(index = "yearweek") %>%
  tsibble::fill_gaps() %>%
  na.locf() #%>%

model_3 <-
  deaths %>%
  filter(age_group == "45-64 years") %>%
  filter(!are_duplicated(., index=yearweek)) %>%
  select(yearweek, number_of_deaths) %>%
  as_tsibble(index = "yearweek") %>%
  tsibble::fill_gaps() %>%
  na.locf() #%>%

model_4 <-
  deaths %>%
  filter(age_group == "65-74 years") %>%
  filter(!are_duplicated(., index=yearweek)) %>%
  select(yearweek, number_of_deaths) %>%
  as_tsibble(index = "yearweek") %>%
  tsibble::fill_gaps() %>%
  na.locf() #%>%

model_5 <-
  deaths %>%
  filter(age_group == "75-84 years") %>%
  filter(!are_duplicated(., index=yearweek)) %>%
  select(yearweek, number_of_deaths) %>%
  as_tsibble(index = "yearweek") %>%
  tsibble::fill_gaps() %>%
  na.locf() #%>%

model_6 <-
  deaths %>%
  filter(age_group == "85 years and older") %>%
  filter(!are_duplicated(., index=yearweek)) %>%
  select(yearweek, number_of_deaths) %>%
  as_tsibble(index = "yearweek") %>%
  tsibble::fill_gaps() %>%
  na.locf() #%>%

model_1_components <-
  model_1 %>%
  model(STL(number_of_deaths ~ trend(window = 731) + season(period = "year"))) %>%
  components()

model_2_components <-
  model_2 %>%
  model(STL(number_of_deaths ~ trend(window = 731) + season(period = "year"))) %>%
  components()

model_3_components <-
  model_3 %>%
  model(STL(number_of_deaths ~ trend(window = 731) + season(period = "year"))) %>%
  components()

model_4_components <-
  model_4 %>%
  model(STL(number_of_deaths ~ trend(window = 731) + season(period = "year"))) %>%
  components()

model_5_components <-
  model_5 %>%
  model(STL(number_of_deaths ~ trend(window = 731) + season(period = "year"))) %>%
  components()

model_6_components <-
  model_6 %>%
  model(STL(number_of_deaths ~ trend(window = 731) + season(period = "year"))) %>%
  components()


### Take the above decompositions and focus on the remainder.   Build a tibble
### of excess deaths by age group by taking the remainder
excess_deaths <-
  tibble(
    yearweek = model_1_components$yearweek,
    "Under 25 years" = model_1_components$remainder,
    "25-44 years"    = model_2_components$remainder,
    "45-64 years"    = model_3_components$remainder,
    "65-74 years"    = model_4_components$remainder,
    "75-84 years"    = model_5_components$remainder,
    "85 years and older" = model_6_components$remainder) %>%
  pivot_longer(-yearweek, names_to = "age_group", values_to = "number_of_deaths") %>%
  mutate(age_group = factor(age_group))
    
### Order our graph according to age (otherwise it'll do it by alphabetical order...)
my_order <- c("Under 25 years",
              "25-44 years",
              "45-64 years",
              "65-74 years",
              "75-84 years",
              "85 years and older")

excess_deaths$age_group <- factor(excess_deaths$age_group, levels = my_order)

excess_deaths <-
  excess_deaths %>%
  mutate(year = year(yearweek)) %>%
  mutate(week = week(yearweek)) %>%
  mutate(thisyear = if_else(year == 2020, TRUE, FALSE)) %>%
  group_by(year) %>%
  filter(!(year == 2020 & week %in% seq(max(week) - 1, max(week)))) %>%
  ungroup()

### Create a graph of weekly deaths by age group.   Note that I'm using a
### 5-week moving average of values, as that helps pull the peaks above the 
### noise.   Otherwise the 2020 spike tends to get visually lost in the noise.
g_weekly_deaths_by_age <-
  ggplot(data = excess_deaths, 
         aes(x = week, 
             y = number_of_deaths, 
             group = year)) +
  theme_bw() +
  geom_line(aes(x = week - 2, y = SMA(number_of_deaths, n = 5), col = thisyear)) +
  facet_wrap(~ age_group) + #, scales = "free_y") +
  scale_color_manual(values = c("FALSE" = "gray", 
                                "TRUE" = "red")) +
  guides(col = FALSE) +
  ggtitle("5-week SMA of weekly excess deaths in Tennessee by age") +
  labs(x = "Week", y = "") +
  geom_hline(yintercept = 0, col = "gray") +
  scale_y_continuous(labels = scales::comma)
print(g_weekly_deaths_by_age)


### Instead of looking at excess deaths by age group, let's look at total
### excess deaths across all age groups
total_excess <-
  excess_deaths %>%
  select(yearweek, number_of_deaths) %>%
  group_by(yearweek) %>%
  summarize(number_of_deaths = sum(number_of_deaths)) %>%
  mutate(year = year(yearweek)) %>%
  mutate(week = week(yearweek)) %>%
  mutate(thisyear = if_else(year == 2020, TRUE, FALSE))
  
g_tot_smart <-
  ggplot(data = total_excess, aes(x = week, y = number_of_deaths, group = year)) +
  theme_bw() +
  geom_line(aes(col = thisyear, x = week, y = number_of_deaths)) +
  scale_color_manual(values = c("FALSE" = "gray", 
                                  "TRUE" = "red")) +
  guides(col = FALSE) +
  ggtitle("Excess deaths per week in Tennessee in 2020 compared to 2015-2019 (filtering out growth trend)") +
  labs(x = "Week", y = "") +
  geom_hline(yintercept = 0, col = "gray") +
  scale_y_continuous(labels = scales::comma)
print(g_tot_smart)

g_tot_realsmart <-
  ggplot(data = total_excess, aes(x = week, y = number_of_deaths, group = year)) +
  theme_bw() +
  geom_line(aes(col = thisyear, x = week - 1, y = SMA(number_of_deaths, n = 3))) +
  scale_color_manual(values = c("FALSE" = "gray", 
                                "TRUE" = "red")) +
  guides(col = FALSE) +
  ggtitle("Excess deaths per week in Tennessee in 2020 compared to 2015-2019 (filtering out growth trend and using 3 week SMA)") +
  labs(x = "Week", y = "") +
  geom_hline(yintercept = 0, col = "gray") +
  scale_y_continuous(labels = scales::comma)
print(g_tot_realsmart)

### Let's compare the excess this year with the same periods in previous years
excess_deaths %>% 
  filter(week >= 9 & week <= 30) %>% 
  select(year, number_of_deaths) %>% 
  group_by(year) %>% 
  summarize(number_of_deaths = sum(number_of_deaths))
  

### Let's compare the above approach (filter out the growth trend) to the 
### "naive" approach of just taking an average without filtering it out
n1 <-
  md %>% 
  filter(jurisdiction == "Tennessee") %>% 
  select(yearweek, number_of_deaths) %>% 
  mutate(number_of_deaths = as.integer(number_of_deaths)) %>% 
  group_by(yearweek) %>% 
  summarize(number_of_deaths = sum(number_of_deaths)) %>% 
  mutate(year = year(yearweek)) %>% 
  mutate(week = week(yearweek)) %>% 
  mutate(thisyear = if_else(year == 2020, TRUE, FALSE))

a1 <-
  n1 %>% 
  filter(thisyear == FALSE) %>% 
  select(week, number_of_deaths) %>% 
  group_by(week) %>% 
  summarize(avg_num_of_deaths = mean(number_of_deaths))
  
m1 <- 
  n1 %>%
  left_join(a1, by = "week") %>%
  mutate(excess_deaths = number_of_deaths - avg_num_of_deaths) %>%
  group_by(year) %>%
  filter(!(year == 2020 & week %in% seq(max(week) - 1, max(week)))) %>%
  ungroup()
  
g_tot_naive<-
  ggplot(data = m1, aes(x = week, y = excess_deaths, group = year)) +
  theme_bw() +
  geom_line(aes(col = thisyear)) +
  scale_color_manual(values = c("FALSE" = "gray", 
                                "TRUE" = "red")) +
  guides(col = FALSE) +
  ggtitle("Excess deaths per week in Tennessee in 2020 compared to 2015-2019 (naive approach)") +
  labs(x = "Week", y = "") +
  geom_hline(yintercept = 0, col = "gray") +
  scale_y_continuous(labels = scales::comma)
print(g_tot_naive)

### Graph the naive and smart approaches together so we can see the difference
### it makes.
plot_grid(g_tot_naive,
          g_tot_smart,
          g_tot_realsmart,
          nrow = 3, ncol = 1, align = "hv")

