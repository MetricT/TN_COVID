################################################################################
### The bar chart for cases/deaths by age
################################################################################

age_data <-
  age_df %>%
  filter(DATE == max(DATE)) %>%
  filter(AGE != "TOTAL") %>%
  select(AGE, TOTAL_CASES, TOTAL_DEATHS) %>%
  mutate(TOTAL_CASES = as.double(TOTAL_CASES)) %>%
  rename(age = AGE, infected_statewide = TOTAL_CASES, deaths_statewide = TOTAL_DEATHS) %>%
  mutate(infected_per = infected_statewide / sum(infected_statewide)) %>%
  mutate(deaths_per   = deaths_statewide   / sum(deaths_statewide)) %>%
  mutate(age = as_factor(age))

################################################################################
### Graph:  COVID-19 Infected by Age
################################################################################

graph_age_infected_per <-
  ggplot(data = age_data, aes(x = infected_per, y = age)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.7)) +
  theme(axis.text.x = element_text(hjust = 0.8)) +
  geom_age_orientation +
  geom_bar(stat = "identity", fill = graph_color) +
  scale_x_continuous(labels = scales::percent) +
  labs(title = "TN Infected by Age", x = "", y = "")
print(graph_age_infected_per)

################################################################################
### Graph:  COVID-19 Deaths by Age
################################################################################

graph_age_deaths_per <-
  ggplot(data = age_data, aes(x = deaths_per, y = age)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.7)) +
  theme(axis.text.x = element_text(hjust = 0.8)) +
  geom_age_orientation +
  geom_bar(stat = "identity", fill = graph_color) +
  scale_x_continuous(labels = scales::percent) +
  labs(title = "TN Deaths by Age", x = "", y = "")
print(graph_age_deaths_per)
