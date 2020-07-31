################################################################################
### The bar chart for cases/deaths by age
################################################################################

age_data <-
  age_df %>%
  filter(DATE == max(DATE)) %>%
  filter(AGE != "TOTAL") %>%
  select(AGE, TOTAL_CASES, TOTAL_DEATHS) %>%
  mutate(TOTAL_CASES = as.double(TOTAL_CASES)) %>%
  rename(age = AGE, cases_statewide = TOTAL_CASES, deaths_statewide = TOTAL_DEATHS) %>%
  mutate(age = as_factor(age)) %>%
  mutate(age = gsub(" years", "", age))

num_inf <- age_data$cases_statewide %>% sum() %>% format(big.mark = ",", scientific = FALSE)
num_dea <- age_data$deaths_statewide   %>% sum() %>% format(big.mark = ",", scientific = FALSE)

################################################################################
### Graph:  COVID-19 cases by Age
################################################################################

graph_age_cases <-
  ggplot(data = age_data, aes(x = cases_statewide, y = age)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.7)) +
  theme(axis.text.x = element_text(hjust = 0.8)) +
  geom_age_orientation +
  geom_bar(stat = "identity", fill = graph_color) +
  geom_text(data = age_data, aes(x = cases_statewide, y = age),
            nudge_x = 0.05 * max(age_data$cases_statewide),
            label = age_data$cases_statewide %>% format(big.mark = ",", scientific = FALSE)) +
  scale_x_continuous(labels = scales::comma,
                     sec.axis = sec_axis(~ . / sum(age_data$cases_statewide), labels = scales::percent_format(accuracy = 1))) +
  labs(title = paste("TN Cases by Age: ", num_inf, sep = ""), x = "", y = "")
#print(graph_age_cases)

################################################################################
### Graph:  COVID-19 Deaths by Age
################################################################################

graph_age_deaths <- ggplot(data = age_data, aes(x = deaths_statewide, y = age)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.7)) +
  theme(axis.text.x = element_text(hjust = 0.8)) +
  geom_age_orientation +
  geom_bar(stat = "identity", fill = graph_color) +
  geom_text(data = age_data, aes(x = deaths_statewide, y = age),
            nudge_x = 0.05 * max(age_data$deaths_statewide),
            label = age_data$deaths_statewide %>% format(big.mark = ",", scientific = FALSE)) +
  scale_x_continuous(labels = scales::comma,
                     sec.axis = sec_axis(~ . / sum(age_data$deaths_statewide), labels = scales::percent)) +
  labs(title = paste("TN Deaths by Age: ", num_dea, sep = ""), x = "", y = "")
#print(graph_age_deaths)

