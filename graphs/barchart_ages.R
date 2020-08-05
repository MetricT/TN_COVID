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
  mutate(age = gsub(" years", "", age)) %>% 
  filter(age != "Pending")

num_inf <- age_data$cases_statewide %>% sum() %>% format(big.mark = ",", scientific = FALSE)
num_dea <- age_data$deaths_statewide   %>% sum() %>% format(big.mark = ",", scientific = FALSE)

################################################################################
### Graph:  COVID-19 cases by Age
################################################################################

graph_age_cases <-
  ggplot(data = age_data, 
         aes(x = age, y = cases_statewide)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.7)) +
  theme(axis.text.x = element_text(hjust = 0.8)) +
  geom_bar(stat = "identity", fill = graph_color) +
  geom_text(nudge_y = 0.05 * max(age_data$cases_statewide),
            label = age_data$cases_statewide %>% format(big.mark = ",", scientific = FALSE)) +
  scale_y_continuous(labels = scales::comma,
                     sec.axis = sec_axis(~ . / sum(age_data$cases_statewide), labels = scales::percent_format(accuracy = 1))) +
  labs(title = paste("TN Cases by Age: ", num_inf, sep = ""), x = "", y = "")
#print(graph_age_cases)

################################################################################
### Graph:  COVID-19 Deaths by Age
################################################################################

graph_age_deaths <- 
  ggplot(data = age_data, 
         aes(x = age, y = deaths_statewide)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.7)) +
  theme(axis.text.x = element_text(hjust = 0.8)) +
  geom_bar(stat = "identity", fill = graph_color) +
  geom_text(nudge_y = 0.05 * max(age_data$deaths_statewide),
            label = age_data$deaths_statewide %>% format(big.mark = ",", scientific = FALSE)) +
  scale_y_continuous(labels = scales::comma,
                     sec.axis = sec_axis(~ . / sum(age_data$deaths_statewide), labels = scales::percent)) +
  labs(title = paste("TN Deaths by Age: ", num_dea, sep = ""), x = "", y = "")
#print(graph_age_deaths)

### Death rate by age

death_rate_data <-
  age_df %>% 
  filter(DATE == as.Date("2020-07-31")) %>% 
  filter(AGE != "Pending") %>%
  mutate(AGE = gsub(" years", "", AGE)) %>%
  rename(age = AGE) %>%
  select(-NEW_CASES, -NEW_DEATHS) %>% 
  mutate(death_rate_by_age = round(TOTAL_DEATHS / TOTAL_CASES, 4))

graph_age_deathrate <- 
  ggplot(data = death_rate_data, aes(x = age, y = death_rate_by_age)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.7)) +
  theme(axis.text.x = element_text(hjust = 0.8)) +
  geom_bar(stat = "identity", fill = graph_color) +
  geom_text(nudge_y = 0.05 * max(death_rate_data$death_rate_by_age),
            label = paste(100 * death_rate_data$death_rate_by_age, "%", sep = "")) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Death Rate as a % of Confirmed Cases", x = "", y = "")
#print(graph_age_deathrate)

new_death_rate_data <-
  age_df %>%
  filter(DATE >= Sys.Date() - 14) %>%
  select(DATE, AGE, NEW_CASES, NEW_DEATHS) %>% 
  group_by(AGE) %>% 
  summarize(new_deaths_sum = sum(NEW_DEATHS), 
            new_cases_sum = sum(NEW_CASES)) %>% 
  mutate(new_deaths_sum = if_else(new_deaths_sum < 0, 0, new_deaths_sum)) %>% 
  mutate(death_rate_by_age = round(new_deaths_sum / new_cases_sum, 4)) %>% 
  filter(AGE != "Pending") %>%
  rename(age = AGE)

graph_new_age_deathrate <- 
  ggplot(data = new_death_rate_data, aes(x = age, y = death_rate_by_age)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.7)) +
  theme(axis.text.x = element_text(hjust = 0.8)) +
  geom_bar(stat = "identity", fill = graph_color) +
  geom_text(nudge_y = 0.05 * max(new_death_rate_data$death_rate_by_age),
            label = paste(100 * new_death_rate_data$death_rate_by_age, "%", sep = "")) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Death Rate as a % of Confirmed Cases using last 14 days", x = "", y = "")
#print(graph_new_age_deathrate)

plot_grid(graph_age_deathrate, graph_new_age_deathrate, nrow = 1, ncol = 2, align = "hv")