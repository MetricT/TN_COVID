################################################################################
### Graph:  COVID-19 Total Confirmed Cases
################################################################################

#infected_data <-
#  infected_data %>%
#  mutate(numdays = difftime(as.POSIXct(date),
#                            as.POSIXct(as.Date(min(date)) - 1, tz = "UTC"),
#                            units = "days")) %>%
#  mutate(numdays = as.numeric(numdays))

### Optional:  Create weights to help work around any "weird" data
#infected_data <-
#  infected_data %>%
#  mutate(weights = if_else(date > as.Date("2020-03-23"), 0, 1))

### Back up when it's feeling singular (use to generate starting values for nls())
#fit_total_infected <- lm(log(total_infected) ~  numdays, data = infected_data)

### Use non-linear fitting to do the exponential fit
#fit_total_infected <-
#  nls(total_infected ~ a * exp(b * numdays), #weights = weights,
#      data = infected_data, start = list(a = 90, b = 0.1))

### Extract the regression coefficents so we can add the regression formula
### to the graph
#intercept  <- coef(fit_total_infected)[1]
#d_constant <- coef(fit_total_infected)[2]

### Add the fit to our dataframe
#infected_data <-
#  infected_data %>%
#  mutate(fit_total_infected = intercept * exp(d_constant * infected_data$numdays))

### Compute the doubling time
#doubling_time <- log(2) / d_constant

### Compute the "order-of-magnitude" time
#magnitude_time <- log(10) / d_constant

### Growth rate
#growth_rate <- exp(log(2) / doubling_time) - 1

### What's the current number of total infected cases
total_num <- infected_data %>% tail(n = 1) %>% select("total_infected") %>% pull()
total_max <- infected_data %>% select("total_infected") %>% max()

# What's the date of the last data point
data_date <- infected_data  %>% select("date") %>% tail(n = 1) %>% pull()

title_string <- paste("COVID-19 Cases Reported - ", location,
                      " [", as.Date(data_date), "]", sep = "")

graph_total_infected <- ggplot(data = infected_data) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.7)) +
  theme(axis.text.x = element_text(hjust = 0.8)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  geom_point(data = infected_data, shape = 19, size = 0.5,
             aes(x = as.Date(date), y = total_infected), color = "black") +
  geom_line(data = infected_data, color = graph_color, size = 1.0,
            aes(x = as.Date(date) - 3, y = SMA(total_infected, n = 7))) + 
  scale_x_date(date_breaks = "3 days", date_labels = "%m/%d") +
  #  geom_vline(xintercept = as.Date("2020-03-15")) + 
  graph_log10_opts1 +
  graph_log10_opts2 +
  labs(title = paste("Total Cases: ", total_num, sep = ""), x = "", y = "")
#print(graph_total_infected)
