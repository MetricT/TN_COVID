################################################################################
### Graph:  COVID-19 New Confirmed Cases
################################################################################

### If we assume infected = a * B^(c * days), then
###
###  d(infected)/dt = c * log_e(B) * infected
###
### We can use the same constants from the total fit on the new fit.   If new
### cases are still growing exponentially, it will work.   And when it doesn't
### work, that's a sign that we're either "flattening the curve", or something
### else weird is going on

### Add the fit to our dataframe
#infected_data <- infected_data %>% mutate(fit_new_infected = fit_total_infected * d_constant)

### Set any reversions (ie, when TN DOH revises cases downwards due to error) to 0
infected_data <- 
  infected_data %>%
  mutate(new_infected = if_else(new_infected < 0, 0, new_infected))

### What's the current number of total infected cases
new_num <- infected_data %>% tail(n = 1) %>% select("new_infected") %>% pull() %>% format(big.mark = ",", scientific = FALSE)
SMA <- infected_data$new_infected %>% SMA(n = 7) %>% tail(n = 1) %>% round() %>% format(big.mark = ",", scientific = FALSE)

newinf_title <- paste("New Cases: ", new_num, ", SMA: ", SMA, sep = "")

### Graph:  new COVID-19 Cases
graph_new_infected <- ggplot(data = infected_data) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.7)) +
  theme(axis.text.x = element_text(hjust = 0.8)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  geom_point(data = infected_data, shape = 19, size = 0.5, 
             aes(x = as.Date(date), y = new_infected), color = "black") +
  geom_line(data = infected_data, color = graph_color, size = 1.0,
            aes(x = as.Date(date) - 3, y = SMA(new_infected, n = 7))) + 
  scale_x_date(#date_breaks = "3 days", 
               date_labels = "%m/%d") +
  scale_y_continuous(labels = scales::comma) + 
  #  scale_y_continuous(limits = c(100, 650)) +
  graph_log10_opts1 +
  graph_log10_opts2 +
  labs(title = newinf_title, x = "", y = "")
print(graph_new_infected)

