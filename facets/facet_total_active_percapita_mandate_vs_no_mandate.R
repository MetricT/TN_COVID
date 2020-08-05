################################################################################
### Graph:  COVID-19 Total active
################################################################################

library(tidyverse)
library(geofacet)

data <- 
  read_csv("data/mandates.csv", col_names = TRUE, col_types = "cDc") %>%
  mutate(woy = week(effective_date))

counties <- data$county

graph_color <- "steelblue2"

county_pop <-
  tn_pop_df %>%
  filter(County %in% counties) %>%
  select(County, POP2018)

total_active <-
  total_active_tib %>%
  select("Date", counties) %>%
  pivot_longer(-Date) %>%
  left_join(county_pop, by = c("name" = "County")) %>%
  left_join(data, by = c("name" = "county")) %>%)
  s


            
  mutate(active_rate = 100 * value / POP2018) %>%
  rename(County = name) %>%
  select(Date, County, active_rate) %>%
  filter(Date >= as.Date("2020-06-01"))

totact_title <- "Active COVID-19 Cases as % of County Population"

combined <-
  total_active %>%
  left_join(data, by = c("County" = "county"))

g_mask_mandate <-
  ggplot(data = combined %>% subset(mandate_type == "general"), 
         aes(x = as.Date(Date), y = active_rate / 100)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.7)) +
  theme(axis.text.x = element_text(hjust = 0.8)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +

  geom_vline(mapping = aes(xintercept = as.Date(effective_date))) + 
  
  geom_line(aes(color = mandate_type), size = line_thickness) +

  facet_wrap(~ County) +

  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous(labels = scales::percent, breaks = c(0.000, 0.002, 0.004, 0.006, 0.008, 0.010)) +
  labs(title = totact_title, x = "", y = "")
print(g_mask_mandate)


graph_total_active_county_percapita <-
  ggplot(data = combined, aes(x = as.Date(Date), y = active_rate / 100)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.7)) +
  theme(axis.text.x = element_text(hjust = 0.8)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  
  geom_vline(combined %>% subset(mandate_type == "general"),
             mapping = aes(xintercept = as.Date(effective_date))) + 
  
  geom_line(aes(color = mandate_type), size = line_thickness) +
  
  facet_wrap(~ mandate_type + County) +
  
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous(labels = scales::percent, breaks = c(0.000, 0.002, 0.004, 0.006, 0.008, 0.010)) +
  labs(title = totact_title, x = "", y = "")
print(graph_total_active_county_percapita)

# merge into "mandate" and "no mandate" graphs:
foo <-
  combined %>% 
  filter(mandate_type != "limited_public_buildings") %>% 
  select(-County, -effective_date) %>%
  group_by(Date, mandate_type, woy) %>%
  summarize(active_rate = mean(active_rate))

g_both <-
  ggplot(data = foo, aes(x = as.Date(Date), y = active_rate / 100)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.7)) +
  theme(axis.text.x = element_text(hjust = 0.8)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  
  geom_line(aes(color = mandate_type), size = line_thickness) +
  
  facet_wrap(~ mandate_type + woy) +
  
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous(labels = scales::percent, breaks = c(0.000, 0.002, 0.004, 0.006, 0.008, 0.010)) +
  labs(title = totact_title, x = "", y = "")
print(g_both)