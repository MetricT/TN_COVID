################################################################################
### Graph Vanderbilt University COVID-19 confirmed cases
################################################################################

### Load necessary packages
packages <- c("tidyverse", "lubridate", "TTR", "tsibble", "feasts", "cowplot", "EpiEstim")
new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages, quiet = TRUE)
invisible(lapply(packages, "library", quietly = TRUE,
                 character.only = TRUE, warn.conflicts = FALSE))


### Load COVID data from Google Sheets
library("googlesheets4")
gs4_deauth()
vandy_covid_data_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRgMqMAGHvc1hDZjiBn5NnT11v5MPLkukIGsvuXeSVKHKHIGZ_DgnInuh9Pa2OlCORp0ejxRJpfECY2/pubhtml?gid=153995963"
vandy_covid_data <- read_sheet(vandy_covid_data_url)






### Scrape data off the Vandy COVID-19 dashboard
# https://www.vanderbilt.edu/coronavirus/covid19dashboard

vandy <- 
  read_csv("Vandy/vandy_data.csv", col_names = TRUE, col_types = "Dddddd") %>%
#  mutate("Staff and Postdocs" = Staff + Postdocs) %>%
#  select(date, 
#         `Graduate/Professional`, 
#         `Undergrad - Off Campus`, 
#         `Undergrad - On Campus`,
#         `Staff and Postdocs`,
#         Faculty)
  pivot_longer(-date, names_to = "type", values_to = "values")


#                Faculty     Staff     Postdocs   Ugrad-On   Ugrad-Off  Grad
my_palette <- c("#f8766d", "#00bfc4", "#00ba38", "#f564e3", "#619cff", "#b79f00")

order <- c("Undergrad - On Campus", "Undergrad - Off Campus", "Graduate/Professional",
           "Faculty", "Staff and Postdocs")

### Add a SMA column for each data type
vandy <- 
  vandy %>%
  pivot_wider(id_cols = "date", 
              names_from = "type", 
              names_prefix = "values:",
              values_from = "values") %>%
  mutate(across(!starts_with("date"),
                .fns = list(sma = ~ SMA(., n = 7)),
                .names = "{fn}:{col}"
  )) %>%
  rename_at(vars(starts_with("sma:values")),
            ~ str_replace(., "sma:values", "sma")) %>%
  pivot_longer(-date, names_to = c("data", "type"), names_sep = ":", values_to = "values") %>%
  pivot_wider(id_cols = c("date", "type"), names_from = "data", values_from = "values")

vandy$type = factor(vandy$type, levels = rev(order))

p_stacked <-
  ggplot(data = vandy,
         aes(x = date, y = values)) +
  theme_bw() + 
  theme(legend.title = element_blank()) +
  geom_col(aes(fill = type), width = 0.7) + 
  geom_line(data = vandy %>% select(date, sma) %>% group_by(date) %>% summarize(sma = sum(sma)),
            aes(x = as.Date(date), y = sma), size = 1.0, color = "darkseagreen4") +
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%m/%d") +
  
  # Set the color scale manually
  scale_fill_manual(values =  c("Faculty"                = my_palette[1],
                                "Staff and Postdocs"     = my_palette[2],
                                "Undergrad - On Campus"  = my_palette[4],
                                "Undergrad - Off Campus" = my_palette[5],
                                "Graduate/Professional"  = my_palette[6])) +
  
  labs(x = "",
       y = "# of cases",
       title = "Vanderbilt University Confirmed COVID-19 Cases by date")
print(p_stacked)

vandy$type = factor(vandy$type, levels = order)

p_facet <-
  ggplot(data = vandy, aes(x = date, y = values)) +
  theme_bw() + 
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  geom_col(aes(fill = type), width = 0.7) + 
  geom_line(data = vandy, aes(x = as.Date(date), y = sma), size = 1.0, color = "darkseagreen4") +
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%m/%d") +
  
  # Set the color scale manually
  scale_fill_manual(values =  c("Faculty"                = my_palette[1],
                                "Staff and Postdocs"     = my_palette[2],
                                "Undergrad - On Campus"  = my_palette[4],
                                "Undergrad - Off Campus" = my_palette[5],
                                "Graduate/Professional"  = my_palette[6])
                    ) +
  
  facet_wrap(~type, nrow = 1, ncol = 5) +
  labs(x = "", y = "# of cases")
print(p_facet)


################################################################################
### Load testing data and graph it
################################################################################

testing <- read_csv("Vandy/vandy_tests.txt")

scale <- 500000

p_test <-
  ggplot(data = testing) +
  theme_bw() + 
  geom_line(aes(x = as.Date(date), y = tests),           color = "steelblue2") +
  geom_line(aes(x = as.Date(date), y = positivity_rate * scale), color = "firebrick2") +
  scale_y_continuous(labels = scales::comma,
                     sec.axis = sec_axis(~ . / scale, 
                                         name   = "Positivity Rate",
                                         labels = scales::percent_format(accuracy = 0.01))) +
  
  
  labs(title = "Vanderbilt University Tests (blue) and Positivity Rate (red)", x = "Week", y = "Tests")
print(p_test)

################################################################################
### The model EpiEstim uses requires the "serial interval", ie the lag in time
### between infection and symptoms appearing.    I use values from the CDC paper
### below.
### https://wwwnc.cdc.gov/eid/article/26/6/20-0357_article
################################################################################

data <- 
  vandy %>% 
  select(date, type, values) %>% 
  rename(dates = date,
         location = type,
         I = values) %>%
  select(dates, I) %>% 
  group_by(dates) %>% 
  summarize(I = sum(I))

si_mean <- 3.96
si_std  <- 4.75

### Create a blank tibble to hold our EpiEstim output
Rt_tib <-
  tribble(
    ~dates, ~mean_r, ~std_r,
  )

estimate <- 
  estimate_R(data,
             method = "parametric_si", 
             config = make_config(list(mean_si = si_mean,
                                       std_si = si_std)))

e_dates  <- estimate$dates %>% as_tibble() %>% filter(row_number() > 7)
e_values <- estimate$R %>% as_tibble() %>% select("Mean(R)", "Std(R)")

r_data <- 
  e_dates %>% 
  bind_cols(e_values) %>% 
  janitor::clean_names() %>% 
  rename(dates = value)

g_Rt_estimate <-
  ggplot(data = r_data, 
         aes(x = as.Date(dates), y = mean_r)) + 
  theme_bw() + 
  scale_y_continuous(limits = c(0.5, 1.75)) +
  geom_hline(yintercept = 1, linetype = "dashed") + 
  geom_point(aes(y = mean_r), size = 1.0) + 
  geom_ribbon(aes(ymin = mean_r - std_r, 
                  ymax = mean_r + std_r),
              color = NA, fill = "darkgrey", alpha = 0.2) +
  labs(title = "Rt estimate for Vanderbilt University", x = "", y = "Rt")
print(g_Rt_estimate)

################################################################################
### Combine it into a final graph
################################################################################

top <- plot_grid(p_test, p_stacked, nrow = 1, ncol = 2, align = "hv", rel_widths = c(1, 1))
print(top)


final <- plot_grid(top, p_facet, g_Rt_estimate, nrow = 3, ncol = 1, align = "hv", rel_heights = c(1, 0.6, 1))
print(final)





