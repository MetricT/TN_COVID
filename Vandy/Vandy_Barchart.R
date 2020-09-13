################################################################################
### Graph Vanderbilt University COVID-19 confirmed cases
################################################################################

### Load necessary packages
packages <- c("tidyverse", "lubridate", "tsibble", "feasts", "cowplot")
new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages, quiet = TRUE)
invisible(lapply(packages, "library", quietly = TRUE,
                 character.only = TRUE, warn.conflicts = FALSE))

### Scrape data off the Vandy COVID-19 dashboard
# https://www.vanderbilt.edu/coronavirus/covid19dashboard

vandy <- 
  read_csv("vandy_data.csv", col_names = TRUE, col_types = "Ddddddd") %>%
  pivot_longer(-date, names_to = "type", values_to = "values")


#                Faculty     Staff     Postdocs   Ugrad-On   Ugrad-Off  Grad
my_palette <- c("#f8766d", "#00bfc4", "#00ba38", "#f564e3", "#619cff", "#b79f00")

#colfunc <- colorRampPalette(c("#000000", "#866d4b"))
#my_palette <- colfunc(6)

order <- c("Undergrad - On Campus", "Undergrad - Off Campus", "Graduate/Professional",
           "Faculty", "Staff", "Postdocs")

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

#vandy <-
#  vandy %>%
#  filter(date > as.Date("2020-08-24"))

p_stacked <-
  ggplot(data = vandy,
         aes(x = date, y = values)) +
  theme_bw() + 
  theme(legend.title = element_blank()) +
  geom_col(aes(fill = type), width = 0.7) + 
  geom_line(data = vandy %>% select(date, sma) %>% group_by(date) %>% summarize(sma = sum(sma)),
            aes(x = as.Date(date), y = sma), size = 1.0, color = "darkseagreen4") +
  scale_x_date(date_breaks = "3 days", 
               date_labels = "%m/%d") +
  
  # Set the color scale manually
  scale_fill_manual(values =  c("Faculty"                = my_palette[1],
                                "Staff"                  = my_palette[2],
                                "Postdocs"               = my_palette[3],
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
  scale_x_date(date_breaks = "3 days", 
               date_labels = "%m/%d") +
  
  # Set the color scale manually
  scale_fill_manual(values =  c("Faculty"                = my_palette[1],
                                "Staff"                  = my_palette[2],
                                "Postdocs"               = my_palette[3],
                                "Undergrad - On Campus"  = my_palette[4],
                                "Undergrad - Off Campus" = my_palette[5],
                                "Graduate/Professional"  = my_palette[6])
                    ) +
  
  facet_wrap(~type) +
  labs(x = "", y = "# of cases")
print(p_facet)


final <- plot_grid(p_stacked, p_facet, nrow = 2, ncol = 1, align = "hv")
print(final)
