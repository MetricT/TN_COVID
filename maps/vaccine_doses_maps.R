################################################################################
### Map of first, second, and third vaccines doses by county
################################################################################

# RECIPIENT_COUNT  = Number of recipients/people who received a dose of COVID vaccine (administered and historical) reported to TennIIS.
# RECIP_FULLY_VACC = Number of recipients/people who are fully vaccinated (administered and historical) reported to TennIIS.
# RECIP_ADDL_DOSE  = Number of recipients/people who are have received a third dose of Moderna or Pfizer post 8/13/2021 

doses <- 
  vaccine_age_df %>% 
  filter(Date == "2021-12-07") %>%
  group_by(County) %>% 
  summarize(
    RECIPIENT_COUNT  = sum(RECIPIENT_COUNT,  na.rm = TRUE),
    RECIP_FULLY_VACC = sum(RECIP_FULLY_VACC, na.rm = TRUE),
    RECIP_ADDL_DOSE  = sum(RECIP_ADDL_DOSE,  na.rm = TRUE)
  ) %>% 
  ungroup() %>%
  mutate(
    first_dose  = RECIPIENT_COUNT - RECIP_FULLY_VACC,
    second_dose = RECIP_FULLY_VACC,
    third_dose  = RECIP_ADDL_DOSE) %>%
  select(County, first_dose, second_dose, third_dose)

doses_map <- 
  county_acs %>% 
  mutate(NAME = gsub(" County, Tennessee", "", NAME) %>% str_to_upper()) %>%
  left_join(doses, by = c("NAME" = "County")) %>%
  mutate(
    first_dose_per  =  first_dose / POP2018,
    second_dose_per = second_dose / POP2018,
    third_dose_per  =  third_dose / POP2018,
    no_dose_per     = 1 - (first_dose_per + second_dose_per + third_dose_per)
  )

no_dose_label <- (100 * doses_map$no_dose_per) %>% round(1)
no_dose_label[no_dose_label == 0] <- "0"

first_dose_label <- (100 * doses_map$first_dose_per) %>% round(1)
first_dose_label[first_dose_label == 0] <- "0"

second_dose_label <- (100 * doses_map$second_dose_per) %>% round(1)
second_dose_label[second_dose_label == 0] <- "0"

third_dose_label <- (100 * doses_map$third_dose_per) %>% round(1)
third_dose_label[third_dose_label == 0] <- "0"

### By default use black for the font, but if the value is over 1/2's of the way
### to the max, use the white font instead as it looks nicer
frac_no <- 0.5 * (doses_map$no_dose_per %>% na.omit() %>% max())
doses_map$textcolor_no = if_else(doses_map$no_dose_per > frac_no, "white", "black")

frac_first <- 0.5 * (doses_map$first_dose_per %>% na.omit() %>% max())
doses_map$textcolor_first = if_else(doses_map$first_dose_per > frac_first, "white", "black")

frac_second <- 0.5 * (doses_map$second_dose_per %>% na.omit() %>% max())
doses_map$textcolor_second = if_else(doses_map$second_dose_per > frac_second, "white", "black")

frac_third <- 0.5 * (doses_map$third_dose_per %>% na.omit() %>% max())
doses_map$textcolor_third  = if_else(doses_map$third_dose_per > frac_third , "white", "black")

map_no_dose <-
  ggplot(doses_map) +
  theme_void() +
  theme(legend.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") +
  geom_sf(data = doses_map$geometry, aes(fill = doses_map$no_dose_per), size = geom_thickness) +
  geom_text(data = doses_map,
            color = doses_map$textcolor_no,
            aes(x     = county_centers$x,
                y     = county_centers$y,
                label = no_dose_label),
            nudge_x = doses_map$nudge_x,
            nudge_y = doses_map$nudge_y) +
  labs(title = "Percent of Population Unvaccinated") + 
  scale_fill_viridis(direction = -1, option = "plasma")
print(map_no_dose)

map_first_dose <-
  ggplot(doses_map) +
  theme_void() +
  theme(legend.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") +
  geom_sf(data = doses_map$geometry, aes(fill = doses_map$first_dose_per), size = geom_thickness) +
  geom_text(data = doses_map,
            color = doses_map$textcolor_first,
            aes(x     = county_centers$x,
                y     = county_centers$y,
                label = first_dose_label),
            nudge_x = doses_map$nudge_x,
            nudge_y = doses_map$nudge_y) +
  labs(title = "Percent of Population receiving one dose of vaccine") + 
  scale_fill_viridis(direction = -1, option = "plasma")
print(map_first_dose)

map_second_dose <-
  ggplot(doses_map) +
  theme_void() +
  theme(legend.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") +
  geom_sf(data = doses_map$geometry, aes(fill = doses_map$second_dose_per), size = geom_thickness) +
  geom_text(data = doses_map,
            color = doses_map$textcolor_second,
            aes(x     = county_centers$x,
                y     = county_centers$y,
                label = second_dose_label),
            nudge_x = doses_map$nudge_x,
            nudge_y = doses_map$nudge_y) +
  labs(title = "Percent of Population receiving two doses of vaccine") + 
  scale_fill_viridis(direction = -1, option = "plasma")
print(map_second_dose)

map_third_dose <-
  ggplot(doses_map) +
  theme_void() +
  theme(legend.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") +
  geom_sf(data = doses_map$geometry, aes(fill = doses_map$third_dose_per), size = geom_thickness) +
  geom_text(data = doses_map,
            color = doses_map$textcolor_third,
            aes(x     = county_centers$x,
                y     = county_centers$y,
                label = third_dose_label),
            nudge_x = doses_map$nudge_x,
            nudge_y = doses_map$nudge_y) +
  labs(title = "Percent of Population receiving three doses of vaccine") + 
  scale_fill_viridis(direction = -1, option = "plasma")
print(map_third_dose)

plot_grid(
  map_no_dose,
  map_first_dose,
  map_second_dose,
  map_third_dose,
  ncol = 1, align = "hv")
)


