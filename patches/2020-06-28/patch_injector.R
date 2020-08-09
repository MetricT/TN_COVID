################################################################################
###
### Merge the patches for the "missing" date of2020-06-28 into the data
###
### Using this as an alternative to patch.R as it's faster and easier to pull
### bits into other scripts.
###
################################################################################

### Load patched data from spreadsheet
county_new_df_patch_06_28_2020 <-
  "patches/2020-06-28/county_new.csv" %>%
  read_csv(col_names = TRUE, col_types = "Dcdddddddddddddddddddd")

age_by_county_df_patch_06_28_2020 <-
  "patches/2020-06-28/age_by_county.csv" %>%
  read_csv(col_names = TRUE, col_types = "Dccd")

age_ss_df_patch_06_28_2020 <-
  "patches/2020-06-28/age_ss.csv" %>%
  read_csv(col_names = TRUE, col_types = "Dcdddddd")

daily_case_df_patch_06_28_2020 <-
  "patches/2020-06-28/daily_case.csv" %>%
  read_csv(col_names = TRUE, col_types = "Ddddddddddddddddddd")

race_ethnicity_sex_df_patch_06_28_2020 <-
  "patches/2020-06-28/race_ethnicity_sex.csv" %>%
  read_csv(col_names = TRUE, col_types = "cDcdddd")


### Now take the spreadsheets and merge them into our existing data
county_new_df <-
  county_new_df %>%
  bind_rows(county_new_df_patch_06_28_2020) %>%
  unique() %>%
  arrange(DATE)

age_by_county_df <-
  age_by_county_df %>%
  bind_rows(age_by_county_df_patch_06_28_2020) %>%
  unique() %>%
  arrange(DATE)

age_ss_df <-
  age_ss_df %>%
  bind_rows(age_ss_df_patch_06_28_2020) %>%
  unique() %>%
  arrange(DATE)

daily_case_df <-
  daily_case_df %>%
  bind_rows(daily_case_df_patch_06_28_2020) %>%
  unique() %>%
  arrange(DATE)

race_ethnicity_sex_df <-
  race_ethnicity_sex_df %>%
  bind_rows(race_ethnicity_sex_df_patch_06_28_2020) %>%
  unique() %>%
  arrange(Date) 