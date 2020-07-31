### The State of TN missed one day's worth of data on 6/28.  That missing date
### precludes several forms of analysis (particularly STL) which require no
### missing data points.
###
### Add that data back in by taking total's, interpolating values by averaging
### the values the date before/after.
###
### Writing this sucked *so* hard...    It's not simple *after* the fact.   It
### sure as fuck all wasn't simple *before*...

county_new_df_patch_06_28_2020 <-
  county_new_df %>%
  filter(DATE >= as.Date("2020-06-26") & DATE <= as.Date("2020-06-30")) %>%
  select(DATE, COUNTY, TOTAL_CASES, TOTAL_CONFIRMED, TOTAL_PROBABLE, POS_TESTS,
         NEG_TESTS, TOTAL_TESTS, TOTAL_DEATHS, TOTAL_RECOVERED, TOTAL_ACTIVE,
         TOTAL_HOSPITALIZED) %>%
  rename(TOTAL_POS_TESTS = POS_TESTS,
         TOTAL_NEG_TESTS = NEG_TESTS) %>%
  pivot_longer(cols = starts_with("TOTAL_")) %>%
  pivot_wider(
    id_cols = c("DATE", "COUNTY"),
    names_from = c("name", "COUNTY"),
    names_sep = "-",
    values_from = c("value")
  ) %>%
  as_tsibble(index = "DATE") %>%
  tsibble::fill_gaps() %>%
  as_tibble() %>%
  mutate(across(!starts_with("DATE"),
                ~ if_else(is.na(.), round((lag(.) + lead(.)) / 2), .))
  ) %>%
  mutate(across(!starts_with("DATE"),
                .fns = list(new = ~ c(0, diff(.))),
                .names = "{fn}_{col}"
  )) %>%
  rename_at(vars(starts_with("new_TOTAL")),
            ~ str_replace(., "new_TOTAL", "NEW")) %>%
  filter(DATE == as.Date("2020-06-28")) %>%
  pivot_longer(cols = !starts_with("DATE"),
               names_to = c("variable", "COUNTY"),
               names_sep = "-",
               values_to = "values") %>%
  pivot_wider(id_cols = c("DATE", "COUNTY"),
              names_from = "variable",
              values_from = "values") %>%
  select(DATE, COUNTY, TOTAL_CASES, NEW_CASES, TOTAL_CONFIRMED, NEW_CONFIRMED,
         TOTAL_PROBABLE, NEW_PROBABLE, TOTAL_POS_TESTS, NEW_POS_TESTS,
         TOTAL_NEG_TESTS, NEW_NEG_TESTS, TOTAL_TESTS, NEW_TESTS, NEW_DEATHS,
         TOTAL_DEATHS, NEW_RECOVERED, TOTAL_RECOVERED, NEW_ACTIVE, TOTAL_ACTIVE,
         NEW_HOSPITALIZED, TOTAL_HOSPITALIZED) %>%
  rename(POS_TESTS = TOTAL_POS_TESTS,
         NEG_TESTS = TOTAL_NEG_TESTS)

### And another one for age_ss_df...
age_ss_df_patch_06_28_2020 <-
  age_ss_df %>%
  select(DATE, AGE_RANGE, AR_CASECOUNT, AR_TOTALDEATHS) %>%
  rename(TOTAL_CASES = AR_CASECOUNT, TOTAL_DEATHS = AR_TOTALDEATHS) %>%
  filter(DATE >= as.Date("2020-06-26") & DATE <= as.Date("2020-06-30")) %>%
  pivot_wider(id_cols = c("DATE"),
              names_from = c("AGE_RANGE"),
              names_sep = ":",
              values_from = c("TOTAL_CASES", "TOTAL_DEATHS")) %>%
  as_tsibble(index = "DATE") %>%
  tsibble::fill_gaps() %>%
  as_tibble() %>%
  mutate(across(!starts_with("DATE"),
                ~ if_else(is.na(.), round((lag(.) + lead(.)) / 2), .))
  ) %>%
  mutate(across(!starts_with("DATE"),
                .fns = list(new = ~ c(0, diff(.))),
                .names = "{fn}_{col}"
  )) %>%
  rename_at(vars(starts_with("new_TOTAL")),
            ~ str_replace(., "new_TOTAL", "NEW")) %>%
  filter(DATE == as.Date("2020-06-28")) %>%
  pivot_longer(cols = !starts_with("DATE"),
               names_to = c("variable", "AGE_RANGE"),
               names_sep = ":",
               values_to = "values") %>%
  pivot_wider(id_cols = c("DATE", "AGE_RANGE"),
              names_from = "variable",
              values_from = "values") %>%
  rename(AR_CASECOUNT = TOTAL_CASES,
         AR_TOTALDEATHS = TOTAL_DEATHS,
         NEW_ARCASES = NEW_CASES,
         AR_NEWDEATHS = NEW_DEATHS) %>%
  mutate(AR_TOTALPERCENT = round(AR_CASECOUNT / sum(AR_CASECOUNT), 2)) %>%
  mutate(AR_NEWPERCENT = round(NEW_ARCASES / sum(NEW_ARCASES), 4)) %>%
  select(DATE, AGE_RANGE, AR_CASECOUNT, AR_TOTALPERCENT, NEW_ARCASES,
         AR_NEWPERCENT, AR_TOTALDEATHS, AR_NEWDEATHS)


### And another one for age_by_county_df...
age_by_county_df_patch_06_28_2020 <-
  age_by_county_df %>%
  rename(TOTAL_CASES = CASE_COUNT) %>%
  filter(DATE >= as.Date("2020-06-26") & DATE <= as.Date("2020-06-30")) %>%
  pivot_wider(id_cols = c("DATE"),
              names_from = c("COUNTY", "AGE_GROUP"),
              names_sep = ":",
              names_prefix = "TOTAL:",
              values_from = c("TOTAL_CASES")) %>%
  as_tsibble(index = "DATE") %>%
  tsibble::fill_gaps() %>%
  as_tibble() %>%
  mutate(across(!starts_with("DATE"),
                ~ if_else(is.na(.), round((lag(.) + lead(.)) / 2), .))
  ) %>%
  mutate(across(!starts_with("DATE"),
                .fns = list(new = ~ c(0, diff(.))),
                .names = "{fn}_{col}"
  )) %>%
  rename_at(vars(starts_with("new_TOTAL")),
            ~ str_replace(., "new_TOTAL", "NEW")) %>%
  filter(DATE == as.Date("2020-06-28")) %>%
  pivot_longer(cols = !starts_with("DATE"),
               names_to = c("TYPE", "COUNTY", "AGE_GROUP"),
               names_sep = ":",
               values_to = "values") %>%
  pivot_wider(id_cols = c("DATE", "COUNTY", "AGE_GROUP"),
              names_from = "TYPE",
              values_from = "values") %>%
  rename(CASE_COUNT = TOTAL,
         NEW_CASE_COUNTY = NEW) %>%
  select(-NEW_CASE_COUNTY)


### And another one for race_ethnicity_sex_df...
race_ethnicity_sex_df_patch_06_28_2020 <-
  race_ethnicity_sex_df %>%
  select(Date, Category, CAT_DETAIL, Cat_CaseCount, CAT_DEATHCOUNT) %>%
  rename(DATE = Date,
         TOTAL_CASES = Cat_CaseCount,
         TOTAL_DEATHS = CAT_DEATHCOUNT) %>%
  filter(DATE >= as.Date("2020-06-26") & DATE <= as.Date("2020-06-30")) %>%
  pivot_wider(id_cols = c("DATE"),
              names_from = c("Category", "CAT_DETAIL"),
              names_sep = ":",
              values_from = c("TOTAL_CASES", "TOTAL_DEATHS")) %>%
  as_tsibble(index = "DATE") %>%
  tsibble::fill_gaps() %>%
  as_tibble() %>%
  mutate(across(!starts_with("DATE"),
                ~ if_else(is.na(.), round((lag(.) + lead(.)) / 2), .))
  ) %>%
  mutate(across(!starts_with("DATE"),
                .fns = list(new = ~ c(0, diff(.))),
                .names = "{fn}_{col}"
  )) %>%
  rename_at(vars(starts_with("new_TOTAL")),
            ~ str_replace(., "new_TOTAL", "NEW")) %>%
  filter(DATE == as.Date("2020-06-28")) %>%
  pivot_longer(cols = !starts_with("DATE"),
               names_to = c("TYPE", "Category", "CAT_DETAIL"),
               names_sep = ":",
               values_to = "values") %>%
  pivot_wider(id_cols = c("CAT_DETAIL", "DATE", "Category"),
              names_from = "TYPE",
              values_from = "values") %>%
  rename(Date = DATE,
         Cat_CaseCount = TOTAL_CASES,
         CAT_DEATHCOUNT = TOTAL_DEATHS) %>%
  select(CAT_DETAIL, Date, Category, Cat_CaseCount, CAT_DEATHCOUNT) %>%
  pivot_wider(id_cols = c("Date", "Category"),
              names_from = c("CAT_DETAIL"),
              names_sep = ":",
              values_from = c("Cat_CaseCount", "CAT_DEATHCOUNT")) %>%
  mutate(Total = rowSums(select(., !starts_with(c("Date", "Category"))),
                         na.rm = TRUE)) %>%
  mutate(across(!starts_with(c("Date", "Category", "Total")),
         .fns = list(per = ~ . / Total),
         .names = "{fn}_{col}")) %>%
  select(-Total) %>%
  pivot_longer(cols = !starts_with(c("Date", "Category")),
               names_to = c("type", "CAT_DETAIL"),
               names_sep = ":",
               values_to = "values") %>%
  filter(!is.na(values)) %>%
  pivot_wider(id_cols = c("Date", "CAT_DETAIL", "Category"),
              names_from = c("type"),
              values_from = c("values")) %>%
  select(CAT_DETAIL, Date, Category, Cat_CaseCount, per_Cat_CaseCount,
         CAT_DEATHCOUNT, per_CAT_DEATHCOUNT) %>%
  rename(Cat_Percent = per_Cat_CaseCount,
         CAT_DEATHPERCENT = per_CAT_DEATHCOUNT)

### And another one for daily_case_df...
daily_case_df_patch_06_28_2020 <-
  daily_case_df %>%
  filter(DATE >= as.Date("2020-06-26") & DATE <= as.Date("2020-06-30")) %>%
  rename(TOTAL_POS_TESTS = POS_TESTS,
         TOTAL_NEG_TESTS = NEG_TESTS) %>%
  as_tsibble(index = "DATE") %>%
  tsibble::fill_gaps() %>%
  select(-starts_with("NEW_")) %>%
  mutate(across(!starts_with("DATE"),
                ~ if_else(is.na(.), round((lag(.) + lead(.)) / 2), .))
  ) %>%
  mutate(across(!starts_with("DATE"),
                .fns = list(new = ~ c(0, diff(.))),
                .names = "{fn}_{col}"
  )) %>%
  rename_at(vars(starts_with("new_TOTAL")),
            ~ str_replace(., "new_TOTAL", "NEW")) %>%
  filter(DATE == as.Date("2020-06-28")) %>%
  rename(POS_TESTS = TOTAL_POS_TESTS,
         NEG_TESTS = TOTAL_NEG_TESTS) %>%
  select(DATE, TOTAL_CASES, NEW_CASES, TOTAL_CONFIRMED, NEW_CONFIRMED,
         TOTAL_PROBABLE, NEW_PROBABLE, POS_TESTS, NEG_TESTS, TOTAL_TESTS,
         NEW_TESTS, NEW_DEATHS, TOTAL_DEATHS, NEW_RECOVERED, TOTAL_RECOVERED,
         NEW_ACTIVE, TOTAL_ACTIVE, NEW_HOSP, TOTAL_HOSP)

### Now merge the patches into the original data
county_new_df <-
  county_new_df %>%
  bind_rows(county_new_df_patch_06_28_2020) %>%
  arrange(DATE)

age_by_county_df <-
  age_by_county_df %>%
  bind_rows(age_by_county_df_patch_06_28_2020) %>%
  arrange(DATE)

age_ss_df <-
  age_ss_df %>%
  bind_rows(age_ss_df_patch_06_28_2020) %>%
  arrange(DATE)

race_ethnicity_sex_df <-
  race_ethnicity_sex_df %>%
  bind_rows(race_ethnicity_sex_df_patch_06_28_2020) %>%
  arrange(Date)

daily_case_df <-
  daily_case_df %>%
  bind_rows(daily_case_df_patch_06_28_2020) %>%
  arrange(DATE)
