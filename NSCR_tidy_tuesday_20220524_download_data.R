# Download data from Statistics Netherlands 
# Wim Bernasco, May 19, 2022

# Load packages -----------------------------------------------------------
library(tidyverse)   # data manipulation
library(cbsodataR)   # access CBS data (cbs_get_data() function)


setwd("C:/Users/wimbe/KINGSTON/R/Work/MyTidyTuesdays/NSC-R_tidy_tuesday_20220524")

  # Download crime frequencies 
  crimes <-
    cbs_get_data(
      id = "47022NED", catalog = "Politie",
        base_url = "https://dataderden.cbs.nl",
      # only national level frequencies are needed
      WijkenEnBuurten = has_substring("NL")
    ) %>%
    # create a date-class for time
    cbs_add_date_column() %>%
    select(crime_type_code  = SoortMisdrijf,
           date             = Perioden_Date,
           frequency        = GeregistreerdeMisdrijven_1) %>%
    # replace NA with 0
    mutate(frequency = replace_na(frequency, 0),
           # remove trailing whitespace
           crime_type_code  = trimws(crime_type_code)) %>%
    filter(crime_type_code %in% c("1.1.1", "1.2.3", "1.4.5")) %>%
    mutate(crime_type = 
             case_when(crime_type_code == "1.1.1" ~ "burglary", 
                       crime_type_code == "1.2.3" ~ "bike theft",
                       crime_type_code == "1.4.5" ~ "assault")) %>%
    select(-crime_type_code)
                                  
  write_csv(crimes, "crimes.csv")

  # Download population frequencies 
  population <-
    cbs_get_data(
      id = "83474NED", catalog = "CBS") %>%
    # create a date-class for time
    cbs_add_date_column() %>%
    # select only monthly (not yearly) records
    filter(Perioden_freq == "M") %>%
    select(date             = Perioden_Date,
           population       = BevolkingAanHetBeginVanDePeriode_1) %>%
    filter(date >= as.Date("2012-01-01"))

    write_csv(population, "population.csv")

