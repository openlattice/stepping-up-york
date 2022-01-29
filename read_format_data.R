basedir = "data"

cat(file=stderr(), "\nreading data...\n")

#---------------- Read in the csv of cleaned, combined data made in `data_creation.Rmd`
yorkjail_data <- vroom(file.path(basedir, "york_jaildata_combined.csv"),
                       col_types = c(yr_booked = "c"))


#---- table for Jail Value Boxes - BOOKINGS PER YEAR
# total ppl, bookings, ppl with D per yr
numbookings_status <-  yorkjail_data %>%
  filter(yr_booked_num >= 2017) %>%
  mutate(status = replace_na(status, "Not Evaluated"),
         status = factor(status, levels = c("MHSR-A", "MHSR-B", "MHSR-C", "MHSR-D", "Not Evaluated"))) %>%
  group_by(yr_booked_num, yr_booked, status) %>%
  summarize(n_bookings = n_distinct(personID)) %>%
  ungroup() %>%
  rbind(
    yorkjail_data %>%
      filter(yr_booked_num >= 2017) %>%
      group_by(yr_booked_num, yr_booked) %>%
      summarize(n_bookings = n_distinct(personID)) %>%
      mutate(status = "All") %>%
      ungroup()
  ) %>%
  arrange(yr_booked, status)

#-------- AVG LENGTH OF STAY (ALOS)
# lump everything not D into non-SMI?
alos_allinmates <- yorkjail_data %>%
  mutate(yr_released=if_else(!is.na(DateTimeRelease),
                             year(DateTimeRelease),
                             year(Sys.Date())),
         status = replace_na(status, "Not Evaluated"),
         status = ordered(status, levels = c("MHSR-A", "MHSR-B", "MHSR-C", "MHSR-D", "Not Evaluated"))) %>%
  group_by(status, yr_released) %>%
  summarise(sum_time = sum(ol_timeserveddays, na.rm = TRUE),
            n_releases = n_distinct(GeneralIdentifier),
            avg_los = round(sum(ol_timeserveddays, na.rm = TRUE) / n_distinct(GeneralIdentifier), 1)) %>%
  ungroup() %>%
  arrange(yr_released, status)

#-------- RECIDIVISM
#' Calculating the Stats
#' = (# admitted who have a prior jail admission in your county jail in the past year) / (total admissions during the reporting period)
  
recidivism <- yorkjail_data %>% 
  filter(lookbackflag==1 ) %>% #& !is.na(status)
  mutate(status = replace_na(status, "Not Evaluated"),
         status = ordered(status, levels = c("MHSR-A", "MHSR-B", "MHSR-C", "MHSR-D", "Not Evaluated"))) %>%
  group_by(yr_booked, status) %>%
  summarize(num_withflag = n_distinct(personID)) %>%
  ungroup() %>%
  left_join(
    yorkjail_data %>% 
      mutate(status = replace_na(status, "Not Evaluated"),
             status = ordered(status, levels = c("MHSR-A", "MHSR-B", "MHSR-C", "MHSR-D", "Not Evaluated"))) %>%
      group_by(yr_booked, status) %>%
      summarize(
        n_bookings = n_distinct(GeneralIdentifier)),
    by = c("yr_booked", "status")
  ) %>%
  mutate(recidivism = (num_withflag/n_bookings)*100)





