### =========== Connect to database =============
### =============================================
oxrep_db <- dbPool(
  drv = RMySQL::MySQL(),
  dbname = "oxrep",
  port = 3306,
  host = "163.1.169.203",
  user = "oerc0118",
  # user = "shiney"
  password = "wioPLHSAK6nxN2"
)

### =========== Get main table =============
### ========================================

dt_main_data <- {
  mines <- oxrep_db %>%
  tbl("Mines") %>%
  collect()
}

# set chars to numeric
dt_main_data <- dt_main_data %>%
  mutate(latitude = as.numeric(latitude),
         longitude = as.numeric(longitude),
         notBeforeOpeningDate = as.numeric(notBeforeOpeningDate),
         notBeforeClosingDate = as.numeric(notBeforeClosingDate),
         notAfterOpeningDate = as.numeric(notAfterOpeningDate),
         notAfterClosingDate = as.numeric(notBeforeClosingDate)
         )

total_observations_dt_main_data <- nrow(dt_main_data)

### =========== Display version of tables

## Display version omits IDs
display_main_data <- dt_main_data %>%
  select(-contains("ID")) %>%
  # select(display_main_label_df$data.name) %>%
  collect()

### =========== Replace empty strings with NA
display_main_data[display_main_data == ""] <- NA

#delete the data with missing coordinates 
#what about the locations 0,0?
display_main_data <- display_main_data %>% filter(!is.na(longitude) | !is.na(latitude))
display_main_data <-  display_main_data[display_main_data$latitude != 0 &
                      display_main_data$longitude != 0,]
nr_missed_coord <- total_observations_dt_main_data - nrow(display_main_data)

#options and labels for metals mined
metals <- dt_main_data %>% 
          select(+contains("metalMined")) %>% 
          colnames()
# metals <- gsub("^.*?metalMined","",metals)

#many missing dates on the mines set the NA to -999 to 999
nr_dates_missing <- display_main_data %>% filter(is.na(notBeforeOpeningDate)
                                                | is.na(notAfterOpeningDate)
                                                | is.na(notBeforeClosingDate)
                                                | is.na(notAfterClosingDate)) %>% nrow()

display_main_data[is.na(display_main_data$notBeforeOpeningDate),"notBeforeOpeningDate"] <- -999
display_main_data[is.na(display_main_data$notAfterOpeningDate),"notAfterOpeningDate"] <- -999
display_main_data[is.na(display_main_data$notBeforeClosingDate),"notBeforeClosingDate"] <- 999
display_main_data[is.na(display_main_data$notAfterClosingDate),"notAfterClosingDate"] <- 999

display_tbl_labels <- c("site",
                       "ancientName",
                       "province",
                       "country",
                       "region",
                       "metalMinedGold")
display_tbl_names <- c("Mine site",
                       "Ancient Name",
                       "Province",
                       "Country",
                       "Region",
                       "Gold")

tick_columns <- c("metalMinedGold")
display_main_data <- display_main_data %>%
  mutate_at(vars(one_of(tick_columns)), funs(plyr::mapvalues(
    .,
    from = c("0", "1", ""),
    to = c(FALSE, TRUE, FALSE)
  ))) %>%
  mutate_at(vars(one_of(tick_columns)), funs(as.logical))


