### =========== Connect to database =============
### =============================================
oxrep_db <- dbPool(
  drv = RMySQL::MySQL(),
  # drv = RMariaDB::MariaDB(),
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
metals_name <- c("Gold", "Silver", "Lead", "Copper", "Tin", "Iron", "Mercury/Cinnabar", "Zinc", "Other")
# metals <- gsub("^.*?metalMined","",metals)
metals_choices <- setNames(metals, metals_name)
technique <- dt_main_data %>% 
  select(+contains("technique")) %>% 
  colnames()
technique_name <- c("Opencast", "Underground", "Hydraulic", "Hushing", "Ground Sluicing", "Ruina Montium", "Rake Comp", "Gold Washing", "Other")
tehcnique_choices <- setNames(technique,technique_name)

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
                       metals)
display_tbl_names <- c("Mine site",
                       "Ancient Name",
                       "Province",
                       "Country",
                       "Region",
                       metals_name)

## change tto logical values
tick_columns <- metals
display_main_data <- display_main_data %>%
  mutate_at(vars(one_of(tick_columns)), funs(as.logical))
tick_columns <- technique
display_main_data <- display_main_data %>%
  mutate_at(vars(one_of(tick_columns)), funs(as.logical))


## charts utils
choices_group <- list("Country" = "country", "Province" = "province", "Region" = "region", "Metals" = "metals")
choices_count <- c("Metals", "Mining Techniques", "Number of Mines")
# choices_count <- c("Metals", "Number of Mines")
choices_stack <- list("Percent" = "percent", "Number of Sources" = "normal")
list_ancientnames <- unique(display_main_data$ancientName)
list_country <- unique(display_main_data$country)
list_region <- unique(display_main_data$region)
list_district <- unique(display_main_data$miningDistrict)

# close db conection
poolClose(oxrep_db)
