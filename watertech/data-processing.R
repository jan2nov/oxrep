###### utility functions ####
nice_col_headings <- function(data){
  plyr::mapvalues(
    colnames(data),
    from = display_table_labels$data.name,
    to = display_table_labels$display.name,
    warn_missing = FALSE
  ) %>%
  tools::toTitleCase() %>%
  trimws()
}
#############################

############### load main data ###############
secure_database_details <- read_csv("data/secure_details.csv", locale = locale(encoding = "UTF-8"))
username_oxrep_db <- "local.username"

oxrep_db <- dbPool(
  drv = RMariaDB::MariaDB(),
  dbname = "oxrep",
  port = 3306,
  host = secure_database_details %>%
    filter(property == "host") %>%
    select(value) %>%
    .[[1]],
  user = secure_database_details %>%
    filter(property == username_oxrep_db) %>%
    select(value) %>%
    .[[1]],
  password = secure_database_details %>%
    filter(property == "local.password") %>% 
    select(value) %>%
    .[[1]]
)

data_water <- {water <- oxrep_db %>% tbl("WaterTechDocs") %>% collect() }
data_tech <- {tech <- oxrep_db %>% tbl("WaterTechWords") %>% collect() }
data_tech$IDnumber <- 1:nrow(data_tech)

# load table labels
display_table_labels <- read_csv("data/watertech-table-labels.csv")
colnames(display_table_labels) <- tolower(make.names(colnames(display_table_labels)))

display_table_labels <- display_table_labels %>%
    mutate(display.name = ifelse(is.na(display.name), data.name , display.name)) %>%
      mutate(
        display.name = gsub('([[:upper:]])', ' \\1', display.name),
        display.name = gsub(
          "(^|[[:space:]])([[:alpha:]])",
          "\\1\\U\\2",
          display.name,
          perl = TRUE
        ))
##############################################

######### basic info of the database #########
total_observations_data <- nrow(data_tech)
##############################################

######### transfer to display_data ###########
display_main_data <- merge(data_tech,data_water,by.x = "documentID",by.y = "waterTechDocID",all.x = TRUE)
display_main_data <- display_main_data[order(display_main_data$IDnumber),] %>%
   select(-contains("ID")) %>%
   collect()
############### choices for filters ##########
choices_filter <- unique(display_main_data$type)
##############################################

############### other utils ##################
display_main_label_df <- display_table_labels %>%
  filter(!is.na(main.table)) %>%
  select(data.name, display.name) %>%
  mutate(display.name = trimws(display.name),
         display.name = gsub("\\s+", " ", display.name))
##############################################

## charts utils
choices_group <- list("Site" = "siteName", "Area" = "egyptianDistrict")
# choices_count <- c("Type of Press", "Number of Presses" )
choices_stack <- list("Percent" = "percent", "Number of Devices" = "normal")

### =========== Replace empty strings with NA
display_main_data[display_main_data == ""] <- NA
display_main_data[is.na(display_main_data$notBeforeWrittenDate),"notBeforeWrittenDate"] <- 0
display_main_data[is.na(display_main_data$notAfterWrittenDate),"notAfterWrittenDate"] <- 0
############### na dates #####################

##############################################
############### close the database ###########
poolClose(oxrep_db)
##############################################