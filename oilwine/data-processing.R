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

data_presses <- {presses <- oxrep_db %>% tbl("PressLoc") %>% collect() }
data_structure <- {structure <- oxrep_db %>% tbl("PressStructures") %>% collect() } 

# load table labels
display_table_labels <- read_csv("data/presses-table-labels.csv")
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
total_observations_data <- nrow(data_presses)
##############################################

######### transfer to display_data ###########
display_main_data <- data_presses %>%
  select(-contains("ID")) %>%
  # select(display_main_label_df$data.name) %>%
  collect()

############### choices for filters ##########
choices_press_type <- display_main_data %>%   select(+contains("countOf")) %>% colnames()
press_type_labels <- display_table_labels[] %>% filter(data.name %in% choices_press_type) %>% 
                      select(display.name) %>% as.list()  %>% unlist() %>% trimws()
choices_press_type <- setNames(choices_press_type,press_type_labels)
##############################################

############### other utils ##################
display_main_label_df <- display_table_labels %>%
  filter(!is.na(main.table)) %>%
  select(data.name, display.name) %>%
  mutate(display.name = trimws(display.name),
         display.name = gsub("\\s+", " ", display.name))

##############################################

## charts utils
choices_group <- list("Country" = "country", "Province" = "province", "Type of Press" = "press")
choices_count <- c("Type of Press", "Number of Presses" )
choices_stack <- list("Percent" = "percent", "Number of Presses" = "normal")
# choices_count <- c("Metals", "Number of Mines")

### =========== Replace empty strings with NA
display_main_data[display_main_data == ""] <- NA
############### close the database ###########
poolClose(oxrep_db)
##############################################