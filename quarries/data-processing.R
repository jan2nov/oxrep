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

data_quarries <- {quarries <- oxrep_db %>% tbl("Quarries") %>% collect() }
data_countries <- {countries <- oxrep_db %>% tbl("Countries") %>% collect() } 
data_provinces <- {provinces <- oxrep_db %>% tbl("Provinces") %>% collect() } 
dt_references_data <- {feature <- oxrep_db %>% tbl("References") %>%  collect()}
dt_pub_data <- {feature <- oxrep_db %>% tbl("Publications") %>% collect()}

# load table labels
display_table_labels <- read_csv("data/quarries-table-labels.csv")
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
total_observations_data <- nrow(data_quarries)
##############################################

######### transfer to display_data ###########
display_main_data <- data_quarries %>%
  select(-contains("ID")) %>%
  # select(display_main_label_df$data.name) %>%
  collect()

############### choices for filters ##########
quarries_choices <- display_main_data %>% 
    select("marble", +contains("quarry"), -contains("quarryReferences"))
quarries_choices <- setNames(colnames(quarries_choices),nice_col_headings(quarries_choices))
##############################################
################## repair data ###############
## change to logical values
tick_columns <- quarries_choices
display_main_data <- display_main_data %>%
  mutate_at(vars(one_of(tick_columns)), funs(as.logical))
##############################################

############### other utils ##################
display_main_data$country <- data_countries[match(display_main_data$country,data_countries$countryID),]$name
display_main_data$province <- data_provinces[match(display_main_data$province,data_provinces$provinceID),]$name

display_main_label_df <- display_table_labels %>%
  filter(!is.na(main.table)) %>%
  select(data.name, display.name) %>%
  mutate(display.name = trimws(display.name),
         display.name = gsub("\\s+", " ", display.name))


display_tbl_labels <- c("name",
                        "province",
                        "country",
                        unname(quarries_choices), 
                        "materialNotes")
display_tbl_labels <- display_main_label_df %>% select(data.name) %>% .[[1]]
display_tbl_names <- c("Quarry site",
                       "Province",
                       "Country",
                       names(quarries_choices),
                       "Material Notes")
display_tbl_names <- display_main_label_df %>% select(display.name)  %>% .[[1]]
##############################################

## charts utils
choices_group <- list("Country" = "country", "Province" = "province", "Quarries" = "quarries")
choices_count <- c("Quarries", "Number of Sites" )
choices_stack <- list("Percent" = "percent", "Number of Sources" = "normal")
# choices_count <- c("Metals", "Number of Mines")

### =========== Replace empty strings with NA
display_main_data[display_main_data == ""] <- NA
############### close the database ###########
poolClose(oxrep_db)
##############################################