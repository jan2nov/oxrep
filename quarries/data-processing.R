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

display_main_label_df <- display_table_labels %>%
  filter(!is.na(main.table)) %>%
  select(data.name, display.name) %>%
  mutate(display.name = trimws(display.name),
         display.name = gsub("\\s+", " ", display.name))


############### load main data ###############
data_quarries <- read_csv("data/Quarries.csv")
data_countries <- read_csv("data/Countries.csv")
data_provinces <- read_csv("data/Provinces.csv")

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
    select("marble", +contains("quarry"))
quarries_choices <- setNames(colnames(quarries_choices),nice_col_headings(quarries_choices))
##############################################

display_main_data$country <- data_countries[match(display_main_data$country,data_countries$countryID),]$name
display_main_data$province <- data_provinces[match(display_main_data$province,data_provinces$provinceID),]$name

display_tbl_labels <- c("name",
                        "province",
                        "country",
                        unname(quarries_choices), 
                        "materialNotes")
display_tbl_names <- c("Quarry site",
                       "Province",
                       "Country",
                       names(quarries_choices),
                       "Material Notes")
