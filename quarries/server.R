library("tidyverse")
library("shiny")
library("DT")
library("pool")
library("shinyBS")
library("leaflet")
library("knitr")
library("htmlTable")
library("dbplyr")
library("RMySQL")
library("stringr")
library("shinyjs")
library("sf")
library("readxl")
library("openxlsx")
library("httr")
library("htmlwidgets")

# load the function of timeline slider 
source("gg_timeline_plot.R", local = TRUE)

# loading and cleaning the data; processing
source("data-processing.R", local = TRUE)

#helping function for display data
filter_data <- function(data,data_filter){
  data <- filter_at(data, vars(data_filter),
                    all_vars(. == TRUE))
  return(data)
}

# disconnect from the database
# lapply(dbListConnections(MySQL()), dbDisconnect)

# source("map_tools.R", local = TRUE)

# Define server logic
shinyServer(function(input, output, session) {

  ################# timeline ################
  # output$timeperiod_main_DT_UI <- renderUI({
  #   sliderInput(
  #     "timeperiod_data",
  #     "Selected time period",
  #     min = min(display_main_data$notBeforeOpeningDate, na.rm = TRUE),
  #     max = max(display_main_data$notAfterClosingDate, na.rm = TRUE),
  #     value = c(
  #       min(display_main_data$notBeforeOpeningDate, na.rm = TRUE),
  #       max(display_main_data$notBeforeClosingDate, na.rm = TRUE)
  #     ),
  #     width = "100%"
  #   )
  # })
  ###########################################
  
  ############### text of total stone mines ########
  output$text_total_nr <- renderUI({
    #filter of stones
    selected_quarries <- input$quarries_mined
    if (!is.null(selected_quarries)) {
      display_main_data <- filter_data(display_main_data,selected_quarries)
    }

    paste0(
      "With your current filters, there are ",
      nrow(display_main_data),
      " observations from a total of ",
      total_observations_data,
      " stone quarries in the database."
    )
    
  })
  ##################################################
  ################## stone types ###################
  output$quarries_bar <- renderUI({
    selectInput(
      "quarries_mined",
      "Quarries to show",
      choices = quarries_choices,
      multiple = TRUE,
      width = "100%"
    )
  })
  ##################################################
  ################### render maps ##################
  output$overview_map <- renderLeaflet({
    locs_sf <- st_as_sf(display_main_data, coords = c("longitude", "latitude"))  
    locs_sf %>% 
      leaflet()
  })
  ##################################################
  ################## main datatable ################
  output$main_DT <- DT::renderDataTable({

    # filter by stones    
    selected_quarries <- input$quarries_mined
    if (!is.null(selected_quarries)) {
      display_main_data <- filter_data(display_main_data,selected_quarries)
    }
    
    display_tbl <- display_main_data %>% 
      select(display_tbl_labels)
    datatable(
      display_tbl,
      rownames = FALSE,
      colnames = display_tbl_names,
      selection = 'none',
      escape = FALSE,
      options = list(
        columnDefs = list(
          list(width = "80px", targets = list(0, 1, 2)),
          list(width = "16px", targets = list(3:18)),
          list(width = "160px",targets = list(19,20))      
        ),
        # columnDefs = list(list(width = "80px", targets = "_all")),
        server = FALSE,
        autoWidth = TRUE,
        scrollX = TRUE,
        # scrollY = 500,
        # scroller = TRUE,
        pageLength = 50,
        dom = 'Bfrtip',
        buttons = c('csv', 'excel'),
        rowCallback = htmlwidgets::JS(
          "function(row, data, rowi) {
          data.forEach(function(d,i) {
          if(typeof(d) === 'boolean') {
          $($('td', row)[i]).html(
          [
          '<center><i class=\\'',
          d ? 'fa fa-circle' : 'fa fa-circle-o',
          '\\'>',
          '</i></center>'
          ].join('')
          )
          }
          })
          }"
        ) # rowcallback
      ) # list
    ) %>%
      formatStyle(1:2, color = "#6d9dc8", cursor = "default")
  }, server = FALSE)
  ##################################################
  
  ################## download table ################
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("OxRep-Quarries", ".xlsx", sep = "")
    },
    content = function(file) {
      
      # filter by stones    
      selected_quarries <- input$quarries_mined
      if (!is.null(selected_quarries)) {
        display_main_data <- filter_data(display_main_data,selected_quarries)
      }
      
      display_main_data <- display_main_data %>% select(display_main_label_df)
      table_labels <- nice_col_headings(display_main_data) %>% 
                      tools::toTitleCase() %>% 
                      trimws()
      
      write.xlsx(display_main_data %>%
                   setNames(table_labels), file)
    }
  )
  
  ##################################################
  
})
