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
library("highcharter")

# load the function of timeline slider 
source("gg_timeline_plot.R", local = TRUE)

# loading and cleaning the data; processing
source("data-processing.R", local = TRUE)

#helping function for display data
filter_data <- function(data,data_filter){
  data <- filter(data, type %in% data_filter)
  return(data)
}
filter_time_data <- function(data,start,end){
  data <- data %>%
    filter(
      notBeforeWrittenDate >= start &
        notAfterWrittenDate <= end
    )
  return(data)
}

# Define server logic
shinyServer(function(input, output, session) {
  
  source("modal_summary_tab.R", local = TRUE)$value
  
  ################# timeline ################
  output$timeperiod_main_DT_UI <- renderUI({
    sliderInput(
      "timeperiod_data",
      "Selected time period",
      min = min(display_main_data$notBeforeWrittenDate, na.rm = TRUE),
      max = max(display_main_data$notAfterWrittenDate, na.rm = TRUE),
      value = c(
        min(display_main_data$notBeforeWrittenDate, na.rm = TRUE),
        max(display_main_data$notAfterWrittenDate, na.rm = TRUE)
      ),
      width = "100%"
    )
  })
  ###########################################
  
  ############### text of total ########
  output$text_total_nr <- renderUI({
    # filter by type
    selected_filter_bar <- input$filterbar
    if (!is.null(selected_filter_bar)) {
      display_main_data <- filter_data(display_main_data,selected_filter_bar)
    }
    
    #time period selection
    if (is.null(input$timeperiod_data)) {
      return()
    } else {
      display_main_data <- filter_time_data(display_main_data,
                                            input$timeperiod_data[1],
                                            input$timeperiod_data[2])
    }
    
    paste0(
      "With your current filters, there are ",
      nrow(display_main_data),
      " observations from a total of ",
      total_observations_data,
      " Water Tech documents in the database."
    )
    
  })
  ##################################################
  ################## filter bar# ###################
  output$filter_bar <- renderUI({
    selectInput(
      "filterbar",
      "Type: ",
      choices = choices_filter,
      multiple = TRUE,
      width = "100%"
    )
  })
  ##################################################
  
  ################## main datatable ################
  output$main_DT <- DT::renderDataTable({

    # filter by type
    selected_filter_bar <- input$filterbar
    if (!is.null(selected_filter_bar)) {
      display_main_data <- filter_data(display_main_data,selected_filter_bar)
    }
    
    #time period selection
    if (is.null(input$timeperiod_data)) {
      return()
    } else {
      display_main_data <- filter_time_data(display_main_data,
                                            input$timeperiod_data[1],
                                            input$timeperiod_data[2])
    }
    
    display_tbl <- display_main_data %>% 
      select(display_main_label_df$data.name)
    
    datatable(
      display_tbl,
      rownames = FALSE,
      colnames = display_main_label_df$display.name,
      selection = 'none',
      escape = FALSE,
      options = list(
        # columnDefs = list(
        #   list(width = "80px", targets = list(0, 1, 2))
        # ),
        # columnDefs = list(list(width = "80px", targets = "_all")),
        server = FALSE,
        autoWidth = TRUE,
        scrollX = TRUE,
        # scrollY = 500,
        # scroller = TRUE,
        pageLength = 50,
        dom = 'Bfrtip',
        buttons = c('csv', 'excel')
      ) # list
    ) 
  }, server = FALSE)
  ##################################################
  
  ################## download table ################
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("OxRep-waterTech", ".xlsx", sep = "")
    },
    content = function(file) {
      
      # filter by type
      selected_filter_bar <- input$filterbar
      if (!is.null(selected_filter_bar)) {
        display_main_data <- filter_data(display_main_data,selected_filter_bar)
      }
      
      #time period selection
      if (is.null(input$timeperiod_data)) {
        return()
      } else {
        display_main_data <- filter_time_data(display_main_data,
                                              input$timeperiod_data[1],
                                              input$timeperiod_data[2])
      }

      display_main_data <- display_main_data %>% select(display_main_label_df$data.name)
      table_labels <- nice_col_headings(display_main_data) %>% 
                      tools::toTitleCase() %>% 
                      trimws()
      
      write.xlsx(display_main_data %>%
                   setNames(table_labels), file)
    }
  )
  
  ##################################################
  
  # charts plots and data processing
  source("charts.R", local = TRUE)
  
})
