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
  data <- filter_at(data, vars(data_filter),
                    all_vars(. == TRUE))
  return(data)
}
filter_time_data <- function(data,start,end){
  data <- data %>%
    filter(
      notBeforeConstructionDate >= start &
        notAfterConstructionDate <= end
    )
  return(data)
}
# disconnect from the database
# lapply(dbListConnections(MySQL()), dbDisconnect)


source("map_tools.R", local = TRUE)

# Define server logic
shinyServer(function(input, output, session) {
  
  source("modal_summary_tab.R", local = TRUE)$value
  
  ################# timeline ################
  output$timeperiod_main_DT_UI <- renderUI({
    sliderInput(
      "timeperiod_data",
      "Selected time period",
      min = min(display_main_data$notBeforeConstructionDate, na.rm = TRUE),
      max = max(display_main_data$notAfterConstructionDate, na.rm = TRUE),
      value = c(
        min(display_main_data$notBeforeConstructionDate, na.rm = TRUE),
        max(display_main_data$notAfterConstructionDate, na.rm = TRUE)
      ),
      width = "100%"
    )
  })
  ###########################################
  
  ############### text of total stone mines ########
  output$text_total_nr <- renderUI({
    #time period selection
    if (is.null(input$timeperiod_data)) {
      return()
    } else {
      display_main_data <- filter_time_data(display_main_data,
                                            input$timeperiod_data[1],
                                            input$timeperiod_data[2])
    }
    # filter by press type    
    selected_presses <- input$press_type
    if (!is.null(selected_presses)) {
      display_main_data <- filter_data(display_main_data,selected_presses)
    }
    
    paste0(
      "With your current filters, there are ",
      nrow(display_main_data),
      " observations from a total of ",
      total_observations_data,
      " Oil & Wine presses in the database."
    )
    
  })
  ##################################################
  ################## stone types ###################
  output$presstype_bar <- renderUI({
    selectInput(
      "press_type",
      "Press type: ",
      choices = choices_press_type,
      multiple = TRUE,
      width = "100%"
    )
  })
  ##################################################
  ################### render maps ##################
  output$map_markers <- renderUI({
    selectInput("map_marker" , "Plot Markers", choices = c("Circles", "Cluster"), selected = "Circles")
  })
  
  output$map_view <- renderLeaflet({
    #time period selection
    if (is.null(input$timeperiod_data)) {
      return()
    } else {
      display_main_data <- filter_time_data(display_main_data,
                                            input$timeperiod_data[1],
                                            input$timeperiod_data[2])
    }
    # filter by press type    
    selected_presses <- input$press_type
    if (!is.null(selected_presses)) {
      display_main_data <- filter_data(display_main_data,selected_presses)
    }
    
    locs_sf <- st_as_sf(display_main_data, coords = c("longitude", "latitude"))  
    map <- locs_sf %>% 
      leaflet() %>%
      addProviderTiles(providers$Esri.WorldShadedRelief) %>%
      addTiles()
      
      switch(input$map_marker, 
        "Cluster" = { map %>% addMarkers(clusterOptions = markerClusterOptions(zoomToBoundsOnClick = TRUE),
                        popup = ~ map_point_labeller(
                        site,
                        province,
                        country
                      )
                    ) %>%
                      my_fitBounds(locs_sf %>%
                                     st_bbox() %>%
                                     as.list())
          },
        "Circles" = { map %>% addCircleMarkers(
                      popup = ~ map_point_labeller(site, province, country),
                      radius = 7,
                      fillOpacity = 0.7,
                      stroke = FALSE,
                      color = "#756bb1",
                      weight = 2
                    )
          }
      )
  })
  ##################################################
  
  ################## main datatable ################
  output$main_DT <- DT::renderDataTable({

    #time period selection
    if (is.null(input$timeperiod_data)) {
      return()
    } else {
      display_main_data <- filter_time_data(display_main_data,
                                            input$timeperiod_data[1],
                                            input$timeperiod_data[2])
    }
    # filter by press type    
    selected_presses <- input$press_type
    if (!is.null(selected_presses)) {
      display_main_data <- filter_data(display_main_data,selected_presses)
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
      paste("OxRep-OilWine", ".xlsx", sep = "")
    },
    content = function(file) {
      
      #time period selection
      if (is.null(input$timeperiod_data)) {
        return()
      } else {
        display_main_data <- filter_time_data(display_main_data,
                                              input$timeperiod_data[1],
                                              input$timeperiod_data[2])
      }
      # filter by press type    
      selected_presses <- input$press_type
      if (!is.null(selected_presses)) {
        display_main_data <- filter_data(display_main_data,selected_presses)
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
  ################ modal window ####################
  observeEvent(input$main_DT_cell_clicked,
               {
                 info <- input$main_DT_cell_clicked
                 if (is.null(info$value) || info$col >= 2) {
                   return()
                 } else {
                   selected_row <- data_presses[info$row,]
                   toggleModal(session, "selected_row_modal", toggle = "toggle")
                 }
               })
  
  output$the_modal_call <- renderUI({
    modal_row_data <- modal_row_data()
    
    bsModal(
      "selected_row_modal",
      modal_row_data$site,
      trigger = "main_DT_cell_clicked",
      size = "large",
      uiOutput("modal_body")
    )
  })
  
  modal_row_data <- eventReactive(input$main_DT_cell_clicked,
                                  {
                                    info <- input$main_DT_cell_clicked

                                    #time period selection
                                    if (is.null(input$timeperiod_data)) {
                                      return()
                                    } else {
                                      display_main_data <- filter_time_data(display_main_data,
                                                                            input$timeperiod_data[1],
                                                                            input$timeperiod_data[2])
                                    }
                                    # filter by press type    
                                    selected_presses <- input$press_type
                                    if (!is.null(selected_presses)) {
                                      display_main_data <- filter_data(display_main_data,selected_presses)
                                    }
                                    selected_row <- display_main_data[info$row,]
                                    selected_row
                                  })
  
  output$modal_body <- renderUI({
    tabsetPanel(
      tabPanel("Summary tab",
               uiOutput("modal_summaryTab")
      )
    )
  })
  ##################################################
  # charts plots and data processing
  source("charts.R", local = TRUE)
  
})
