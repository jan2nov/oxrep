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

# disconnect from the database
# lapply(dbListConnections(MySQL()), dbDisconnect)

# source("map_tools.R", local = TRUE)

# Define server logic
shinyServer(function(input, output, session) {
  
  source("modal_summary_tab.R", local = TRUE)$value
  
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
  output$map_markers <- renderUI({
    selectInput("map_marker" , "Plot Markers", choices = c("Circles", "Cluster"), selected = "Circles")
  })
  
  output$map_view <- renderLeaflet({
    
    map_point_labeller <-
      function(quarrySite = NA,
               province = NA,
               country = NA) {
        paste0(
          # "<p>", Name, "</p>",
          "<p><b>Quarry Site:</b> ",
          quarrySite,
          "</p>",
          "<p><b>Province:</b> ",
          province,
          "</p>",
          "<p><b>Country:</b> ",
          country,
          "</p>"
        )
      }
    
    my_fitBounds <- function(map, bbox) {
      fitBounds(map, bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax)
    }
    
    # filter by stones    
    selected_quarries <- input$quarries_mined
    if (!is.null(selected_quarries)) {
      display_main_data <- filter_data(display_main_data,selected_quarries)
    }
    
    locs_sf <- st_as_sf(display_main_data, coords = c("longitude", "latitude"))  
    map <- locs_sf %>% 
      leaflet() %>%
      addProviderTiles(providers$Esri.WorldShadedRelief) %>%
      addTiles()
      
      switch(input$map_marker, 
        "Cluster" = { map %>% addMarkers(clusterOptions = markerClusterOptions(zoomToBoundsOnClick = TRUE),
                        popup = ~ map_point_labeller(
                        name,
                        province,
                        country
                      )
                    ) %>%
                      my_fitBounds(locs_sf %>%
                                     st_bbox() %>%
                                     as.list())
          },
        "Circles" = { map %>% addCircleMarkers(
                      popup = ~ map_point_labeller(name, province, country),
                      radius = 7,
                      fillOpacity = 0.5,
                      stroke = FALSE,
                      color = "#de2d26",
                      weight = 2
                    )
          }
      )
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
          list(width = "200px",targets = list(19))      
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
  ################ modal window ####################
  observeEvent(input$main_DT_cell_clicked,
               {
                 info <- input$main_DT_cell_clicked
                 if (is.null(info$value) || info$col >= 1) {
                   return()
                 } else {
                   selected_row <- data_quarries[info$row,]
                   toggleModal(session, "selected_row_modal", toggle = "toggle")
                 }
               })
  
  output$the_modal_call <- renderUI({
    modal_row_data <- modal_row_data()
    
    bsModal(
      "selected_row_modal",
      modal_row_data$name,
      trigger = "main_DT_cell_clicked",
      size = "large",
      uiOutput("modal_body")
    )
  })
  
  modal_row_data <- eventReactive(input$main_DT_cell_clicked,
                                  {
                                    info <- input$main_DT_cell_clicked

                                    # filter by stones    
                                    selected_quarries <- input$quarries_mined
                                    if (!is.null(selected_quarries)) {
                                      display_main_data <- filter_data(display_main_data,selected_quarries)
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
