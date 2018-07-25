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

#helping function for display data
filter_data <- function(data,data_filter){
      data <- filter_at(data, vars(data_filter),
                all_vars(. == TRUE))
      return(data)
}

filter_time_data <- function(data,start,end){
  data <- data %>%
    filter(
      notBeforeOpeningDate >= start &
        notAfterClosingDate <= end
    )
  return(data)
}

# load the function of timeline slider 
source("gg_timeline_plot.R", local = TRUE)

# loading and cleaning the data; processing
source("data-processing.R", local = TRUE)

# disconnect from the database
# lapply(dbListConnections(MySQL()), dbDisconnect)

source("map_tools.R", local = TRUE)

# Define server logic
shinyServer(function(input, output, session) {
  
  source("modal_summary_tab.R", local = TRUE)$value
  source("modal_associated_features_tab.R", local = TRUE)$value
  
  output$text_total_nr <- renderUI({

    #time period selection
    if (is.null(input$timeperiod_data)) {
      return()
    } else {
      display_main_data <- filter_time_data(display_main_data,
                                            input$timeperiod_data[1],
                                            input$timeperiod_data[2])
    }

    #filter of metals
    selected_metals <- input$metals_mined
    if (!is.null(selected_metals)) {
      display_main_data <- filter_data(display_main_data,selected_metals)
    } 
    
    paste0(
      "With your current filters, there are ",
      nrow(display_main_data),
      " observations from a total of ",
      total_observations_dt_main_data,
      " mines in the database."
    )
    
  })
    
  output$text_missing_nr <- renderUI({
    paste0("Total number of mines without coordinates: ",
           nr_missed_coord,
           ".")
  })

  output$metals_mined_UI <- renderUI({
    selectInput(
      "metals_mined",
      "Metals to show",
      choices = metals_choices,
      multiple = TRUE,
      width = "100%"
    )
  })

  output$timeperiod_main_DT_UI <- renderUI({
    sliderInput(
      "timeperiod_data",
      "Selected time period",
      min = min(display_main_data$notBeforeOpeningDate, na.rm = TRUE),
      max = max(display_main_data$notAfterClosingDate, na.rm = TRUE),
      value = c(
        min(display_main_data$notBeforeOpeningDate, na.rm = TRUE),
        max(display_main_data$notBeforeClosingDate, na.rm = TRUE)
      ),
      width = "100%"
    )
  })
  
    
  output$overview_map <- renderLeaflet({
    #filter data by the timeline
    if (is.null(input$timeperiod_data)) {
      return()
    } else {
      display_main_data <- filter_time_data(display_main_data,
                                            input$timeperiod_data[1],
                                            input$timeperiod_data[2])
    }
    
    #filter data by metals
    selected_metals <- input$metals_mined
    if (!is.null(selected_metals)) {
      display_main_data <- filter_data(display_main_data,selected_metals)
    }
    
    if (nrow(display_main_data)==0) {
      # shinyjs::show(id = "No data for rendering with current filters.")
      return()
    } else {
    locs_sf <- st_as_sf(display_main_data, coords = c("longitude", "latitude"))  
      locs_sf %>% 
        leaflet() %>%
        addProviderTiles(providers$Esri.WorldShadedRelief) %>%
      addMarkers(
        popup = ~ map_point_labeller(
          site,
          province,
          country,
          region,
          notBeforeOpeningDate,
          notAfterClosingDate
        ),
        icon = makeIcon(
          "icons/mine.png",
          iconWidth = 18,
          iconHeight = 18
        )
      ) %>%
      my_fitBounds(locs_sf %>%
                   st_bbox() %>%
                   as.list())
    }
    })  
  
  ##table
  output$main_DT <- DT::renderDataTable({
    # shinyjs::show(id = "loading-main-table",
    #               anim = TRUE,
    #               animType = "fade")
    
    #filter data by the timeline
    if (is.null(input$timeperiod_data)) {
      return()
    } else {
      display_main_data <- filter_time_data(display_main_data,
                                            input$timeperiod_data[1],
                                            input$timeperiod_data[2])
    }
    
    #filter data by metals
    selected_metals <- input$metals_mined
    if (!is.null(selected_metals)) {
      display_main_data <- filter_data(display_main_data,selected_metals)
    }
    
    display_tbl <- display_main_data %>% 
                   select(display_tbl_labels)
    datatable(
      display_tbl,
      rownames = FALSE,
      colnames = display_tbl_names,
      # extensions = c('Scroller'),
      selection = 'none',
      escape = FALSE,
      options = list(
        columnDefs = list(list(
          width = "80px", targets = list(0, 1, 5)
        )),
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
  
  source("charts.R", local = TRUE)
 
  observeEvent(input$main_DT_cell_clicked,
               {
                 info <- input$main_DT_cell_clicked
                 if (is.null(info$value) || info$col >= 1) {
                   return()
                 } else {
                   selected_row <- dt_main_data[info$row,]
                    toggleModal(session, "selected_row_modal", toggle = "toggle")
                 }
               })
  
  modal_row_data <- eventReactive(input$main_DT_cell_clicked,
                                  {
                                    info <- input$main_DT_cell_clicked
                                    display_main_data <- filter_time_data(display_main_data,
                                                                            input$timeperiod_data[1],
                                                                            input$timeperiod_data[2])
                                    #filter data by metals
                                    selected_metals <- input$metals_mined
                                    if (!is.null(selected_metals)) {
                                      display_main_data <- filter_data(display_main_data,selected_metals)
                                    }
                                    selected_row <- display_main_data[info$row,]
                                    selected_row
                                  })
  
  output$modal_body <- renderUI({
     tabsetPanel(
                 tabPanel("Summary tab",
                          uiOutput("modal_summaryTab")
                          ),
                 tabPanel("Associated Features tab",
                          uiOutput("modal_featureTab")
                          ),
                 tabPanel("Associated Objects tab",
                          print("obj")
                 )
                 )
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
 
})
