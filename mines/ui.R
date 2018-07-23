library("DT")
library("shinyBS")
library("leaflet")
library("shinyjs")

appCSS <- "
#loading-content {
position: absolute;
background: #FFFFFF;
opacity: 0.9;
z-index: 100;
left: 0;
right: 0;
height: 100%;
text-align: center;
color: #000000;
}
"


# load.fontawesome()
# Define UI for application that draws a histogram
shinyUI(
  
   fluidPage(
     inlineCSS(appCSS),
     theme = "animate.min.css",
     useShinyjs(),
     includeScript('www/fontawesome.js'),
    #for the circles in the table
    uiOutput("timeperiod_main_DT_UI"),
    p(),
    uiOutput("text_total_nr"),
    p(),
    uiOutput("text_missing_nr"),
    p(),
    uiOutput("metals_mined_UI"),
    fluidRow(
      column(width = 1),
      column(leafletOutput("overview_map"), width = 10),
      column(width = 1)
    ),
    p(),
    fluidPage(
      # div(DT::dataTableOutput("main_DT", width = "100%"), style = "font-size: 90%"),
      DT::dataTableOutput("main_DT", width = "100%")
    #   bsModal(
    #     "selected_row_modal",
    #     "Shipwreck Info",
    #     trigger = "main_DT_cell_clicked",
    #     size = "large",
    #     uiOutput("modal_body")
    #   ),
    #   uiOutput("the_modal_call")
    ),
    p()
  )
)
