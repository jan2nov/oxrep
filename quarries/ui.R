library("DT")
library("shinyBS")
library("leaflet")
library("shinyjs")
library("highcharter")

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
     #for the circles in the table
     # includeScript('www/fontawesome.js'),
     #for the modal window size
     tags$head(tags$style(HTML(
       '.modal-lg {width: 85%;}'
     ))),
    # uiOutput("timeperiod_main_DT_UI")
    # p(),
    uiOutput("text_total_nr"),
    p(),
    uiOutput("quarries_bar"),
    downloadButton("downloadData", "Download Table"),
    p(),
    fluidRow(
      column(width = 1),
      column(leafletOutput("overview_map"), width = 10),
      column(width = 1)
    ),
    DT::dataTableOutput("main_DT")
  )
)

