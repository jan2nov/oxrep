library("DT")
library("shinyBS")
library("leaflet")
library("shinyjs")
library("highcharter")

appCSS <- "
.leaflet-top { z-index: 999;}
.sorting_disabled {text-align: center;}
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
     includeScript('www/fontawesome.js'),
     #for the modal window size
     tags$head(tags$style(HTML(
       '.modal-lg {width: 85%;}'
     ))),
    # uiOutput("timeperiod_main_DT_UI")
    # p(),
    uiOutput("text_total_nr"),
    p(),
    fluidRow(
      column(uiOutput("quarries_bar"), width = 8),
      column(uiOutput("map_markers"), width = 4)
    ),
    p(),
    downloadButton("downloadData", "Download Table"),
    p(),
    fluidRow(
      column(width = 1),
      column(leafletOutput("map_view"), width = 10),
      column(width = 1)
    ),
    p(),
    tabsetPanel(
      tabPanel("Table of Mines",
        DT::dataTableOutput("main_DT"),
        uiOutput("the_modal_call")
      ),
      tabPanel("Summary Charts",
               fluidRow(
                 column(uiOutput("groupby"), width = 4),
                 column(uiOutput("countby"), width = 4),
                 column(uiOutput("stackby"), width = 4)
               ),
               fluidRow(
                 column(width=1),
                 column(highchartOutput("chart",height = "600px"),width=10),
                 column(width=1)
               )
      )
    )
  )
)

