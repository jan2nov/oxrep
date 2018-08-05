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
     includeScript('www/fontawesome.js'),
     #for the modal window size
     tags$head(tags$style(HTML(
       '.modal-lg {width: 85%;}'
     ))),
    uiOutput("timeperiod_main_DT_UI"),
    p(),
    uiOutput("text_total_nr"),
    p(),
    uiOutput("taxtype_bar"),
    p(),
    div(id = "loading-main-table",
        fluidPage(
          h2(class = "animated infinite pulse", "Loading database...")
          # HTML("<img src=images/cruk-logo.png width='50%'></img>")
        )),
    downloadButton("downloadData", "Download Table"),
    p(),
    tabsetPanel(
      tabPanel("Summary Table",
        DT::dataTableOutput("main_DT"),
        uiOutput("the_modal_call")
      ),
      tabPanel("Summary Charts",
               fluidRow(
                 # column(uiOutput("groupby"), width = 4),
                 # column(uiOutput("countby"), width = 4),
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

