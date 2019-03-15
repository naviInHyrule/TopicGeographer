
ui <-  fluidPage(
  titlePanel("Topics in Space"),
  fluidRow(
    column(3,
           wellPanel(
             htmlOutput("select_area"),
             htmlOutput("select_topic"),
             plotOutput("topicProdsPlot",width = "100%", height = "800px"),
             width = 4
           )),
    column(9,
           fluidRow(
             column(6,
                    tags$style(type="text/css",
                               ".shiny-output-error { visibility: hidden; }",
                               ".shiny-output-error:before { visibility: hidden; }"),
                    leafletOutput("within_map",width = "600px", height = "800px")),
             column(6,
                    leafletOutput("across_map",width = "600", height = "800px")))
    ))
  #tableOutput("neighbours")
  #verbatimTextOutput("shiny_nn")
)