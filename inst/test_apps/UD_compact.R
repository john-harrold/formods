if(interactive()){
library(shiny)
library(formods)
library(shinydashboard)
#https://fontawesome.com/icons?from=io

ui <- dashboardPage(
  skin="red",
  dashboardHeader(title="Upload Data"),
  dashboardSidebar(
     sidebarMenu(
       menuItem("Upload Data",    tabName="dupload",  icon=icon("table"))
     )
  ),
  dashboardBody(
    tabItems(
       tabItem(tabName="dupload",
        fluidRow(
          htmlOutput(NS("UD",  "ui_ud_compact"))),
        fluidRow(tags$br()),
        fluidRow(
          verbatimTextOutput("ui_state"),width=12)
       )
     )
   )
 )

# Main app server
server <- function(input, output, session) {
  # Empty reactive object to track and react to
  # changes in the module state outside of the module
  react_FM = reactiveValues()

  # Module server
  UD_Server(id="UD", react_state=react_FM)

  # Current state outside of the module
  output$ui_state  =  renderText({
    input$input_data_file
    uiele = paste(capture.output(str(react_FM[["UD"]])), collapse="\n")
  uiele})
}

shinyApp(ui, server)
}
