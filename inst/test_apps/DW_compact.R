if(interactive()){
library(shiny)
library(formods)
library(shinydashboard)
#https://fontawesome.com/icons?from=io

ui <- dashboardPage(
  skin="red",
  dashboardHeader(title="Data Wrangling"),
  dashboardSidebar(
     sidebarMenu(
       menuItem("Data Wrangling",    tabName="main",  icon=icon("table"))
     )
  ),
  dashboardBody(
    tabItems(
       tabItem(tabName="main",
        fluidRow(
          htmlOutput(NS("DW",  "DW_ui_compact"))),
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

  # Creating upstream data for the UD module
  id_UD = "UD"
  res = UD_test_mksession(session, id_UD)
  react_FM[[id_UD]] = res[["rsc"]][[id_UD]]

  # Module server
  DW_Server(id="DW", id_UD = id_UD, react_state=react_FM)

  # Current state outside of the module
  output$ui_state  =  renderText({
    input$input_data_file
    uiele = paste(capture.output(str(react_FM[["DW"]])), collapse="\n")
  uiele})
}

shinyApp(ui, server)
}
