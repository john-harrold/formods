#library(formods)
library(shinydashboard)
#https://fontawesome.com/icons?from=io

ui <- dashboardPage(
  skin="red",
  dashboardHeader(title="Upload Data"),
  dashboardSidebar(
     sidebarMenu(
       menuItem("Upload Data",    tabName="dupload",  icon=icon("table")) ,
       menuItem("Other",  tabName="other", icon=icon("archive"))
     )
  ),
  dashboardBody(
    tabItems(
       tabItem(tabName="dupload",
       fluidRow(
         box(title="Example",
             "This app demonstrates how to use the data upload
             module with each ui component isolated to make
             it easy to see the behavior.",
           width=12)),
       fluidRow(
         box(title="Upload",
           "ui_ud_load_data",
           htmlOutput(NS("UD", "ui_ud_load_data")),
           "ui_ud_clean",
           htmlOutput(NS("UD", "ui_ud_clean"))
           ),
         box(title="Select Sheet for Excel Files",
           "ui_ud_select_sheets",
           htmlOutput(NS("UD", "ui_ud_select_sheets")))),
       fluidRow(
         box(title="Load Results",
           "ui_ud_text_load_result",
           htmlOutput(NS("UD", "ui_ud_text_load_result"))),
         box(title="Data Preview",
           "ui_ud_data_preview",
           htmlOutput(NS("UD", "ui_ud_data_preview")))),
       fluidRow(
         box(title="Generated Code",
           "ui_ud_ace_code",
           shinyAce::aceEditor(NS("UD", "ui_ud_ace_code")), width=12)),
       fluidRow(
         box(title="Current Module State",
           verbatimTextOutput("ui_state"),width=12))
       ),
       tabItem(tabName="other", "Here you can put other elements of your App")
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
    uiele = paste(capture.output(str(react_FM[["UD"]])), collapse="\n")
  uiele})
}

shinyApp(ui, server)
