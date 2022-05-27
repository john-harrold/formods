#library(formods)

library(devtools)
load_all()

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
           htmlOutput(NS("UD", "UD_ui_load_data"))),
         box(title="Select Sheet for Excel Files",
           htmlOutput(NS("UD", "UD_ui_select_sheets")))),
       fluidRow(
         box(title="Load Results",
           htmlOutput(NS("UD", "UD_ui_text_load_result"))),
         box(title="Data Preview",
           htmlOutput(NS("UD", "UD_ui_data_preview")))),
       fluidRow(
         box(title="Generated Code",
           #htmlOutput(NS("UD", "UD_ui_code")), width=12)),
           shinyAce::aceEditor(NS("UD", "UD_ui_ace_code")), width=12)),
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
