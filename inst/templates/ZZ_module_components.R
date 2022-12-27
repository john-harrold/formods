#library(formods)
library(shinydashboard)

library(devtools)
load_all()

#https://fontawesome.com/icons?from=io

ui <- dashboardPage(
  skin="red",
  dashboardHeader(title="ZZ Module Template"),
  dashboardSidebar(
     sidebarMenu(
       menuItem("Save/Load",    tabName="appstate",  icon=icon("archive")) ,
       menuItem("Other",  tabName="other", icon=icon("archive"))
     )
  ),
  dashboardBody(
    tabItems(
       tabItem(tabName="appstate",
       fluidRow(
         box(title="Example",
             "This app demonstrates how to use the app state manager
             module with each ui component isolated to make
             it easy to see the behavior.",
           width=12)),
       fluidRow(
         box(title="Save",
           "ASM_ui_save_name",
           htmlOutput(NS("ASM", "ASM_ui_save_name")),
           "ASM_ui_save_input",
           htmlOutput(NS("ASM", "ASM_ui_save_button"))
         ),
         box(title="Load",
           "ASM_ui_load_state",
           htmlOutput(NS("ASM", "ASM_ui_load_state"))
         )
         ),
       fluidRow(
         box(title="Messages",
           "ui_zz_msg",
           verbatimTextOutput(NS("ZZ", "ui_zz_msg")), width=12)),
       fluidRow(
         box(title="Generated Code",
           "ZZ_ui_ace_code",
           shinyAce::aceEditor(NS("ZZ", "ui_zz_code")), width=12)),
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
  ZZ_Server(id="ZZ", react_state=react_FM)

  # Current state outside of the module
  output$ui_state  =  renderText({
    uiele = paste(capture.output(str(react_FM[["ZZ"]])), collapse="\n")
  uiele})
}

shinyApp(ui, server)
