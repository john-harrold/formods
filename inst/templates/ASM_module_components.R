library(shiny)
library(shinydashboard)
#https://fontawesome.com/icons?from=io

#library(formods)
library(devtools)
load_all()

ui <- dashboardPage(
  skin="red",
  dashboardHeader(title="App State Manager"),
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
           "ui_asm_save_name",
           htmlOutput(NS("ASM", "ui_asm_save_name")),
           "ui_asm_switch_gen_rpts",
           htmlOutput(NS("ASM", "ui_asm_switch_gen_rpts")),
           "ui_asm_save_input",
           htmlOutput(NS("ASM", "ui_asm_save_button"))
         ),
         box(title="Load",
           "ui_asm_load_state",
           htmlOutput(NS("ASM", "ui_asm_load_state"))
         )
         ),
       fluidRow(
         box(title="Reporting",
           div(style="display:inline-block",
           "ui_asm_rpt_xlsx",
           htmlOutput(NS("ASM", "ui_asm_rpt_xlsx"))) ,
           div(style="display:inline-block",
           "ui_asm_rpt_pptx",
           htmlOutput(NS("ASM", "ui_asm_rpt_pptx"))) ,
           div(style="display:inline-block",
           "ui_asm_rpt_docx",
           htmlOutput(NS("ASM", "ui_asm_rpt_docx"))) ,
           width=12)),
       fluidRow(
         box(title="Messages",
           "ui_asm_msg",
           verbatimTextOutput(NS("ASM", "ui_asm_msg")), width=12)),
     # JMH delete this too?
     # fluidRow(
     #   box(title="Generated Code",
     #     "ui_asm_ace_code",
     #     shinyAce::aceEditor(NS("ASM", "ui_asm_ace_code")), width=12)),
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
  ASM_Server(id="ASM", react_state=react_FM)

  # Current state outside of the module
  output$ui_state  =  renderText({
    uiele = paste(capture.output(str(react_FM[["ASM"]])), collapse="\n")
  uiele})
}

shinyApp(ui, server)
