#library(formods)
library(shinydashboard)

#https://fontawesome.com/icons?from=io

ui <- dashboardPage(
  skin="red",
  dashboardHeader(title="===ZZ=== Module Template"),
  dashboardSidebar(
     sidebarMenu(
       menuItem("Module UI Components",    tabName="appstate",  icon=icon("archive")),
       menuItem("Compact View",  tabName="compact", icon=icon("archive"))
     )
  ),
  dashboardBody(
    tabItems(
       tabItem(tabName="appstate",
       # Required for tooltips
       prompter::use_prompt(),
       fluidRow(
         box(title=NULL,
             "This app demonstrates how to use the ===ZZ===
             module with each ui component isolated to make
             it easy to see the behavior.",
        fluidRow(
          box(title="Analysis Actions",
              div(style="display:inline-block",
                  "ui_===zz===_new_btn",
                  htmlOutput(NS("===ZZ===", "ui_===zz===_new_btn"))),
              div(style="display:inline-block",
                  "ui_===zz===_save_btn",
                  htmlOutput(NS("===ZZ===", "ui_===zz===_save_btn"))),
              div(style="display:inline-block",
                  "ui_===zz===_clip_code",
                  htmlOutput(NS("===ZZ===", "ui_===zz===_clip_code")) ),
              div(style="display:inline-block",
                  "ui_===zz===_del_btn",
                  htmlOutput(NS("===ZZ===", "ui_===zz===_del_btn"))),
              div(style="display:inline-block",
                  "ui_===zz===_copy_btn",
                  htmlOutput(NS("===ZZ===", "ui_===zz===_copy_btn"))),
              width = 12)
        ),
       width=12)),
       fluidRow(
         box(title="Select current element",
           "===ZZ===_ui_select_element",
           htmlOutput(NS("===ZZ===", "===ZZ===_ui_select_element")),
           "===ZZ===_ui_text_element_name",
           htmlOutput(NS("===ZZ===", "===ZZ===_ui_text_element_name")),
         ),
         box(title="Element",
           "===ZZ===_ui_element",
           htmlOutput(NS("===ZZ===", "===ZZ===_ui_element"))
         )
         ),
       fluidRow(
         box(title="Messages",
           "ui_===zz===_msg",
           verbatimTextOutput(NS("===ZZ===", "ui_===zz===_msg")), width=12)),
       fluidRow(
         box(title="Generated Code",
           "ui_===zz===_code",
           shinyAce::aceEditor(NS("===ZZ===", "ui_===zz===_code")), width=12)),
       fluidRow(
         box(title="Current Module State",
           verbatimTextOutput("ui_state"),width=12))
       ),
       tabItem(tabName="compact",
          "===ZZ===_ui_compact",
          htmlOutput(NS("===ZZ===", "===ZZ===_ui_compact")))
      )
    )
  )

# Main app server
server <- function(input, output, session) {
  # Empty reactive object to track and react to
  # changes in the module state outside of the module
  react_FM = reactiveValues()

  # Module server
  ===ZZ===_Server(id="===ZZ===", id_ASM = "ASM", react_state=react_FM)

  # Current state outside of the module
  output$ui_state  =  renderText({
    uiele = paste(capture.output(str(react_FM[["===ZZ==="]])), collapse="\n")
  uiele})
}

shinyApp(ui, server)
