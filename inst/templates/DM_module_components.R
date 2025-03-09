#library(formods)
library(shinydashboard)
library(prompter)
library(clipr)

#https://fontawesome.com/icons?from=io

ui <- dashboardPage(
  skin="red",
  dashboardHeader(title="DM Module Template"),
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
             "This app demonstrates how to use the DM
             module with each ui component isolated to make
             it easy to see the behavior.",
        fluidRow(
          box(title="Analysis Actions",
              div(style="display:inline-block",
                  "ui_dm_new_btn",
                  htmlOutput(NS("DM", "ui_dm_new_btn"))),
              div(style="display:inline-block",
                  "ui_dm_save_btn",
                  htmlOutput(NS("DM", "ui_dm_save_btn"))),
              div(style="display:inline-block",
                  "ui_dm_clip_code",
                  htmlOutput(NS("DM", "ui_dm_clip_code")) ),
              div(style="display:inline-block",
                  "ui_dm_del_btn",
                  htmlOutput(NS("DM", "ui_dm_del_btn"))),
              div(style="display:inline-block",
                  "ui_dm_copy_btn",
                  htmlOutput(NS("DM", "ui_dm_copy_btn"))),
              width = 12)
        ),
       width=12)),
       fluidRow(
         box(title="Select current element",
           "DM_ui_select_element",
           htmlOutput(NS("DM", "DM_ui_select_element")),
           "DM_ui_text_element_name",
           htmlOutput(NS("DM", "DM_ui_text_element_name")),
         ),
         box(title="Element",
           "DM_ui_element",
           htmlOutput(NS("DM", "DM_ui_element"))
         )
         ),
       fluidRow(
         box(title="Sources",
          "DM_ui_upload",
          htmlOutput(NS("DM", "DM_ui_file_upload")),
          "DM_ui_source_url",
          htmlOutput(NS("DM", "DM_ui_source_url")),
          "DM_ui_button_get_url",
          htmlOutput(NS("DM", "DM_ui_button_get_url")),
          "DM_hot_resources",
          rhandsontable::rHandsontableOutput(NS("DM", "DM_hot_resources")),
         width=12)
         ),
       fluidRow(
         box(title="Dataset Details",
           "DM_ui_source_id",
           htmlOutput(NS("DM", "DM_ui_source_id")),
           "DM_ui_ds_sheet",
           htmlOutput(NS("DM", "DM_ui_ds_sheet")),
         width=6),
         box(title="",
           "DM_ui_clean_ds",     
           htmlOutput(NS("DM", "DM_ui_clean_ds")),
           "DM_ui_res_label",     
           htmlOutput(NS("DM", "DM_ui_res_label")),
           "DM_ui_res_label_val",     
           htmlOutput(NS("DM", "DM_ui_res_label_val")),
         width=6)
         ),
       fluidRow(
         box(title="Messages",
           "ui_dm_msg",
           verbatimTextOutput(NS("DM", "ui_dm_msg")), width=12)),
       fluidRow(
         box(title="Data Source Preview",
           "DM_hot_ds_preview",
          rhandsontable::rHandsontableOutput(NS("DM", "DM_hot_ds_preview")), width=12)),
       fluidRow(
         box(title="Generated Code",
           "ui_dm_code",
           shinyAce::aceEditor(NS("DM", "ui_dm_code")), width=12)),
       fluidRow(
         box(title="Current Module State",
           verbatimTextOutput("ui_state"),width=12))
       )
      )
    )
  )

# Main app server
server <- function(input, output, session) {
  # Empty reactive object to track and react to
  # changes in the module state outside of the module
  react_FM = reactiveValues()

  res = DM_test_mksession(session)

  # Module server
  DM_Server(id="DM", react_state=react_FM)

  # Current state outside of the module
  output$ui_state  =  renderText({
    uiele = paste(capture.output(str(react_FM[["DM"]])), collapse="\n")
  uiele})
}

shinyApp(ui, server)
