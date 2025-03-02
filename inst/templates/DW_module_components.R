#library(formods)
library(shinydashboard)
#https://fontawesome.com/icons?from=io
ui <- dashboardPage(
  skin="red",
  dashboardHeader(title="Wrangle Data"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Wrangle Data", tabName="DW", icon=icon("hat-cowboy")),
      menuItem("Other", tabName="other", icon=icon("archive"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName="DW",
             fluidRow(
              # Required for tooltips
              prompter::use_prompt(),
              box(title="Data View Actions",
              div(style="display:inline-block",
              "ui_dw_views",
              htmlOutput(NS("DW", "ui_dw_views")),
              "ui_dw_key",
              htmlOutput(NS("DW", "ui_dw_key"))),
              div(style="display:inline-block",
                 "ui_dw_new_view",
                 htmlOutput(NS("DW", "ui_dw_new_view")) ),
              div(style="display:inline-block",
                 "ui_dw_save_view",
                 htmlOutput(NS("DW", "ui_dw_save_view"))),
              div(style="display:inline-block",
                 "ui_dw_clip_code",
                 htmlOutput(NS("DW", "ui_dw_clip_code")) ),
              div(style="display:inline-block",
                 "ui_dw_del_view",
                 htmlOutput(NS("DW", "ui_dw_del_view")) ),
              div(style="display:inline-block",
                 "ui_dw_copy_view",
                 htmlOutput(NS("DW", "ui_dw_copy_view"))),
              width = 12)
               ),
              fluidRow(
                box(title="Add Data Wrangling Element Button",
                    "ui_dw_add_element_button",
                    htmlOutput(NS("DW", "ui_dw_add_element_button"), width=6)),

                box(title="Add Data Wrangling Select",
                    "ui_dw_select",
                    htmlOutput(NS("DW", "ui_dw_select")), width=6)),

              fluidRow(
                box(title="New Element Row",
                    "ui_dw_new_element_row",
                    htmlOutput(NS("DW", "ui_dw_new_element_row")), width=12)),

              fluidRow(
                box(title="Button Click Message",
                    "ui_dw_msg",
                    verbatimTextOutput(NS("DW", "ui_dw_msg")), width=12)),

              fluidRow(
                box(title="Current Elements",
                    "hot_dw_elements",
                    rhandsontable::rHandsontableOutput(NS("DW", "hot_dw_elements")), width=12)),

              fluidRow(
                box(title="Generated Code",
                    "ui_dw_code",
                    shinyAce::aceEditor(NS("DW", "ui_dw_code")), width=12)),

              fluidRow(box(title="Wrangled Data",
                    "hot_data_preview",
                     rhandsontable::rHandsontableOutput(NS("DW", "hot_data_preview")), width=12)),

              fluidRow(
                box(title="Current Module State",
                    verbatimTextOutput( "ui_state"),width=12))
      ),
      tabItem(tabName="other", "Here you can put other elements of your App")
    )
  )
)




# Main app server
server <- function(input, output, session) {

  # Test dataset in the package
  DATA = rio::import(
           file  = system.file(package="formods", "test_data", "TEST_DATA.xlsx"),
           which = "DATA")

  # Module server
  react_FM    = reactiveValues()

  # Creating upstream data for the UD module
  id_UD = "UD"
  res = UD_test_mksession(session)
  react_FM[[id_UD]] = res[["rsc"]][[id_UD]]

  DW_Server(id="DW", react_state=react_FM)

  # Current state outside of the module
  output$ui_state  =  renderText({
    uiele = paste(capture.output(str(react_FM[["DW"]])), collapse="\n")
    uiele})

}

shinyApp(ui, server)
