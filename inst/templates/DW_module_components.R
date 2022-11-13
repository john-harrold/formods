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
  DATA = readxl::read_excel(
           path  = system.file(package="formods", "test_data", "TEST_DATA.xlsx"),
           sheet = "DATA")

  # Module server
  react_FM    = reactiveValues()

  # Format of ds is described in the help for UD_fetch_state
  ds = list(
     UD = list(
          data_file_local = NULL,
          data_file_ext   = NULL,
          data_file       = NULL,
          sheet           = NULL,
          sheets          = NULL,
          code            = "# Raw data loading code goes here",
          object_name     = "TMPDS",
          contents        = DATA,
          checksum        = digest::digest(DATA, algo=c("md5")),
          isgood          = TRUE
        )
    )

  react_FM$UD = ds

  DW_Server(id="DW", id_UD = "UD", react_state=react_FM)

  # Current state outside of the module
  output$ui_state  =  renderText({
    uiele = paste(capture.output(str(react_FM[["DW"]])), collapse="\n")
    uiele})

}

shinyApp(ui, server)
