#library(formods)
library(shinydashboard)

library(devtools)
load_all()

#https://fontawesome.com/icons?from=io
ui <- dashboardPage(
  skin="red",
  dashboardHeader(title="Wrangle Data"),
  dashboardSidebar(
     sidebarMenu(
       menuItem("Wrangle Data",     tabName="DW",  icon=icon("hat-cowboy")),
       menuItem("Other",  tabName="other", icon=icon("archive"))
     )
  ),
  dashboardBody(
    tabItems(
     tabItem(tabName="DW",
             fluidRow(
               box(title="Add Data Wrangling Element Button",
                 htmlOutput(NS("DW", "ui_dw_add_element_button"), width=6)),
               box(title="Add Data Wrangling Select",
                 htmlOutput(NS("DW", "ui_dw_select")), width=6)),
             fluidRow(
               box(title="New Element Row",
                 htmlOutput(NS("DW", "ui_dw_new_element_row")), width=12)),
             fluidRow(
               box(title="Element Add Message",
                 verbatimTextOutput(NS("DW", "ui_dw_new_element_msg")), width=12)),
             fluidRow(
               box(title="Current Elements",
                rhandsontable::rHandsontableOutput(NS("DW", "hot_dw_elements")), width=12)),
             fluidRow(
               box(title="Generated Code",
                 shinyAce::aceEditor(NS("DW", "ui_dw_code")), width=12)),
             fluidRow(box(title="Wrangled Data",
                 rhandsontable::rHandsontableOutput(NS("DW", "hot_data_preview")), width=12)),
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

  # Test dataset in the package
  DATA = readxl::read_excel(
           path  = system.file(package="formods", "data", "PK_DATA.xlsx"),
           sheet = "DATA")

  # Module server
  react_FM    = reactiveValues()

  # Format of ds is described in JMH
  ds = list(
     DS = list(
          data_file_local = NULL,
          data_file_ext   = NULL,
          data_file       = NULL,
          sheet           = NULL,
          sheets          = NULL,
          code            = "# NULL",
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
