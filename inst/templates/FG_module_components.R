#library(formods)
library(shinydashboard)
library(devtools)
load_all()

#https://fontawesome.com/icons?from=io
ui <- dashboardPage(
  skin="red",
  dashboardHeader(title="Generate Figures"),
  dashboardSidebar(
     sidebarMenu(
       menuItem("Generate Figures",     tabName="FG",  icon=icon("chart-line")),
       menuItem("Other",  tabName="other", icon=icon("archive"))
     )
  ),
  dashboardBody(
    tabItems(
     tabItem(tabName="FG",
            #fluidRow(
            #  box(title="Add Data Wrangling Element Button",
            #    htmlOutput(NS("FG", "ui_dw_add_element_button"), width=6)),
            #  box(title="Add Data Wrangling Select",
            #    htmlOutput(NS("FG", "ui_dw_select")), width=6)),
            #fluidRow(
            #  box(title="New Element Row",
            #    htmlOutput(NS("FG", "ui_dw_new_element_row")), width=12)),
            #fluidRow(
            #  box(title="Element Add Message",
            #    verbatimTextOutput(NS("FG", "ui_dw_new_element_msg")), width=12)),
            #fluidRow(
            #  box(title="Current Elements",
            #   rhandsontable::rHandsontableOutput(NS("FG", "hot_dw_elements")), width=12)),
             fluidRow(
               box(title="Generated Code",
                 shinyAce::aceEditor(NS("FG", "ui_fg_code")), width=12)),
           # fluidRow(box(title="Wrangled Data",
           #     rhandsontable::rHandsontableOutput(NS("FG", "hot_data_preview")), width=12)),
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
  session$userData$pkdata = Theoph

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
          contents        = Theoph,
          checksum        = digest::digest(Theoph, algo=c("md5")),
          isgood          = TRUE
        )
    )

  react_FM$UD = ds

  FG_Server(id="FG", id_UD = "UD", id_DW = "DW", react_state=react_FM)

  # Current state outside of the module
  output$ui_state  =  renderText({
    uiele = paste(capture.output(str(react_FM[["FG"]])), collapse="\n")
  uiele})

}

shinyApp(ui, server)
