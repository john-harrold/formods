#library(formods)
library(shinydashboard)

library(devtools)
load_all()

#https://fontawesome.com/icons?from=io
ui <- dashboardPage(
  skin="red",
  dashboardHeader(title="Figure Generation"),
  dashboardSidebar(
     sidebarMenu(
       menuItem("Figure Generation", tabName="FG",  icon=icon("chart-line")),
       menuItem("Other",  tabName="other", icon=icon("archive"))
     )
  ),
  dashboardBody(
    tabItems(
     tabItem(tabName="FG",
             fluidRow(
               box(title="Current Figures",
                 htmlOutput(NS("FG", "ui_fg_curr_figs")), width=5)),
             fluidRow(
              box(title="Figure Actions",
              div(style="display:inline-block",
                 htmlOutput(NS("FG", "ui_fg_new_fig")) ),
              div(style="display:inline-block",
                 htmlOutput(NS("FG", "ui_fg_save_fig"))),
              div(style="display:inline-block",
                 htmlOutput(NS("FG", "ui_fg_del_fig")) ),
              div(style="display:inline-block",
                 htmlOutput(NS("FG", "ui_fg_copy_fig"))),
              div(style="display:inline-block",
                 htmlOutput(NS("FG", "ui_fg_upds_fig")) ),
              width = 12)
               ),
             fluidRow(
               box(title="Figure Caption",
                 htmlOutput(NS("FG", "ui_fg_fig_name")),
                 htmlOutput(NS("FG", "ui_fg_fig_cap")),
              width=12)),
             fluidRow(
               box(title="Add Plot Element Button",
                 htmlOutput(NS("FG", "ui_fg_add_element_button")), width=4),
               box(title="Plot Element Select",
                 htmlOutput(NS("FG", "ui_fg_select")), width=4)),
             fluidRow(
               box(title="New Element Row",
                 htmlOutput(NS("FG", "ui_fg_new_element_row")), width=12)),
             fluidRow(
               box(title="Button Message",
                 verbatimTextOutput(NS("FG", "ui_fg_button_click_msg")), width=12)),
             fluidRow(
               box(title="Plot Preview",
                 plotOutput(NS("FG", "ui_fg_preview_ggplot")), width=12)),
             fluidRow(
               box(title="Plotly Preview",
                 plotly::plotlyOutput(NS("FG", "ui_fg_preview_plotly")), width=12)),
             fluidRow(
               box(title="Multipage Slider",
                 htmlOutput(NS("FG", "ui_fg_slider_page")), width=12)),
             fluidRow(
               box(title="Current Elements",
                rhandsontable::rHandsontableOutput(NS("FG", "hot_fg_elements")), width=12)),
             fluidRow(
               box(title="Generated Code",
                 shinyAce::aceEditor(NS("FG", "ui_fg_code")), width=12)),
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
           sheet = "DATA") %>%
    dplyr::filter(EVID==0)

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
          object_name     = "TMPDS",
          code            = "# NULL",
          contents        = DATA,
          checksum        = digest::digest(DATA, algo=c("md5")),
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
