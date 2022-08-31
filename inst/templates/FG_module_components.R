#library(formods)
library(shinydashboard)
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
                 "ui_fg_curr_figs",
                 htmlOutput(NS("FG", "ui_fg_curr_figs")), width=5),
               box(title="Current Data Views",
                 "ui_fg_curr_views",
                 htmlOutput(NS("FG", "ui_fg_curr_views")), width=5)),
             fluidRow(
              box(title="Figure Actions",
              div(style="display:inline-block",
                "ui_fg_new_fig",
                 htmlOutput(NS("FG", "ui_fg_new_fig"))),
              div(style="display:inline-block",
                "ui_fg_save_fig",
                 htmlOutput(NS("FG", "ui_fg_save_fig"))),
              div(style="display:inline-block",
                 "ui_fg_del_fig",
                 htmlOutput(NS("FG", "ui_fg_del_fig"))),
              div(style="display:inline-block",
                 "ui_fg_copy_fig",
                 htmlOutput(NS("FG", "ui_fg_copy_fig"))),
              width = 12)
               ),
             fluidRow(
               box(title="Figure Caption",
                 "ui_fg_fig_name",
                 htmlOutput(NS("FG", "ui_fg_fig_name")),
                 tags$br(),
                 "ui_fg_fig_notes",
                 htmlOutput(NS("FG", "ui_fg_fig_notes")),
              width=12)),
             fluidRow(
               box(title="Add Plot Element Button",
                 "ui_fg_add_element_button",
                 htmlOutput(NS("FG", "ui_fg_add_element_button")), width=4),
               box(title="Plot Element Select",
                 "ui_fg_select",
                 htmlOutput(NS("FG", "ui_fg_select")), width=4)),
             fluidRow(
               box(title="New Element Row",
                 "ui_fg_new_element_row",
                 htmlOutput(NS("FG", "ui_fg_new_element_row")), width=12)),
             fluidRow(
               box(title="Messages",
                 "ui_fg_msg",
                 verbatimTextOutput(NS("FG", "ui_fg_msg")), width=12)),
             fluidRow(
               box(title="Plot Preview",
                 "ui_fg_preview_ggplot",
                 plotOutput(NS("FG", "ui_fg_preview_ggplot")), width=12)),
             fluidRow(
               box(title="Plotly Preview",
                  "The plotly output is experimental. It doesnt work with all the plotting elements, namly the mulit-page faceting with ggforce, so it's not reccomended for use until these things are worked out.",
                 "ui_fg_preview_plotly",
                 plotly::plotlyOutput(NS("FG", "ui_fg_preview_plotly"), width="1000px", height="600px"), width=12)),
             fluidRow(
               box(title="Multipage Slider",
                 "ui_fg_slider_page",
                 htmlOutput(NS("FG", "ui_fg_slider_page")), width=12)),
             fluidRow(
               box(title="Current Elements",
                 "hot_fg_elements",
                rhandsontable::rHandsontableOutput(NS("FG", "hot_fg_elements")), width=12)),
             fluidRow(
               box(title="Generated Code",
                 "ui_fg_code",
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
           path  = system.file(package="formods", "data", "TEST_DATA.xlsx"),
           sheet = "DATA") %>%
    dplyr::filter(EVID==0)

  # Module server
  react_FM    = reactiveValues()

  # Format of ds is described in JMH
  ds = list(
     UD = list(
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
