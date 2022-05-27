#library(formods)

library(devtools)
load_all()

#https://fontawesome.com/icons?from=io

ui <- dashboardPage(
  skin="red",
  dashboardHeader(title="formods"),
  dashboardSidebar(
     sidebarMenu(
       menuItem("Upload",     tabName="dupload",  icon=icon("table")) ,
       menuItem("Wrangle",    tabName="wrangle",  icon=icon("hat-cowboy")),
       menuItem("Plot",       tabName="plot",     icon=icon("chart-line"))
     )
  ),
  dashboardBody(
    tabItems(
       tabItem(tabName="dupload", htmlOutput(NS("UD", "UD_ui_compact"))),
       tabItem(tabName="wrangle", htmlOutput(NS("DW", "DW_ui_compact"))),
       tabItem(tabName="plot",    "plot")
      )
    )
  )


# Main app server
server <- function(input, output, session) {
  # Empty reactive object to track and react to
  # changes in the module state outside of the module
  react_FM = reactiveValues()

  # Module servers
  UD_Server(id="UD",               react_state=react_FM)
  DW_Server(id="DW", id_UD = "UD", react_state=react_FM)
}

shinyApp(ui, server)
