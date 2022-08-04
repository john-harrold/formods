#library(formods)

library(devtools)
library(shinydashboard)
load_all()

#https://fontawesome.com/icons?from=io

ui <- dashboardPage(
  skin="black",
  dashboardHeader(title="formods"),
  dashboardSidebar(
     sidebarMenu(
       menuItem("App State",       tabName="app_state",   icon=icon("archive")) ,
       menuItem("Source Data",     tabName="upload",      icon=icon("table")) ,
       menuItem("Wrangle",         tabName="wrangle",     icon=icon("hat-cowboy")),
       menuItem("Plot",            tabName="plot",        icon=icon("chart-line"))
     )
  ),
  dashboardBody(
    tabItems(
       tabItem(tabName="app_state",

               fluidRow(
                 column(width=4,
                   htmlOutput(NS("ASM", "ASM_ui_compact"))),
                 column(width=4,
                     tags$p("Formods is a set of modules and an framework for developing modules which interact and create code to replicate analyses performed within an app.")),
                 column(width=2,
                     tags$figure(
                       align = "right",
                       tags$img(
                         src="https://r.ubiquity.tools/reference/figures/logo.png",
                         width = 100,
                         alt = "formods logo"
                       )
                     ) #,
                   #rep("text ", times=1000)
                 )
               )
               ),
       tabItem(tabName="upload",      htmlOutput(NS("UD",  "UD_ui_compact"))),
       tabItem(tabName="wrangle",     htmlOutput(NS("DW",  "DW_ui_compact"))),
       tabItem(tabName="plot",        htmlOutput(NS("FG",  "FG_ui_compact")))
      )
    )
  )

# Main app server
server <- function(input, output, session) {
  # Empty reactive object to track and react to
  # changes in the module state outside of the module
  react_FM = reactiveValues()

  #FM_load_test_state(session=session, react_state=react_FM, input=input)

  # Module servers
  ASM_Server(id="ASM",                                           react_state=react_FM)
  UD_Server( id="UD", id_ASM = "ASM",                            react_state=react_FM)
  DW_Server( id="DW", id_ASM = "ASM",id_UD = "UD",               react_state=react_FM)
  FG_Server( id="FG", id_ASM = "ASM",id_UD = "UD", id_DW = "DW", react_state=react_FM)
}

shinyApp(ui, server)
