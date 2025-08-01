if(interactive()){
# These are suggested packages
library(shinydashboard)
library(ggpubr)
library(plotly)
library(shinybusy)
library(prompter)
library(utils)
library(clipr)
library(formods)

CSS <- "
.wrapfig {
  float: right;
  shape-margin: 20px;
  margin-right: 20px;
  margin-bottom: 20px;
}
"

formods.yaml  = system.file(package="formods",  "templates",  "formods.yaml")
ASM.yaml      = system.file(package="formods",  "templates",  "ASM.yaml")
UD.yaml       = system.file(package="formods",  "templates",  "UD.yaml")
DW.yaml       = system.file(package="formods",  "templates",  "DW.yaml")
DM.yaml       = system.file(package="formods",  "templates",  "DM.yaml")
FG.yaml       = system.file(package="formods",  "templates",  "FG.yaml")


# Name of  file to indicate we need to load testing data
ftmptest = file.path(tempdir(), "formods.test")


# Default to not deployed
if(!exists("deployed")){
  deployed = FALSE
}

#https://fontawesome.com/icons?from=io
data_url =
"https://github.com/john-harrold/formods/raw/master/inst/test_data/TEST_DATA.xlsx"

ui <- dashboardPage(
  skin="black",
  dashboardHeader(title="formods"),
  dashboardSidebar(
     sidebarMenu(
       menuItem("Source Data",     tabName="upload",      icon=icon("table")) ,
       menuItem("Manage Data",     tabName="manage_data", icon=icon("folder-tree")),
       menuItem("Workflows",       tabName="workflows",   icon=icon("robot")),
       menuItem("Wrangle",         tabName="wrangle",     icon=icon("hat-cowboy")),
       menuItem("Plot",            tabName="plot",        icon=icon("chart-line")),
       menuItem("App State",       tabName="app_state",   icon=icon("archive")),
       menuItem("App Info",        tabName="sysinfo",     icon=icon("book-medical"))
     )
  ),
  dashboardBody(
  tags$head(
    tags$style(HTML(CSS))
  ),
    tabItems(
       tabItem(tabName="app_state",
                 box(title="Manage App State",
                     htmlOutput(NS("ASM", "ui_asm_compact")))),
       tabItem(tabName="upload",
               box(title="Load Data", width=12,
                 fluidRow(
                   prompter::use_prompt(),
                   column(width=6,
                     htmlOutput(NS("UD",  "UD_ui_compact"))),
                   column(width=6,
       tags$p(
           tags$img(
           class = "wrapfig",
           src   = "https://github.com/john-harrold/formods/raw/master/man/figures/logo.png",
           width = 100,
           alt = "formods logo" ),
         'Formods is a set of modules and an framework for developing modules
         which interact and create code to replicate analyses performed within an app.
         To experiment download this',
       tags$a("test dataset", href=data_url),
              'and upload it into the App using the form on the left.'))
                 )
               )
               ),
       tabItem(tabName="workflows",
                 box(title="Run Workflows",
                     htmlOutput(NS("ASM", "ASM_ui_workflows")))),
       tabItem(tabName="wrangle",
               box(title="Transform and Create Views of Your Data", width=12,
               htmlOutput(NS("DW",  "DW_ui_compact")))),
       tabItem(tabName="manage_data",
               box(title="Manage and Create Data Sources", width=12,
               htmlOutput(NS("DM",  "DM_ui_compact")))),
       tabItem(tabName="plot",
               box(title="Visualize Data", width=12,
               htmlOutput(NS("FG",  "FG_ui_compact")))),
       tabItem(tabName="sysinfo",
               box(title="System Details", width=12,
               shinydashboard::tabBox(
                 width = 12,
                 title = NULL,
                 shiny::tabPanel(id="sys_modules",
                          title=tagList(shiny::icon("ghost"),
                                        "Modules"),
                 htmlOutput(NS("ASM", "ui_asm_sys_modules"))
                 ),
                 shiny::tabPanel(id="sys_packages",
                          title=tagList(shiny::icon("ghost"),
                                        "Packages"),
                 htmlOutput(NS("ASM", "ui_asm_sys_packages"))
                 ),
                 shiny::tabPanel(id="sys_log",
                          title=tagList(shiny::icon("clipboard-list"),
                                        "App Log"),
                 verbatimTextOutput(NS("ASM", "ui_asm_sys_log"))
                 ),
                 shiny::tabPanel(id="sys_options",
                          title=tagList(shiny::icon("sliders"),
                                        "R Options"),
                 htmlOutput(NS("ASM", "ui_asm_sys_options"))
                 )
                 )
               ))
      )
    )
  )

# Main app server
server <- function(input, output, session) {
  # Empty reactive object to track and react to
  # changes in the module state outside of the module
  react_FM = reactiveValues()

 #react_FM[["UD"]]   = list()
 #react_FM[["FG"]]   = list()
 #react_FM[["DW"]]   = list()
 #react_FM[["DM"]]   = list()
 #react_FM[["ASM"]]  = list()

  #Uncommenet to populate with test data
  # If the ftmptest file is present we load test data
  if(file.exists(ftmptest)){
    sources = c(system.file(package="formods", "preload", "ASM_preload.yaml"),
                system.file(package="formods", "preload", "UD_preload.yaml"),
                system.file(package="formods", "preload", "DM_preload.yaml"),
                system.file(package="formods", "preload", "DW_preload.yaml"),
                system.file(package="formods", "preload", "FG_preload.yaml"))
    res = FM_app_preload(session=session, sources=sources, react_state = react_FM)
  } else if(file.exists("preload.yaml")){
    
    shinybusy::show_modal_spinner(text="Preloading analysis, be patient", session=session)
    res = FM_app_preload(session=session, sources="preload.yaml", react_state=react_FM)
    shinybusy::remove_modal_spinner(session = session)
  }


  # This is the list of module ids used for reproducible script generation. The
  # order here is important.
  mod_ids = c("UD", "DM", "DW", "FG")


  # Module servers
  formods::ASM_Server(id="ASM",
             FM_yaml_file  = formods.yaml,
             MOD_yaml_file = ASM.yaml,
             deployed      = deployed,
             react_state   = react_FM,
             mod_ids       = mod_ids)
  formods::UD_Server( id="UD", 
             FM_yaml_file  = formods.yaml,
             MOD_yaml_file = UD.yaml,
             deployed      = deployed,
             react_state   = react_FM)
  formods::DW_Server( id="DW",
             FM_yaml_file  = formods.yaml,
             MOD_yaml_file = DW.yaml,
             deployed      = deployed,
             react_state   = react_FM)
  formods::DM_Server( id="DM", 
             FM_yaml_file  = formods.yaml,
             MOD_yaml_file = DM.yaml,
             deployed      = deployed,
             react_state   = react_FM)
  formods::FG_Server( id="FG", 
             FM_yaml_file  = formods.yaml,
             MOD_yaml_file = FG.yaml,
             deployed      = deployed,
             react_state   = react_FM)
}

shinyApp(ui, server)
}
