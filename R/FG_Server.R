



#'@export
#'@title Figure Generation Server
#'@description Server function for the figure generation module
#'@param id An ID string that corresponds with the ID used to call the module's UI function
#'@param yaml_section  Section of the yaml file with the module configuration (`"FG"`)
#'@param yaml_file Upload Data cofiguration file
#'@param id_UD  ID string for the upload data module used to handle uploads or the name of the list element in react_state where the data set is stored.
#'@param id_DW  ID string for the data wrangling module to process any uploaded data
#'@param react_state Variable passed to server to allow reaction outside of module (`NULL`)
#'@return return
FG_Server <- function(id,
                yaml_section = "FG",
                yaml_file    = system.file(package = "formods", "templates", "config.yaml"),
                id_UD        = NULL,
                id_DW        = NULL,
                react_state  = NULL) {
  moduleServer(id, function(input, output, session) {


    #------------------------------------
    # Generating the figure generation code
    observe({
      # Forcing a reaction to changes from the upload data module
      react_state[[id_UD]]
      react_state[[id_DW]]

      # Force update on button click
      #input$button_dw_add_element
      # Force update on deletion clicks
      #input$hot_dw_elements

      state = FG_fetch_state(id           = id,
                             input        = input,
                             session      = session,
                             yaml_file    = yaml_file,
                             yaml_section = yaml_section,
                             id_UD        = id_UD,
                             id_DW        = id_DW,
                             react_state  = react_state)

      uiele = NULL

     #if(state[["DW"]][["DS"]][["isgood"]]){
     #  if(is.null(state[["DW"]][["elements_table"]])){
     #    uiele = "# No data wragling elements defined yet!"
     #  } else {
     #    uiele = paste(state[["DW"]][["elements_table"]][["cmd"]], collapse="\n")
     #  }
     #
     #  shinyAce::updateAceEditor(
     #    session         = session,
     #    editorId        = "ui_dw_code",
     #    theme           = state[["yaml"]][["FM"]][["code"]][["theme"]],
     #    showLineNumbers = state[["yaml"]][["FM"]][["code"]][["showLineNumbers"]],
     #    readOnly        = state[["MC"]][["code"]][["readOnly"]],
     #    mode            = state[["MC"]][["code"]][["mode"]],
     #    value           = uiele)
     #}

      })

    #------------------------------------
    # Creates the ui for the compact view of the module
    output$FG_ui_compact  =  renderUI({
      state = FG_fetch_state(id           = id,
                             input        = input,
                             session      = session,
                             yaml_file    = yaml_file,
                             yaml_section = yaml_section,
                             id_UD        = id_UD,
                             id_DW        = id_DW,
                             react_state  = react_state)

      uiele = NULL

    # uiele_main = tagList(
    #   div(style="display:inline-block", htmlOutput(NS(id, "ui_dw_select"))),
    #   div(style="display:inline-block", htmlOutput(NS(id, "ui_dw_add_element_button"))),
    #    htmlOutput(NS(id, "ui_dw_new_element_row")),
    #    verbatimTextOutput(NS(id, "ui_dw_new_element_msg")),
    #    rhandsontable::rHandsontableOutput(NS(id, "hot_dw_elements")))
    # if( state$MC$compact$preview){
    #   uiele_main = tagList(uiele_main, tags$br(),
    #     rhandsontable::rHandsontableOutput(NS(id, "hot_data_preview")))
    # }
    #
    # if( state$MC$compact$code){
    # # uiele_preview = tagList(htmlOutput(NS(id, "UD_ui_data_preview")))
    #   uiele_code = tagList(shinyAce::aceEditor(NS(id, "ui_dw_code")))
    #
    #   uiele_str ="tabPanel(state$MC$labels$tab_main,   uiele_main)"
    #   if(state$MC$compact$code){
    #     uiele_str = paste0(uiele_str, ",tabPanel(state$MC$labels$tab_code, uiele_code)") }
    #
    #   uiele_str = paste0("tabsetPanel(",uiele_str, ")")
    #
    #   uiele = eval(parse(text=uiele_str))
    # } else {
    #   uiele = uiele_main
    # }

    uiele})
    #------------------------------------
    # Creating reaction if a variable has been specified
    if(!is.null(react_state)){
      # Here we list the ui inputs that will result in a state change:
      toListen <- reactive({
        list(input$button_dw_add_element)
      })
      # This updates the reaction state:
      observeEvent(toListen(), {
        react_state[[id]] = FG_fetch_state(
                             id           = id,
                             input        = input,
                             session      = session,
                             yaml_file    = yaml_file,
                             yaml_section = yaml_section,
                             id_UD        = id_UD,
                             id_DW        = id_DW,
                             react_state  = react_state)
      })
    }
  })
}


#'@export
#'@title Fetch Figure Generation State
#'@description Merges default app options with the changes made in the UI
#'@param id Shiny module ID
#'@param input Shiny input variable
#'@param session Shiny session variable
#'@param yaml_file cofiguration file
#'@param yaml_section  Section of the yaml file with the module configuration
#'@param id_UD  ID string for the upload data module used to handle uploads or the name of the list element in react_state where the data set is stored.
#'@param id_DW  ID string for the data wrangling module to process any uploaded data
#'@param react_state Variable passed to server to allow reaction outside of module (`NULL`)
#'@return list containing the current state of the app including default
#'values from the yaml file as well as any changes made by the user
FG_fetch_state = function(id,           input,           session,
                          yaml_file,    yaml_section,    id_UD,      id_DW,
                          react_state){

  # After the app has loaded the state must be initialized
  FM_FG_ID = paste0("FM_FG_", id)

  #---------------------------------------------
  # Getting the current state
  if(is.null(session$userData[[FM_FG_ID]])){
    # General state information
    state = FG_init_state(yaml_file, yaml_section)
  } else {
    # If it's not null we just pluck the state
    # from the session variable
    state = session$userData[[FM_FG_ID]]
  }

 ##---------------------------------------------
 ## Here we update the state based on user input
 ## Dataset changes
 #if(is.null(state[["DW"]][["DS"]])){
 #  if(!is.null(react_state)){
 #    # This contains the input dataset:
 #    state[["DW"]][["DS"]] = isolate(react_state[[id_UD]][["DS"]])
 #
 #    # This contains the output dataset which when initialized will
 #    # be just the input above:
 #    state = set_wds(state, state[["DW"]][["DS"]][["contents"]])
 #  } else {
 #    state[["DW"]][["DS"]][["isgood"]] = FALSE
 #    state[["DW"]][["isgood"]]         = FALSE
 #  }
 #}

 ##---------------------------------------------
 ## Getting the current ui elements into the state
 #ui_elements = list(
 #  "hot_dw_elements"            = "update_elements",
 #  "button_dw_add_element"      = "add_element",
 #  "select_fds_filter_column"   = "filter",
 #  "select_fds_filter_operator" = "filter",
 #  "fds_filter_rhs"             = "filter",
 #  "select_fds_mutate_column"   = "mutate",
 #  "select_fds_mutate_rhs"      = "mutate",
 #  "select_fds_rename_column"   = "rename",
 #  "fds_rename_rhs"             = "rename",
 #  "select_fds_group_column"    = "group",
 #  "select_dw_element"          = "element_type"
 #)
 #for(ui_name in names(ui_elements)){
 #  if(!is.null(isolate(input[[ui_name]]))){
 #    state[["FG"]][["ui"]][[ui_name]] = isolate(input[[ui_name]])
 #  } else {
 #    state[["FG"]][["ui"]][[ui_name]] = ""
 #  }
 #}
  # Saving the state
  session$userData[[FM_FG_ID]] = state

  # Returning the state

state}


#'@export
#'@title Initialize FG Module State
#'@description Creates a list of the initialized module state
#'@param yaml_file App cofiguration file
#'@param yaml_section  Section of the yaml file with the module configuration
#'@return list containing an empty app state object
FG_init_state = function(yaml_file, yaml_section){
  state = list()
  # Reading in default information from the yaml file
  state[["yaml"]] = yaml::read_yaml(yaml_file)

  # This assigns the module config "MC" element to the correct yaml_section.
  state[["MC"]] = state[["yaml"]][[yaml_section]]


  # Defaults for the module
  FG_NULL =
    list(isgood           = TRUE,
         add_counter      = 0          # counter tracking the ds_add_element button
         )

  state[["FG"]] = FG_NULL

state}
