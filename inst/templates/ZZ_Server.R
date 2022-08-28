#'@import rhandsontable
#'@import shiny
#'@importFrom digest digest
#'@importFrom shinyAce aceEditor updateAceEditor


# JMH
# Load state notes:
# - Replace current state with loaded state
# - Change button values to current or zero

#'@export
#'@title ZZDESC State Server
#'@description Server function for the ZZDESC  Shiny Module
#'@param id An ID string that corresponds with the ID used to call the modules UI elements
#'@param yaml_section  Section of the yaml file with the module configuration (\code{"UD"})
#'@param yaml_file Upload Data configuration file
#'@param react_state Variable passed to server to allow reaction outside of module (\code{NULL})
#'@return UD Server object
ZZ_Server <- function(id,
                      id_ASM       = NULL,
                      yaml_file    = system.file(package = "formods",
                                                 "templates",
                                                 "config.yaml"),
                      yaml_section = "ZZ",
                      react_state  = NULL) {
  moduleServer(id, function(input, output, session) {


    #------------------------------------
    # Create ui outputs here:
    output$ZZ_ui_element = renderUI({
      uiele = NULL
      uiele})

    #------------------------------------
    # Generated data reading code
    observe({
      # Reacting to file changes
      input$input_data_file
      input$input_select_sheet
      state = ZZ_fetch_state(id           = id, 
                             input        = input, 
                             session      = session, 
                             yaml_file    = yaml_file, 
                             yaml_section = yaml_section,
                             id_ASM       = id_ASM,
                             react_state  = react_state)

      if(is.null(state[["ZZ"]][["code"]])){
        uiele = "# No file loaded"
      } else {
        uiele = state[["ZZ"]][["code"]]
      }


      shinyAce::updateAceEditor(
        session         = session,
        editorId        = "ZZ_ui_ace_code",
        theme           = state[["yaml"]][["FM"]][["code"]][["theme"]],
        showLineNumbers = state[["yaml"]][["FM"]][["code"]][["showLineNumbers"]],
        readOnly        = state[["MC"]][["code"]][["readOnly"]],
        mode            = state[["MC"]][["code"]][["mode"]],
        value           = uiele)

    })
    #------------------------------------
    # User messages:
    output$ui_asm_msg = renderText({
      input[["button_state_save"]]
      state = ZZ_fetch_state(id           = id, 
                             input        = input, 
                             session      = session, 
                             yaml_file    = yaml_file, 
                             yaml_section = yaml_section,
                             id_ASM       = id_ASM,
                             react_state  = react_state)

      uiele = state[["ZZ"]][["ui_msg"]]

      uiele})
    #------------------------------------
    # Creating reaction if a variable has been specified
   #if(!is.null(react_state)){
   #  # Here we list the ui inputs that will result in a state change:
   #  toListen <- reactive({
   #    list(
   #         input$A # list inputs to react to here
   #        )
   #  })
   #  # This updates the reaction state:
   #  observeEvent(toListen(), {
   #  state = ZZ_fetch_state(id           = id, 
   #                         input        = input, 
   #                         session      = session, 
   #                         yaml_file    = yaml_file, 
   #                         yaml_section = yaml_section,
   #                         id_ASM       = id_ASM,
   #                         react_state  = react_state)
   #
   #    FM_le(state, "reaction state updated")
   #    react_state[[id]] = state
   #  })
   #}
    #------------------------------------
#   # Removing holds
#   remove_hold_listen  <- reactive({
#     list(
#          input$A # List inputs to trigger hold removal
#         )
#   })
#   observeEvent(remove_hold_listen(), {
#     # Once the UI has been regenerated we
#     # remove any holds for this module
#     state = ZZ_fetch_state(id           = id, 
#                            input        = input, 
#                            session      = session, 
#                            yaml_file    = yaml_file, 
#                            yaml_section = yaml_section,
#                            id_ASM       = id_ASM,
#                            react_state  = react_state)
#    
#     FM_le(state, "removing holds")
#     # Removing all holds
#     for(hname in names(state[["ZZ"]][["ui_hold"]])){
#       remove_hold(state, session, hname)
#     }
#   }, priority = -100)
#   #------------------------------------


  })
}

#'@export
#'@title Fetch ZZDESC State
#'@description Merges default app options with the changes made in the UI
#'@param id Shiny module ID
#'@param id_ASM ID string for the app state managment module used to save and load app states
#'@param input Shiny input variable
#'@param session Shiny session variable
#'@param yaml_file cofiguration file
#'@param yaml_section  Section of the yaml file with the module configuration
#'@return list containing the current state of the app including default
#'values from the yaml file as well as any changes made by the user. The list
#'has the following structure:
#' \itemize{
#' \item{yaml:} Full contents of the supplied yaml file.
#' \item{MC:} Module components of the yaml file.
#' \item{ZZ:} 
#' \itemize{
#'   \item{isgood:} Boolean object indicating if the file was successfully loaded.
#'   \item{checksum:} This is an MD5 sum of the contents element and can be
#'   used to detect changes in the state.
#' }
#'  \item{MOD_TYPE:} Character data containing the type of module \code{"ZZ"}
#'  \item{id:} Character data containing the module id module in the session variable.
#'  \item{FM_yaml_file:} App configuration file with FM as main section.
#'  \item{MOD_yaml_file:}  Module configuration file with MC as main section.
#'}
ZZ_fetch_state = function(id, input, session, yaml_file, yaml_section, id_ASM, react_state){

  # Template for an empty dataset
  #---------------------------------------------
  # Getting the current state
  state = FM_fetch_mod_state(session, id)
  # If the state has not yet been defined then we
  # initialize it
  if(is.null(state)){
    # General state information
    state = ZZ_init_state(yaml_file, yaml_section, id)
  }

  #---------------------------------------------
  # Here we update the state based on user input
  for(ui_name in state[["ZZ"]][["ui_ids"]]){
    if(!is.null(isolate(input[[ui_name]]))){
       state[["ZZ"]][["ui"]][[ui_name]] = isolate(input[[ui_name]])
     } else {
       state[["ZZ"]][["ui"]][[ui_name]] = ""
     }
   }
   msgs = c()

  #---------------------------------------------
  # Here we react to changes between the UI and the current state

  #---------------------------------------------
  # Passing any messages back to the user
  state = FM_set_ui_msg(state, msgs)

  #---------------------------------------------
  # Saving the state
  FM_set_mod_state(session, id, state)

  # Returning the state
  state}

#'@export
#'@title Initialize ZZ Module State
#'@description Creates a list of the initialized module state
#'@param yaml_file App configuration file
#'@param yaml_section  Section of the yaml file with the module configuration
#'@param id ID string for the module.
#'@return list containing an empty ZZ state
ZZ_init_state = function(yaml_file, yaml_section, id){

  button_counters = c()
  ui_ids          = c()
  ui_hold         = c()


  state = FM_init_state(
    yaml_file       = yaml_file,
    yaml_section    = yaml_section,
    id              = id,
    MT              = "ZZ",
    button_counters = button_counters,
    ui_ids          = ui_ids,
    ui_hold         = ui_hold)

  FM_le(state, "State initialized")
  state}

