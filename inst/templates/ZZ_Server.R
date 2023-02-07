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
#'@param FM_yaml_file App configuration file with FM as main section.
#'@param MOD_yaml_file  Module configuration file with ZZ as main section.
#'@param react_state Variable passed to server to allow reaction outside of module (\code{NULL})
#'@return UD Server object
ZZ_Server <- function(id,
                      id_ASM       = NULL,
                      FM_yaml_file  = system.file(package = "formods", "templates", "formods.yaml"),
                      MOD_yaml_file = system.file(package = "ZZDESC",  "templates", "ZZ.yaml"),
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
      state = ZZ_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_UD           = id_UD,
                             react_state     = react_state)

      if(is.null(state[["ZZ"]][["code"]])){
        uiele = "# No file loaded"
      } else {
        uiele = state[["ZZ"]][["code"]]
      }


      shinyAce::updateAceEditor(
        session         = session,
        editorId        = "ui_zz_code",
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
      state = ZZ_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_UD           = id_UD,
                             react_state     = react_state)

      uiele = state[["ZZ"]][["ui_msg"]]

      uiele})
    #------------------------------------
    # This can be used to trigger notifications
   #toNotify <- reactive({
   #  list(input$A,
   #       input$B)
   #})
   #observeEvent(toNotify(), {
   #  state = ZZ_fetch_state(id              = id,
   #                         input           = input,
   #                         session         = session,
   #                         FM_yaml_file    = FM_yaml_file,
   #                         MOD_yaml_file   = MOD_yaml_file,
   #                         id_UD           = id_UD,
   #                         react_state     = react_state)
   #
   #  # Triggering optional notifications
   #  notify_res =
   #  FM_notify(state   = state,
   #            session = session)
   #})
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
   #  state = ZZ_fetch_state(id              = id,
   #                         input           = input,
   #                         session         = session,
   #                         FM_yaml_file    = FM_yaml_file,
   #                         MOD_yaml_file   = MOD_yaml_file,
   #                         id_UD           = id_UD,
   #                         react_state     = react_state)
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
#     state = ZZ_fetch_state(id              = id,
#                            input           = input,
#                            session         = session,
#                            FM_yaml_file    = FM_yaml_file,
#                            MOD_yaml_file   = MOD_yaml_file,
#                            id_UD           = id_UD,
#                            react_state     = react_state)
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
#'@param input Shiny input variable
#'@param session Shiny session variable
#'@param FM_yaml_file App configuration file with FM as main section.
#'@param MOD_yaml_file  Module configuration file with MC as main section.
#'@param id_ASM ID string for the app state management module used to save and load app states
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
ZZ_fetch_state = function(id, input, session, FM_yaml_file, MOD_yaml_file, id_ASM, react_state){

  # Template for an empty dataset
  #---------------------------------------------
  # Getting the current state
  state = FM_fetch_mod_state(session, id)
  # If the state has not yet been defined then we
  # initialize it
  if(is.null(state)){
    # General state information
    state = ZZ_init_state(FM_yaml_file, MOD_yaml_file, id, session)

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
#'@param FM_yaml_file App configuration file with FM as main section.
#'@param MOD_yaml_file  Module configuration file with MC as main section.
#'@param id ID string for the module.
#'@param session Shiny session variable
#'@return list containing an empty ZZ state
ZZ_init_state = function(FM_yaml_file, MOD_yaml_file,  id, session){


  button_counters = c()
  ui_ids          = c()
  ui_hold         = c()

  state = FM_init_state(
    FM_yaml_file    = FM_yaml_file,
    MOD_yaml_file   = MOD_yaml_file,
    id              = id,
    MT              = "ZZ",
    button_counters = button_counters,
    ui_ids          = ui_ids,
    ui_hold         = ui_hold,
    session         = session)

  FM_le(state, "State initialized")
state}

#'@export
#'@title Fetch Module Code
#'@description Fetches the code to generate results seen in the app
#'@param state ZZ state from \code{ZZ_fetch_state()}
#'@return Character object vector with the lines of code
ZZ_fetch_code = function(state){

  code = NULL

code}

#'@export
#'@title Append Report Elements
#'@description Description
#'@param state ZZ state from \code{ZZ_fetch_state()}
#'@param rpt Report with the current content of the report which will be appended to in
#'this function. For details on the structure see the documentation for \code{\link{formods::FM_generate_report}}.
#'@param rpttype Type of report to generate (supported "xlsx", "pptx", "docx").
#'@param gen_code_only Boolean value indicating that only code should be
#'generated (\code{FALSE}).
#'@return list containing the following elements
#'\itemize{
#'  \item{isgood:}    Return status of the function.
#'  \item{hasrptele:} Boolean indicator if the module has any reportable elements.
#'  \item{code:}      Code to generate reporting elements. 
#'  \item{msgs:}      Messages to be passed back to the user.
#'  \item{rpt:}       Report with any additions passed back to the user.
#'}
#'@seealso \code{\link{formods::FM_generate_report}}
ZZ_append_report = function(state, rpt, rpttype, gen_code_only=FALSE){

  isgood    = TRUE
  hasrptele = FALSE
  code      = c()
  msgs      = c()


  # The ZZ module only supports the following report types:
  supported_rpttypes = c("xlsx", "pptx", "docx")

  if(rpttype %in% supported_rpttypes){
  }

  res = list(
    isgood    = isgood,
    hasrptele = hasrptele,
    code      = code,
    msgs      = msgs,
    rpt       = rpt
  )

res}

#'@export
#'@title Fetch Module Datasets
#'@description Fetches the datasets contained in the module. 
#'@param state ZZ state from \code{ZZ_fetch_state()}
#'@return Character object vector with the lines of code
#'@return list containing the following elements
#'\itemize{
#'  \item{isgood:}    Return status of the function.
#'  \item{hasds:}     Boolean indicator if the module has any datasets
#'  \item{msgs:}      Messages to be passed back to the user.
#'  \item{ds:}        List with datasets. Each list element has the name of
#'  the R-object for that dataset. Each element has the following structure:
#'  \itemize{
#'    \item{label: Text label for the dataset}
#'    \item{MOD_TYPE: Short name for the type of module.}
#'    \item{id: module ID}
#'    \item{DS: Dataframe containing the actual dataset.}
#'    \item{DSMETA: Metadata describing DS}
#'    \item{code: Complete code to build dataset.}
#'    \item{checksum: Module checksum.}
#'    \item{DSchecksum: Dataset checksum.}
#'  }
#'}
ZZ_fetch_ds = function(state){
  hasds  = FALSE
  isgood = TRUE
  msgs   = c()
  ds     = list()

  # Empty list for new datasets
  NEWDS = list(label      = NULL,
               MOD_TYPE   = NULL,
               id         = NULL,
               DS         = NULL,
               DSMETA     = NULL,
               code       = NULL,
               checksum   = NULL,
               DSchecksum = NULL)

  # This prevents returning a dataset if this is triggered before data has
  # been loaded
  if(state[["ZZ"]][["isgood"]]){

    # Fill in the DS creation stuff here
    isgood = FALSE

    # Putting it all into the ds object to be returned
    ds[[object_name]] = NEWDS
  }

  res = list(hasds  = hasds,
             isgood = isgood,
             msgs   = msgs,
             ds     = ds)
res}
