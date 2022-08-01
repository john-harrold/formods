#'@import rhandsontable
#'@import shiny
#'@importFrom digest digest
#'@importFrom magrittr "%>%"
#'@importFrom shinyAce aceEditor updateAceEditor


# JMH
# Load state notes:
# - Replace current state with loaded state
# - Change button values to current or zero

#'@export
#'@title Save State Server
#'@description Server function for the Save State Shiny Module
#'@param id An ID string that corresponds with the ID used to call the modules UI elements
#'@param FM_yaml_file App configuration file with FM as main section.
#'@param MOD_yaml_file  Module configuration file with MC as main section.
#'@param react_state Variable passed to server to allow reaction outside of module (\code{NULL})
#'@return UD Server object
ASM_Server <- function(id,
                      FM_yaml_file  = system.file(package = "formods",
                                                  "templates",
                                                  "formods.yaml"),
                      MOD_yaml_file = system.file(package = "formods",
                                                  "templates",
                                                  "ASM.yaml"),
                      react_state  = NULL) {
  moduleServer(id, function(input, output, session) {

    #------------------------------------
    # Create ui outputs here:
    output$ASM_ui_save_name  = renderUI({
      state = ASM_fetch_state(id           = id, 
                              input        = input, 
                              session      = session, 
                              FM_yaml_file = FM_yaml_file,
                              MOD_yaml_file = MOD_yaml_file)

      uiele =
        textInput(
          inputId     = NS(id, "ui_asm_save_name"),
          label       = state[["MC"]][["labels"]][["ui_asm_save_name"]],
          width       = state[["MC"]][["formatting"]][["ui_asm_save_name"]][["width"]],
          placeholder = state[["MC"]][["labels"]][["ui_asm_save_name_ph"]]
        )
      uiele})
    #------------------------------------
    output$ASM_ui_save_button  = renderUI({
      state = ASM_fetch_state(id           = id, 
                              input        = input, 
                              session      = session, 
                              FM_yaml_file = FM_yaml_file,
                              MOD_yaml_file = MOD_yaml_file)

        uiele = downloadBttn(
                  outputId = NS(id, "button_state_save"),
                  label    = state[["MC"]][["labels"]][["save_state"]],
                  style    = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
                  size     = state[["MC"]][["formatting"]][["button_state_save"]][["size"]],
                  block    = state[["MC"]][["formatting"]][["button_state_save"]][["block"]],
                  color    = "primary",
                  icon     = icon("arrow-down"))
      uiele})
    #------------------------------------
    output$ASM_ui_compact  =  renderUI({
      uiele = tagList(
           htmlOutput(NS("ASM", "ASM_ui_save_name")),
           htmlOutput(NS("ASM", "ASM_ui_save_button")),
           tags$br(),
           tags$br(),
           htmlOutput(NS("ASM", "ASM_ui_load_state"))
      ) 

      uiele})
    #------------------------------------
    # Download State
    output$button_state_save   = downloadHandler(

      filename = function() {
        state = ASM_fetch_state(id           = id, 
                                input        = input, 
                                session      = session, 
                                FM_yaml_file = FM_yaml_file,
                                MOD_yaml_file = MOD_yaml_file)
        dlfn = ASM_fetch_dlfn(state)
        FM_le(state, paste0("pushing app state download: ", dlfn))
        dlfn},
      content = function(file) {
        state = ASM_fetch_state(id           = id, 
                                input        = input, 
                                session      = session, 
                                FM_yaml_file = FM_yaml_file,
                                MOD_yaml_file = MOD_yaml_file)
        ASM_write_state(state, session, file)
        }
    )
    #------------------------------------
    # Upload State
    output$ASM_ui_load_state = renderUI({
      state = ASM_fetch_state(id           = id, 
                              input        = input, 
                              session      = session, 
                              FM_yaml_file = FM_yaml_file,
                              MOD_yaml_file = MOD_yaml_file)

      uiele = fileInput(NS(id, "input_load_state"),
        label       = state[["MC"]][["labels"]][["input_load_state"]],
        buttonLabel = state[["MC"]][["labels"]][["upload_button"]],
        placeholder = state[["MC"]][["labels"]][["upload_placeholder"]],
        width       = state[["MC"]][["formatting"]][["input_load_state"]][["width"]],
        multiple    = FALSE)

      uiele})
    #------------------------------------
    # User messages:
    output$ui_asm_msg = renderText({
      input[["button_state_save"]]
      input[["input_load_state"]]
      state = ASM_fetch_state(id           = id, 
                              input        = input, 
                              session      = session, 
                              FM_yaml_file = FM_yaml_file,
                              MOD_yaml_file = MOD_yaml_file)

      uiele = state[["ASM"]][["ui_msg"]]

      uiele})
    #------------------------------------
    # Generated data reading code
    observe({
      # Reacting to file changes
      input$input_load_state
      input$input_select_sheet
      state = ASM_fetch_state(id           = id, 
                              input        = input, 
                              session      = session, 
                              FM_yaml_file = FM_yaml_file,
                              MOD_yaml_file = MOD_yaml_file)

      if(is.null(state[["ASM"]][["code"]])){
        uiele = "# code"
      } else {
        uiele = state[["ASM"]][["code"]]
      }


      shinyAce::updateAceEditor(
        session         = session,
        editorId        = "ASM_ui_ace_code",
        theme           = state[["yaml"]][["FM"]][["code"]][["theme"]],
        showLineNumbers = state[["yaml"]][["FM"]][["code"]][["showLineNumbers"]],
        readOnly        = state[["MC"]][["code"]][["readOnly"]],
        mode            = state[["MC"]][["code"]][["mode"]],
        value           = uiele)

    })
    #------------------------------------
    if(!is.null(react_state)){
      # Here we list the ui inputs that will result in a state change:
      toListen <- reactive({
        req(input$input_load_state)
        list(
             input$input_load_state
            )
      })
      # This updates the reaction state:
      observeEvent(toListen(), {
        state = ASM_fetch_state(id           = id, 
                                input        = input, 
                                session      = session, 
                                FM_yaml_file = FM_yaml_file,
                                MOD_yaml_file = MOD_yaml_file)
        
        FM_le(state, "reaction state updated")
        react_state[[id]] = state
      })
    }
  })
}

#'@export
#'@title Fetch State Manager State
#'@description Merges default app options with the changes made in the UI
#'@param id Shiny module ID
#'@param input Shiny input variable
#'@param session Shiny session variable
#'@param FM_yaml_file App configuration file with FM as main section.
#'@param MOD_yaml_file  Module configuration file with MC as main section.
#'@return list containing the current state of the app including default
#'values from the yaml file as well as any changes made by the user. The list
#'has the following structure:
#' \itemize{
#' \item{yaml:} Full contents of the supplied yaml file.
#' \item{MC:} Module components of the yaml file.
#' \item{ASM:}
#' \itemize{
#'   \item{isgood:} Boolean object indicating if the file was successfully loaded.
#'   \item{checksum:} This is an MD5 sum of the loaded state file
#' }
#'  \item{MOD_TYPE:} Character data containing the type of module \code{"ASM"}
#'  \item{id:} Character data containing the module id module in the session variable.
#'  \item{FM_yaml_file:} App configuration file with FM as main section.
#'  \item{MOD_yaml_file:}  Module configuration file with MC as main section.
#'}
ASM_fetch_state = function(id, input, session, FM_yaml_file, MOD_yaml_file){

  # Template for an empty dataset
  #---------------------------------------------
  # Getting the current state
  state = FM_fetch_mod_state(session, id)
  # If the state has not yet been defined then we
  # initialize it
  if(is.null(state)){
    # General state information
    state = ASM_init_state(FM_yaml_file, MOD_yaml_file, id)
  }

  #---------------------------------------------
  # Here we update the state based on user input
  for(ui_name in state[["ASM"]][["ui_ids"]]){
    if(!is.null(isolate(input[[ui_name]]))){
       state[["ASM"]][["ui"]][[ui_name]] = isolate(input[[ui_name]])
     } else {
       state[["ASM"]][["ui"]][[ui_name]] = ""
     }
   }
   msgs = c()

  #---------------------------------------------
  # Here we react to changes between the UI and the current state

  # This detects file uploads
  if(!is.null(isolate(input$input_load_state))){
    file_path = isolate(input$input_load_state$datapath)
    file_name = isolate(input$input_load_state$name)

    ls_isgood = TRUE

    if(!(file_ext(file_path) == "zip")){
      ls_isgood = FALSE
      msgs = c(msgs, paste0(
           "Unknown file extension (",
             file_ext(file_path)),
           " for saved state")
    }

    if(ls_isgood){

      test_checksum = digest::digest(file_path, algo=c("md5"))

      # This detects if the temporary file has changed. This is
      # how we tell if a file has been uploaded
      if(test_checksum != state[["ASM"]][["checksum"]]){
        FM_le(state, paste0("State upload file: ", file_name))
        FM_le(state, "  Unpacking saved state")
        # Making a diretory to unpack the files into:
        unpack_dir = tempfile(pattern="FM")
        if(dir.exists(unpack_dir)){
          unlink(unpack_dir, recursive = TRUE, force=TRUE)
        }
        dir.create(unpack_dir)
        zip::unzip(file_path, exdir=unpack_dir)

        # checking for the state rds file
        rds_file = file.path(unpack_dir, "fmas.rds")
        if(file.exists(rds_file)){

          # Reading in the app state
          app_state = readRDS(rds_file)
          # Removing the rds_file so it wont be in the
          # new app state
          unlink(rds_file)

          FM_le(state, "  Replacing app files")
          user_dir = FM_fetch_user_files_path(state)
          unlink(user_dir, recursive = TRUE, force=TRUE)
          file.rename(unpack_dir, user_dir)

          FM_le(state, "  Replacing app state/setting holds")
          FM_set_app_state(session, app_state, set_holds=TRUE)

        }else {
          FM_le(state, "  fmas.rds file not found in save state")
          ls_isgood = FALSE
          msgs = c(msgs, "fmas.rds file not found in saved state")
        }


        # The last thing we do is replace the state checksum with
        # the uploaded file checksum to prevent multiple uploads
        # each time the state has been fetched. We do this even if
        # the load failed because otherwise subsequenty fetch state
        # calls will attempt to load the failed state again.
        state[["ASM"]][["checksum"]] = test_checksum
        #---------------------------------------------
        # Passing any messages back to the user
        state = FM_set_ui_msg(state, msgs)
      }
    }
  }


  #---------------------------------------------
  # Saving the state
  FM_set_mod_state(session, id, state)

  # Returning the state
  state}

#'@export
#'@title Initialize ASM Module State
#'@description Creates a list of the initialized module state
#'@param FM_yaml_file App configuration file with FM as main section.
#'@param MOD_yaml_file  Module configuration file with MC as main section.
#'@param id ID string for the module.
#'@return list containing an empty ASM state
ASM_init_state = function(FM_yaml_file, MOD_yaml_file, id){

  button_counters = c(
   "button_state_save"
  )
  ui_ids          = c(
   "button_state_save",
   "ui_asm_save_name"
    )
  ui_hold         = c()


  state = FM_init_state(
    FM_yaml_file    = FM_yaml_file,
    MOD_yaml_file   = MOD_yaml_file,
    id              = id,
    MT              = "ASM",
    button_counters = button_counters,
    ui_ids          = ui_ids,
    ui_hold         = ui_hold)


  # Default checksum is NULL:
  state[["ASM"]][["checksum"]] = digest::digest(NULL, algo=c("md5"))

  FM_le(state, "State initialized")
  state}

#'@export
#'@title Fetch Download File Name
#'@description Gets either the file name specified by the user or the default
#'value if that is null
#'@param state List withe the current app state
#'@return character object with the download file name
ASM_fetch_dlfn = function(state){
  save_bfn  = state[["ASM"]][["ui"]][["ui_asm_save_name"]]
  if(is.null(save_bfn )){
     save_bfn  = state[["MC"]][["labels"]][["ui_asm_save_name"]]
  } else if(save_bfn  == "") {
     save_bfn  = state[["MC"]][["labels"]][["ui_asm_save_name"]]
  }
  dlfn    = paste0(save_bfn, ".zip")
  dlfn}

#'@export
#'@title Write State to File for Saving
#'@description Called from download handler and used to write a saved state
#'value if that is null
#'@param state List withe the current app state.
#'@param session Shiny session variable
#'@param file File name to write zipped state.
#'@return NULL
ASM_write_state = function(state, session, file){

  FM_le(state, paste0("writing app state to file on server: "))
  FM_le(state, paste0("  ", file))

  # User directory where uploaded files, logs, etc will be stored
  user_dir = FM_fetch_user_files_path(state)

  # Pulling out the app state
  app_state = FM_fetch_app_state(session)

  # Writing the app state object to a file:
  saveRDS(app_state, file.path(user_dir, "fmas.rds"))

  # Zipping everything up into an archive
  zip::zip(zipfile=file,
           files=dir(user_dir),
           recurse=TRUE,
           root = user_dir,
           include_directories=TRUE)
  FM_le(state, "done writing app state")
  NULL}


