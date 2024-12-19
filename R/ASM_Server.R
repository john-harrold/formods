#'@import rhandsontable
#'@import shiny
#'@importFrom digest digest
#'@importFrom shinyAce aceEditor updateAceEditor
#'@importFrom zip unzip zip


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
#'@param deployed Boolean variable indicating whether the app is deployed or not.
#'@param react_state Variable passed to server to allow reaction outside of module (\code{NULL})
#'@param mod_ids Vector of module IDs and order they are needed (used for code generation).
#'@return UD Server object
#'@example inst/test_apps/FM_compact.R
ASM_Server <- function(id,
                      FM_yaml_file  = system.file(package = "formods",
                                                  "templates",
                                                  "formods.yaml"),
                      MOD_yaml_file = system.file(package = "formods",
                                                  "templates",
                                                  "ASM.yaml"),
                      deployed     = FALSE,
                      react_state  = NULL,
                      mod_ids) {
  moduleServer(id, function(input, output, session) {

  # Used to trigger messages from download button
  toMessage = reactiveValues()

    #------------------------------------
    # Create ui outputs here:
    output$ui_asm_save_name_text  = renderUI({
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
    output$ui_asm_save_button  = renderUI({
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
    output$ui_asm_compact  =  renderUI({
      state = ASM_fetch_state(id           = id,
                              input        = input,
                              session      = session,
                              FM_yaml_file = FM_yaml_file,
                              MOD_yaml_file = MOD_yaml_file)


      docx_ph_uiele  =  NULL
      if(length(state[["ASM"]][["ph_uis"]]) > 0){
        docx_ph_uiele  =  tagList(
             tags$br(),
             tags$b(state[["MC"]][["labels"]][["gen_rpt_docx_ph"]]),
             div(style="text-align:center",
               htmlOutput(NS(id, "ui_asm_rpt_docx_ph"))
             ))
      }

      uiele = tagList(
           htmlOutput(NS(id, "ui_asm_save_name_text")),
           htmlOutput(NS(id, "ui_asm_switch_gen_rpts")),
           htmlOutput(NS(id, "ui_asm_save_button")),
           tags$br(),
           htmlOutput(NS(id, "ui_asm_load_state")),
           tags$b(state[["MC"]][["labels"]][["gen_rpt_header"]]),
           div(style="text-align:center",
             div(style="display:inline-block;width:32%",
             htmlOutput(NS(id, "ui_asm_rpt_pptx"))),
             div(style="display:inline-block;width:32%",
             htmlOutput(NS(id, "ui_asm_rpt_docx"))),
             div(style="display:inline-block;width:32%",
             htmlOutput(NS(id, "ui_asm_rpt_xlsx"))),
           ),
           docx_ph_uiele,
           tags$br(),
           verbatimTextOutput(NS(id, "ui_asm_msg"))
      )

      uiele})
    #------------------------------------
    output$ui_asm_switch_gen_rpts = renderUI({
      state = ASM_fetch_state(id           = id,
                              input        = input,
                              session      = session,
                              FM_yaml_file = FM_yaml_file,
                              MOD_yaml_file = MOD_yaml_file)

         uiele = materialSwitch(
           inputId     = NS(id, "switch_gen_rpts"),
           width       = state[["MC"]][["formatting"]][["switch_gen_reports"]][["width"]],
           right       = state[["MC"]][["formatting"]][["switch_gen_reports"]][["right"]],
           inline      = state[["MC"]][["formatting"]][["switch_gen_reports"]][["inline"]],
           label       = state[["MC"]][["labels"]][["ui_asm_switch_gen_rpts"]],
           status      = "success",
           value       = state[["MC"]][["formatting"]][["switch_gen_reports"]][["default"]]
           )

      uiele})
    #------------------------------------
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

        # Runing in a tryCatch enviornment to trap errors otherwise
        # they are lost. If it fails we log them.
     #  tcres = FM_tc("ws_res = ASM_write_state(state, session, file, mod_ids)",
     #                list(state   = state,
     #                     session = session,
     #                     file    = file,
     #                     mod_ids = mod_ids),
     #                c("ws_res"))

        if((any(c("ShinySession", "session_proxy") %in% class(session)))){
          if(system.file(package = "shinybusy") !=""){
            shinybusy::show_modal_spinner(text=state[["MC"]][["labels"]][["busy"]][["saving_state"]])
          }
        }

        tmp_cmd = "ws_res = ASM_save_state(state=state, session=session, file_path=file, pll=NULL)"
        tcres = FM_tc(cmd = tmp_cmd,
                      tc_env =
                      list(state     = state,
                           session   = session,
                           file = file),
                      capture = c("ws_res"))

        if((any(c("ShinySession", "session_proxy") %in% class(session)))){
          if(system.file(package = "shinybusy") !=""){
            shinybusy::remove_modal_spinner()
          }
        }


        if(!tcres$isgood){
          FM_le(state, "Failed to write state")
          FM_le(state, tmp_cmd)
          FM_le(state, tcres$msgs)

          state = FM_set_notification(
            state       = state,
            notify_text =  state[["MC"]][["errors"]][["save_failed"]],
            notify_id   = "ASM save failed",
            type        = "failure")

          state = FM_set_ui_msg(state, tcres$msgs)
          FM_set_mod_state(session, id, state)

          toMessage[["message"]] = TRUE
          notify_res =
          FM_notify(state = state,
           session     = session)
        }
      }
    )
    #------------------------------------
    # Upload State
    output$ui_asm_load_state = renderUI({
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

    #------------------------------------
    # Download pptx report
    output$button_rpt_pptx   = downloadHandler(
      filename = function() {
        state = ASM_fetch_state(id           = id,
                                input        = input,
                                session      = session,
                                FM_yaml_file = FM_yaml_file,
                                MOD_yaml_file = MOD_yaml_file)
       dlfn = ASM_fetch_dlfn(state, ".pptx")
       FM_le(state, paste0("pushing report: ", dlfn))
       dlfn},
      content = function(file) {
        state = ASM_fetch_state(id           = id,
                                input        = input,
                                session      = session,
                                FM_yaml_file = FM_yaml_file,
                                MOD_yaml_file = MOD_yaml_file)
        FM_pause_screen(state   = state,
                        message = state[["MC"]][["labels"]][["busy"]][["pptx"]],
                        session = session)
        rpt_res =
        FM_generate_report(state     = state,
                           session   = session,
                           file_dir  = dirname(file),
                           file_name = basename(file))

        FM_resume_screen(state   = state,
                         session = session)
        }
    )
    #------------------------------------
    # Download docx report
    output$button_rpt_docx   = downloadHandler(
      filename = function() {
        state = ASM_fetch_state(id           = id,
                                input        = input,
                                session      = session,
                                FM_yaml_file = FM_yaml_file,
                                MOD_yaml_file = MOD_yaml_file)
       dlfn = ASM_fetch_dlfn(state, ".docx")
       FM_le(state, paste0("pushing report: ", dlfn))
       dlfn},
      content = function(file) {
        state = ASM_fetch_state(id           = id,
                                input        = input,
                                session      = session,
                                FM_yaml_file = FM_yaml_file,
                                MOD_yaml_file = MOD_yaml_file)
        FM_pause_screen(state   = state,
                        message = state[["MC"]][["labels"]][["busy"]][["docx"]],
                        session = session)
        # Creating placeholder list to overwrite defaults in the yaml file
        # with values from the ui
        ph = list()
        if(length(state[["ASM"]][["ph_uis"]]) > 0){
          for(ph_ui in names(state[["ASM"]][["ph_uis"]])){
            ph_value = state[["ASM"]][["ui"]][[ph_ui]]
            if(!is.null(ph_value)){
              ph_name = state[["ASM"]][["ph_uis"]][[ph_ui]][["name"]]
              ph[[ph_name]] = ph_value
            }
          }
        }
        rpt_res =
        FM_generate_report(state     = state,
                           session   = session,
                           ph        = ph,
                           file_dir  = dirname(file),
                           file_name = basename(file))
        FM_resume_screen(state   = state,
                         session = session)
        }
    )
      uiele})
    #------------------------------------
    # Download xlsx report
    output$button_rpt_xlsx   = downloadHandler(
      filename = function() {
        state = ASM_fetch_state(id           = id,
                                input        = input,
                                session      = session,
                                FM_yaml_file = FM_yaml_file,
                                MOD_yaml_file = MOD_yaml_file)
       dlfn = ASM_fetch_dlfn(state, ".xlsx")
       FM_le(state, paste0("pushing report: ", dlfn))
       dlfn},
      content = function(file) {
        state = ASM_fetch_state(id           = id,
                                input        = input,
                                session      = session,
                                FM_yaml_file = FM_yaml_file,
                                MOD_yaml_file = MOD_yaml_file)
        FM_pause_screen(state   = state,
                        message = state[["MC"]][["labels"]][["busy"]][["xlsx"]],
                        session = session)
        rpt_res =
        FM_generate_report(state     = state,
                           session   = session,
                           file_dir  = dirname(file),
                           file_name = basename(file))
        FM_resume_screen(state   = state,
                         session = session)
        }
    )
    #------------------------------------
    # User messages:
    output$ui_asm_msg = renderText({
      input[["button_state_save"]]
      input[["input_load_state"]]
      toMessage$message
      state = ASM_fetch_state(id           = id,
                              input        = input,
                              session      = session,
                              FM_yaml_file = FM_yaml_file,
                              MOD_yaml_file = MOD_yaml_file)

      uiele = state[["ASM"]][["ui_msg"]]

      uiele})
    #------------------------------------
    # rpt xlsx
    output$ui_asm_rpt_xlsx  = renderUI({
      #req(input$X)
      state = ASM_fetch_state(id           = id,
                              input        = input,
                              session      = session,
                              FM_yaml_file = FM_yaml_file,
                              MOD_yaml_file = MOD_yaml_file)

      uiele = NULL
      if(state[["ASM"]][["isgood"]]){
        # Only generate the button if reporting is enabled
        if(state[["yaml"]][["FM"]][["reporting"]][["enabled"]]){
          uiele = downloadBttn(
                    outputId = NS(id, "button_rpt_xlsx"),
                    label    = state[["MC"]][["labels"]][["ui_asm_rpt_xlsx"]],
                    style    = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
                    size     = state[["MC"]][["formatting"]][["button_rpt_xlsx"]][["size"]],
                    block    = state[["MC"]][["formatting"]][["button_rpt_xlsx"]][["block"]],
                    color    = "success",
                    icon     = icon("arrow-down"))
        }
      }
      uiele})
    # rpt docx
    output$ui_asm_rpt_pptx  = renderUI({
      #req(input$X)
      state = ASM_fetch_state(id           = id,
                              input        = input,
                              session      = session,
                              FM_yaml_file = FM_yaml_file,
                              MOD_yaml_file = MOD_yaml_file)

      uiele = NULL
      if(state[["ASM"]][["isgood"]]){
        # Only generate the button if reporting is enabled
        if(state[["yaml"]][["FM"]][["reporting"]][["enabled"]]){
          uiele = downloadBttn(
                    outputId = NS(id, "button_rpt_pptx"),
                    label    = state[["MC"]][["labels"]][["ui_asm_rpt_pptx"]],
                    style    = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
                    size     = state[["MC"]][["formatting"]][["button_rpt_pptx"]][["size"]],
                    block    = state[["MC"]][["formatting"]][["button_rpt_pptx"]][["block"]],
                    color    = "danger",
                    icon     = icon("arrow-down"))
        }
      }
      uiele})
    # rpt docx
    output$ui_asm_rpt_docx  = renderUI({
      #req(input$X)
      state = ASM_fetch_state(id           = id,
                              input        = input,
                              session      = session,
                              FM_yaml_file = FM_yaml_file,
                              MOD_yaml_file = MOD_yaml_file)

      uiele = NULL
      if(state[["ASM"]][["isgood"]]){
        # Only generate the button if reporting is enabled
        if(state[["yaml"]][["FM"]][["reporting"]][["enabled"]]){
          uiele = downloadBttn(
                    outputId = NS(id, "button_rpt_docx"),
                    label    = state[["MC"]][["labels"]][["ui_asm_rpt_docx"]],
                    style    = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
                    size     = state[["MC"]][["formatting"]][["button_rpt_docx"]][["size"]],
                    block    = state[["MC"]][["formatting"]][["button_rpt_docx"]][["block"]],
                    color    = "primary",
                    icon     = icon("arrow-down"))
        }
      }
      uiele})
    # rpt docx
    output$ui_asm_rpt_docx_ph  = renderUI({
      #req(input$X)
      state = ASM_fetch_state(id           = id,
                              input        = input,
                              session      = session,
                              FM_yaml_file = FM_yaml_file,
                              MOD_yaml_file = MOD_yaml_file)

      uiele = NULL
      if(state[["ASM"]][["isgood"]]){
        if( length(state[["ASM"]][["ph_uis"]]) > 0){
          for(ph_ui in names(state[["ASM"]][["ph_uis"]])){
            tmp_value = state[["ASM"]][["ui"]][[ph_ui]]
            if(tmp_value == ""){
              tmp_value = state[["ASM"]][["ph_uis"]][[ph_ui]][["value"]]
            }

            tmp_uiele =
              textInput(
                inputId     = NS(id, ph_ui),
                width = state[["yaml"]][["FM"]][["reporting"]][["phs_formatting"]][["width"]],
                label = NULL,
                placeholder = state[["ASM"]][["ph_uis"]][[ph_ui]][["name"]],
                value = tmp_value)

            if(is.character( state[["ASM"]][["ph_uis"]][[ph_ui]][["tooltip"]])){
              tmp_uiele = FM_add_ui_tooltip(
                state    = state,
                uiele    = tmp_uiele,
                tooltip  = state[["ASM"]][["ph_uis"]][[ph_ui]][["tooltip"]],
                position = state[["yaml"]][["FM"]][["reporting"]][["phs_formatting"]][["tt_position"]],
                size     = state[["yaml"]][["FM"]][["reporting"]][["phs_formatting"]][["tt_size"]])
            }
            uiele = tagList(uiele, tags$div(tmp_uiele))
          }
        }
      }
      uiele})
    #------------------------------------
    output$ui_asm_sys_modules  = renderUI({
      state = ASM_fetch_state(id           = id,
                              input        = input,
                              session      = session,
                              FM_yaml_file = FM_yaml_file,
                              MOD_yaml_file = MOD_yaml_file)
      app_info = FM_fetch_app_info(session)

      uiele = app_info[["uiele_modules"]]

    uiele})
    #------------------------------------
    output$ui_asm_sys_packages = renderUI({
      state = ASM_fetch_state(id           = id,
                              input        = input,
                              session      = session,
                              FM_yaml_file = FM_yaml_file,
                              MOD_yaml_file = MOD_yaml_file)
      app_info = FM_fetch_app_info(session)

      uiele = app_info[["uiele_packages"]]

    uiele})
    #------------------------------------
    output$ui_asm_sys_options = renderUI({
      state = ASM_fetch_state(id           = id,
                              input        = input,
                              session      = session,
                              FM_yaml_file = FM_yaml_file,
                              MOD_yaml_file = MOD_yaml_file)
      app_info = FM_fetch_app_info(session)

      uiele = app_info[["uiele_options"]]

    uiele})
    #------------------------------------
    # fileReaderData must be defined outside of the outputs below so it
    # will react properly
    # This is the path to the log file
    state = ASM_fetch_state(id           = id,
                            input        = input,
                            session      = session,
                            FM_yaml_file = FM_yaml_file,
                            MOD_yaml_file = MOD_yaml_file)
    log_file = FM_fetch_log_path(state)
    fileReaderData <- shiny::reactiveFileReader(500, session, log_file, readLines)
    # Shows the rolling log
    output$ui_asm_sys_log  = renderText({
      state = ASM_fetch_state(id           = id,
                              input        = input,
                              session      = session,
                              FM_yaml_file = FM_yaml_file,
                              MOD_yaml_file = MOD_yaml_file)

      # Read the text, and make it a consistent number of lines so
      # that the output box doesn't grow in height.
      text               =  fileReaderData()
      max_n              =  state[["MC"]][["show_log"]]
      if(length(text) > max_n){
        n_start = length(text)-max_n+1
        n_stop  = length(text)
        text=text[c(n_start:n_stop)]
      }
      text[is.na(text)]  = ""
      uiele = paste(text, collapse = '\n')

    uiele})

#     cfg=gui_fetch_cfg(session)
# output$text_user_log <- renderText({
#   # Read the text, and make it a consistent number of lines so
#   # that the output box doesn't grow in height.
#   text <- fileReaderData()
#   length(text) <- cfg$gui$user_log_length
#   text[is.na(text)] <- ""
#   paste(text, collapse = '\n')
# })
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

  #------------------------------------
  toNotify <- reactive({
    list(input$input_load_state)
  })
  observeEvent(toNotify(), {
    state = ASM_fetch_state(id           = id,
                            input        = input,
                            session      = session,
                            FM_yaml_file = FM_yaml_file,
                            MOD_yaml_file = MOD_yaml_file)
    # Triggering optional notifications
    notify_res =
    FM_notify(state = state,
     session     = session)
  })
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
#'@examples
#' # Within shiny both session and input variables will exist,
#' # this creates examples here for testing purposes:
#' sess_res = ASM_test_mksession()
#' session = sess_res$session
#' input   = sess_res$input
#'
#' # Configuration files
#' FM_yaml_file  = system.file(package = "formods", "templates", "formods.yaml")
#' MOD_yaml_file = system.file(package = "formods", "templates", "ASM.yaml")
#'
#' # We need to specify the ID of the ASM module
#' id = "ASM"
#'
#' state = ASM_fetch_state(id           = id,
#'                         input        = input,
#'                         session      = session,
#'                         FM_yaml_file = FM_yaml_file,
#'                         MOD_yaml_file = MOD_yaml_file)
#'
#' state
ASM_fetch_state = function(id, input, session, FM_yaml_file, MOD_yaml_file){

  # Template for an empty dataset
  #---------------------------------------------
  # Getting the current state
  state = FM_fetch_mod_state(session, id)
  # If the state has not yet been defined then we
  # initialize it
  if(is.null(state)){
    # General state information
    state = ASM_init_state(FM_yaml_file, MOD_yaml_file, id, session)
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

    if(!(tolower(file_ext(file_path)) == "zip")){
      ls_isgood = FALSE
      msgs = c(msgs, paste0(
           "Unknown file extension (",
             file_ext(file_path),
           ") for saved state. Only .zip files allowed."))
    }

    if(ls_isgood){
      # This detects if the temporary file has changed. This is
      # how we tell if a file has been uploaded
      test_checksum = digest::digest(file_path, algo=c("md5"))

      if(test_checksum != state[["ASM"]][["checksum"]]){
        FM_pause_screen(state   = state,
                        message = state[["MC"]][["labels"]][["busy"]][["loading_state"]],
                        session = session)
        ls_res =
          ASM_load_state(state     = state,
                         session   = session,
                         file_path = file_path)

        # Pulling the state out of the load results:
        state = ls_res[["state"]]

        if(!ls_res[["isgood"]]){
          ls_isgood = FALSE
          msgs = c(msgs, ls_res[["msgs"]])
        }
        state[["ASM"]][["checksum"]] = test_checksum
      }
      FM_resume_screen(state   = state,
                       session = session)
    }

    # Setting notifications for the user
    if(ls_isgood){
      state = FM_set_notification(
        state       = state,
        notify_text =  state[["MC"]][["labels"]][["load_success"]],
        notify_id   = "ASM load failed",
        type        = "success")

    } else {
      state = FM_set_notification(
        state       = state,
        notify_text =  state[["MC"]][["errors"]][["load_failed"]],
        notify_id   = "ASM load failed",
        type        = "failure")
    }

    # Passing any messages back to the user
    state = FM_set_ui_msg(state, msgs)
  }


  #---------------------------------------------
  # Saving the state
  FM_set_mod_state(session, id, state)

  # Returning the state
  state}

#'@export
#'@title Updates ASM After State Load
#'@description Creates a list of the initialized module state
#'@param state ASM state object
#'@param session Shiny session variable
#'@return List contianing the ASM state objects and shiny session object
ASM_onload  = function(state, session){
  if(is_shiny(session)){
    for(ph_ui in names(state[["ASM"]][["ph_uis"]])){
      updateTextInput(session, inputId=ph_ui, value=state[["ASM"]][["ui"]][[ph_ui]])
    }
  }

  res =
  list(state   = state,
       session = session)
res}

#'@export
#'@title Initialize ASM Module State
#'@description Creates a list of the initialized module state
#'@param FM_yaml_file App configuration file with FM as main section.
#'@param MOD_yaml_file  Module configuration file with MC as main section.
#'@param id ID string for the module.
#'@param session Shiny session variable
#'@return list containing an empty ASM state
#'@examples
#' # Within shiny the session variable will exist,
#' # this creates an example here for testing purposes:
#' sess_res = ASM_test_mksession()
#' session = sess_res$session
#'state = ASM_init_state(
#'    FM_yaml_file  = system.file(package = "formods",
#'                                "templates",
#'                                "formods.yaml"),
#'    MOD_yaml_file = system.file(package = "formods",
#'                                "templates",
#'                                "ASM.yaml"),
#'    id              = "ASM",
#'    session         = session)
#' state
ASM_init_state = function(FM_yaml_file, MOD_yaml_file, id, session){

  button_counters = c(
   "button_state_save",
   "button_rpt_xlsx",
   "button_rpt_docx",
   "button_rpt_pptx"
  )

  ui_ids          = c(
   "button_state_save",
   "button_rpt_xlsx",
   "button_rpt_docx",
   "button_rpt_pptx",
   "ui_asm_save_name",
   "switch_gen_rpts"
    )


  # Reading in the yaml file to get the names of the placeholders
  formods_yaml =   yaml::read_yaml(FM_yaml_file)

  ph_uis = list()
  if(!is.null(formods_yaml[["FM"]][["reporting"]][["phs"]])){
    for(ph_idx in 1:length(formods_yaml[["FM"]][["reporting"]][["phs"]])){
      ph_ui = paste0("ui_asm_docx_ph_", formods_yaml[["FM"]][["reporting"]][["phs"]][[ph_idx]][["name"]])
      ph_uis[[ph_ui]]             = formods_yaml[["FM"]][["reporting"]][["phs"]][[ph_idx]]
      ph_uis[[ph_ui]][["ph_idx"]] = ph_idx
    }
  }

  # Adding the placeholder uis to the uis for the module
  ui_ids = c(ui_ids, names(ph_uis))

  ui_hold         = c()

  state = FM_init_state(
    FM_yaml_file    = FM_yaml_file,
    MOD_yaml_file   = MOD_yaml_file,
    id              = id,
    MT              = "ASM",
    button_counters = button_counters,
    ui_ids          = ui_ids,
    ui_hold         = ui_hold,
    session         = session)


  # Default checksum is NULL:
  state[["ASM"]][["checksum"]] = digest::digest(NULL, algo=c("md5"))

  # Saving the placeholder information
  state[["ASM"]][["ph_uis"]] = ph_uis

  # Saving the values in the ui portion of the state
  state[["ASM"]][["ui"]] = list()

  if(!is.null(formods_yaml[["FM"]][["reporting"]][["phs"]])){
    for(ph_ui  in names(ph_uis)){
      state[["ASM"]][["ui"]][[ph_ui]] = ph_uis[[ph_ui]][["value"]]
    }
  }

  FM_le(state, "State initialized")
  state}

#'@export
#'@title Fetch Download File Name
#'@description Gets either the file name specified by the user or the default
#'value if that is null
#'@param state ASM state from \code{ASM_fetch_state()}
#'@param extension File extension for the download (default: ".zip")
#'@return character object with the download file name
#'@examples
#' # Creating a state object for testing
#' sess_res = ASM_test_mksession()
#' state = sess_res$state
#' dlfn = ASM_fetch_dlfn(state)
#' dlfn
ASM_fetch_dlfn = function(state, extension=".zip"){
  save_bfn  = state[["ASM"]][["ui"]][["ui_asm_save_name"]]
  if(is.null(save_bfn )){
     save_bfn  = state[["MC"]][["labels"]][["ui_asm_save_name"]]
  } else if(save_bfn  == "") {
     save_bfn  = state[["MC"]][["labels"]][["ui_asm_save_name"]]
  }
  dlfn    = paste0(save_bfn, extension)
  dlfn}

#  #'@export
#  #'@title Write State to File for Saving (depreciated)
#  #'@description Called from download handler and used to write a saved state
#  #'value if that is null
#  #'@param state ASM state from \code{ASM_fetch_state()}
#  #'@param session Shiny session variable
#  #'@param file File name to write zipped state.
#  #'@param mod_ids Vector of module IDs and order they are needed (used for code generation).
#  #'@return This function only writes the state and has no return value.
#  #'@examples
#  #' \donttest{
#  #' # Within shiny both session and input variables will exist,
#  #' # this creates examples here for testing purposes:
#  #' sess_res = ASM_test_mksession()
#  #' session = sess_res$session
#  #' input   = sess_res$input
#  #'
#  #' # Configuration files
#  #' FM_yaml_file  = system.file(package = "formods", "templates", "formods.yaml")
#  #' MOD_yaml_file = system.file(package = "formods", "templates", "ASM.yaml")
#  #'
#  #' # We need to specify the ID of the ASM module
#  #' id = "ASM"
#  #'
#  #' state = ASM_fetch_state(id           = id,
#  #'                         input        = input,
#  #'                         session      = session,
#  #'                         FM_yaml_file = FM_yaml_file,
#  #'                         MOD_yaml_file = MOD_yaml_file)
#  #'
#  #' ASM_write_state(state, session,
#  #'                 file    = tempfile(fileext=".zip"),
#  #'                 mod_ids = c("UD"))
#  #' }
#  ASM_write_state = function(state, session, file, mod_ids){
#
#    if((any(c("ShinySession", "session_proxy") %in% class(session)))){
#      if(system.file(package = "shinybusy") !=""){
#        shinybusy::show_modal_spinner(text=state[["MC"]][["labels"]][["busy"]][["saving_state"]])
#      }
#    }
#
#
#    FM_le(state, paste0("writing app state to file on server: "))
#    FM_le(state, paste0("  ", file))
#
#    # User directory where uploaded files, logs, etc will be stored
#    user_dir = FM_fetch_user_files_path(state)
#
#    # Pulling out the app state
#    app_state = FM_fetch_app_state(session)
#
#    # Pulling out the reproducible app code
#    app_code = FM_fetch_app_code(session=session, state = state, mod_ids = mod_ids)
#
#    # Generating reports
#    switch_gen_rpts = state[["ASM"]][["ui"]][["switch_gen_rpts"]]
#    if(!is.logical(switch_gen_rpts)){
#      switch_gen_rpts =  FALSE
#    }
#
#    # Clearing reports from the user directory
#    rptdir = file.path(user_dir, "reports")
#    if(dir.exists(rptdir)){
#      unlink(rptdir, recursive=TRUE)
#    }
#    dir.create(rptdir)
#
#    rpttypes = c("xlsx", "pptx", "docx")
#    rptctr = 1
#
#    code_only_msg = ""
#    if(!switch_gen_rpts){
#      code_only_msg = " code only "
#      FM_le(state, "Generating reports (code only)")
#    } else {
#      FM_le(state, "Generating reports")
#    }
#
#    for(rpttype in rpttypes){
#      if(system.file(package = "shinybusy") !=""){
#        if((any(c("ShinySession", "session_proxy") %in% class(session)))){
#          shinybusy::update_modal_spinner(text=
#                  paste0(state[["MC"]][["labels"]][["busy"]][[rpttype]], code_only_msg, "(",rptctr, "/", length(rpttypes),")"))
#        }
#      }
#
#      rpt_file_name = paste0("report.", rpttype)
#      grres = FM_generate_report(
#         state         = state,
#         session       = session,
#         file_dir      = rptdir ,
#         file_name     = rpt_file_name,
#         gen_code_only = !(switch_gen_rpts),
#         rpterrors     = TRUE)
#
#      # Appending the report generation code
#      if(grres[["isgood"]]){
#        app_code[["code"]] = c(app_code[["code"]], paste0("# Generating report: ", rpttype))
#        app_code[["code"]] = c(app_code[["code"]], grres[["code"]])
#      } else {
#        app_code[["code"]] = c(app_code[["code"]], paste0("# ", rpttype, " not generated"))
#        app_code[["code"]] = c(app_code[["code"]], paste0("# ", grres[["errmsg"]]))
#      }
#
#      rptctr = rptctr + 1
#    }
#
#    if(app_code[["isgood"]]){
#      # Writing app_code to the export script
#      gen_file = state[["yaml"]][["FM"]][["code"]][["gen_file"]]
#      if(file.exists(file.path(user_dir, gen_file))){
#        unlink(file.path(user_dir, gen_file))
#      }
#      write(app_code[["code"]], file=file.path(user_dir, gen_file), append=FALSE)
#    } else {
#      if(!is.null(app_code[["msgs"]])){
#        FM_le(state, app_code[["msgs"]])
#      }
#    }
#
#    # Writing the app state object to a file:
#    saveRDS(app_state, file.path(user_dir, "fmas.rds"))
#
#
#    if((any(c("ShinySession", "session_proxy") %in% class(session)))){
#      if(system.file(package = "shinybusy") !=""){
#        shinybusy::remove_modal_spinner()
#      }
#    }
#
#    # Zipping everything up into an archive
#    zip::zip(zipfile=file,
#             files=dir(user_dir),
#             recurse=TRUE,
#             root = user_dir,
#             include_directories=TRUE)
#    FM_le(state, "done writing app state")
#    NULL}

#'@export
#'@title Fetch Module Code
#'@description Fetches the code to generate results seen in the app
#'@param state ASM state from \code{ASM_fetch_state()}
#'@return The ASM module does not generate code
#'@examples
#' # Creating a state object for testing
#' sess_res = ASM_test_mksession()
#' state = sess_res$state
#' code = ASM_fetch_code(state)
ASM_fetch_code = function(state){

  code = NULL

code}

#'@export
#'@title Populate Session Data for Module Testing
#'@description Populates the supplied session variable for testing.
#'@param session Shiny session variable (in app) or a list (outside of app)
#'@return The ASM portion of the `all_sess_res` returned from \code{\link{FM_app_preload}}
#'@examples
#' session = shiny::MockShinySession$new()
#' sess_res = ASM_test_mksession(session=session)
#'@seealso \code{\link{FM_app_preload}}
ASM_test_mksession = function(session=list()){

  sources = c(system.file(package="formods", "preload", "ASM_preload.yaml"))
  res = FM_app_preload(session=session, sources=sources)
  res = res[["all_sess_res"]][["ASM"]]

res}



#'@export
#'@title Preload Data for ASM Module
#'@description Populates the supplied session variable with information from
#'list of sources.
#'@param session     Shiny session variable (in app) or a list (outside of app)
#'@param src_list    List of preload data (all read together with module IDs at the top level)
#'@param yaml_res    Result of reading in the formods (fm_cfg) and module (mod_cfg) yaml files
#'@param mod_ID      Module ID of the module being loaded.
#'@param react_state Reactive shiny object (in app) or a list (outside of app) used to trigger reactions
#'@param quickload   Logical \code{TRUE} to load reduced analysis \code{FALSE} to load the full analysis
#'@return list with the following elements
#' \itemize{
#'   \item{isgood:}      Boolean indicating the exit status of the function.
#'   \item{msgs:}        Messages to be passed back to the user.
#'   \item{session:}     Session object
#'   \item{input:}       The value of the shiny input at the end of the session initialization.
#'   \item{state:}       App state.
#'   \item{react_state:} The \code{react_state} components.
#'}
ASM_preload  = function(session, src_list, yaml_res=NULL, mod_ID=NULL, react_state = list(), quickload=FALSE){
  isgood = TRUE
  input  = list()
  msgs   = c()

  FM_yaml_file  = render_str(src_list[[mod_ID]][["fm_yaml"]])
  MOD_yaml_file = render_str(src_list[[mod_ID]][["mod_yaml"]])

  state = ASM_fetch_state(id            = mod_ID,
                          input         = input,
                          session       = session,
                          FM_yaml_file  = FM_yaml_file,
                          MOD_yaml_file = MOD_yaml_file)


  # Populating any word document preload values:
  if(length(names(src_list[[mod_ID]][["docx_ph"]])) > 0){
    # Placeholders defined for the app in the formods.yaml file
    fm_phs = yaml_res[[mod_ID]][["fm_cfg"]][["FM"]][["reporting"]][["phs"]]

    # Just the names found
    found_ph_names = as.vector(unlist(fm_phs)[names(unlist(fm_phs)) == "name"])

    # placeholders loaded from the preload file
    l_phs = src_list[[mod_ID]][["docx_ph"]]

    # Setting word placeholders
    FM_le(state, paste0("setting word placeholders: "))
    for(ph_ui in names(state[["ASM"]][["ph_uis"]])){
      # If the placeholder was found in the preload AND
      # if the name exists in formods.yaml then we set it
      ph_name = state[["ASM"]][["ph_uis"]][[ ph_ui ]][["name"]]
      if(ph_name %in% names(l_phs) & ph_name %in% found_ph_names){
        formods::FM_le(state,paste0("  -> setting docx ph: ",ph_name, " = ", l_phs[[ph_name]]))
        # Updates at the ui storage location
        state[["ASM"]][["ui"]][[ph_ui]]                = l_phs[[ph_name]]

        # Updates the default value as well
        state[["ASM"]][["ph_uis"]][[ph_ui]][["value"]] = l_phs[[ph_name]]
      }
    }
  }

  # Required for proper reaction:
  react_state[[mod_ID]]  = list(ASM = list(checksum=state[["ASM"]][["checksum"]]))

  # Saving the state
  if(is_shiny(session)){
    FM_set_mod_state(session, mod_ID, state)
  } else {
    session = FM_set_mod_state(session, mod_ID, state)
  }

  formods::FM_le(state,paste0("module isgood: ",isgood))

  res = list(isgood      = isgood,
             msgs        = msgs,
             session     = session,
             input       = input,
             react_state = react_state,
             state       = state)
res}


#'@export
#'@title Read App State From Yaml Files
#'@description Reads in the app state from yaml files.
#'@param sources     Vector of at corresponds with the ID used to call the modules UI elements
#'@return list with the following elements
#' \itemize{
#'   \item{isgood:}       Boolean indicating the exit status of the function.
#'   \item{msgs:}         Messages to be passed back to the user.
#'   \item{src_list:}     List containing the result of reading all of the sources.
#'   \item{yaml_res:}     Lists with elements for each module ID found in src_list with elements holding the modules configuration file \code{"mod_cfg"} and the modules formods configuration file \code{"fm_cfg"}
#'}
#'@examples
#'res = ASM_read_app_state(sources=system.file(package="formods", "preload", "UD_preload.yaml"))
ASM_read_app_state = function(sources=NULL){

  isgood   = TRUE
  msgs     = c()
  src_list = list()
  err_msgs = c()

  for(tmp_source in sources){
    if(file.exists(tmp_source)){
      src_list = c(src_list, yaml::read_yaml(tmp_source))
    } else {
      isgood = FALSE
      err_msgs = c(err_msgs, paste0("File does not exist: ", tmp_source))
    }
  }

  # Getting the module information from the different IDs found in the sources
  yaml_res = list()
  for(mod_ID in names(src_list)){
    mod_yaml = formods::render_str(src_list[[mod_ID]][["mod_yaml"]])
    if(file.exists(mod_yaml)){
      yaml_res[[mod_ID]][["mod_cfg"]]  = yaml::read_yaml(mod_yaml)
    } else {
      isgood = FALSE
      err_msgs = c(err_msgs, paste0("Module ", mod_ID, ", mod_yaml does not exist: ", mod_yaml))
    }


    fm_yaml  = formods::render_str(src_list[[mod_ID]][["fm_yaml"]])
    if(file.exists(fm_yaml)){
      yaml_res[[mod_ID]][["fm_cfg"]]  = yaml::read_yaml(fm_yaml)
    } else {
      isgood = FALSE
      err_msgs = c(err_msgs, paste0("Module ", mod_ID, ", fm_yaml does not exist: ", fm_yaml))
    }
  }

  if(!isgood){
    for(msg_line in err_msgs){
      FM_message(msg_line, entry_type="danger")
    }
    msgs = c(msgs, err_msgs)
  }

  res = list(
    isgood   = isgood,
    msgs     = msgs,
    src_list = src_list,
    yaml_res = yaml_res)


res}


#'@export
#'@title Make List of Current ASM State
#'@description Converts the current ASM state into a preload list.
#'@param state ASM state object
#'@return list with the following elements
#' \itemize{
#'   \item{isgood:}       Boolean indicating the exit status of the function.
#'   \item{msgs:}         Messages to be passed back to the user.
#'   \item{yaml_list:}    Lists with preload components.
#'}
#'@examples
#' sess_res = ASM_test_mksession()
#' state = sess_res$state
#' res = ASM_mk_preload(state)
ASM_mk_preload     = function(state){
  isgood    = TRUE
  msgs      = c()
  yaml_list = list()

  yaml_list[[ state[["id"]] ]] = list(
      fm_yaml  = file.path("config", basename(state[["FM_yaml_file"]])),
      mod_yaml = file.path("config", basename(state[["MOD_yaml_file"]]))
  )

  # Setting the word placeholder values
  for(ph_ui in names(state[["ASM"]][["ph_uis"]])){
    ph_name = state[["ASM"]][["ph_uis"]][[ ph_ui ]][["name"]]
    yaml_list[[ state[["id"]] ]][["docx_ph"]][[ph_name]] = state[["ASM"]][["ui"]][[ph_ui]]
  }

  formods::FM_le(state,paste0("mk_preload isgood: ",isgood))

  res = list(
    isgood    = isgood,
    msgs      = msgs,
    yaml_list = yaml_list)
}

#'@export
#'@title Write State to File for Saving
#'@description Called from download handler and used to write a saved state
#'value if that is null
#'@param state ASM state from \code{ASM_fetch_state()}
#'@param session Shiny session variable
#'@param file_path File path to write zipped state
#'@param pll Preload list of the format generated by \code{FM_mk_app_preload()}. IF set to \code{NULL} it will be generated from the contents of the session variable.
#'@return This function only writes the state and returns a list with the
#'following elements:
#' \itemize{
#'   \item{isgood:}      Boolean indicating the exit status of the function.
#'   \item{msgs:}        Messages to be passed back to the user.
#' }
#'@examples
#' # Within shiny the session variable will exist
#' # this creates examples here for testing purposes:
#' sess_res = ASM_test_mksession()
#' session = sess_res$session
#' state   = sess_res$state
#'
#' ssf  = tempfile(fileext=".zip")
#'
#' ss_res =
#' ASM_save_state(state, session,
#'                file_path  = ssf)
ASM_save_state = function(state, session, file_path, pll = NULL){

  isgood = TRUE
  msgs   = c()

  # If pll is null then we generate it here:
  if(is.null(pll)){
    mkp_res  = FM_mk_app_preload(session)
    if(mkp_res[["isgood"]]){
      pll = mkp_res[["yaml_list"]]
    }else{
      isgood = FALSE
      msg  = c(msg, "FM_mk_app_preload() failed", mkp_res[["msgs"]])
    }
  }

  if(isgood){
    FM_le(state, paste0("writing app state to file on server: "))
    FM_le(state, paste0("  ", file_path))

    # Pulling out the module IDs being saved
    mod_ids = names(pll)

    # User directory where uploaded files, logs, etc will be stored
    user_dir = FM_fetch_user_files_path(state)

    # Writing preload yaml file
    yaml::write_yaml(x=pll, file=file.path(user_dir, "preload.yaml"))

    # Pulling out the reproducible app code
    app_code = FM_fetch_app_code(session=session, state = state, mod_ids = mod_ids)

    # Generating reports
    switch_gen_rpts = state[["ASM"]][["ui"]][["switch_gen_rpts"]]
    if(!is.logical(switch_gen_rpts)){
      switch_gen_rpts =  FALSE
    }

    # Clearing reports from the user directory
    rptdir = file.path(user_dir, "reports")
    if(dir.exists(rptdir)){
      unlink(rptdir, recursive=TRUE)
    }
    dir.create(rptdir)

    rpttypes = c("xlsx", "pptx", "docx")
    rptctr = 1

    code_only_msg = ""
    if(!switch_gen_rpts){
      code_only_msg = " code only "
      FM_le(state, "Generating reports (code only)")
    } else {
      FM_le(state, "Generating reports")
    }

    for(rpttype in rpttypes){
      if(system.file(package = "shinybusy") !=""){
        if((any(c("ShinySession", "session_proxy") %in% class(session)))){
          shinybusy::update_modal_spinner(text=
                  paste0(state[["MC"]][["labels"]][["busy"]][[rpttype]], code_only_msg, "(",rptctr, "/", length(rpttypes),")"))
        }
      }

      rpt_file_name = paste0("report.", rpttype)
      grres = FM_generate_report(
         state         = state,
         session       = session,
         file_dir      = rptdir ,
         file_name     = rpt_file_name,
         gen_code_only = !(switch_gen_rpts),
         rpterrors     = TRUE)

      # Appending the report generation code
      if(grres[["isgood"]]){
        app_code[["code"]] = c(app_code[["code"]], paste0("# Generating report: ", rpttype))
        app_code[["code"]] = c(app_code[["code"]], grres[["code"]])
      } else {
        app_code[["code"]] = c(app_code[["code"]], paste0("# ", rpttype, " not generated"))
        app_code[["code"]] = c(app_code[["code"]], paste0("# ", grres[["errmsg"]]))
      }

      rptctr = rptctr + 1
    }

    if(app_code[["isgood"]]){
      # Writing app_code to the export script
      gen_file = state[["yaml"]][["FM"]][["code"]][["gen_file"]]
      if(file.exists(file.path(user_dir, gen_file))){
        unlink(file.path(user_dir, gen_file))
      }
      write(app_code[["code"]], file=file.path(user_dir, gen_file), append=FALSE)
    } else {
      if(!is.null(app_code[["msgs"]])){
        FM_le(state, app_code[["msgs"]])
      }
    }


    # Zipping everything up into an archive
    zip::zip(zipfile=file_path,
             files=dir(user_dir),
             recurse=TRUE,
             root = user_dir,
             include_directories=TRUE)
    FM_le(state, "done writing app state")
  }

  res = list(isgood = isgood,
             msgs   = msgs)
  res}

#'@export
#'@title Write State to File for Saving
#'@description Called from download handler and used to write a saved state
#'value if that is null
#'@param state ASM state from \code{ASM_fetch_state()}
#'@param session Shiny session variable
#'@param file_path Zip file with the saved sate
#'@return This function only writes the state and returns a list with the
#'following elements:
#' \itemize{
#'   \item{isgood:}      Boolean indicating the exit status of the function.
#'   \item{msgs:}        Messages to be passed back to the user.
#'   \item{state:}       AMS state
#'   \item{session:}     Session object
#' }
#'@examples
#' # Within shiny the session variable will exist
#' # this creates examples here for testing purposes:
#' sess_res = ASM_test_mksession()
#' session = sess_res$session
#' state   = sess_res$state
#'
#' ssf  = tempfile(fileext=".zip")
#'
#' ss_res =
#' ASM_save_state(state, session,
#'                file_path = ssf)
#'
#' ls_res =
#' ASM_load_state(state, session,
#'                file_path = ssf)
ASM_load_state = function(state, session, file_path){
  isgood     = TRUE
  msgs       = c()
  tmp_ol_res = NULL

  unpack_dir = tempfile(pattern="FM")
  if(dir.exists(unpack_dir)){
    unlink(unpack_dir, recursive = TRUE, force=TRUE)
  }
  dir.create(unpack_dir)
  zip::unzip(file_path, exdir=unpack_dir)

  # Moving to the unpack_dir to work from
  old_wd = getwd()
  setwd(unpack_dir)
  on.exit( setwd(old_wd))

  #----------------------------------------
  # cleaning out the files in the user directory basically keeping the old log
  # file:
  user_dir = FM_fetch_user_files_path(state)

  # These are the files that are not replaced. Mainly the log file:
  excludes = c(state[["yaml"]][["FM"]][["logging"]][["log_file"]])

  # These are the files to delete:
  fdel = dir(user_dir)
  fdel = fdel[!(fdel %in% excludes)]

  # This will just delete what is left in fdel
  for(fname in fdel){
    unlink(file.path(user_dir, fname), recursive = TRUE, force=TRUE)
  }

  # This resets the app to empty:
  reset_res = FM_reset_app(session)
  if(!("ShinySession" %in% class(session))){
    session = reset_res
  }

  # Next we run the preload yaml
  res = FM_app_preload(session=session, sources="preload.yaml")

  if(res[["isgood"]]){
    if(!("ShinySession" %in% class(session))){
      session = res[["session"]]
    }
  } else {
    isgood = FALSE
    msgs = c(msgs, res[["msgs"]])
  }

  if(isgood){
    # Applying any onload functions that were found
    app_info = FM_fetch_app_info(session)
    for(tmp_id in names(app_info[["modules"]])){

      tmp_MOD_TYPE = app_info[["modules"]][[tmp_id]][["MOD_TYPE"]]
      MOD_FUNC     = paste0(tmp_MOD_TYPE, "_onload")

      # Pulling out the state
      tmp_state = FM_fetch_mod_state(id=tmp_id, session=session)
      FM_le(state, paste0("post-processing state for module: ", tmp_MOD_TYPE, " id: ", tmp_id))

      # Looking for onload function
      if(exists(MOD_FUNC, mode="function")){
        FM_le(state, paste0("  -> running ", MOD_FUNC, "() for module id: ", tmp_id))

        # If there is we update the state/session and save the state
        FUNC_CALL = paste0("tmp_ol_res = ", MOD_FUNC,"(state = tmp_state, session=session)")
        eval(parse(text=FUNC_CALL))

        # Pulling out the state and session objects to use here
        tmp_state = tmp_ol_res[["state"]]
        if(!is_shiny(session)){
          session = tmp_ol_res[["session"]]
        }
      }
      # Setting all holds in the state
      for(tmp_ui_hold in names(  tmp_state[[tmp_MOD_TYPE]][["ui_hold"]])){
        tmp_state[[tmp_MOD_TYPE]][["ui_hold"]][[tmp_ui_hold]] = TRUE
      }

      # Saving any changes to the state
      if(is_shiny(session)){
        FM_set_mod_state(id = tmp_id, state = tmp_state, session = session)
      } else {
        session = FM_set_mod_state(id = tmp_id, state = tmp_state, session = session)
      }
    }
  }

  setwd(old_wd)

  # Passing any messages back to the user
  state = FM_set_ui_msg(state, msgs)
  res = list(isgood   = isgood,
             state    = state,
             session  = session,
             msgs     = msgs)
  res}
