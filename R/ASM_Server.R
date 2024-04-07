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
      uiele = tagList(
           htmlOutput(NS(id, "ui_asm_save_name_text")),
           htmlOutput(NS(id, "ui_asm_switch_gen_rpts")),
           htmlOutput(NS(id, "ui_asm_save_button")),
           tags$br(),
           div(style="text-align:center",
             div(style="display:inline-block;width:32%",
             htmlOutput(NS(id, "ui_asm_rpt_pptx"))),
             div(style="display:inline-block;width:32%",
             htmlOutput(NS(id, "ui_asm_rpt_docx"))),
             div(style="display:inline-block;width:32%",
             htmlOutput(NS(id, "ui_asm_rpt_xlsx"))),
           ),
           tags$br(),
           tags$br(),
           htmlOutput(NS(id, "ui_asm_load_state")),
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
        tcres = FM_tc("ws_res = ASM_write_state(state, session, file, mod_ids)",
                      list(state   = state,
                           session = session,
                           file    = file,
                           mod_ids = mod_ids),
                      c("ws_res"))

        if(!tcres$isgood){
          FM_le(state, "Failed to write state")
          FM_le(state, tcres$msgs)
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
        rpt_res =
        FM_generate_report(state     = state,
                           session   = session,
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
 # JMH delete?
 #  # Generated data reading code
 #  observe({
 #    # Reacting to file changes
 #    input$input_load_state
 #    input$input_select_sheet
 #    state = ASM_fetch_state(id           = id,
 #                            input        = input,
 #                            session      = session,
 #                            FM_yaml_file = FM_yaml_file,
 #                            MOD_yaml_file = MOD_yaml_file)
 #
 #    if(is.null(state[["ASM"]][["code"]])){
 #      uiele = "# code"
 #    } else {
 #      uiele = state[["ASM"]][["code"]]
 #    }
 #
 #
 #    shinyAce::updateAceEditor(
 #      session         = session,
 #      editorId        = "ui_asm_ace_code",
 #      theme           = state[["yaml"]][["FM"]][["code"]][["theme"]],
 #      showLineNumbers = state[["yaml"]][["FM"]][["code"]][["showLineNumbers"]],
 #      readOnly        = state[["MC"]][["code"]][["readOnly"]],
 #      mode            = state[["MC"]][["code"]][["mode"]],
 #      value           = uiele)
 #
 #  })
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
#' sess_res = ASM_test_mksession(session=list(), full_session=FALSE)
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
      FM_pause_screen(state   = state,
                      message = state[["MC"]][["labels"]][["busy"]][["loading_state"]],
                      session = session)


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

          FM_le(state, "  Replacing app files")
          # These are the files from the upload that we want to keep
          fkeep = dir(unpack_dir)
          fkeep = fkeep[!(fkeep %in% excludes)]
          for(fname in fkeep){
            FM_le(state, paste0("   -> ", fname))
            file.rename(from       = file.path(unpack_dir, fname),
                        to         = file.path(user_dir,   fname))
          }

          # The shiny_token changes from session to session. When we load the
          # old session we need to replace the token from the previous session
          # with the token from the current session which should be stored in
          # the state object.
          FM_le(state, "  Token update")
          for(asele in names(app_state)){
            if("shiny_token" %in% names(app_state[[asele]])){
              app_state[[asele]][["shiny_token"]] = state[["shiny_token"]]
            }
          }

          # Applying any onload functions that were found
          for(asele in names(app_state)){
            if("shiny_token" %in% names(app_state[[asele]])){
              tmp_state = app_state[[asele]]

              # Next we look to see if there is an onload function for the
              # current module:
              tmp_MOD_TYPE = tmp_state[["MOD_TYPE"]]
              MOD_FUNC     = paste0(tmp_MOD_TYPE, "_onload")
              
              if(exists(MOD_FUNC, mode="function")){
                FM_le(state, paste0("  Processing ", MOD_FUNC, "() for module id: ", tmp_state[["id"]]))
                # If there is we update the state and put it back in the
                # app_state object
                FUNC_CALL = paste0("tmp_state = ", MOD_FUNC,"(state = tmp_state, session=session)")
                eval(parse(text=FUNC_CALL))
                app_state[[asele]] = tmp_state
              }
            }
          }

          FM_le(state, "  Replacing app state/setting holds")
          FM_fetch_user_files_path(state)
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
#' sess_res = ASM_test_mksession(session=list(), full_session=FALSE)
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
#' sess_res = ASM_test_mksession(session=list(), full_session=FALSE)
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

#'@export
#'@title Write State to File for Saving
#'@description Called from download handler and used to write a saved state
#'value if that is null
#'@param state ASM state from \code{ASM_fetch_state()}
#'@param session Shiny session variable
#'@param file File name to write zipped state.
#'@param mod_ids Vector of module IDs and order they are needed (used for code generation).
#'@return This function only writes the state and has no return value.
#'@examples
#' \donttest{
#' # Within shiny both session and input variables will exist,
#' # this creates examples here for testing purposes:
#' sess_res = ASM_test_mksession(session=list(), full_session=FALSE)
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
#' ASM_write_state(state, session,
#'                 file    = tempfile(fileext=".zip"),
#'                 mod_ids = c("UD"))
#' }
ASM_write_state = function(state, session, file, mod_ids){

  if((any(c("ShinySession", "session_proxy") %in% class(session)))){
    if(system.file(package = "shinybusy") !=""){
      shinybusy::show_modal_spinner(text=state[["MC"]][["labels"]][["busy"]][["saving_state"]])
    }
  }


  FM_le(state, paste0("writing app state to file on server: "))
  FM_le(state, paste0("  ", file))

  # User directory where uploaded files, logs, etc will be stored
  user_dir = FM_fetch_user_files_path(state)

  # Pulling out the app state
  app_state = FM_fetch_app_state(session)

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

  # Writing the app state object to a file:
  saveRDS(app_state, file.path(user_dir, "fmas.rds"))


  if((any(c("ShinySession", "session_proxy") %in% class(session)))){
    if(system.file(package = "shinybusy") !=""){
      shinybusy::remove_modal_spinner()
    }
  }

  # Zipping everything up into an archive
  zip::zip(zipfile=file,
           files=dir(user_dir),
           recurse=TRUE,
           root = user_dir,
           include_directories=TRUE)
  FM_le(state, "done writing app state")
  NULL}

#'@export
#'@title Fetch Module Code
#'@description Fetches the code to generate results seen in the app
#'@param state ASM state from \code{ASM_fetch_state()}
#'@return The ASM module does not generate code
#'@examples
#' # Creating a state object for testing
#' sess_res = ASM_test_mksession(session=list(), full_session=FALSE)
#' state = sess_res$state
#' code = ASM_fetch_code(state)
ASM_fetch_code = function(state){

  code = NULL

code}

#'@export
#'@title Populate Session Data for Module Testing
#'@description Populates the supplied session variable for testing.
#'@param session Shiny session variable (in app) or a list (outside of app)
#'@param id An ID string that corresponds with the ID used to call the modules UI elements
#'@param id_UD An ID string that corresponds with the ID used to call the UD modules UI elements
#'@param id_DW An ID string that corresponds with the ID used to call the DW modules UI elements
#'@param full_session  Boolean to indicate if the full test session should be created (default \code{TRUE}).
#'@return list with the following elements
#' \itemize{
#'   \item{isgood:} Boolean indicating the exit status of the function.
#'   \item{session:} The value Shiny session variable (in app) or a list (outside of app) after initialization.
#'   \item{input:} The value of the shiny input at the end of the session initialization.
#'   \item{state:} App state.
#'   \item{rsc:} The \code{react_state} components.
#'}
#'@examples
#' sess_res = ASM_test_mksession(session=list(), full_session=FALSE)
ASM_test_mksession = function(session, id="ASM", id_UD="UD", id_DW = "DW", full_session=TRUE){

  isgood = TRUE
  rsc    = list()
  input  = list()

  # Populating the session with DW components
  #sess_res = FG_test_mksession(session, id=id_FG, id_UD = id_UD, id_DW=id_DW, full_session=full_session)
  sess_res = DW_test_mksession(session, id=id_DW, id_UD=id_UD)
  if(!("ShinySession" %in% class(session))){
    session = sess_res[["session"]]
  }

  # Pulling out the react state components
  rsc         = sess_res$rsc
  react_state = rsc

  # YAML files for the fetch calls below
  FM_yaml_file  = system.file(package = "formods", "templates", "formods.yaml")
  MOD_yaml_file = system.file(package = "formods", "templates", "ASM.yaml")

  # empty input
  input = list()

  # Creating an empty state object
  state = ASM_fetch_state(id           = id,
                          input        = input,
                          session      = session,
                          FM_yaml_file = FM_yaml_file,
                          MOD_yaml_file = MOD_yaml_file)

  # This functions works both in a shiny app and outside of one
  # if we're in a shiny app then the 'session' then the class of
  # session will be a ShinySession. Otherwise it'll be a list if
  # we're not in the app (ie just running test examples) then
  # we need to set the state manually
  if(("ShinySession" %in% class(session))){
    FM_set_mod_state(session, id, state)
  } else {
    session = FM_set_mod_state(session, id, state)
  }

  # Required for proper reaction:
  rsc[[id]]  = list(ASM = list(checksum=state[["ASM"]][["checksum"]]))

  # Defaults to not generating the reports and only the code when saving:
  state[["ASM"]][["ui"]][["switch_gen_rpts"]] = FALSE

  res = list(
    isgood  = isgood,
    session = session,
    input   = input,
    state   = state,
    rsc     = rsc
  )
res}
