#'@import rhandsontable
#'@import shiny
#'@importFrom digest digest
#'@importFrom shinyAce aceEditor updateAceEditor
#'@importFrom zip unzip zip

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
                              react_state  = react_state,
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
                              react_state  = react_state,
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
                              react_state  = react_state,
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
    output$ASM_ui_workflows     =  renderUI({
      uiele = tagList(
        htmlOutput(NS(id, "ui_asm_select_workflow")),
        htmlOutput(NS(id, "ui_asm_workflow_check_res")),
        htmlOutput(NS(id, "ui_asm_workflow_run_res")),
        verbatimTextOutput(NS(id, "ui_asm_msg_wf")))

      uiele})
    #------------------------------------
    # Workflow form elements
    output$ui_asm_select_workflow     =  renderUI({
      state = ASM_fetch_state(id           = id,
                              input        = input,
                              session      = session,
                              react_state  = react_state,
                              FM_yaml_file = FM_yaml_file,
                              MOD_yaml_file = MOD_yaml_file)
      wf = state[["yaml"]][["FM"]][["workflows"]]

      uiele    = NULL
      WF_FOUND = FALSE


      # place holders for selection elements
      groups          = c() # List of all groups found in the workflows
      values          = c() # Workflow names from formods yaml file
      desc            = c() # Verbose description of workflow names show to the user
      choices_simple  = c()
      choices_group   = c()

      if(length(names(wf)) > 0){
        for(wfn in names(wf)){
          # Checking for the existence of the preload file. We only
          # show preload options that actually exist
          plf= render_str(wf[[wfn]][["preload"]])
          if(file.exists(plf)){
            # We found at least one workflow
            WF_FOUND = TRUE

            # These are the relevant components needed to construct the UI
            # elements:
            groups = c(groups, wf[[wfn]][["group"]])
            #values = c(values, wfn)
            #desc   = c(desc  , wf[[wfn]][["desc"]])
            choices_simple = c(choices_simple, eval(parse(text=paste0('c("',wf[[wfn]][["desc"]], '"=wfn)'))))
            choices_group[[  wf[[wfn]][["group"]] ]] = c(choices_group[[  wf[[wfn]][["group"]] ]], eval(parse(text=paste0('c("',wf[[wfn]][["desc"]], '"=wfn)'))))
          } else {
            FM_le(state, paste0("preload file not found: ", plf), entry_type="warning")
          }
        }
      }

      # If no workflow elements are found we return a no workflows found
      # message
      if(WF_FOUND){

        liveSearch = FALSE
        if(state[["MC"]][["formatting"]][["workflow"]][["liveSearch"]]){
          if(length(choices_simple) >state[["MC"]][["formatting"]][["workflow"]][["size"]]){
            liveSearch = TRUE
          }
        }

        po = shinyWidgets::pickerOptions(
            liveSearch =liveSearch,
            size       =state[["MC"]][["formatting"]][["workflow"]][["size"]])

        if(length(groups) <2){
          choices = choices_simple
        } else {
          choices = choices_group
        }

        uiele_btn =
          shinyWidgets::actionBttn(
                  inputId = NS(id, "btn_run_wf_sys"),
                  label   = state[["MC"]][["labels"]][["run_wf_sys"]],
                  style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
                  size    = state[["MC"]][["formatting"]][["btn_run_wf_sys"]][["size"]],
                  block   = state[["MC"]][["formatting"]][["btn_run_wf_sys"]][["block"]],
                  color   = "primary",
                  icon    = icon("play"))
        uiele_btn =
          div(style=paste0("width:",state[["MC"]][["formatting"]][["btn_run_wf_sys"]][["width"]]),uiele_btn)

        uiele_select =
            pickerInput(
               inputId = NS(id, "workflow"),
               label = state[["MC"]][["labels"]][["workflow"]],
               choices = choices,
               options = po,
               width      = state[["MC"]][["formatting"]][["workflow"]][["width"]]
              )
        uiele_select =
          div(style=paste0("width:",state[["MC"]][["formatting"]][["workflow"]][["width"]]),uiele_select)

        uiele_select = FM_add_ui_tooltip(state, uiele_select,
                 tooltip     = state[["MC"]][["formatting"]][["btn_run_wf_sys"]][["tooltip"]],
                 position    = state[["MC"]][["formatting"]][["btn_run_wf_sys"]][["tooltip_position"]])

        uiele =
          tagList(uiele_select, uiele_btn)
      } else {
        uiele = state[["MC"]][["errors"]][["no_workflows_found"]]
      }
      uiele})
    #------------------------------------
    # Workflow form elements: check results
    output$ui_asm_workflow_check_res     =  renderUI({
      req(input$workflow)
      input$btn_run_wf_sys
      input$btn_chk_wf_sys

      state = ASM_fetch_state(id           = id,
                              input        = input,
                              session      = session,
                              react_state  = react_state,
                              FM_yaml_file = FM_yaml_file,
                              MOD_yaml_file = MOD_yaml_file)


      uiele = NULL
      wfn = state[["ASM"]][["ui"]][["workflow"]]
      wfl = state[["yaml"]][["FM"]][["workflows"]][[wfn]]
      if(!is.null(wfl)){

        # Getting the preload list for the selected workflow
        # Preload list:
        plf = render_str(wfl[["preload"]])
        pll = FM_read_yaml(plf)

        uiele_chk_btn =
          shinyWidgets::actionBttn(
                  inputId = NS(id, "btn_chk_wf_sys"),
                  label   = state[["MC"]][["labels"]][["chk_wf_sys"]],
                  style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
                  size    = state[["MC"]][["formatting"]][["btn_chk_wf_sys"]][["size"]],
                  block   = state[["MC"]][["formatting"]][["btn_chk_wf_sys"]][["block"]],
                  color   = "success",
                  icon    = icon("circle-check"))


        wfc_res = ASM_check_workflow(state=state, session=session, pll=pll)

        uiele = tagList(tags$br(),
         tags$b(state[["MC"]][["formatting"]][["workflow"]][["check_label"]]),
         tags$br(),
         wfc_res[["chk_msgs"]],
         tags$br(),
         uiele_chk_btn
        )

      }
    uiele})
    #------------------------------------
    # Workflow form elements: run results
    output$ui_asm_workflow_run_res     =  renderUI({
      req(input$workflow)
      req(input$btn_run_wf_sys)
      state = ASM_fetch_state(id           = id,
                              input        = input,
                              session      = session,
                              react_state  = react_state,
                              FM_yaml_file = FM_yaml_file,
                              MOD_yaml_file = MOD_yaml_file)


      uiele = NULL
      if(!is.null(state[["ASM"]][["rwf_res"]][["msgs"]])){
        #message("a")
        uiele = 
          tagList(tags$br(),
                  tags$b(state[["MC"]][["formatting"]][["workflow"]][["run_label"]]),
                  tags$br())
      }
    uiele})
    #------------------------------------
    output$ui_asm_switch_gen_rpts = renderUI({
      state = ASM_fetch_state(id           = id,
                              input        = input,
                              session      = session,
                              react_state  = react_state,
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
                                react_state  = react_state,
                                FM_yaml_file = FM_yaml_file,
                                MOD_yaml_file = MOD_yaml_file)
        dlfn = ASM_fetch_dlfn(state)
        FM_le(state, paste0("pushing app state download: ", dlfn))
        dlfn},
      content = function(file) {
        state = ASM_fetch_state(id           = id,
                                input        = input,
                                session      = session,
                                react_state  = react_state,
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
                              react_state  = react_state,
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
                                react_state  = react_state,
                                FM_yaml_file = FM_yaml_file,
                                MOD_yaml_file = MOD_yaml_file)
       dlfn = ASM_fetch_dlfn(state, ".pptx")
       FM_le(state, paste0("pushing report: ", dlfn))
       dlfn},
      content = function(file) {
        state = ASM_fetch_state(id           = id,
                                input        = input,
                                session      = session,
                                react_state  = react_state,
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
                                react_state  = react_state,
                                FM_yaml_file = FM_yaml_file,
                                MOD_yaml_file = MOD_yaml_file)
       dlfn = ASM_fetch_dlfn(state, ".docx")
       FM_le(state, paste0("pushing report: ", dlfn))
       dlfn},
      content = function(file) {
        state = ASM_fetch_state(id           = id,
                                input        = input,
                                session      = session,
                                react_state  = react_state,
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
                                react_state  = react_state,
                                FM_yaml_file = FM_yaml_file,
                                MOD_yaml_file = MOD_yaml_file)
       dlfn = ASM_fetch_dlfn(state, ".xlsx")
       FM_le(state, paste0("pushing report: ", dlfn))
       dlfn},
      content = function(file) {
        state = ASM_fetch_state(id           = id,
                                input        = input,
                                session      = session,
                                react_state  = react_state,
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
      input[["btn_run_wf_sys"]]
      toMessage$message
      state = ASM_fetch_state(id           = id,
                              input        = input,
                              session      = session,
                              react_state  = react_state,
                              FM_yaml_file = FM_yaml_file,
                              MOD_yaml_file = MOD_yaml_file)

      uiele = state[["ASM"]][["ui_msg"]]

      uiele})
    output$ui_asm_msg_wf = renderText({
      input[["button_state_save"]]
      input[["input_load_state"]]
      input[["btn_run_wf_sys"]]
      toMessage$message
      state = ASM_fetch_state(id           = id,
                              input        = input,
                              session      = session,
                              react_state  = react_state,
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
                              react_state  = react_state,
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
                              react_state  = react_state,
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
                              react_state  = react_state,
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
                              react_state  = react_state,
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
                              react_state  = react_state,
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
                              react_state  = react_state,
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
                              react_state  = react_state,
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
                            react_state  = react_state,
                            FM_yaml_file = FM_yaml_file,
                            MOD_yaml_file = MOD_yaml_file)
    log_file = FM_fetch_log_path(state)
    fileReaderData <- shiny::reactiveFileReader(500, session, log_file, readLines)
    # Shows the rolling log
    output$ui_asm_sys_log  = renderText({
      state = ASM_fetch_state(id           = id,
                              input        = input,
                              session      = session,
                              react_state  = react_state,
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
                                react_state  = react_state,
                                FM_yaml_file = FM_yaml_file,
                                MOD_yaml_file = MOD_yaml_file)

        FM_le(state, "reaction state updated")
        react_state[[id]] = state
      })
    }

  #------------------------------------
  toNotify <- reactive({
    list(input$input_load_state,
         input$btn_run_wf_sys)
  })
  observeEvent(toNotify(), {
    state = ASM_fetch_state(id           = id,
                            input        = input,
                            session      = session,
                            react_state  = react_state,
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
#'@param react_state Variable passed to server to allow reaction outside of
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
#'                         react_state  = react_state,
#'                         FM_yaml_file = FM_yaml_file,
#'                         MOD_yaml_file = MOD_yaml_file)
#'
#' state
ASM_fetch_state = function(id, input, session, FM_yaml_file, MOD_yaml_file, react_state){

  # Poupulated below whenever the app loads a save state or runs a workflow
  init_react_state = list()
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

        # Pulling out the initilized reaction state to return it below
        init_react_state = ls_res[["react_state"]]

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
  # Running workflows

  change_detected =
    has_updated(ui_val   = state[["ASM"]][["ui"]][["btn_run_wf_sys"]],
                old_val  = state[["ASM"]][["button_counters"]][["btn_run_wf_sys"]],
                init_val = c("", "0"))
  if(change_detected){

    FM_le(state, "running system workflow")
    rwf_isgood = TRUE
    wfn = state[["ASM"]][["ui"]][["workflow"]]
    wfl = state[["yaml"]][["FM"]][["workflows"]][[wfn]]

    FM_pause_screen(state   = state,
                    message = state[["MC"]][["labels"]][["busy"]][["rwf"]],
                    session = session)

    state = ASM_run_workflow(state=state, session=session, wfl=wfl)

    # Pulling out the initilized reaction state to return it below
    init_react_state = state[["ASM"]][["rwf_res"]][["react_state"]]

    FM_resume_screen(state   = state,
                     session = session)

    if(state[["ASM"]][["rwf_res"]][["isgood"]]){
      state = FM_set_notification(state,
        notify_text =  state[["MC"]][["labels"]][["rwf_success"]],
        notify_id   = "rwf_result",
        type        = "success")
    } else {
      state = FM_set_notification(state,
        notify_text =  state[["MC"]][["errors"]][["rwf_failed"]],
        notify_id   = "rwf_result",
        type        = "failure")
    }

    # Updating the old value to prevent further reactions:
    state[["ASM"]][["button_counters"]][["btn_run_wf_sys"]] = state[["ASM"]][["ui"]][["btn_run_wf_sys"]]
  }


  #---------------------------------------------
  # Saving the state
  FM_set_mod_state(session, id, state)

  # If we've loaded a saved state or run a workflow then we need to syncroize
  # the react_state for the app with the values populated by running the preload
  # functions.
  if(length(names(init_react_state))>0){
    FM_le(state, "initializing react_state")
    for(tmp_mod_id in names(init_react_state)){
      react_state[[tmp_mod_id]] = init_react_state[[tmp_mod_id]]
    }
  }

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
   "button_rpt_pptx",
   "btn_run_wf_sys"
  )

  ui_ids          = c(
   "button_state_save",
   "button_rpt_xlsx",
   "button_rpt_docx",
   "button_rpt_pptx",
   "ui_asm_save_name",
   "btn_run_wf_sys",
   "workflow",
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
                          react_state   = react_state,
                          FM_yaml_file  = FM_yaml_file,
                          MOD_yaml_file = MOD_yaml_file)

  if(!formods::is_shiny(session)){
    session = FM_set_mod_state(session, mod_ID, state)
  }

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
#'@param update_modal Logical controlling updates to modal messages (\code{TRUE})
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
ASM_save_state = function(state, session, file_path, pll = NULL, update_modal=TRUE){

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
      if(formods::is_installed("shinybusy") & update_modal){
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
#'@title Load App State
#'@description Used to load the saved app state from a zip file
#'@param state ASM state from \code{ASM_fetch_state()}
#'@param session Shiny session variable
#'@param file_path Zip file with the saved sate
#'@return This function overwrites the current app state and returns a list with the
#'following elements:
#' \itemize{
#'   \item{isgood:}      Boolean indicating the exit status of the function.
#'   \item{msgs:}        Messages to be passed back to the user.
#'   \item{state:}       AMS state
#'   \item{session:}     Session object
#'   \item{react_state:} Reaction state initilized after loading
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

  # Pulling out the react_state variables from each module
  react_state = list()
  for(tmp_id in names(res[["all_sess_res"]])){
    for(tmp_id_2 in names(res[["all_sess_res"]][[tmp_id]][["react_state"]])){
      react_state[[tmp_id_2]] = res[["all_sess_res"]][[tmp_id]][["react_state"]][[tmp_id_2]]
    }
  }

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
  res = list(isgood      = isgood,
             state       = state,
             session     = session,
             react_state = react_state,
             msgs        = msgs)
  res}

#'@export
#'@title Run Specified Workflow
#'@description Called from download handler and used to write a saved state
#'value if that is null
#'@param state ASM state from \code{ASM_fetch_state()}
#'@param session Shiny session variable
#'@param wfl   List contining details about the workflow
#'@return The ASM state object with the results stored as a list in the field \code{rwf_res}
#'following elements:
#' \itemize{
#'   \item{isgood:}      Boolean indicating the exit status of the function.
#'   \item{msgs:}        Messages to be passed back to the user.
#'   \item{react_state:} Reaction state initilized after loading
#' }
#'@examples
#' # Populating the session with data
#' session = list()
#' ds_plf = c(system.file(package="formods", "preload", "ASM_preload_empty.yaml"),
#'            system.file(package="formods", "preload", "UD_preload.yaml"),
#'            system.file(package="formods", "preload", "DM_preload_empty.yaml"))
#' session = list()
#' res_ds = FM_app_preload(session=session, sources=ds_plf)
#'
#' session = res_ds[["session"]]
#' state   = res_ds[["all_sess_res"]][["ASM"]][["state"]]
#'
#' # Creating the workflow preload list
#' wf_pll = c(
#'   FM_read_yaml( system.file(package="formods", "preload", "ASM_preload.yaml")),
#'   FM_read_yaml( system.file(package="formods", "preload", "UD_preload.yaml")),
#'   FM_read_yaml( system.file(package="formods", "preload", "DM_preload_empty.yaml")),
#'   FM_read_yaml( system.file(package="formods", "preload", "DW_preload_empty.yaml")))
#'
#' tmp_preload = tempfile(fileext=".yaml")
#' yaml::write_yaml(file=tmp_preload, x=wf_pll)
#'
#' wfl = list(
#'   require_ds = TRUE,
#'   preload   = tmp_preload
#' )
#'
#' # Running the workflow
#' state = ASM_run_workflow(state=state, session=session, wfl=wfl)
#'
#' state$ASM$rwf_res
ASM_run_workflow = function(state, session, wfl){
  isgood      = TRUE
  msgs        = c()
  react_state = list()

    fds_res = FM_fetch_ds(state=state, session=session, meta_only=TRUE)

    # JMH add checks for DW and resource labels
    if(wfl[["require_ds"]] & !fds_res[["hasds"]]){
      # To run this workflow a dataset is required but one has not been
      # uploaded.
      msgs  = c(msgs, state[["MC"]][["errors"]][["no_ds_for_workflow"]])
      isgood = FALSE
    } else {
      FM_le(state, paste0("Running workflow: ", wfl[["desc"]]))
      # Preload file
      plf = render_str(wfl[["preload"]])

      # Preload list:
      pll = FM_read_yaml(plf)

      # Walking trough each module we need to preserve. For example if we're running a workflow
      # against the loaded datasets then we need to create preload lists for
      # each of those modules based on the IDs specified in the ASM module yaml file.
      for(tmp_mod_ID in names(state[["MC"]][["module"]][["workflow"]])){
        tmp_state = FM_fetch_mod_state(id=tmp_mod_ID, session=session)

        # Making a preload list for the current module
        cmd = paste0("res_mpl =  ", tmp_state[["MOD_TYPE"]],"_mk_preload(state=tmp_state)")
        tcres =
          FM_tc(capture="res_mpl",
                cmd = cmd,
                tc_env = list(tmp_state=tmp_state))

        if(tcres[["isgood"]]){
          # The results of the mk_preload command above
          res_mpl = tcres[["capture"]][["res_mpl"]]

          # Removing the source module from the preload list from the workflow
          pll[[ tmp_state[["id"]] ]] = NULL

          # Appending the current state:
          pll = c(res_mpl[["yaml_list"]], pll)

        } else {
          isgood = FALSE
          msgs = c(msgs, tcres[["msgs"]])
        }
      }

      # We only proceed if the preload list was created
      if(isgood){
        # Because preload files from saved analyses can have relative
        # paths to configuration yaml files in them, we need replace those
        # With the paths to those files used in the current app
        for(tmp_modID in names(pll)){
          tmp_state = FM_fetch_mod_state(session, tmp_modID)
          if(!is.null(tmp_state)){
            pll[[tmp_modID]][["fm_yaml"]]  = tmp_state[["FM_yaml_file"]]
            pll[[tmp_modID]][["mod_yaml"]] = tmp_state[["MOD_yaml_file"]]
          } else {
            tmp_msg = paste0("Module with ID: ", tmp_modID, " found in workflow but not in app. ")
            FM_le(state, tmp_msg, entry_type="error")
          }
        }

        # Writing the new workflow yaml list to a save file:
        ssf  = tempfile(fileext=".zip")
        ss_res = ASM_save_state(state=state, session=session, file_path=ssf, pll=pll, update_modal=FALSE)
        if(ss_res[["isgood"]]){

          ls_res =
              ASM_load_state(state, session,
                             file_path = ssf)

          state       = ls_res[["state"]]
          # Here we're pulling any react_state values out from the load
          react_state = ls_res[["react_state"]]

          if(!ls_res[["isgood"]]){
            msgs =  c(msgs, state[["MC"]][["errors"]][["ls_failed"]], ls_res[["msgs"]])
            isgood = FALSE
          }

        } else {
          msgs =  c(msgs, state[["MC"]][["errors"]][["ss_failed"]], ss_res[["msgs"]])
          isgood = FALSE
        }
      }
    }

  state[["ASM"]][["rwf_res"]] = list(
    isgood      = isgood,
    react_state = react_state,
    msgs        = msgs
  )
state}

#'@export
#'@title Checks Workflow Preload List Against Current App State
#'@description Compares the resource dependencies in a workflow preload list to
#' those currently avialble in the state to determine if the workflow can be run.
#'@param state ASM state from \code{ASM_fetch_state()}
#'@param session Shiny session variable
#'@param pll   Workflow preload list.
#'@return A list with the following attributes
#'following elements:
#' \itemize{
#'   \item{isgood:}      Boolean indicating the exit status of the function.
#'   \item{msgs:}        Messages to be passed back to the user.
#'   \item{chk_msgs:}    Results of check, this can contain \code{"tags()"}
#'   \item{deps_found:}  Boolean indicating if all resoures were found.
#'   \item{dep_table:}   Table of dependency information with the following columns:
#'   \itemize{
#'     \item{mod_ID:}      The formods module ID of the module that needs the resource.
#'     \item{dep_type :}   Type of resource dependency currently only \code{"ds"} for data source.
#'     \item{id       :}   The formods module ID supplying the resource.
#'     \item{res_label:}   The resource label.
#'     \item{res_found:}   Logical indicating if the resource was found or not.
#'   }
#' }
#'@examples
#' # The ASM session won't have any components so the check below should fail
#' sess_res = ASM_test_mksession()
#' session = sess_res[["session"]]
#' state = FM_fetch_mod_state(id="ASM", session=session)
#'
#' # The DW test merge should require resource labels that are not currently present
#' pll = formods::FM_read_yaml(system.file(package="formods", "preload", "workflow_DW_merge.yaml"))
#'
#' cwf_res = ASM_check_workflow(state=state, session=session, pll=pll)
#'
#' cwf_res
ASM_check_workflow = function(state, session, pll){
  isgood     = TRUE
  msgs       = c()
  deps_found = TRUE

  # Defaulting to no resources
  chk_msgs = state[["MC"]][["formatting"]][["workflow"]][["chk_msgs"]][["no_res"]]

  #pll = formods::FM_read_yaml(system.file(package="formods", "preload", "DW_test_merge.yaml"))

  # Finding resource dependencies
  res_deps = NULL
  for(tmp_mod_ID in names(pll)){
    for(tmp_dep_type in names(pll[[tmp_mod_ID]][["res_deps"]])){
      for(tmp_res_mod_ID in names(pll[[tmp_mod_ID]][["res_deps"]][[tmp_dep_type]])){

        pll[[tmp_mod_ID]][["res_deps"]][[tmp_dep_type]][[tmp_res_mod_ID]]

        res_deps = rbind(res_deps,
          data.frame(mod_ID      = tmp_mod_ID,
                     dep_type    = tmp_dep_type,
                     id          = tmp_res_mod_ID,
                     res_label   = pll[[tmp_mod_ID]][["res_deps"]][[tmp_dep_type]][[tmp_res_mod_ID]],
                     res_found   = FALSE)
        )
      }
    }
  }

  if(!is.null(res_deps)){
    # Getting a catalog of the ds resources avialble in the app:
    fds_res = FM_fetch_ds(state=state, session=session, meta_only=TRUE)
    for(ridx in 1:nrow(res_deps)){
      # Right now we only have dataset dependencies but this could expand in the future:
      if(res_deps[ridx, ][["dep_type"]] == "ds"){
        fr_res =
          fetch_resource(
            catalog   = fds_res[["catalog"]],
            id        = res_deps[ridx, ][["id"]],
            res_label = res_deps[ridx, ][["res_label"]])

        # If the resource exists we flag it as found
        if(fr_res[["isgood"]]){
          res_deps[ridx, ][["res_found"]] = TRUE
        } else {
          deps_found = FALSE
        }
      }
    }

    if(all(res_deps[["res_found"]])){
      chk_msgs = state[["MC"]][["formatting"]][["workflow"]][["chk_msgs"]][["all_found"]]
    } else {
      chk_msgs = state[["MC"]][["formatting"]][["workflow"]][["chk_msgs"]][["missing"]]
      mdeps                 = res_deps[!res_deps[["res_found"]], ]
      #mdeps[["res_label"]]  = paste0("tags$b(", mdeps[["res_label"]],")")
      mdeps_str             = paste0(mdeps[["res_label"]], collapse=", ")
      chk_msgs              = stringr::str_replace(patter="===MISSING===", string=chk_msgs, replacement=mdeps_str)
    }
  }

  # Rendering any code in the messages
  chk_msgs = formods::render_str(chk_msgs)

  res = list(
    isgood     = isgood,
    msgs       = msgs,
    chk_msgs   = chk_msgs,
    deps_found = deps_found,
    dep_table  = res_deps)
res}


#  #'@export
#  #'@title Test Specified Workflow
#  #'@description This will take a preload file for loading data and a separate preload file for a workflow and test whether it can be run.
#  #'@param ds_plf Vector of preload yaml list files containing data loading
#  #'@param wf_plf Vector of preload yaml list files containing workflows
#  #'@return List with the following elements:
#  #' \itemize{
#  #'   \item{isgood:}      Boolean indicating the exit status of the function.
#  #'   \item{msgs:}        Messages to be passed back to the user.
#  #' }
#  #'@examples
#  #'  ds_plf = c(system.file(package="formods", "preload", "ASM_preload.yaml"),
#  #'             system.file(package="formods", "preload", "UD_preload.yaml"),
#  #'             system.file(package="formods", "preload", "DM_preload_empty.yaml"))
#  #'
#  #'  wf_plf = c(system.file(package="formods", "preload", "ASM_preload.yaml"),
#  #'             system.file(package="formods", "preload", "UD_preload.yaml"),
#  #'             system.file(package="formods", "preload", "DM_preload_empty.yaml"),
#  #'             system.file(package="formods", "preload", "DW_preload.yaml"))
#  #'
#  #'  res = ASM_test_workflow(ds_plf=ds_plf, wf_plf=wf_plf)
#  ASM_test_workflow = function(ds_plf, wf_plf){
#
#    isgood = TRUE
#    msgs   = c()
#
#    # Building the session with the datasets attached
#    session = list()
#    res_ds = FM_app_preload(session=session, sources=ds_plf)
#
#
#    if(res_ds[["isgood"]]){
#      # Session with the ds_plf populated
#      session = res_ds[["session"]]
#
#      # ASM state object
#      state = res_ds[["all_sess_res"]][["ASM"]][["state"]]
#
#      # turning preload files (plf) into a single preload lists (pll)
#      wf_pll = list()
#      for(tmp_wf_plf in wf_plf){
#         wf_pll = c(wf_pll, formods::FM_read_yaml(tmp_wf_plf))
#      }
#
#      # Some basic testing of the workflow
#      cwf_res = ASM_check_workflow(state=state, session=session, pll = wf_pll)
#      if(cwf_res[["isgood"]]){
#
#        # The run_workflow function takes in a yaml file so we need to write the wf_pll out to a file
#        tmp_preload = tempfile(fileext=".yaml")
#        yaml::write_yaml(file=tmp_preload, x=wf_pll)
#
#        wfl = list(
#          require_ds = TRUE,
#          preload   = tmp_preload
#        )
#
#        state = ASM_run_workflow(state=state, session=session, wfl=wfl)
#
#        # This tests to see if the preload failed:
#        if(!state[["ASM"]][["rwf_res"]][["isgood"]]){
#          isgood = FALSE
#          if(!is.null(state[["ASM"]][["rwf_res"]][["isgood"]])){
#            msgs = c(msgs,
#              "workflow run failed (ASM_run_workflow()), see messages below for details",
#              state[["ASM"]][["rwf_res"]][["isgood"]])
#          } else {
#            msgs = c(msgs, "workflow check failed (ASM_run_workflow())")
#          }
#        }
#      } else {
#        if(!is.null(cwf_res[["msgs"]])){
#          msgs = c(msgs, "workflow check failed (ASM_check_workflow()), see messages below for details", cwf_res[["msgs"]])
#        } else {
#          msgs = c(msgs, "workflow check failed (ASM_check_workflow())")
#        }
#      }
#
#    } else {
#      isgood = FALSE
#      if(!is.null(res_ds[["msgs"]])){
#        msgs = c(msgs, "ds preload failed, see messages below for details", res_ds[["msgs"]])
#      } else {
#        msgs = c(msgs, "ds preload failed")
#      }
#    }
#
#    res = list(
#      isgood    = isgood,
#      msgs      = msgs)
#  res}

#'@export
#'@title Test Specified Preload Functionality
#'@description
#' Loads specified preload lists to verify execution, saves the state to the
#' specified zip file and attempts to load the saved state if
#' \code{test_save_state} is \code{TRUE}. To test a workflow you can use
#' \code{sources} to specify the loading of data and then \code{workflow} to
#' specify the yaml workflow.
#'@param sources     Vector of at corresponds with the ID used to call the modules UI elements
#'@param preload_files   Dataframe of files needed to run the workflow with a
#'column called \code{src} for the local source of the file and \code{dest} for the
#'destination. Not strictly required but almost always needed.
#'@param preload_dir     Directory to run out of (\code{tempdir()})
#'@param save_state_file When testing `ASM_save_state()` this is the file name where the stat will be written (\code{tempfile(fileext=".zip")})
#'@param test_save_state  Logical when set to \code{TRUE} (default) it will test the zip file (\code{save_state_file}) to make sure it can be loaded.
#'@param workflow        Yaml preload file to run a workflow (optional, \code{NULL} default).
#'@return list with the following elements
#' \itemize{
#'   \item{isgood:}          Boolean indicating the exit status of the function.
#'   \item{msgs:}            Messages to be passed back to the user.
#'   \item{apl_res:}         Results from FM_app_preload() if run,   \code{list(isgood=FALSE, msgs="not run")} otherwise.
#'   \item{ss_res:}          Results from ASM_save_state() if run,   \code{list(isgood=FALSE, msgs="not run")} otherwise.
#'   \item{ls_res:}          Results from ASM_load_state() if run,   \code{list(isgood=FALSE, msgs="not run")} otherwise.
#'   \item{rwf_res:}         Results from ASM_run_workflow() if run, \code{list(isgood=FALSE, msgs="not run")} otherwise.
#'   \item{save_state_file:} If ss_res$isgood is TRUE this is the path to the zip file generated when saving the app state after loading.
#'}
#'@examples
#'
#'sources = c(
#'  system.file(package="formods", "preload", "UD_preload.yaml"),
#'  system.file(package="formods", "preload", "ASM_preload.yaml"))
#'
#'res = ASM_test_preload(sources=sources, test_save_state = FALSE)
#'
#'res$isgood
ASM_test_preload = function(
  sources         = NULL,
  preload_files   = NULL,
  preload_dir     = tempfile(pattern="preload_"),
  save_state_file = tempfile(fileext=".zip"),
  test_save_state = TRUE,
  workflow        = NULL){


  isgood          = TRUE
  msgs            = c()

  # The yaml files in sources will be read in and put here:
  preload_yaml_fn = file.path(preload_dir, "preload.yaml")

  # This is a placeholder for the FM_app_preload() function
  # results. By  default its isgood state is FALSE in case we
  # cannot run it for some reason below
  apl_res         = list(isgood=FALSE, msgs=c("not run"))

  # This is a placeholder for the ASM_save_state() function
  # results. By  default its isgood state is FALSE in case we
  # cannot run it for some reason below
  ss_res         = list(isgood=FALSE, msgs=c("not run"))

  # This is a placeholder for the ASM_load_state() function
  # results. By  default its isgood state is FALSE in case we
  # cannot run it for some reason below
  ls_res         = list(isgood=FALSE, msgs=c("not run"))

  # This is a placeholder for the results from the
  # ASM_run_workflow() function. By  default its isgood
  # state is FALSE in case we cannot run it for
  # some reason below
  rwf_res         = list(isgood=FALSE, msgs=c("not run"))


  # Creating preload directory tree:
  if(dir.exists(preload_dir)){
    unlink(preload_dir, recursive = TRUE)
  }
  dir.create(preload_dir)

  # Here we're going to start working out of preload_dir
  old_wd=getwd()
  setwd(preload_dir)
  on.exit( setwd(old_wd))

  dir.create(file.path(preload_dir, "config"), recursive=TRUE)
  dir.create(file.path(preload_dir, "data", "DM"), recursive=TRUE)

  yaml_list = list()

  # processing sources
  if(!is.null(sources)>0){
    for(yaml_src in sources){
      if(file.exists(yaml_src)){
        yaml_list = c(yaml_list, FM_read_yaml(yaml_src))
      } else {
        isgood = FALSE
        msgs = c(msgs, paste0("yaml source not found: ", yaml_src))
      }
    }

    if(isgood){
      # Saving the sources to a single preload file:
      yaml::write_yaml(yaml_list, preload_yaml_fn)
    }
  } else {
    isgood = FALSE
    msgs   = c(msgs, "sources was not specified")
  }

  # Copying the files locally
  if(!is.null(preload_files)>0){
    if(all(c("src", "dest") %in% names(preload_files))){
      for(ridx in 1:nrow(preload_files)){
        tmp_src  = preload_files[["src"]][ridx]
        tmp_dest = preload_files[["dest"]][ridx]
        if(file.exists(tmp_src)){
          if(!file.copy(from=tmp_src, to=tmp_dest, overwrite=TRUE)){
            isgood=FALSE
            msgs = c(msgs, paste0("unable to copy file:"))
            msgs = c(msgs, paste0(" -> src:  ", tmp_src))
            msgs = c(msgs, paste0(" -> dest: ", tmp_dest))
          }
        } else {
          isgood = FALSE
          msgs = c(msgs, paste0("preload_files src not found: ", tmp_src))
        }
      }
    } else {
      isgood = FALSE
      msgs = c(msgs, "preload_files must have both src and dest columns")
    }
  }

  # Preloading the app:
  if(isgood){
    apl_res = FM_app_preload(session=list(), sources=preload_yaml_fn)
    if(!apl_res[["isgood"]]){
      isgood = FALSE
      msgs = c(msgs, "FM_app_preload() failed")
      if(!is.null(apl_res[["msgs"]]) ){
        msgs = c(msgs, paste0("  ", apl_res[["msgs"]]))
      }
    }
  }

  # If the preload was good then we will save the app state
  if(apl_res[["isgood"]] & isgood){
    tmp_state   = apl_res[["all_sess_res"]][["ASM"]][["state"]]
    tmp_session = apl_res[["session"]]

    ss_res = ASM_save_state(
      state    = tmp_state,
      session  = tmp_session,
      file_path = save_state_file)

    if(!ss_res[["isgood"]]){
      isgood = FALSE
      msgs = c(msgs, "ASM_save_state() failed")
      if(!is.null(ss_res[["msgs"]]) ){
        msgs = c(msgs, paste0("  ", ss_res[["msgs"]]))
      }
    }
  }


  # If we are testing the save state then we give that a shot
  if(test_save_state & ss_res[["isgood"]] & isgood){
    tmp_state   = apl_res[["all_sess_res"]][["ASM"]][["state"]]
    tmp_session = apl_res[["session"]]

    ls_res =
      ASM_load_state(state     = tmp_state,
                     session   = tmp_session,
                     file_path = save_state_file)
    if(!ls_res[["isgood"]]){
      isgood = FALSE
      msgs = c(msgs, "ASM_save_state() failed")
      if(!is.null(ls_res[["msgs"]]) ){
        msgs = c(msgs, paste0("  ", ls_res[["msgs"]]))
      }
    }
  }


  # JMH add workflow portion here
  if(!is.null(workflow)){
    tmp_state   = apl_res[["all_sess_res"]][["ASM"]][["state"]]
    tmp_session = apl_res[["session"]]
    wfl = list(require_ds = TRUE, preload=workflow)
    tmp_state = ASM_run_workflow(
      state   = tmp_state,
      session = tmp_session,
      wfl     = wfl)

    # This will be passed back to the user
    rwf_res = tmp_state[["ASM"]][["rwf_res"]]

    # Notifying the user if there were any issues with rwf
    if(!rwf_res[["isgood"]]){
      isgood = FALSE
      msgs = c(msgs, "ASM_save_state() failed")
      if(!is.null(rwf_res[["msgs"]]) ){
        msgs = c(msgs, paste0("  ", rwf_res[["msgs"]]))
      }
    }
  }

  setwd(old_wd)

  if(!isgood){
    msgs = paste0("  ", msgs)
    msgs = c("FM_test_preload()", msgs)
    for(msg in msgs){
      FM_message(line=msg, entry_type="warning")
    }
  }

  res = list(
    isgood          = isgood,
    msgs            = msgs,
    apl_res         = apl_res,
    ss_res          = ss_res,
    ls_res          = ls_res,
    rwf_res         = rwf_res,
    save_state_file = save_state_file)

res}
