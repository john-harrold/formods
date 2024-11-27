#'@import rhandsontable
#'@import readxl
#'@import shiny
#'@importFrom digest digest
#'@importFrom readr read_csv
#'@importFrom shinyAce aceEditor updateAceEditor
#'@importFrom stats setNames
#'@importFrom stringr str_replace_all
#'@importFrom tools file_ext
#'@importFrom yaml read_yaml


#'@export
#'@title Data Upload Server
#'@description Server function for the Data Uplaod Shiny Module
#'@param id An ID string that corresponds with the ID used to call the modules UI elements
#'@param id_ASM ID string for the app state management module used to save and load app states
#'@param FM_yaml_file App configuration file with FM as main section.
#'@param MOD_yaml_file  Module configuration file with MC as main section.
#'@param deployed Boolean variable indicating whether the app is deployed or not.
#'@param react_state Variable passed to server to allow reaction outside of module (\code{NULL})
#'@return UD Server object
#'@example inst/test_apps/FM_compact.R
UD_Server <- function(id,
                      id_ASM       = "ASM",
                      FM_yaml_file  = system.file(package = "formods",
                                                  "templates",
                                                  "formods.yaml"),
                      MOD_yaml_file = system.file(package = "formods",
                                                  "templates",
                                                  "UD.yaml"),
                      deployed     = FALSE,
                      react_state  = NULL) {
  moduleServer(id, function(input, output, session) {


    #------------------------------------
    # Creates the file upload elements
    output$ui_ud_load_data = renderUI({
      state = UD_fetch_state(id            = id,
                             id_ASM        = id_ASM,
                             input         = input,
                             session       = session,
                             FM_yaml_file  = FM_yaml_file,
                             MOD_yaml_file = MOD_yaml_file)
      accept = state[["MC"]][["allowed_extensions"]]
      label  = paste0( state[["MC"]][["labels"]][["upload_button"]],
                       " (", paste(accept, collapse=", "), ")")
      uiele =
        tagList(
          fileInput(NS(id, "input_data_file"),
                    label = label,
                    multiple = FALSE,
                    accept = state[["MC"]][["allowed_extensions"]]))
      uiele})
    #------------------------------------
    # Clean switch
    output$ui_ud_clean = renderUI({
      state = UD_fetch_state(id            = id,
                             id_ASM        = id_ASM,
                             input         = input,
                             session       = session,
                             FM_yaml_file  = FM_yaml_file,
                             MOD_yaml_file = MOD_yaml_file)

      uiele = NULL
      if(state[["MC"]][["clean_data"]][["enabled"]]){
        uiele =
        shinyWidgets::materialSwitch(
           inputId = NS(id, "switch_clean"),
           label   = state[["MC"]][["labels"]][["switch_clean"]],
           value   = state[["UD"]][["clean"]],
           status  = "success"
        )

      uiele = FM_add_ui_tooltip(state, uiele,
              tooltip     = state[["MC"]][["formatting"]][["switch_clean"]][["tooltip"]],
              position    = state[["MC"]][["formatting"]][["switch_clean"]][["tooltip_position"]])

      }



      uiele})
    #------------------------------------
    # If the user has uploaded an excel file this will
    # allow them to select the sheets:
    output$ui_ud_select_sheets =  renderUI({
      # Reacting to data file changes
      input$input_data_file
      state = UD_fetch_state(id            = id,
                             id_ASM        = id_ASM,
                             input         = input,
                             session       = session,
                             FM_yaml_file  = FM_yaml_file,
                             MOD_yaml_file = MOD_yaml_file)

      if(!is.null(state[["UD"]][["data_file_ext"]]) &
         !is.null(state[["UD"]][["sheets"]])){
        uiele =
          selectInput(
            NS(id, "input_select_sheet"),
            "Select Sheet",
            choices  = state[["UD"]][["sheets"]],
            selected = state[["UD"]][["sheet"]],
            multiple = FALSE)

      } else {
        uiele = NULL
      }
      uiele})
    #------------------------------------
    # Data loading messages go here
    output$ui_ud_text_load_result  =  renderUI({
      # Reacting to data file changes
      input$input_data_file
      input$input_select_sheet
      input$btn_run_wf
      state = UD_fetch_state(id            = id,
                             id_ASM        = id_ASM,
                             input         = input,
                             session       = session,
                             FM_yaml_file  = FM_yaml_file,
                             MOD_yaml_file = MOD_yaml_file)

      if(!is.null(state[["UD"]][["load_msg"]])){
        uiele = state[["UD"]][["load_msg"]]
      } else {
        uiele = NULL
      }
      uiele})
    #------------------------------------
    # A simple preview of the data:
    output$ui_ud_data_preview  =  renderUI({
      # Forcing a reaction to changes in other modules
      react_state[[id_ASM]]
      # Reacting to data file changes
      input$input_data_file
      input$input_select_sheet
      input$btn_run_wf
      state = UD_fetch_state(id            = id,
                             id_ASM        = id_ASM,
                             input         = input,
                             session       = session,
                             FM_yaml_file  = FM_yaml_file,
                             MOD_yaml_file = MOD_yaml_file)

      if(is.data.frame(state[["UD"]][["contents"]])){
        uiele = tagList(tags$b("Dataset Preveiw"),
                        rhandsontable::rHandsontableOutput(NS(id, "hot_data_preview")))
      } else {uiele = NULL}
      uiele})
    #------------------------------------
    # Workflow form elements
    output$ui_ud_workflows     =  renderUI({
      state = UD_fetch_state(id            = id,
                             id_ASM        = id_ASM,
                             input         = input,
                             session       = session,
                             FM_yaml_file  = FM_yaml_file,
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
                  inputId = NS(id, "btn_run_wf"),
                  label   = state[["MC"]][["labels"]][["run_wf"]],
                  style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
                  size    = state[["MC"]][["formatting"]][["btn_run_wf"]][["size"]],
                  block   = state[["MC"]][["formatting"]][["btn_run_wf"]][["block"]],
                  color   = "primary",
                  icon    = icon("plus-sign", lib="glyphicon"))
        uiele_btn = 
          div(style=paste0("width:",state[["MC"]][["formatting"]][["btn_run_wf"]][["width"]]),uiele_btn)

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
                 tooltip     = state[["MC"]][["formatting"]][["btn_run_wf"]][["tooltip"]],
                 position    = state[["MC"]][["formatting"]][["btn_run_wf"]][["tooltip_position"]])

        uiele = 
          tagList(uiele_select, uiele_btn)
      } else {
        uiele = state[["MC"]][["errors"]][["no_workflows_found"]]
      }


      uiele})

    #------------------------------------
    # Generated data reading code
    observe({
      # Reacting to file changes
      input$input_data_file
      input$input_select_sheet
      state = UD_fetch_state(id            = id,
                             id_ASM        = id_ASM,
                             input         = input,
                             session       = session,
                             FM_yaml_file  = FM_yaml_file,
                             MOD_yaml_file = MOD_yaml_file)

      uiele = "# No file loaded"
      if(!is.null(state[["UD"]][["code"]])){
        if(state[["UD"]][["code"]]!=""){
          uiele = state[["UD"]][["code"]]
          # Adding the preamble to load necessary packages
          mod_deps = FM_fetch_deps(state = state, session = session)
          if("package_code" %in% names(mod_deps)){
            uiele = paste0(c(mod_deps$package_code, "", uiele), collapse="\n")
          }
        }
      }

      shinyAce::updateAceEditor(
        session         = session,
        editorId        = "ui_ud_ace_code",
        theme           = state[["yaml"]][["FM"]][["code"]][["theme"]],
        showLineNumbers = state[["yaml"]][["FM"]][["code"]][["showLineNumbers"]],
        readOnly        = state[["MC"]][["code"]][["readOnly"]],
        mode            = state[["MC"]][["code"]][["mode"]],
        value           = uiele)

    })
    #------------------------------------
    # A simple preview of the data:
    output$hot_data_preview  =  rhandsontable::renderRHandsontable({
      # Reacting to data file changes
      input$input_data_file
      input$input_select_sheet
      # Forcing a reaction to changes in other modules
      react_state[[id_ASM]]
      state = UD_fetch_state(id            = id,
                             id_ASM        = id_ASM,
                             input         = input,
                             session       = session,
                             FM_yaml_file  = FM_yaml_file,
                             MOD_yaml_file = MOD_yaml_file)

      if(is.data.frame(state[["UD"]][["contents"]])){

        # Pulling out column header formatting information.
        hfmt = FM_fetch_data_format(state[["UD"]][["contents"]], state)

        uiele = rhandsontable::rhandsontable(state[["UD"]][["contents"]],
                                             colHeaders = as.vector(unlist(hfmt[["col_heads"]])),
                                             width      = state[["MC"]][["formatting"]][["preview"]][["width"]],
                                             height     = state[["MC"]][["formatting"]][["preview"]][["height"]])
      } else {uiele=NULL}
      uiele})
    #------------------------------------
    # Creates the ui for the compact view of the module
    output$UD_ui_compact  =  renderUI({
      state = UD_fetch_state(id            = id,
                             id_ASM        = id_ASM,
                             input         = input,
                             session       = session,
                             FM_yaml_file  = FM_yaml_file,
                             MOD_yaml_file = MOD_yaml_file)

      uiele_code_button = NULL
      # Creating the code button if it's enabled
      if(state[["MC"]][["compact"]][["code"]]){
        uiele_code    = tagList(shinyAce::aceEditor(
           NS(id, "ui_ud_ace_code"),
           height  = state[["MC"]][["formatting"]][["code"]][["height"]]
           ))
        uiele_code_button = tagList(
         shinyWidgets::dropdownButton(
           uiele_code,
           inline  = TRUE,
           right   = FALSE,
           size    = "sm",
           circle  = FALSE,
           width   = state[["MC"]][["formatting"]][["code"]][["width"]],
           status  = "danger",
           icon    = icon("code", lib="font-awesome"),
           tooltip = shinyWidgets::tooltipOptions(title = state[["MC"]][["tooltips"]][["show_code"]]))
        )
      }

      uiele  = tagList(
           div(style="display:inline-block;width:100%", htmlOutput(NS(id, "ui_ud_load_data"))),
           htmlOutput(NS(id, "ui_ud_clean")),
           htmlOutput(NS(id, "ui_ud_select_sheets")),
           htmlOutput(NS(id, "ui_ud_workflows")),
           div(style="display:inline-block;vertical-align:top;width:40px", uiele_code_button),
           htmlOutput(NS(id, "ui_ud_text_load_result")))

      # Attaching the preview to the bottom if it's enabled
      if(state$MC$compact$preview){
        uiele_preview = tagList(div(style="display:inline-block;vertical-align:top",
                                    htmlOutput(NS(id, "ui_ud_data_preview"))),
                                )
        uiele = tagList(uiele, uiele_preview, tags$br())}

      uiele})
    outputOptions(output, "UD_ui_compact", priority = -1)
    #------------------------------------
    # Creating reaction if a variable has been specified
    if(!is.null(react_state)){
      # Here we list the ui inputs that will result in a state change:
      toListen <- reactive({
        list(input$input_data_file,
             input$input_select_sheet,
             react_state[[id_ASM]]) })
      # This updates the reaction state:
      observeEvent(toListen(), {
        state = UD_fetch_state(id            = id,
                               id_ASM        = id_ASM,
                               input         = input,
                               session       = session,
                               FM_yaml_file  = FM_yaml_file,
                               MOD_yaml_file = MOD_yaml_file)
        FM_le(state, "reaction state updated")
        #react_state[[id]] = state
        react_state[[id]][["UD"]][["checksum"]] = state[["UD"]][["checksum"]]
      }, priority=100)
    }

    #------------------------------------
    toNotify <- reactive({
      list(input$btn_run_wf)
    })
    observeEvent(toNotify(), {
      state = UD_fetch_state(id            = id,
                             id_ASM        = id_ASM,
                             input         = input,
                             session       = session,
                             FM_yaml_file  = FM_yaml_file,
                             MOD_yaml_file = MOD_yaml_file)

      # Triggering optional notifications
      notify_res =
      FM_notify(state = state,
       session     = session)
    })

  })
}

#'@export
#'@title Fetch Upload Data State
#'@description Merges default app options with the changes made in the UI
#'@param id Shiny module ID
#'@param id_ASM ID string for the app state management module used to save and load app states
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
#' \item{DS:} Loaded dataset with the following elements
#' \itemize{
#'   \item{isgood:} Boolean object indicating if the file was successfully loaded.
#'   \item{load_msg:} Text message indicated the success or any problems
#'   encountered when uploading the file.
#'   \item{data_file_local:} Full path to the data file on the server.
#'   \item{data_file:} Dataset file name without the path.
#'   \item{data_file_ext:} File extension of the uploaded file.
#'   \item{sheet:} If the uploaded file is an excel file, this is the
#'   currently selected sheet.
#'   \item{sheets:} If the uploaded file is an excel file, this is a character vector of the sheets present in that file.
#'   \item{contents:} Data frame containting the contents of the data file.
#'   \item{checksum:} This is an MD5 sum of the contents element and can be
#'   used to detect changes in the loaded file.
#' }
#'  \item{MOD_TYPE:} Character data containing the type of module \code{"UD"}
#'  \item{id:} Character data containing the module id module in the session variable.
#'  \item{FM_yaml_file:} App configuration file with FM as main section.
#'  \item{MOD_yaml_file:}  Module configuration file with MC as main section.
#'}
#'@examples
#' # YAML configuration files from the package:
#' FM_yaml_file  = system.file(package = "formods", "templates", "formods.yaml")
#' MOD_yaml_file = system.file(package = "formods", "templates", "UD.yaml")
#' # This is the module id:
#' id = "UD"
#' # Within shiny both session and input variables will exist,
#' # this creates examples here for testing purposes:
#' sess_res = UD_test_mksession(session=list())
#' session = sess_res$session
#' input   = sess_res$input
#' state = UD_fetch_state(
#'            id            = id,
#'            input         = input,
#'            session       = session,
#'            FM_yaml_file  = FM_yaml_file,
#'            MOD_yaml_file = MOD_yaml_file )
UD_fetch_state = function(id, id_ASM, input, session, FM_yaml_file,  MOD_yaml_file ){

  # Template for an empty dataset
  #---------------------------------------------
  # Getting the current state
  state = FM_fetch_mod_state(session, id)
  # If the state has not yet been defined then we
  # initialize it
  if(is.null(state)){
    # General state information
    state = UD_init_state(FM_yaml_file, MOD_yaml_file, id, session)
  }

  #---------------------------------------------
  # Here we update the state based on user input
  for(ui_name in state[["UD"]][["ui_ids"]]){
    if(!is.null(isolate(input[[ui_name]]))){
       state[["UD"]][["ui"]][[ui_name]] = isolate(input[[ui_name]])
     } else {
       if(ui_name %in% names(state[["UD"]][["button_counters"]])){
         state[["UD"]][["ui"]][[ui_name]] = 0
       } else {
         state[["UD"]][["ui"]][[ui_name]] = ""
       }

       # initializing the previous ui values as well:
       if(is.null(state[["UD"]][["ui_old"]][[ui_name]])){
         state[["UD"]][["ui_old"]][[ui_name]] = state[["UD"]][["ui"]][[ui_name]]
       }
     }
   }

  #---------------------------------------------
  # Now we sync the ui in the state with the button click
  # tracking or current element. This ensures that every
  # time the state is fetched all of the components of
  # the current element are in sync.

  # This is a list of ui changes that were detected and
  # can be used to trigger different actions below:
  changed_uis = c()


  for(ui_name in state[["UD"]][["ui_ids"]]){
    if(!fetch_hold(state, ui_name)){
      if(ui_name %in% names(state[["UD"]][["button_counters"]])){
        # Button changes are compared to the button click tracking values
        change_detected =
          has_updated(ui_val   = state[["UD"]][["ui"]][[ui_name]],
                      old_val  = state[["UD"]][["button_counters"]][[ui_name]],
                      init_val = c("", "0"))

        if(change_detected){
          formods::FM_le(state, paste0("button click: ", ui_name, " = ", state[["UD"]][["ui"]][[ui_name]]))

          # Saving the change:
          state[["UD"]][["button_counters"]][[ui_name]] =
            state[["UD"]][["ui"]][[ui_name]]

          # logging the changed ui name:
          changed_uis = c(changed_uis, ui_name)
        }
      }else{
        change_detected =
          has_updated(ui_val   = state[["UD"]][["ui"]][[ui_name]],
                      old_val  = state[["UD"]][["ui_old"]][[ui_name]],
                      init_val = c(""))
        if(change_detected){
          formods::FM_le(state, paste0("setting: ", ui_name, " = ", paste(state[["UD"]][["ui"]][[ui_name]], collapse=", ")))

          # Saving the change:
          state[["UD"]][["ui_old"]][[ui_name]] = state[["UD"]][["ui"]][[ui_name]]

          # logging the changed ui name:
          changed_uis = c(changed_uis, ui_name)
        }
      }
    }
  }



  clean_ds = state[["MC"]][["clean_data"]][["default"]]
  if(!is.null(isolate(input$switch_clean))){
     clean_ds =   isolate(input$switch_clean)
  }
  #---------------------------------------------
  # Here we update the state based on user input
  # Loadinng the data file
  if(!is.null(isolate(input$input_data_file))){
    # Pulling the data_file and sheet from interface
    data_file            = isolate(input$input_data_file$name)
    data_file_local_form = isolate(input$input_data_file$datapath)
    sheet                = isolate(input$input_select_sheet)
    sheets               = c()
    contents             = NULL
    data_file_ext        = tolower(tools::file_ext(data_file))
    load_msg             = NULL

    # This is where the user files will be stored on the server
    data_file_local = file.path(FM_fetch_user_files_path(state), data_file)

    # Copying the temporary form file to the user directory
    file.copy(data_file_local_form, data_file_local, overwrite=TRUE)

    allowed_extensions = state[["MC"]][["allowed_extensions"]]
    # determining if we need to load data
    load_data  = FALSE
    clear_data = FALSE
    if(data_file_ext %in% allowed_extensions){
      # If we're dealing with an excel file we get the sheet names
      if(data_file_ext %in% c("xls", "xlsx")){
        sheets = readxl::excel_sheets(data_file_local_form)
      }
      load_data = TRUE
    } else {
      # pulling out the template:
      load_msg = state[["MC"]][["labels"]][["msg_bad_extension"]]

      load_msg = stringr::str_replace_all(load_msg, "===EXT===",        data_file_ext)
      load_msg = stringr::str_replace_all(load_msg, "===FILE===",       data_file)
      load_msg = stringr::str_replace_all(load_msg, "===ALLOWEDEXT===", paste(allowed_extensions, collapse=", "))
      load_msg = tagList(tags$em(load_msg))

      # This will force a reset of the DS field in the state
      clear_data = TRUE
    }

    # If load data is true then we store all that in the state variable
    if(load_data){
      # Generating the read code and reading in the contents of the file.
      read_res = UD_ds_read(
        state           = state,
        data_file_ext   = data_file_ext,
        data_file       = data_file,
        data_file_local = data_file_local,
        sheets          = sheets,
        sheet           = sheet)

      load_msg = tagList(tags$em(paste0("File loaded.")))

      # Storing all the elements in the state
      state = UD_attach_ds(
                state,
                clean           = clean_ds,
                data_file_local = data_file_local        ,
                data_file_ext   = data_file_ext          ,
                data_file       = data_file              ,
                sheet           = sheet                  ,
                sheets          = sheets                 ,
                code            = read_res[["code"]]     ,
                contents        = read_res[["contents"]] ,
                object_name     = read_res[["object_name"]] ,
                isgood          = TRUE)

      FM_le(state, paste0("module checksum updated:", state[["UD"]][["checksum"]]))
    }

    # If someone loads a good file then a bad one (e.g. bad file extension)
    # We clear the data and set the dataset to bad to prevent showing the old
    # dataset that is no longer relevant.
    if(clear_data){
      state = UD_attach_ds(state,
                clean = clean_ds)
    }

    # Picking up any loading messages that were defined
    state[["UD"]][["load_msg"]]        = load_msg
  }

  #---------------------------------------------
  # Running workflows

  if("btn_run_wf" %in% changed_uis){
    load_msg = c()
    rwf_isgood = TRUE
    wfn = state[["UD"]][["ui"]][["workflow"]]
    wfl = state[["yaml"]][["FM"]][["workflows"]][[wfn]]

    if(wfl[["require_ds"]] & !state[["UD"]][["isgood"]]){
      # To run this workflow a dataset is required but one has not been
      # uploaded. 
      load_msg  = state[["MC"]][["errors"]][["no_ds_for_workflow"]]
      rwf_isgood = FALSE
      FM_set_mod_state(session, id, state)
    } else {
      FM_le(state, paste0("Running workflow (", wfn, "): ", wfl[["desc"]]))
      # Preload file
      plf = render_str(wfl[["preload"]])

      # Preload list:
      pll = yaml::read_yaml(plf)

      # If require_ds is true we need to append (or replace existing) UD
      # portion of the preload file:
      if(wfl[["require_ds"]]){
        res_mpl = UD_mk_preload(state=state)

        # Removing any previous references to the UD module:
        pll[[ state[["id"]] ]] = NULL

        # Appending the current state:
        pll = c(res_mpl[["yaml_list"]], pll)
      }


      ASM_state = FM_fetch_mod_state(id=state[["MC"]][["module"]][["depends"]][["id_ASM"]], session=session)

      if(is.null(ASM_state)){
        load_msg =  state[["MC"]][["errors"]][["no_asm_state"]]
        rwf_isgood = FALSE
      } else {
        # Writing the new workflow yaml list to a save file:
        ssf  = tempfile(fileext=".zip")
        ss_res = ASM_save_state(state=state, session=session, file_path=ssf, pll=pll)
        if(ss_res[["isgood"]]){
          FM_pause_screen(state   = state,
                          message = state[["MC"]][["labels"]][["busy"]][["rwf"]],
                          session = session)
          ls_res = 
          ASM_load_state(state, session,
                         file_path = ssf)
          FM_resume_screen(state   = state,
                           session = session)


          state = ls_res[["state"]]

          if(!ls_res[["isgood"]]){
            load_msg =  c(state[["MC"]][["errors"]][["ls_failed"]], ls_res[["msgs"]])
            rwf_isgood = FALSE
          }

        } else {
          load_msg =  c(state[["MC"]][["errors"]][["ss_failed"]], ss_res[["msgs"]])
          rwf_isgood = FALSE
        }
      }
    }

    if(rwf_isgood){
      state = FM_set_notification(state,
        notify_text =  state[["MC"]][["labels"]][["rwf_success"]],
        notify_id   = "rwf_success",
        type        = "success")
    } else {
      state = FM_set_notification(state,
        notify_text =  state[["MC"]][["errors"]][["rwf_failed"]],
        notify_id   = "rwf_failed", 
        type        = "failure")
    }
    state[["UD"]][["load_msg"]] = paste0(load_msg, collapse="\n")
  }

  # Saving the state
  FM_set_mod_state(session, id, state)

  # Returning the state
  state}


#'@export
#'@title Initialize UD Module State
#'@description Creates a list of the initialized module state
#'@param FM_yaml_file App configuration file with FM as main section.
#'@param MOD_yaml_file  Module configuration file with MC as main section.
#'@param id ID string for the module.
#'@param session Shiny session variable
#'@return list containing an empty UD state
#'@examples
#' # Within shiny a session variable will exist,
#' # this creates one here for testing purposes:
#' sess_res = UD_test_mksession(session=list())
#' session = sess_res$session
#'state = UD_init_state(
#'    FM_yaml_file  = system.file(package = "formods",
#'                                "templates",
#'                                "formods.yaml"),
#'    MOD_yaml_file = system.file(package = "formods",
#'                                "templates",
#'                                "UD.yaml"),
#'    id              = "UD",
#'    session         = session)
#' state
UD_init_state = function(FM_yaml_file, MOD_yaml_file,  id, session){


  button_counters = c("btn_run_wf")
  ui_ids = c("workflow",
             button_counters)


  state = FM_init_state(
    FM_yaml_file    = FM_yaml_file,
    MOD_yaml_file   = MOD_yaml_file,
    id              = id,
    MT              = "UD",
    button_counters = button_counters,
    ui_ids          = ui_ids,
    ui_hold         = ui_ids, 
    session         = session)

  state[["UD"]][["ui_ids"]] = ui_ids


  # Assigning the default clean option
  clean_ds =   state[["MC"]][["clean_data"]][["default"]]

  state = UD_attach_ds(state,
            clean = clean_ds)

  FM_le(state, "State initialized")
  state}

#'@export
#'@title Attach Data Set to UD State
#'@description Attaches a dataset to the UD state supplied.
#'@param state UD state module.
#'@param clean  Boolean switch to determine if the headers in the loaded dataset was cleaned. 
#'@param isgood Boolean object indicating if the file was successfully loaded.
#'@param load_msg Text message indicated the success or any problems encountered when uploading the file.
#'@param data_file_local Full path to the data file on the server.
#'@param data_file Dataset file name without the path.
#'@param data_file_ext File extension of the uploaded file.
#'@param sheet If the uploaded file is an excel file, this is the currently selected sheet.
#'@param sheets If the uploaded file is an excel file, this is a character vector of the sheets present in that file.
#'@param code Code to load dataset.
#'@param object_name Name of the dataset object created when code is evaluated.
#'@param contents Data frame containting the contents of the data file.
#'@return state with data set attached
#'@examples
#' # We need a module state object to use this function:
#' id="UD"
#' sess_res = UD_test_mksession(session=list())
#' state = sess_res$state
#'
#' # This is the full path to a test data file:
#' data_file_local  =  system.file(package="formods", "test_data", "TEST_DATA.xlsx")
#'
#' # Excel file extension
#' data_file_ext    = "xlsx"
#'
#' # Base file name
#' data_file        = "TEST_DATA.xlsx"
#'
#' # Excel files need a sheet specification:
#' sheet           = "DATA"
#'
#' # We will also attach the sheets along with it
#' sheets = readxl::excel_sheets(data_file_local)
#'
#' ds_read_res = UD_ds_read(state,
#'   data_file_ext   = data_file_ext,
#'   data_file_local = data_file_local,
#'   data_file       = data_file,
#'   sheets          = sheets,
#'   sheet          = sheet)
#'
#' # This would contain the loading code that will cascade down
#' # to the other modules when generating snippets and
#' # reproducible scripts
#' code = ds_read_res$code
#'
#' # This is the R Object name that is used internally
#' # and in generated scripts. Should be the same as in
#' # the code above
#' object_name = ds_read_res$object_name
#'
#' # This is the actual dataset:
#' contents   = ds_read_res$contents
#'
#' state =  UD_attach_ds(
#'          state,
#'          data_file_local = data_file_local,
#'          data_file_ext   = ".xlsx",
#'          data_file       = data_file,
#'          sheet           = sheet,
#'          sheets          = sheets,
#'          code            = code,
#'          object_name     = object_name,
#'          contents        = contents)
#'
#' state
UD_attach_ds = function(
         state,
         clean           = NULL,
         isgood          = TRUE,
         load_msg        = NULL,
         data_file_local = NULL,
         data_file_ext   = NULL,
         data_file       = NULL,
         sheet           = NULL,
         sheets          = NULL,
         code            = "",
         object_name     = NULL,
         contents        = NULL){

  # Calculating the checksum we include both the contents as well as the clean
  # state:
  checksum = digest::digest(c(contents,  state[["UD"]][["clean"]], algo=c("md5")))


  if(is.null(object_name)){
  # getting the object name:
    if(is.null(state[["MC"]][["ds_object_name"]])){
      object_name  = "UD"
      warning(paste0("Unable to find ds_object_name in yaml file. Using default: ", object_name))
    } else {
      object_name = state[["MC"]][["ds_object_name"]]
    }
  }

  # Contents is null at init. We want to set the flag to false since there is
  # no data loaded
  if(is.null(contents)){
    isgood = FALSE
  }

  state[["UD"]][["isgood"]]          = isgood          
  state[["UD"]][["clean"]]           = clean           
  state[["UD"]][["load_msg"]]        = load_msg        
  state[["UD"]][["data_file_local"]] = data_file_local 
  state[["UD"]][["data_file_ext"]]   = data_file_ext   
  state[["UD"]][["data_file"]]       = data_file       
  state[["UD"]][["sheet"]]           = sheet           
  state[["UD"]][["sheets"]]          = sheets          
  state[["UD"]][["code"]]            = code            
  state[["UD"]][["object_name"]]     = object_name     
  state[["UD"]][["checksum"]]        = checksum        
  state[["UD"]][["contents"]]        = contents        

  state}


#'@export
#'@title Generate Code and Load DS
#'@description Generates the code for loading a dataset and returns both the
#'code and the contents
#'@param state UD state from \code{UD_fetch_state()}
#'@param data_file_local Full path to the data file on the server.
#'@param data_file Dataset file name without the path.
#'@param data_file_ext File extension of the uploaded file (e.g. "xlsx",
#'"csv", etc).
#'@param sheets If the uploaded file is an excel file, this is all the sheets in the file.
#'@param sheet If the uploaded file is an excel file, this is the currently selected sheet.
#'@param sheets If the uploaded file is an excel file, this is a character vector of the sheets present in that file.
#'@return list with the elements of the dataset (contents, object_name, code,
#'and isgood)
#'@examples
#' # We need a module state object to use this function:
#' id="UD"
#' sess_res = UD_test_mksession(session=list())
#' state = sess_res$state
#'
#' # This is the full path to a test data file:
#' data_file_local  =  system.file(package="formods", "test_data", "TEST_DATA.xlsx")
#'
#' # Excel file extension
#' data_file_ext    = "xlsx"
#'
#' # Base file name
#' data_file        = "TEST_DATA.xlsx"
#'
#' # Excel files need a sheet specification:
#' sheet            = "DATA"
#'
#' # We will also attach the sheets along with it
#' sheets = readxl::excel_sheets(data_file_local)
#'
#' ds_read_res = UD_ds_read(state,
#'   data_file_ext   = data_file_ext,
#'   data_file_local = data_file_local,
#'   data_file       = data_file,
#'   sheets          = sheets,
#'   sheet          = sheet)
#'
#' ds_read_res
UD_ds_read = function(state,
                      data_file_ext    = NULL,
                      data_file_local  = NULL,
                      data_file        = NULL,
                      sheets           = NULL,
                      sheet            = NULL){

  contents = c()
  code     = NULL
  isgood   = FALSE

  # making sure the extension is lower case for comparisons below.
  data_file_ext = tolower(data_file_ext)

  # getting the object name:
  if(is.null(state[["MC"]][["ds_object_name"]])){
    object_name  = "UD"
    warning(paste0("Unable to find ds_object_name in yaml file. Using default: ", object_name))
  } else {
    object_name = state[["MC"]][["ds_object_name"]]
  }

  # Reading in the file contents:
  if(data_file_ext %in% c("csv")){
    contents = readr::read_csv(file=data_file_local)
    code     = paste0(object_name, ' = readr::read_csv(file="',data_file,'")')
    isgood   = TRUE
  }
  if(data_file_ext %in% c("tsv")){
    contents = readr::read_tsv(file=data_file_local)
    code     = paste0(object_name, ' = readr::read_tsv(file="',data_file,'")')
    isgood   = TRUE
  }
  if(data_file_ext %in% c("xls", "xlsx")){
    # If you load one excel sheet and then switch to another
    # where that sheet isn't present this will reset "sheet"
    # in that scenario.
    if(!is.null(sheet)){
      if(!(sheet %in% sheets)){
        sheet = NULL
      }
    }
    # By default we read the first sheet
    if(is.null(sheet)){
      sheet = sheets[1] }
    contents = readxl::read_excel(path=data_file_local, sheet=sheet)
    code = paste0(object_name, ' = readxl::read_excel(path="',data_file,'", sheet="',sheet,'")')
    isgood   = TRUE
  }

  # If there is code cleaning in the configuration file we try to run it here
  if(!is.null(state[["MC"]][["clean_data"]][["code"]])){
    # We only do that if cleaning is selected
    if(state[["UD"]][["clean"]]){
      # Attempting to clean the code
      tcres = FM_tc(
        cmd     = state[["MC"]][["clean_data"]][["code"]],
        tc_env  = list(
          contents    = contents,
          code        = code,
          object_name = object_name),
        capture = c("code", "contents"))

      # Here we process the results of the cleaning
      if(tcres[["isgood"]]){
        code     = tcres[["capture"]][["code"]]
        contents = tcres[["capture"]][["contents"]]
      } else {
        FM_le(state, "clean_data failed")
        FM_le(state, tcres[["msgs"]])
      }
    }
  }

  res = list(contents    = contents,
             object_name = object_name,
             isgood      = isgood,
             code        = code)

res}


#'@export
#'@title Fetch Module Code
#'@description Fetches the code to generate results seen in the app
#'@param state UD state from \code{UD_fetch_state()}
#'@return Character object vector with the lines of code
#'@examples
#' # This creates a session variable that will be available in Shiny
#' state = UD_test_mksession(session=list())$state
#' UD_fetch_code(state)
UD_fetch_code = function(state){

  # If the contents are NULL then nothing has been uploaded and we return NULL
  if(is.null(state[["UD"]][["contents"]])){
    code = NULL
  } else {
    code = state[["UD"]][["code"]]
  }

code}

#'@export
#'@title Fetch Module Datasets
#'@description Fetches the datasets contained in the module.
#'@param state UD state from \code{UD_fetch_state()}
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
#'    \item{idx: unique numerical ID to identify this dataset in the module.}
#'    \item{DS: Dataframe containing the actual dataset.}
#'    \item{DSMETA: Metadata describing DS, see \code{FM_fetch_ds()} for
#'    details on the format.}
#'    \item{code: Complete code to build dataset.}
#'    \item{checksum: Module checksum.}
#'    \item{DSchecksum: Dataset checksum.}
#'  }
#'}
#'@examples
#' # YAML configuration files from the package:
#' FM_yaml_file  = system.file(package = "formods", "templates", "formods.yaml")
#' MOD_yaml_file = system.file(package = "formods", "templates", "UD.yaml")
#' # This is the module id:
#' id = "UD"
#' # Within shiny both session and input variables will exist,
#' # this creates examples here for testing purposes:
#' sess_res = UD_test_mksession(session=list())
#' session = sess_res$session
#' input   = sess_res$input
#' state = UD_fetch_state(
#'            id            = id,
#'            input         = input,
#'            session       = session,
#'            FM_yaml_file  = FM_yaml_file,
#'            MOD_yaml_file = MOD_yaml_file )
#'   
#'  ds_res = UD_fetch_ds(state) 
UD_fetch_ds = function(state){
  hasds  = FALSE
  isgood = TRUE
  msgs   = c()
  ds     = list()

  # Empty list for new datasets
  NEWDS = list(label      = NULL,
               MOD_TYPE   = NULL,
               id         = NULL,
               idx        = 1,    
               DS         = NULL,
               DSMETA     = NULL,
               code       = NULL,
               checksum   = NULL,
               DSchecksum = NULL)

  # This prevents returning a dataset if this is triggered before data has
  # been loaded
  if(state[["UD"]][["isgood"]]){
    NEWDS[["label"]]      = state[["MC"]][["labels"]][["default_ds"]]
    NEWDS[["DS"]]         = state[["UD"]][["contents"]]
    NEWDS[["checksum"]]   = state[["UD"]][["checksum"]]
    NEWDS[["DSchecksum"]] = state[["UD"]][["checksum"]]
    NEWDS[["code"]]       = state[["UD"]][["code"]]
    NEWDS[["MOD_TYPE"]]   = "UD"
    NEWDS[["id"]]         = state[["id"]]
    object_name           = state[["UD"]][["object_name"]]
    hasds                 = TRUE

    # Putting it all into the ds object to be returned
    ds[[object_name]] = NEWDS
  }

  res = list(hasds  = hasds,
             isgood = isgood,
             msgs   = msgs,
             ds     = ds)
res}

#'@export
#'@title Populate Session Data for Module Testing
#'@description Populates the supplied session variable for testing.
#'@param session Shiny session variable (in app) or a list (outside of app)
#'@return The UD portion of the `all_sess_res` returned from \code{\link{FM_app_preload}} 
#'@examples
#' sess_res = UD_test_mksession()
#'@seealso \code{\link{FM_app_preload}}
UD_test_mksession = function(session=list()){

  sources = c(system.file(package="formods", "preload", "ASM_preload.yaml"),
              system.file(package="formods", "preload", "UD_preload.yaml"))
  res = FM_app_preload(session=session, sources=sources)
  res = res[["all_sess_res"]][["UD"]]

res}


#'@export
#'@title Preload Data for UD Module
#'@description Populates the supplied session variable with information from
#'list of sources.
#'@param session     Shiny session variable (in app) or a list (outside of app)
#'@param src_list    List of preload data (all read together with module IDs at the top level) 
#'@param yaml_res    List data from module yaml config
#'@param mod_ID      Module ID of the module being loaded. 
#'@param react_state Reactive shiny object (in app) or a list (outside of app) used to trigger reactions. 
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
UD_preload  = function(session, src_list, yaml_res, mod_ID=NULL, react_state = list(), quickload=FALSE){
  isgood = TRUE
  input  = list()
  msgs   = c()

  FM_yaml_file  = render_str(src_list[[mod_ID]][["fm_yaml"]])
  MOD_yaml_file = render_str(src_list[[mod_ID]][["mod_yaml"]])
  id_ASM        = yaml_res[[mod_ID]][["mod_cfg"]][["MC"]][["module"]][["depends"]][["id_ASM"]]


  full_file_path = render_str(src_list[[mod_ID]][["data_source"]][["file_name"]])
  clean_ds       = render_str(src_list[[mod_ID]][["data_source"]][["clean"]])


  input[["input_data_file"]][["datapath"]] = full_file_path
  input[["input_data_file"]][["name"]]     = basename(full_file_path)
  # Handling the situation where the sheet has not been defined:
  input[["input_select_sheet"]] = NULL
  if(!is.null(src_list[[mod_ID]][["data_source"]][["sheet"]])){
    input[["input_select_sheet"]] = 
      render_str(src_list[[mod_ID]][["data_source"]][["sheet"]])
  }

  # Setting cleaning options
  if(!is.null(clean_ds)){
    input[["input_data_file"]][["switch_clean"]] = as.character(clean_ds)
  }

  state = UD_fetch_state(id            = mod_ID,
                         id_ASM        = id_ASM,
                         input         = input,
                         session       = session,
                         FM_yaml_file  = FM_yaml_file,
                         MOD_yaml_file = MOD_yaml_file)


  # Required for proper reaction:
  react_state[[mod_ID]]  = list(UD  = list(checksum=state[["UD"]][["checksum"]]))

  if(!state[["UD"]][["isgood"]]){
    isgood = FALSE
    msgs = c(msgs, "Failed to load dataset")
  }
 
  # Saving the state
  if(formods::is_shiny(session)){
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
#'@title Make List of Current ASM State
#'@description Converts the current ASM state into a preload list.
#'@param state UD state object
#'@return list with the following elements
#' \itemize{
#'   \item{isgood:}       Boolean indicating the exit status of the function.
#'   \item{msgs:}         Messages to be passed back to the user.
#'   \item{yaml_list:}    Lists with preload components.
#'}
#'@examples
#' sess_res = UD_test_mksession()
#' state = sess_res$state
#' res = UD_mk_preload(state)
UD_mk_preload     = function(state){
  isgood    = TRUE
  msgs      = c()  
  yaml_list = list()

  yaml_list[[ state[["id"]] ]] = list(
      fm_yaml  = file.path("config", basename(state[["FM_yaml_file"]])),
      mod_yaml = file.path("config", basename(state[["MOD_yaml_file"]]))
  )

  yaml_list[[ state[["id"]] ]][[ "data_source" ]] = list(
    file_name = state[["UD"]][["data_file"]],
    sheet     = state[["UD"]][["sheet"]],
    clean     = state[["UD"]][["clean"]])

  formods::FM_le(state,paste0("mk_preload isgood: ",isgood))

  res = list(
    isgood    = isgood,
    msgs      = msgs,
    yaml_list = yaml_list)
res}
