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

      if(is.null(state[["UD"]][["code"]])){
        uiele = "# No file loaded"
      } else {
        uiele = state[["UD"]][["code"]]
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
        uiele = rhandsontable::rhandsontable(state[["UD"]][["contents"]],
                                             width  = state[["MC"]][["formatting"]][["preview"]][["width"]],
                                             height = state[["MC"]][["formatting"]][["preview"]][["height"]])
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
           tooltip = tooltipOptions(title = state[["MC"]][["tooltips"]][["show_code"]]))
        )
      }

      uiele  = tagList( 
           div(style="display:inline-block;width:100%", htmlOutput(NS(id, "ui_ud_load_data"))),
           htmlOutput(NS(id, "ui_ud_select_sheets")),
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
    file.copy(data_file_local_form, data_file_local)

    allowed_extensions = state[["MC"]][["allowed_extensions"]]
    # determining if we need to load data
    load_data  = FALSE
    clear_data = FALSE
    if(data_file_ext %in% allowed_extensions){
      # If we're dealing with an excel file we get the sheet names
      if(data_file_ext %in% c("xls", "xlsx")){
        sheets = readxl::excel_sheets(data_file_local_form)
      }
      # If we have an allowed file type and nothing loaded before then we load
      # the file:
      if(is.null(state[["UD"]][["data_file_local"]])){
        load_data = TRUE
      } else{
        # If there is already a file loaded we need to see
        # if there are any changes. For example if the sheet
        # name has changed.
        # This is triggered when a new file has been uploaded
        if(state[["UD"]][["data_file_local"]] != data_file_local){
          load_data = TRUE
        } else if(!is.null(sheet)){
          # This should be triggered when the sheet has changed
          if(is.null(state[["UD"]][["sheet"]] )){
            load_data = TRUE
          } else if(sheet !=  state[["UD"]][["sheet"]]){
            load_data = TRUE
          }
        }
      }
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
      state = UD_attach_ds(state)
    }

    # Picking up any loading messages that were defined
    state[["UD"]][["load_msg"]]        = load_msg
  }

  #---------------------------------------------
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


  state = FM_init_state(
    FM_yaml_file    = FM_yaml_file,
    MOD_yaml_file   = MOD_yaml_file,
    id              = id,
    MT              = "UD",
    button_counters = NULL,
    ui_ids          = NULL,
    ui_hold         = NULL,
    session         = session)

  state = UD_attach_ds(state)


  FM_le(state, "State initialized")
  state}

#'@export
#'@title Attach Data Set to UD State
#'@description Attaches a dataset to the UD state supplied.
#'@param state UD state module.
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

  # Calculating the checksum
  checksum        = digest::digest(contents, algo=c("md5"))

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

  state[["UD"]] =
    list(isgood          = isgood         ,
         load_msg        = load_msg       ,
         data_file_local = data_file_local,
         data_file_ext   = data_file_ext  ,
         data_file       = data_file      ,
         sheet           = sheet          ,
         sheets          = sheets         ,
         code            = code           ,
         object_name     = object_name    ,
         checksum        = checksum       ,
         contents        = contents      )

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
#'@description Fetches the datasets contained in the model
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
#'    \item{DS: Dataframe containing the actual dataset.}
#'    \item{DSMETA: Metadata describing DS, see \code{FM_fetch_ds()} for
#'    details on the format.}
#'    \item{code: Complete code to build dataset.}
#'    \item{checksum: Module checksum.}
#'    \item{DSchecksum: Dataset checksum.}
#'  }
#'}
UD_fetch_ds = function(state){
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
#'@param id An ID string that corresponds with the ID used to call the modules UI elements
#'@return list with the following elements
#' \itemize{
#'   \item{isgood:} Boolean indicating the exit status of the function.
#'   \item{session:} The value Shiny session variable (in app) or a list (outside of app) after initialization.
#'   \item{input:} The value of the shiny input at the end of the session initialization.
#'   \item{state:} App state.
#'   \item{rsc:} The \code{react_state} components.
#'}
#'@examples
#' res = UD_test_mksession(session=list())
UD_test_mksession = function(session, id = "UD"){

  isgood = TRUE
  rsc    = list()
  input  = list()

  input[["input_data_file"]][["datapath"]] = system.file(package="formods", "test_data","TEST_DATA.xlsx")
  input[["input_data_file"]][["name"]]     = "TEST_DATA.xlsx"

  FM_yaml_file  = system.file(package = "formods", "templates", "formods.yaml")
  MOD_yaml_file = system.file(package = "formods", "templates", "UD.yaml")

  state = UD_fetch_state(id            = id,
                         input         = input,
                         session       = session,
                         FM_yaml_file  = FM_yaml_file,
                         MOD_yaml_file = MOD_yaml_file)


  # This functions works both in a shiny app and outside of one
  # if we're in a shiny app then the 'session' then the class of
  # session will be a ShinySession. Otherwise it'll be a list if 
  # we're not in the app (ie just running test examples) then
  # we need to set the state manually
  if(!("ShinySession" %in% class(session))){
    session = FM_set_mod_state(session, id, state)
  }

  # Required for proper reaction:
  rsc[[id]] = list(UD = list(checksum=state[["UD"]][["checksum"]]))

  res = list(
    isgood  = isgood,
    session = session,
    input   = input,
    state   = state,
    rsc     = rsc
  )
}
