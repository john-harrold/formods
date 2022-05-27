#'@import rhandsontable
#'@import readxl
#'@import shiny
#'@importFrom digest digest
#'@importFrom magrittr "%>%"
#'@importFrom readr read_csv
#'@importFrom shinyAce aceEditor updateAceEditor
#'@importFrom stats setNames
#'@importFrom stringr str_replace_all
#'@importFrom tools file_ext
#'@importFrom yaml read_yaml


#'@export
#'@title Data Upload Server
#'@description Server function for the Data Uplaod Shiny Module
#'@param id An ID string that corresponds with the ID used to call the module's UI function
#'@param yaml_section  Section of the yaml file with the module configuration (`"UD"`)
#'@param yaml_file Upload Data cofiguration file
#'@param react_state Variable passed to server to allow reaction outside of module (`NULL`)
#'@return return
UD_Server <- function(id,
                yaml_section = "UD",
                yaml_file    = system.file(package = "formods", "templates", "config.yaml"),
                react_state  = NULL) {
  moduleServer(id, function(input, output, session) {


    #------------------------------------
    # Creates the file upload elements
    output$UD_ui_load_data = renderUI({
      state = UD_fetch_state(id, input, session, yaml_file, yaml_section)
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
    output$UD_ui_select_sheets =  renderUI({
      # Reacting to data file changes
      input$input_data_file
      state = UD_fetch_state(id, input, session, yaml_file)

      if(!is.null(state[["DS"]][["data_file_ext"]]) &
         !is.null(state[["DS"]][["sheet"]]) &
         !is.null(state[["DS"]][["sheets"]])){
         uiele =
           selectInput(
               NS(id, "input_select_sheet"),
               "Select Sheet",
               choices  = state[["DS"]][["sheets"]],
               selected = state[["DS"]][["sheet"]],
               multiple = FALSE)

      } else {
        uiele = NULL
      }
    uiele})
    #------------------------------------
    # Data loading messages go here
    output$UD_ui_text_load_result  =  renderUI({
      # Reacting to data file changes
      input$input_data_file
      input$input_select_sheet
      state = UD_fetch_state(id, input, session, yaml_file, yaml_section)
      if(!is.null(state[["DS"]][["load_msg"]])){
        uiele = state[["DS"]][["load_msg"]]
      } else {
        uiele = NULL
      }
    uiele})
    #------------------------------------
    # A simple preview of the data:
    output$UD_ui_data_preview  =  renderUI({
      # Reacting to data file changes
      input$input_data_file
      input$input_select_sheet
      state = UD_fetch_state(id, input, session, yaml_file, yaml_section)

      if(is.data.frame(state[["DS"]][["contents"]])){
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
      state = UD_fetch_state(id, input, session, yaml_file, yaml_section)

      if(is.null(state[["DS"]][["code"]])){
        uiele = "# No file loaded"
      } else {
        uiele = state[["DS"]][["code"]]
      }


      shinyAce::updateAceEditor(
        session         = session, 
        editorId        = "UD_ui_ace_code", 
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
      state = UD_fetch_state(id, input, session, yaml_file, yaml_section)
      if(is.data.frame(state[["DS"]][["contents"]])){
        uiele = rhandsontable::rhandsontable(state[["DS"]][["contents"]],
          width  = state[["MC"]][["preview"]][["width"]],
          height = state[["MC"]][["preview"]][["height"]])
      } else {uiele=NULL}
    uiele})
    #------------------------------------
    # Creates the ui for the compact view of the module
    output$UD_ui_compact  =  renderUI({
      state = UD_fetch_state(id, input, session, yaml_file, yaml_section)

      uiele = NULL

      uiele_main   = tagList(htmlOutput(NS(id, "UD_ui_load_data")),
                             htmlOutput(NS(id, "UD_ui_select_sheets")),
                             htmlOutput(NS(id, "UD_ui_text_load_result")))
      
      if( state$MC$compact$code | state$MC$compact$preview){
         uiele_preview = tagList(htmlOutput(NS(id, "UD_ui_data_preview")))
         uiele_code    = tagList(shinyAce::aceEditor(NS(id, "UD_ui_ace_code")))

        uiele_str ="tabPanel(state$MC$labels$tab_main,  uiele_main  )"
        if(state$MC$compact$preview){
          uiele_str = paste0(uiele_str, ",tabPanel(state$MC$labels$tab_preview, uiele_preview)") }
        if(state$MC$compact$code){
          uiele_str = paste0(uiele_str, ",tabPanel(state$MC$labels$tab_code, uiele_code)") }

        uiele_str = paste0("tabsetPanel(",uiele_str, ")")

        uiele = eval(parse(text=uiele_str))
      } else {
        uiele = uiele_main  
      }

    uiele})
    #outputOptions(output, "UD_ui_compact", priority = -1)
    #------------------------------------
    # Creating reaction if a variable has been specified
    if(!is.null(react_state)){
      # Here we list the ui inputs that will result in a state change:
      toListen <- reactive({
        list(input$input_data_file,
             input$input_select_sheet) })
      # This updates the reaction state:
      observeEvent(toListen(), {
        react_state[[id]] = UD_fetch_state(id, input, session, yaml_file, yaml_section)
      })
    }


  })
}

#'@export
#'@title Fetch Upload Data State
#'@description Merges default app options with the changes made in the UI
#'@param id Shiny module ID
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
#' \item{DS:} Loaded dataset with the following elements
#' \itemize{
#'   \item{isgood:} Boolean indicating the success of the file being loaded.
#'   \item{load_msg:} Text message indicated the success or any problems
#'   encountered when uploading the file.
#'   \item{data_file_local:} Full path to the data file on the server.
#'   \item{data_file:} Dataset file name without the path. 
#'   \item{data_file_ext:} File extension of the uploaded file.
#'   \item{sheet:} If the uploaded file is an excel file, this is the
#'   currently selected sheet.
#'   \item{sheets:} If the uploaded file is an excel file, this is a character
#'   vector of the sheets present in that file.
#'   \item{contents:} Data frame containting the contents of the data file.
#'   \item{checksum:} This is an MD5 sum of the contents element and can be
#'   used to detect changes in the loaded file.
#' }
#'}
UD_fetch_state = function(id, input, session, yaml_file, yaml_section){

  # After the app has loaded the state must be initialized
  FM_UD_ID = paste0("FM_UD_", id)

  # Template for an empty dataset
  #---------------------------------------------
  # Getting the current state
  if(is.null(session$userData[[FM_UD_ID]])){
    # General state information
    state = UD_init_state(yaml_file)

    # This assigns the module config "MC" element to the correct yaml_section.
    state[["MC"]] = state[["yaml"]][[yaml_section]]
  } else {
    # If it's not null we just pluck the state
    # from the session variable
    state = session$userData[[FM_UD_ID]]
  }

  #---------------------------------------------
  # Here we update the state based on user input
  # Loadinng the data file
  if(!is.null(isolate(input$input_data_file))){
    # Pulling the data_file and sheet from interface
    data_file       = isolate(input$input_data_file$name)
    data_file_local = isolate(input$input_data_file$datapath)
    sheet           = isolate(input$input_select_sheet)
    sheets          = c()
    contents        = NULL
    data_file_ext   = tools::file_ext(data_file)
    load_msg        = NULL

    allowed_extensions = state[["MC"]][["allowed_extensions"]]
    # determining if we need to load data
    load_data  = FALSE
    clear_data = FALSE
    if(data_file_ext %in% allowed_extensions){
      # If we're dealing with an excel file we get the sheet names
      if(data_file_ext %in% c("xls", "xlsx")){
        sheets = readxl::excel_sheets(data_file_local)
      }
      # If we have an allowed file type and nothing loaded before then we load
      # the file:
      if(is.null(state[["DS"]][["data_file_local"]])){
        load_data = TRUE
      } else{
        # If there is already a file loaded we need to see
        # if there are any changes. For example if the sheet
        # name has changed.
        # This is triggered when a new file has been uploaded
        if(state[["DS"]][["data_file_local"]] != data_file_local){
          load_data = TRUE
        } else if(!is.null(sheet)){
          # This should be triggered when the sheet has changed
          if(sheet !=  state[["DS"]][["data_file_local"]]){
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
      code = NULL
      # Reading in the file contents:
      if(data_file_ext %in% c("csv")){
        contents = readr::read_csv(file=data_file_local)
        code = paste0('DS = readr::read_csv(file="',data_file,'")\n')
      }
      if(data_file_ext %in% c("tsv")){
        contents = readr::read_tsv(file=data_file_local)
        code = paste0('DS = readr::read_tsv(file="',data_file,'")\n')
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
        code = paste0('DS = readxl::read_excel(path=(path="',data_file,'", sheet="',sheet,'")\n')

      }
      load_msg = tagList(tags$em(paste0("File loaded.")))
      # Storing all the elements in the state
      state[["DS"]][["data_file_local"]] = data_file_local
      state[["DS"]][["data_file_ext"]]   = data_file_ext
      state[["DS"]][["data_file"]]       = data_file
      state[["DS"]][["sheet"]]           = sheet
      state[["DS"]][["sheets"]]          = sheets
      state[["DS"]][["code"]]            = code
      state[["DS"]][["contents"]]        = contents
      state[["DS"]][["checksum"]]        = digest::digest(contents, algo=c("md5"))
      state[["DS"]][["isgood"]]          = TRUE
    }

    # If someone loads a good file then a bad one (e.g. bad file extension)
    # We clear the data and set the dataset to bad to prevent showing the old
    # dataset that is no longer relevant.
    if(clear_data){
      state[["DS"]] = fetch_DS_NULL()
    }

    # Picking up any loading messages that were defined
    state[["DS"]][["load_msg"]]        = load_msg
  }

  #---------------------------------------------
  # Saving the state
  session$userData[[FM_UD_ID]] = state

  # Returning the state
state}


#'@export
#'@title Initialize App State
#'@description Creates a list of the initialized app state
#'@param yaml_file App cofiguration file
#'@return list containing an empty app state object
UD_init_state = function(yaml_file){

    state = list()
    # Reading in default information from the yaml file
    state[["yaml"]] = yaml::read_yaml(yaml_file)

    state[["DS"]] = fetch_DS_NULL()

state}

#'@export
#'@title Empty Data Set Object
#'@description Creates a list with default elements for a dataset
#'@return Null dataset list
fetch_DS_NULL = function(){

    DS_NULL =
      list(isgood          = FALSE,
           load_msg        = NULL,
           data_file_local = NULL,
           data_file_ext   = NULL,
           data_file       = NULL,
           sheet           = NULL,
           sheets          = NULL,
           checksum        = digest::digest(NULL, algo=c("md5")),
           contents        = NULL)

DS_NULL}

