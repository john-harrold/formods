#' formods: Shiny modules for common tasks.
#'
#' Shiny apps can often make use of the same key elements, this package
#' provides modules for common tasks (data upload, wrangling data, figure
#' generation and saving the app state). These modules can react and interact
#' as well as generate code to create reproducible analyses.
#'
#' @seealso \url{https://formods.ubiquity.tools/}
#' @docType package
#' @name formods
"_PACKAGE"


#'@import cli
#'@importFrom digest digest


#'@export
#'@title Fetches data sets from modules in the app
#'@description  Loops through each specified module ID or all modules if no ID
#'was specified. For each ID, an attempt will be made to extract any datasets
#'available.
#'@param state Current module state after yaml file has been read
#'@param session Shiny session variable
#'@param ids  Vector of ID strings for the modules containing the datasets or
#'NULL for all datasets available.
#'@return list containing the current dataset with the following format:
#' \itemize{
#'   \item{isgood:} Boolean indicating the whether a dataset was found
#'   (\code{FALSE})
#'   \item{ds:} List of datasets with element names corresponding to the
#'   R object name for that dataset. This has the following format
#'   \itemize{
#'     \item{label:}  Text label for the dataset (used to display to the user)
#'     \item{DS:}     Data frame with the dataset
#'     \item{DSMETA:} Data frame with metadata about the colunns of the
#'     dataset in \code{DS}. The data frame should have the following columns:
#'     \itemize{
#'        \item{col1:} column 1
#'     }
#'     \item{code:} Code to generate the dataset.
#'     \item{checksum:} Module checksum when the dataset was pulled
#'     \item{DSchecksum:} Checksum of the dataframe in DS
#'   }
#'   \item{catalog:} Dataframe containing the a tabular catalog of the
#'   datasets found.
#'   \itemize{
#'     \item{label:} Text label
#'     \item{object:} Name of the R Object containing the data frame
#'     \item{MOD_TYPE:} Short name of the type of module
#'     \item{id:} Module ID
#'     \item{checksum:} Module checksum
#'     \item{DSchecksum:} Checksum of the dataset
#'     \item{code:} Code to generate the dataset
#'   }
#'   \item{modules:} List with an entry for each module. The element name is
#'   the short name. Each of these is a list with an entry that is the shiny module
#'   ID. For each of these there is a checksum. For example to access the
#'   checksum of a DW module with a  module ID of 'my_id', you would use the
#'   following: \code{res$modules$DW$my_id}.
#' }
#'@examples
#' # We need a module state and a Shiny session variable
#' # to use this function:
#' id="UD"
#' sess_res = UD_test_mksession(session=list(), id=id)
#' session = sess_res$session
#' state   = sess_res$state
#' ds = FM_fetch_ds(state, session)
#' ds$catalog
FM_fetch_ds = function(state, session, ids=NULL){

  isgood  = TRUE
  hasds   = FALSE
  catalog = NULL
  ds      = list()
  modules = list()

  # Pulling out the app state:
  app_state = FM_fetch_app_state(session)

  # If we're null then we walk through the session variable and pull out all
  # the IDs to be used below
  if(is.null(ids)){
    for(mod_state in names(app_state)){
      ids = c(ids, app_state[[mod_state]]$id)
    }
  }

  # Walking through each module id and attempting to extract a dataset
  for(tmp_id in ids){

    # pulling out the current module state and creating the
    # name of the ds fetching function for that module
    tmp_state    = FM_fetch_mod_state(session, tmp_id)
    tmp_MOD_TYPE = tmp_state[["MOD_TYPE"]]
    MOD_FUNC     = paste0(tmp_MOD_TYPE, "_fetch_ds")

    # If that module has a ds fetching function then we try to fetch it:
    if(exists(MOD_FUNC, mode="function")){

      # Function call used to fetch a dataset
      fetch_ds_res = NULL
      FUNC_CALL = paste0("fetch_ds_res = ", MOD_FUNC,"(state = tmp_state)")
      eval(parse(text=FUNC_CALL))

      if(fetch_ds_res[["hasds"]]){
        # We've found at least one dataset
        hasds = TRUE
        ds = c(ds, fetch_ds_res[["ds"]])
      }
    }
  }

  if(hasds){
    # Creating catalog and modules elements:
    for(dsname in names(ds)){
      catalog = rbind(
      catalog,
      data.frame(
        label       = ds[[dsname]][["label"]],
        object      = dsname,
        MOD_TYPE    = ds[[dsname]][["MOD_TYPE"]],
        id          = ds[[dsname]][["id"]],
        checksum    = ds[[dsname]][["checksum"]],
        DSchecksum  = ds[[dsname]][["DSchecksum"]],
        code        = ds[[dsname]][["code"]])
      )

      modules[[ ds[[dsname]][["MOD_TYPE"]]]  ][[ ds[[dsname]][["id"]] ]] = ds[[dsname]][["checksum"]]
    }
  } else {
    isgood = FALSE
  }

  # Packing everything up to be returned to the user
  res = list(isgood  = isgood,
             hasds   = hasds,
             catalog = catalog,
             modules = modules,
             ds      = ds)

res}

#'@export
#'@title Automatically Cast UI Input Variable
#'@description Takes UI input and tries to figure out if it's numeric or text
#'@param ui_input UI input from a shiny form
#'@param quote_char TRUE will include double quotes in the character string
#'@return Best guess of type casting applied to the ui_input
#'@examples
#' number = autocast('10')
#' text   = autocast('ten')
autocast = function(ui_input, quote_char=TRUE){


  ui_input_num = as.numeric(as.character(ui_input))

  if(any(is.na(ui_input_num))){
    res = as.character(ui_input)
    if(quote_char){
      res = paste0('"', res, '"')
    }
  } else {
    res = ui_input_num
  }

res}



#'@export
#'@title Remove Factor From Object
#'@description Takes an object that is a factor and returns an unfactored
#'vector with the same type by the value removed
#'@param fctobj Factorized object
#'@return Object with factors removed
#'@examples
#'
#' df = data.frame(
#'    text  = c("a", "b", "c"),
#'    float = c( 1 ,  2 ,  3 ))
#'
#' df$float = as.factor(df$float)
#' # This is a factor
#' df$float
#' # This is not a factor
#' unfactor(df$float)
unfactor = function(fctobj){
  res = fctobj
  if(is.factor(fctobj)){
    objtype = typeof(fctobj)
    cmd = paste0("res = as.", objtype,"(as.character(fctobj))");
    eval(parse(text=cmd))
  }
res}


#'@export
#'@title Detect if a UI element has changed
#'@description Takes a UI element value and an older value and determines if
#'it has been modified
#'@param ui_val     Current value from the UI.
#'@param old_val    Last value of of the element.
#'@param init_value Default value for reading in UI data when it has not been
#'defined.
#'@return Boolean result of the comparison
#'@examples
#' changed_true  = has_changed(ui_val = "a", old_val = "")
#' changed_true
#' changed_false = has_changed(ui_val = "a", old_val = "a")
#' changed_false
has_changed = function(ui_val     = NULL,
                       old_val    = NULL,
                       init_value = c("")){
  res = FALSE
  # We only Sure
  if(!is.null(ui_val)){
    if(ui_val    != 0 & ui_val   !=init_value){
      if(ui_val    != old_val){
        res = TRUE
      }
    }
  }
res}

#'@export
#'@title Removes Hold on UI Element
#'@description When some buttons are clicked they will change the state of the
#'system, but other UI components will not detect that change correctly. So those
#'triggers are put on hold. This will remove the hold after those UI
#'components have updated.
#'@param state module state with all of the current ui elements populated
#'@param session Shiny session variable
#'@param inputId The input ID of the UI element that was put on hold
#'@return NULL
#'@example inst/test_apps/FM_holds.R
remove_hold = function(state, session, inputId){

  FM_ID = paste0("FM_", state[["id"]])

  MOD_TYPE = state[["MOD_TYPE"]]

  # pulling out the state
  state = session$userData[["FM"]][[FM_ID]]

  # removing hold on inputId
  state[[MOD_TYPE]][["ui_hold"]][[inputId]] = FALSE

  # Saving the state
  session$userData[["FM"]][[FM_ID]] = state

NULL}


#'@export
#'@title Sets Hold on One or All UI Elements
#'@description When some buttons are clicked they will change the state of the
#'system, but other UI components will not detect that change correctly. So those
#'triggers are put on hold. This will set the hold for a specified inputId or
#'all ids if that value is set to NULL
#'@param state module state with all of the current ui elements populated
#'@param inputId The input ID of the UI element that was put on hold or
#'\code{NULL} to hold all IDs in the module
#'@return state with hold or holds set
#'@example inst/test_apps/FM_holds.R
set_hold = function(state, inputId=NULL){

  isgood = TRUE
  MOD_TYPE = state[["MOD_TYPE"]]
  if(is.null(inputId)){
    # here we set the hold for all inputIds in the module
    for(tmpinputId in names(state[[MOD_TYPE]][["ui_hold"]])){
      state[[MOD_TYPE]][["ui_hold"]][[tmpinputId]] = TRUE
    }
  } else {
    if(inputId %in% names(state[[MOD_TYPE]][["ui_hold"]])){
      state[[MOD_TYPE]][["ui_hold"]][[inputId]] = TRUE
    }else{
      # here we set the hold for a single inputId
      cli::cli_alert_danger(paste0("Unable to set hold for unknown inputId: ", inputId))
      isgood = FALSE
    }
  }

  if(!isgood){
    stop("Hold not set") }

state}

#'@export
#'@title Fetches the Hold Status UI Element Supplied
#'@description When some buttons are clicked they will change the state of the
#'system, but other UI components will not detect that change correctly. So those
#'triggers are put on hold. This will fetch hold status for a specified inputId
#'@param state module state with all of the current ui elements populated
#'@param inputId The input ID of the UI element that was put on hold
#'@return Boolean value with the hold status
fetch_hold = function(state, inputId=NULL){

  hold_status = NULL
  isgood = TRUE

  MOD_TYPE = state[["MOD_TYPE"]]
  if(is.null(inputId)){
    isgood = FALSE
  } else {
    if(inputId %in% names(state[[MOD_TYPE]][["ui_hold"]])){
      hold_status =   state[[MOD_TYPE]][["ui_hold"]][[inputId]]
    }else{
      # here we set the hold for a single inputId
      stop(paste0("Unable to fetch hold for unknown inputId: ", inputId))
      isgood = FALSE
    }
  }

  if(!isgood){
    stop("Hold status not found") }

hold_status}

#'@export
#'@title Fetches the code to reproduce analysis
#'@description Takes the current state of the app and builds a script to
#'reproduce the analysis within the app.
#'@param session Shiny session variable
#'@return list with the following elements:
#' \itemize{
#'   \item{isgood:} Boolean indicating the whether code generation was
#'   successful
#'   (\code{TRUE})
#' \item{msgs:} Any messages generated
#' \item{code:} Code to regenerate the app
#' }
#'@examples
#' # We need a Shiny session object to use this function:
#' id="UD"
#' sess_res = UD_test_mksession(session=list(), id=id)
#' session = sess_res$session
#' app_code = FM_fetch_app_code(session)
#' cat(app_code$code)
FM_fetch_app_code = function(session){
  isgood = TRUE
  msgs   = c()
  code   = ""
  code_chunks = c()

  app_state = FM_fetch_app_state(session)

  if(length(names(app_state))>0){
    # Pulling out the generation options from the first app state
    state_key = names(app_state)[1]
    state = app_state[[state_key]]

    mods_found = list()

    # We start with the preamble
    code_chunks = c(state[["yaml"]][["FM"]][["code"]][["gen_preamble"]], "\n")

    # This contains the modules that should generate code in the order in which the should be generated
    gen_mods = state[["yaml"]][["FM"]][["code"]][["gen_mods"]]
    # Now we walk through each module type
    for(gen_mod in names(gen_mods)){
      # Then we walk through each state key and pull the code
      # if it matchs the value in gen_mod
      for(state_key in names(app_state)){
        tmp_state = app_state[[state_key]]
        # This can be null if we're processing something other than a module
        # (e.g. notifications).
        MOD_TYPE  = tmp_state[["MOD_TYPE"]]
        if(!is.null(MOD_TYPE)){
          if(MOD_TYPE == gen_mod){
            MOD_FUNC  = paste0(MOD_TYPE, "_fetch_code")
            # We make sure the code generation function exists
            # and if it does we generate the code for that module
            if(exists(MOD_FUNC, mode="function")){
              tmp_code  = NULL
              FUNC_CALL = paste0("tmp_code = ", MOD_FUNC,"(tmp_state)")
          
          
              eval(parse(text=FUNC_CALL))
          
              if(!is.null(tmp_code)){
                # This adds a header the first time a module type is encountered
                if(!(MOD_TYPE %in% names(mods_found))){
                  code_chunks = c(code_chunks,  gen_mods[[gen_mod]])
                  mods_found[[MOD_TYPE]] = TRUE
                }
                code_chunks = c(code_chunks, tmp_code, "\n")
              }
            } else {
              msgs = c(msgs, paste0("Unable to find code fetching function: ", MOD_FUNC, "() for module type: ", MOD_TYPE))
            }
          }
        }
      }
    }
  } else {
    isgood = FALSE
    msgs   = "No modules found"
  }

  code = paste(code_chunks, collapse="\n")

  res = list(isgood = isgood,
             msgs   = msgs,
             code   = code)
res}

#'@export
#'@title Fetches the path to the log file
#'@description Use this to get the path to the formods log file
#'@param state module state after yaml read
#'@return Character string with the path to the log file.
#'@examples
#'# This function assumes that some module state exists:
#'state = UD_init_state(
#'  FM_yaml_file  = system.file(package = "formods",
#'                              "templates",
#'                              "formods.yaml"),
#'  MOD_yaml_file = system.file(package = "formods",
#'                              "templates",
#'                              "UD.yaml"),
#'  id = "UD")
#' FM_fetch_log_path(state)
FM_fetch_log_path = function(state){

  res = state[["yaml"]][["FM"]][["logging"]][["log_file"]]

  res = file.path(FM_fetch_user_files_path(state), res)
res}

#'@export
#'@title Fetches the path to the user files
#'@description Use this to get the path to the formods log file
#'@param state module state after yaml read
#'@return Character string with the path to the log file.
#'@examples
#' # We need a state object to use this function:
#' id="UD"
#' sess_res = UD_test_mksession(session=list(), id=id)
#' state = sess_res$state
#' user_dir = FM_fetch_user_files_path(state)
#' user_dir
FM_fetch_user_files_path = function(state){

  # Finding the path to the user directory:
  use_tmpdir = TRUE
  if(!is.null(state[["yaml"]][["FM"]][["user_"]][["use_tmpdir"]])){
    use_tmpdir = state[["yaml"]][["FM"]][["logging"]][["use_tmpdir"]]
  }

  if(use_tmpdir){
    user_dir = file.path(tempdir(), "FM")
  } else{
    user_dir = file.path(getwd(), "FM")
  }

  # Making sure the directory exits
  if(!dir.exists(user_dir)){
    dir.create(user_dir, recursive = TRUE)
  }

user_dir}

#'@export
#'@title Appends entry to log file
#'@description Add the supplied txt and the module type to the log file
#'@param state Module state after yaml read
#'@param entry Text to add
#'@param escape_braces Set to \code{TRUE} (default) to escape curly braces in the entry, set to \code{FALSE} to have the values interpreted.
#'@return NULL
#'@examples
#' # We need a module state to use this function:
#' id="UD"
#' sess_res = UD_test_mksession(session=list(), id=id)
#' state   = sess_res$state
#' FM_le(state, "This is a message")
FM_le = function(state, entry, escape_braces=TRUE){
  # pulling out the log file
  log_file = FM_fetch_log_path(state)

  isgood = NULL

  # If the log file does not exist we initalize it
  if(!file.exists(log_file)){
    file.create(log_file)
    write("for mods log init", file=log_file, append=TRUE)
  }

  # module type
  if(is.null(state[["MOD_TYPE"]])){
    mod_type = "UK"
  }else {
    mod_type = state[["MOD_TYPE"]]
  }

  # Appending the module type:
  entry = paste0(mod_type, ": ", entry)

  # Writing messages to the console
  if(state[["yaml"]][["FM"]][["logging"]][["console"]]){
    for(line in entry){
      if(escape_braces){
        cli::cli_alert("{line}")
      } else {
        cli::cli_alert(line)
      }
    }
  }

  # Appending the optional time stamp
  if(state[["yaml"]][["FM"]][["logging"]][["timestamp"]]){
     entry = paste0(format(Sys.time(),
                           state[["yaml"]][["FM"]][["logging"]][["timestamp_fmt"]]),
                    " ", entry)

  }

  # Appending the log entry to the log file
  isgood = write(entry, file=log_file, append=TRUE)

isgood}



#'@export
#'@title Run Try/Catch and Process Results
#'@description Attempts to execute the text in cmd. This is done in a
#'try/catch environment to capture any errors.
#'@param cmd    Character object containing the R command to evaluate in the try/catch block
#'@param tc_env list of with names corresponding to object names and
#'corresponding Values to define in the try/catch environment
#'@param capture Character vector of values to capture after the command is
#'successfully captured
#'@return list with the following fields:
#' \itemize{
#'   \item{isgood:} Boolean indicating the whether the evaluation was
#'   successful.
#'   \item{error:} If the evaluation failed this contains the error object.
#'   \item{msgs:} Character vector of messages and/or errors.
#'   \item{capture:} List with names of objects to be captured and values
#'   corresponding to those captured objects.
#' }
#'@examples
#' # Successful command
#' res_good = FM_tc("good_cmd=ls()", list(), c("good_cmd"))
#' res_good
#'
#' # Failed command
#' res_bad = FM_tc("bad_cmd =not_a_command()", list(), c("bad_cmd"))
#' res_bad
FM_tc = function(cmd, tc_env, capture){

  isgood = TRUE
  tcres  = list()
  msgs   = c()

  # Defining the environment
  for(name in names(tc_env)){
    assign(name, tc_env[[name]])
  }

  tcres = tryCatch({
    # Running the command
    eval(parse(text=cmd))
    # Capturing objects
    obj_cap = list()
    for(obj_name in capture){
      obj_cap[[obj_name]] = get(obj_name)
    }
    list(capture = obj_cap, isgood=TRUE)},
    error = function(e) {
      list(error=e, isgood=FALSE)}
  )


  # If there was an error we want to capture any messages from that here
  if(!tcres$isgood){
    if(!is.null(tcres[["error"]][["message"]])){
      msgs = c(msgs, paste0("message: ", tcres[["error"]][["message"]])) }
    if(!is.null(tcres[["error"]][["call"]])){
      msgs = c(msgs, paste0("call:    ", tcres[["error"]][["call"]])) }
  }



  tcres[["msgs"]] = msgs

tcres}


#'@export
#'@title Generates `ggplot` Object with Error Message
#'@description Takes a vector of messages and returns a ggplot object with the
#'text in the figure. This can be used in automated figure generation to
#'cascade an error message to the end user.
#'@param msgs Vector of error messages
#'@return ggplot object
#'@examples
#'FM_mk_error_fig("Oh nos! You've made a mistake!")
FM_mk_error_fig  <- function(msgs){
  p_res = ggplot()+annotate("text",
                   hjust= 0, vjust=1,
                   x=0, y=0,
                   label = paste(msgs, collapse="\n")) +
    xlab(NULL) + ylab(NULL)  + theme(axis.ticks = element_blank()) +
    scale_x_continuous(labels = NULL, limits = c(-.1,1))        +
    scale_y_continuous(labels = NULL, limits = c(-1,0))

p_res}

#'@export
#'@title Fetch the Module State
#'@description Fetches the module state from the userdata under the specified
#'id
#'@param session Shiny session variable.
#'@param id ID string for the module.
#'@return module state or NULL if it's not defined.
#'@examples
#' # We need a Shiny session variable to use this function:
#' id="UD"
#' sess_res = UD_test_mksession(session=list(), id=id)
#' session = sess_res$session
#' state = FM_fetch_mod_state(session, id)
FM_fetch_mod_state <- function(session,id){

  FM_ID = paste0("FM_", id)
  state = session$userData[["FM"]][[FM_ID]]

state}

#'@export
#'@title Set the Module State
#'@description Sets the module state from the userdata under the specified
#'id
#'@param session Shiny session variable
#'@param id ID string for the module.
#'@param state Module state to set.
#'@return NULL
#'@examples
#' # We need a Shiny session variable and a module state
#' # object to use this function:
#' id="UD"
#' sess_res = UD_test_mksession(session=list(), id=id)
#' session = sess_res$session
#' state   = sess_res$state
#' FM_set_mod_state(session, id, state)
FM_set_mod_state <- function(session,id,state){

  FM_ID = paste0("FM_", id)
  session$userData[["FM"]][[FM_ID]]=state

session}


#'@export
#'@title Set the App State
#'@description Takes a loaded app state and overwrites the current app state
#'@param session Shiny session variable.
#'@param app_state Loaded app state.
#'@param set_holds If TRUE (default) the holds will be set for all of the
#' modules present in the app state.
#'@return NULL
#'@examples
#' # We need a Shiny session object to use this function:
#' id="UD"
#' sess_res = UD_test_mksession(session=list(), id=id)
#' session = sess_res$session
#' app_state = FM_fetch_app_state(session)
#' FM_set_app_state(session, app_state)
FM_set_app_state <- function(session, app_state, set_holds = TRUE){


  # We want to pick up a state for reporting:
  state       = NULL 
  state_found = FALSE

  if(set_holds){
    for(mod_key in names(app_state)){
      # Current module type:
      MT = app_state[[mod_key]][["MOD_TYPE"]]
      # Walking through each ui element in the hold list and setting it to
      # TRUE


      # This ensures we only process state elements
      if(!is.null(MT)){
        # We're creating a state here so we can do messaging below
        # if necessary
        if(MT == "ASM"){
          state       = app_state[[mod_key]]
          state_found = TRUE
        }
        for(tmp_ui_hold in names(app_state[[mod_key]][[MT]][["ui_hold"]])){
          app_state[[mod_key]][[MT]][["ui_hold"]][[tmp_ui_hold]] = TRUE
        }
      }
    }
  }


  # Replacing the configuration files:
  # JMH before this is implemented the portion of the FG_init_state
  # where changes are made to "MC" needs to be fixed.
  if(state_found){
    if(length(names(app_state))>0){
      for(state_key in names(app_state)){
    
        # Pulling out the current state:
        tmp_state      = app_state[[state_key]]
        MOD_TYPE       = tmp_state[["MOD_TYPE"]]
    
        # This ensures we only process state elements
        if(!is.null(MOD_TYPE)){
          # By default we add the yaml contents
          add_yaml       = TRUE
          FM_yaml_file   = tmp_state[["FM_yaml_file"]]
          MOD_yaml_file  = tmp_state[["MOD_yaml_file"]]
    
          if(!file.exists(FM_yaml_file)){
            add_yaml = FALSE
            FM_le(state, paste0(MOD_TYPE, ", file not found:"))
            FM_le(state, paste0("  ", FM_yaml_file))
          }
          if(!file.exists(MOD_yaml_file)){
            add_yaml = FALSE
            FM_le(state, paste0(MOD_TYPE, ", file not found:"))
            FM_le(state, paste0("  ", MOD_yaml_file))
          }

          if(add_yaml){
            MOD_CONFIG =   yaml::read_yaml(MOD_yaml_file)
            app_state[[state_key]][["MC"]]   = MOD_CONFIG[["MC"]]
            app_state[[state_key]][["yaml"]] =  yaml::read_yaml(FM_yaml_file)
            FM_le(state, paste0("Config file loaded for module: ", MOD_TYPE))
          } else{
            FM_le(state, paste0("Unable to load config files for module: ", MOD_TYPE))
            FM_le(state, paste0("See above for details."))
          }
        }
      }
    }
  } else {
    cli::cli_alert("FM_set_app_state()")
    cli::cli_alert("Unable to find ASM state.")
  }


  # Replacing the app state in session:
  session$userData[["FM"]] = app_state


NULL}


#'@export
#'@title Fetches the App State
#'@description Returns the entire state of the App
#'@param session Shiny session variable.
#'@return App state or NULL if it's not defined.
#'@examples
#' # We need a Shiny session object to use this function:
#' id="UD"
#' sess_res = UD_test_mksession(session=list(), id=id)
#' session = sess_res$session
#' app_state = FM_fetch_app_state(session)
#' app_state
FM_fetch_app_state <- function(session){


  # Fetching the app state
  app_state = session$userData[["FM"]]

app_state}


#'@export
#'@title Initialize a formods State Object
#'@description Initializes a formods state object with common elements.
#'@param FM_yaml_file App configuration file with FM as main section.
#'@param MOD_yaml_file  Module configuration file with MC as main section.
#'@param id Shiny module ID.
#'@param MT Type of module using the short name (e.g. "UD", "FG", etc.).
#'@param button_counters Vector of button UI elements that need to be tracked.
#'@param ui_ids List of UI ids in the model.
#'@param ui_hold Vector of UI elements that require holding.
#'@param session Shiny session variable
#'@return List with state initialized.
#'@examples
#' # Within shiny a session variable will exist, 
#' # this creates examples here for testing purposes:
#' sess_res = UD_test_mksession(session=list())
#' session = sess_res$session
#'state = FM_init_state(
#'    FM_yaml_file  = system.file(package = "formods",
#'                                "templates",
#'                                "formods.yaml"),
#'    MOD_yaml_file = system.file(package = "formods",
#'                                "templates",
#'                                "UD.yaml"),
#'    id              = "UD",
#'    MT              = "UD",
#'    button_counters = NULL,
#'    ui_ids          = NULL,
#'    ui_hold         = NULL,
#'    session         = session)
#'
#' state
FM_init_state = function(
                      FM_yaml_file,
                      MOD_yaml_file,
                      id,
                      MT,
                      button_counters,
                      ui_ids,
                      ui_hold, 
                      session){

  state = list()

  # Reading in default information from the yaml file
  state[["yaml"]] = yaml::read_yaml(FM_yaml_file)

  # This assigns the module config "MC" element
  MOD_CONFIG = yaml::read_yaml(MOD_yaml_file)
  state[["MC"]] = MOD_CONFIG[["MC"]]

  # Initializing the button counters
  state[[MT]][["button_counters"]]    = list()
  for(tmp_bc in button_counters){
    state[[MT]][["button_counters"]][[tmp_bc]] = 0
  }

  #Initializing the ui_hold
  state[[MT]][["ui_hold"]]    = list()
  for(tmp_ui_hold in ui_hold){
    state[[MT]][["ui_hold"]][[tmp_ui_hold]] = FALSE
  }

  # This holds all the ui IDs from the interface
  state[[MT]][["ui_ids"]]    = ui_ids

  # Messaging passed back to the user
  state[[MT]][["ui_msg"]]    = NULL

  state[[MT]][["isgood"]]    = TRUE

  state[["MOD_TYPE"]]        = MT
  state[["id"]]              = id
  state[["FM_yaml_file"]]    = FM_yaml_file
  state[["MOD_yaml_file"]]   = MOD_yaml_file


state}


#'@export
#'@title Sets Message in State from UI Processing
#'@description Any errors that need to be passed back to the user can be set
#'with this function.
#'@param state formods State object.
#'@param msgs Character vector of messages.
#'@return state with ui message set.
#'@examples
#' # We need a module state object to use this function:
#' id="UD"
#' sess_res = UD_test_mksession(session=list(), id=id)
#' state = sess_res$state
#' state = FM_set_ui_msg(state, "Something happend.")
FM_set_ui_msg = function(state, msgs){

  MT = state[["MOD_TYPE"]]

  if(is.null(msgs)){
    state[[MT]][["ui_msg"]] = NULL
  } else {
    state[[MT]][["ui_msg"]] = paste(msgs, collapse = "\n")
  }

state}

#'@export
#'@title Fetches Details About Current Modules
#'@description Use this to get information about the currently supported
#'modules. This includes short names, UI elements,
#'@return list with details about the currently supported modules.
#'@examples
#' FM_fetch_current_mods()
FM_fetch_current_mods = function(){

  res = list(mods=list(), df=NULL)

  # Outlining the metadata for each module:
  res[["mods"]][["ASM"]] = list(
    Name       = "App State Mangement",
    Short_Name = "ASM",
    UI         =
      list(htmlOutput=c("ui_asm_save_name",
                        "ui_asm_save_button",
                        "ui_asm_load_state"),
            other    =c("ui_asm_msg",
                        "ui_asm_ace_code"))
  )
  res[["mods"]][["UD"]] = list(
    Name       = "Upload Data",
    Short_Name = "UD",
    UI         =
      list(htmlOutput=c("ui_ud_load_data",
                        "ui_ud_select_sheets",
                        "ui_ud_text_load_result",
                        "ui_ud_data_preview"),
            other=c("ui_ud_ace_code"))
  )
  res[["mods"]][["DW"]] = list(
    Name       = "Data Wrangling",
    Short_Name = "DW",
    UI         =
      list(htmlOutput=c("ui_dw_views",
                        "ui_dw_key",
                        "ui_dw_new_view",
                        "ui_dw_save_view",
                        "ui_dw_del_view",
                        "ui_dw_copy_view",
                        "ui_dw_add_element_button",
                        "ui_dw_select",
                        "ui_dw_new_element_row"
            ),
           other=c(     "hot_dw_elements",
                        "hot_data_preview",
                        "ui_dw_msg",
                        "ui_dw_code"))
  )
  res[["mods"]][["FG"]] = list(
    Name       = "Figure Generation",
    Short_Name = "FG",
    UI         =
      list(htmlOutput=c("ui_fg_curr_views",
                        "ui_fg_curr_figs",
                        "ui_fg_new_fig",
                        "ui_fg_save_fig",
                        "ui_fg_del_fig",
                        "ui_fg_copy_fig",
                        "ui_fg_fig_name",
                        "ui_fg_fig_notes",
                        "ui_fg_add_element_button",
                        "ui_fg_select",
                        "ui_fg_new_element_row",
                        "ui_fg_msg",
                        "ui_fg_slider_page"
            ),
           other=c("hot_fg_elements",
                   "ui_fg_preview_ggplot",
                   "ui_fg_msg",
                   "ui_fg_code"))
  )

  for(mn in names(res[["mods"]])){
    res[["df"]] =
      rbind(res[["df"]],
      data.frame(
      Module               = res[["mods"]][[mn]][["Name"]],
      SN                   = res[["mods"]][[mn]][["Short_Name"]],
      htmlOutput           = paste0(res[["mods"]][[mn]][["UI"]][["htmlOutput"]],          collapse=", "),
      otherOutput          = paste0(res[["mods"]][[mn]][["UI"]][["other"]],               collapse=", "))
      )

  }

  res}

#'@export
#'@title Generate Report
#'@description Generates a report from the states of the different modules.
#'The type of report is based on the file extension of file_name.
#'@param state Module state requesting the report generation
#'@param session Shiny session variable
#'@param file_dir  path to the location where the file should be written.
#'@param file_name base_filename (acceptable extensions are xlsx, docx, or pptx).
#'@param gen_code_only Boolean value indicating that only code should be
#'generated (\code{FALSE}).
#'@param rpterrors Boolean variable to generate reports with errors.
#'@return List with the following elements
#'@details
#'  This function will look through the loaded modules and find those with
#'  reporting enabbled. If reporting is enabled it will look for reporting functions
#'  for that module. Reporting functions should be of the following format
#'  (name and arguments):
#'
#'    \code{XX_append_report(state, rpt, rpttype)}
#'
#'   Where \code{XX} is the module short name. The state is the current state of the
#'   module. The rpt contains the current content of the report. This will
#'   vary based on the report type:
#'
#'\itemize{
#'  \item{xlsx:} List with two elements. The first is \code{summary} a data
#'  frame with two columns. The first column is called \code{Sheet_Name} and
#'  the second column is called \code{Description}. This is a catalog of
#'  sheets added to the report by the user and can be appended to using rbind.
#'  The second element in xlsx rpt is another list with element names
#'  corresponding to the report sheet names and the values corresponding to
#'  dataframes to be exported in the report.
#'  \item{pptx or docx:} Corresponding onbrand reporting object.
#'}
#'
#'@example inst/test_apps/FM_report.R
FM_generate_report = function(state,
                              session,
                              file_dir ,
                              file_name,
                              gen_code_only = FALSE,
                              rpterrors = TRUE){

  # Tracking whether we found any reporting elements.
  hasrptele    = FALSE

  # Pulling out the app state
  app_state    = FM_fetch_app_state(session)
  mod_rpt_info = NULL

  # This will hold the reporting code
  code = c()

  # Collecting the reporting details for each module
  for(MOD in names(app_state)){
    MODDATA = app_state[[MOD]]


    if(!is.null(MODDATA[["MC"]][["reporting"]])){
      # Constructing a data frame of each module and
      # the reporting information for that module
      mod_rpt_info =
      rbind(mod_rpt_info,
      data.frame(
        MOD_TYPE = MODDATA[["MOD_TYPE"]],
        id       = MODDATA[["id"]],
        priority = MODDATA[["MC"]][["reporting"]][["priority"]],
        enabled  = MODDATA[["MC"]][["reporting"]][["enabled"]]
      )
      )
    }
  }

  #finding the report type
  rpttype = "unknown"
  rpt = NULL

  # Reports will be initialized regardless gen_code_only this way the rpt
  # object will be present for below:
  if(stringr::str_detect(file_name, ".xlsx$")){
    code = state[["yaml"]][["FM"]][["reporting"]][["content_init"]][["xlsx"]]
    eval(parse(text=paste0(code, collapse="\n")))
    rpttype   = "xlsx" }
  if(stringr::str_detect(file_name, ".pptx$")){
    code = state[["yaml"]][["FM"]][["reporting"]][["content_init"]][["pptx"]]
    eval(parse(text=paste0(code, collapse="\n")))
    rpttype   = "pptx" }
  if(stringr::str_detect(file_name, ".docx$")){
    code = state[["yaml"]][["FM"]][["reporting"]][["content_init"]][["docx"]]
    eval(parse(text=paste0(code, collapse="\n")))
    rpttype   = "docx" }

  # Defaulting to success
  isgood = TRUE
  errmsg = ""

  # These are the allowed report types
  allowed_rpttypes = c("xlsx", "pptx", "docx")

  if(rpttype %in% allowed_rpttypes){
    # This looks at the summary table of the loaded modules and their
    # reporting status to determine if we need to generate a report
    if(!is.null(mod_rpt_info) & any(mod_rpt_info$enabled)){
      # Getting a vector of the different priorities sorted from highest to
      # lowest:
      priorities = rev(unique(sort(mod_rpt_info[["priority"]])))
      # Now we walk through the priorities
      for(tmp_priority in priorities){
        tmp_info = dplyr::filter(mod_rpt_info, .data[["priority"]] == tmp_priority)
        # next we walk through each row in the current priority level
        for(ridx in 1:nrow(tmp_info)){
          if(tmp_info[ridx,][["enabled"]]){
            tmp_MOD_TYPE = tmp_info[ridx,][["MOD_TYPE"]]
            tmp_id       = tmp_info[ridx,][["id"]]

            # Next we look to see if there is a report generation function
            MOD_FUNC  = paste0(tmp_MOD_TYPE, "_append_report")

            if(exists(MOD_FUNC, mode="function")){

              FM_le(state, paste0("  appending report for module:", tmp_MOD_TYPE, " id:", tmp_id, " priority:", tmp_priority))

              # We need the module state:
              tmp_state = FM_fetch_mod_state(session, tmp_id)

              # This is the function call used to append the report
              gen_rpt_res = NULL # this is to get around "no visible binding" NOTE
              FUNC_CALL = paste0("gen_rpt_res = ", MOD_FUNC,"(state = tmp_state, rpt=rpt, rpttype=rpttype, gen_code_only=gen_code_only)")

              # This will evaluate it and store the results in the gen_rpt_res
              eval(parse(text=FUNC_CALL))

              # JMH check gen_rpt_res$isgood for failure and add contengencies for failure

              # If reporting elements have been found
              if(gen_rpt_res[["hasrptele"]]){
                # we flag that we found reporting elements
                hasrptele = TRUE

                # We set the rpt to the rpt returned by the function
                rpt = gen_rpt_res[["rpt"]]

                # We append the code as well
                code    = c(code, gen_rpt_res[["code"]])
              }
            }
          }
        }
      }

      # If we made it here and there are no reportable elments
      # then we want to mark the status as bad
      if(!hasrptele){
        isgood = FALSE
        errmsg = paste0("No reportable content in the App yet. You need to do something.")
      }

      # If we've made it this far then we've appended all the reporting elements
      # and now we need to write the report:
      if(isgood){
        if(rpttype == "xlsx"){
          # This combins the summary and sheets together:
          rpt_list = NULL # this is to get around "no visible binding" NOTE
          code_chunk = c('rpt_list = append(',
                         '  list("Summary" = rpt[["summary"]]),',
                         '  rpt[["sheets"]])')
          eval(parse(text=code_chunk))
          code = c(code, code_chunk)

          # This is the main difference between the app and the exported code.
          # in the app we write to whichever location is specified:
          if(!gen_code_only){
            writexl::write_xlsx(rpt_list,
              path=file.path(file_dir, file_name))
          }
          # In the exported code we just write to the working directory:
          code = c(code, paste0('writexl::write_xlsx(rpt_list, path=file.path("reports", "report.', rpttype, '"))' ))
        }
        if(rpttype == "pptx" | rpttype=="docx"){
          # Saving the report on the app
          if(!gen_code_only){
            onbrand::save_report(rpt, file.path(file_dir, file_name))
          }

          # Code to save the report:
          code = c(code, paste0('onbrand::save_report(rpt, file.path("reports", "report.', rpttype,'"))'))
        }

      }
    } else{
      isgood = FALSE
      errmsg = paste0("No reportable modules found")
    }

  } else{
    isgood = FALSE
    errmsg = paste0("Invalid report type provided (", rpttype,"), allowed types:", paste(allowed_rpttypes, collapse=", "))
  }


  # If there was a failure up there somewhere we need to create a file with
  # the error so the user can have some idea of what went wrong.
  if(!isgood  & !gen_code_only){

    # We also want to return no code as well:
    code = c()

    # if we managed to find a state we also report the error
    # message and log it
    if(!is.null(state)){
     FM_le(state, errmsg)
    }

    # This will push the errors to the user in the document
    if(rpterrors){
      if(rpttype == "pptx"){
        # Creating a pptx document containing the errors
        # generated above. We use the rpt object created
        # above when the rpttype is determined:
        rpt = onbrand::report_add_slide(rpt,
        template = "content_text",
        elements = list(
           title         = list( content      = "Failed to generate report",
                                 type         = "text"),
           content_body  = list( content      = errmsg,
                                 type         = "text")))
        onbrand::save_report(rpt, file.path(file_dir, file_name))
      }
      if(rpttype == "docx"){
        # Creating a docx document containing the errors
        # generated above. We use the rpt object created
        # above when the rpttype is determined:
        rpt = onbrand::report_add_doc_content(rpt,
          type     = "text",
          content  = list(text=errmsg))
        onbrand::save_report(rpt, file.path(file_dir, file_name))
      }
      if(rpttype == "xlsx"){
        # This writes a document containing errors
        # so that they can be passed back to the user
        writexl::write_xlsx(
          list(Message = data.frame(Message = errmsg)),
          path=file.path(file_dir, file_name))
      }
    }
  }

  res = list(
    isgood = isgood,
    errmsg = errmsg,
    code   = code
  )

res}

#'@export
#'@title Shiny Notification
#'@description Generates a notification that should only show once.
#'@param state Module state generating the notification
#'@param session Shiny session variable
#'@return Boolean variable indicating if the notification was triggered
#'@example inst/test_apps/FM_notify.R
FM_notify = function(state, session){

  # current state id
  id = state[["id"]]

  if(system.file(package = "shinybusy") !=""){
    if("notifications" %in% names(state)){
      for(notify_id in names(state[["notifications"]])){

        # Defaulting to not notifying user
        notify_user = FALSE

        notify_text = state[["notifications"]][[notify_id]][["text"]]
        timestamp   = state[["notifications"]][[notify_id]][["timestamp"]]
        type        = state[["notifications"]][[notify_id]][["type"]]

        if(is.null(session$userData[["FM"]][["notifications"]][[id]][[notify_id]])){
          # This checks to see if this notification ID has been used yet
          notify_user = TRUE
        } else {
          # Now we check to see if this notification text has been sent to
          # the user yet. To do that we see if the contents of the notify_id
          # are the same or different from the notification text
          if(timestamp != session$userData[["FM"]][["notifications"]][[id]][[notify_id]]){
            notify_user = TRUE
          }
        }

        if(notify_user){
          # Default notify config
          cn = shinybusy::config_notify()

          # Loading formods
          if(type %in% names(state[["yaml"]][["FM"]][["notifications"]][["config"]])){
            tc_env = state[["yaml"]][["FM"]][["notifications"]][["config"]][[type]]
            cmd_args = paste(paste(names(tc_env), "=", names(tc_env)), collapse = ",\n ")
            cmd = paste("cn = shinybusy::config_notify(\n", cmd_args, ")")

            # How we run it in the try catch environment in case there are any errors
            tcres = FM_tc(cmd, tc_env, "cn")

            # if we were successful we pluck out the config_notify object
            if(tcres[["isgood"]]){
              cn = tcres[["capture"]][["cn"]]
            }else{
              # Otherwise we throw some errors
              FM_le(state, "Unable to extract the config_notify from yaml file")
              for(msg in tcres[["msgs"]]){
                FM_le(state, msg)
              }
            }
          } else {
            FM_le(state, paste0("Notification type >", type, "< not found using defaults."))
          }

          # Notifying the user
          shinybusy::notify(notify_text, cn)
          # Updating the text in the session variable to prevent further
          # notifications
          session$userData[["FM"]][["notifications"]][[id]][[notify_id]] = timestamp
        }
      }
    }
  }

NULL}

#'@export
#'@title Shiny Notification
#'@description Generates a notification that should only show once.
#'@param state Module state generating the notification
#'@param notify_id Unique string for this notification
#'@param notify_text Text to go in the notification
#'@param type - Can be either "success", "failure", "info" (default), or
#'"warning"
#'@return Module state with notification text set
#'@example inst/test_apps/FM_notify.R
FM_set_notification = function(state, notify_text, notify_id, type="info"){
  isgood = TRUE
  if( !is.character(notify_text) ){
    isgood = FALSE
    FM_le(state, "notify_text should be charater data.") }
  if( !is.character(notify_id) ){
    isgood = FALSE
    FM_le(state, "notify_id should be charater data.") }
  if(isgood){
    # Setting notification
    state[["notifications"]][[notify_id]][["text"]]      = notify_text
    state[["notifications"]][[notify_id]][["type"]]      = type
    state[["notifications"]][[notify_id]][["timestamp"]] = as.numeric(Sys.time())
  } else {
    FM_le(state, "Notification not set, see above for details.")
    FM_le(state, "FM_set_notification()")
  }

state}



