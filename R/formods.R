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
#'@importFrom writexl write_xlsx
.onAttach <- function(libname, pkgname){

  #------------------------------------
  # If all the suggested packages are found this will be true:
  suggested_found = TRUE
  packageStartupMessage("Loading formods")

  fcres = formods_check(verbose = FALSE)
  if(!fcres[["all_found"]]){
    packageStartupMessage("Missing suggested packages")
    for(pkg in fcres[["missing_pkgs"]]){
      packageStartupMessage(paste0(" - ",pkg))
    }
  }
}

#'@export
#'@title Checks `formods` Dependencies
#'@description  Looks at the suggested dependencies and checks to make sure
#'@param verbose Logical indicating if messages should be displayed
#'@return List with the following elements:
#' \itemize{
#'   \item{all_found:}    Boolean indicating if all packages were found
#'   \item{found_pkgs:}   Character vector of found packages
#'   \item{missing_pkgs:} Character vector of missing packages
#'}
#'@examples
#' fcres = formods_check()
formods_check <- function(verbose=TRUE){

  #------------------------------------
  # Checking for rxpackages
  # If all the suggested packages are found this will be true:
  suggested_found = TRUE
  if(verbose){
    mr = FM_message("Checking formods for suggested packages", entry_type="h1")
  }

  pkgs = c(
    "clipr",
    "covr",
    "devtools",
    "DT",
    "flextable",
    "ggpubr",
    "gtools",
    "here",
    "janitor",
    "knitr",
    "plotly",
    "prompter",
    "rmarkdown",
    "shinybusy",
    "shinydashboard",
    "testthat",
    "utils")

  pkg_found   = c()
  pkg_missing =  c()
  for(pkg in pkgs){
    if(!requireNamespace(pkg, quietly=TRUE)){
      if(verbose){
        mr = FM_message(paste0("missing ", pkg), entry_type="danger")
      }
      pkg_missing = c(pkg_missing, pkg)
      suggested_found = FALSE
    } else {
      if(verbose){
        mr = FM_message(paste0("found ", pkg), entry_type="success")
      }
      pkg_found   = c(pkg_found  , pkg)
    }
  }

  res = list(
    all_found     = suggested_found,
    found_pkgs    = pkg_found,
    missing_pkgs  = pkg_missing
  )
res}



#'@export
#'@title Fetches Datasets from Modules in the App
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
                                # NULL returns numeric length zero
  if(any(is.na(ui_input_num)) | (length(ui_input_num) == 0)){
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

  # Detecting length differences
  if(length(ui_val) != length(old_val)){
    res = TRUE
  } else if((length(ui_val) == length(old_val)) &
             length(ui_val) > 1){
    # here we're comparing vectors
    if(!all(ui_val %in% old_val)){
     res = TRUE
    }
  }

  # here we're comparing scalers
  if((length(ui_val)  == 1) &
     (length(old_val) == 1) &
      !res
     ){
    if(!is.null(ui_val)){
      if(ui_val    != 0 & ui_val   !=init_value){
        if(ui_val    != old_val){
          res = TRUE
        }
      }
    }
  }
res}

#'@export
#'@title Detect if a UI element has updated
#'@description Takes a UI element value and an older value and determines if
#'it has been modified
#'@param ui_val     Current value from the UI.
#'@param old_val    Last value of of the element.
#'defined.
#'@return Boolean result of the comparison
#'@examples
#' changed_true  = has_updated(ui_val = "a", old_val = "")
#' changed_true
#' changed_false = has_updated(ui_val = "a", old_val = "a")
#' changed_false
has_updated = function(ui_val     = NULL,
                       old_val    = NULL){
  res = FALSE

  # Detecting length differences
  if(length(ui_val) != length(old_val)){
    res = TRUE
  } else if((length(ui_val) == length(old_val)) &
             length(ui_val) > 1){
    # here we're comparing vectors
    if(!all(ui_val %in% old_val)){
     res = TRUE
    }
  } else {
    # here we're comparing scalers
    if((length(ui_val)  == 1) &
       (length(old_val) == 1)){
      if(ui_val != old_val){
        res = TRUE
        #message(paste0("old_val: ", old_val))
        #message(paste0("ui_val: ",  ui_val))
      }
    } else {
      message("Unknown scenario has_updated:")
      message(paste0("old_val: ", old_val))
      message(paste0("ui_val: ",  ui_val ))
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
#'@return No return value, called to remove holds.
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
      FM_le(state     = state,
            entry     = paste0("Unable to set hold for unknown inputId: ", inputId),
            entry_type="danger")
      #cli::cli_alert_danger(paste0("Unable to set hold for unknown inputId: ", inputId))
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
#'@example inst/test_apps/FM_holds.R
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
#'@title Fetches the Code to Reproduce Analysis
#'@description Takes the current state of the app and builds a script to
#'reproduce the analysis within the app.
#'@param session Shiny session variable
#'@param state module state after yaml read
#'@param mod_ids Vector of module IDs and order they are needed (used for code generation).
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
#' sess_res = DW_test_mksession(session=list())
#' session = sess_res$session
#' state   = sess_res$state
#' app_code = FM_fetch_app_code(session = session,
#'                              state   = state,
#'                              mod_ids = c("UD", "DW"))
#' cat(app_code$code)
FM_fetch_app_code = function(session, state, mod_ids){
  isgood          = TRUE
  msgs            = c()
  code            = ""
  code_chunks     = c()
  #preamble_chunks = c()

  app_state = FM_fetch_app_state(session)

  if(length(names(app_state))>0){

    mods_found = list()

    # We start with the preamble
    code_chunks = c(state[["yaml"]][["FM"]][["code"]][["gen_preamble"]])

    # Finding the packages used by each module and creating
    app_packages = c()
    for(tmp_mod_key in names(app_state)){
      app_packages = c(app_packages,
        app_state[[tmp_mod_key]][["MC"]][["code"]][["packages"]])
    }

    app_packages = sort(unique(app_packages))
    app_packages = paste0('library("',app_packages,'")')

    code_chunks = c(code_chunks, app_packages, "\n")

    # Creating a word reporting object to use generate elements
    code_chunks = c(code_chunks,
                    "# This reporting object has the formatting  ",
                    "# information for table generation",
                    state[["yaml"]][["FM"]][["reporting"]][["content_init"]][["docx"]],
                    "")

    # Now we walk through each module type
    for(gen_mod in mod_ids){
      # Then we walk through each state key and pull the code
      # if it matchs the value in gen_mod
      for(state_key in names(app_state)){
        tmp_state = app_state[[state_key]]
        # This can be null if we're processing something other than a module
        # (e.g. notifications).
        MOD_TYPE  = tmp_state[["MOD_TYPE"]]
        if(!is.null(MOD_TYPE)){
          if(MOD_TYPE == gen_mod){

           ## Getting the preamble code. This just collects it in
           ## preamble_chunks and will be appended to the top down below.
           #mod_deps = FM_fetch_deps(state=tmp_state, session = session)
           #if("package_code" %in% names(mod_deps)){
           #  preamble_chunks = c(preamble_chunks, mod_deps$package_code)
           #}

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
                  code_chunks = c(code_chunks,   tmp_state[["MC"]][["code"]][["preamble"]])
                  mods_found[[MOD_TYPE]] = TRUE
                }
                code_chunks = c(code_chunks, tmp_code, "")
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

 ## If there are preamble elements we prepend
 ## those to the code_chunks
 #if(!is.null(preamble_chunks)){
 #  preamble_chunks = sort(unique(preamble_chunks))
 #  code_chunks = c(preamble_chunks, "", code_chunks)
 #}

  code = paste(code_chunks, collapse="\n")

  res = list(isgood = isgood,
             msgs   = msgs,
             code   = code)
res}

#'@export
#'@title Fetches the Path to the Log File
#'@description Use this to get the path to the formods log file
#'@param state module state after yaml read
#'@return Character string with the path to the log file.
#'@examples
#' # Within shiny a session variable will exist,
#' # this creates one here for testing purposes:
#' sess_res = UD_test_mksession(session=list())
#' session = sess_res$session
#'# This function assumes that some module state exists:
#'state = UD_init_state(
#'  FM_yaml_file  = system.file(package = "formods",
#'                              "templates",
#'                              "formods.yaml"),
#'  MOD_yaml_file = system.file(package = "formods",
#'                              "templates",
#'                              "UD.yaml"),
#'  id = "UD",
#'  session = session)
#' FM_fetch_log_path(state)
FM_fetch_log_path = function(state){

  res = state[["yaml"]][["FM"]][["logging"]][["log_file"]]

  res = file.path(FM_fetch_user_files_path(state), res)
res}

#'@export
#'@title Fetches the Path to the User Files
#'@description Use this to get the path to the temporary directory where formods stores user files.
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
    user_dir = file.path(tempdir(), state[["shiny_token"]], "FM")
  } else{
    user_dir = file.path(getwd(), state[["shiny_token"]], "FM")
  }

  # Making sure the directory exits
  if(!dir.exists(user_dir)){
    dir.create(user_dir, recursive = TRUE)
  }

user_dir}

#'@export
#'@title Adds Message to Log File and Displays it to the Console
#'@description Add the supplied txt and the module type to the log file and
#'display it to the console.
#'@param state Module state after yaml read
#'@param entry Text to add
#'@param escape_braces Set to \code{TRUE} (default) to escape curly braces in the entry, set to \code{FALSE} to have the values interpreted.
#'@param entry_type  Set to either "alert"(default), "danger", "info", "success", or "warning"
#'@return Boolean value indicating success (\code{TRUE}) or failure (\code{FALSE}).
#'@examples
#' # We need a module state to use this function:
#' id="UD"
#' sess_res = UD_test_mksession(session=list(), id=id)
#' state   = sess_res$state
#' FM_le(state, "This is a normal  message")
#' FM_le(state, "This is a danger  message", entry_type="danger")
#' FM_le(state, "This is a info    message", entry_type="info")
#' FM_le(state, "This is a success message", entry_type="success")
#' FM_le(state, "This is a warning message", entry_type="warning")
FM_le = function(state, entry, escape_braces=TRUE, entry_type="alert"){
  # pulling out the log file
  log_file = FM_fetch_log_path(state)

  isgood = NULL

  # If the log file does not exist we initalize it
  if(!file.exists(log_file)){
    file.create(log_file)
    write("formods log init", file=log_file, append=TRUE)
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
      FM_message(line=line,
                 escape_braces=escape_braces,
                 entry_type=entry_type)
      # This will conditionally show the entry if the cli packages is present:
     #if(system.file(package="cli") != ""){
     #  if(escape_braces){
     #    if(entry_type=="alert"){
     #      cli::cli_alert("{line}") }
     #    if(entry_type=="danger"){
     #      cli::cli_alert_danger("{line}") }
     #    if(entry_type=="warning"){
     #      cli::cli_alert_warning("{line}") }
     #    if(entry_type=="info"){
     #      cli::cli_alert_info("{line}") }
     #    if(entry_type=="success"){
     #      cli::cli_alert_success("{line}") }
     #  } else {
     #    if(entry_type=="alert"){
     #      cli::cli_alert(line)}
     #    if(entry_type=="danger"){
     #      cli::cli_alert_danger(line)}
     #    if(entry_type=="warning"){
     #      cli::cli_alert_warning(line)}
     #    if(entry_type=="info"){
     #      cli::cli_alert_info(line)}
     #    if(entry_type=="success"){
     #      cli::cli_alert_success(line)}
     #  }
     #} else {
     #  message(line)
     #}
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
#'@title Show Message to User
#'@description Writes a message to the console depending on whether cli is
#'installed or not.
#'@param line  Text to display
#'@param escape_braces Set to \code{TRUE} (default) to escape curly braces in the entry, set to \code{FALSE} to have the values interpreted.
#'@param entry_type  Set to either "alert"(default), "danger", "info", "success", "warning", "h1", "h2", or "h3"
#'@return Returns NULL
#'@examples
#' mr = FM_message("This is a normal  message")
#' mr = FM_message("This is a danger  message", entry_type="danger")
#' mr = FM_message("This is a info    message", entry_type="info")
#' mr = FM_message("This is a success message", entry_type="success")
#' mr = FM_message("This is a warning message", entry_type="warning")
#' mr = FM_message("This is an H1 header",      entry_type="h1")
#' mr = FM_message("This is an H2 header",      entry_type="h2")
#' mr = FM_message("This is an H3 header",      entry_type="h3")
FM_message = function(line, escape_braces=TRUE, entry_type="alert"){
  if(is_installed("cli") != ""){
    if(escape_braces){
      if(entry_type=="alert"){
        cli::cli_alert("{line}") }
      if(entry_type=="danger"){
        cli::cli_alert_danger("{line}") }
      if(entry_type=="warning"){
        cli::cli_alert_warning("{line}") }
      if(entry_type=="info"){
        cli::cli_alert_info("{line}") }
      if(entry_type=="success"){
        cli::cli_alert_success("{line}") }
      if(entry_type=="h1"){
        cli::cli_h1("{line}") }
      if(entry_type=="h2"){
        cli::cli_h2("{line}") }
      if(entry_type=="h3"){
        cli::cli_h3("{line}") }
    } else {
      if(entry_type=="alert"){
        cli::cli_alert(line)}
      if(entry_type=="danger"){
        cli::cli_alert_danger(line)}
      if(entry_type=="warning"){
        cli::cli_alert_warning(line)}
      if(entry_type=="info"){
        cli::cli_alert_info(line)}
      if(entry_type=="success"){
        cli::cli_alert_success(line)}
      if(entry_type=="h1"){
        cli::cli_h1(line)}
      if(entry_type=="h2"){
        cli::cli_h2(line)}
      if(entry_type=="h3"){
        cli::cli_h3(line)}
    }
  } else {
    message(line)
  }
NULL}

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
#'@return Session variable with the module state set.
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
#'@return No return value, just updates the app state in the session variable.
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
    FM_message(line="FM_set_app_state()")
    FM_message(line="Unable to find ASM state.")
  }


  # Replacing the app state in session:
  session$userData[["FM"]] = app_state


NULL}



#'@export
#'@title Fetches Informaiton About the App
#'@description Returns diagnostic information about the app
#'@param session Shiny session variable.
#'@return List with information about the app with the following structure
#' \itemize{
#'   \item{uiele:} All system information as UI elements to be used in shiny apps.
#'   \item{uiele_packages:} UI element for installed packages to be used in shiny apps.
#'   \item{uiele_options:}  UI element for current options.
#'   \item{uiele_modules: } UI element for loaded formods modules to be used in shiny apps.
#'   \item{msgs:}  System information as text to be used in a report/terminal.
#'   \item{si_packages} Dataframe with currently used packages.
#'   \item{si_options} Dataframe with current options
#' }
#'@examples
#' # We need a Shiny session object to use this function:
#' id="UD"
#' sess_res = UD_test_mksession(session=list(), id=id)
#' session = sess_res$session
#' app_info  = FM_fetch_app_info(session)
#' app_info$msgs
FM_fetch_app_info <- function(session){
  msgs            = c()
  uiele           = NULL
  uiele_packages  = NULL
  uiele_modules   = NULL
  si_packages     = NULL

  # The devtools package is needed for some information we want to find out if
  # it's here and create a message if it's not
  if(system.file(package="devtools") == ""){
    found_devtools = FALSE
    tmp_msg = "The devtools package was not found, some app information will not be reported."
    msgs =  c(msgs, tmp_msg)
    uiele = tagList(uiele, tags$em(tmp_msg))
  } else {
    found_devtools = TRUE
  }

  if(system.file(package="DT") == ""){
    found_DT   = FALSE
    tmp_msg    = "The DT package was not found, some app information will not be reported."
    msgs =  c(msgs, tmp_msg)
    uiele = tagList(uiele, tags$em(tmp_msg))
  } else {
    found_DT = TRUE
  }


  # Adding modules
  app_state = FM_fetch_app_state(session)
  if(length(names(app_state))>0){
    uiele = tagList(uiele, tags$h3("Modules"))
    msgs  = c(msgs, "Modules")
  }

  for(MODDATA in names(app_state)){

    # This pulls out the current module state:
    state   = FM_fetch_mod_state(session = session, id =app_state[[MODDATA]][["id"]])

    if(!is.null(state)){
      tmp_msg = paste0("ID: ",state[["id"]])
      uiele_modules   = tagList(uiele_modules, tags$h4(tmp_msg))
      msgs    = c(msgs, tmp_msg)

      tmp_msg = paste0("type: ",state[["MOD_TYPE"]])
      uiele_modules   = tagList(uiele_modules,tags$ul(tags$li(tmp_msg)))
      msgs    = c(msgs, tmp_msg)

      tmp_msg = paste0("FM_yaml_file: ",state[["FM_yaml_file"]])
      uiele_modules   = tagList(uiele_modules,tags$ul(tags$li(tmp_msg)))
      msgs    = c(msgs, tmp_msg)

      tmp_msg = paste0("MOD_yaml_file: ",state[["MOD_yaml_file"]])
      uiele_modules   = tagList(uiele_modules,tags$ul(tags$li(tmp_msg)))
      msgs    = c(msgs, tmp_msg)

      tmp_msg = paste0("User files: ",FM_fetch_user_files_path(state))
      uiele_modules   = tagList(uiele_modules,tags$ul(tags$li(tmp_msg)))
      msgs    = c(msgs, tmp_msg)

      tmp_msg = paste0("Log file: ",FM_fetch_log_path(state))
      uiele_modules   = tagList(uiele_modules,tags$ul(tags$li(tmp_msg)))
      msgs    = c(msgs, tmp_msg)

      # Finding the package dependencies of the current module
      deps = FM_fetch_deps(state=state, session=session )
      if(length(deps[["packages"]]) > 0){
        tmp_msg = paste0("Package dependencies: ", paste0(deps[["packages"]], collapse=', '))
        uiele_modules   = tagList(uiele_modules,tags$ul(tags$li(tmp_msg)))
        msgs    = c(msgs, tmp_msg)
      }
    }
  }

  uiele = tagList(uiele, uiele_modules)

  if(found_devtools){
    si          =  devtools::session_info()
    si_packages = si$packages
    msgs = c(msgs, as.character(si_packages))
    if(found_DT){
      uiele            = tagList(uiele, tags$h3("Packages"))
      uiele            = tagList(uiele,DT::datatable(si_packages))
      uiele_packages   = DT::datatable(si_packages)
    }
  }

  si_opt_list   = options()
  si_options    = NULL
  uiele_options = NULL
  for(oname in names(si_opt_list)){

    value  = deparse(si_opt_list[[oname]])
    value  = paste0(value, collapse = "\n")
    si_options  =
      rbind(si_options,
        data.frame(option = oname,
                   value  = value ))
  }


  if(found_DT){
    tmp_si_options = si_options
    tmp_si_options[["value"]] = str_replace_all(
      tmp_si_options[["value"]],
      pattern     = "\n",
      replacement = "<BR/>")
    tmp_si_options[["value"]] = str_replace_all(
      tmp_si_options[["value"]],
      pattern = " ",
      replacement = "&nbsp;")
    tmp_si_options[["value"]] = paste0(
      "<TT>",
      tmp_si_options[["value"]],
      "<TT>" )
    tmp_si_options[["option"]] = paste0(
      "<TT>",
      tmp_si_options[["option"]],
      "<TT>" )
    uiele_options = DT::datatable(tmp_si_options, escape=FALSE)
  }

  res = list(uiele          = uiele,
             uiele_packages = uiele_packages,
             uiele_modules  = uiele_modules ,
             uiele_options  = uiele_options ,
             msgs           = msgs,
             si_options     = si_options,
             si_packages    = si_packages)

res}

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
#'@param dep_mod_ids Vector of module ids this module depends on.
#'@param MT Type of module using the short name (e.g. "UD", "FG", etc.).
#'@param button_counters Vector of button UI elements that need to be tracked.
#'@param ui_ids List of UI ids in the module.
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
                      dep_mod_ids = c(),
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

 ## This tracks if the ui_id has been initialized or not:
 #for(tmp_ui_id in ui_ids){
 #  state[[MT]][["ui_ids_init"]][[tmp_ui_id]] = FALSE
 #}

  # Messaging passed back to the user
  state[[MT]][["ui_msg"]]    = NULL

  state[[MT]][["isgood"]]    = TRUE

  state[["MOD_TYPE"]]        = MT
  state[["id"]]              = id
  state[["dep_mod_ids"]]     = dep_mod_ids
  state[["FM_yaml_file"]]    = FM_yaml_file
  state[["MOD_yaml_file"]]   = MOD_yaml_file

  # If we're not in a shiny environment then
  # the token will ne NULL otherwise it will
  # be a checksum
  if(is.null(session$token)){
    state[["shiny_token"]]     = "non_shiny"
  } else {
    state[["shiny_token"]]     = session$token
  }

  # Copying yaml files to the user dir so they will be
  # available for export scripts
  user_dir   = FM_fetch_user_files_path(state)
  config_dir = file.path(user_dir, "config")
  if(!dir.exists(config_dir)){
    dir.create(config_dir, recursive=TRUE)
  }
  file.copy(FM_yaml_file,  config_dir, overwrite=TRUE)
  file.copy(MOD_yaml_file, config_dir, overwrite=TRUE)


  FM_proc_include(state, session)

state}

#'@export
#'@title Sets Message in State from UI Processing
#'@description Any errors that need to be passed back to the user can be set
#'with this function.
#'@param state formods State object.
#'@param session Shiny session variable.
#'@return No return value, sets message in supplied session variable.
#'@examples
#' # We need a module state object to use this function:
#' id="UD"
#' sess_res = UD_test_mksession(session=list(), id=id)
#' state = sess_res$state
#' session = sess_res$session
#' FM_proc_include(state, session)
FM_proc_include = function(state, session){
  # pulling out the user directory and defining the config direcotry
  user_dir   = FM_fetch_user_files_path(state)

  #--------------------------------------------------------
  # First we process the formods general includes
  # Going through the include files:
  proc_include = FALSE
  if(is.null(session$userData[["FM"]][["proc_include"]][["formods"]])){
    proc_include = TRUE
  } else if(!session$userData[["FM"]][["proc_include"]][["formods"]]){
    proc_include = TRUE
  }

  if(proc_include){
    if(!is.null(state[["yaml"]][["FM"]][["include"]][["files"]])){
      for(fidx in 1:length(state[["yaml"]][["FM"]][["include"]][["files"]])){
        fsource = state[["yaml"]][["FM"]][["include"]][["files"]][[fidx]][["file"]][["source"]]
        tc_src_res = FM_tc(cmd=paste0("fpath = ", fsource), tc_env = list(), capture = "fpath")
        fdest   = state[["yaml"]][["FM"]][["include"]][["files"]][[fidx]][["file"]][["dest"]]
        tc_dst_res = FM_tc(cmd=paste0("fpath = ", fdest), tc_env = list(), capture = "fpath")
        proc_curr_file = TRUE
        if(!tc_src_res[["isgood"]]){
          proc_curr_file = FALSE
          FM_le(state, paste0("Could not include: ", fsource))
          FM_le(state, tc_src_res[["msgs"]])
        }
        if(!tc_dst_res[["isgood"]]){
          proc_curr_file = FALSE
          FM_le(state, paste0("Could not include: ", fdest))
          FM_le(state, tc_dst_res[["msgs"]])
        }

        if(proc_curr_file){
          FM_le(state, "including file")
          FM_le(state, paste0("  source: ", fsource))
          FM_le(state, paste0("  dest:   ", fdest))
          fc_res = file.copy(
             from      = tc_src_res[["capture"]][["fpath"]],
             to        = file.path(user_dir, tc_dst_res[["capture"]][["fpath"]]),
             overwrite = TRUE)
        }
      }
    }
  }

  # Setting the bit to prevent multiple includes
  session$userData[["FM"]][["proc_include"]][["formods"]] = TRUE
  #--------------------------------------------------------
  MOD_ID = state[["id"]]
  proc_include = FALSE
  if(is.null(session$userData[["FM"]][["proc_include"]][[MOD_ID]])){
    proc_include = TRUE
  } else if(!session$userData[["FM"]][["proc_include"]][[MOD_ID]]){
    proc_include = TRUE
  }
  if(proc_include){
    if(!is.null(state[["yaml"]][["MC"]][["include"]][["files"]])){
      for(fidx in 1:length(state[["yaml"]][["MC"]][["include"]][["files"]])){
        fsource = state[["yaml"]][["MC"]][["include"]][["files"]][[fidx]][["file"]][["source"]]
        tc_src_res = FM_tc(cmd=paste0("fpath = ", fsource), tc_env = list(), capture = "fpath")
        fdest   = state[["yaml"]][["MC"]][["include"]][["files"]][[fidx]][["file"]][["dest"]]
        tc_dst_res = FM_tc(cmd=paste0("fpath = ", fdest), tc_env = list(), capture = "fpath")
        proc_curr_file = TRUE
        if(!tc_src_res[["isgood"]]){
          proc_curr_file = FALSE
          FM_le(state, paste0("Could not include: ", fsource))
          FM_le(state, tc_src_res[["msgs"]])
        }
        if(!tc_dst_res[["isgood"]]){
          proc_curr_file = FALSE
          FM_le(state, paste0("Could not include: ", fdest))
          FM_le(state, tc_dst_res[["msgs"]])
        }

        if(proc_curr_file){
          FM_le(state, "including file")
          FM_le(state, paste0("  source: ", fsource))
          FM_le(state, paste0("  dest:   ", fdest))
          fc_res = file.copy(
             from      = tc_src_res[["capture"]][["fpath"]],
             to        = file.path(user_dir, tc_dst_res[["capture"]][["fpath"]]),
             overwrite = TRUE)
        }
      }
    }
  }

  session$userData[["FM"]][["proc_include"]][[MOD_ID]] = TRUE
}

#'@export
#'@title Sets Message in State from UI Processing
#'@description Any errors that need to be passed back to the user can be set
#'with this function.
#'@param state formods State object.
#'@param msgs Character vector of messages.
#'@param append When \code{TRUE}, msgs will be appended to any current messages. When
#'\code{FALSE} (default) msgs will replace any existing messaages.
#'@return state with ui message set.
#'@examples
#' # We need a module state object to use this function:
#' id="UD"
#' sess_res = UD_test_mksession(session=list(), id=id)
#' state = sess_res$state
#' state = FM_set_ui_msg(state, "Something happend.")
FM_set_ui_msg = function(state, msgs, append=FALSE){

  MT = state[["MOD_TYPE"]]

  if(append){
    state[[MT]][["ui_msg"]] = paste(state[[MT]][["ui_msg"]], msgs, collapse="\n")
  } else {
    state[[MT]][["ui_msg"]] = paste(msgs, collapse="\n")
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
#'@param ph List containing placeholders used when generating Word documents
#'(e.g., \code{ph = list(HEADERRIGHT = "My text"}).
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
                              ph = list(),
                              gen_code_only = FALSE,
                              rpterrors = TRUE){

  # Tracking whether we found any reporting elements.
  hasrptele    = FALSE

  # Pulling out the app state
  app_state    = FM_fetch_app_state(session)
  mod_rpt_info = NULL

  # Changing the working directory to the user directory
  current_dir = getwd()
  user_dir    = FM_fetch_user_files_path(state)
  setwd(user_dir)
  on.exit(setwd(current_dir))

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
      FM_le(state, paste0("  Generating report: ", rpttype))
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

              FM_le(state, paste0("    appending report for module:", tmp_MOD_TYPE, " id:", tmp_id, " priority:", tmp_priority))

              # We need the module state:
              tmp_state = FM_fetch_mod_state(session, tmp_id)

              # This is the function call used to append the report
              gen_rpt_res = NULL # this is to get around "no visible binding" NOTE
              FUNC_CALL = paste0("gen_rpt_res = ", MOD_FUNC,"(state = tmp_state, rpt=rpt, rpttype=rpttype, gen_code_only=gen_code_only)")

              # This will evaluate it and store the results in the gen_rpt_res
              #eval(parse(text=FUNC_CALL))
              tcres = FM_tc(
                cmd    = FUNC_CALL,
                tc_env = list(tmp_state     = tmp_state,
                              gen_code_only = gen_code_only,
                              rpt           = rpt,
                              rpttype       = rpttype),
                capture = c("gen_rpt_res"))

              if(tcres[["isgood"]]){
                gen_rpt_res = tcres[["capture"]][["gen_rpt_res"]]
              } else {
                gen_rpt_res = list(hasrptele = FALSE)
                FM_le(state,"FM_report() failure")
                FM_le(state,paste0("  - Module function: ", MOD_FUNC))
                FM_le(state,tcres[["msgs"]])
              }

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

        # Adding placeholder substitution to Word documents
        if(rpttype=="docx"){

          if(length(state[["yaml"]][["FM"]][["reporting"]][["phs"]]) >0){
            code = c(code, "# Adding placeholder information")
          }


          # Here we do any placeholder substitution if necessary
          if(!is.null(state[["yaml"]][["FM"]][["reporting"]][["phs"]])){
            for(phidx in 1:length(state[["yaml"]][["FM"]][["reporting"]][["phs"]])){
              cph = state[["yaml"]][["FM"]][["reporting"]][["phs"]][[phidx]]
              tmp_name     = cph[["name"]]
              tmp_location = cph[["location"]]
              tmp_value    = cph[["value"]]

              # This will overwrite the defaults with any user specified values:
              if(length(ph) > 0){
                if(tmp_name %in% names(ph)){
                  tmp_value = ph[[tmp_name]]
                }
              }

              # Code to
              code_chunk = c(
                paste0('rpt  = onbrand::report_add_doc_content(rpt,                '),
                paste0('  type     = "ph",                                         '),
                paste0('  content  = list(name     = ',deparse(tmp_name),     ',   '),
                paste0('                  location = ',deparse(tmp_location), ',   '),
                paste0('                  value    = ',deparse(tmp_value),    '))  '),
                ""
              )

              # Evaluating th eplaceholders if we're actually generating the
              # report
              if(!gen_code_only){
                eval(parse(text=paste0(code_chunk,collapse="\n")))
              }

              # Appending it to the code
              code = c(code, code_chunk)
            }
          }
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


  # Changing the working directory back to the current directory
  setwd(current_dir)
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


#'@export
#'@title Starts Modal Screen Pause
#'@description Start a modal screen pause.
#'@param state Current module state after yaml file has been read.
#'@param session Shiny session variable.
#'@param message Optional message for the pause.
#'@return Pauses the screen and has no return value.
#'@examples
#' # We need a module state object and Shiny session objects to use this function:
#' sess_res = UD_test_mksession(session=list())
#' session = sess_res$session
#' state = sess_res$state
#' FM_pause_screen(state, session)
#' FM_resume_screen(state, session)
FM_pause_screen = function(state, session, message){

  if((any(c("ShinySession", "session_proxy") %in% class(session)))){
    if(system.file(package = "shinybusy") !=""){
     shinybusy::show_modal_spinner(text=message, session=session)
    }
  }
NULL}

#'@export
#'@title Creates Formatting Information for Datasets
#'@description Takes a data frame and information in the site configureation
#'to produce formatting information to make it easier for the user to see data
#'type information.
#'@param df Raw dataframe to be built into an rhandsontable.
#'@param state Current module state after yaml file has been read.
#'@return list with the following elements:
#' \itemize{
#'   \item{col_heads:} List (element for each column) of formatting
#'    information for column  headers to be  use with rhandsontable.
#'   \item{col_subtext:} List (element for each column) of subtext to
#'    be displayed in selections using `pickerInput` from the `shinyWidgets` package.
#' }
#'@examples
#' # We need a module state object to use this function:
#' sess_res = UD_test_mksession(session=list())
#' state = sess_res$state
#'
#' data_file_local =  system.file(package="formods", "test_data", "TEST_DATA.xlsx")
#' sheet           = "DATA"
#'
#' df = readxl::read_excel(path=data_file_local, sheet=sheet)
#'
#' hfmt = FM_fetch_data_format(df, state)
#'
#' # Column header formatting
#' head(as.vector(unlist( hfmt[["col_heads"]])))
#'
#' # Column select subtext
#' head(as.vector(unlist( hfmt[["col_subtext"]])))
FM_fetch_data_format = function(df, state){
  col_heads   = list()
  col_subtext = list()
  col_info    = list()
  for(cname in names(df)){

    # This pulls out the current column type (numeric, character, etc)
    ctype = typeof(df[[cname]])

    cfactor = is.factor(df[[cname]])

    # These are the unique sorted column elements
    col_ele = FM_pretty_sort(unique(df[[cname]]))
    if(length(col_ele) > 3){
      crange = paste0(col_ele[1],
                      state[["yaml"]][["FM"]][["data_meta"]][["many_sep"]],
                      col_ele[length(col_ele)])
    } else {
      crange = paste0(col_ele, collapse=", ")
    }

    # If column type has not been defined then we default to "other"
    if(!(ctype %in% names(state[["yaml"]][["FM"]][["data_meta"]][["data_types"]]))){
      ctype = "other"
    }

    ccolor = state[["yaml"]][["FM"]][["data_meta"]][["data_types"]][[ctype]][["color"]]
    clab   = state[["yaml"]][["FM"]][["data_meta"]][["data_types"]][[ctype]][["label"]]

    # Building the header:
    new_span = state[["yaml"]][["FM"]][["data_meta"]][["data_header"]]
    new_span = stringr::str_replace_all(new_span, "===COLOR===",  ccolor)
    new_span = stringr::str_replace_all(new_span, "===NAME===",   cname)
    new_span = stringr::str_replace_all(new_span, "===LABEL===",  clab)
    new_span = stringr::str_replace_all(new_span, "===RANGE===",  crange)

    # Building the subtext
    new_sub = state[["yaml"]][["FM"]][["data_meta"]][["subtext"]]
    new_sub = stringr::str_replace_all(new_sub, "===COLOR===",  ccolor)
    new_sub = stringr::str_replace_all(new_sub, "===NAME===",   cname)
    new_sub = stringr::str_replace_all(new_sub, "===LABEL===",  clab)
    new_sub = stringr::str_replace_all(new_sub, "===RANGE===",  crange)

    # Stores the column information to be returned to the user
    col_heads[[cname]]            = new_span
    col_subtext[[cname]]          = new_sub

    col_info[[cname]][["color"]]  = ccolor
    col_info[[cname]][["label"]]  = clab
    col_info[[cname]][["factor"]] = cfactor
  }
  res = list(col_heads   = col_heads,
             col_subtext = col_subtext,
             col_info    = col_info)
res}


#'@export
#'@title Stops Modal Screen Pause
#'@description  Stops Modal Screen Pause
#'@param state Current module state after yaml file has been read.
#'@param session Shiny session variable.
#'@return No return value, called to disable screen pause.
#'@examples
#' # We need a module state object and Shiny session objects to use this function:
#' sess_res = UD_test_mksession(session=list())
#' session = sess_res$session
#' state = sess_res$state
#' FM_pause_screen(state, session)
#' FM_resume_screen(state, session)
FM_resume_screen=function(state, session){

  if((any(c("ShinySession", "session_proxy") %in% class(session)))){
    if(system.file(package = "shinybusy") !=""){
      shinybusy::remove_modal_spinner(session = session)
    }
  }
NULL}


#'@export
#'@title Add Tooltip to UI Element
#'@description Adds a tool tip to a user element.
#'@param state Current module state after yaml file has been read.
#'@param uiele UI element to add the toooltip to.
#'@param tooltip Text containing the tool tip.
#'@param position Position of the tooltip.
#'@param size     size of the tooltip
#'@return If tooltips are enabled and the suggested packages are installed
#'then a uiele with the tooltip added will be returned. Otherwise it will just
#'return the original uiele unchanged.
#'@examples
#'if(interactive()){
#' # We need a module state object to use this function:
#' id="UD"
#' sess_res = UD_test_mksession(session=list(), id=id)
#' state = sess_res$state
#' uiele = shiny::textInput(inputId = "my input", label="example input")
#'
#' uiele = FM_add_ui_tooltip(state, uiele)
#' }
FM_add_ui_tooltip = function(state, uiele, tooltip = "mytooltip", position="right", size="medium"){
   if(state[["MC"]][["tooltips"]][["include"]]){
     if(system.file(package="prompter") != ""){
       if(!is.null(tooltip)){
         uiele = prompter::add_prompt(
           tags$div(style = "width: 100%;", uiele),
           position = position,
           size     = "medium",
           message  = tooltip
         )
       }
     }
   }
uiele}


#'@export
#'@title Fetches Dependency Information
#'@description  For a given state and session this function will determine the
#'module ids that are dependent as well as any packages the module elements
#'might depend on.
#'@param state Current module state after yaml file has been read
#'@param session Shiny session variable
#'@return list with the following elements:
#' \itemize{
#' \item{mod_ids}  Dependent module ids.
#' \item{packages}     List of package dependencies.
#' \item{package_code} Library commands to load packages.
#' }
#'@examples
#' # We need a Shiny session object to use this function:
#' id="UD"
#' sess_res = UD_test_mksession(session=list(), id=id)
#' session  = sess_res$session
#' state    = sess_res$state
#' mod_deps = FM_fetch_deps(state, session)
FM_fetch_deps = function(state, session){
  # determining module id dependencies
  packages      = c()
  package_code  = c()
  deps_found    = c()
  deps_to_check = c()

  deps_found    = state[["dep_mod_ids"]]
  deps_to_check = state[["dep_mod_ids"]]

  # This should determine all of the module ids the current state depends on
  while(!is.null(deps_to_check) > 0){
    tmp_dep = deps_to_check[1]
    tmp_state = FM_fetch_mod_state(id=tmp_dep, session=session)
    if(!is.null(tmp_state[["dep_mod_ids"]])){
      # If any dependencies in the current state are new then we
      # add them to deps_found and deps_to_check
      if(any(!(tmp_state[["dep_mod_ids"]] %in% deps_found))){

        # it's kind of ugly so I'm just creating a new vector
        # for the new dependencies.
        new_deps = tmp_state[["dep_mod_ids"]][!(tmp_state[["dep_mod_ids"]] %in% deps_found)]

        # This appends them
        deps_found    = c(deps_found,    new_deps)
        deps_to_check = c(deps_to_check, new_deps)
      }
    }

    if(length(deps_to_check) == 1){
      deps_to_check = NULL
    } else {
      deps_to_check = deps_to_check[-1]
    }
  }

  # We also add itself
  deps_found = sort(unique(c(deps_found, state[["id"]])))

  # Now we loop through each dependency and pull out the required packages
  if(!is.null(deps_found)){
    for(tmp_dep in deps_found){
      tmp_state = FM_fetch_mod_state(id=tmp_dep, session=session)
      if(!is.null(tmp_state[["MC"]][["code"]][["packages"]])){
        packages = c(packages,
          tmp_state[["MC"]][["code"]][["packages"]])
      }
    }
  }

  if(!is.null(packages)){
    packages = sort(unique(packages))
    package_code = paste0('library("', packages, '")')
  }


  res = list(
    mod_ids      = deps_found,
    packages     = packages,
    package_code = package_code
  )

res}


#'@export
#'@title Centralized Sorting Function
#'@description  When displaying information in a pull down this function can
#'be used to sort those options.
#'@param unsrt_data Unsorted data.
#'@return sorted data
#'@examples
#' # This is the full path to a test data file:
#' data_file_local  =  system.file(package="formods", "test_data", "TEST_DATA.xlsx")
#' # Excel files need a sheet specification:
#' sheet           = "DATA"
#' # We will also attach the sheets along with it
#' df = readxl::read_excel(path=data_file_local, sheet=sheet)
#' # Regular sorting:
#' sort(unique(df$Cohort))
#' FM_pretty_sort(unique(df$Cohort))
FM_pretty_sort = function(unsrt_data){
  res = unsrt_data
  use_normal_sort = TRUE
  if(system.file(package="gtools") != ""){
    if(is.character(res)){
      res = gtools::mixedsort(res)
      use_normal_sort = FALSE
    }
  }

  if(use_normal_sort){
    res = sort(res)
  }

res}



#'@export
#'@title Create RStudio Formatted Comments
#'@description Takes a character string and builds a comment so it will be
#'formatted as a section at the specified level in RStudio
#'@param comment_str Character object.
#'@param level Integer (1 (default),2, or 3) indicating the section level of the comment.
#'@return Formatted comment.
#'@examples
#' FM_build_comment(1, "This is a level 1 header")
#'
#' FM_build_comment(2, paste0(rep("Long string repeated.", 5), collapse=" "))
#'
FM_build_comment  = function(level=1, comment_str){
  max_len = 75
  res = paste0("# ", comment_str, " ")
  # Level 3 sections have 2 extra has tags
  if(level == 3){
    res = paste0("##",res)
  }

  # If we have a really long string we need to truncate it:
  if(nchar(res) >= max_len){
    res = paste0(substr(res, 1,max_len-6), " ")
  }

  # Calculating the padding length:
  pad_len = max_len - nchar(res)

  # The individual padding characters
  if(level == 1){
    pad_str = "-"
  }else if(level == 2){
    pad_str = "="
  }else if(level == 3){
    pad_str = "#"
  }else{
    pad_str = " "
  }

  # Expanding the padding out:
  pad_str = paste0(rep(pad_str, pad_len), collapse="")

  # Putting it all together.
  res =  paste0(res, pad_str)

res}

#'@export
#'@title Creates Icon Link
#'@description Creates a link to a Shiny icon
#'@param href URL to link to.
#'@param target New tab name.
#'@param icon_name Name of icon to use (arguemnt to shiny::icon, default: "circle-info")
#'@return A list with a shiny.tag class that can be converted into an HTML string via as.character() and saved to a file with save_html(). Note if href is \code{NULL} then \code{NULL} is returned.
#'@examples
#' icon_link(href="https://formods.ubiquity.tools")
icon_link = function(href, target="_blank", icon_name="circle-info"){

  res = NULL

  if(!is.null(href)){
    res = tags$a(href=href, target=target, shiny::icon(icon_name))
  }

res}

#'@export
#'@title Fetches the Current Version of Pacakge
#'@description The specified package version is extracted and returned. This
#'can simply be the version installed from CRAN or if a development version
#'from GitHub is used details from that will be returned.
#'@param pkgname Name of package
#'@return String with the version information
#'@examples
#' # This package should exist
#' fetch_package_version('digest')
#'
#' # This package should not exist
#' fetch_package_version('bad package name')
fetch_package_version = function(pkgname){
  isgood       = TRUE
  version      = "NA"
  version_verb = "NA"
  msgs    = c()

  if(system.file(package="devtools")!=""){
    all_pkgs  = devtools::session_info()
    found_idx = all_pkgs$packages$package == pkgname
    if(any(found_idx)){
      pkg_row      = all_pkgs$packages[found_idx,]
      version      = pkg_row[["loadedversion"]]
      version_verb = pkg_row[["loadedversion"]]
      version_verb = paste0(version_verb, " (", pkg_row[["date"]])
      version_verb = paste0(version_verb, ", ",  pkg_row$source)
      version_verb = paste0(version_verb, ")")
    } else {
      isgood       = FALSE
      msgs         = paste0("The package: ", pkgname, " was not found. You may need to load it first.")
    }

  } else {
    isgood       = FALSE
    version_verb = "devtools required"
    msgs         = "devtools was not found"
  }

  res = list(isgood       = isgood,
             msgs         = msgs,
             version_verb = version_verb,
             version      = version)
res}

#'@export
#'@title Determines if a Package is Installed
#'@description Determines if the specified package is installed.
#'@param pkgname Name of package
#'@return Logical indicating if the packages is installed or not
#'@examples
#' # This package should exist
#' is_installed('digest')
#'
#' # This package should not exist
#' is_installed('bad package name')
is_installed = function(pkgname){

  res = TRUE
  if(!requireNamespace(pkgname, quietly=TRUE)){
    res = FALSE
  }

res}

#'@export
#'@title Makes Template Files for formods New Module
#'@description If you want to create a new formods module this function will
#'create the template files for you.
#'@param SN   Module short name
#'@param Module_Name  Module long name
#'@param package Name of package that will contain the module
#'@param element What you would call the thing the module provides for example
#'the FG module provides "figures", the DW module provides "data views".
#'@param file_dir Directory to save file
#'@return list with the following elements:
#' \itemize{
#' \item{mc:}     Module components.
#' \item{server:} Server.R file.
#' \item{yaml:}   Yaml configureation file.
#' }
#' Each of these is a list with paths to the respective files:
#' \itemize{
#' \item{source:}     Template source.
#' \item{dest:}       Destination file name.
#' \item{dest_full:}  Full path to the destination file name.
#' }
#'@examples
#' new_module_template()
#'
new_module_template = function(
  SN          = "NM",
  Module_Name = "New Module",
  package     = "pkgname",
  element     = "analysis",
  file_dir    = tempdir()){

  # Source and destination files:
  mod_files = list(
    mc     = list(source = system.file(package="formods", "templates", "ZZ_module_components.R"),
                  dest   = paste0(SN, "_module_components.R")),
    server = list(source = system.file(package="formods", "templates", "ZZ_Server.R"),
                  dest   = paste0(SN, "_Server.R")),
    yaml   = list(source = system.file(package="formods", "templates", "ZZ.yaml"),
                  dest   = paste0(SN, ".yaml")),
    funcs  = list(source = system.file(package="formods", "templates", "ZZ_funcs.R"),
                  dest   = paste0(SN, "_funcs.R"))
  )

  # Placeholder substitutions
  ph_subs = list(
    ZZ      = SN,
    zz      = tolower(SN),
    ZZ_NAME = Module_Name,
    ELEMENT = element,
    PKG     = package  )



  # We walk through each file
  for(mod_file in names(mod_files)){
    # Reads the contents of the source file into a character vector:
    source  = mod_files[[mod_file]][["source"]]
    lines   = readLines(source)

    # This will apply all the substituations listed above:
    for(ph_sub   in names(ph_subs)){
      st_find    = paste0("===", ph_sub,"===")
      st_replace = ph_subs[[ph_sub]]

      lines = stringr::str_replace_all(lines, st_find,  st_replace)
    }

    dest          = mod_files[[mod_file]][["dest"]]
    dest_full     = file.path(file_dir, dest)
    mod_files[[mod_file]][["dest_full"]] = dest_full

    # This should save the new templates with the substitutions applied:
    write(lines, file=dest_full, append=FALSE)
  }
mod_files}



#'@export
#'@title Create Module Templates in a Package Repository
#'@description If you are developing a package within a repository (i.e. git)
#'and want to create a new formods module this function will
#'create the template files for you and install them in the correct location.
#'@param SN   Module short name
#'@param Module_Name  Module long name
#'@param package Name of package that will contain the module
#'@param element What you would call the thing the module provides for example
#'the FG module provides "figures", the DW module provides "data views"
#'@param overwrite Boolean to indicate if you should overwrite files
#'@param repo_root Root of the repository.
#'@return Same as the return value for new_module_template()
#'@examples
#' if(FALSE){
#'   use_formods(repo_root=tempdir())
#' }
use_formods = function(
  SN          = "NM",
  Module_Name = "New Module",
  package     = "pkgname",
  element     = "analysis",
  overwrite   = FALSE,
  repo_root   = NULL){


  if(is.null(repo_root)){
    if(system.file(package="here") == ""){
      message("The repo_root is not specified and the here package is not installed.")
      message("You need to either specify the repo_root or install the here package.")
      stop("use_formod()")
     } else{
       repo_root = here::here()
    }
  }

  # Making sure the installation directories exist
  R_dir        = file.path(repo_root, "R")
  if(!dir.exists(R_dir)){
    dir.create(R_dir, recursive=TRUE)
  }
  template_dir = file.path(repo_root, "inst", "templates")
  if(!dir.exists(template_dir)){
    dir.create(template_dir, recursive=TRUE)
  }

  test_apps_dir = file.path(repo_root, "inst", "test_apps")
  if(!dir.exists(test_apps_dir)){
    dir.create(test_apps_dir, recursive=TRUE)
  }

  # Creating the new template files in the temp directory
  nmr = new_module_template(
          SN          = SN,
          Module_Name = Module_Name,
          package  = package,
          element  = element,
          file_dir = tempdir())

  tmp_server = file.path(repo_root, "R",                 nmr[["server"]][["dest"]])
  tmp_yaml   = file.path(repo_root, "inst", "templates", nmr[["yaml"]][["dest"]])
  tmp_mc     = file.path(repo_root, "inst", "templates", nmr[["mc"]][["dest"]])
  tmp_funcs  = file.path(repo_root, "inst", "test_apps", nmr[["funcs"]][["dest"]])

  message("Creating module files:")
  message(paste0(" - ", tmp_server))
  message(paste0(" - ", tmp_yaml))
  message(paste0(" - ", tmp_mc))
  message(paste0(" - ", tmp_funcs))

  file.copy(from = nmr[["server"]][["dest_full"]],  to = tmp_server,  overwrite =  overwrite)
  file.copy(from = nmr[["yaml"]][["dest_full"]],    to = tmp_yaml,    overwrite =  overwrite)
  file.copy(from = nmr[["mc"]][["dest_full"]],      to = tmp_mc,      overwrite =  overwrite)
  file.copy(from = nmr[["funcs"]][["dest_full"]],   to = tmp_funcs,   overwrite =  overwrite)
nmr}

#'@export
#'@title Fetches Models from Modules in the App
#'@description  Loops through each specified module ID or all modules if no ID
#'was specified. For each ID, an attempt will be made to extract any models
#'available.
#'@param state Current module state after yaml file has been read
#'@param session Shiny session variable
#'@param ids  Vector of ID strings for the modules containing models or
#'NULL for all modules with models available.
#'@return list containing the current dataset with the following format:
#' \itemize{
#'    \item{isgood:} General logical indicator of successfully.
#'    \item{hasmdl:} Logical indicating if at least one model was found.
#'    \item{modules:} List of module checksums.
#'    \item{mdl:} Result of MM_fetch_mdl, see  \code{vignette("making_modules", package = "formods")}
#'   \item{catalog:} Dataframe containing the a tabular catalog of the
#'   models found.
#'   \itemize{
#'     \item{label:}  Text label for the model.
#'     \item{object :}  Name of the object that contains the compiled rxode2 model.
#'     \item{MOD_TYPE:}  Type of `formods` module the model came from.
#'     \item{id:} Source `formods` Module ID.
#'     \item{checksum:} Checksum of the module where the model came from.
#'     \item{MDLchecksum:} Checksum of the model.
#'     \item{code:}  Code to generate the model.
#'   }
#' }
#'@examples
#' # We need a module state and a Shiny session variable
#' # to use this function:
#' id="UD"
#' sess_res = UD_test_mksession(session=list(), id=id)
#' session = sess_res$session
#' state   = sess_res$state
#' mdl = FM_fetch_mdl(state, session)
#' mdl$catalog
FM_fetch_mdl = function(state, session, ids=NULL){


  hasmdl  = FALSE
  isgood  = TRUE
  modules = list()
  catalog = NULL
  msgs    = c()
  mdl     = list()


  # If we're null then we walk through the session variable and pull out all
  # the IDs to be used below
  if(is.null(ids)){
    # Pulling out the app state:
    app_state = FM_fetch_app_state(session)
    for(mod_state in names(app_state)){
      ids = c(ids, app_state[[mod_state]]$id)
    }
  }

  # Walking through each module id and attempting to extract models
  for(tmp_id in ids){

    # pulling out the current module state and creating the
    # name of the model fetching function for that module
    tmp_state    = FM_fetch_mod_state(session, tmp_id)
    tmp_MOD_TYPE = tmp_state[["MOD_TYPE"]]
    MOD_FUNC     = paste0(tmp_MOD_TYPE, "_fetch_mdl")

    # If that module has a mdl fetching function then we try to fetch it:
    if(exists(MOD_FUNC, mode="function")){
      #
      #     # Function call used to fetch a dataset
      fetch_res = NULL
      FUNC_CALL = paste0("fetch_res = ", MOD_FUNC,"(state = tmp_state)")
      eval(parse(text=FUNC_CALL))

      if(fetch_res[["hasmdl"]]){
        # We've found at least one model
        hasmdl = TRUE
        mdl = c(mdl, fetch_res[["mdl"]])
      }
    }
  }

  if(hasmdl){
    # Creating catalog and modules elements:
    for(mdlname in names(mdl)){
      catalog = rbind(
        catalog,
        data.frame(
          label       = mdl[[mdlname]][["label"]],
          object      = mdlname,
          MOD_TYPE    = mdl[[mdlname]][["MOD_TYPE"]],
          id          = mdl[[mdlname]][["id"]],
          checksum    = mdl[[mdlname]][["checksum"]],
          MDLchecksum = mdl[[mdlname]][["MDLchecksum"]],
          code        = mdl[[mdlname]][["code"]])
      )

      modules[[ mdl[[mdlname]][["MOD_TYPE"]]]  ][[ mdl[[mdlname]][["id"]] ]] = mdl[[mdlname]][["checksum"]]
    }
  } else {
    isgood = FALSE
  }

  # Packing everything up to be returned to the user
  res = list(isgood  = isgood,
             hasmdl  = hasmdl,
             catalog = catalog,
             modules = modules,
             mdl     = mdl)


  res}

#'@export
#'@title Implementation of the \code{linspace} Function from Matlab
#'@description Creates a vector of n elements equally spaced apart.
#'
#'@param a initial number
#'@param b final number
#'@param n number of elements  (integer >= 2)
#'
#'@return vector of numbers from \code{a} to \code{b} with
#'\code{n} linearly spaced apart
#'@examples
#' linspace(0,100, 20)
linspace = function(a, b, n=100){
   isgood = TRUE

   n = as.integer(n)

   if(!is.integer(n)){
     isgood = FALSE }

   if(n < 2){
     isgood = FALSE }

   if(!isgood){
     message("#> linspace error:")
     message("#> n should be a positive integer >= 2 ")
     message("#> defaulting to 100")
     n = 100
   }

   step = (b-a)/(n-1)
   return(seq(a,b,step))

}
