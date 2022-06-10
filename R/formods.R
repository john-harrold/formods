#'@importFrom digest digest

#'@export
#'@title Finds Dataset when both UD and DW modules are in use
#'@description The upload data and data wranling modules can be used together
#'but a user may opt to upload the data and skip the wrangling part. This will
#'take the ids for both modules and try to determine which one is being used.
#'The function first looks for a dataset in data wrangling module associated
#'with id_DW. If it finds a dataset there it returns that value. If it fails
#'it then looks for a dataset in the upload data module associated with id_UD.
#'If it fails ther it returns a list with the defaults below.
#'@param id_UD  ID string for the upload data module used to handle uploads or
#'the name of the list element in react_state where the data set is stored.
#'@param id_DW  ID string for the data wrangling module to process any uploaded data
#'@param react_state Variable passed to server to allow reaction outside of module (`NULL`)
#'@return list containing the current dataset with the following format:
#'values from the yaml file as well as any changes made by the user
#' \itemize{
#'   \item{isgood:} Boolean indicating the whether a dataset was found
#'   (`FALSE`)
#'   \item{dsm:} Module where the dataset was found or "Not Found"
#'   \item{contents:} Data frame containting the contents of the dataset
#'   (`NULL`)
#'   \item{columns:} Columns names from the contents or ('NULL')
#'   \item{object_name:} Object name for the dataset (e.g. DS)
#'   \item{checksum:} This is an MD5 sum of the contents element and can be
#'   used to detect changes in the loaded file.
#' }
FM_find_DS = function(id_UD, id_DW, react_state){

  # Creating default values for the dataset contents, checksum, etc
  contents     = NULL
  checksum     = digest::digest(contents, algo=c("md5"))
  columns      = NULL
  object_name  = NULL
  code_ds_load = NULL
  code_dw      = NULL
  dsm          = "Not Found"
  isgood       = FALSE

  # First we check the DW module
  if(!isgood){
    if(id_DW %in% names(react_state)){
      browser()
    }
  }

  # If that fails we check the upload data module
  if(!isgood){
    if(id_UD %in% names(react_state)){
      tmp_checksum    = isolate(react_state[[id_UD]][["DS"]][["checksum"]])
      tmp_contents    = isolate(react_state[[id_UD]][["DS"]][["contents"]])
      tmp_object_name = isolate(react_state[[id_UD]][["DS"]][["object_name"]])
      code_ds_load    = isolate(react_state[[id_UD]][["DS"]][["code"]])
      # If both of these are not null then we're good:
      if(!is.null(tmp_checksum)    &
         !is.null(tmp_object_name) &
         !is.null(tmp_contents)){
        isgood      = TRUE
        dsm         = "UD"
        checksum    = tmp_checksum
        object_name = tmp_object_name
        contents    = tmp_contents
      }
    }
  }

  # If we found a dataset we pull out the column names
  if(isgood){
    columns = names(contents)
  }

  # putting it all together
  res = list(
     contents     =  contents,
     checksum     =  checksum,
     object_name  =  object_name,
     columns      =  columns,
     code_ds_load =  code_ds_load,
     dsm          =  dsm,
     isgood       =  isgood)

res}

#'@export
#'@title Automatically Cast UI Input Variable
#'@description Takes UI input and tries to figure out if it's numeric or text
#'@param ui_input UI input from a shiny form
#'@param quote_char TRUE will include double quotes in the character string
#'@return best guess at type casting a variable
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
#'@return object with factors removed
unfactor = function(fctobj){
  res = fctobj
  if(is.factor(fctobj)){
    objtype = typeof(fctobj)
    cmd = paste0("res = as.", objtype,"(as.character(fctobj))");
    eval(parse(text=cmd))
  }
res}


#'@export
#'@title Detect if a UI Button Has Been Clicked
#'@description Takes a UI element value and a button counter and determines if
#'it has been clicked
#'@param ui_value   Current value from the button UI.
#'@param counter    Current value of the button counter.
#'@param init_value Default value for reading in UI data when it has not been
#'defined.
#'@return object with factors removed
was_clicked = function(ui_value   = NULL,
                       counter    = NULL,
                       init_value = c("")){
  res = FALSE
  # We only Sure
  if(!is.null(ui_value)){
    if(ui_value  != 0 & ui_value !=init_value){
      if(ui_value  != counter){
        res = TRUE
      }
    }
  }
res}

