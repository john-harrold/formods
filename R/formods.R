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
#'   \item{checksum:} This is an MD5 sum of the contents element and can be
#'   used to detect changes in the loaded file.
#' }
FM_find_DS = function(id_UD, id_DW, react_state){
  
  # Creating default values for the dataset contents, checksum, etc
  contents  = NULL
  checksum  = digest::digest(contents, algo=c("md5"))
  columns   = NULL
  dsm       = "Not Found"
  isgood    = FALSE
  
  # First we check the DW module
  if(id_DW %in% names(react_state)){
    browser()
  }
  
  # If that fails we check the upload data module
  if(!isgood){
    if(id_UD %in% names(react_state)){
      tmp_checksum = isolate(react_state[[id_UD]][["DS"]][["checksum"]])
      tmp_contents = isolate(react_state[[id_UD]][["DS"]][["contents"]])
      # If both of these are not null then we're good:
      if(!is.null(tmp_checksum) &
         !is.null(tmp_contents)){
        isgood   = TRUE
        dsm      = "UD"
        checksum = tmp_checksum 
        contents = tmp_contents 
      }
    }
  }
  
  # If we found a dataset we pull out the column names
  if(isgood){
    columns = names(contents)
  }
  
  # putting it all together 
  res = list(
     contents  =  contents,  
     checksum  =  checksum,  
     columns   =  columns,   
     dsm       =  dsm     ,  
     isgood    =  isgood  )  

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

}
