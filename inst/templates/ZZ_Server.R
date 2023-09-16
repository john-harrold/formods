#'@import rhandsontable
#'@import shiny
#'@importFrom digest digest
#'@importFrom shinyAce aceEditor updateAceEditor

#'@export
#'@title ===ZZ_NAME=== State Server
#'@description Server function for the ===ZZ_NAME===  Shiny Module
#'@param id An ID string that corresponds with the ID used to call the modules UI elements
#'@param id_ASM ID string for the app state managment module used to save and load app states
#'@param FM_yaml_file App configuration file with FM as main section.
#'@param MOD_yaml_file  Module configuration file with MC as main section.
#'@param deployed Boolean variable indicating whether the app is deployed or not.
#'@param react_state Variable passed to server to allow reaction outside of module (\code{NULL})
#'@return UD Server object
#'@examples
#' NOTE: you will need to create a server example
===ZZ===_Server <- function(id,
               id_ASM        = "ASM",
               FM_yaml_file  = system.file(package = "formods", "templates", "formods.yaml"),
               MOD_yaml_file = system.file(package = "===PKG===",  "templates", "===ZZ===.yaml"),
               deployed      = FALSE,
               react_state   = NULL) {
  moduleServer(id, function(input, output, session) {


    #------------------------------------
    # Select the active ===ELEMENT===
    output$===ZZ===_ui_select_element = renderUI({
      input$button_clk_save
      input$button_clk_del
      input$button_clk_copy
      input$button_clk_new
      input$current_element
      state = ===ZZ===_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)
      choices = list()
      for(element_id in names(state[["===ZZ==="]][["elements"]])){
        choices[[ state[["===ZZ==="]][["elements"]][[element_id]][["ui"]][["element_name"]] ]] = element_id
      }

      uiele =
      shinyWidgets::pickerInput(
        selected   = state[["===ZZ==="]][["current_element"]],
        inputId    = NS(id, "element_selection"),
        label      = state[["MC"]][["labels"]][["current_element"]],
        choices    = choices,
        width      = state[["MC"]][["formatting"]][["current_element"]][["width"]])

      uiele})
    #------------------------------------
    # Current ===ELEMENT=== name:    
    output$===ZZ===_ui_text_element_name = renderUI({
      input$button_clk_save
      input$button_clk_del
      input$button_clk_copy
      input$button_clk_new
      input$element_selection
      state = ===ZZ===_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      current_ele = ===ZZ===_fetch_current_element(state)

      uiele =
      textInput(
        inputId     = NS(id, "element_name"),
        label       = NULL,
        width       = state[["MC"]][["formatting"]][["element_name"]][["width"]] ,
        value       = current_ele[["ui"]][["element_name"]],
        placeholder = state[["MC"]][["labels"]][["element_name"]]
      )
      
      uiele})


    #------------------------------------
    # Create ui outputs here:
    output$===ZZ===_ui_element = renderUI({
      state = ===ZZ===_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)
      uiele = "UI Element"
      uiele})

    #------------------------------------
    # Generated data reading code
    observe({
      # Reacting to file changes
      state = ===ZZ===_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      if(is.null(state[["===ZZ==="]][["code"]])){
        uiele = "# No code to generate"
      } else {
        uiele = state[["===ZZ==="]][["code"]]
      }


      shinyAce::updateAceEditor(
        session         = session,
        editorId        = "ui_===zz===_code",
        theme           = state[["yaml"]][["FM"]][["code"]][["theme"]],
        showLineNumbers = state[["yaml"]][["FM"]][["code"]][["showLineNumbers"]],
        readOnly        = state[["MC"]][["code"]][["readOnly"]],
        mode            = state[["MC"]][["code"]][["mode"]],
        value           = uiele)

    })
    #------------------------------------
    # Side buttons:
    # new
    output$ui_===zz===_new_btn = renderUI({
      state = ===ZZ===_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      uiele = shinyWidgets::actionBttn(
        inputId = NS(id, "button_clk_new"),
        label   = state[["MC"]][["labels"]][["new_btn"]],
        style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
        size    = state[["MC"]][["formatting"]][["button_clk_new"]][["size"]],
        block   = state[["MC"]][["formatting"]][["button_clk_new"]][["block"]],
        color   = "success",
        icon    = icon("plus"))

      # Optinally adding the tooltip:
      uiele = formods::FM_add_ui_tooltip(state, uiele,
                                         tooltip     = state[["MC"]][["formatting"]][["button_clk_new"]][["tooltip"]],
                                         position    = state[["MC"]][["formatting"]][["button_clk_new"]][["tooltip_position"]])

      uiele})

    #------------------------------------
    # Save
    output$ui_===zz===_save_btn = renderUI({
      state = ===ZZ===_fetch_state(id        = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      uiele = shinyWidgets::actionBttn(
                inputId = NS(id, "button_clk_save"),
                label   = state[["MC"]][["labels"]][["save_btn"]],
                style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
                size    = state[["MC"]][["formatting"]][["button_clk_save"]][["size"]],
                block   = state[["MC"]][["formatting"]][["button_clk_save"]][["block"]],
                color   = "primary",
                icon    = icon("arrow-down"))

      # Optinally adding the tooltip:
      uiele = formods::FM_add_ui_tooltip(state, uiele,
               tooltip     = state[["MC"]][["formatting"]][["button_clk_save"]][["tooltip"]],
               position    = state[["MC"]][["formatting"]][["button_clk_save"]][["tooltip_position"]])

      uiele})
    #------------------------------------
    # clip code
    output$ui_===zz===_clip_code = renderUI({
      state = ===ZZ===_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)
      uiele = NULL
      if((system.file(package="clipr") != "") & !deployed){
        uiele = shinyWidgets::actionBttn(
                  inputId = NS(id, "button_clk_clip"),
                  label   = state[["MC"]][["labels"]][["clip_btn"]],
                  style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
                  size    = state[["MC"]][["formatting"]][["button_clk_clip"]][["size"]],
                  block   = state[["MC"]][["formatting"]][["button_clk_clip"]][["block"]],
                  color   = "royal",
                  icon    = icon("clipboard", lib="font-awesome"))
        # Optinally adding the tooltip:
        uiele = formods::FM_add_ui_tooltip(state, uiele,
                 tooltip             = state[["MC"]][["formatting"]][["button_clk_clip"]][["tooltip"]],
                 position    = state[["MC"]][["formatting"]][["button_clk_clip"]][["tooltip_position"]])
      }
      uiele})
    #------------------------------------
    # delete
    output$ui_===zz===_del_btn   = renderUI({
      state = ===ZZ===_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)
      uiele = shinyWidgets::actionBttn(
                inputId = NS(id, "button_clk_del"),
                label   = state[["MC"]][["labels"]][["del_btn"]],
                style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
                size    = state[["MC"]][["formatting"]][["button_clk_del"]][["size"]],
                block   = state[["MC"]][["formatting"]][["button_clk_del"]][["block"]],
                color   = "danger",
                icon    = icon("minus"))

      # Optinally adding the tooltip:
      uiele = formods::FM_add_ui_tooltip(state, uiele,
               tooltip     = state[["MC"]][["formatting"]][["button_clk_del"]][["tooltip"]],
               position    = state[["MC"]][["formatting"]][["button_clk_del"]][["tooltip_position"]])
      uiele})
    #------------------------------------
    # copy
    output$ui_===zz===_copy_btn   = renderUI({
      state = ===ZZ===_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      uiele = shinyWidgets::actionBttn(
                inputId = NS(id, "button_clk_copy"),
                label   = state[["MC"]][["labels"]][["copy_btn"]],
                style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
                size    = state[["MC"]][["formatting"]][["button_clk_copy"]][["size"]],
                block   = state[["MC"]][["formatting"]][["button_clk_copy"]][["block"]],
                color   = "royal",
                icon    = icon("copy"))

      # Optinally adding the tooltip:
      uiele = formods::FM_add_ui_tooltip(state, uiele,
               tooltip             = state[["MC"]][["formatting"]][["button_clk_copy"]][["tooltip"]],
               position    = state[["MC"]][["formatting"]][["button_clk_copy"]][["tooltip_position"]])
      uiele})
    #------------------------------------
    # User messages:
    output$ui_===zz===_msg = renderText({
      state = ===ZZ===_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      uiele = state[["===ZZ==="]][["ui_msg"]]

      uiele})
    # Creates the ui for the compact view of the module
    #------------------------------------
    # Compact ui
    output$ui_===zz===_compact  =  renderUI({
      state = ===ZZ===_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)


      uiele_code_button = NULL
      # Generating code button if enabled
      if( state[["MC"]][["compact"]][["code"]]){
        uiele_code = tagList(shinyAce::aceEditor(
          NS(id, "ui_===zz===_code"),
          height  = state[["MC"]][["formatting"]][["code"]][["height"]]
          ))

        uiele_code_button = tagList(
         shinyWidgets::dropdownButton(
           uiele_code,
           inline  = FALSE,
           right   = TRUE ,
           size    = "sm",
           circle  = FALSE,
           width   = state[["MC"]][["formatting"]][["code"]][["width"]],
           status  = "danger btn-custom-===zz===",
           icon    = icon("code", lib="font-awesome"),
           tooltip = shinyWidgets::tooltipOptions(title = state[["MC"]][["tooltips"]][["show_code"]]))
        )

      }

      # Button with ===ZZ=== elements table
      uiele_===zz===_elements_button = NULL
     # Uncomment this if your ===ELEMENT=== has a components table
     #uiele_===zz===_elements = rhandsontable::rHandsontableOutput(NS(id, "hot_===zz===_elements"))
     #uiele_===zz===_elements_button = tagList(
     # shinyWidgets::dropdownButton(
     #   uiele_===zz===_elements,
     #   inline  = FALSE,
     #   right   = TRUE ,
     #   size    = "sm",
     #   circle  = FALSE,
     #   status  = "primary btn-custom-===zz===",
     #   icon    = icon("layer-group", lib="font-awesome"),
     #   tooltip = tooltipOptions(title = state[["MC"]][["tooltips"]][["elements"]]))
     #)

      uiele = tagList(
        div(style="display:inline-block", "Place ===ELEMENT=== name, attributes and inputs here."),
        tags$br(),
        div(style="display:inline-block", htmlOutput(NS(id, "ui_===zz===_msg")))
      )

      # We only show the clip button if it's enabled
      uiele_clip_button = NULL
      if(state[["MC"]][["compact"]][["clip"]]){
        uiele_clip_button = htmlOutput(NS(id, "ui_===zz===_clip_code"))
      }

      uiele_buttons_right = tagList(
               tags$style(".btn-custom-===zz=== {width: 100px;}"),
               div(style="display:inline-block;vertical-align:top;height:100px",
               uiele_===zz===_elements_button,
               uiele_code_button,
               uiele_clip_button,
               htmlOutput(NS(id, "ui_===zz===_save_btn")),
               htmlOutput(NS(id, "ui_===zz===_copy_btn")),
               htmlOutput(NS(id, "ui_===zz===_del_btn")),
               htmlOutput(NS(id, "ui_===zz===_new_btn"))
               ))

      # Appending the preview
      div_style = paste0("display:inline-block;vertical-align:top;", 
        "width:",   state[["MC"]][["formatting"]][["preview"]][["width"]],  ";",
        "height: ", state[["MC"]][["formatting"]][["preview"]][["height"]])
      uiele_preview = div(style=div_style,
                          "Place your module ===ELEMENT=== preview here.")
      uiele = tagList(
        uiele,
        uiele_preview,
        uiele_buttons_right,
        tags$br()
      )


      uiele = tagList( uiele,
        tags$br(),
        "Place module construction elements here."
      )
      uiele
    })

    #------------------------------------
    # Creating reaction if a variable has been specified
    if(!is.null(react_state)){
      # Here we list the ui inputs that will result in a state change:
      toListen <- reactive({
        list(
           # react_state[[id_ASM]])
             input$button_clk_new,
             input$button_clk_del,
             input$button_clk_copy,
             input$button_clk_save)
      })
      # This updates the reaction state:
      observeEvent(toListen(), {
        state = ===ZZ===_fetch_state(id        = id,
                               input           = input,
                               session         = session,
                               FM_yaml_file    = FM_yaml_file,
                               MOD_yaml_file   = MOD_yaml_file,
                               react_state     = react_state)

        FM_le(state, "reaction state updated")
        #react_state[[id]] = state
        react_state[[id]][["===ZZ==="]][["checksum"]] = state[["===ZZ==="]][["checksum"]]
      }, priority=99)
    }


  })
}

#'@export
#'@title Fetch ===ZZ_NAME=== State
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
#' \item{===ZZ===:}
#' \itemize{
#'   \item{isgood:} Boolean object indicating if the file was successfully loaded.
#'   \item{checksum:} This is an MD5 sum of the contents element and can be
#'   used to detect changes in the state.
#' }
#'  \item{MOD_TYPE:} Character data containing the type of module \code{"===ZZ==="}
#'  \item{id:} Character data containing the module id module in the session variable.
#'  \item{FM_yaml_file:} App configuration file with FM as main section.
#'  \item{MOD_yaml_file:}  Module configuration file with MC as main section.
#'@examples
#' # Within shiny both session and input variables will exist,
#' # this creates examples here for testing purposes:
#' sess_res = ===ZZ===_test_mksession(session=list())
#' session = sess_res$session
#' input   = sess_res$input
#'
#' # Configuration files
#' FM_yaml_file  = system.file(package = "formods", "templates", "formods.yaml")
#' MOD_yaml_file = system.file(package = "===PKG===", "templates", "===ZZ===.yaml")
#'
#' # Creating an empty state object
#' state = ===ZZ===_fetch_state(id              = "===ZZ===",
#'                        input           = input,
#'                        session         = session,
#'                        FM_yaml_file    = FM_yaml_file,
#'                        MOD_yaml_file   = MOD_yaml_file,
#'                        react_state     = NULL)
#'}
===ZZ===_fetch_state = function(id, input, session, FM_yaml_file, MOD_yaml_file, react_state){

  # Template for an empty dataset
  #---------------------------------------------
  # Getting the current state
  state = FM_fetch_mod_state(session, id)
  # If the state has not yet been defined then we
  # initialize it
  if(is.null(state)){
    # General state information
    state = ===ZZ===_init_state(FM_yaml_file, MOD_yaml_file, id, session)
  }

  #---------------------------------------------
  # Here we update the state based on user input
  for(ui_name in state[["===ZZ==="]][["ui_ids"]]){
    if(!is.null(isolate(input[[ui_name]]))){
       state[["===ZZ==="]][["ui"]][[ui_name]] = isolate(input[[ui_name]])
     } else {
       if(ui_name %in% names(state[["===ZZ==="]][["button_counters"]])){
         state[["===ZZ==="]][["ui"]][[ui_name]] = 0
       } else {
         state[["===ZZ==="]][["ui"]][[ui_name]] = ""
       
         # initializing the previous ui values as well:
         if(is.null(state[["===ZZ==="]][["ui_prev"]][[ui_name]])){
           state[["===ZZ==="]][["ui_old"]][[ui_name]] = state[["===ZZ==="]][["ui"]][[ui_name]]
         }
       }
     }
   }
   msgs = c()

  #---------------------------------------------
  # Now we sync the ui in the state with the button click 
  # tracking or current element. This ensures that every 
  # time the state is fetched all of the components of 
  # the current element are in sync.

  # This is a list of ui changes that were detected and 
  # can be used to trigger different actions below:
  changed_uis = c()

  # We need to pull out the current element for updating:
  current_ele = ===ZZ===_fetch_current_element(state)
  # There are scenarios where you wouldn't want to do this. Like when
  # switching elements in the ui. You would need to add some logic to 
  # only update below conditionally.

  for(ui_name in state[["===ZZ==="]][["ui_ids"]]){
    if(!fetch_hold(state, ui_name)){
      if(ui_name %in% names(state[["===ZZ==="]][["button_counters"]])){
        # Button changes are compared to the button click tracking values
        change_detected = 
          has_changed(ui_val  = state[["===ZZ==="]][["ui"]][[ui_name]],
                      old_val = state[["===ZZ==="]][["button_counters"]][[ui_name]])
        if(change_detected){
          formods::FM_le(state, paste0("button click: ", ui_name, " = ", state[["===ZZ==="]][["ui"]][[ui_name]]))

          # Saving the change:
          state[["===ZZ==="]][["button_counters"]][[ui_name]] =
            state[["===ZZ==="]][["ui"]][[ui_name]]        

          # logging the changed ui name:
          changed_uis = c(changed_uis, ui_name)
        }
      }else{
        change_detected = 
          has_changed(ui_val  = state[["===ZZ==="]][["ui"]][[ui_name]],
                      old_val = state[["===ZZ==="]][["ui_old"]][[ui_name]])
        if(change_detected){
          formods::FM_le(state, paste0("setting ===ELEMENT=== : ", ui_name, " = ", paste(state[["===ZZ==="]][["ui"]][[ui_name]], collapse=", ")))
        
          # Saving the change:
          state[["===ZZ==="]][["ui_old"]][[ui_name]] = state[["===ZZ==="]][["ui"]][[ui_name]]
        
          # logging the changed ui name:
          changed_uis = c(changed_uis, ui_name)

          # This also updates the current element if that ui_name is part of
          # an element
          if(ui_name %in% state[["===ZZ==="]][["ui_ele"]]){
            current_ele[["ui"]][[ui_name]] = state[["===ZZ==="]][["ui"]][[ui_name]]
          }
        }
      }
    }
  }
  # Updating the element with any changes:
  state = ===ZZ===_set_current_element(
    state   = state,
    element = current_ele)
  #---------------------------------------------
  # Here we react to changes between the UI and the current state
  # save ===ELEMENT===
  if("button_clk_save" %in% changed_uis){
    FM_le(state, "save ===ELEMENT===")
    current_ele = ===ZZ===_fetch_current_element(state)

    current_ele[["ui"]][["element_name"]] = 
      state[["===ZZ==="]][["ui"]][["element_name"]]

    state = ===ZZ===_set_current_element(
      state   = state,
      element = current_ele)
  }
  #---------------------------------------------
  # clip ===ELEMENT===
  if("button_clk_clip" %in% changed_uis){
    FM_le(state, "clip ===ELEMENT===")
  }
  #---------------------------------------------
  # copy ===ELEMENT===
  if("button_clk_copy" %in% changed_uis){
    FM_le(state, "copy ===ELEMENT===")

    # First we pull out the current element:
    old_ele = ===ZZ===_fetch_current_element(state)

    # Now we create a new element and make it the current element
    state   = ===ZZ===_new_element(state)
    new_ele = ===ZZ===_fetch_current_element(state)

    # We keep the new name:
    old_ele[["ui"]][["element_name"]] = new_ele[["ui"]][["element_name"]] 

    # NOTE: You may need to add other code here 
    # to change other aspects about the old element
    # that is being copied.

    state = ===ZZ===_set_current_element(
      state   = state,
      element = old_ele)
  }
  #---------------------------------------------
  # del ===ELEMENT===
  if("button_clk_del" %in% changed_uis){
    FM_le(state, "delete ===ELEMENT===")
    state = ===ZZ===_del_current_element(state)
  }
  #---------------------------------------------
  # new ===ELEMENT===
  if("button_clk_new" %in% changed_uis){
    FM_le(state, "new ===ELEMENT===")
    state = ===ZZ===_new_element(state)
  }
  if("element_selection" %in% changed_uis){
    state[["===ZZ==="]][["current_element"]] =
       state[["===ZZ==="]][["ui"]][["element_selection"]]
  }
  #---------------------------------------------
  # Passing any messages back to the user
  state = FM_set_ui_msg(state, msgs)

  #---------------------------------------------
  # Saving the state
  FM_set_mod_state(session, id, state)

  # Returning the state
  state}

#'@export
#'@title Initialize ===ZZ=== Module State
#'@description Creates a list of the initialized module state
#'@param FM_yaml_file App configuration file with FM as main section.
#'@param MOD_yaml_file  Module configuration file with MC as main section.
#'@param id ID string for the module.
#'@param session Shiny session variable
#'@return list containing an empty ===ZZ=== state
#'@examples
#' # Within shiny both session and input variables will exist,
#' # this creates examples here for testing purposes:
#' sess_res = ===ZZ===_test_mksession(session=list())
#' session = sess_res$session
#' input   = sess_res$input
#'
#' state = ===ZZ===_init_state(
#'    FM_yaml_file  = system.file(package = "formods",
#'                                "templates",
#'                                "formods.yaml"),
#'    MOD_yaml_file = system.file(package = "===PKG===",
#'                                "templates",
#'                                "===ZZ===.yaml"),
#'    id              = "===ZZ===",
#'    session         = session)
#'
#' state
===ZZ===_init_state = function(FM_yaml_file, MOD_yaml_file,  id, session){


  button_counters = c("button_clk_save",
                      "button_clk_clip",
                      "button_clk_del",
                      "button_clk_copy",
                      "button_clk_new")

  # This contains all of the relevant ui_ids in the module
  ui_ids          = c(button_counters,
                      "element_selection",
                    # "current_element",
                      "element_name")

  # These are the module ui elements that are associated with 
  # the current element
  ui_ele          = c()

  # Making all the ui_ids holdable
  ui_hold         = ui_ids

  state = FM_init_state(
    FM_yaml_file    = FM_yaml_file,
    MOD_yaml_file   = MOD_yaml_file,
    id              = id,
    MT              = "===ZZ===",
    button_counters = button_counters,
    ui_ids          = ui_ids,
    ui_hold         = ui_hold,
    session         = session)


  # Storing the ui_ids for the elements 
  state[["===ZZ==="]][["ui_ele"]]               = ui_ele

  # This tracks elements for the module
  state[["===ZZ==="]][["code_previous"]]        = NULL
  state[["===ZZ==="]][["elements"]]             = NULL
  state[["===ZZ==="]][["current_element"]]      = NULL
  state[["===ZZ==="]][["element_cntr"]]         = 0



  # Creating a default element:
  state = ===ZZ===_new_element(state)
  
  FM_le(state, "State initialized")
state}

#'@export
#'@title Fetch Module Code
#'@description Fetches the code to generate results seen in the app
#'@param state ===ZZ=== state from \code{===ZZ===_fetch_state()}
#'@return Character object vector with the lines of code
#'@examples
#' # We need a module state:
#' sess_res = ===ZZ===_test_mksession(session=list())
#' state = sess_res$state
#'
#' code = ===ZZ===_fetch_code(state)
#'
#' cat(code)
===ZZ===_fetch_code = function(state){

  code = NULL

code}

#'@export
#'@title Append Report Elements
#'@description Description
#'@param state ===ZZ=== state from \code{===ZZ===_fetch_state()}
#'@param rpt Report with the current content of the report which will be appended to in
#'this function. For details on the structure see the documentation for \code{\link{formods::FM_generate_report}}.
#'@param rpttype Type of report to generate (supported "xlsx", "pptx", "docx").
#'@param gen_code_only Boolean value indicating that only code should be
#'generated (\code{FALSE}).
#'@return list containing the following elements
#'\itemize{
#'  \item{isgood:}    Return status of the function.
#'  \item{hasrptele:} Boolean indicator if the module has any reportable elements.
#'  \item{code:}      Code to generate reporting elements. 
#'  \item{msgs:}      Messages to be passed back to the user.
#'  \item{rpt:}       Report with any additions passed back to the user.
#'}
#'@seealso \code{\link{formods::FM_generate_report}}
===ZZ===_append_report = function(state, rpt, rpttype, gen_code_only=FALSE){

  isgood    = TRUE
  hasrptele = FALSE
  code      = c()
  msgs      = c()


  # The ===ZZ=== module only supports the following report types:
  supported_rpttypes = c("xlsx", "pptx", "docx")

  if(rpttype %in% supported_rpttypes){
  }

  res = list(
    isgood    = isgood,
    hasrptele = hasrptele,
    code      = code,
    msgs      = msgs,
    rpt       = rpt
  )

res}

#'@export
#'@title Fetch ===ZZ_NAME=== Module Datasets
#'@description Fetches the datasets contained in the module. 
#'@param state ===ZZ=== state from \code{===ZZ===_fetch_state()}
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
#'    \item{DSMETA: Metadata describing DS}
#'    \item{code: Complete code to build dataset.}
#'    \item{checksum: Module checksum.}
#'    \item{DSchecksum: Dataset checksum.}
#'  }
#'}
#'@examples
#' # We need a module state:
#' sess_res = ===ZZ===_test_mksession(session=list())
#' state = sess_res$state
#'
#' ds = ===ZZ===_fetch_ds(state)
#'
#' ds
===ZZ===_fetch_ds = function(state){
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
  if(state[["===ZZ==="]][["isgood"]]){

    # Fill in the DS creation stuff here
    isgood = FALSE

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
#' sess_res = ===ZZ===_test_mksession(session=list())
===ZZ===_test_mksession = function(session, id = "===ZZ==="){

  isgood = TRUE
  rsc    = list()
  input  = list()

  # Configuration files
  FM_yaml_file  = system.file(package = "formods", "templates", "formods.yaml")
  MOD_yaml_file = system.file(package = "===PKG===", "templates", "===ZZ===.yaml")
 
  # Creating an empty state object
  state = ===ZZ===_fetch_state(id              = "===ZZ===",
                         input           = input,
                         session         = session,
                         FM_yaml_file    = FM_yaml_file,
                         MOD_yaml_file   = MOD_yaml_file,
                         react_state     = NULL)

  res = list(
    isgood  = isgood,
    session = session,
    input   = input,
    state   = state,
    rsc     = rsc
  )
}

#'@export
#'@title New ===Module_Name=== ===ELEMENT===
#'@description Appends a new empty ===ELEMENT=== to the ===ZZ=== state object
#'and makes this new ===ELEMENT=== the active ===ELEMENT===.
#'@param state ===ZZ=== state from \code{===ZZ===_fetch_state()}
#'@return ===ZZ== state object containing a new ===ELEMENT=== and that
#'===ELEMENT=== is set as the current active ===ELEMENT===. See the help for
#'\code{===ZZ===_fetch_state()} for ===ELEMENT== format.
#'@examples
#' sess_res = ===ZZ===_test_mksession(session=list())
#' session = sess_res$session
#' input   = sess_res$input
#'
#' # Configuration files
#' FM_yaml_file  = system.file(package = "formods", "templates", "formods.yaml")
#' MOD_yaml_file = system.file(package = "===PKG===", "templates", "===ZZ===.yaml")
#'
#' # Creating an empty state object
#' state = ===ZZ===_fetch_state(id              = "===ZZ===",
#'                        input           = input,
#'                        session         = session,
#'                        FM_yaml_file    = FM_yaml_file,
#'                        MOD_yaml_file   = MOD_yaml_file,
#'                        react_state     = NULL)
#'
#' # Creates a new empty element
#' state = ===ZZ===_new_element(state)
#'
#' # Delete the current element
#' state = ===ZZ===_del_current_element(state)
#'
#' # Fetch a list of the current element
#' element = ===ZZ===_fetch_current_element(state)
#'
#' # You can modify the element
#' element[["name"]] = "A more descriptive name"
#'
#' # You can now place element back in the state
#' state = ===ZZ===_set_current_element(state, element)                                   
===ZZ===_new_element = function(state){

  # Incrementing the element counter
  state[["===ZZ==="]][["element_cntr"]] = state[["===ZZ==="]][["element_cntr"]] + 1

  # Creating a default element ID
  element_id = paste0("element_", state[["===ZZ==="]][["element_cntr"]])

  # Creating the object name for this element
  element_ds_object_name = paste0(state[["MC"]][["element_object_name"]],
                          "_", state[["===ZZ==="]][["element_cntr"]])
  # Default for a new element:
  element_def =
    list(
         # internal use only
         isgood                 = TRUE,
         # This will hold the ui values for the current element
         ui                     = list(
           element_name  = paste0("===ELEMENT=== ", state[["===ZZ==="]][["element_cntr"]])
           ),
         id                     = element_id,
         idx                    = state[["===ZZ==="]][["element_cntr"]],
         element_ds_object_name = element_ds_object_name,
         code_previous          = NULL,
         # user facing          
         # This is used if you build the element in a layering method sort of
         # like how the ggplot figures in the FG module builds using different
         # ggplot commands (layers). 
         elements_table         = NULL,
         # Generated on save    
         checksum               = NULL,
         code                   = NULL,
         code_dw_only           = NULL)


  # This contains the code to generate the input dataset
  code_previous = c(
    paste0(
           element_ds_object_name,
           " = ",
            state[["===ZZ==="]][["UD"]][["object_name"]]))
  element_def[["code_previous"]] = code_previous

  # Dropping the new element into the state
  state[["===ZZ==="]][["elements"]][[element_id]] = element_def
  # Setting the new element as current
  state[["===ZZ==="]][["current_element"]]     = element_id

state}


#'@export
#'@title Fetches Current ===ELEMENT===
#'@description Takes a ===ZZ=== state and returns the current active
#'===ELEMENT===
#'@param state ===ZZ=== state from \code{===ZZ===_fetch_state()}
#'@return List containing the details of the active data view. The structure
#'of this list is the same as the structure of \code{state$===ZZ===$elements} in the output of
#'\code{===ZZ===_fetch_state()}.
#'@examples
#' sess_res = ===ZZ===_test_mksession(session=list())
#' session = sess_res$session
#' input   = sess_res$input
#'
#' # Configuration files
#' FM_yaml_file  = system.file(package = "formods", "templates", "formods.yaml")
#' MOD_yaml_file = system.file(package = "===PKG===", "templates", "===ZZ===.yaml")
#'
#' # Creating an empty state object
#' state = ===ZZ===_fetch_state(id              = "===ZZ===",
#'                        input           = input,
#'                        session         = session,
#'                        FM_yaml_file    = FM_yaml_file,
#'                        MOD_yaml_file   = MOD_yaml_file,
#'                        react_state     = NULL)
#'
#' # Creates a new empty element
#' state = ===ZZ===_new_element(state)
#'
#' # Delete the current element
#' state = ===ZZ===_del_current_element(state)
#'
#' # Fetch a list of the current element
#' element = ===ZZ===_fetch_current_element(state)
#'
#' # You can modify the element
#' element[["name"]] = "A more descriptive name"
#'
#' # You can now place element back in the state
#' state = ===ZZ===_set_current_element(state, element)                                   
===ZZ===_fetch_current_element    = function(state){

  element_id = state[["===ZZ==="]][["current_element"]]

  current_element = state[["===ZZ==="]][["elements"]][[element_id]]

current_element}


#'@export
#'@title Sets the Value for the  Current ===ELEMENT===
#'@description Takes a ===ZZ=== state and returns the current active
#'===ELEMENT===
#'@param state ===ZZ=== state from \code{===ZZ===_fetch_state()}
#'@param element Element list from \code{===ZZ===_fetch_current_element()}
#'@return ===ZZ== state object with the current ===ELEMENT=== set using the
#'supplied value. 
#'@examples
#' sess_res = ===ZZ===_test_mksession(session=list())
#' session = sess_res$session
#' input   = sess_res$input
#'
#' # Configuration files
#' FM_yaml_file  = system.file(package = "formods", "templates", "formods.yaml")
#' MOD_yaml_file = system.file(package = "===PKG===", "templates", "===ZZ===.yaml")
#'
#' # Creating an empty state object
#' state = ===ZZ===_fetch_state(id              = "===ZZ===",
#'                        input           = input,
#'                        session         = session,
#'                        FM_yaml_file    = FM_yaml_file,
#'                        MOD_yaml_file   = MOD_yaml_file,
#'                        react_state     = NULL)
#'
#' # Creates a new empty element
#' state = ===ZZ===_new_element(state)
#'
#' # Delete the current element
#' state = ===ZZ===_del_current_element(state)
#'
#' # Fetch a list of the current element
#' element = ===ZZ===_fetch_current_element(state)
#'
#' # You can modify the element
#' element[["name"]] = "A more descriptive name"
#'
#' # You can now place element back in the state
#' state = ===ZZ===_set_current_element(state, element)                                   
===ZZ===_set_current_element    = function(state, element){

  element_id = state[["===ZZ==="]][["current_element"]]

  state[["===ZZ==="]][["elements"]][[element_id]] = element

state}

#'@export
#'@title Deletes Current ===ELEMENT===
#'@description Takes a ===ZZ=== state and deletes the current ===ELEMENT===. 
#'If that is the last element, then a new default will be added. 
#'@param state ===ZZ=== state from \code{===ZZ===_fetch_state()}
#'@return ===ZZ== state object with the current ===ELEMENT=== deleted. 
#'@examples
#' sess_res = ===ZZ===_test_mksession(session=list())
#' session = sess_res$session
#' input   = sess_res$input
#'
#' # Configuration files
#' FM_yaml_file  = system.file(package = "formods", "templates", "formods.yaml")
#' MOD_yaml_file = system.file(package = "===PKG===", "templates", "===ZZ===.yaml")
#'
#' # Creating an empty state object
#' state = ===ZZ===_fetch_state(id              = "===ZZ===",
#'                        input           = input,
#'                        session         = session,
#'                        FM_yaml_file    = FM_yaml_file,
#'                        MOD_yaml_file   = MOD_yaml_file,
#'                        react_state     = NULL)
#'
#' # Creates a new empty element
#' state = ===ZZ===_new_element(state)
#'
#' # Delete the current element
#' state = ===ZZ===_del_current_element(state)
#'
#' # Fetch a list of the current element
#' element = ===ZZ===_fetch_current_element(state)
#'
#' # You can modify the element
#' element[["name"]] = "A more descriptive name"
#'
#' # You can now place element back in the state
#' state = ===ZZ===_set_current_element(state, element)                                   
===ZZ===_del_current_element    = function(state){

  # We need the current element and corresponding ID
  current_element = ===ZZ===_fetch_current_element(state)
  element_id = current_element[["id"]]

  # This deletes the current element ID
  state[["===ZZ==="]][["elements"]][[element_id]] = NULL

  if(length(names(state[["===ZZ==="]][["elements"]])) == 0){
    # This is triggered when we've deleted the last element, 
    # So now we will create a new one that will be active:
    state = ===ZZ===_new_element(state)
  } else {
    # If there is at least one left, we pull off the first 
    # one and make that active: 
    element_id = names(state[["===ZZ==="]][["elements"]])[1]
    state[["===ZZ==="]][["current_element"]] = element_id
  }

state}
