#'@import rhandsontable
#'@import shiny
#'@import formods
#'@importFrom digest digest
#'@importFrom shinyAce aceEditor updateAceEditor

#'@export
#'@title ===ZZ_NAME=== State Server
#'@description Server function for the ===ZZ_NAME===  Shiny Module
#'@param id An ID string that corresponds with the ID used to call the modules UI elements
#'@param FM_yaml_file App configuration file with FM as main section.
#'@param MOD_yaml_file  Module configuration file with MC as main section.
#'@param deployed Boolean variable indicating whether the app is deployed or not.
#'@param react_state Variable passed to server to allow reaction outside of module (\code{NULL})
#'@return UD Server object
#'@examples
#' NOTE: you will need to create a server example
===ZZ===_Server <- function(id,
               FM_yaml_file  = system.file(package = "formods", "templates", "formods.yaml"),
               MOD_yaml_file = system.file(package = "===PKG===",  "templates", "===ZZ===.yaml"),
               deployed      = FALSE,
               react_state   = NULL) {
  moduleServer(id, function(input, output, session) {

    MOD_yaml_cont = FM_read_yaml(MOD_yaml_file)
    id_ASM = MOD_yaml_cont[["MC"]][["module"]][["depends"]][["id_ASM"]]

    #------------------------------------
    # Select the active ===ELEMENT===
    output$===ZZ===_ui_select_element = renderUI({
      input$button_clk_save
      input$button_clk_del
      input$button_clk_copy
      input$button_clk_new
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
    output$===ZZ===_ui_compact  =  renderUI({
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
     #   tooltip = shinyWidgets::tooltipOptions(title = state[["MC"]][["tooltips"]][["elements"]]))
     #)

      uiele = tagList(
        div(style="display:inline-block", "Place ===ELEMENT=== name, attributes and inputs here."),
        tags$br(),
        div(style="display:inline-block", verbatimTextOutput(NS(id, "ui_===zz===_msg")))
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
    # This will be used to trigger a response when the dependent modules have
    # changed
    force_mod_update = reactiveValues()
    # Use the following to force updates in the UI for things like when an
    # analysis is loaded and the ASM module is updated:
    # force_mod_update[["triggered"]]
      observe({
        react_state[[id_ASM]]

        state = FG_fetch_state(id             = id,
                               input          = input,
                               session        = session,
                               FM_yaml_file   = FM_yaml_file,
                               MOD_yaml_file  = MOD_yaml_file,
                               react_state    = react_state)

        FM_le(state, "upstream modules forcing update")
        force_mod_update[["triggered"]] = rnorm(n=1)
      }, priority = 100)


      # Here we list the ui inputs that will result in a state change:
      toListen <- reactive({
        list(
             force_mod_update[["triggered"]],
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

        # The hasds and hasmdl options below should be dynamic. So if it
        # currently has a dataset but then the user deletes the dataset it
        # should switch from TRUE to FALSE

        # Update hasds appropriately if the module currently provides datasets
        react_state[[id]][["===ZZ==="]][["hasds"]]    =  FALSE

        # Update hasmdl appropriately if the module currently provides models
        react_state[[id]][["===ZZ==="]][["hasmdl"]]    =  FALSE


      }, priority=99)
    }
    #------------------------------------
    # This can be used to trigger notifications
    # You need to add reactive inputs here when those
    # inputs can trigger a notification.
    toNotify <- reactive({
      list(
       input$button_clk_save,
       input$button_clk_copy,
       input$button_clk_del,
       input$button_clk_new
      )
    })
    observeEvent(toNotify(), {
      state = ===ZZ===_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      # Triggering optional notifications
      notify_res = formods::FM_notify(
        state   = state,
        session = session)
    })
    #------------------------------------
    # Copying element code to the clipboard
    observeEvent(input$button_clk_clip, {
      state = ===ZZ===_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      # This is all conditional on the whether clipr is installed $
      # and if the app isn't deployed
      if((system.file(package="clipr") != "") &
         !deployed){

          # Pulling out the current element
          current_ele = ===ZZ===_fetch_current_element(state)
          uiele = current_ele[["code_ele_only"]]

          clipr::write_clip(uiele)
        }
    })
    #------------------------------------
    # Removing holds
    # The react_state[[id_ASM]] is required in order to
    # load analyses using the application state manager
    remove_hold_listen  <- reactive({
        list(
             force_mod_update[["triggered"]],
           # input$button_clk_new,
           # input$button_clk_del,
           # input$button_clk_copy,
           # input$button_clk_save,
             input$element_selection
           # input$current_element
           )
      })
    observeEvent(remove_hold_listen(), {
      # Once the UI has been regenerated we
      # remove any holds for this module
      state = ===ZZ===_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      FM_le(state, "removing holds")
      # Removing all holds
      for(hname in names(state[["===ZZ==="]][["ui_hold"]])){
        remove_hold(state, session, hname)
      }
    }, priority = -100)

  })
}

#'@export
#'@title Fetch ===ZZ_NAME=== State
#'@description Merges default app options with the changes made in the UI
#'@param id Shiny module ID
#'@param id_ASM ID string for the app state management module used to save and load app states
#'@param input Shiny input variable
#'@param session Shiny session variable
#'@param FM_yaml_file App configuration file with FM as main section.
#'@param MOD_yaml_file  Module configuration file with MC as main section.
#'@param react_state Variable passed to server to allow reaction outside of module (\code{NULL})
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
#'}
#'@examples
#' # Within shiny both session and input variables will exist,
#' # this creates examples here for testing purposes:
#' sess_res = ===ZZ===_test_mksession()
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
  id_ASM = state[["MC"]][["module"]][["depends"]][["id_ASM"]]

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
       }

       # initializing the previous ui values as well:
       if(is.null(state[["===ZZ==="]][["ui_old"]][[ui_name]])){
         state[["===ZZ==="]][["ui_old"]][[ui_name]] = state[["===ZZ==="]][["ui"]][[ui_name]]
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
          has_updated(ui_val   = state[["===ZZ==="]][["ui"]][[ui_name]],
                      old_val  = state[["===ZZ==="]][["button_counters"]][[ui_name]],
                      init_val = c("", "0"))

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
          has_updated(ui_val   = state[["===ZZ==="]][["ui"]][[ui_name]],
                      old_val  = state[["===ZZ==="]][["ui_old"]][[ui_name]],
                      init_val = c(""))
        if(change_detected){
          changed_data_str = paste(state[["===ZZ==="]][["ui"]][[ui_name]], collapse=", ")
          changed_data_str = substr(changed_data_str, 1, 70)
          formods::FM_le(state, paste0("setting ===ELEMENT===: ", ui_name, " = ", changed_data_str))

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


    # This is a list of UI elements to skip when copying:
    ui_copy_skip = c("element_name")

    # Here we copy all the ui elements from old to new skipping those flagged
    # for skipping.
    for(tmp_ui_name in names(new_ele[["ui"]])){
      if(!(tmp_ui_name %in% ui_copy_skip)){
        new_ele[["ui"]][[tmp_ui_name]]  = old_ele[["ui"]][[tmp_ui_name]]
      }
    }

    # NOTE: You may need to add other code here
    # to change other aspects about the old element
    # that is being copied.

    state = ===ZZ===_set_current_element(
      state   = state,
      element = new_ele)
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
  #---------------------------------------------
  # selected ===ELEMENT=== changed
  if("element_selection" %in% changed_uis){
    state[["===ZZ==="]][["current_element"]] =
       state[["===ZZ==="]][["ui"]][["element_selection"]]

    # Setting the hold for all the other UI elements
    state = set_hold(state)
  }
  #---------------------------------------------
  # Passing any messages back to the user
  # NOTE: this only occurs when ui changes have been detected you may need to
  # add additional logic for a given module
  if(!is.null(changed_uis)){
    state = FM_set_ui_msg(state, msgs)
  }


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
#' sess_res = ===ZZ===_test_mksession()
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

  MOD_yaml_contents = FM_read_yaml(MOD_yaml_file)
  button_counters = MOD_yaml_contents[["MC"]][["ui_ids"]][["buttons"]]
  ui_module       = MOD_yaml_contents[["MC"]][["ui_ids"]][["module"]]

  # These are the module ui elements that are associated with
  # the current element
  ui_ele          = MOD_yaml_contents[["MC"]][["ui_ids"]][["element"]]

  # This contains all of the relevant ui_ids in the module. You need to append
  # ui_ids that are outside of the current element here as well.
  ui_ids          = c(button_counters, ui_ele, ui_ids)

  # Making all the ui_ids holdable
  ui_hold         = ui_ids

  state = FM_init_state(
    FM_yaml_file    = FM_yaml_file,
    MOD_yaml_file   = MOD_yaml_file,
    id              = id,
    # Add module dependencies here
    dep_mod_ids     = NULL,
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

  # initializing the module checksum:
  state = ===ZZ===_update_checksum(state)

  FM_le(state, "State initialized")

  # Saving the state (must be done before FM_fetch_deps below)
  FM_set_mod_state(session=session, id=id, state=state)

  # Setting the module dependencies
  state[["===ZZ==="]][["mod_deps"]] = FM_fetch_deps(state, session)

state}

#'@export
#'@title Fetch Module Code
#'@description Fetches the code to generate results seen in the app
#'@param state ===ZZ=== state from \code{===ZZ===_fetch_state()}
#'@return Character object vector with the lines of code
#'@example inst/test_apps/===ZZ===_funcs.R
===ZZ===_fetch_code = function(state){

  code = NULL

code}

#'@export
#'@title Append Report Elements
#'@description Appends report elements to a formods report.
#'@param state ===ZZ=== state from \code{===ZZ===_fetch_state()}
#'@param rpt Report with the current content of the report which will be appended to in
#'this function. For details on the structure see the documentation for
#' \code{\link[onbrand]{template_details}}
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
#'@seealso \code{\link[formods:FM_generate_report]{formods::FM_generate_report()}}
#'@examples
#' # We need a state object to use below
#' sess_res = ===ZZ===_test_mksession()
#' state = sess_res$state
#'
#' rpt = list(summary = list(), sheets=list())
#'
#' rpt_res = ===ZZ===_append_report(state,
#'   rpt     = rpt,
#'   rpttype = "xlsx")
#'
#' # Shows if report elements are present
#' rpt_res$hasrptele
#'
#' # Code chunk to generate report element
#' cat(paste(rpt_res$code, collapse="\n"))
#'
#' # Tabular summary of data views
#' rpt_res$rpt$summary
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
#'@param meta_only Include only metadata and not the dataset (default \code{FALSE})
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
#'    \item{id: Module ID.}
#'    \item{idx: unique numerical ID to identify this dataset in the module.}
#'    \item{res_label: optional label that can be defined by a user and used in
#'    workflows. Must be unique to the module.}
#'    \item{DS: Dataframe containing the actual dataset.}
#'    \item{DSMETA: Metadata describing DS.}
#'    \item{code: Complete code to build dataset.}
#'    \item{checksum: Module checksum.}
#'    \item{DSchecksum: Dataset checksum.}
#'  }
#'}
#'@examples
#' # We need a module state:
#' sess_res = ===ZZ===_test_mksession()
#' state = sess_res$state
#'
#' ds = ===ZZ===_fetch_ds(state)
#'
#' ds
===ZZ===_fetch_ds = function(state, meta_only=FALSE){
  hasds  = FALSE
  isgood = TRUE
  msgs   = c()
  ds     = list()

  mod_checksum = state[["===ZZ==="]][["checksum"]]

  # Empty list for new datasets
  NEWDS = list(label      = NULL,
               MOD_TYPE   = NULL,
               id         = state[["id"]],
               idx        = NULL,
               res_label  = "",
               DS         = NULL,
               DSMETA     = NULL,
               code       = NULL,
               checksum   = mod_checksum,
               DSchecksum = NULL)

  if(meta_only){
    tmp_DS = NULL
  } else {
    tmp_DS = data.frame()
  }

  # This prevents returning a dataset if this is triggered before data has
  # been loaded
  if(state[["===ZZ==="]][["isgood"]]){

    # Fill in the DS creation stuff here
    isgood = FALSE

    # Putting it all into the ds object to be returned
    #TMPDS = NEWDS
    #ds[[object_name]] = TMPDS
  }

  res = list(hasds  = hasds,
             isgood = isgood,
             msgs   = msgs,
             ds     = ds)
res}

#'@export
#'@title Fetch ===ZZ_NAME=== Module Models
#'@description Fetches the models contained in the module.
#'@param state ===ZZ=== state from \code{===ZZ===_fetch_state()}
#'@return list containing the following elements
#'\itemize{
#'  \item{isgood:}    Return status of the function.
#'  \item{hasmdl:}    Boolean indicator if the module has any models
#'  \item{msgs:}      Messages to be passed back to the user.
#'  \item{mdl:}       List with models. Each list element has the name of
#'  the R-object for that dataset. Each element has the following structure:
#'  \itemize{
#'    \item{label:}         Text label for the model (e.g. one-compartment model).
#'    \item{MOD_TYPE:}      Type of module.
#'    \item{id:}            Module ID.
#'    \item{idx:}           Numeric ID for the element.
#'    \item{rx_obj:}        The rxode2 object.
#'    \item{rx_obj_name:}   The rxode2 object name that holds the model.
#'    \item{ts_obj:}        List of timescale information for the system and
#'                          details of other timescales (\code{list(system="weeks", details = list(days=list(verb="days", conv=86400)))})
#'    \item{ts_obj_name:}   The object name that holds the timescale for this  model.
#'    \item{fcn_def:}      Text to define the model.
#'    \item{MDLMETA:}      Notes about the model.
#'    \item{code:}         Code to generate the model.
#'    \item{checksum:}     Module checksum.
#'    \item{MDLchecksum:} Model checksum.
#'  }
#'}
#'@examples
#' # We need a module state:
#' sess_res = ===ZZ===_test_mksession()
#' state = sess_res$state
#'
#' mdls = ===ZZ===_fetch_mdl(state)
#'
#' names(mdls)
===ZZ===_fetch_mdl = function(state){

  hasmdl     = FALSE
  isgood     = TRUE
  msgs       = c()
  mdl        = list()

 #if(state[["===ZZ==="]][["isgood"]]){
 #
 #  # Checksum for the module
 #  m_checksum = state[["===ZZ==="]][["checksum"]]
 #  elements = names(state[["===ZZ==="]][["elements"]])
 #  if(!is.null(elements)){
 #    # We have at least 1 model
 #    hasmdl = TRUE
 #    for(element in elements){
 #      # current element
 #      ce = state[["===ZZ==="]][["elements"]][[element]]
 #      ce_checksum = ce[["checksum"]]
 #
 #
 #      # NOTE: You need to populate teh NULL pieces below:
 #      mdl[[ ce[["rx_obj_name"]] ]] =
 #        list(label       = ce[["ui"]][["element_name"]],
 #             MOD_TYPE    = "===ZZ===",
 #             id          = state[["id"]],
 #             idx         = ce[["idx"]],
 #             rx_obj      = NULL, #
 #             rx_obj_name = NULL, #
 #             ts_obj      = NULL, #
 #             ts_obj_name = NULL, #
 #             fcn_def     = NULL, #
 #             MDLMETA     = NULL, #
 #             code        = NULL, #
 #             checksum    = m_checksum,
 #             MDLchecksum = ce_checksum)
 #    }
 #  }
 #
 #} else {
 #  isgood = FALSE
 #  msgs = c(msgs, "Bad ===ZZ=== state")
 #}

  res = list(hasmdl     = hasmdl,
             isgood     = isgood,
             msgs       = msgs,
             mdl        = mdl)
  res}


# JMH Add module checksum template
# JMH Add element checksum template

#'@export
#'@title Updates ===ZZ=== Module Checksum
#'@description Takes a ===ZZ=== state and updates the checksum used to trigger
#'downstream updates
#'@param state ===ZZ=== state from \code{===ZZ===_fetch_state()}
#'@return ===ZZ=== state object with the checksum updated
#'@example inst/test_apps/===ZZ===_funcs.R
===ZZ===_update_checksum     = function(state){

  # checksum string
  chk_str = ""

  # We'll concatinate all the individual checksums together
  # and create a checksum of those:
  element_ids = names(state[["===ZZ==="]][["elements"]])
  for(element_id in element_ids){
    # We trigger updates when the element changes:
    chk_str = paste0(chk_str, ":", state[["===ZZ==="]][["elements"]][[element_id]][["checksum"]])

    #JMH add element_name here?
  }

  # This prevents messaging when no change has been made to the module.
  old_chk = state[["===ZZ==="]][["checksum"]]
  new_chk = digest::digest(chk_str, algo=c("md5"))

  if(has_updated(old_chk, new_chk)){
    state[["===ZZ==="]][["checksum"]] = new_chk
    FM_le(state, paste0("module checksum updated: ", state[["===ZZ==="]][["checksum"]]))
  }

state}


#'@export
#'@title Populate Session Data for Module Testing
#'@description Populates the supplied session variable for testing.
#'@param session Shiny session variable (in app) or a list (outside of app)
#'@return The ===ZZ=== portion of the `all_sess_res` returned from \code{\link{FM_app_preload}}
#'@examples
#' session = shiny::MockShinySession$new()
#' sess_res = ===ZZ===_test_mksession(session=session)
===ZZ===_test_mksession = function(session = list()){

  sources = c(system.file(package="formods", "preload", "ASM_preload.yaml"),
              system.file(package="formods", "preload", "UD_preload.yaml"))
  res = FM_app_preload(session=list(), sources=sources)
  res = res[["all_sess_res"]][["===ZZ==="]]

res}

#'@export
#'@title New ===ZZ_NAME=== ===ELEMENT===
#'@description Appends a new empty ===ELEMENT=== to the ===ZZ=== state object
#'and makes this new ===ELEMENT=== the active ===ELEMENT===.
#'@param state ===ZZ=== state from \code{===ZZ===_fetch_state()}
#'@return ===ZZ=== state object containing a new ===ELEMENT=== and that
#'===ELEMENT=== is set as the current active ===ELEMENT===. See the help for
#'\code{===ZZ===_fetch_state()} for ===ELEMENT== format.
#'@example inst/test_apps/===ZZ===_funcs.R
===ZZ===_new_element = function(state){

  # Incrementing the element counter
  state[["===ZZ==="]][["element_cntr"]] = state[["===ZZ==="]][["element_cntr"]] + 1

  # Creating a default element ID
  element_id = paste0("element_", state[["===ZZ==="]][["element_cntr"]])

  # Creating the object name for this element
  element_object_name = paste0(state[["MC"]][["element_object_name"]],
                       "_", state[["===ZZ==="]][["element_cntr"]])
  # Default for a new element:
  element_def =
    list(
         # internal use only
         isgood                 = TRUE,
         # This will hold object names used in generated code
         objs                   = list(
           element_object_name    = element_object_name
         ),
         # This will hold the ui values for the current element
         ui                     = list(
           element_name  = paste0("===ELEMENT=== ", state[["===ZZ==="]][["element_cntr"]])
           ),
         id                     = element_id,
         idx                    = state[["===ZZ==="]][["element_cntr"]],
         code_previous          = NULL,
         # user facing
         # This is used if you build the element in a layering method sort of
         # like how the ggplot figures in the FG module builds using different
         # ggplot commands (layers).
         components_table       = NULL,
         # Generated on save
         checksum               = NULL,
         # code is the code to generate the element by itself. It assumes
         # any other module code, library calls, etc will be present before
         # this is run. It is used to generate the reproducible script on
         # export.
         code                   = NULL,
         # code_ele_only is meant to stand alone and be run to regenerate the
         # element by itself. It should contain any library calls and module
         # components that the current module and element depend on. It is
         # what you see in the code pull down for the current element in the
         # UI
         code_ele_only          = NULL)


  # This contains the code to generate inputs for the current element (e.g.
  # datasets that are needed).
  code_previous = ""
  element_def[["code_previous"]] = code_previous

  # Dropping the new element into the state
  state[["===ZZ==="]][["elements"]][[element_id]] = element_def


  # updating the checksum for the current element
  state[["===ZZ==="]][["elements"]][[element_id]][["checksum"]] = digest::digest(element_def, algo=c("md5"))

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
#'@example inst/test_apps/===ZZ===_funcs.R
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
#'@return ===ZZ=== state object with the current ===ELEMENT=== set using the
#'supplied value.
#'@example inst/test_apps/===ZZ===_funcs.R
===ZZ===_set_current_element    = function(state, element){

  element_id = state[["===ZZ==="]][["current_element"]]

  # updating the checksum for the current element
  tmp_ele = element
  tmp_ele[["checksum"]]  = ""

  tmp_checksum  = digest::digest(tmp_ele, algo=c("md5"))
  if(has_updated(element[["checksum"]], tmp_checksum)){
    FM_le(state, paste0("===ELEMENT=== checksum updated: ", tmp_checksum))
    element[["checksum"]]  = tmp_checksum
  }

  # this updates the current element
  state[["===ZZ==="]][["elements"]][[element_id]] = element

  # This will update the checksum for the module
  state = ===ZZ===_update_checksum(state)

state}

#'@export
#'@title Deletes Current ===ELEMENT===
#'@description Takes a ===ZZ=== state and deletes the current ===ELEMENT===.
#'If that is the last element, then a new default will be added.
#'@param state ===ZZ=== state from \code{===ZZ===_fetch_state()}
#'@return ===ZZ=== state object with the current ===ELEMENT=== deleted.
#'@example inst/test_apps/===ZZ===_funcs.R
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


#'@export
#'@title Processes State After Loading
#'@description When loading a saved analysis, this will process the state
#'object to account for differences that may apply between servers.
#'@param state ===ZZ=== state from \code{===ZZ===_fetch_state()}
#'@param session Shiny session variable
#'@return List contianing the ===ZZ=== state object and shiny session object
#'@examples
#' sess_res = ===ZZ===_test_mksession()
#' session = sess_res$session
#' state   = sess_res$state
#' state = ===ZZ===_onload(state, session)
===ZZ===_onload     = function(state, session){
  # Put any post processing you would use after loading here. If you do not
  # have any you can leave this function as a passthrough for the state object
  # or just delete it.


  res =
  list(state   = state,
       session = session)
res}


#'@export
#'@title Preload Data for ===ZZ=== Module
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
===ZZ===_preload  = function(session, src_list, yaml_res, mod_ID=NULL, react_state = list(), quickload=FALSE){
  isgood  = TRUE
  input   = list()
  msgs    = c()
  res     = c()
  err_msg = c()



  FM_yaml_file  = render_str(src_list[[mod_ID]][["fm_yaml"]])
  MOD_yaml_file = render_str(src_list[[mod_ID]][["mod_yaml"]])
  id_ASM        = yaml_res[[mod_ID]][["mod_cfg"]][["MC"]][["module"]][["depends"]][["id_ASM"]]
# id_UD         = yaml_res[[mod_ID]][["mod_cfg"]][["MC"]][["module"]][["depends"]][["id_UD"]]
# id_DW         = yaml_res[[mod_ID]][["mod_cfg"]][["MC"]][["module"]][["depends"]][["id_DW"]]

  # Creating an empty state object
  state = ===ZZ===_fetch_state(id              = mod_ID,
                               input           = input,
                               session         = session,
                               FM_yaml_file    = FM_yaml_file,
                               MOD_yaml_file   = MOD_yaml_file,
                               react_state     = react_state)

  # This saves the module to the session variable so it's there to be used
  # below
  if(!formods::is_shiny(session)){
    session = FM_set_mod_state(session, mod_ID, state)
  }

  elements = src_list[[mod_ID]][["elements"]]


  # Checks to see if we can add elements
  ADD_ELEMENTS = TRUE
  if(is.null(elements)){
    ADD_ELEMENTS = FALSE
  }

  if(ADD_ELEMENTS){
    # All of the numeric IDs in the preload
    enumeric    = c()

    # Map between list index and internal figure ID
    element_map = list()
    for(ele_idx in 1:length(elements)){
      enumeric = c(enumeric, elements[[ele_idx]][["element"]][["idx"]])
      element_map[[ paste0("element_",elements[[ele_idx]][["element"]][["idx"]] )]] = ele_idx
    }

    # Creating empty element placeholders
    while(state[["===ZZ==="]][["element_cntr"]] < max(enumeric)){
      state = ===ZZ===_new_element(state)
    }

    # culling any unneeded views
    for(ele_id  in names(state[["===ZZ==="]][["elements"]])){
      # This is a view that doesn't exist in elements so
      # we need to cull it
      if(!(ele_id  %in% names(element_map))){
        # Setting the view to be deleted as the current view
        state[["===ZZ==="]][["elements"]][[ ele_id  ]] = NULL
      }
    }


    # TODO: This adds the main ui components. These are not element specific
    # and apply to the entire module. For example the currently selected
    # element. 
    for(uiname in names(src_list[["===ZZ==="]][["ui"]])){
      state[["===ZZ==="]][["ui"]][[uiname]] = src_list[["===ZZ==="]][["ui"]][[uiname]]
    }

    # TODO: You need to process the elements and components here
    #browser()
    # Now we have empty elements defined
    for(element_id in names(element_map)){
      # Making the current element id active
      state[["===ZZ==="]][["current_element"]]  =  element_id
      ele_err_msg = c()

      # Getting the numeric position in the list corresponding
      # to the current element id
      ele_idx = element_map[[element_id]]
      ele_isgood = TRUE

      #-------------------------------------------------------
      # Defining general options
      FM_le(state, paste0("loading element idx: ", ele_idx ))

      current_ele = ===ZZ===_fetch_current_element(state)
      # Pulling the element level uis components from the preload. This just
      # replaces the default element uis with the ones on the preload. This
      # may require more finesse depending on how the module works:
      # current_ele[["ui"]] = elements[[ele_idx]][["element"]][["ui"]]

      # Place checks for required fields here:
      # req_ele_opts =c("field1", "field2")
      # if(!all(req_ele_opts    %in% names( elements[[ele_idx]][["element"]]))){
      #   ele_isgood      = FALSE
      #   missing_opts    = req_ele_opts[!(req_ele_opts %in% names(elements[[ele_idx]][["element"]]))]
      #   ele_err_msg = c(ele_err_msg,
      #     paste0("element idx:  ",ele_idx, " missing option(s):" ),
      #     paste0("  -> ", paste0(missing_opts, collapse=", "))
      #     )
      # }

      # If the module requires components check here:
      # if(!("components" %in% names(elements[[ele_idx]][["element"]]))){
      #   ele_isgood = FALSE
      #   ele_err_msg = c(ele_err_msg,
      #       paste0("element idx: ",ele_idx, " no components defined"))
      # }

      # Next we process the components 
      if(ele_isgood){

      }

      # This saves any changes made to the current element. You may need to
      # move it around depending on how functions operate
      state = ===ZZ===_set_current_element(
        state   = state,
        element = current_ele)

      if(ele_isgood){
        formods::FM_le(state,paste0("added element idx: ",ele_idx))
      } else {
        ele_err_msg = c(
          paste0("failed to add element idx: ",ele_idx),
          ele_err_msg)
        msgs = c(msgs, ele_err_msg)
        isgood = FALSE
      }
    }
  }

  if(!isgood && !is.null(err_msg)){
    formods::FM_le(state,err_msg,entry_type="danger")
    msgs = c(msgs, err_msg)
  }

  # Required for proper reaction:
  # Update hasds appropriately if the module provides a dataset
  react_state[[mod_ID]]  = list(===ZZ===  =
          list(checksum = state[["===ZZ==="]][["checksum"]],
               hasds    = FALSE))
  
  # Setting old ui values to current to prevent reactions on load
  for(ui_name in names(state[["===ZZ==="]][["ui"]])){
     state[["===ZZ==="]][["ui_old"]][[ui_name]] = state[["===ZZ==="]][["ui"]][[ui_name]] }
  current_ele  = ===ZZ===_fetch_current_element(state)
  for(ui_name in names(current_ele[["ui"]])){
     state[["===ZZ==="]][["ui_old"]][[ui_name]] = current_ele[["ui"]][[ui_name]] }

  formods::FM_le(state,paste0("module isgood: ",isgood))

  if(formods::is_shiny(session)){
    FM_set_mod_state(session, mod_ID, state)
  } else {
    session = FM_set_mod_state(session, mod_ID, state)
  }

  res = list(isgood      = isgood,
             msgs        = msgs,
             session     = session,
             input       = input,
             react_state = react_state,
             state       = state)

res}

#'@export
#'@title Make List of Current ===ZZ=== State
#'@description Reads in the app state from yaml files.
#'@param state ===ZZ=== state object
#'@return list with the following elements
#' \itemize{
#'   \item{isgood:}       Boolean indicating the exit status of the function.
#'   \item{msgs:}         Messages to be passed back to the user.
#'   \item{yaml_list:}    Lists with preload components.
#'}
#'@examples
#' sess_res = ===ZZ===_test_mksession()
#' state = sess_res$state
#' res = ===ZZ===_mk_preload(state)
===ZZ===_mk_preload     = function(state){
  isgood    = TRUE
  msgs      = c()
  err_msg   = c()
  yaml_list = list()

  ylist = list(
      fm_yaml  = file.path("config", basename(state[["FM_yaml_file"]])),
      mod_yaml = file.path("config", basename(state[["MOD_yaml_file"]]))
  )

  # Creating the yaml list with the module ID at the top level
  yaml_list = list()
  yaml_list[[ state[["id"]] ]]  = ylist

  ele_idx = 1
  # Walking through each element:
  for(element_id in names(state[["===ZZ==="]][["elements"]])){
    tmp_source_ele = state[["===ZZ==="]][["elements"]][[element_id]]

    FM_le(state, paste0("saving element (", tmp_source_ele[["idx"]], ") ", tmp_source_ele[["ui"]][["element_name"]]))

    tmp_element = list(
      idx               = tmp_source_ele[["idx"]],
      name              = tmp_source_ele[["ui"]][["element_name"]],
      components  = list())

    # Add element and component details here:

    # Appending element
    ylist[["elements"]][[ele_idx]] = list(element = tmp_element)
    ele_idx = ele_idx + 1
  }

  formods::FM_le(state,paste0("mk_preload isgood: ",isgood))

  yaml_list = list()
  yaml_list[[ state[["id"]] ]]  = ylist

  if(!isgood && !is.null(err_msg)){
    formods::FM_le(state,err_msg,entry_type="danger")
    msgs = c(msgs, err_msg)
  }

  res = list(
    isgood    = isgood,
    msgs      = msgs,
    yaml_list = yaml_list)
}
