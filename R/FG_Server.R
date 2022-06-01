
#'@import rhandsontable
#'@import shiny
#'@import shinyWidgets
#'@importFrom shinyAce aceEditor updateAceEditor


#'@export
#'@title Figure Generation Server
#'@description Server function for the figure generation module
#'@param id An ID string that corresponds with the ID used to call the module's UI function
#'@param yaml_section  Section of the yaml file with the module configuration (`"FG"`)
#'@param yaml_file Upload Data cofiguration file
#'@param id_UD  ID string for the upload data module used to handle uploads or the name of the list element in react_state where the data set is stored.
#'@param id_DW  ID string for the data wrangling module to process any uploaded data
#'@param react_state Variable passed to server to allow reaction outside of module (`NULL`)
#'@return return
FG_Server <- function(id,
                yaml_section = "FG",
                yaml_file    = system.file(package = "formods", "templates", "config.yaml"),
                id_UD        = NULL,
                id_DW        = NULL,
                react_state  = NULL) {
  moduleServer(id, function(input, output, session) {

    #------------------------------------
    output$hot_fg_elements = rhandsontable::renderRHandsontable({
      req(input$select_dw_element)
      # Force update on button click
      input$button_dw_add_element
      # Force update on deletion clicks
      input$hot_dw_elements

      state = FG_fetch_state(id           = id,
                             input        = input,
                             session      = session,
                             yaml_file    = yaml_file,
                             yaml_section = yaml_section,
                             id_UD        = id_UD,
                             id_DW        = id_DW,
                             react_state  = react_state)
      uiele = NULL

      uiele})
    #------------------------------------
    output$ui_fg_preview   = renderPlot({
      #req(input$X)
      state = FG_fetch_state(id           = id,
                             input        = input,
                             session      = session,
                             yaml_file    = yaml_file,
                             yaml_section = yaml_section,
                             id_UD        = id_UD,
                             id_DW        = id_DW,
                             react_state  = react_state)
      uiele = NULL
      uiele})
    #------------------------------------
    output$ui_fg_select    = renderUI({
      #req(input$X)
      state = FG_fetch_state(id           = id,
                             input        = input,
                             session      = session,
                             yaml_file    = yaml_file,
                             yaml_section = yaml_section,
                             id_UD        = id_UD,
                             id_DW        = id_DW,
                             react_state  = react_state)
      uiele = NULL
      if(state[["FG"]][["isgood"]]){
        uiele = tagList()
        choicesOpt = list(
          subtext = c(
                      state[["MC"]][["elements"]][["line"   ]][["subtext"]],
                      state[["MC"]][["elements"]][["point"  ]][["subtext"]],
                      state[["MC"]][["elements"]][["hguide" ]][["subtext"]],
                      state[["MC"]][["elements"]][["vguide" ]][["subtext"]],
                      state[["MC"]][["elements"]][["facet"  ]][["subtext"]],
                      state[["MC"]][["elements"]][["label"  ]][["subtext"]]
                      ) #,
        # icon    = c(
        #             "glyphicon-filter" ,
        #             "glyphicon-wrench",
        #             "glyphicon-edit",
        #             "glyphicon-resize-small",
        #             "glyphicon-resize-full"
        #             )
        )

        cnames = c( state[["MC"]][["elements"]][["line"   ]][["choice"]] ,
                    state[["MC"]][["elements"]][["point"  ]][["choice"]] ,
                    state[["MC"]][["elements"]][["hguide" ]][["choice"]] ,
                    state[["MC"]][["elements"]][["vguide" ]][["choice"]] ,
                    state[["MC"]][["elements"]][["facet"  ]][["choice"]] ,
                    state[["MC"]][["elements"]][["label"  ]][["choice"]]
                  )
        choices   = c(
                      "line"  ,
                      "point" ,
                      "hguide" ,
                      "vguide" ,
                      "facet" ,
                      "label"
                     )
        names(choices) = cnames

        uiele = tagList(uiele,
          shinyWidgets::pickerInput(
            inputId = NS(id, "select_fg_element"),
            choices    = choices,
             width = "fit",
             inline = TRUE,
            choicesOpt = choicesOpt))

      }

      uiele})
    #------------------------------------
    output$ui_fg_curr_figs = renderUI({
      #req(input$X)
      state = FG_fetch_state(id           = id,
                             input        = input,
                             session      = session,
                             yaml_file    = yaml_file,
                             yaml_section = yaml_section,
                             id_UD        = id_UD,
                             id_DW        = id_DW,
                             react_state  = react_state)
      uiele = "ui_fg_curr_figs"
      uiele})
    #------------------------------------
    output$ui_fg_fig_name  = renderUI({
      #req(input$X)
      state = FG_fetch_state(id           = id,
                             input        = input,
                             session      = session,
                             yaml_file    = yaml_file,
                             yaml_section = yaml_section,
                             id_UD        = id_UD,
                             id_DW        = id_DW,
                             react_state  = react_state)



      uiele = NULL
      if(state[["FG"]][["isgood"]]){
        current_fig = FG_fetch_current_fig(state)
        value       = current_fig[["key"]]
        uiele = textInput(inputId      = NS(id, "text_fig_key"),
                          label        = NULL,
                          value        = value,
                          placeholder  = state[["MC"]][["labels"]][["ph"]][["fig_key"]])
      }

      uiele})
    #------------------------------------
    output$ui_fg_new_fig   = renderUI({
      #req(input$X)
      state = FG_fetch_state(id           = id,
                             input        = input,
                             session      = session,
                             yaml_file    = yaml_file,
                             yaml_section = yaml_section,
                             id_UD        = id_UD,
                             id_DW        = id_DW,
                             react_state  = react_state)

      uiele = NULL
      if(state[["FG"]][["isgood"]]){
        uiele = actionBttn(
                  inputId = NS(id, "button_fig_new"),
                  label   = state[["MC"]][["labels"]][["new_fig"]],
                  style   = "fill",
                  color   = "success",
                  icon    = icon("plus"))
      }
      uiele})
    #------------------------------------
    output$ui_fg_save_fig   = renderUI({
      #req(input$X)
      state = FG_fetch_state(id           = id,
                             input        = input,
                             session      = session,
                             yaml_file    = yaml_file,
                             yaml_section = yaml_section,
                             id_UD        = id_UD,
                             id_DW        = id_DW,
                             react_state  = react_state)
      uiele = NULL
      if(state[["FG"]][["isgood"]]){
        uiele = actionBttn(
                  inputId = NS(id, "button_fig_save"),
                  label   = state[["MC"]][["labels"]][["save_fig"]],
                  style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
                  color   = "primary",
                  icon    = icon("arrow-down"))
      }
      uiele})
    #------------------------------------
    output$ui_fg_del_fig   = renderUI({
      #req(input$X)
      state = FG_fetch_state(id           = id,
                             input        = input,
                             session      = session,
                             yaml_file    = yaml_file,
                             yaml_section = yaml_section,
                             id_UD        = id_UD,
                             id_DW        = id_DW,
                             react_state  = react_state)
      uiele = NULL
      if(state[["FG"]][["isgood"]]){
        uiele = actionBttn(
                  inputId = NS(id, "button_fig_del"),
                  label   = state[["MC"]][["labels"]][["del_fig"]],
                  style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
                  color   = "danger",
                  icon    = icon("minus"))
      }
      uiele})
    #------------------------------------
    output$ui_fg_fig_cap   = renderUI({
      #req(input$X)
      state = FG_fetch_state(id           = id,
                             input        = input,
                             session      = session,
                             yaml_file    = yaml_file,
                             yaml_section = yaml_section,
                             id_UD        = id_UD,
                             id_DW        = id_DW,
                             react_state  = react_state)



      uiele = NULL
      if(state[["FG"]][["isgood"]]){
        current_fig = FG_fetch_current_fig(state)
        value       = current_fig[["caption"]]
        uiele = textAreaInput(inputId     = NS(id, "text_fig_cap"),
                             width        = state[["MC"]][["dimensions"]][["caption"]][["width"]],
                             height       = state[["MC"]][["dimensions"]][["caption"]][["height"]],
                             label        = NULL,
                             value        = value,
                             placeholder  = state[["MC"]][["labels"]][["ph"]][["caption"]])

      }

      uiele})
    #------------------------------------
    output$ui_fg_add_element_button = renderUI({
      #req(input$X)
      state = FG_fetch_state(id           = id,
                             input        = input,
                             session      = session,
                             yaml_file    = yaml_file,
                             yaml_section = yaml_section,
                             id_UD        = id_UD,
                             id_DW        = id_DW,
                             react_state  = react_state)
      uiele = "ui_fg_add_element_button"
      uiele = NULL
      if(state[["FG"]][["isgood"]]){
        uiele = actionBttn(
                  inputId = NS(id, "button_element_add"),
                  label   = state[["MC"]][["labels"]][["add_ele"]],
                  style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
                  color   = "success",
                  icon    = icon("plus"))
      }
      uiele})
    #------------------------------------
    output$ui_fg_new_element_msg = renderText({
  #   # Force update on button click
  #   input$button_dw_add_element
       req(input$select_fg_element)
  #   state = DW_fetch_state(id           = id,
  #                          input        = input,
  #                          session      = session,
  #                          yaml_file    = yaml_file,
  #                          yaml_section = yaml_section,
  #                          id_UD        = id_UD,
  #                          react_state  = react_state)
  #
  #   uiele = NULL
  #   if(state[["DW"]][["DS"]][["isgood"]]){
  #     # If the add element message isn't NULL we return that.
  #     if(!is.null(state[["DW"]][["add_element_msg"]])){
  #       uiele = state[["DW"]][["add_element_msg"]]
  #     }
  #   }
  #
      uiele = "ui_dw_new_element_msg"
      uiele})
    #------------------------------------
    output$ui_fg_new_element_row = renderUI({
      req(input$select_fg_element)
      # force update when a new plot element is selected
      state = FG_fetch_state(id           = id,
                             input        = input,
                             session      = session,
                             yaml_file    = yaml_file,
                             yaml_section = yaml_section,
                             id_UD        = id_UD,
                             id_DW        = id_DW,
                             react_state  = react_state)
      # Pulling out the current plot element:
      curr_element = state[["FG"]][["ui"]][["select_fg_element"]]

      # These are the elements that are governed by aesthetics
      aes_elements = state[["FG"]][["aes_elements"]]

      uiele = NULL
      if(state[["FG"]][["isgood"]]){

        # The UI for plot elements governed by aesthetics are constructed
        # consistently
        if(curr_element %in% aes_elements){
          aes_list = tagList()
          for(aes_idx in 1:length(state[["MC"]][["elements"]][[curr_element]][["ui_aes"]])){
            # Aesthetic name
            ui_aes = state[["MC"]][["elements"]][[curr_element]][["ui_aes"]][aes_idx]
            # these are the IDs for the UI elements to be generated
            id_select = state[["MC"]][["elements"]][[curr_element]][["ui_aes_select_id"]][aes_idx]
            id_manual = state[["MC"]][["elements"]][[curr_element]][["ui_aes_manual_id"]][aes_idx]

            # Constructing the choices
            sel_names      = c()
            sel_choices    = c()
            sel_style      = c()

            # Making the first option Not Used if the aesthetic isn't
            # required:
            if(!ui_aes %in% state[["MC"]][["elements"]][[curr_element]][["aes_req"]]){
              sel_names      = c(sel_names  , state[["MC"]][["labels"]][["not_used"]])
              sel_choices    = c(sel_choices, "not_used")
              sel_style      = c(sel_style,
                  paste0("background: ", state[["yaml"]][["FM"]][["ui"]][["color_red"]] ,"; color: white;"))
            }

            # Adding the columns
            sel_names   = c(sel_names  , state[["FG"]][["DS"]][["columns"]])
            sel_choices = c(sel_choices, state[["FG"]][["DS"]][["columns"]])
            sel_style   = c(sel_style  , rep("", length(state[["FG"]][["DS"]][["columns"]])))

            # Adding manual option
            sel_names      = c(sel_names  , state[["MC"]][["labels"]][["manual"]])
            sel_choices    = c(sel_choices, 'manual')
            sel_style      = c(sel_style,
                paste0("background: ", state[["yaml"]][["FM"]][["ui"]][["color_blue"]] ,"; color: white;"))

            # For the columns in the dataset the names and the values are the
            # same but for not_used and manual the value displaed in the UI
            # may be changed so we need to make sure that the value returned
            # in the server is consistent. This will map the displayed value
            # to the value returned by the by the UI to the server:
            names(sel_choices) = sel_names

            aes_list    = tagList(aes_list,
              div(style="display:inline-block",
                div(
                  # Picker on top
                  pickerInput(
                    inputId    = NS(id, id_select),
                    label      = state[["MC"]][["labels"]][["elements"]][[ui_aes]],
                    choices    = sel_choices,
                    width      = state[["MC"]][["dimensions"]][["components"]][["aes"]][["width"]],
                    choicesOpt = list( style = sel_style)),
                  # Manual text input on the bottom
                  textInput(
                     inputId     = NS(id, id_manual),
                     label       = NULL,
                     placeholder = state[["MC"]][["labels"]][["ph"]][["manual"]],
                     width       = state[["MC"]][["dimensions"]][["components"]][["aes"]][["width"]])
                )
              )
            )
          }
          uiele = aes_list
        } else if(curr_element == "facet") {

          sel_choices = state[["FG"]][["DS"]][["columns"]]

          uiele =
            pickerInput(
              inputId    = NS(id, "select_component_facet"),
              label      = NULL,
              multiple   = TRUE,
              choices    = sel_choices,
              width      = state[["MC"]][["dimensions"]][["components"]][["facet"]][["width"]],
              options    = list(maxItems=2))
        } else if(curr_element == "label") {
          uiele =
            tagList(
            div(style="display:inline-block",
             textInput(
                inputId     = NS(id, "text_component_xlab"),
                label       = NULL,
                placeholder = state[["MC"]][["labels"]][["ph"]][["xlab"]],
                width       = state[["MC"]][["dimensions"]][["components"]][["label"]][["width"]])),
            div(style="display:inline-block",
             textInput(
                inputId     = NS(id, "text_component_ylab"),
                label       = NULL,
                placeholder = state[["MC"]][["labels"]][["ph"]][["ylab"]],
                width       = state[["MC"]][["dimensions"]][["components"]][["label"]][["width"]])),
            div(style="display:inline-block",
             textInput(
                inputId     = NS(id, "text_component_ggtitle"),
                label       = NULL,
                placeholder = state[["MC"]][["labels"]][["ph"]][["ggtitle"]],
                width       = state[["MC"]][["dimensions"]][["components"]][["label"]][["width"]]))
             )
        }
      }
      uiele})
    #------------------------------------
    output$ui_fg_curr_figs = renderUI({
      #req(input$X)
      state = FG_fetch_state(id           = id,
                             input        = input,
                             session      = session,
                             yaml_file    = yaml_file,
                             yaml_section = yaml_section,
                             id_UD        = id_UD,
                             id_DW        = id_DW,
                             react_state  = react_state)


      uiele  = tags$em(state[["MC"]][["labels"]][["curr_figs_none"]])

      uiele})
    #------------------------------------
    # Generating the figure generation code
    observe({
      # Forcing a reaction to changes from the upload data module
      react_state[[id_UD]]
      react_state[[id_DW]]

      # Force update on button click
      #input$button_dw_add_element
      # Force update on deletion clicks
      #input$hot_dw_elements

      state = FG_fetch_state(id           = id,
                             input        = input,
                             session      = session,
                             yaml_file    = yaml_file,
                             yaml_section = yaml_section,
                             id_UD        = id_UD,
                             id_DW        = id_DW,
                             react_state  = react_state)

      uiele = NULL

     #if(state[["DW"]][["DS"]][["isgood"]]){
     #  if(is.null(state[["DW"]][["elements_table"]])){
     #    uiele = "# No data wragling elements defined yet!"
     #  } else {
     #    uiele = paste(state[["DW"]][["elements_table"]][["cmd"]], collapse="\n")
     #  }
     #
     #  shinyAce::updateAceEditor(
     #    session         = session,
     #    editorId        = "ui_fg_code",
     #    theme           = state[["yaml"]][["FM"]][["code"]][["theme"]],
     #    showLineNumbers = state[["yaml"]][["FM"]][["code"]][["showLineNumbers"]],
     #    readOnly        = state[["MC"]][["code"]][["readOnly"]],
     #    mode            = state[["MC"]][["code"]][["mode"]],
     #    value           = uiele)
     #}

      })

    #------------------------------------
    # Creates the ui for the compact view of the module
    output$FG_ui_compact  =  renderUI({
      state = FG_fetch_state(id           = id,
                             input        = input,
                             session      = session,
                             yaml_file    = yaml_file,
                             yaml_section = yaml_section,
                             id_UD        = id_UD,
                             id_DW        = id_DW,
                             react_state  = react_state)

      uiele = NULL

    # uiele_main = tagList(
    #   div(style="display:inline-block", htmlOutput(NS(id, "ui_dw_select"))),
    #   div(style="display:inline-block", htmlOutput(NS(id, "ui_dw_add_element_button"))),
    #    htmlOutput(NS(id, "ui_dw_new_element_row")),
    #    verbatimTextOutput(NS(id, "ui_dw_new_element_msg")),
    #    rhandsontable::rHandsontableOutput(NS(id, "hot_dw_elements")))
    # if( state$MC$compact$preview){
    #   uiele_main = tagList(uiele_main, tags$br(),
    #     rhandsontable::rHandsontableOutput(NS(id, "hot_data_preview")))
    # }
    #
    # if( state$MC$compact$code){
    # # uiele_preview = tagList(htmlOutput(NS(id, "UD_ui_data_preview")))
    #   uiele_code = tagList(shinyAce::aceEditor(NS(id, "ui_dw_code")))
    #
    #   uiele_str ="tabPanel(state$MC$labels$tab_main,   uiele_main)"
    #   if(state$MC$compact$code){
    #     uiele_str = paste0(uiele_str, ",tabPanel(state$MC$labels$tab_code, uiele_code)") }
    #
    #   uiele_str = paste0("tabsetPanel(",uiele_str, ")")
    #
    #   uiele = eval(parse(text=uiele_str))
    # } else {
    #   uiele = uiele_main
    # }

    uiele})
    #------------------------------------
    # Creating reaction if a variable has been specified
    if(!is.null(react_state)){
      # Here we list the ui inputs that will result in a state change:
      toListen <- reactive({
        list(input$button_element_add,
             input$button_fig_new,
             input$button_fig_save,
             input$button_fig_fig)
      })
      # This updates the reaction state:
      observeEvent(toListen(), {
        react_state[[id]] = FG_fetch_state(
                             id           = id,
                             input        = input,
                             session      = session,
                             yaml_file    = yaml_file,
                             yaml_section = yaml_section,
                             id_UD        = id_UD,
                             id_DW        = id_DW,
                             react_state  = react_state)
      })
    }
  })
}


#'@export
#'@title Fetch Figure Generation State
#'@description Merges default app options with the changes made in the UI
#'@param id Shiny module ID
#'@param input Shiny input variable
#'@param session Shiny session variable
#'@param yaml_file cofiguration file
#'@param yaml_section  Section of the yaml file with the module configuration
#'@param id_UD  ID string for the upload data module used to handle uploads or the name of the list element in react_state where the data set is stored.
#'@param id_DW  ID string for the data wrangling module to process any uploaded data
#'@param react_state Variable passed to server to allow reaction outside of module (`NULL`)
#'@return list containing the current state of the app including default
#'values from the yaml file as well as any changes made by the user
FG_fetch_state = function(id,
                          input,
                          session,
                          yaml_file,
                          yaml_section,
                          id_UD = NULL,
                          id_DW = NULL,
                          react_state){

  # After the app has loaded the state must be initialized
  FM_FG_ID = paste0("FM_FG_", id)

  #---------------------------------------------
  # Getting the current state
  if(is.null(session$userData[[FM_FG_ID]])){
    # General state information
    state = FG_init_state(yaml_file    = yaml_file,
                          yaml_section = yaml_section,
                          id_UD        = id_UD,
                          id_DW        = id_DW,
                          react_state  = react_state)
  } else {
    # If it's not null we just pluck the state
    # from the session variable
    state = session$userData[[FM_FG_ID]]
  }

 ##---------------------------------------------
 ## Here we update the state based on user input
 ## Dataset changes
 #if(is.null(state[["DW"]][["DS"]])){
 #  if(!is.null(react_state)){
 #    # This contains the input dataset:
 #    state[["DW"]][["DS"]] = isolate(react_state[[id_UD]][["DS"]])
 #
 #    # This contains the output dataset which when initialized will
 #    # be just the input above:
 #    state = set_wds(state, state[["DW"]][["DS"]][["contents"]])
 #  } else {
 #    state[["DW"]][["DS"]][["isgood"]] = FALSE
 #    state[["DW"]][["isgood"]]         = FALSE
 #  }
 #}

 ##---------------------------------------------
 ## Getting the current ui elements into the state

  for(ui_name in state[["FG"]][["ui_ids"]]){
    if(!is.null(isolate(input[[ui_name]]))){
      state[["FG"]][["ui"]][[ui_name]] = isolate(input[[ui_name]])
    } else {
      state[["FG"]][["ui"]][[ui_name]] = ""
    }
  }
  # Saving the state
  session$userData[[FM_FG_ID]] = state

  # Returning the state

state}


#'@export
#'@title Initialize FG Module State
#'@description Creates a list of the initialized module state
#'@param yaml_file App cofiguration file
#'@param yaml_section  Section of the yaml file with the module configuration
#'@param id_UD  ID string for the upload data module used to handle uploads or the name of the list element in react_state where the data set is stored.
#'@param id_DW  ID string for the data wrangling module to process any uploaded data
#'@param react_state Variable passed to server to allow reaction outside of module (`NULL`)
#'@return list containing an empty app state object
FG_init_state = function(yaml_file, yaml_section, id_UD, id_DW, react_state){
  state = list()
  # Reading in default information from the yaml file
  state[["yaml"]] = yaml::read_yaml(yaml_file)

  # This assigns the module config "MC" element to the correct yaml_section.
  state[["MC"]] = state[["yaml"]][[yaml_section]]

  isgood = TRUE
  #---------------------------------------------
  # Finding the dataset
  DS = FM_find_DS(id_UD       = id_UD,
                  id_DW       = id_DW,
                  react_state = react_state)
  # If the dataset isn't good then we need to
  # flag the whole module as not being good
  if(!DS[["isgood"]]){
    isgood = FALSE
  }

  #---------------------------------------------
  # Creating UI ids for each aesthetic in each element
  ui_ids = c()
  for(element in names(state[["MC"]][["elements"]])){
    if("ui_aes" %in% names(state[["MC"]][["elements"]][[element]])){
      select_id = paste0("select_component_", state[["MC"]][["elements"]][[element]][["ui_aes"]])
      manual_id = paste0("text_component_", state[["MC"]][["elements"]][[element]][["ui_aes"]], "_manual")

      # Appending the IDs to the full list
      ui_ids = c(ui_ids, select_id, manual_id)
      # Saving the ids corresponding to the elements here:
      state[["MC"]][["elements"]][[element]][["ui_aes_select_id"]] = select_id
      state[["MC"]][["elements"]][[element]][["ui_aes_manual_id"]] = manual_id
    }
  }

  # Adding other ui_ids here
  ui_ids = c(ui_ids,
    "button_fig_new",
    "button_fig_save",
    "button_fig_del",
    "button_element_add",
    "text_fig_key",
    "text_fig_cap",
    "text_component_xlab",
    "text_component_ylab",
    "text_component_ggtitle",
    "select_component_facet",
    "select_fg_element")

  # Plot elements defined by aesthetics
  aes_elements = c("line", "point", "hguide", "vguide")

  # Since some IDs can be reused in the elements above we do this to
  # remove any extras:
  ui_ids = unique(ui_ids)

  # Defaults for the module
  FG_NULL =
    list(isgood           = isgood,
         DS               = DS,            # Dataset
        #add_counter      = 0,             # Counter tracking the ds_add_element button
        #fig_id           = 0,             # Current figure ID
         aes_elements     = aes_elements,  # Plot elements defined by aesthetics
         figs             = NULL,          # Placeholder for the figures
         fig_cntr         = 0,             # Internal counter for creating unique figure ids
         current_fig      = NULL,          # currently active fig id
         ui_ids           = ui_ids         # List of the possible ui_ids in the model
         )

  state[["FG"]] = FG_NULL

  # Initializing an empty figure
  state = FG_new_fig(state)

state}


#'@export
#'@title Initialize New Figure
#'@description Creates a new figure in a FG module
#'@param state FG state created with FG_init_state
#'@return list containing an empty app state object
FG_new_fig    = function(state){

  # Incrementing the figure counter
  state[["FG"]][["fig_cntr"]] = state[["FG"]][["fig_cntr"]] + 1

  # Creating a default figure ID
  fig_id = paste0("Fig_", state[["FG"]][["fig_cntr"]])

  # This is the object that contains the different components of the figure:
  fig_def =
    list(key            = fig_id,
         caption        = NULL,
         elements_table = NULL)

  # Storing the empty figure object in the state
  state[["FG"]][["figs"]][[fig_id]] = fig_def

  # Setting the new figure id as the curernt figure
  state[["FG"]][["current_fig"]]    = fig_id

state}

#'@export
#'@title Fetchs Current Figure
#'@description Takes an FG state and returns the ccurrent active figure
#'@param state FG state created with FG_init_state
#'@return list containing an empty app state object
FG_fetch_current_fig    = function(state){

  # Current figure ID
  fig_id = state[["FG"]][["current_fig"]]

  # Current figure
  fig = state[["FG"]][["figs"]][[fig_id]]

fig}
