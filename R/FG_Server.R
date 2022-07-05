#'@import ggplot2
#'@import rhandsontable
#'@import shiny
#'@import shinyWidgets
#'@importFrom shinyAce aceEditor updateAceEditor
#'@importFrom ggforce facet_grid_paginate facet_wrap_paginate
#'@importFrom plotly  ggplotly plotlyOutput renderPlotly


#'@export
#'@title Figure Generation Server
#'@description Server function for the figure generation module
#'@param id An ID string that corresponds with the ID used to call the module's UI function
#'@param yaml_section  Section of the yaml file with the module configuration (\code{"FG"})
#'@param yaml_file Upload Data cofiguration file
#'@param id_UD  ID string for the upload data module used to handle uploads or the name of the list element in react_state where the data set is stored.
#'@param id_DW  ID string for the data wrangling module to process any uploaded data
#'@param react_state Variable passed to server to allow reaction outside of module (\code{NULL})
#'@return FG Server object
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
    output$ui_fg_preview_ggplot   = renderPlot({
      #req(input$X)
      # Forcing reactions:
      input[["button_fig_new"]]
      input[["button_fig_save"]]
      input[["button_fig_del"]]
      input[["button_fig_copy"]]
      input[["button_fig_upds"]]
      input[["button_element_add"]]
      state = FG_fetch_state(id           = id,
                             input        = input,
                             session      = session,
                             yaml_file    = yaml_file,
                             yaml_section = yaml_section,
                             id_UD        = id_UD,
                             id_DW        = id_DW,
                             react_state  = react_state)

      current_fig = FG_fetch_current_fig(state)
      # Figuring out the page to use
      if(state[["FG"]][["ui"]][["select_fig_page"]] == ""){
        # Default to the first page if the select_fig_page ui element hasn't
        # been populated. It wont populate if there is only one page.
        fig_page =  names(current_fig[["pages"]])[1]
      } else {
        fig_page = state[["FG"]][["ui"]][["select_fig_page"]]
      }

      uiele = current_fig[["pages"]][[fig_page]][["p"]]

      uiele})
    #------------------------------------
    output$ui_fg_preview_plotly   = plotly::renderPlotly({
      #req(input$X)
      state = FG_fetch_state(id           = id,
                             input        = input,
                             session      = session,
                             yaml_file    = yaml_file,
                             yaml_section = yaml_section,
                             id_UD        = id_UD,
                             id_DW        = id_DW,
                             react_state  = react_state)

      current_fig = FG_fetch_current_fig(state)
      # Figuring out the page to use
      if(state[["FG"]][["ui"]][["select_fig_page"]] == ""){
        # Default to the first page if the select_fig_page ui element hasn't
        # been populated. It wont populate if there is only one page.
        fig_page =  names(current_fig[["pages"]])[1]
      } else {
        fig_page = state[["FG"]][["ui"]][["select_fig_page"]]
      }

      uiele = plotly::ggplotly(current_fig[["pages"]][[fig_page]][["p"]])


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
                      )
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
                  style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
                  size    = state[["MC"]][["formatting"]][["button_fig_new"]][["size"]],
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
                  size    = state[["MC"]][["formatting"]][["button_fig_save"]][["size"]],
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
                  size    = state[["MC"]][["formatting"]][["button_fig_del"]][["size"]],
                  color   = "danger",
                  icon    = icon("minus"))
      }
      uiele})
    #------------------------------------
    output$ui_fg_upds_fig   = renderUI({
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
                  inputId = NS(id, "button_fig_upds"),
                  label   = state[["MC"]][["labels"]][["upds_fig"]],
                  style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
                  size    = state[["MC"]][["formatting"]][["button_fig_upds"]][["size"]],
                  color   = "warning",
                  icon    = icon("sync"))
      }
      uiele})
    #------------------------------------
    output$ui_fg_copy_fig   = renderUI({
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
                  inputId = NS(id, "button_fig_copy"),
                  label   = state[["MC"]][["labels"]][["copy_fig"]],
                  style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
                  size    = state[["MC"]][["formatting"]][["button_fig_copy"]][["size"]],
                  color   = "royal",
                  icon    = icon("copy"))
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
                             width        = state[["MC"]][["formatting"]][["caption"]][["width"]],
                             height       = state[["MC"]][["formatting"]][["caption"]][["height"]],
                             label        = NULL,
                             value        = value,
                             placeholder  = state[["MC"]][["labels"]][["ph"]][["caption"]])
        if(state[["MC"]][["tooltips"]][["include"]]){
          if(!is.null(state[["MC"]][["tooltips"]][["caption"]])){
            uiele = tagList(uiele,
              shinyBS::bsPopover(NS(id,"text_fig_cap"),
                                 title=NULL,
                                 state[["MC"]][["tooltips"]][["caption"]],
                                 "bottom"))
          }
        }
      }

      uiele})
    #------------------------------------
    output$ui_fg_slider_page = renderUI({
      # This will update when the preview does
      req(input$ui_fg_preview)
      state = FG_fetch_state(id           = id,
                             input        = input,
                             session      = session,
                             yaml_file    = yaml_file,
                             yaml_section = yaml_section,
                             id_UD        = id_UD,
                             id_DW        = id_DW,
                             react_state  = react_state)

      # Figuring out the pages in the current figure
      current_fig = FG_fetch_current_fig(state)

      pages = names(current_fig[["pages"]])

      uiele = 1
      if(length(pages) > 1){
        uiele =
           pickerInput(
             inputId  = NS(id, "select_fig_page"),
             label    = NULL,
             width    = 200,
             choices  = pages
           # options  = list(
           #   title = state[["MC"]][["labels"]][["fds_mutate_column"]])
           )
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
                  size    = state[["MC"]][["formatting"]][["button_fig_add"]][["size"]],
                  color   = "success",
                  icon    = icon("plus"))
      }
      uiele})
    #------------------------------------
    output$ui_fg_button_click_msg = renderText({
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
      uiele = "ui_fg_button_click_msg"
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

      # Getting the current figure
      current_fig = FG_fetch_current_fig(state)

      # Getting the columns of the dataset attached to the current figure:
      fig_dscols = state[["FG"]][["DSV"]][["dsviews"]][["columns"]][[current_fig[["fig_dsview"]]]]

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
            sel_names   = c(sel_names  , fig_dscols)
            sel_choices = c(sel_choices, fig_dscols)
            sel_style   = c(sel_style  , rep("", length(fig_dscols)))

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

            # Defining the tool tip for the manual text elements
            manual_tool_tip = NULL
            if(state[["MC"]][["tooltips"]][["include"]]){
              if(!is.null(state[["MC"]][["tooltips"]][["components"]][["manual"]][[ui_aes]])){
                manual_tool_tip =shinyBS::bsPopover(NS(id,id_manual),
                                                    title=NULL,
                                                    state[["MC"]][["tooltips"]][["components"]][["manual"]][[ui_aes]],
                                                    "bottom")
              }
            }

            aes_list    = tagList(aes_list,
              div(style="display:inline-block",
                div(
                  # Picker on top
                  pickerInput(
                    inputId    = NS(id, id_select),
                    label      = state[["MC"]][["labels"]][["components"]][[ui_aes]],
                    choices    = sel_choices,
                    width      = state[["MC"]][["formatting"]][["components"]][["aes"]][["width"]],
                    choicesOpt = list( style = sel_style, "live-search"=TRUE)),
                  # Manual text input on the bottom
                  textInput(
                     inputId     = NS(id, id_manual),
                     label       = NULL,
                     placeholder = state[["MC"]][["labels"]][["ph"]][["manual"]],
                     width       = state[["MC"]][["formatting"]][["components"]][["aes"]][["width"]]),
                    manual_tool_tip
                )
              )
            )
          }
          uiele = aes_list
        } else if(curr_element == "facet") {

          sel_choices = fig_dscols

          uiele =
            pickerInput(
              inputId    = NS(id, "select_component_facet"),
              label      = NULL,
              multiple   = TRUE,
              choices    = sel_choices,
              width      = state[["MC"]][["formatting"]][["components"]][["facet"]][["width"]],
              options    = list(maxItems=2))
        } else if(curr_element == "label") {
          uiele =
            tagList(
            div(style="display:inline-block",
             textInput(
                inputId     = NS(id, "text_component_xlab"),
                label       = NULL,
                placeholder = state[["MC"]][["labels"]][["ph"]][["xlab"]],
                width       = state[["MC"]][["formatting"]][["components"]][["label"]][["width"]])),
            div(style="display:inline-block",
             textInput(
                inputId     = NS(id, "text_component_ylab"),
                label       = NULL,
                placeholder = state[["MC"]][["labels"]][["ph"]][["ylab"]],
                width       = state[["MC"]][["formatting"]][["components"]][["label"]][["width"]])),
            div(style="display:inline-block",
             textInput(
                inputId     = NS(id, "text_component_ggtitle"),
                label       = NULL,
                placeholder = state[["MC"]][["labels"]][["ph"]][["ggtitle"]],
                width       = state[["MC"]][["formatting"]][["components"]][["label"]][["width"]]))
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

      if(is.null(names(state[["FG"]][["figs"]]))){
        uiele  = tags$em(state[["MC"]][["labels"]][["curr_figs_none"]])
      } else {

        # This is the current fig ID
        current_fig_id = state[["FG"]][["current_fig"]]

        choices = c()
        cnames   = c()
        subtext     = c()

        for(fig_id in names(state[["FG"]][["figs"]])){
           tmp_fig = state[["FG"]][["figs"]][[fig_id]]
           # Creating the select subtext from the caption
           if(is.null(tmp_fig[["caption"]])){
             subtext = c(subtext, "")
           } else {
             subtext = c(subtext, strtrim(tmp_fig[["caption"]], 20))
           }

           choices = c(choices, fig_id)
           cnames  = c(cnames,  tmp_fig[["key"]])
        }

        choicesOpt = list( subtext = subtext)
        names(choices) = cnames

        uiele =
        shinyWidgets::pickerInput(
          selected   = current_fig_id,
          inputId    = NS(id, "select_current_fig"),
          label      = state[["MC"]][["labels"]][["select_current_fig"]],
          choices    = choices,
          width      = state[["MC"]][["formatting"]][["select_current_fig"]][["width"]],
          choicesOpt = choicesOpt)
      }

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
             input$button_fig_upds,
             input$button_fig_copy,
             input$button_fig_del)
      })
      # This updates the reaction state:
      observeEvent(toListen(), {
        state = FG_fetch_state(
                             id           = id,
                             input        = input,
                             session      = session,
                             yaml_file    = yaml_file,
                             yaml_section = yaml_section,
                             id_UD        = id_UD,
                             id_DW        = id_DW,
                             react_state  = react_state)

        FG_le(state, "reaction state updated")
        react_state[[id]] = state
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
#'@param react_state Variable passed to server to allow reaction outside of module (\code{NULL})
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

 #---------------------------------------------
 # Here we update the state based on user input
 for(ui_name in state[["FG"]][["ui_ids"]]){
   if(!is.null(isolate(input[[ui_name]]))){
      state[["FG"]][["ui"]][[ui_name]] = isolate(input[[ui_name]])
    } else {
      state[["FG"]][["ui"]][[ui_name]] = ""
    }
  }

  #---------------------------------------------
  # Reacting to button clicks
  # Adding a new element
  if(has_changed(ui_val   = state[["FG"]][["ui"]][["button_element_add"]],
                 old_val  = state[["FG"]][["button_counters"]][["add"]])){

    FG_le(state, "adding figure element")
    msgs = c()

    # Building the plot element command
    fgb_res = fers_builder(state)

    # saving the messages
    msgs = c(msgs,  fgb_res[["msgs"]])


    # If figure generation command was successfully built we
    # evaluate this element to make sure it works correctly
    if( fgb_res[["isgood"]]){
      # Evaluating the element
      fbee_res = FG_eval(state,  fgb_res[["cmd"]])
      # Appending any messages
      msgs = c(msgs, fbee_res[["msgs"]])

      if(fbee_res[["isgood"]]){
        current_fig_id = state[["FG"]][["current_fig"]]

        # Getting the current elements table
        ET = state[["FG"]][["figs"]][[current_fig_id]][["elements_table"]]

        # Appending the new element:
        ET = rbind(ET,
          data.frame(
          Element     = fgb_res[["element"]],
          Description = fgb_res[["desc"]],
          cmd         = fgb_res[["cmd"]],
          Status      = "Success",
          Delete        = FALSE))
        # Sticking it back in the elements tbale
        state[["FG"]][["figs"]][[current_fig_id]][["elements_table"]] = ET

        # Saving the new pages:
        state[["FG"]][["figs"]][[current_fig_id]][["pages"]] = fbee_res[["pages"]]
      }
    }

    # Saving the button state to the counter
    state[["FG"]][["button_counters"]][["add"]] =
      state[["FG"]][["ui"]][["button_element_add"]]

    # Updating any messages
    state = FG_set_button_click_msgs(state, msgs)
  }
  # New figure
  if(has_changed(ui_val   = state[["FG"]][["ui"]][["button_fig_new"]],
                 old_val  = state[["FG"]][["button_counters"]][["new"]])){

    FG_le(state, "creating new figure")
    msgs = c()

    # Saving the button state to the counter
    state[["FG"]][["button_counters"]][["new"]] =
      state[["FG"]][["ui"]][["button_fig_new"]]

    # Updating any messages
    state = FG_set_button_click_msgs(state, msgs)
  }
  # Delete figure
  if(has_changed(ui_val   = state[["FG"]][["ui"]][["button_fig_del"]],
                 old_val  = state[["FG"]][["button_counters"]][["del"]])){

    FG_le(state, "deleting figure view")
    msgs = c()
    # Saving the button state to the counter
    state[["FG"]][["button_counters"]][["del"]] =
      state[["FG"]][["ui"]][["button_fig_del"]]

    # Updating any messages
    state = FG_set_button_click_msgs(state, msgs)
  }
  # Save figure
  if(has_changed(ui_val   = state[["FG"]][["ui"]][["button_fig_save"]],
                 old_val  = state[["FG"]][["button_counters"]][["save"]])){

    FG_le(state, "saving changes to current figure")
    msgs = c()
    # Saving the button state to the counter
    state[["FG"]][["button_counters"]][["save"]] =
      state[["FG"]][["ui"]][["button_fig_save"]]

    # Updating any messages
    state = FG_set_button_click_msgs(state, msgs)
  }
  # Copy figure
  if(has_changed(ui_val   = state[["FG"]][["ui"]][["button_fig_copy"]],
                 old_val  = state[["FG"]][["button_counters"]][["copy"]])){

    FG_le(state, "copying figure")
    msgs = c()
    # Saving the button state to the counter
    state[["FG"]][["button_counters"]][["copy"]] =
      state[["FG"]][["ui"]][["button_fig_copy"]]

    # Updating any messages
    state = FG_set_button_click_msgs(state, msgs)
  }
  # Update dataset for figure
  if(has_changed(ui_val   = state[["FG"]][["ui"]][["button_fig_upds"]],
                 old_val  = state[["FG"]][["button_counters"]][["upds"]])){

    FG_le(state, "updating dataset")
    msgs = c()
    # Saving the button state to the counter
    state[["FG"]][["button_counters"]][["upds"]] =
      state[["FG"]][["ui"]][["button_fig_upds"]]

    # Updating any messages
    state = FG_set_button_click_msgs(state, msgs)
  }

  # Saving the session location
  state[["SESSION_LOCATION"]] = FM_FG_ID

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
#'@param react_state Variable passed to server to allow reaction outside of module (\code{NULL})
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
  DSV = FM_fetch_dsviews(
    state       = state,
    id_UD       = id_UD,
    id_DW       = id_DW,
    react_state = react_state)


  # Plot elements defined by aesthetics
  aes_elements = c("line", "point", "hguide", "vguide")

  # This will hold the ids of the UI elements that need to be collected
  # when module fetch_state function is called. Some of them will be
  # specified explicitly and others will be generated on the fly from the
  # configuration file.
  ui_ids = c()

  # If the dataset isn't good then we need to
  # flag the whole module as not being good
  if(!DSV[["isgood"]]){
    isgood = FALSE
  }

  # We only do the rest if
  if(isgood){
    #---------------------------------------------
    # Creating UI ids for each aesthetic in each element
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
      "button_fig_copy",
      "button_fig_upds",
      "button_element_add",
      "text_fig_key",
      "text_fig_cap",
      "text_component_xlab",
      "text_component_ylab",
      "text_component_ggtitle",
      "select_fig_page",
      "select_current_fig",
      "select_component_facet",
      "select_fg_element")

    # Since some IDs can be reused in the elements above we do this to
    # remove any extras:
    ui_ids = unique(ui_ids)
  }

  # Defaults for the module
  FG_NULL =
    list(isgood           = isgood,
      button_counters = list(            # Counters to track button clicks
        "add"             = 0,           # Element: Adding a new element
        "save"            = 0,           # Figure:  Saving the current figure
        "new"             = 0,           # Figure:  New blank figure
        "del"             = 0,           # Figure:  Delete the current figure
        "copy"            = 0,           # Figure:  Copy the current figure
        "upds"            = 0),          # Figure:  Update the dataset for the current figure
      aes_elements     = aes_elements,   # Plot elements defined by aesthetics
      figs             = NULL,           # Placeholder for the figures
      fig_cntr         = 0,              # Internal counter for creating unique figure ids
      current_fig      = NULL,           # currently active fig id
      ui_hold          = list(),         # List of states to hold to prevent updates/refresh until after ui has been rebuilt with the curretn state
      ui_ids           = ui_ids,         # List of the possible ui_ids in the model
      DSV              = DSV             # List containing the dataset views from FM_fetch_dsviews()
      )

  state[["FG"]] = FG_NULL

  if(isgood){
    # Initializing an empty figure
    state = FG_new_fig(state, id_UD, id_DW, react_state)
  }

  state[["MOD_TYPE"]] = "FG"

  FG_le(state, "State initialized")

state}


#'@export
#'@title Initialize New Figure
#'@description Creates a new figure in a FG module
#'@param state FG state from \code{FG_fetch_state()}
#'@param id_UD  ID string for the upload data module used to handle uploads or the name of the list element in react_state where the data set is stored.
#'@param id_DW  ID string for the data wrangling module to process any uploaded data
#'@param react_state Variable passed to server to allow reaction outside of module (\code{NULL})
#'@return FG state object containing a new empty figure  and that figure set as the
#'current active figure
FG_new_fig    = function(state, id_UD, id_DW, react_state){

  # Incrementing the figure counter
  state[["FG"]][["fig_cntr"]] = state[["FG"]][["fig_cntr"]] + 1

  # Creating a default figure ID
  fig_id = paste0("Fig_", state[["FG"]][["fig_cntr"]])

  # Initialzing the ggplot object

  # Pulling out the dataset views
  DSV = state[["FG"]][["DSV"]]

  # Using the default dsview for the new figure
  fig_dsview = DSV[["active"]]


  # Creating the dataset object
  # This object contains the name of the dataset
  ds_object_name = DSV[["dsviews"]][["object_name"]][[fig_dsview]]

  # Creating that object loally
  assign(ds_object_name, DSV[["dsviews"]][["contents"]][[fig_dsview]])

  # ggplot initialization code:
  fg_object_name = state[["MC"]][["fg_object_name"]]
  code_init = paste0(fg_object_name, " = ggplot2::ggplot(data=", ds_object_name,")")

  # This is the object that contains the different components of the figure:
  fig_def =
    list(key            = fig_id,
         num_pages      = 1,
         pages          = NULL,
         fig_dsview     = fig_dsview,
         UD_checksum    = DSV[["UD_checksum"]],
         DW_checksum    = DSV[["DW_checksum"]],
         DSV_checksum   = DSV[["dsviews"]][["checksum"]][[fig_dsview]],
         code_init      = code_init,
         caption        = NULL,
         elements_table = NULL)

  # Storing the empty figure object in the state
  state[["FG"]][["figs"]][[fig_id]] = fig_def

  # Setting the new figure id as the current figure
  state[["FG"]][["current_fig"]]    = fig_id

  # Creating the new figure pages
  new_fig = FG_eval(state, NULL)
  state[["FG"]][["figs"]][[fig_id]][["pages"]] =  new_fig[["pages"]]

state}

#'@export
#'@title Fetchs Current Figure
#'@description Takes an FG state and returns the ccurrent active figure
#'@param state FG state from \code{FG_fetch_state()}
#'@return list containing the current figure       
FG_fetch_current_fig    = function(state){

  # Current figure ID
  fig_id = state[["FG"]][["current_fig"]]

  # Current figure
  fig = state[["FG"]][["figs"]][[fig_id]]

fig}

#'@export
#'@title Builds a Figure Element R Statement From UI Elements:
#'@description Takes the current ui elements and constructs the appropriate
#'ggplot commands forom the user input. The plot commands assume the existance
#'of a ggplot object \code{p}.
#'@param state FG state from \code{FG_fetch_state()}
#'@return list containing the following elements
#'\itemize{
#'  \item{isgood:} Return status of the function.
#'  \item{cmd:}    ggplot R command as a character string
#'  \item{element:} The type of element being added
#'  \item{desc:}   Verbose description of the element
#'  \item{msgs:}   Messages to be passed back to the user
#'}
fers_builder = function(state){

  isgood = TRUE
  msgs   = c()
  cmd    = ""
  desc   = ""
  descs  = c()
  element= ""

  element        = state[["FG"]][["ui"]][["select_fg_element"]]
  ui             = state[["FG"]][["ui"]]
  aes_elements   = state[["FG"]][["aes_elements"]]
  fg_object_name = state[["MC"]][["fg_object_name"]]

  # Pulling out the element configuration from the yaml file:
  element_cfg = state[["MC"]][["elements"]][[element]]

  if(element%in% aes_elements){

    # The geom function name:
    fcn = element_cfg[["fcn"]]

    # Now we walk through each component and constrcut the aesthetic and
    # manual portions of the geom
    man_comp = c()
    aes_comp = c()
    aes_req  = element_cfg[["aes_req"]]
    for(aes_idx in 1:length(element_cfg[["ui_aes"]])){

      comp_idx     = element_cfg[["ui_aes"]][aes_idx]
      sel_idx      = element_cfg[["ui_aes_select_id"]][aes_idx]
      man_idx      = element_cfg[["ui_aes_manual_id"]][aes_idx]

      # First we check to make sure that the selection is not "not_used"
      # this means it's either an aesthetic or manual specification
      if(ui[[sel_idx]] != "not_used"){

        if(ui[[sel_idx]] == "manual"){
          # We need to make sure that the actually input a manual value
          if(ui[[man_idx]] == ""){
            # This is the default value when reading in the ui, so we need to
            # fail the element and return an error message:
            man_msg = state[["MC"]][["labels"]][["msg_bad_manual_comp"]]
            man_msg = stringr::str_replace_all(man_msg, "===COMP===",  comp_idx)

            isgood = FALSE
            msgs = c(msgs, man_msg)
          } else {
            # If the selection is "manual" then we add it to
            # the list of manual components
            man_rhs = autocast(ui[[man_idx]], quote_char=TRUE)
            man_comp = c(man_comp, paste0(comp_idx, "=", man_rhs))

            # updating description
            descs = c(descs, paste0(comp_idx,":", man_rhs))
          }
        } else {
          aes_comp = c(aes_comp, paste0(comp_idx, "=", ui[[sel_idx]]))
          # updating description
          descs = c(descs, paste0(comp_idx,":", ui[[sel_idx]]))
        }
      }
    }

    aes_chunk = NULL
    man_chunk = NULL
    # Constructing the geom_X function call
    if(length(aes_comp) > 0){
      aes_chunk = paste0("aes(", paste0(aes_comp, collapse=", "),")")
    }
    if(length(man_comp) > 0){
      man_chunk = paste0(man_comp, collapse=", ")
    }
    cmd = paste0(fg_object_name , " = ", fg_object_name, " + ",
                 fcn,"(",
                paste0(c(aes_chunk, man_chunk), collapse=", "),
                ")")

    # creating the description
    desc = paste(descs, collapse = ", ")
  } else if(element == "facet"){

    # JMH add support for multi-figure facet
    # ggforce::facet_wrap_paginate
    # ggforce::facet_grid_paginate

    # We want to make sure at least one column has been selected
    if(ui[["select_component_facet"]][1] == ""){
      isgood = FALSE
      msgs = c(msgs, state[["MC"]][["labels"]][["msg_bad_facet"]])

    } else {
      # The faceting command will depend on the number of columns selected
      if(length(ui[["select_component_facet"]]) == 2){
        cmd = paste0(
                     fg_object_name , " = ", fg_object_name, " + ",
                     "facet_grid(",
                     ui[["select_component_facet"]][1],
                     "~",
                     ui[["select_component_facet"]][2], ")")
      } else{
        cmd = paste0(
                     fg_object_name , " = ", fg_object_name, " + ",
                     "facet_wrap(vars(",
                     paste0(ui[["select_component_facet"]], collapse=", ")
                     , "))")
      }

      desc = paste0(ui[["select_component_facet"]], collapse= ", ")

    }
  } else if(element == "label"){

    # We'll construct the indiviudal commands xlab(), ylab(), etc, and
    # Combine them in the end
    cmds = c()

    for(comp_cmd in element_cfg[["ui_text"]]){
      # This is the ui_id for the current component
      comp_ui_id = paste0("text_component_", comp_cmd)
      # If the ui for this component isn't empty we construct
      # the command for that component
      if(ui[[comp_ui_id]] != ""){
        cmds = c(cmds, paste0(comp_cmd, '("', ui[[comp_ui_id]], '")'))
        descs = c(descs, paste0(comp_cmd, ": ", ui[[comp_ui_id]]))
      }
    }

    # If this is null then someone tried to add labels but didn't specify any
    if(is.null(cmds)){
      isgood = FALSE
      msgs = c(msgs, state[["MC"]][["labels"]][["msg_bad_label"]])
    } else {
      cmd  = paste0(
                    fg_object_name , " = ", fg_object_name, " + ",
                    paste0(cmds, collapse = " + "))
      desc = paste0(descs, collapse= ", ")
    }
  }else{
    isgood = FALSE
    err_msg =
    err_msg = state[["MC"]][["labels"]][["msg_bad_element"]]
    err_msg = stringr::str_replace_all(err_msg, "===ELEMENT===",  element)
    msgs = c(msgs, err_msg)
  }


  res = list(isgood   = isgood,
             cmd      = cmd,
             element  = element,
             desc     = desc,
             msgs     = msgs)

res}


#'@export
#'@title Evaluates Figure Generation Generated Code
#'@description Takes the current state and a string containing a data
#'wranlging command and evaluates it.
#'@param state FG state from \code{FG_fetch_state()}
#'@param cmd String containing the plotting command.  Set to NULL to initialize a
#'new figure or force a rebuild after a dataset update.
#'@return list with the following elements
#'\itemize{
#'  \item{isgood:} Return status of the function.
#'  \item{msgs:}   Messages to be passed back to the user.
#'  \item{pages:}  List with each element containing a ggplot object (\code{p}) and the code to generate that object (\code{code})
#'}
FG_eval = function(state, cmd){

  # Pulling out the current figure
  current_fig = FG_fetch_current_fig(state)
  msgs        = c()

  # Defining the dataset locally:
  assign(state[["FG"]][["DSV"]][["dsviews"]][["object_name"]][[current_fig[["fig_dsview"]]]],
         state[["FG"]][["DSV"]][["dsviews"]][["contents"]][[current_fig[["fig_dsview"]]]])

  # Defining the figure object name locally:
  fg_object_name = state[["MC"]][["fg_object_name"]]

  # Number of pages to generate:
  num_pages = current_fig[["num_pages"]]

  # Each figure will be stored as separate pages here:
  pages = list()

  # This will be used to flag any failures in the loop below
  isgood = TRUE

  for(page_idx in c(1:num_pages)){
    if(isgood){
      # The page code is initalized with the code init:
      page_code = current_fig[["code_init"]]

      # Adding current elements of the figure:
      # JMH add later

      # Appending the code to be added
      if(!is.null(cmd)){
        page_code = c(page_code,
          state[["MC"]][["post_processing"]])
      }

      # Adding other options (e.g. log scale)
      # JMH add later

      # Adding the post processinng
      if(!is.null(state[["MC"]][["post_processing"]])){
        page_code = c(page_code,
          state[["MC"]][["post_processing"]])
      }


      # Replacing the ===PAGE=== placeholders with the current page:
      page_code = stringr::str_replace_all(page_code, "===PAGE===",  as.character(page_idx))

      # Trying to evaluate the generated command against p
      # to see if any errors are generated:
      tcres = tryCatch({
        eval(parse(text=page_code))
        p = get(fg_object_name)
        list(p = p, isgood=TRUE)},
        error = function(e) {
          list(error=e, isgood=FALSE)}
      )

      if(!tcres[["isgood"]]){
        msgs = c(msgs, state[["MC"]][["errors"]][["element_not_added"]])
        if(!is.null(tcres[["error"]][["message"]])){
          msgs = c(msgs, paste0("message: ", tcres[["error"]][["message"]])) }
        if(!is.null(tcres[["error"]][["call"]])){
          msgs = c(msgs, paste0("call:    ", tcres[["error"]][["call"]])) }
        isgood = FALSE
        p      = NULL
      } else {
        p = tcres[["p"]]
      }

      # Storing the current page:
      pages[[paste0("page_", page_idx)]] = list(
        p = p,
        code = page_code)
    }
  }

  res = list(isgood = isgood,
             msgs   = msgs,
             pages  = pages)

res}


#'@export
#'@title Sets State Message from Button Click
#'@description Any errors that need to be passed back to the user can be set
#'with this function.
#'@param state FG state from \code{FG_fetch_state()}
#'@param cmd Character vector of messages.
#'@return state with button message set
FG_set_button_click_msgs = function(state, msgs){

  if(is.null(msgs)){
    state[["FG"]][["button_click_msg"]] = NULL
  } else {
    state[["FG"]][["button_click_msg"]] = paste(msgs, collapse = "\n")
  }

state}
