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
      req(input$select_fg_element)
      input[["button_fig_new"]]
      input[["button_fig_save"]]
      input[["button_fig_del"]]
      input[["button_fig_copy"]]
      #input[["button_fig_upds"]]
      input[["button_element_add"]]
      input[["hot_fg_elements"]]
      input[["select_current_fig"]]

      state = FG_fetch_state(id           = id,
                             input        = input,
                             session      = session,
                             yaml_file    = yaml_file,
                             yaml_section = yaml_section,
                             id_UD        = id_UD,
                             id_DW        = id_DW,
                             react_state  = react_state)
      current_fig = FG_fetch_current_fig(state)

      if(is.null(current_fig[["elements_table"]])){
        df = data.frame("No_Elements"="# No figure elements defined yet!")
        hot= rhandsontable::rhandsontable(
          df,
          stretchH = "all",
          width  = state[["MC"]][["formatting"]][["fg_elements"]][["width"]],
          height = state[["MC"]][["formatting"]][["fg_elements"]][["height"]],
          rowHeaders = NULL
        )

        uiele =  hot
      } else {
        df = current_fig[["elements_table"]]
        df[["cmd"]] = NULL

        hot = rhandsontable::rhandsontable(
          df,
          stretchH = "all",
          width  = state[["MC"]][["formatting"]][["fg_elements"]][["width"]],
          height = state[["MC"]][["formatting"]][["fg_elements"]][["height"]],
          rowHeaders = NULL
        ) %>%
          hot_cols(renderer = "
               function (instance, td, row, col, prop, value, cellProperties) {
               Handsontable.renderers.TextRenderer.apply(this, arguments);

               if(value == 'Success') {
               td.style.background = 'lightgreen';
               } else if(value == 'Failure' ) {
               td.style.background = 'lightpink';
               } else if(value == 'Not Run') {
               td.style.background = 'lightorange'}

              return td;
               }") %>%
          hot_col(col = "Delete",
                  renderer = "
               function(instance, td, row, col, prop, value, cellProperties) {
                 Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
                 return td;
               }") %>%
          hot_col("Element" ,      readOnly = TRUE) %>%
          hot_col("Description" , readOnly = TRUE) %>%
          hot_col("Status" ,      readOnly = TRUE)

        uiele = hot
      }


      uiele})
    #------------------------------------
    output$ui_fg_preview_ggplot   = renderPlot({
      # Forcing reactions:
      input[["button_fig_new"]]
      input[["button_fig_save"]]
      input[["button_fig_del"]]
      input[["button_fig_copy"]]
      #input[["button_fig_upds"]]
      input[["hot_fg_elements"]]
      input[["button_element_add"]]
      input[["select_current_fig"]]

      state = FG_fetch_state(id           = id,
                             input        = input,
                             session      = session,
                             yaml_file    = yaml_file,
                             yaml_section = yaml_section,
                             id_UD        = id_UD,
                             id_DW        = id_DW,
                             react_state  = react_state)

      current_fig = FG_fetch_current_fig(state)
  #   # Figuring out the page to use
  #   if(state[["FG"]][["ui"]][["select_fig_page"]] == ""){
  #     # Default to the first page if the select_fig_page ui element hasn't
  #     # been populated. It wont populate if there is only one page.
  #     fig_page =  names(current_fig[["pages"]])[1]
  #   } else {
  #     fig_page = state[["FG"]][["ui"]][["select_fig_page"]]
  #   }

      uiele = current_fig[["fobj"]]

      uiele})
    #------------------------------------
    output$ui_fg_preview_plotly   = plotly::renderPlotly({
      # Forcing reactions:
      input[["button_fig_new"]]
      input[["button_fig_save"]]
      input[["button_fig_del"]]
      input[["button_fig_copy"]]
      #input[["button_fig_upds"]]
      input[["hot_fg_elements"]]
      input[["button_element_add"]]
      input[["select_current_fig"]]
      state = FG_fetch_state(id           = id,
                             input        = input,
                             session      = session,
                             yaml_file    = yaml_file,
                             yaml_section = yaml_section,
                             id_UD        = id_UD,
                             id_DW        = id_DW,
                             react_state  = react_state)

      current_fig = FG_fetch_current_fig(state)
    # # Figuring out the page to use
    # if(state[["FG"]][["ui"]][["select_fig_page"]] == ""){
    #   # Default to the first page if the select_fig_page ui element hasn't
    #   # been populated. It wont populate if there is only one page.
    #   fig_page =  names(current_fig[["pages"]])[1]
    # } else {
    #   fig_page = state[["FG"]][["ui"]][["select_fig_page"]]
    # }

      uiele = plotly::ggplotly(current_fig[["fobj"]])

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
    output$ui_fg_fig_name  = renderUI({
      #req(input$X)
      input[["button_fig_new"]]
      input[["button_fig_save"]]
      input[["button_fig_del"]]
      input[["button_fig_copy"]]
      #input[["button_fig_upds"]]
      input[["select_current_fig"]]
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
  # #------------------------------------
  # output$ui_fg_upds_fig   = renderUI({
  #   #req(input$X)
  #   state = FG_fetch_state(id           = id,
  #                          input        = input,
  #                          session      = session,
  #                          yaml_file    = yaml_file,
  #                          yaml_section = yaml_section,
  #                          id_UD        = id_UD,
  #                          id_DW        = id_DW,
  #                          react_state  = react_state)
  #   uiele = NULL
  #   if(state[["FG"]][["isgood"]]){
  #     uiele = actionBttn(
  #               inputId = NS(id, "button_fig_upds"),
  #               label   = state[["MC"]][["labels"]][["upds_fig"]],
  #               style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
  #               size    = state[["MC"]][["formatting"]][["button_fig_upds"]][["size"]],
  #               color   = "warning",
  #               icon    = icon("sync"))
  #   }
  #   uiele})
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
      input[["button_fig_new"]]
      input[["button_fig_save"]]
      input[["button_fig_del"]]
      input[["button_fig_copy"]]
      input[["button_fig_upds"]]
      input[["select_current_fig"]]
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


      uiele = 1
    # if(length(pages) > 1){
    #   uiele =
    #      pickerInput(
    #        inputId  = NS(id, "select_fig_page"),
    #        label    = NULL,
    #        width    = 200,
    #        choices  = pages
    #      # options  = list(
    #      #   title = state[["MC"]][["labels"]][["fds_mutate_column"]])
    #      )
    # }

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
    output$ui_fg_msg = renderText({
      req(input$select_fg_element)
      input[["button_fig_new"]]
      input[["button_fig_save"]]
      input[["button_fig_del"]]
      input[["button_fig_copy"]]
      #input[["button_fig_upds"]]
      input[["button_element_add"]]
      state = FG_fetch_state(id           = id,
                             input        = input,
                             session      = session,
                             yaml_file    = yaml_file,
                             yaml_section = yaml_section,
                             id_UD        = id_UD,
                             id_DW        = id_DW,
                             react_state  = react_state)

      uiele = state[["FG"]][["ui_msg"]]

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
      input[["button_fig_new"]]
      input[["button_fig_save"]]
      input[["button_fig_del"]]
      input[["button_fig_copy"]]
      input[["button_fig_upds"]]
      input[["button_element_add"]]
      input[["hot_fg_elements"]]
      input[["select_current_fig"]]

      state = FG_fetch_state(id           = id,
                             input        = input,
                             session      = session,
                             yaml_file    = yaml_file,
                             yaml_section = yaml_section,
                             id_UD        = id_UD,
                             id_DW        = id_DW,
                             react_state  = react_state)

      uiele = NULL
      current_fig = FG_fetch_current_fig(state)

      if(is.null(current_fig[["elements_table"]])){
        uiele = "# No figure elements defined yet!"
      } else {
        uiele = current_fig[["code"]]
      }

      shinyAce::updateAceEditor(
        session         = session,
        editorId        = "ui_fg_code",
        theme           = state[["yaml"]][["FM"]][["code"]][["theme"]],
        showLineNumbers = state[["yaml"]][["FM"]][["code"]][["showLineNumbers"]],
        readOnly        = state[["MC"]][["code"]][["readOnly"]],
        mode            = state[["MC"]][["code"]][["mode"]],
        value           = uiele)

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
             #input$button_fig_upds,
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

        FM_le(state, "reaction state updated")
        react_state[[id]] = state
      })
    }

    # Removing holds
    observeEvent(input$select_current_fig, {
      # Once that's generated we need to remove any holds on this UI element
        state = FG_fetch_state(
                             id           = id,
                             input        = input,
                             session      = session,
                             yaml_file    = yaml_file,
                             yaml_section = yaml_section,
                             id_UD        = id_UD,
                             id_DW        = id_DW,
                             react_state  = react_state)
      remove_hold(state, session, "select_current_fig")
    }, priority = 100)

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
#'values from the yaml file as well as any changes made by the user. The
#'structure ofthe list is defined below:
#'\itemize{
#'  \item{yaml:} Contents of the yaml file.
#'  \item{MC:} Section of the yaml file, specified by \code{yaml_section}, containing the FG module components
#'  \item{FG:} Data wrangling state
#'  \itemize{
#'    \item{isgood:} Boolean status of the state. Currently just TRUE
#'    \item{button_counters:}  List of counters to detect button clicks.
#'    \item{ui_msg:}           Message returned when users perform actions.
#'    \item{ui:}               Current value of form elements in the UI.
#'    \item{ui_ids:}           Vector of UI elements for the module.
#'    \item{ui_hold:}          List of hold elements to disable updates before a full ui referesh is complete.
#'    \item{aes_elements:}     JMH
#'    \item{current_fig:}      JMH
#'    \item{fig_cntr:}         JMH
#'    \item{DSV:}              JMH
#'    \item{figs:}             List of figures. Each view has the following structure:
#'      \itemize{
#'        \item{isgood:} Boolean status of the figure. False if evaluation fails.
#'        \item{key:     Figure key (user editable)}
#'        \item{caption: Figure caption  (user editable)}
#'        \item{id: Character id (\code{fig_idx})}
#'        \item{idx: Numeric id (\code{1})}
#'        \item{fig_dsview:  Name of the dataset view for the current figure.}
#'        \item{UD_checksum: checksum of the UD state when the figure was created.}
#'        \item{DW_checksum: checksum of DW module if data view was used.}
#'        \item{DSV_checksum:checksum of the dataset view that was used to create the figure }
#'        \item{elements_table: Table of figure generation elements.}
#'        \item{code:          Code to generate figure from start to finish.}
#'        \item{code_previous: Code to load and/or wrangle the dataset.}
#'        \item{code_fg_only:  Code to just generate the figure.}
#'      }
#'  }
#'  \item{MOD_TYPE:} Character data containing the type of module \code{"DW"}
#'  \item{SESSION_LOCATION:} Character data containing the location of this
#'  module in the session variable.
#'}
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

  # Deleting any elements flagged by the user:
  if(is.list(state[["FG"]][["ui"]][["hot_fg_elements"]])){
    hot_df = rhandsontable::hot_to_r(state[["FG"]][["ui"]][["hot_fg_elements"]])
    if("Delete" %in% names(hot_df)){
      # This checks to see if anything has been selected for deletion
      if(any(hot_df$Delete == TRUE)){
        # Now we pull out the current figure:
        current_fig = FG_fetch_current_fig(state)
        # We compare the number of rows in each. This prevents multiple
        # deletions :)
        if(!is.null(current_fig[["elements_table"]])){
          if(nrow(current_fig[["elements_table"]]) == nrow(hot_df)){
            # Flagging that row for removal and forcing a rebuild of the figure:
            del_row = which(hot_df$Delete == TRUE)
            state = FG_build( state=state, del_row = del_row, cmd = NULL)
          }
        }
      }
    }
  }


  # Detecting figure selection changes
  if(has_changed(ui_val   = state[["FG"]][["ui"]][["select_current_fig"]],
                 old_val  = state[["FG"]][["current_fig"]]) &
      (!state[["FG"]][["ui_hold"]][["select_current_fig"]]) ){

    # Changing the current view to the one selected in the UI
    state[["FG"]][["current_fig"]]  =  state[["FG"]][["ui"]][["select_current_fig"]]
  }
  # Adding a new element
  if(has_changed(ui_val   = state[["FG"]][["ui"]][["button_element_add"]],
                 old_val  = state[["FG"]][["button_counters"]][["add"]])){

    FM_le(state, "adding figure element")
    msgs = c()

    # Building the plot element command
    fgb_res = fers_builder(state)

    # saving the messages
    msgs = c(msgs,  fgb_res[["msgs"]])


    # If figure generation command was successfully built we
    # evaluate this element to make sure it works correctly
    if( fgb_res[["isgood"]]){
      # Evaluating the element
      state = FG_build( state,
        cmd     = fgb_res[["cmd"]],
        element = fgb_res[["element"]],
        desc    = fgb_res[["desc"]])
    }

    # Saving the button state to the counter
    state[["FG"]][["button_counters"]][["add"]] =
      state[["FG"]][["ui"]][["button_element_add"]]

    # pulling out the current figure to extract any messages
    # generated from the build process
    current_fig = FG_fetch_current_fig(state)
    msgs = c(msgs, current_fig[["msgs"]])

    # Updating any messages
    state = FG_set_ui_msg(state, msgs)
  }
  # New figure
  if(has_changed(ui_val   = state[["FG"]][["ui"]][["button_fig_new"]],
                 old_val  = state[["FG"]][["button_counters"]][["new"]])){

    FM_le(state, "creating new figure")
    msgs = c()

    # Creating a new figure
    state = FG_new_fig(state, id_UD, id_DW, react_state)

    # Setting hold for figure select
    state[["FG"]][["ui_hold"]][["select_current_fig"]] = TRUE

    # Saving the button state to the counter
    state[["FG"]][["button_counters"]][["new"]] =
      state[["FG"]][["ui"]][["button_fig_new"]]

    # Updating any messages
    state = FG_set_ui_msg(state, msgs)
  }
  # Delete figure
  if(has_changed(ui_val   = state[["FG"]][["ui"]][["button_fig_del"]],
                 old_val  = state[["FG"]][["button_counters"]][["del"]])){

    FM_le(state, "deleting figure")
    msgs = c()

    # Getting the current figure
    current_fig = FG_fetch_current_fig(state)

    # Deleting the figure
    state[["FG"]][["figs"]][[current_fig[["id"]]]] = NULL

    # If there are no figures left then we create an empty one
    if( length(state[["FG"]][["figs"]])  == 0){
      state = FG_new_fig(state, id_UD, id_DW, react_state)
    } else {
      # If there are figures then we set the first one as active
      state[["FG"]][["current_fig"]] = names(state[["FG"]][["figs"]])[1]
    }

    # Setting hold for figure select
    state[["FG"]][["ui_hold"]][["select_current_fig"]] = TRUE

    # Saving the button state to the counter
    state[["FG"]][["button_counters"]][["del"]] =
      state[["FG"]][["ui"]][["button_fig_del"]]

    # Updating any messages
    state = FG_set_ui_msg(state, msgs)
  }
  # Save figure
  if(has_changed(ui_val   = state[["FG"]][["ui"]][["button_fig_save"]],
                 old_val  = state[["FG"]][["button_counters"]][["save"]])){

    FM_le(state, "saving changes to current figure")
    msgs = c()

    # Getting the current figure
    current_fig = FG_fetch_current_fig(state)

    if(state[["FG"]][["ui"]][["text_fig_key"]] != ""){
      # Resetting the key
      current_fig[["key"]] = state[["FG"]][["ui"]][["text_fig_key"]]
    } else {
      # returning an error
      msgs = c(msgs,
          state[["MC"]][["errors"]][["current_key_empty"]])
    }

    # Saving the caption as well
    current_fig[["caption"]] = state[["FG"]][["ui"]][["text_fig_cap"]]

    # Saving changes to the current figure
    state = FG_set_current_fig(state, current_fig)

    # Saving the button state to the counter
    state[["FG"]][["button_counters"]][["save"]] =
      state[["FG"]][["ui"]][["button_fig_save"]]

    # Updating any messages
    state = FG_set_ui_msg(state, msgs)
  }
  # Copy figure
  if(has_changed(ui_val   = state[["FG"]][["ui"]][["button_fig_copy"]],
                 old_val  = state[["FG"]][["button_counters"]][["copy"]])){

    FM_le(state, "copying figure")
    msgs = c()

    # Getting the original figure that is being copied:
    old_fig = FG_fetch_current_fig(state)

    # This creates a new figure and makes it active:
    state = FG_new_fig(state, id_UD, id_DW, react_state)

    # Now we pull out the new figure:
    new_fig = FG_fetch_current_fig(state)

    # Changing object references
    # Each figure has a unique object that is generated (e.g. the first figure
    # will have something like FG_myFG_1, the second one will have FG_myFG_2,
    # etc). When we copy an old figure to a new one, we need those object
    # references to change to the new one as well:
    if(!is.null(old_fig[["elements_table"]])){
      old_fig[["elements_table"]]  =
        dplyr::mutate( old_fig[["elements_table"]],
          cmd = str_replace_all(
            cmd,
            paste0("\\b", old_fig[["fg_object_name"]], "\\b"),
            new_fig[["fg_object_name"]]))
    }


    #From the original figure we copy several fields:
    new_fig[["fig_dsview"    ]]  = old_fig[["fig_dsview"    ]]
    new_fig[["UD_checksum"   ]]  = old_fig[["UD_checksum"   ]]
    new_fig[["DW_checksum"   ]]  = old_fig[["DW_checksum"   ]]
    new_fig[["DSV_checksum"  ]]  = old_fig[["DSV_checksum"  ]]
    new_fig[["elements_table"]]  = old_fig[["elements_table"]]
    new_fig[["code"          ]]  = old_fig[["code"          ]]
    new_fig[["fobj"          ]]  = old_fig[["fobj"          ]]
    new_fig[["code_previous" ]]  = old_fig[["code_previous" ]]
    new_fig[["code_fg_only"  ]]  = old_fig[["code_fg_only"  ]]
    new_fig[["caption"       ]]  = old_fig[["caption"       ]]

    # Now we dump the new figure with the old components
    # copied into it back into the state object:
    state = FG_set_current_fig(state, new_fig)

    # Setting hold for figure select
    state[["FG"]][["ui_hold"]][["select_current_fig"]] = TRUE

    # Saving the button state to the counter
    state[["FG"]][["button_counters"]][["copy"]] =
      state[["FG"]][["ui"]][["button_fig_copy"]]

    # Updating any messages
    state = FG_set_ui_msg(state, msgs)
  }
  # Update dataset for figure
# if(has_changed(ui_val   = state[["FG"]][["ui"]][["button_fig_upds"]],
#                old_val  = state[["FG"]][["button_counters"]][["upds"]])){
#
#   FM_le(state, "updating dataset")
#   msgs = c()
#   # Saving the button state to the counter
#   state[["FG"]][["button_counters"]][["upds"]] =
#     state[["FG"]][["ui"]][["button_fig_upds"]]
#
#   # Updating any messages
#   state = FG_set_ui_msg(state, msgs)
# }

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
    # "button_fig_upds",
      "button_element_add",
      "hot_fg_elements",
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
      ui_hold          = list(           # List of states to hold to prevent updates/refresh until after ui has been rebuilt with the curretn state
       select_current_fig = FALSE  ),
      ui_ids           = ui_ids,         # List of the possible ui_ids in the model
      DSV              = DSV             # List containing the dataset views from FM_fetch_dsviews()
      )

  state[["FG"]] = FG_NULL

  if(isgood){
    # Initializing an empty figure
    state = FG_new_fig(state, id_UD, id_DW, react_state)
  }

  state[["MOD_TYPE"]] = "FG"

  FM_le(state, "State initialized")

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
  fg_object_name = paste0("FG_", state[["MC"]][["fg_object_name"]], "_", state[["FG"]][["fig_cntr"]])
  code_init = paste0(fg_object_name, " = ggplot2::ggplot(data=", ds_object_name,")")

  # This is the object that contains the different components of
  # the figure list:
  fig_def =
    list(key            = fig_id,
         id             = fig_id,
         idx            = state[["FG"]][["fig_cntr"]],
         num_pages      = 1,
         fg_object_name = fg_object_name,
         fobj           = NULL,
         msgs           = c(),
         fig_dsview     = fig_dsview,
         UD_checksum    = DSV[["UD_checksum"]],
         DW_checksum    = DSV[["DW_checksum"]],
         DSV_checksum   = DSV[["dsviews"]][["checksum"]][[fig_dsview]],
         code_init      = code_init,
         code_fg_only   = NULL,
         code_previous  = NULL,
         code           = NULL,
         caption        = "",
         isgood         = TRUE,
         add_isgood     = TRUE,
         elements_table = NULL)

  # Setting the new figure id as the current figure
  state[["FG"]][["current_fig"]]    = fig_id

  # Storing the empty figure object in the state
  state = FG_set_current_fig(state, fig_def)

  # Creating the new figure pages
  state = FG_build(state, NULL)

state}

#'@export
#'@title Fetches Current Figure
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
#'@title Sets Current Figure
#'@description Takes an FG state and a figure list and sets that figure list
#'as the value for the active figure
#'@param state FG state from \code{FG_fetch_state()}
#'@param fig Figure list from \code{FG_fetch_current_fig}
#'@return State with the current figure updated
FG_set_current_fig    = function(state, fig){

  # Current figure ID
  fig_id = state[["FG"]][["current_fig"]]

  # Current figure
  state[["FG"]][["figs"]][[fig_id]] = fig

state}



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

  # Pulling out the current figure to get the object name
  current_fig = FG_fetch_current_fig(state)
  fg_object_name = current_fig[["fg_object_name"]]

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
#'@title Evaluates Figure Generation Code
#'@description Takes the current state and rebuilds the active figure. If the
#'elements table has a row flagged for deletion, it will be deleted. If the
#'cmd input is not NULL it will attempt to append that element to the figure.
#'@param state FG state from \code{FG_fetch_state()}
#'@param del_row Row number to be deleted (NULL if no rows need to be deleted)
#'@param cmd String containing the plotting command.  Set to NULL to initialize a
#'new figure or force a rebuild after a dataset update.
#'@param Action Short name for the action being performed, eg. point
#'@param Description Verbose description for the action being performed
#'@return list with the following elements
#'\itemize{
#'  \item{isgood:} Return status of the function.
#'  \item{msgs:}   Messages to be passed back to the user.
#'  \item{pages:}  List with each element containing a ggplot object (\code{p}) and the code to generate that object (\code{code})
#'}
FG_build = function(state,
                   del_row     = NULL,
                   cmd         = NULL,
                   element     = "unknown",
                   desc        = "unknown"){

  # Pulling out the current figure
  current_fig = FG_fetch_current_fig(state)
  msgs        = c()

  # Defining the dataset locally:
  assign(state[["FG"]][["DSV"]][["dsviews"]][["object_name"]][[current_fig[["fig_dsview"]]]],
         state[["FG"]][["DSV"]][["dsviews"]][["contents"]][[current_fig[["fig_dsview"]]]])

  # Defining the figure object name locally:
  fg_object_name = current_fig[["fg_object_name"]]

  # Initializing the figure object
  assign(fg_object_name, NULL)

  # These will be used to flag any failures below:
  isgood     = TRUE
  add_isgood = TRUE

  # The figure code is initalized with the code init:
  code_lines = current_fig[["code_init"]]

  # This is the elements table
  curr_ET = current_fig[["elements_table"]]

  # Here we process row deletions
  if(!is.null(curr_ET) & !is.null(del_row)){
    # Removing the specified row from the elements table:
    curr_ET = curr_ET[-c(del_row), ]

    # If there was only one row and we deleted it we need
    # to set the elements table to NULL
    if(nrow(curr_ET) ==0){
      curr_ET = NULL
    }
  }

  if(!is.null(cmd)){
    if(!is.null(curr_ET)){
      if(element == "facet" & any(curr_ET[["Element"]] == "facet")){
        add_isgood = FALSE
        msgs = c(msgs, state[["MC"]][["errors"]][["only_one_facet"]])
      }
    }
  }

  if(isgood){

    # First we create the code to initialize the
    # figure
    eval(parse(text=current_fig[["code_init"]]))

    # Process current elements
    if(!is.null(curr_ET)){
      for(row_idx in 1:nrow(curr_ET)){
        if(isgood){
          tc_env  = list()
          tc_env[[fg_object_name]] = get(fg_object_name)
          tcres = FM_tc(
             cmd     = curr_ET[row_idx, ]$cmd,
             tc_env  = tc_env,
             capture = c(fg_object_name))


          if(tcres[["isgood"]]){
            # If the try catch was successful we extract the updated plot object
            assign(fg_object_name, tcres[["capture"]][[fg_object_name]])

            # Mark the row as success
            curr_ET[row_idx, ][["Status"]] = "Success"

            # Saving the command for the code block
            code_lines = c(code_lines,  curr_ET[row_idx, ]$cmd)

          } else {
            # Otherwise we set the figure to a failed state:
            isgood = FALSE
            # Then we mark this row as failure
            curr_ET[row_idx, ][["Status"]] = "Failure"
            # Append any messages as well
            msgs = c(msgs, tcres[["msgs"]])
          }
        }  else {
          # If we ran into evaluation issues above we
          # mark the others as Not Run
          curr_ET[row_idx, ][["Status"]] = "Not Run"
        }
      }
    }

    # Adding any new elements
    if(add_isgood){
      if(!is.null(cmd)){
        # Defining the environment for the try/catch
        tc_env  = list()
        tc_env[[fg_object_name]] = get(fg_object_name)
        tcres = FM_tc(
           cmd     = cmd,
           tc_env  = tc_env,
           capture = c(fg_object_name))

        if(tcres[["isgood"]]){
          # If the try catch was successful we extract the updated plot object
          assign(fg_object_name, tcres[["capture"]][[fg_object_name]])
          # Then we add the new row to the event table
          curr_ET = rbind(curr_ET,
                data.frame(Element     = element,
                           cmd         = cmd,
                           Description = desc,
                           Status      = "Success",
                           Delete      = FALSE))
          # Saveing the command for the code block
          code_lines = c(code_lines, cmd)
        } else {
          # Otherwise we mark the add_isgood as false
          add_isgood = FALSE
          # Append any messages as well
          msgs = c(msgs, tcres[["msgs"]])
        }
      }
    }

    # If there is no elements table and the plot command is null then we don't
    # have a figure to generate yet so we just add a message to the user:
    if(is.null(curr_ET) & is.null(cmd)) {
      assign(fg_object_name,
             FM_mk_error_fig(state[["MC"]][["labels"]][["no_fig_elements"]]))
    }



    # Lastly we apply any post processing
    if(!is.null(state[["MC"]][["post_processing"]])){
      # Pulling out the post processing code:
      ppstr = state[["MC"]][["post_processing"]]
      # Replacing figure object placeholders with the correct figure object
      ppstr = stringr::str_replace_all(
        string=ppstr,
        pattern = "===FGOBJ===",
        replacement = fg_object_name)
      # Running the post processing:
      eval(parse(text=ppstr))

      # Saving the code
      code_lines = c(code_lines, ppstr)
    }


    # Now we force a build of the figure to capture errors that only occur
    # when a build has been forced:
    tc_env  = list()
    tc_env[[fg_object_name]] = get(fg_object_name)
    ggb_cmd = paste0("ggb_res = ggplot2::ggplot_build(", fg_object_name, ")")
    tcres = FM_tc(
       cmd     = ggb_cmd,
       tc_env  = tc_env,
       capture = c("ggb_res"))
    if(!tcres[["isgood"]]){
      if(is.null(cmd)){

        # If cmd is null then we're just processing the figure like normal:
        isgood = FALSE
      } else {
        # Otherwise we're trying to add an element to it
        add_isgood = FALSE
      }
      # Appending the messages:
      msgs = c(msgs, tcres[["msgs"]])

    }

  }

  # By default there is one page. The only way to have more than one is if
  # faceting has been chosen. In that case one of the ggforce pageinate
  # functions would have been used. So we go through a series of checks to
  # see if that's true. Finally If one of the figure Elements is facet then
  # we see if there is more than one page.
  num_pages = 1
  if( is.ggplot(get(fg_object_name))){
    if(!is.null(curr_ET)){
      if(any("facet" %in% curr_ET[["Element"]])){
        num_pages = ggforce::n_pages(get(fg_object_name))
        if(is.null(num_pages)){
          num_pages = 1
        }
      }
    }
  }

  # Code for the modules feeding into this one
  code_previous   = state[["FG"]][["DSV"]][["dsviews"]][["code"]][[current_fig[["fig_dsview"]]]]
  # Just the code to build the figure
  code_fg_only    = paste(code_lines, collapse="\n")
  # All the code required to generate this module
  code            = paste(c(code_previous, code_fg_only), collapse="\n")

  # Updating figure with the components above
  current_fig[["num_pages"]]        = num_pages
  current_fig[["msgs"]]             = msgs
  current_fig[["code_previous"]]    = code_previous
  current_fig[["code_fg_only"]]     = code_fg_only
  current_fig[["code"]]             = code
  current_fig[["isgood"]]           = isgood
  current_fig[["add_isgood"]]       = add_isgood
  current_fig[["fobj"]]             = get(fg_object_name)
  current_fig[["elements_table"]]   = curr_ET

  # updating the current figure with the changes above
  state = FG_set_current_fig(state, current_fig)

state}


#'@export
#'@title Sets State Message from Button Click
#'@description Any errors that need to be passed back to the user can be set
#'with this function.
#'@param state FG state from \code{FG_fetch_state()}
#'@param cmd Character vector of messages.
#'@return state with button message set
FG_set_ui_msg = function(state, msgs){

  if(is.null(msgs)){
    state[["FG"]][["ui_msg"]] = NULL
  } else {
    state[["FG"]][["ui_msg"]] = paste(msgs, collapse = "\n")
  }

state}
