#'@import dplyr
#'@import rhandsontable
#'@import shiny
#'@import shinyWidgets
#'@importFrom digest digest
#'@importFrom readr read_csv
#'@importFrom rlang .data
#'@importFrom shinyAce aceEditor updateAceEditor
#'@importFrom yaml read_yaml

# https://getbootstrap.com/docs/3.3/components/

#'@export
#'@title Data Wrangling Server
#'@description Server function for the data wrangling module
#'@param id An ID string that corresponds with the ID used to call the modules UI elements
#'@param id_ASM ID string for the app state managment module used to save and load app states
#'@param id_UD  ID string for the upload data module used to handle uploads or
#'the name of the list element in react_state where the data set is stored.
#'@param FM_yaml_file App configuration file with FM as main section.
#'@param MOD_yaml_file  Module configuration file with MC as main section.
#'@param react_state Variable passed to server to allow reaction outside of
#'module (\code{NULL})
#'@return DW Server object
DW_Server <- function(id,
                      id_ASM       = "ASM",
                      id_UD        = "UD",
                      FM_yaml_file  = system.file(package = "formods",
                                                  "templates",
                                                  "formods.yaml"),
                      MOD_yaml_file = system.file(package = "formods",
                                                  "templates",
                                                  "DW.yaml"),
                      react_state  = NULL) {
  moduleServer(id, function(input, output, session) {

    #------------------------------------
    # Current DW elements
    output$hot_dw_elements = rhandsontable::renderRHandsontable({
      req(input$select_dw_element)
      # Force update on button click
      input$button_dw_add_element
      input$button_dw_new
      input$button_dw_del
      input$button_dw_copy
      # input$button_dw_save
      # Force update on deletion clicks
      input$hot_dw_elements
      # Changes in view selected
      input$select_dw_views
      # Forcing a reaction to changes in other modules
      react_state[[id_UD]]
      react_state[[id_ASM]]


      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_UD           = id_UD,
                             react_state     = react_state)

      # Pulling out the current view
      current_view = DW_fetch_current_view(state)

      uiele = NULL

      if(state[["DW"]][["UD"]][["isgood"]]){
        if(is.null(current_view[["elements_table"]])){
          df = data.frame("No_Data"= paste0("# ", state[["MC"]][["labels"]][["no_dw_elements"]]))

          hot= rhandsontable::rhandsontable(
            df,
            stretchH = "all",
            width  = state[["MC"]][["formatting"]][["dw_elements"]][["width"]],
            height = state[["MC"]][["formatting"]][["dw_elements"]][["height"]],
            rowHeaders = NULL
          )

          uiele =  hot

        } else {
          df = current_view[["elements_table"]]
          df[["cmd"]] = NULL

          hot = rhandsontable::rhandsontable(
            df,
            stretchH = "all",
            width  = state[["MC"]][["formatting"]][["dw_elements"]][["width"]],
            height = state[["MC"]][["formatting"]][["dw_elements"]][["height"]],
            rowHeaders = NULL
          ) |>
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
                 }") |>
            hot_col(col = "Delete",
                    renderer = "
                 function(instance, td, row, col, prop, value, cellProperties) {
                   Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
                   return td;
                 }") |>
            hot_col("Action" ,      readOnly = TRUE) |>
            hot_col("Description" , readOnly = TRUE) |>
            hot_col("Status" ,      readOnly = TRUE)

          uiele = hot
        }
      }

      uiele

    })
    #------------------------------------
    # Generated data wrangling code
    observe({
      # Forcing a reaction to changes in other modules
      react_state[[id_UD]]
      react_state[[id_ASM]]
      # Force update on button click
      input$button_dw_add_element
      input$button_dw_new
      input$button_dw_del
      input$button_dw_copy
      # input$button_dw_save
      # Force update on deletion clicks
      input$hot_dw_elements
      # Changes in view selected
      input$select_dw_views

      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_UD           = id_UD,
                             react_state     = react_state)

      uiele = NULL

      if(state[["DW"]][["UD"]][["isgood"]]){

        # Pulling out the current view
        current_view = DW_fetch_current_view(state)

        if(is.null(current_view[["elements_table"]])){
          uiele = "# No data wragling elements defined yet!"
        } else {
          uiele = current_view[["code"]]
        }

        shinyAce::updateAceEditor(
          session         = session,
          editorId        = "ui_dw_code",
          theme           = state[["yaml"]][["FM"]][["code"]][["theme"]],
          showLineNumbers = state[["yaml"]][["FM"]][["code"]][["showLineNumbers"]],
          readOnly        = state[["MC"]][["code"]][["readOnly"]],
          mode            = state[["MC"]][["code"]][["mode"]],
          value           = uiele)
      }

    })
    #------------------------------------
    # Selection the change the current view
    output$ui_dw_views = renderUI({
      input$button_dw_add_element
      input$button_dw_new
      input$button_dw_del
      input$button_dw_copy
      input$button_dw_save
      # Forcing a reaction to changes in other modules
      react_state[[id_UD]]
      react_state[[id_ASM]]

      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_UD           = id_UD,
                             react_state     = react_state)

      view_ids = names(state[["DW"]][["views"]])
      uiele = NULL

      if(length(view_ids)> 0){

        # Building up the vew choices
        current_view = DW_fetch_current_view(state)
        choices = c()
        cnames  = c()
        for(view_id in view_ids){
          cnames  = c(cnames, state[["DW"]][["views"]][[view_id]][["key"]])
          choices = c(choices, view_id)
        }

        names(choices) = cnames

        uiele = tagList(uiele,
          shinyWidgets::pickerInput(
             inputId    = NS(id, "select_dw_views"),
             selected   = current_view[["id"]],
             choices    = choices,
             width      = "fit",
             inline     = TRUE,
             choicesOpt = NULL))
      }


    uiele})

    #------------------------------------
    # Sele
    output$ui_dw_key = renderUI({
      input$button_dw_new
      input$button_dw_del
      input$button_dw_copy
      input$button_dw_save
      input$select_dw_views
      # Forcing a reaction to changes in other modules
      react_state[[id_UD]]
      react_state[[id_ASM]]

      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_UD           = id_UD,
                             react_state     = react_state)

    current_view = DW_fetch_current_view(state)
      uiele =
      textInput(
        inputId     = NS(id, "current_key"),
        label       = NULL,
        width       = state[["MC"]][["formatting"]][["current_key"]][["width"]] ,
        value       = current_view[["key"]],
        placeholder = state[["MC"]][["labels"]][["current_key"]]
      )
    uiele})
    #------------------------------------
    output$ui_dw_msg = renderText({
      # Force update on button click
      input$button_dw_add_element
      input$button_dw_new
      input$button_dw_del
      input$button_dw_copy
      # input$button_dw_save
      # Update when they delete elements as well
      input$hot_dw_elements
      #req(input$select_dw_element)
      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_UD           = id_UD,
                             react_state     = react_state)
      uiele = NULL
      if(state[["DW"]][["UD"]][["isgood"]]){
        uiele = state[["DW"]][["ui_msg"]]
      }
      uiele})
    #------------------------------------
    # Row for new DW elements
    output$ui_dw_new_element_row = renderUI({
      #input$button_dw_add_element
      req(input$select_dw_element)
      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_UD           = id_UD,
                             react_state     = react_state)

      if(state[["DW"]][["UD"]][["isgood"]]){
        if(state[["DW"]][["isgood"]]){
          # Current final dataset
          WDS = DW_fetch_current_view(state)[["WDS"]]
          # Columns in that dataset
          dscols = names(WDS)
          #browser()

          # Constructing the uiele based on the users selection
          if(state[["DW"]][["ui"]][["select_dw_element"]] == "filter"){
            uiele = tagList(
              div(style = "display: flex;",
                  htmlOutput(NS(id, "ui_dw_fds_filter_column_select"  )),
                  htmlOutput(NS(id, "ui_dw_fds_filter_operator_select")),
                  htmlOutput(NS(id, "ui_dw_fds_filter_rhs"))))


          }
          if(state[["DW"]][["ui"]][["select_dw_element"]] == "mutate"){
            uiele = tagList(htmlOutput(NS(id, "ui_dw_fds_mutate_row")))
          }
          if(state[["DW"]][["ui"]][["select_dw_element"]] == "rename"){
            uiele = tagList(
              htmlOutput(NS(id, "ui_dw_fds_rename"  )))
          }
          if(state[["DW"]][["ui"]][["select_dw_element"]] == "group" ){
            uiele = tagList(
              htmlOutput(NS(id, "ui_dw_fds_group_column_select"  )))
          }
          if(state[["DW"]][["ui"]][["select_dw_element"]] == "ungroup" ){
            uiele = tagList(
              htmlOutput(NS(id, "ui_dw_fds_ungroup"  )))
          }
          if(state[["DW"]][["ui"]][["select_dw_element"]] == "onerow" ){
            uiele = tagList(
              htmlOutput(NS(id, "ui_dw_fds_onerow"  )))
          }
          if(state[["DW"]][["ui"]][["select_dw_element"]] == "select" ){
            uiele = tagList(
              htmlOutput(NS(id, "ui_dw_fds_select"  )))
          }
          if(state[["DW"]][["ui"]][["select_dw_element"]] == "longer" ){
            uiele = tagList(
              htmlOutput(NS(id, "ui_dw_fds_longer"  )))
          }
          if(state[["DW"]][["ui"]][["select_dw_element"]] == "wider" ){
            uiele = tagList(
              htmlOutput(NS(id, "ui_dw_fds_wider"  )))
          }

          # this makes the inputs into a line
          #uiele =   div(style = "display: flex;", uiele)
        }
      } else {
        uiele = NULL
      }

      uiele})
    #------------------------------------
    # mutate chunks
    output$ui_dw_fds_mutate_row = renderUI({
      req(input$select_dw_element)
      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_UD           = id_UD,
                             react_state     = react_state)

      req(input$select_dw_element)
      uiele = NULL
      if(state[["DW"]][["isgood"]]){
        # Current final dataset
        WDS = DW_fetch_current_view(state)[["WDS"]]
        # Columns in that dataset
        dscols = names(WDS)
        uiele = tagList(
          div(style = "display: flex;",
              textInput(
                inputId     = NS(id, "select_fds_mutate_column"),
                label       = NULL,
                width       = 200,
                placeholder = state[["MC"]][["labels"]][["fds_mutate_column"]]
              ),
              textInput(
                inputId     = NS(id, "select_fds_mutate_rhs"),
                label       = NULL,
                width       = 400,
                placeholder = state[["MC"]][["labels"]][["fds_mutate_rhs"]]
              )
          )
        )

      }
      uiele
    })
    #------------------------------------
    # rename chunks
    output$ui_dw_fds_rename = renderUI({
      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_UD           = id_UD,
                             react_state     = react_state)

      req(input$select_dw_element)
      uiele = NULL
      if(state[["DW"]][["isgood"]]){
        # Current final dataset
        WDS = DW_fetch_current_view(state)[["WDS"]]
        # Columns in that dataset
        dscols = names(WDS)
        uiele = tagList(
          div(style = "display: flex;",
              pickerInput(
                inputId  = NS(id, "select_fds_rename_column"),
                label    = NULL,
                width    = 200,
                choices  = dscols,
                options  = list(
                  title = state[["MC"]][["labels"]][["fds_rename_column"]])
              ),
              textInput(
                inputId     = NS(id, "fds_rename_rhs"),
                label       = NULL,
                width       = 200,
                placeholder = state[["MC"]][["labels"]][["fds_rename_rhs"]]
              )
          ))
      }
      uiele
    })
    #------------------------------------
    output$ui_dw_fds_group_column_select = renderUI({
      req(input$select_dw_element)
      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_UD           = id_UD,
                             react_state     = react_state)

      req(input$select_dw_element)
      uiele = NULL
      if(state[["DW"]][["isgood"]]){
        # Current final dataset
        WDS = DW_fetch_current_view(state)[["WDS"]]
        # Columns in that dataset
        dscols = names(WDS)
        uiele = tagList(
          div(style = "display: flex;",
              pickerInput(
                inputId  = NS(id, "select_fds_group_column"),
                label    = NULL,
                width    = 200,
                choices  = dscols,
                multiple = TRUE,
                options  = list(
                  title = state[["MC"]][["labels"]][["uknown_action"]])
              )
          ))

      }
      uiele
    })
    #------------------------------------
    output$ui_dw_fds_wider = renderUI({
      req(input$select_dw_element)
      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_UD           = id_UD,
                             react_state     = react_state)

      req(input$select_dw_element)
      uiele = NULL
      if(state[["DW"]][["isgood"]]){
        # Current final dataset
        WDS = DW_fetch_current_view(state)[["WDS"]]
        # Columns in that dataset
        dscols = names(WDS)
        uiele = tagList(
          div(style = "display: flex;",
              pickerInput(
                inputId  = NS(id, "select_fds_wider_names"),
                label    = NULL,
                width    = 200,
                choices  = dscols,
                multiple = FALSE,
                options  = list(
                  title = state[["MC"]][["labels"]][["fds_wider_names"]])
              ),
              pickerInput(
                inputId  = NS(id, "select_fds_wider_values"),
                label    = NULL,
                width    = 200,
                choices  = dscols,
                multiple = FALSE,
                options  = list(
                  title = state[["MC"]][["labels"]][["fds_wider_values"]])
              )
          ))

      }
      uiele
    })
    #------------------------------------
    output$ui_dw_fds_longer = renderUI({
      req(input$select_dw_element)
      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_UD           = id_UD,
                             react_state     = react_state)

      req(input$select_dw_element)
      uiele = NULL
      if(state[["DW"]][["isgood"]]){
        # Current final dataset
        WDS = DW_fetch_current_view(state)[["WDS"]]
        # Columns in that dataset
        dscols = names(WDS)
        uiele = tagList(
          div(style = "display: flex;",
              pickerInput(
                inputId  = NS(id, "select_fds_longer_column"),
                label    = NULL,
                width    = 200,
                choices  = dscols,
                multiple = TRUE,
                options  = list(
                  title = state[["MC"]][["labels"]][["fds_longer_column"]])
              ),
              textInput(
                inputId     = NS(id, "select_fds_longer_names"),
                label       = NULL,
                width       = 200,
                value       = "names",
                placeholder = state[["MC"]][["labels"]][["fds_longer_names"]]
              ),
              textInput(
                inputId     = NS(id, "select_fds_longer_values"),
                label       = NULL,
                width       = 200,
                value       = "values",
                placeholder = state[["MC"]][["labels"]][["fds_longer_values"]]
              )
          ))

      }
      uiele
    })
    #------------------------------------
    #------------------------------------
    output$ui_dw_fds_select = renderUI({
      req(input$select_dw_element)
      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_UD           = id_UD,
                             react_state     = react_state)

      req(input$select_dw_element)
      uiele = NULL
      if(state[["DW"]][["isgood"]]){
        # Current final dataset
        WDS = DW_fetch_current_view(state)[["WDS"]]
        # Columns in that dataset
        dscols = names(WDS)
        uiele = tagList(
          div(style = "display: flex;",
              pickerInput(
                inputId  = NS(id, "select_fds_select_column"),
                label    = NULL,
                width    = 200,
                choices  = dscols,
                multiple = TRUE,
                options  = list(
                  title = state[["MC"]][["labels"]][["uknown_action"]])
              )
          ))

      }
      uiele
    })
    #------------------------------------
    output$ui_dw_fds_ungroup             = renderUI({
      req(input$select_dw_element)
      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_UD           = id_UD,
                             react_state     = react_state)

      req(input$select_dw_element)
      uiele = NULL
      if(state[["DW"]][["isgood"]]){
        # Current final dataset
        WDS = DW_fetch_current_view(state)[["WDS"]]
        # Columns in that dataset
        dscols = names(WDS)
        uiele = tagList(
          tags$b(state[["MC"]][["labels"]][["ungroup_data"]])
        )
      }
      uiele
    })
    #------------------------------------
    output$ui_dw_fds_onerow              = renderUI({
      req(input$select_dw_element)
      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_UD           = id_UD,
                             react_state     = react_state)

      req(input$select_dw_element)
      uiele = NULL
      if(state[["DW"]][["isgood"]]){
        # Current final dataset
        WDS = DW_fetch_current_view(state)[["WDS"]]
        # Columns in that dataset
        dscols = names(WDS)
        uiele = tagList(
          tags$b(state[["MC"]][["labels"]][["keep_onerow"]])
        )
      }
      uiele
    })
    #------------------------------------
    # filter
    output$ui_dw_fds_filter_column_select = renderUI({
      # Forcing a reaction to changes in other modules
      react_state[[id_UD]]
      react_state[[id_ASM]]
      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_UD           = id_UD,
                             react_state     = react_state)

      req(input$select_dw_element)
      uiele = NULL
      if(state[["DW"]][["isgood"]]){
        # Current final dataset
        WDS = DW_fetch_current_view(state)[["WDS"]]
        # Columns in that dataset
        dscols = names(WDS)

        uiele = pickerInput(
          inputId  = NS(id, "select_fds_filter_column"),
          label    = NULL,
          width    = 200,
          choices  = dscols,
          options  = list(
            title = state[["MC"]][["labels"]][["fds_filter_column"]])
        )

      }
      uiele
    })

    #------------------------------------
    output$ui_dw_fds_filter_operator_select = renderUI({
      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_UD           = id_UD,
                             react_state     = react_state)

      req(input$select_fds_filter_column)
      uiele = NULL
      if(state[["DW"]][["isgood"]]){
        # Current final dataset
        WDS = DW_fetch_current_view(state)[["WDS"]]
        # Columns in that dataset
        dscols = names(WDS)

        filter_col = state[["DW"]][["ui"]][["select_fds_filter_column"]]

        if(!is.numeric(WDS[[filter_col]])){
          choices  = state[["DW"]][["op_choices"]][["factor"]]
        } else {
          choices  = state[["DW"]][["op_choices"]][["not_factor"]]
        }
        uiele = pickerInput(
          inputId  = NS(id, "select_fds_filter_operator"),
          label    = NULL,
          width    = 200,
          choices  = choices,
          options  = list(
            title = state[["MC"]][["labels"]][["fds_filter_operator"]])
        )

      }
      uiele
    })
    #------------------------------------
    output$ui_dw_fds_filter_rhs = renderUI({
      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_UD           = id_UD,
                             react_state     = react_state)

      req(input$select_fds_filter_column)
      req(input$select_fds_filter_operator)
      uiele = NULL
      if(state[["DW"]][["isgood"]]){
        # Current final dataset
        WDS = DW_fetch_current_view(state)[["WDS"]]

        # If the user filters down to zero rows
        if(nrow(WDS) > 0){
          # Columns in that dataset
          dscols = names(WDS)

          filter_col = state[["DW"]][["ui"]][["select_fds_filter_column"]]

          choices  = sort(unique(unfactor((WDS[[filter_col]]))))

          # We process factors different than
          if(!is.numeric(WDS[[filter_col]])){
            uiele = pickerInput(
              inputId  = NS(id, "fds_filter_rhs"),
              label    = NULL,
              width    = 200,
              choices  = choices,
              multiple = TRUE,
              options  = list(
                title = state[["MC"]][["labels"]][["fds_filter_rhs"]])
            )

          } else {
            if(state[["DW"]][["ui"]][["select_fds_filter_operator"]] %in%
               c("within")){
              selected =  c(min(choices), max(choices))
            } else {
              selected =  choices[ceiling(length(choices)/2)]
            }
            uiele    = sliderTextInput(
                 inputId   = NS(id, "fds_filter_rhs"),
                 label     = NULL,
                 choices   = choices,
                 selected  = selected,
                 width     = 200,
                 dragRange = TRUE
               )
          }
        } else {
          # Return an error
          uiele = tags$em(state[["MC"]][["errors"]][["no_rows"]])
        }
      }
      uiele
    })
    #------------------------------------
    # UI for selecting the type of dw element to add
    output$ui_dw_select = renderUI({
      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_UD           = id_UD,
                             react_state     = react_state)


      if(state[["DW"]][["UD"]][["isgood"]]){
        uiele = tagList()
        choicesOpt = list(
          subtext = c(
            state[["MC"]][["actions"]][["filter"]] [["subtext"]],
            state[["MC"]][["actions"]][["mutate"]] [["subtext"]],
            state[["MC"]][["actions"]][["rename"]] [["subtext"]],
            state[["MC"]][["actions"]][["select"]] [["subtext"]],
            state[["MC"]][["actions"]][["group"]]  [["subtext"]],
            state[["MC"]][["actions"]][["ungroup"]][["subtext"]],
            state[["MC"]][["actions"]][["longer"]][["subtext"]],
            state[["MC"]][["actions"]][["wider"]][["subtext"]],
            state[["MC"]][["actions"]][["onerow"]][["subtext"]]
          ),
          icon    = c(
            "glyphicon-filter" ,
            "glyphicon-wrench",
            "glyphicon-edit",
            "glyphicon-basket-check",
            "glyphicon-resize-small",
            "glyphicon-resize-full",
            "glyphicon-resize-vertical",
            "glyphicon-resize-horizontal",
            "glyphicon-export"
          ),
          lib = rep("glyphicon", length(icon)))


        cnames = c( state[["MC"]][["actions"]][["filter"]] [["choice"]] ,
                    state[["MC"]][["actions"]][["mutate"]] [["choice"]] ,
                    state[["MC"]][["actions"]][["rename"]] [["choice"]] ,
                    state[["MC"]][["actions"]][["select"]] [["choice"]] ,
                    state[["MC"]][["actions"]][["group"]]  [["choice"]] ,
                    state[["MC"]][["actions"]][["ungroup"]][["choice"]],
                    state[["MC"]][["actions"]][["longer"]] [["choice"]],
                    state[["MC"]][["actions"]][["wider"]]  [["choice"]],
                    state[["MC"]][["actions"]][["onerow"]] [["choice"]]
        )
        choices   = c(
          "filter",
          "mutate",
          "rename",
          "select",
          "group",
          "ungroup",
          "longer",
          "wider",
          "onerow"
        )
        names(choices) = cnames

        uiele = tagList(uiele,
          shinyWidgets::pickerInput(
             inputId    = NS(id, "select_dw_element"),
             choices    = choices,
             width      = "fit",
             inline     = TRUE,
             choicesOpt = choicesOpt))
      } else {
        uiele = NULL
      }

      uiele
    })

    #------------------------------------
    # Current DW elements
    output$ui_dw_add_element_button = renderUI({
      # req(input$fds_filter_rhs)
      input$select_dw_element
      # Forcing a reaction to changes in other modules
      react_state[[id_UD]]
      react_state[[id_ASM]]
      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_UD           = id_UD,
                             react_state     = react_state)

      if(state[["DW"]][["UD"]][["isgood"]]){
        uiele = tagList(
          actionBttn(
            inputId  = NS(id, "button_dw_add_element"),
             label   = state[["MC"]][["labels"]][["add_element"]],
             icon    = icon("plus-sign", lib="glyphicon"),
             size    = state[["MC"]][["formatting"]][["button_dw_add_element"]][["size"]],
             block   = state[["MC"]][["formatting"]][["button_dw_add_element"]][["block"]],
             color   = "success",
             style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]]
             ))
      } else {
        uiele = NULL
      }

      uiele
    })
    #------------------------------------
    output$ui_dw_new_view  = renderUI({
      #req(input$X)
      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_UD           = id_UD,
                             react_state     = react_state)

      uiele = NULL
      if(state[["DW"]][["isgood"]]){
        uiele = actionBttn(
                  inputId = NS(id, "button_dw_new"),
                  label   = state[["MC"]][["labels"]][["new_dw"]],
                  style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
                  size    = state[["MC"]][["formatting"]][["button_dw_new"]][["size"]],
                  block   = state[["MC"]][["formatting"]][["button_dw_new"]][["block"]],
                  color   = "success",
                  icon    = icon("plus"))
      }
      uiele})
    #------------------------------------
    output$ui_dw_save_view  = renderUI({
      #req(input$X)
      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_UD           = id_UD,
                             react_state     = react_state)

      uiele = NULL
      if(state[["DW"]][["isgood"]]){
        uiele = actionBttn(
                  inputId = NS(id, "button_dw_save"),
                  label   = state[["MC"]][["labels"]][["save_dw"]],
                  style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
                  size    = state[["MC"]][["formatting"]][["button_dw_save"]][["size"]],
                  block   = state[["MC"]][["formatting"]][["button_dw_save"]][["block"]],
                  color   = "primary",
                  icon    = icon("arrow-down"))
      }
      uiele})
    #------------------------------------
    output$ui_dw_copy_view  = renderUI({
      #req(input$X)
      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_UD           = id_UD,
                             react_state     = react_state)

      uiele = NULL
      if(state[["DW"]][["isgood"]]){
        uiele = actionBttn(
                  inputId = NS(id, "button_dw_copy"),
                  label   = state[["MC"]][["labels"]][["copy_dw"]],
                  style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
                  size    = state[["MC"]][["formatting"]][["button_dw_copy"]][["size"]],
                  block   = state[["MC"]][["formatting"]][["button_dw_copy"]][["block"]],
                  color   = "royal",
                  icon    = icon("copy"))
      }
      uiele})
    #------------------------------------
    output$ui_dw_del_view  = renderUI({
      #req(input$X)
      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_UD           = id_UD,
                             react_state     = react_state)

      uiele = NULL
      if(state[["DW"]][["isgood"]]){
        uiele = actionBttn(
                  inputId = NS(id, "button_dw_del"),
                  label   = state[["MC"]][["labels"]][["del_dw"]],
                  style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
                  size    = state[["MC"]][["formatting"]][["button_dw_del"]][["size"]],
                  block   = state[["MC"]][["formatting"]][["button_dw_del"]][["block"]],
                  color   = "danger",
                  icon    = icon("minus"))
      }
      uiele})
    #------------------------------------
    # table preview of the data
    output$hot_data_preview =  rhandsontable::renderRHandsontable({
      # Forcing a reaction to changes in other modules
      react_state[[id_UD]]
      react_state[[id_ASM]]
      # Triggering rebuilding of the table
      input$button_dw_add_element
      input$button_dw_new
      input$button_dw_del
      input$button_dw_copy
      # Force update on deletion clicks
      input$hot_dw_elements
      # Force update when the view is changed
      input$select_dw_views
      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_UD           = id_UD,
                             react_state     = react_state)

      df = NULL

      if(state[["DW"]][["UD"]][["isgood"]]){
        df = DW_fetch_current_view(state)[["WDS"]]
        ds = state[["DW"]][["UD"]]
      }

     if(is.null(df)){
       df = data.frame("No_Data"= state[["MC"]][["labels"]][["no_dataset"]])
     }

     uiele = rhandsontable::rhandsontable(
       df,
       width  = state[["MC"]][["formatting"]][["preview"]][["width"]],
       height = state[["MC"]][["formatting"]][["preview"]][["height"]],
       rowHeaders = NULL
       )

    uiele})
    #------------------------------------
    # Creates the ui for the compact view of the module
    output$DW_ui_compact  =  renderUI({
      # Forcing a reaction to changes in other modules
      react_state[[id_UD]]
      react_state[[id_ASM]]
      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_UD           = id_UD,
                             react_state     = react_state)

      current_view = DW_fetch_current_view(state)


      if(is.null(current_view[["WDS"]])){
        uiele = state[["MC"]][["labels"]][["no_dataset"]]
      } else {
        uiele_code_button = NULL
        # Generating code button if enabled
        if( state$MC$compact$code){
          uiele_code = tagList(shinyAce::aceEditor(
            NS(id, "ui_dw_code"),
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
             status  = "danger btn-custom-dw",
             icon    = icon("code", lib="font-awesome"),
             tooltip = tooltipOptions(title = state[["MC"]][["tooltips"]][["show_code"]]))
          )

        }

        # Button with DW elements table
        uiele_dw_elements = rhandsontable::rHandsontableOutput(NS(id, "hot_dw_elements"))
        uiele_dw_elements_button = tagList(
         shinyWidgets::dropdownButton(
           uiele_dw_elements,
           inline  = FALSE,
           right   = TRUE ,
           size    = "sm",
           circle  = FALSE,
           #width   = state[["MC"]][["formatting"]][["dw_elements"]][["width"]],
           status  = "primary btn-custom-dw",
           icon    = icon("layer-group", lib="font-awesome"),
           tooltip = tooltipOptions(title = state[["MC"]][["tooltips"]][["dw_elements"]]))
        )

        uiele = tagList(
          div(style="display:inline-block", htmlOutput(NS(id, "ui_dw_views"))),
          div(style="display:inline-block", htmlOutput(NS(id, "ui_dw_key"))),
          tags$br(),
          verbatimTextOutput(NS(id, "ui_dw_new_element_msg"))
        )

        uiele_buttons_right = tagList(
                 tags$style(".btn-custom-dw {width: 100px;}"),
                 div(style="display:inline-block;vertical-align:top",
                 uiele_dw_elements_button,
                 uiele_code_button,
                 htmlOutput(NS(id, "ui_dw_save_view")),
                 htmlOutput(NS(id, "ui_dw_copy_view")),
                 htmlOutput(NS(id, "ui_dw_del_view")),
                 htmlOutput(NS(id, "ui_dw_new_view"))
                 ))

        uiele_buttons_left = tagList(
        # div(style="display:inline-block;vertical-align:top",
        # htmlOutput(NS(id, "ui_dw_save_view")),
        # htmlOutput(NS(id, "ui_dw_copy_view")),
        # htmlOutput(NS(id, "ui_dw_del_view")),
        # htmlOutput(NS(id, "ui_dw_new_view"))
        # )
        )



        # Appending the buttons to the main uiele
        uiele = tagList(
          uiele,
          uiele_buttons_left)


        # Appending the preview
        uiele_preview = NULL
        if( state$MC$compact$preview){
         uiele_preview =
            div(style="display:inline-block;vertical-align:top",
              rhandsontable::rHandsontableOutput(NS(id, "hot_data_preview")))
          uiele = tagList(
            uiele,
            uiele_preview,
            uiele_buttons_right,
            tags$br()
          )
        }



        uiele = tagList( uiele,
          tags$br(),
          div(style="display:inline-block", htmlOutput(NS(id, "ui_dw_add_element_button"))),
          div(style="display:inline-block", htmlOutput(NS(id, "ui_dw_select"))),
          tags$br(),
          htmlOutput(NS(id, "ui_dw_new_element_row")),
          tags$br(),
          verbatimTextOutput(NS(id, "ui_dw_msg"))
        )
      }



      uiele
    })
    #------------------------------------
    # Creating reaction if a variable has been specified
    if(!is.null(react_state)){
      # Here we list the ui inputs that will result in a state change:
      toListen <- reactive({
        list(input$button_dw_add_element,
             input$select_dw_views,
             input$button_dw_new,
             input$button_dw_del,
             input$button_dw_copy,
             input$button_dw_save,
             react_state[[id_ASM]])
      })
      # This updates the reaction state:
      observeEvent(toListen(), {
        state = DW_fetch_state(id              = id,
                               input           = input,
                               session         = session,
                               FM_yaml_file    = FM_yaml_file,
                               MOD_yaml_file   = MOD_yaml_file,
                               id_UD           = id_UD,
                               react_state     = react_state)

        FM_le(state, "reaction state updated")
        react_state[[id]] = state
      }, priority=99)
    }

    # Removing holds
    remove_hold_listen  <- reactive({
      list(input$select_dw_views)
    })
    observeEvent(remove_hold_listen(), {
      # Once the UI has been regenerated we
      # remove any holds for this module
      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_UD           = id_UD,
                             react_state     = react_state)

      FM_le(state, "removing holds")
      # Removing all holds
      for(hname in names(state[["DW"]][["ui_hold"]])){
        remove_hold(state, session, hname)
      }
    }, priority = -100)

  })
}


#'@export
#'@title Fetch Data Wrangling State
#'@description Merges default app options with the changes made in the UI
#'@param id Shiny module ID
#'@param input Shiny input variable
#'@param session Shiny session variable
#'@param FM_yaml_file App configuration file with FM as main section.
#'@param MOD_yaml_file  Module configuration file with MC as main section.
#'@param id_UD  ID string for the upload data module used to handle uploads or
#'the name of the list element in react_state where the data set is stored.
#'@param react_state Variable passed to server to allow reaction outside of
#'module (\code{NULL})
#'@return List containing the current state of the DM module including default
#'values from the yaml file as well as any changes made by the user. The
#'structure of the list is defined below.
#'\itemize{
#'  \item{yaml:} Contents of the yaml file.
#'  \item{MC:} Module components of the yaml file.
#'  \item{DW:} Data wrangling state
#'  \itemize{
#'    \item{isgood:} Boolean status of the state. FALSE if the dataset
#'    identified by id_UD is bad.
#'    \item{checksum:}         MD5 sum indicating if there was a change in the
#'    datasets within the view. Use this to trigger updates in respose to
#'    changes in this module.
#'    \item{button_counters:}  List of counters to detect button clicks.
#'    \item{code_previous:}    Loading code from the UD field.
#'    \item{current_view:}     View id of the current active data wrangling view.
#'    \item{UD:}               Copy of the \code{"UD"} field of the  \code{id_UD} from the \code{react_state} input.
#'    \item{ui:}               Current value of form elements in the UI
#'    \item{ui_hold:}          List of hold elements to disable updates before a full ui referesh is complete.
#'    \item{view_cntr:}        Counter for tracking view ids, value contains the id of the last view created.
#'    \item{views:}            List of data wrangling views. Each view has the following structure:
#'      \itemize{
#'        \item{isgood:} Boolean status of the data view. False if evaluation fails
#'        \item{id: Character id (\code{view_idx})}
#'        \item{idx: Numeric id (\code{1})}
#'        \item{view_ds_object_name: Object name for this data view}
#'        \item{code_previous: Code to load data and assign to view object.}
#'        \item{key: User key (short description)}
#'        \item{WDS: Current value of the data view with all of the successful commands in elements_table evaluated.}
#'        \item{elements_table: Table of data wrangling elements.}
#'        \item{checksum: MD5 sum of WDS}
#'        \item{code: Code to generate WDS from start to finish}
#'        \item{code_dw_only: Code for jsut the wrangling portion.}
#'      }
#'  }
#'  \item{MOD_TYPE:} Character data containing the type of module \code{"DW"}
#'  \item{id:} Character data containing the module id
#'  \item{FM_yaml_file:} App configuration file with FM as main section.
#'  \item{MOD_yaml_file:}  Module configuration file with MC as main section.
#'  module in the session variable.
#'}
DW_fetch_state = function(id,                    input,     session,
                          FM_yaml_file,  MOD_yaml_file,       id_UD,
                          react_state){

  #---------------------------------------------
  # Getting the current state
  state = FM_fetch_mod_state(session, id)
  # If the state has not yet been defined then we
  # initialize it
  if(is.null(state)){
    # General state information
    state = DW_init_state(FM_yaml_file, MOD_yaml_file, id, id_UD, react_state)
  }


  #---------------------------------------------
  # detecting changes in the datasets
  if("checksum" %in% names(isolate(react_state[[id_UD]][["UD"]]))){
    # Checksum of the uploaded dataset from the UD module
    UD_checksum = isolate(react_state[[id_UD]][["UD"]][["checksum"]])
    # Checksum of the copy in the DW module:
    DW_checksum = isolate(state[["DW"]][["UD"]][["checksum"]])

    UPDATE_DS = FALSE
    if(is.null(DW_checksum)){
      # If this is NULL then we've never processed the dataset and need to
      # update it:
      UPDATE_DS = TRUE
    } else {
      # If these are different then the dataset has changed
      if(UD_checksum != DW_checksum){
        UPDATE_DS = TRUE
      }
    }
    # If the dataset has been updated we need to reset the DW app state:
    if(UPDATE_DS){
      # JMH prevent triggering UPDATE_DS on app state reset
      FM_le(state, "original dataset changed")
      state = DW_init_state(FM_yaml_file, MOD_yaml_file, id, id_UD, react_state)
    }
  }

  #---------------------------------------------
  # Getting the current ui elements into the state
  for(ui_name in state[["DW"]][["ui_ids"]]){
    if(!is.null(isolate(input[[ui_name]]))){
      state[["DW"]][["ui"]][[ui_name]] = isolate(input[[ui_name]])
    } else {
      state[["DW"]][["ui"]][[ui_name]] = ""
    }
  }


  # Here we're processing any element delete requests
  # - first we only do this if the hot_dw_elements has been defined
  if(!fetch_hold(state,"hot_dw_elements")){
    if(is.list(state[["DW"]][["ui"]][["hot_dw_elements"]])){
      # - If that's the case we get the data frame for it:
      hot_df = rhandsontable::hot_to_r(state[["DW"]][["ui"]][["hot_dw_elements"]])
      # - Because the UI initialzes to a "no elements defined message" we need
      # to make sure there is a delete column
      if("Delete" %in% names(hot_df)){
        # - lastly we check to see if any have been selected for deletion:
        if(any(hot_df$Delete == TRUE)){

          # Pulling out the current view
          current_view = DW_fetch_current_view(state)

          # We walk through the current elements table and keep only the
          # rows where Delete is FALSE
          NEW_ET = NULL
          OLD_ET = current_view[["elements_table"]]
          # This can be run before the hot_table updates and if the
          # last element has been deleted the OLD_ET (already updated)
          # will be NULL
          if(!is.null(OLD_ET)){
            # If this is run before the hot_table updates we want to skip it
            # So this, so we only update when the OLD_ET and the table in the UI have
            # the same number of rows
            if(nrow(OLD_ET) == nrow(hot_df)){
              for(eridx in 1:nrow(OLD_ET)){
                if(hot_df[eridx, ]$Delete == FALSE){
                  NEW_ET = rbind(NEW_ET,
                                 OLD_ET[eridx,])
                }
              }


              # First we set the elements table to NULL and reset the wrangled
              # dataset to the uploaded value:
              current_view[["elements_table"]]  = NULL
              current_view[["WDS"]]             = state[["DW"]][["UD"]][["contents"]]
              state = DW_set_current_view(state, current_view)

              # If NEW_ET is NULL then we don't have any wrangling elements left
              # (ie we deleted the last one). If it's not null then we need to apply
              # the filters that are left:
              if(!is.null(NEW_ET)){
                # Walking through each element
                error_found = FALSE
                msgs = c()
                for(eridx in 1:nrow(NEW_ET)){
                  # Running the current element
                  if(!error_found){
                    dwee_res = dw_eval_element(state,NEW_ET[eridx,][["cmd"]])

                    # Appending any messages
                    msgs = c(msgs, dwee_res[["msgs"]])

                    if(dwee_res[["isgood"]]){
                      # We set the status to success here:
                      NEW_ET[eridx,][["Status"]] = "Success"
                      # We have to assinge this to the elements table and the
                      # dataset:
                      current_view[["elements_table"]]  = NEW_ET
                      current_view[["WDS"]]             = dwee_res[["DS"]]
                      # We set this view to update the code as well:
                      state = DW_set_current_view(state, current_view)

                    } else {
                      # If an element fails we filp the error_found flag.
                      # This will skip the remaining elements
                      error_found = TRUE
                      # Then we set the status to Failure
                      NEW_ET[eridx,][["Status"]] = "Failure"
                    }
                  } else {
                    # If any errors occured before this
                    # element is set to Not Run
                    NEW_ET[eridx,][["Status"]] = "Not Run"
                  }
                }

                # Passing any messages back to the user
                state = FM_set_ui_msg(state, msgs)
              }
              # Saving the NEW_ET over the elements_table in the state
              current_view[["elements_table"]]  = NEW_ET
              state = DW_set_current_view(state, current_view)
            }
          }
        }
      }
    }
  }

  # Detecting view selection changes
  if(has_changed(ui_val   = state[["DW"]][["ui"]][["select_dw_views"]],
                 old_val  = state[["DW"]][["current_view"]]) &
      (!fetch_hold(state,"select_dw_views"))){

    # Changing the current view to the one selected in the UI
    state[["DW"]][["current_view"]] =  state[["DW"]][["ui"]][["select_dw_views"]]
    FM_le(state, "updated: select_dw_views")
  }
  # Detecting add_element clicks
  if(has_changed(ui_val   = state[["DW"]][["ui"]][["button_dw_add_element"]],
                 old_val  = state[["DW"]][["button_counters"]][["button_dw_add_element"]])){
    # Empty messages:
    msgs = c()
    FM_le(state, "adding wrangling element")

    # Plucking out the current view
    current_view = DW_fetch_current_view(state)
    # Current elements table
    cet        = current_view[["elements_table"]]
    cet_isgood = TRUE
    if(!is.null(cet)){
      if(any(cet[["Status"]] == "Failure")){
        cet_isgood = FALSE
        msgs = c(msgs, state[["MC"]][["errors"]][["fix_bad_element"]])
      }
    }

    if(cet_isgood){
      # Building the data wranglig command
      dwb_res = dwrs_builder(state)

      # saving the messages
      msgs = c(msgs, dwb_res[["msgs"]])

      # If the data wrangling command was successfully built we evaluate
      # the chain to make sure the new element runs correctly:
      if(dwb_res[["isgood"]]){
        # Evaluating the element
        dwee_res = dw_eval_element(state, dwb_res[["cmd"]])
        # Appending any messages
        msgs = c(msgs, dwee_res[["msgs"]])

        # If that was successful we append the element to the elements
        # table
        if(dwee_res[["isgood"]]){
          state = DW_add_wrangling_element(state, dwb_res, dwee_res)
        }

      }
    }
    # Passing any messages back to the user
    state = FM_set_ui_msg(state, msgs)

    # Lastly we save the button value from the UI to the state:
    state[["DW"]][["button_counters"]][["button_dw_add_element"]] =state[["DW"]][["ui"]][["button_dw_add_element"]]

    # updating the state checksum
    state = DW_update_checksum(state)
  }

  #------------------------------------
  if(has_changed(ui_val   = state[["DW"]][["ui"]][["button_dw_new"]],
                 old_val  = state[["DW"]][["button_counters"]][["button_dw_new"]])){

    FM_le(state, "creating new wrangling view")
    # Empty messages:
    msgs = c()

    # Creatign a new view
    state = DW_new_view(state)

    # Setting hold for views select
    state = set_hold(state, inputId = "select_dw_views")

    # Lastly we save the button value from the UI to the state:
    state[["DW"]][["button_counters"]][["button_dw_new"]] =state[["DW"]][["ui"]][["button_dw_new"]]

    # updating the state checksum
    state = DW_update_checksum(state)
  }
  #------------------------------------
  if(has_changed(ui_val   = state[["DW"]][["ui"]][["button_dw_del"]],
                 old_val  = state[["DW"]][["button_counters"]][["button_dw_del"]])){

    FM_le(state, "deleting wrangling view")
    # Empty messages:
    msgs = c()

    # Fetchign the current view to get the id
    current_view = DW_fetch_current_view(state)

    # Deleting the current view
    state[["DW"]][["views"]][[current_view[["id"]]]] = NULL

    # If there are no views then we create an empty one
    if(length(state[["DW"]][["views"]]) == 0){
      state = DW_new_view(state)
    } else {
    # If there are views then we set the first one as active
      state[["DW"]][["current_view"]] = names(state[["DW"]][["views"]])[1]
    }

    # Setting hold for views select
    state = set_hold(state, inputId = "select_dw_views")

    # Lastly we save the button value from the UI to the state:
    state[["DW"]][["button_counters"]][["button_dw_del"]] =state[["DW"]][["ui"]][["button_dw_del"]]

    # updating the state checksum
    state = DW_update_checksum(state)
  }

  #------------------------------------
  if(has_changed(ui_val   = state[["DW"]][["ui"]][["button_dw_copy"]],
                 old_val  = state[["DW"]][["button_counters"]][["button_dw_copy"]])){

    FM_le(state, "copying wrangling view")

    # Empty messages:
    msgs = c()

    # Original is the view being copied
    original_view = DW_fetch_current_view(state)

    # This creates the new view and makes it active:
    state = DW_new_view(state)

    # Now we pull out the new view:
    new_view = DW_fetch_current_view(state)

    # Changing object references
    # Each view has a unique object that is generated (e.g. the first view
    # will have something like DW_myDS_1, the second one will have DW_myDS_2,
    # etc). When we copy an old view to a new one, we need those object
    # references to change to the new one as well:
    if(!is.null(original_view[["elements_table"]])){
      original_view[["elements_table"]]  =
        dplyr::mutate( original_view[["elements_table"]],
          cmd = str_replace_all(
            .data[["cmd"]],
            paste0("\\b", original_view[["view_ds_object_name"]], "\\b"),
            new_view[["view_ds_object_name"]]))
    }


    # From the original view we copy both the WDS and elements_table fields
    new_view[["WDS"]]            = original_view[["WDS"]]
    new_view[["elements_table"]] = original_view[["elements_table"]]
    state = DW_set_current_view(state, new_view)

    # Setting hold for views select
    state = set_hold(state, inputId = "select_dw_views")

    # Lastly we save the button value from the UI to the state:
    state[["DW"]][["button_counters"]][["button_dw_copy"]] =state[["DW"]][["ui"]][["button_dw_copy"]]

    # updating the state checksum
    state = DW_update_checksum(state)
  }
  #------------------------------------
  if(has_changed(ui_val   = state[["DW"]][["ui"]][["button_dw_save"]],
                 old_val  = state[["DW"]][["button_counters"]][["button_dw_save"]])){

    FM_le(state, "saving changes to current wrangling view")
    # Empty messages:
    msgs = c()

    if(state[["DW"]][["ui"]][["current_key"]] != ""){
      # Resetting the key
      current_view = DW_fetch_current_view(state)
      current_view[["key"]] = state[["DW"]][["ui"]][["current_key"]]
      state = DW_set_current_view(state, current_view)

    } else {
      # returning an error
      msgs = c(msgs,
          tags$em(state[["MC"]][["errors"]][["current_key_empty"]]))
    }

    # Lastly we save the button value from the UI to the state:
    state[["DW"]][["button_counters"]][["button_dw_save"]] = state[["DW"]][["ui"]][["button_dw_save"]]

    # updating the state checksum
    state = DW_update_checksum(state)
  }

  # Saving the state
  FM_set_mod_state(session, id, state)

  # Returning the state
state }

#'@export
#'@title Initialize DW Module State
#'@description Creates a list of the initialized module state
#'@param FM_yaml_file App configuration file with FM as main section.
#'@param MOD_yaml_file  Module configuration file with MC as main section.
#'@param id Shiny module ID
#'@param id_UD  ID string for the upload data module used to handle uploads or
#'the name of the list element in react_state where the data set is stored.
#'@param react_state Variable passed to server to allow reaction outside of
#'module (\code{NULL})
#'@return list containing an empty DW state
DW_init_state = function(FM_yaml_file, MOD_yaml_file, id, id_UD, react_state){

  # initializing the state with the required formods elements:
  button_counters = c(
    "button_dw_add_element"   , # Element: Adding a new element
    "button_dw_new"           , # View:    New blank view
    "button_dw_del"           , # View:    Delete the current view
    "button_dw_save"          , # View:    Save the current view
    "button_dw_copy"            # View:    Copy the current view
    )
  ui_ids          = c(
    "hot_dw_elements"            ,
    "button_dw_add_element"      ,
    "button_dw_new"              ,
    "button_dw_del"              ,
    "button_dw_copy"             ,
    "button_dw_save"             ,
    "select_fds_filter_column"   ,
    "select_fds_filter_operator" ,
    "fds_filter_rhs"             ,
    "select_fds_mutate_column"   ,
    "select_fds_mutate_rhs"      ,
    "select_fds_rename_column"   ,
    "fds_rename_rhs"             ,
    "current_key"                ,
    "select_fds_group_column"    ,
    "select_fds_select_column"   ,
    "select_fds_longer_column"   ,
    "select_fds_longer_values"   ,
    "select_fds_longer_names"    ,
    "select_fds_wider_values"    ,
    "select_fds_wider_names"     ,
    "select_dw_views"            ,
    "select_dw_element"
    )

  ui_hold         = c(
    "hot_dw_elements",
    "current_key"    ,
    "select_dw_views"
    )

  state = FM_init_state(
    FM_yaml_file    = FM_yaml_file,
    MOD_yaml_file   = MOD_yaml_file,
    id              = id,
    MT              = "DW",
    button_counters = button_counters,
    ui_ids          = ui_ids,
    ui_hold         = ui_hold)

  # Adding other module-specific fields
  # Creating operator table
  opdf = NULL
  for(op_idx in c(1:length(state[["MC"]][["operators"]]))){
    opdf = rbind(opdf,
                 as.data.frame(state[["MC"]][["operators"]][[op_idx]]))
  }

  # Creating operator choices
  state[["DW"]][["op_choices"]] =  list()
  tmpdf = dplyr::filter(opdf, .data[["type"]] == "factor")
  state[["DW"]][["op_choices"]][["factor"]] = stats::setNames(tmpdf$rop, c(tmpdf$text))
  tmpdf = dplyr::filter(opdf, .data[["type"]] == "not_factor")
  state[["DW"]][["op_choices"]][["not_factor"]] = stats::setNames(tmpdf$rop, c(tmpdf$text))


  state[["DW"]][["code_previous"]]        = NULL
  state[["DW"]][["views"]]                = NULL
  state[["DW"]][["current_view"]]         = NULL
  state[["DW"]][["view_cntr"]]            = 0

  # Attaching the dataset to the state object
  if(is.null(state[["DW"]][["UD"]])){
    if(!is.null(isolate(react_state[[id_UD]][["UD"]]))){
      state = DW_attach_ds(state, isolate(react_state[[id_UD]][["UD"]]))
    } else {
      state[["DW"]][["UD"]][["isgood"]] = FALSE
      state[["DW"]][["isgood"]]         = FALSE
    }
  }


  if(state[["DW"]][["isgood"]]){
    # Initializing an empty figure
    state = DW_new_view(state)
  }

  FM_le(state, "State initialized")

  # initializing the module checksum:
  state = DW_update_checksum(state)


state }


#'@export
#'@title Builds a Data Wrangling R Statement From ui Elements:
#'@description Takes the current ui elements and constructs the appropriate
#'data wrangling command from the user input.
#'@param state DW state from \code{DW_fetch_state()}
#'@return list containing the following elements
#'\itemize{
#'  \item{isgood:} Return status of the function
#'  \item{cmd:}    Data wrangling R command
#'  \item{action:} The action being performed
#'  \item{desc:}   Verbose description of the action
#'  \item{msgs:}   Messages to be passed back to the user
#'}
dwrs_builder = function(state){

  isgood = TRUE
  msgs   = c()
  cmd    = ""
  desc   = ""
  action = ""


  action         = state[["DW"]][["ui"]][["select_dw_element"]]
  ui             = state[["DW"]][["ui"]]

  current_view        = DW_fetch_current_view(state)
  view_ds_object_name = current_view[["view_ds_object_name"]]

  # Checking to make sure the elements are preset for the given action to be
  # added to the chain:
  if(action == "filter"){
    if(paste(ui[["select_fds_filter_column"]], collapse=", ") == ""){
      isgood = FALSE
      msgs = c(msgs, state[["MC"]][["errors"]][["fds_filter_column"]])
    }
    if(paste(ui[["select_fds_filter_operator"]], collapse=", ") == ""){
      isgood = FALSE
      msgs = c(msgs, state[["MC"]][["errors"]][["fds_filter_operator"]])
    }
    if(paste(ui[["fds_filter_rhs"]], collapse=", ") == ""){
      isgood = FALSE
      msgs = c(msgs, state[["MC"]][["errors"]][["fds_filter_rhs"]])
    }
  } else if(action == "mutate"){
    if(paste(ui[["select_fds_mutate_column"]], collapse=", ") == ""){
      isgood = FALSE
      msgs = c(msgs, state[["MC"]][["errors"]][["fds_mutate_column"]])
    }
    if(paste(ui[["select_fds_mutate_rhs"]], collapse=", ") == ""){
      isgood = FALSE
      msgs = c(msgs, state[["MC"]][["errors"]][["fds_mutate_rhs"]])
    }
  } else if(action == "rename"){
    if(paste(ui[["select_fds_rename_column"]], collapse=", ") == ""){
      isgood = FALSE
      msgs = c(msgs, state[["MC"]][["errors"]][["fds_rename_column"]])
    }
    if(paste(ui[["fds_rename_rhs"]], collapse=", ") == ""){
      isgood = FALSE
      msgs = c(msgs, state[["MC"]][["errors"]][["fds_rename_rhs"]])
    }
  } else if(action == "group"){
    if(paste(ui[["select_fds_group_column"]], collapse=", ") == ""){
      isgood = FALSE
      msgs = c(msgs, state[["MC"]][["errors"]][["fds_group_column"]])
    }
  } else if(action == "longer"){
    if(paste(ui[["select_fds_longer_column"]], collapse=", ") == ""){
      isgood = FALSE
      msgs = c(msgs, state[["MC"]][["errors"]][["fds_longer_column"]])
    }
    if( ui[["select_fds_longer_names"]] == ""){
      isgood = FALSE
      msgs = c(msgs, state[["MC"]][["errors"]][["fds_longer_names"]])
    }
    if( ui[["select_fds_longer_values"]] == ""){
      isgood = FALSE
      msgs = c(msgs, state[["MC"]][["errors"]][["fds_longer_values"]])
    }
  } else if(action == "wider"){
    if(paste(ui[["select_fds_wider_names"]], collapse=", ") == ""){
      isgood = FALSE
      msgs = c(msgs, state[["MC"]][["errors"]][["fds_wider_names"]])
    }
    if(paste(ui[["select_fds_wider_values"]], collapse=", ") == ""){
      isgood = FALSE
      msgs = c(msgs, state[["MC"]][["errors"]][["fds_wider_values"]])
    }
  } else if(action == "select"){
    if(paste(ui[["select_fds_select_column"]], collapse=", ") == ""){
      isgood = FALSE
      msgs = c(msgs, state[["MC"]][["errors"]][["fds_select_column"]])
    }
  } else if(action == "ungroup"){
    # Nothing needs to be done here
  } else if(action == "onerow"){
    # Nothing needs to be done here
  } else {
    isgood = FALSE
    msgs = c(msgs, state[["MC"]][["errors"]][["fds_group_column"]])
  }

  # if we made it this far then everything is good and we build the command:)
  if(isgood){
    if(action == "filter"){
      cond_str = ""
      # The statement will depend on the operator
      if(ui[["select_fds_filter_operator"]] == "within"){
        cond_str = paste0(ui[["select_fds_filter_column"]],
                          " >= ", ui[["fds_filter_rhs"]][1], " & ",
                          ui[["select_fds_filter_column"]],
                          " <= ", ui$fds_filter_rhs[2])
        desc = cond_str
      } else if(ui[["select_fds_filter_operator"]] %in% c("==", "!=")){
        cond_str = paste(
          ui[["select_fds_filter_column"]],
          ui[["select_fds_filter_operator"]],
          ui[["fds_filter_rhs"]][1])
        desc = cond_str
      } else if(ui[["select_fds_filter_operator"]] %in% c("%in%", "!%in%")){
        selected = paste0('c("', paste(ui[["fds_filter_rhs"]], collapse='", "'), '")')
        cond_str = paste(
          ui[["select_fds_filter_column"]],
          "%in%",
          selected)
        if( ui[["select_fds_filter_operator"]] == "!%in%"){
          cond_str = paste("!(", cond_str, ")")
        }
        # Creatig a description of the action
        op_desc =  names(
          state[["DW"]][["op_choices"]][["factor"]][state[["DW"]][["op_choices"]][["factor"]]
                                                    == ui[["select_fds_filter_operator"]]] )[1]
        desc = paste0(ui[["select_fds_filter_column"]]," ",
                      op_desc, " ",
                      paste(ui[["fds_filter_rhs"]], collapse=','))
      }
      cmd = paste0(view_ds_object_name,  " = dplyr::filter(", view_ds_object_name, ",", cond_str, ")")
    } else if(action == "mutate"){
      rhs_str = ui[["select_fds_mutate_rhs"]]
      cmd = paste0(view_ds_object_name, " = dplyr::mutate(", view_ds_object_name,",",
                  ui[["select_fds_mutate_column"]],
                  " = ",
                  rhs_str, ")")
      desc = paste( ui[["select_fds_mutate_column"]],
                    "=", rhs_str)
    } else if(action == "rename"){
      new_name =  ui[["fds_rename_rhs"]]
      cmd = paste0(view_ds_object_name, " = dplyr::rename(", view_ds_object_name,",",
                  new_name,
                  " = ",
                  ui[["select_fds_rename_column"]],
                  ")")
      desc = paste(ui[["select_fds_rename_column"]], " to ", new_name)

    } else if(action == "group"){
      group_cols_str   = paste(ui[["select_fds_group_column"]], collapse=', ')
      cmd = paste0(view_ds_object_name, " = dplyr::group_by(", view_ds_object_name,",",
                  group_cols_str,
                  ")")
      desc = paste(group_cols_str)
    } else if(action == "select"){
      select_cols_str   = paste(ui[["select_fds_select_column"]], collapse=', ')
      cmd = paste0(view_ds_object_name, " = dplyr::select(", view_ds_object_name,",",
                  select_cols_str,
                  ")")
      desc = paste(select_cols_str)
    } else if(action == "longer"){
      select_cols_str         = paste(ui[["select_fds_longer_column"]], collapse=', ')
      select_cols_str_quote   = paste(ui[["select_fds_longer_column"]], collapse='", "')
      select_cols_str_quote   = paste0('c("', select_cols_str_quote, '")')
      names_to          = ui[["select_fds_longer_names"]]
      values_to         = ui[["select_fds_longer_values"]]

      cmd = paste0(view_ds_object_name, " = tidyr::pivot_longer(", view_ds_object_name,
                  ', cols      = ', select_cols_str_quote,
                  ', names_to  = "',  names_to,  '"',
                  ', values_to = "', values_to, '"',
                  ")")
      desc = paste(select_cols_str, "-->", names_to, ", ", values_to)
    } else if(action == "wider"){
      names_from        = ui[["select_fds_wider_names"]]
      values_from       = ui[["select_fds_wider_values"]]

      cmd = paste0(view_ds_object_name, " = tidyr::pivot_wider(", view_ds_object_name,
                  ', names_from  = "',  names_from,  '"',
                  ', values_from = "', values_from, '"',
                  ")")
      desc = paste("names_from: ", names_from, ", values_from: ", values_from)
    } else if(action == "ungroup"){
      cmd = paste0(view_ds_object_name, " = dplyr::ungroup(",view_ds_object_name,")")
      desc = state[["MC"]][["labels"]][["ungroup_data"]]
    } else if(action == "onerow"){
      cmd = paste0(view_ds_object_name, " = dplyr::filter(", view_ds_object_name, ",row_number()==1)")
      desc = state[["MC"]][["labels"]][["keep_onerow"]]
    } else {
      isgood = FALSE
      msgs = c(msgs, paste("Action not found:", action))
    }
  }

  res = list(isgood = isgood,
             cmd    = cmd,
             action = action,
             desc   = desc,
             msgs   = msgs)

  res
}


#'@export
#'@title Evaluates Data Wrangling Generated Code
#'@description Takes the current state and a string containing a data
#'wranlging command and evaluates it.
#'@param state DW state from \code{DW_fetch_state()}
#'@param cmd string containing the data wrangling command
#'@return list with the following elements
#'\itemize{
#'  \item{isgood:} Return status of the function.
#'  \item{msgs:}   Messages to be passed back to the user.
#'  \item{DS:}     Wrangled dataset.
#'}
dw_eval_element = function(state, cmd){

  msgs = c()
  current_view        = DW_fetch_current_view(state)
  DS                  = current_view[["WDS"]]
  view_ds_object_name = current_view[["view_ds_object_name"]]

  # Creating the name fo the dataset with the correct
  # object name
  assign(view_ds_object_name,
         DS)

  # Trying to evaluate the generated command against DS
  # to see if any errors are generated:
  tcres = tryCatch({
    eval(parse(text=cmd))
    DS = get(view_ds_object_name)
    list(DS = DS, isgood=TRUE)},
    error = function(e) {
      list(error=e, isgood=FALSE)}
  )
  if(!tcres[["isgood"]]){
    msgs = c(msgs, state[["MC"]][["errors"]][["element_not_added"]])
    if(!is.null(tcres[["error"]][["message"]])){
      msgs = c(msgs, paste0("message: ", tcres[["error"]][["message"]])) }
    if(!is.null(tcres[["error"]][["call"]])){
      msgs = c(msgs, paste0("call:    ", tcres[["error"]][["call"]])) }
  } else {
    DS = tcres[["DS"]]
  }

  res = list(isgood = tcres[["isgood"]],
             msgs   = msgs,
             DS     = DS)

res}

#'@export
#'@title New Data Wrangling View
#'@description Appends a new empty data wrangling view to the DW state object
#'and makes this new view the active view.
#'@param state DW state from \code{DW_fetch_state()}
#'@return DW state object containing a new data view and that view set as the
#'current active view. See the help for \code{DW_fetch_state()} for view
#'format.
DW_new_view = function(state){

  # Incrementing the view   counter
  state[["DW"]][["view_cntr"]] = state[["DW"]][["view_cntr"]] + 1

  # Creating a default View ID
  view_id = paste0("view_", state[["DW"]][["view_cntr"]])

  # Creating the object name for this view
  view_ds_object_name = paste0(state[["MC"]][["ds_object_name"]],
                          "_", state[["DW"]][["view_cntr"]])
  # Default for a new view:
  view_def =
    list(
         # internal use only
         isgood              = TRUE,
         id                  = view_id,
         idx                 = state[["DW"]][["view_cntr"]],
         view_ds_object_name = view_ds_object_name,
         code_previous       = NULL,
         # user facing
         key                 = paste0("data_", view_id),
         WDS                 = state[["DW"]][["UD"]][["contents"]],
         elements_table      = NULL,
         # Generated on save
         checksum            = digest::digest(state[["DW"]][["UD"]][["contents"]], algo=c("md5")),
         code                = NULL,
         code_dw_only        = NULL)


  # This contains the code to generate the input dataset
  code_previous = c(
    paste0(view_ds_object_name,
           " = ",
            state[["DW"]][["UD"]][["object_name"]]))
  view_def[["code_previous"]] = code_previous

  # Dropping the new view into the state
  state[["DW"]][["views"]][[view_id]] = view_def
  # Setting the new view as current
  state[["DW"]][["current_view"]]     = view_id

state}

#'@export
#'@title Fetches Current Data View
#'@description Takes a DW state and returns the current active view
#'@param state DW state from \code{DW_fetch_state()}
#'@return List containing the details of the active data view. The structure
#'of this list is the same as the structure of \code{state$DW$views} in the output of
#'\code{DW_fetch_state()}.
DW_fetch_current_view    = function(state){

  view_id = state[["DW"]][["current_view"]]

  current_view = state[["DW"]][["views"]][[view_id]]

current_view}

#'@export
#'@title Sets Current Data View
#'@description Takes a DW state and an updated view and sets that view to the
#'current view_id
#'@param state DW state from \code{DW_fetch_state()}
#'@param dw_view Data view list of the format returned from \code{DW_fetch_current_view()}
#'(see the structure of \code{state$DW$views} in the output of \code{DW_fetch_state()}).
#'@return DW state object with the value of \code{dw_view} set to the current view id.
DW_set_current_view    = function(state, dw_view){

  view_id = state[["DW"]][["current_view"]]


  # Saving the checksum
  dw_view[["checksum"]]  = digest::digest(dw_view[["WDS"]], algo=c("md5"))

  # We only want the rows where the status was sucessful because
  # that is the only code we want to save
  good_elements = dw_view[["elements_table"]]
  if(!is.null(good_elements)){
    good_elements = dplyr::filter(good_elements,
                                  .data[["Status"]] == "Success")
  }

  # updating only the data wrangling code:
  dw_view[["code_dw_only"]] =
    paste(good_elements[["cmd"]], collapse="\n")

  # updating the all of the code:
  codeall = c(state[["DW"]][["code_previous"]],   # Data loading code
              dw_view[["code_previous"]],         # Copying the data object to the DW object
              "# Data wrangling",
              good_elements[["cmd"]])

  dw_view[["code"]] = paste(codeall, collapse="\n")


  state[["DW"]][["views"]][[view_id]] = dw_view

  # Forcing an checksum update
  state = DW_update_checksum(state)

state}


#'@export
#'@title Updates DW Module Checksum
#'@description Takes a DW state and updates the checksum used to trigger
#'downstream updates
#'@param state DW state from \code{DW_fetch_state()}
#'@return DW state object with the checksum updated
DW_update_checksum     = function(state){

  # checksum string
  chk_str = ""

  # We'll concatinate all the individual checksums together
  # and create a checksum of those:
  view_ids = names(state[["DW"]][["views"]])
  for(view_id in view_ids){
    # We trigger updates when the dataframe changes:
    chk_str = paste0(chk_str, ":", state[["DW"]][["views"]][[view_id]][["checksum"]])

    # We also trigger updates when the key has changed as well:
    chk_str = paste0(chk_str, ":", state[["DW"]][["views"]][[view_id]][["key"]])
  }

  state[["DW"]][["checksum"]] = digest::digest(chk_str, algo=c("md5"))
  FM_le(state, paste0("module checksum updated:", state[["DW"]][["checksum"]]))

state}


#'@export
#'@title Attach Data Set to DW State
#'@description Attaches a dataset to the DW state supplied.
#'@param state DW state from \code{DW_fetch_state()}
#'@param DS Dataset list object with elements described by the DS field
#'returned by \code{UD_fetch_state()}.
#'@return state with data set attached
DW_attach_ds = function(state, DS){

  # This contains the input dataset:
  state[["DW"]][["UD"]]    = DS
  # This is the loading code
  state[["DW"]][["code_previous"]] = state[["DW"]][["UD"]][["code"]]
  # Setting the DW stat to good
  state[["DW"]][["isgood"]]        = TRUE

state}

#'@export
#'@title Adding Wrangling Element to Current Data View
#'@description Adds the wrangling element to the current data view.
#'@param state DW state from \code{DW_fetch_state()}
#'@param dwb_res  Output from \code{dwrs_builder()}
#'@param dwee_res Output from \code{dw_eval_element()}
#'returned by \code{UD_fetch_state()}.
#'@return state with data set attached
DW_add_wrangling_element = function(state, dwb_res, dwee_res){

  current_view = DW_fetch_current_view(state)
  # - append the cmd and description to the DW table
  current_view[["elements_table"]] =
    rbind(current_view[["elements_table"]],
      data.frame(
      Action        = dwb_res[["action"]],
      Description   = dwb_res[["desc"]],
      cmd           = dwb_res[["cmd"]],
      Status        = "Success",
      Delete        = FALSE))

  current_view[["WDS"]]  = dwee_res[["DS"]]
  state = DW_set_current_view(state, current_view)

state}

#'@export
#'@title Fetch Module Code
#'@description Fetches the code to generate results seen in the app
#'@param state DW state from \code{DW_fetch_state()}
#'@return Character object vector with the lines of code
#'and isgood)
DW_fetch_code = function(state){

  # If the UD contents is NULL we return NULL otherwise we return the code
  if(is.null(state[["DW"]][["UD"]][["contents"]])){
    code = NULL
  } else {
    codes = c()
    # code = state[[""]][["code"]]
    for(view_id in names(state[["DW"]][["views"]])){
      codes=c(codes,
              paste0("# ", state[["DW"]][["views"]][[view_id]][["key"]]),
              state[["DW"]][["views"]][[view_id]][["code_previous"]],
              state[["DW"]][["views"]][[view_id]][["code_dw_only"]])
    }
    code = paste(codes, collapse="\n")
  }
code}
