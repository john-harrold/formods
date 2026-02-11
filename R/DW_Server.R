#'@import dplyr
#'@import rhandsontable
#'@import shiny
#'@import shinyWidgets
#'@importFrom digest digest
#'@importFrom rlang .data
#'@importFrom shinyAce aceEditor updateAceEditor

# https://getbootstrap.com/docs/3.3/components/

#'@export
#'@title Data Wrangling Server
#'@description Server function for the data wrangling module
#'@param id An ID string that corresponds with the ID used to call the modules UI elements
#'@param FM_yaml_file App configuration file with FM as main section.
#'@param MOD_yaml_file  Module configuration file with DW as main section.
#'@param deployed Boolean variable indicating whether the app is deployed or not.
#'@param react_state Variable passed to server to allow reaction outside of
#'module (\code{NULL})
#'@return DW Server object
#'@example inst/test_apps/FM_compact.R
DW_Server <- function(id,                            # nocov start
                      FM_yaml_file  = system.file(package = "formods",
                                                  "templates",
                                                  "formods.yaml"),
                      MOD_yaml_file = system.file(package = "formods",
                                                  "templates",
                                                  "DW.yaml"),
                      deployed     = FALSE,
                      react_state  = NULL) {
  moduleServer(id, function(input, output, session) {

  MOD_yaml_cont = FM_read_yaml(MOD_yaml_file)
  id_ASM = MOD_yaml_cont[["MC"]][["module"]][["depends"]][["id_ASM"]]
  id_UD  = MOD_yaml_cont[["MC"]][["module"]][["depends"]][["id_UD"]]
  id_DM  = MOD_yaml_cont[["MC"]][["module"]][["depends"]][["id_DM"]]

    #------------------------------------
    # Current DW elements
    output$hot_dw_elements = rhandsontable::renderRHandsontable({
      req(input$select_dw_element)
      # Force update on button click
      input$button_dw_add_element
      input$button_dw_new
      input$button_dw_del
      input$button_dw_copy
      input$button_dw_save
      # Force update on deletion clicks
      input$hot_dw_elements
      # Changes in view selected
      input$select_dw_views
      # Forcing a reaction to changes in other modules
      force_mod_update[["triggered"]]

      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      # Pulling out the current view
      current_view = DW_fetch_current_view(state)

      uiele = NULL

      if(state[["DW"]][["DSV"]][["hasds"]]){
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
          # Columns that are tracked but we don't want the user to see
          df[["cmd"]]                = NULL
          df[["res_obj"]]            = NULL
          df[["res_obj_mod_id"]]     = NULL
          df[["res_obj_DSchecksum"]] = NULL
          df[["Key"]]                = NULL

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
                    width = 40, 
                    valign='htCenter',
                    renderer = "
                 function(instance, td, row, col, prop, value, cellProperties) {
                   Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
                   return td;
                 }") |>
            hot_col("Action" ,      readOnly = TRUE) |>
            hot_col("Description" , readOnly = TRUE, width=400) |>
            hot_col("Status" ,      readOnly = TRUE, valign='htCenter')

          uiele = hot
        }
      }

      uiele

    })
    #------------------------------------
    # DSV dataset views
    # This creates the selection UI
    output$ui_dw_sources = renderUI({
      state = DW_fetch_state(id             = id,
                             input          = input,
                             session        = session,
                             FM_yaml_file   = FM_yaml_file,
                             MOD_yaml_file  = MOD_yaml_file,
                             react_state    = react_state)

      choicesOpt = NULL
      uiele =
        shinyWidgets::pickerInput(
          selected   = "PH",
          inputId    = NS(id, "select_current_source"),
          label      = state[["MC"]][["labels"]][["select_current_source"]],
          choices    = c("PH"),
          width      = state[["MC"]][["formatting"]][["select_current_source"]][["width"]],
          choicesOpt = choicesOpt)

      uiele})
    #------------------------------------
    # This forces the dataset view selection to update
    observe({
      input$button_element_add
      input$select_dw_views

      # Forcing a reaction to changes in other modules
      force_mod_update[["triggered"]]

      state = DW_fetch_state(id             = id,
                             input          = input,
                             session        = session,
                             FM_yaml_file   = FM_yaml_file,
                             MOD_yaml_file  = MOD_yaml_file,
                             react_state    = react_state)

      current_view = DW_fetch_current_view(state)

      # If this is triggered before datasets have been loaded the state will
      # be bad:
      # JMH TODO: check the dsviews stuff
      if(state[["DW"]][["isgood"]]){

        # Pulling out the data set views catalog
        ds_catalog = state[["DW"]][["DSV"]][["catalog"]]
        state[["DW"]][["DSV"]][["catalog"]][["object"]][1]

        # Pulling out the select choices list:
        all_choices = state[["DW"]][["DSV"]][["choices"]]

        csi_found = FALSE
        if(!is.null(current_view[["ds_source_id"]])){
         if(current_view[["ds_source_id"]] %in% ds_catalog[["object"]]){
            current_source_id = current_view[["ds_source_id"]]
            csi_found = TRUE
          }
        }

        if(!csi_found){
          current_source_id = ds_catalog[["object"]][1]
          FM_le(state, paste0("ui_fg_curr_views: dataset view missing."   ))
          FM_le(state, paste0("ds_view_key: ", current_view[["key"]]       ))
          FM_le(state, paste0("switching to view:", current_source_id ))
        }

        # Using choices grouped by module
        choices = all_choices[["grouped"]]

        choicesOpt = NULL
        shinyWidgets::updatePickerInput(
          session    = session,
          selected   = current_source_id,
          inputId    = "select_current_source",
          choices    = choices,
          choicesOpt = choicesOpt)
      }
    })
    #------------------------------------
    # Generated data wrangling code
    observe({
      # Forcing a reaction to changes in other modules
      force_mod_update[["triggered"]]
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
                             react_state     = react_state)

      uiele = NULL

      if(state[["DW"]][["DSV"]][["hasds"]]){

        # Pulling out the current view
        current_view = DW_fetch_current_view(state)

        if(is.null(current_view[["elements_table"]])){
          #uiele = "# No data wragling elements defined yet!"
          uiele = state[["MC"]][["errors"]][["no_code"]]
        } else {
          uiele = current_view[["code"]]
          # Adding the preamble to load necessary packages
          mod_deps = FM_fetch_deps(state = state, session = session)
          if("package_code" %in% names(mod_deps)){
            uiele = paste0(c(mod_deps$package_code, "", uiele), collapse="\n")
          }
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
    # Generated data wrangling code
    observeEvent(input$button_dw_clip, {
      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      # This is all conditional on the whether clipr is installed $
      # and if the app isn't deployed
      if((system.file(package="clipr") != "") &
         !deployed){

        if(state[["DW"]][["DSV"]][["hasds"]]){

          # Pulling out the current view
          current_view = DW_fetch_current_view(state)

          if(is.null(current_view[["elements_table"]])){
            uiele = "# No data wragling elements defined yet!"
          } else {
            uiele = current_view[["code"]]
          }

          clipr::write_clip(uiele)

        }
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
      input$hot_dw_elements
      # Forcing a reaction to changes in other modules
      force_mod_update[["triggered"]]

      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      view_ids = names(state[["DW"]][["views"]])
      uiele = NULL

      if(length(view_ids)> 0){

        # Building up the vew choices
        current_view = DW_fetch_current_view(state)
        choices = c()
        cnames  = c()
        content = c()
        HL_COLOR = state[["yaml"]][["FM"]][["ui"]][["color_orange"]]
        for(view_id in view_ids){

          if(state[["DW"]][["views"]][[view_id]][["isgood"]]){
            content = c(content,
                paste0(
                 "<div>",
                  state[["DW"]][["views"]][[view_id]][["key"]],
                  "</div>"))
          } else {
            content = c(content,
                paste0(
                 "<div style='font-style:bold; background: ", HL_COLOR ,"; color: black;'>",
                  state[["DW"]][["views"]][[view_id]][["key"]],
                  "</div>"))

          }

          cnames  = c(cnames, state[["dw"]][["views"]][[view_id]][["key"]])
          choices = c(choices, view_id)
        }

        names(choices) = cnames
        choicesOpt = list( content = content)

        uiele = tagList(uiele,
          shinyWidgets::pickerInput(
             label      = state[["MC"]][["labels"]][["select_dw_views"]],
             inputId    = NS(id, "select_dw_views"),
             selected   = current_view[["id"]],
             choices    = choices,
             width      = state[["MC"]][["formatting"]][["select_dw_views"]][["width"]],
             inline     = TRUE,
             choicesOpt = choicesOpt))

        uiele = formods::FM_add_ui_tooltip(state, uiele,
                 tooltip     = state[["MC"]][["formatting"]][["select_dw_views"]][["tooltip"]],
                 position    = state[["MC"]][["formatting"]][["select_dw_views"]][["tooltip_position"]])




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
      force_mod_update[["triggered"]]

      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
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
      input$button_dw_save
      # Update when they delete elements as well
      input$hot_dw_elements
      #req(input$select_dw_element)
      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)
      uiele = NULL
      if(state[["DW"]][["DSV"]][["hasds"]]){
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
                             react_state     = react_state)

      if(state[["DW"]][["DSV"]][["hasds"]]){
        if(state[["DW"]][["isgood"]]){
          # Current final dataset
          WDS = DW_fetch_current_view(state)[["WDS"]]
          # Columns in that dataset
          dscols = names(WDS)

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
          if(state[["DW"]][["ui"]][["select_dw_element"]] == "merge" ){
            uiele = tagList(
              htmlOutput(NS(id, "ui_dw_fds_merge"  )))
          }
          if(state[["DW"]][["ui"]][["select_dw_element"]] == "sort" ){
            uiele = tagList(
              htmlOutput(NS(id, "ui_dw_fds_sort"  )))
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
      input$select_dw_views
      input$hot_dw_elements
      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      req(input$select_dw_element)
      uiele = NULL
      if(state[["DW"]][["isgood"]]){
        # Current final dataset
        WDS = DW_fetch_current_view(state)[["WDS"]]
        # Columns in that dataset
        dscols = names(WDS)

        # Pulling out column formatting information.
        hfmt        = FM_fetch_data_format(WDS, state)
        sel_subtext = as.vector(unlist( hfmt[["col_subtext"]]))
        sel_style   = rep("", length(dscols))



        uiele = tagList(
          div(style = "display: flex;",
              pickerInput(
                inputId  = NS(id, "select_fds_rename_column"),
                label    = NULL,
                width    = 200,
                choices  = dscols,
                choicesOpt =list(
                  style   = sel_style,
                  subtext = sel_subtext),
                      options  = list(
                  size  = state[["yaml"]][["FM"]][["ui"]][["select_size"]],
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
      input$select_dw_views
      input$hot_dw_elements
      req(input$select_dw_element)
      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      req(input$select_dw_element)
      uiele = NULL
      if(state[["DW"]][["isgood"]]){
        # Current final dataset
        WDS = DW_fetch_current_view(state)[["WDS"]]
        # Columns in that dataset
        dscols = names(WDS)

        # Pulling out column formatting information.
        hfmt        = FM_fetch_data_format(WDS, state)
        sel_subtext = as.vector(unlist( hfmt[["col_subtext"]]))
        sel_style   = rep("", length(dscols))

        uiele = tagList(
          div(style = "display: flex;",
              pickerInput(
                inputId  = NS(id, "select_fds_group_column"),
                label    = NULL,
                width    = 200,
                choices  = dscols,
                choicesOpt =list(
                  style   = sel_style,
                  subtext = sel_subtext),
                multiple = TRUE,
                options  = list(
                  size  = state[["yaml"]][["FM"]][["ui"]][["select_size"]],
                  title = state[["MC"]][["labels"]][["uknown_action"]])
              )
          ))

      }
      uiele
    })
    #------------------------------------
    output$ui_dw_fds_wider = renderUI({
      input$select_dw_views
      input$hot_dw_elements
      req(input$select_dw_element)
      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      req(input$select_dw_element)
      uiele = NULL
      if(state[["DW"]][["isgood"]]){
        # Current final dataset
        WDS = DW_fetch_current_view(state)[["WDS"]]
        # Columns in that dataset
        dscols = names(WDS)

        # Pulling out column formatting information.
        hfmt        = FM_fetch_data_format(WDS, state)
        sel_subtext = as.vector(unlist( hfmt[["col_subtext"]]))
        sel_style   = rep("", length(dscols))

        uiele = tagList(
          div(style = "display: flex;",
              pickerInput(
                inputId  = NS(id, "select_fds_wider_names"),
                label    = NULL,
                width    = 200,
                choices  = dscols,
                choicesOpt =list(
                  style   = sel_style,
                  subtext = sel_subtext),
                multiple = FALSE,
                options  = list(
                  size  = state[["yaml"]][["FM"]][["ui"]][["select_size"]],
                  title = state[["MC"]][["labels"]][["fds_wider_names"]])
              ),
              pickerInput(
                inputId  = NS(id, "select_fds_wider_values"),
                label    = NULL,
                width    = 200,
                choices  = dscols,
                choicesOpt =list(
                  style   = sel_style,
                  subtext = sel_subtext),
                multiple = FALSE,
                options  = list(
                  size  = state[["yaml"]][["FM"]][["ui"]][["select_size"]],
                  title = state[["MC"]][["labels"]][["fds_wider_values"]])
              )
          ))

      }
      uiele
    })
    #------------------------------------
    output$ui_dw_fds_longer = renderUI({
      input$select_dw_views
      input$hot_dw_elements
      req(input$select_dw_element)
      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      req(input$select_dw_element)
      uiele = NULL
      if(state[["DW"]][["isgood"]]){
        # Current final dataset
        WDS = DW_fetch_current_view(state)[["WDS"]]
        # Columns in that dataset
        dscols = names(WDS)

        # Pulling out column formatting information.
        hfmt        = FM_fetch_data_format(WDS, state)
        sel_subtext = as.vector(unlist( hfmt[["col_subtext"]]))
        sel_style   = rep("", length(dscols))

        uiele = tagList(
          div(style = "display: flex;",
              pickerInput(
                inputId  = NS(id, "select_fds_longer_column"),
                label    = NULL,
                width    = 200,
                choices  = dscols,
                choicesOpt =list(
                  style   = sel_style,
                  subtext = sel_subtext),
                multiple = TRUE,
                options  = list(
                  size  = state[["yaml"]][["FM"]][["ui"]][["select_size"]],
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
    # Sorting ui
    output$ui_dw_fds_sort = renderUI({
      input$select_dw_views
      input$hot_dw_elements
      input$button_dw_save
      input$select_fds_sort_column
      req(input$select_dw_element)
      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      req(input$select_dw_element)
      uiele = NULL
      if(state[["DW"]][["isgood"]]){
        # Current final dataset
        WDS = DW_fetch_current_view(state)[["WDS"]]
        # Columns in that dataset
        dscols = names(WDS)

        cnames = c(
          stringr::str_replace_all(string = state[["MC"]][["formatting"]][["select_fds_sort_column"]][["names_asc"]],
                                   pattern = "===COL===", replacement = dscols),
          stringr::str_replace_all(string = state[["MC"]][["formatting"]][["select_fds_sort_column"]][["names_des"]],
                                   pattern = "===COL===", replacement = dscols))

        # Putting everything in a data frame so we can sort it by cnames
        ch_df = data.frame(
          choices = c(paste0(dscols, ":", "ascending"), paste0(dscols, ":", "descending")),
          cnames  = cnames) |>
          dplyr::arrange(.data[["cnames"]])

        choices = ch_df[["choices"]]
        names(choices) = ch_df[["cnames"]]
        selected = state[["DW"]][["sort_cols"]]

        if(!(any(selected %in% choices))){
          selected = NULL
        }

        uiele = tagList(
          div(style = "display: flex;",
            slimSelectInput(
              inputId       = NS(id, "select_fds_sort_column"),
              label         = NULL,
              selected      = state[["DW"]][["sort_cols"]],
              placeholder   = state[["MC"]][["labels"]][["fds_sort_column"]],
              allowDeselect = FALSE,
              search        = TRUE,
              choices       = choices,
              keepOrder     = TRUE,
              multiple      = TRUE,
              width         = state[["MC"]][["formatting"]][["select_fds_sort_column"]][["width"]]
            )
          )
        )
      }
   uiele})
    #------------------------------------
    # Merging ui row
    output$ui_dw_fds_merge = renderUI({
      input$button_dw_new
      input$button_dw_save
      input$button_dw_copy
      input$button_dw_del
      input$select_dw_views
      input$hot_dw_elements
      req(input$select_dw_element)
      req(input$select_dw_element)
      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      uiele = NULL
      if(state[["DW"]][["isgood"]]){
        uiele = "Merge row"
        uiele = tagList(
          div(style="display:inline-block", htmlOutput(NS(id, "ui_dw_fds_merge_resource"))),
          div(style="display:inline-block", htmlOutput(NS(id, "ui_dw_fds_merge_method"))),
          div(style="display:inline-block", htmlOutput(NS(id, "ui_dw_fds_merge_condition"))),
          tags$br(),
          htmlOutput(NS(id, "ui_dw_fds_merge_join_by_elements")),
          htmlOutput(NS(id, "ui_dw_fds_merge_join_by_select")),
          verbatimTextOutput(NS(id, "ui_dw_fds_merge_precheck"))
        )
      }
    uiele})
    #------------------------------------
    output$ui_dw_fds_merge_resource = renderUI({
      input$button_dw_new
      input$button_dw_save
      input$button_dw_copy
      input$button_dw_del
      input$select_dw_views
      input$hot_dw_elements
      req(input$select_dw_element)
      req(input$select_dw_element)
      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      current_view      = DW_fetch_current_view(state)
      avail_ds          = DW_fetch_available_sources(state = state, session=session, dw_view = current_view)

      if(avail_ds[["isgood"]]){
        choices = avail_ds[["choices"]][["grouped"]]
      } else {
        choices = NULL
        FM_le(state, "DW_fetch_available_sources() failed in ui_dw_fds_merge_resource", entry_type="danger")
        if(!is.null(avail_ds[["msgs"]])){
          FM_le(state, avail_ds[["msgs"]], entry_type="danger")
        }
      }


      uiele =
        div(style = "display: flex;",
        shinyWidgets::pickerInput(
          label       = NULL,
          selected    = NULL,
          multiple    = TRUE,
          inputId     = NS(id, "select_fds_merge_source"),
          options = pickerOptions(
            maxOptions = 1,
            title      = state[["MC"]][["labels"]][["fds_merge_source"]]),
          choices     = choices,
          width       = state[["MC"]][["formatting"]][["select_fds_merge_source"]][["width"]])
        )

        uiele = FM_add_ui_tooltip(state, uiele,
                 tooltip     = state[["MC"]][["formatting"]][["select_fds_merge_source"]][["tooltip"]],
                 position    = state[["MC"]][["formatting"]][["select_fds_merge_source"]][["tooltip_position"]])

    uiele})
    #------------------------------------
    output$ui_dw_fds_merge_method   = renderUI({
      input$button_dw_new
      input$button_dw_save
      input$button_dw_copy
      input$button_dw_del
      input$select_dw_views
      input$hot_dw_elements
      req(input$select_dw_element)
      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      merge_methods = state[["MC"]][["formatting"]][["select_fds_merge_method"]][["methods"]]

      choices = c()
      content = c()
      for(mchoice in names(merge_methods)){
        choices = c(choices, mchoice)
        content = c(content,
          HTML(paste0('<div style="width: 400px; white-space: normal;">
                       <b>', merge_methods[[mchoice]][["cname"]], '</b>:
                       <span style="color: gray;">',merge_methods[[mchoice]][["subtext"]] ,'</span>
                      </div>'))
        )
      }
      uiele =
        div(style = "display: flex;",
        shinyWidgets::pickerInput(
          inputId     = NS(id, "select_fds_merge_method"),
          label       = NULL,
          multiple    = TRUE,
          selected    = NULL,
          options = pickerOptions(
            html       = TRUE,
            maxOptions = 1,
            title      = state[["MC"]][["labels"]][["fds_merge_method"]]),
          choicesOpt  = list(
            content = content),
          choices     = choices,
          width       = state[["MC"]][["formatting"]][["select_fds_merge_method"]][["width"]])
        )

        uiele = FM_add_ui_tooltip(state, uiele,
                 tooltip     = state[["MC"]][["formatting"]][["select_fds_merge_method"]][["tooltip"]],
                 position    = state[["MC"]][["formatting"]][["select_fds_merge_method"]][["tooltip_position"]])

    uiele})
    #------------------------------------
    output$ui_dw_fds_merge_condition   = renderUI({
      input$button_dw_new
      input$button_dw_save
      input$button_dw_copy
      input$button_dw_del
      input$select_dw_views
      input$hot_dw_elements
      req(input$select_dw_element)
      req(input$select_fds_merge_method)
      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      valid_methods = state[["MC"]][["formatting"]][["select_fds_merge_condition"]][["valid_methods"]]
      merge_method = state[["DW"]][["ui"]][["select_fds_merge_method"]]
      uiele = NULL
      if(merge_method %in% valid_methods){
        cnames  = c()
        choices = c()
        subtext = c()
        for(choice in names(state[["MC"]][["formatting"]][["select_fds_merge_condition"]][["conditions"]])){
          choices = c(choices, choice)
          cnames = c(cnames, state[["MC"]][["formatting"]][["select_fds_merge_condition"]][["conditions"]][[choice]][["cname"]])
          subtext = c(subtext, state[["MC"]][["formatting"]][["select_fds_merge_condition"]][["conditions"]][[choice]][["subtext"]])
        }
        names(choices) = cnames

        uiele =
          div(style = "display: flex;",
          shinyWidgets::pickerInput(
            inputId     = NS(id, "select_fds_merge_condition"),
            label       = NULL,
            multiple    = TRUE,
            selected    = NULL,
            options = pickerOptions(
              maxOptions = 1,
              title      = state[["MC"]][["labels"]][["fds_merge_condition"]]),
            choicesOpt  = list(
              style   = rep("", length(choices)),
              subtext = subtext),
            choices     = choices,
            width       = state[["MC"]][["formatting"]][["select_fds_merge_condition"]][["width"]])
          )

          uiele = FM_add_ui_tooltip(state, uiele,
                   tooltip     = state[["MC"]][["formatting"]][["select_fds_merge_condition"]][["tooltip"]],
                   position    = state[["MC"]][["formatting"]][["select_fds_merge_condition"]][["tooltip_position"]])
      }

    uiele})
    #------------------------------------
    output$ui_dw_fds_merge_precheck              = renderText({
     #input$button_dw_new
     #input$button_dw_save
     #input$button_dw_copy
     #input$button_dw_del
     #input$select_dw_views
     #input$hot_dw_elements
      req(input$select_dw_element)
      req(input$select_fds_merge_method)
      #req(input$select_fds_merge_condition)
      req(input$select_fds_merge_source)
      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      # Here we add any logic to check user input related to
      # mergin before they do anything with it
      uiele = NULL

      if(state[["DW"]][["ui"]][["select_fds_merge_method"]] %in%
         c("rbind", "cbind")){
         uiele   = c()

        # Pulling out the current view
        current_view = DW_fetch_current_view(state)

        # Current value of the data view
        WDS     = current_view[["WDS"]]

        # New data source object name
        NDS_obj = state[["DW"]][["ui"]][["select_fds_merge_source"]]

        NDS_meta = DW_fetch_obj_ds_meta(
                     state   = state, 
                     session = session, 
                     dw_view = current_view, 
                     ds_obj  = NDS_obj)
        
        NDS = NDS_meta$DS
        NDS_str         = paste0(NDS_meta[["res_row"]][["label"]], " (", NDS_obj,")")
        WDS_str         = paste0(current_view[["key"]], " (", current_view[["view_ds_object_name"]],")")

        if(state[["DW"]][["ui"]][["select_fds_merge_method"]] =="cbind"){
          if(nrow(WDS) != nrow(NDS)){
            tmp_mismatch= state[["MC"]][["errors"]][["cbind_row_mismatch"]]
            tmp_mismatch= stringr::str_replace_all(string=tmp_mismatch, pattern="===DS1===", replacement=WDS_str)
            tmp_mismatch= stringr::str_replace_all(string=tmp_mismatch, pattern="===DS2===", replacement=NDS_str)
            tmp_mismatch= stringr::str_replace_all(string=tmp_mismatch, pattern="===NR1===", replacement=as.character(nrow(WDS)))
            tmp_mismatch= stringr::str_replace_all(string=tmp_mismatch, pattern="===NR2===", replacement=as.character(nrow(NDS)))
            uiele = tmp_mismatch
          }
        }
        if(state[["DW"]][["ui"]][["select_fds_merge_method"]] =="rbind"){
         
          NDS_missing     = names(WDS)[!(names(WDS) %in% names(NDS))]
          WDS_missing     = names(NDS)[!(names(NDS) %in% names(WDS))]

         
          if(length(WDS_missing) > 0){
            tmp_missing = state[["MC"]][["errors"]][["rbind_col_missing_details"]]
            WDS_missing_str = paste0(WDS_missing, collapse=", ")
            WDS_missing_str = stringr::str_wrap(string=WDS_missing_str, width=50, indent=2, exdent=2)
            tmp_missing = stringr::str_replace_all(string=tmp_missing, pattern="===DS2===", replacement=WDS_str)
            tmp_missing = stringr::str_replace_all(string=tmp_missing, pattern="===DS1===", replacement=NDS_str)
            tmp_missing = stringr::str_replace_all(string=tmp_missing, pattern="===COLS===", replacement=WDS_missing_str)
            uiele = c(uiele, tmp_missing)
          }
          if(length(NDS_missing) > 0){
            tmp_missing = state[["MC"]][["errors"]][["rbind_col_missing_details"]]
            NDS_missing_str = paste0(NDS_missing, collapse=", ")
            NDS_missing_str = stringr::str_wrap(string=NDS_missing_str, width=50, indent=2, exdent=2)
            tmp_missing = stringr::str_replace_all(string=tmp_missing, pattern="===DS2===", replacement=NDS_str)
            tmp_missing = stringr::str_replace_all(string=tmp_missing, pattern="===DS1===", replacement=WDS_str)
            tmp_missing = stringr::str_replace_all(string=tmp_missing, pattern="===COLS===", replacement=NDS_missing_str)
            uiele = c(uiele, tmp_missing)
          }
         
          if(!is.null(uiele)){
            uiele = c(state[["MC"]][["errors"]][["rbind_col_missing_head"]], uiele)
          }
        }

        uiele = paste0(uiele, collapse="\n")
      }
    uiele})
    #------------------------------------
    output$ui_dw_fds_merge_join_by_elements      = renderUI({
      input$button_dw_new
      input$button_dw_save
      input$button_dw_copy
      input$button_dw_del
      input$select_dw_views
      input$hot_dw_elements
      req(input$select_dw_element)
      req(input$select_fds_merge_method)
      req(input$select_fds_merge_condition)
      req(input$select_fds_merge_source)
      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      # Pulling out the current view
      current_view = DW_fetch_current_view(state)

      uiele = NULL
      merge_method = state[["DW"]][["ui"]][["select_fds_merge_method"]]
      valid_methods = state[["MC"]][["formatting"]][["select_fds_merge_condition"]][["valid_methods"]]
      if(merge_method %in% valid_methods){
        merge_condition = state[["DW"]][["ui"]][["select_fds_merge_condition"]]
        conditions      = state[["MC"]][["formatting"]][["select_fds_merge_condition"]][["conditions"]]

        # This creates the uiele for the button
        uiele_btn_add_rel = tagList(
          actionBttn(
            inputId  = NS(id, "button_dw_add_merge_relationship"),
             label   = state[["MC"]][["labels"]][["add_merge_relationship"]],
             icon    = icon("plus-sign", lib="glyphicon"),
             size    = state[["MC"]][["formatting"]][["button_dw_add_merge_relationship"]][["size"]],
             block   = state[["MC"]][["formatting"]][["button_dw_add_merge_relationship"]][["block"]],
             color   = "success",
             style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]]
             ))
        tmp_style = paste0("width:",state[["MC"]][["formatting"]][["button_dw_add_merge_relationship"]][["width"]])
        uiele_btn_add_rel = tags$div(style=tmp_style, uiele_btn_add_rel)
        uiele_btn_add_rel = FM_add_ui_tooltip(state, uiele_btn_add_rel,
                 tooltip     = state[["MC"]][["formatting"]][["button_dw_add_merge_relationship"]][["tooltip"]],
                 position    = state[["MC"]][["formatting"]][["button_dw_add_merge_relationship"]][["tooltip_position"]])


        #-------------------------------------------------------------------
        # This will pull out the metadata for the current data view (WDS) and
        # New data source (NDS)
        WDS = current_view[["WDS"]]
        # Columns in that dataset
        dscols_dv = names(WDS)

        # Pulling out column formatting information.
        hfmt        = FM_fetch_data_format(WDS, state)
        sel_subtext_dv = as.vector(unlist( hfmt[["col_subtext"]]))
        #sel_style_dv   = rep("", length(dscols_dv))

        new_source_id = state[["DW"]][["ui"]][["select_fds_merge_source"]]

        # We need the new dataset based on the object selected as the merge
        # source. This can be either seomthing from a previous/upstream module
        # or a previously defined data view. This will extract that
        # accordingly.
        ds_meta = DW_fetch_obj_ds_meta(state   = state, 
                                       session = session, 
                                       dw_view = current_view,
                                       ds_obj  = new_source_id)

        if(!ds_meta[["isgood"]]){
          FM_le(state, ds_meta[["msgs"]])
        }

        NDS = ds_meta[["DS"]]
        dscols_ns = names(NDS)
        hfmt        = FM_fetch_data_format(NDS, state)
        sel_subtext_ns = as.vector(unlist( hfmt[["col_subtext"]]))
        #-------------------------------------------------------------------

        # Now we construct UI elements based on the type of relationship
        # Equality, in equality, and closest inequality relationships
        if(state[["DW"]][["ui"]][["select_fds_merge_condition"]] %in%
             c("equality", "inequality", "inequality_closest")){

          uiele_dv_cols = pickerInput(
            inputId  = NS(id, "select_fds_merge_condition_dv_cols"),
            label    = NULL,
            width    = 100,
            choices  = dscols_dv,
            options  = pickerOptions(
              `live-search`=TRUE,
              size  = state[["yaml"]][["FM"]][["ui"]][["select_size"]]),
            choicesOpt =list(
              subtext = sel_subtext_dv)
          )
          uiele_dv_cols = FM_add_ui_tooltip(state, uiele_dv_cols,
                   tooltip     = state[["MC"]][["formatting"]][["select_fds_merge_condition"]][["relationships"]][["eq_ineq"]][["dv_cols"]][["tooltip"]],
                   position    = state[["MC"]][["formatting"]][["select_fds_merge_condition"]][["relationships"]][["eq_ineq"]][["dv_cols"]][["tooltip_position"]])

          uiele_ns_cols = pickerInput(
            inputId  = NS(id, "select_fds_merge_condition_ns_cols"),
            label    = NULL,
            width    = 100,
            options  = pickerOptions(
              `live-search`=TRUE,
              size  = state[["yaml"]][["FM"]][["ui"]][["select_size"]]),
            choices  = dscols_ns,
            choicesOpt =list(
              subtext = sel_subtext_ns)
          )
          uiele_ns_cols = FM_add_ui_tooltip(state, uiele_ns_cols,
                   tooltip     = state[["MC"]][["formatting"]][["select_fds_merge_condition"]][["relationships"]][["eq_ineq"]][["ns_cols"]][["tooltip"]],
                   position    = state[["MC"]][["formatting"]][["select_fds_merge_condition"]][["relationships"]][["eq_ineq"]][["ns_cols"]][["tooltip_position"]])

          if(state[["DW"]][["ui"]][["select_fds_merge_condition"]] == "equality"){
            uiele_operator = "=="
             tmp_style = paste0("font-size: 20px;justify-content: center; text-align: center; width:",
                       state[["MC"]][["formatting"]][["select_fds_merge_condition"]][["relationships"]][["eq_ineq"]][["op_width"]])
             uiele_operator = tags$div(style=tmp_style, uiele_operator)
          } else {
            choices = state[["MC"]][["formatting"]][["select_fds_merge_condition"]][["relationships"]][["eq_ineq"]][["operators"]][["choices"]]
            names(choices) = state[["MC"]][["formatting"]][["select_fds_merge_condition"]][["relationships"]][["eq_ineq"]][["operators"]][["cnames"]]
            uiele_operator = pickerInput(
              inputId    = NS(id, "select_fds_merge_condition_operator"),
              label      = NULL,
              width      = state[["MC"]][["formatting"]][["select_fds_merge_condition"]][["relationships"]][["eq_ineq"]][["op_width"]],
              choices    = choices,
              choicesOpt = NULL,
              options    = pickerOptions(
                size     = state[["yaml"]][["FM"]][["ui"]][["select_size"]]))

          }

          uiele=tagList(div(style="display:inline-block",uiele_btn_add_rel),
                        div(style="display:inline-block",uiele_dv_cols),
                        div(style="display:inline-block",uiele_operator),
                        div(style="display:inline-block",uiele_ns_cols))
        }

        # Between, within and overlap
        if(state[["DW"]][["ui"]][["select_fds_merge_condition"]] %in%
             c("between", "within", "overlaps")){

          bwo_ele = state[["MC"]][["formatting"]][["select_fds_merge_condition"]][["relationships"]][["bwo"]]

          # For the dataview the tooltips are only applicable for the lower
          # bound
          if(state[["DW"]][["ui"]][["select_fds_merge_condition"]] == "between"){
            dv_tooltip_lb          = bwo_ele[["dv_single"]][["tooltip"]]
            dv_tooltip_position_lb = bwo_ele[["dv_single"]][["tooltip_position"]]
            dv_tooltip_ub          = NULL
            dv_tooltip_position_ub = NULL
          }else{
            dv_tooltip_lb          = bwo_ele[["dv_range"]][["tooltip_lb"]]
            dv_tooltip_position_lb = bwo_ele[["dv_range"]][["tooltip_position_lb"]]
            dv_tooltip_ub          = bwo_ele[["dv_range"]][["tooltip_ub"]]
            dv_tooltip_position_ub = bwo_ele[["dv_range"]][["tooltip_position_ub"]]
          }

          ns_tooltip_lb          = bwo_ele[["ns_range"]][["tooltip_lb"]]
          ns_tooltip_position_lb = bwo_ele[["ns_range"]][["tooltip_position_lb"]]
          ns_tooltip_ub          = bwo_ele[["ns_range"]][["tooltip_ub"]]
          ns_tooltip_position_ub = bwo_ele[["ns_range"]][["tooltip_position_ub"]]

          uiele_dv_lb = NULL
          uiele_dv_ub = NULL
          uiele_ns_lb = NULL
          uiele_ns_ub = NULL


          uiele_dv_lb = pickerInput(
            inputId    = NS(id, "select_fds_merge_dv_lb"),
            label      = NULL,
            width      = bwo_ele[["range_width"]],
            choices    = dscols_dv,
            choicesOpt =list(
              subtext = sel_subtext_dv),
            options    = pickerOptions(
              size  = state[["yaml"]][["FM"]][["ui"]][["select_size"]]))
          uiele_dv_lb = FM_add_ui_tooltip(state, uiele_dv_lb,
                   tooltip     = dv_tooltip_lb ,
                   position    = dv_tooltip_position_lb)

          # We only generate the dv_ub ui element if the condition is not
          # "between"
          if(state[["DW"]][["ui"]][["select_fds_merge_condition"]] != "between"){
            uiele_dv_ub = pickerInput(
              inputId    = NS(id, "select_fds_merge_dv_ub"),
              label      = NULL,
              width      = bwo_ele[["range_width"]],
              choices    = dscols_dv,
              choicesOpt =list(
                subtext = sel_subtext_dv),
              options    = pickerOptions(
                size  = state[["yaml"]][["FM"]][["ui"]][["select_size"]]))
            uiele_dv_ub = FM_add_ui_tooltip(state, uiele_dv_ub,
                     tooltip     = dv_tooltip_ub,
                     position    = dv_tooltip_position_ub)
          }


          uiele_ns_lb = pickerInput(
            inputId    = NS(id, "select_fds_merge_ns_lb"),
            label      = NULL,
            width      = bwo_ele[["range_width"]],
            choices    = dscols_ns,
            choicesOpt =list(
              subtext = sel_subtext_ns),
            options    = pickerOptions(
                size  = state[["yaml"]][["FM"]][["ui"]][["select_size"]]))
          uiele_ns_lb = FM_add_ui_tooltip(state, uiele_ns_lb,
                   tooltip     = ns_tooltip_lb,
                   position    = ns_tooltip_position_lb)

          uiele_ns_ub = pickerInput(
            inputId  = NS(id, "select_fds_merge_ns_ub"),
            label    = NULL,
            width    = bwo_ele[["range_width"]],
            choices    = dscols_ns,
            choicesOpt =list(
              subtext = sel_subtext_ns),
            options  = pickerOptions(
              size  = state[["yaml"]][["FM"]][["ui"]][["select_size"]]))
          uiele_ns_ub = FM_add_ui_tooltip(state, uiele_ns_ub,
                   tooltip     = ns_tooltip_ub,
                   position    = ns_tooltip_position_ub)

          uiele=tagList(div(style="display:inline-block",uiele_btn_add_rel),
                        div(style="display:inline-block",uiele_dv_lb),
                        div(style="display:inline-block",uiele_dv_ub),
                        div(style="display:inline-block",uiele_ns_lb),
                        div(style="display:inline-block",uiele_ns_ub))
        }


        uiele = tagList(uiele, tags$br(),
           htmlOutput(NS(id, "ui_dw_fds_merge_join_by_relationships"))
          )
      }

    uiele})
    #------------------------------------
    output$ui_dw_fds_merge_join_by_relationships  = renderUI({
      input$button_dw_new
      input$button_dw_save
      input$button_dw_copy
      input$button_dw_del
      input$select_dw_views
      input$hot_dw_elements
      req(input$select_dw_element)
      req(input$select_fds_merge_method)
      req(input$select_fds_merge_condition)
      req(input$select_fds_merge_source)
      input$button_dw_add_merge_relationship
      input$select_fds_merge_relationships

      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)
      uiele = NULL
      merge_method = state[["DW"]][["ui"]][["select_fds_merge_method"]]
      valid_methods = state[["MC"]][["formatting"]][["select_fds_merge_condition"]][["valid_methods"]]
      if(merge_method %in% valid_methods){
        uiele = paste0("merge relations: ", state[["DW"]][["ui"]][["button_dw_add_merge_relationship"]])

        if(length(state[["DW"]][["merge_rels"]]) == 0){
          uiele = tags$b(state[["MC"]][["formatting"]][["select_fds_merge_relationships"]][["no_rels"]])
        } else {
          choices = names(state[["DW"]][["merge_rels"]])
          uiele =
           slimSelectInput(
             inputId       = NS(id, "select_fds_merge_relationships"),
             label         = NULL,
             selected      = choices,
             choices       = choices,
             keepOrder     = TRUE,
             multiple      = TRUE,
             search        = FALSE,
             choicesOpt    = NULL,
             options       = list(
               size  = 1),
             width         = state[["MC"]][["formatting"]][["select_fds_merge_relationships"]][["width"]]
           )
          uiele = formods::FM_add_ui_tooltip(state, uiele,
                   tooltip     = state[["MC"]][["formatting"]][["select_fds_merge_relationships"]][["tooltip"]],
                   position    = state[["MC"]][["formatting"]][["select_fds_merge_relationships"]][["tooltip_position"]])

        }
      }

    uiele})
    #------------------------------------
    # input$select_fds_merge_source
    # input$select_fds_merge_method
    #------------------------------------
    output$ui_dw_fds_select = renderUI({
      input$select_dw_views
      input$hot_dw_elements
      req(input$select_dw_element)
      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      req(input$select_dw_element)
      uiele = NULL
      if(state[["DW"]][["isgood"]]){
        # Current final dataset
        WDS = DW_fetch_current_view(state)[["WDS"]]
        # Columns in that dataset
        dscols = names(WDS)

        # Pulling out column formatting information.
        hfmt        = FM_fetch_data_format(WDS, state)
        sel_subtext = as.vector(unlist( hfmt[["col_subtext"]]))
        sel_style   = rep("", length(dscols))

        uiele = tagList(
          div(style = "display: flex;",
              pickerInput(
                inputId  = NS(id, "select_fds_select_column"),
                label    = NULL,
                width    = 200,
                choices  = dscols,
                choicesOpt =list(
                  style   = sel_style,
                  subtext = sel_subtext),
                multiple = TRUE,
                options  = list(
                  size  = state[["yaml"]][["FM"]][["ui"]][["select_size"]],
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
      force_mod_update[["triggered"]]
      input$select_dw_views
      input$hot_dw_elements
      req(input$select_dw_element)

      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      req(input$select_dw_element)
      uiele = NULL
      if(state[["DW"]][["isgood"]]){
        # Current final dataset
        WDS = DW_fetch_current_view(state)[["WDS"]]
        # Columns in that dataset
        dscols = names(WDS)

        # Pulling out column formatting information.
        hfmt        = FM_fetch_data_format(WDS, state)
        sel_subtext = as.vector(unlist( hfmt[["col_subtext"]]))
        sel_style   = rep("", length(dscols))

        uiele = pickerInput(
          inputId  = NS(id, "select_fds_filter_column"),
          label    = NULL,
          width    = 200,
          choices  = dscols,
          choicesOpt =list(
            style   = sel_style,
            subtext = sel_subtext),
          options  = list(
            size  = state[["yaml"]][["FM"]][["ui"]][["select_size"]],
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
    #------------------------------------
    output$ui_dw_fds_filter_rhs = renderUI({
      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
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

          choices  = FM_pretty_sort(unique(unfactor((WDS[[filter_col]]))))

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
                             react_state     = react_state)


      if(state[["DW"]][["DSV"]][["hasds"]]){
        uiele = tagList()
        choicesOpt = list(
          subtext = c(),
          icon    = c(),
          lib     = c()
        )
        cnames    = c()
        choices   = c()

        # Building the actions based on the information in the DW yaml file:
        for(aname  in names(state[["MC"]][["actions"]])){
          choicesOpt[["subtext"]] = c(choicesOpt[["subtext"]], state[["MC"]][["actions"]][[aname]][["subtext"]])
          choicesOpt[["icon"]]    = c(choicesOpt[["icon"]],    state[["MC"]][["actions"]][[aname]][["icon"]])
          choicesOpt[["lib"]]     = c(choicesOpt[["lib"]],     state[["MC"]][["actions"]][[aname]][["lib"]])
          cnames                  = c(cnames,                  state[["MC"]][["actions"]][[aname]][["choice"]])
          choices                 = c(choices,                 aname)
        }

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
      force_mod_update[["triggered"]]
      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      if(state[["DW"]][["DSV"]][["hasds"]]){
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

        uiele = FM_add_ui_tooltip(state, uiele,
                 tooltip     = state[["MC"]][["formatting"]][["button_dw_new"]][["tooltip"]],
                 position    = state[["MC"]][["formatting"]][["button_dw_new"]][["tooltip_position"]])

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

        uiele = FM_add_ui_tooltip(state, uiele,
                 tooltip     = state[["MC"]][["formatting"]][["button_dw_save"]][["tooltip"]],
                 position    = state[["MC"]][["formatting"]][["button_dw_save"]][["tooltip_position"]])
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

        uiele = FM_add_ui_tooltip(state, uiele,
                 tooltip     = state[["MC"]][["formatting"]][["button_dw_copy"]][["tooltip"]],
                 position    = state[["MC"]][["formatting"]][["button_dw_copy"]][["tooltip_position"]])
      }
      uiele})
    #------------------------------------
    output$ui_dw_clip_code  = renderUI({
      input$button_dw_clip
      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      # This is a suggest, so we only generate this button conditionally
      uiele = NULL
      if((system.file(package="clipr") != "") &
         !deployed){
        uiele = actionBttn(
                  inputId = NS(id, "button_dw_clip"),
                  label   = state[["MC"]][["labels"]][["clip_dw"]],
                  style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
                  size    = state[["MC"]][["formatting"]][["button_dw_clip"]][["size"]],
                  block   = state[["MC"]][["formatting"]][["button_dw_clip"]][["block"]],
                  color   = "royal",
                  icon    = icon("clipboard", lib="font-awesome"))

        uiele = FM_add_ui_tooltip(state, uiele,
                 tooltip     = state[["MC"]][["formatting"]][["button_dw_clip"]][["tooltip"]],
                 position    = state[["MC"]][["formatting"]][["button_dw_clip"]][["tooltip_position"]])
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

        uiele = FM_add_ui_tooltip(state, uiele,
                 tooltip     = state[["MC"]][["formatting"]][["button_dw_del"]][["tooltip"]],
                 position    = state[["MC"]][["formatting"]][["button_dw_del"]][["tooltip_position"]])
      }
      uiele})
    #------------------------------------
    # table preview of the data
    output$hot_data_preview =  rhandsontable::renderRHandsontable({
      # Forcing a reaction to changes in other modules
      force_mod_update[["triggered"]]
      # Triggering rebuilding of the table
      input$button_dw_add_element
      input$button_dw_new
      input$button_dw_del
      input$button_dw_copy
      input$button_dw_save
      # Force update on deletion clicks
      input$hot_dw_elements
      # Force update when the view is changed
      input$select_dw_views
      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      df = NULL

      if(state[["DW"]][["DSV"]][["hasds"]]){
        df = DW_fetch_current_view(state)[["WDS"]]
      }

     if(is.null(df)){
       df = data.frame("No_Data"= state[["MC"]][["labels"]][["no_dataset"]])
     }

     # Pulling out column header formatting information.
     hfmt = FM_fetch_data_format(df, state)

     uiele = rhandsontable::rhandsontable(
       df,
       width      = state[["MC"]][["formatting"]][["preview"]][["width"]],
       height     = state[["MC"]][["formatting"]][["preview"]][["height"]],
       colHeaders = as.vector(unlist(hfmt[["col_heads"]])),
       rowHeaders = NULL
       )

    uiele})
    #------------------------------------
    # Creates the ui for the compact view of the module
    output$DW_ui_compact  =  renderUI({
      # Forcing a reaction to changes in other modules
      force_mod_update[["triggered"]]
      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      current_view = DW_fetch_current_view(state)

      if(is.null(current_view[["WDS"]])){
        uiele = state[["MC"]][["labels"]][["no_dataset"]]
      } else {
        uiele_code_button = NULL
        # Generating code button if enabled
        if( state[["MC"]][["compact"]][["code"]]){
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
             tooltip = shinyWidgets::tooltipOptions(title = state[["MC"]][["tooltips"]][["show_code"]]))
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
           tooltip = shinyWidgets::tooltipOptions(title = state[["MC"]][["tooltips"]][["dw_elements"]]))
        )

        uiele = tagList(
          div(style="display:inline-block", htmlOutput(NS(id, "ui_dw_views"))),
          div(style="display:inline-block", htmlOutput(NS(id, "ui_dw_key"))),
          div(style="display:inline-block", htmlOutput(NS(id, "ui_dw_sources"))),
          tags$br(),
          verbatimTextOutput(NS(id, "ui_dw_new_element_msg"))
        )

        # We only show the clip button if it's enabled
        uiele_clip_button = NULL
        if(state[["MC"]][["compact"]][["clip"]]){
          uiele_clip_button = htmlOutput(NS(id, "ui_dw_clip_code"))
        }

        uiele_buttons_right = tagList(
                 tags$style(".btn-custom-dw {width: 100px;}"),
                 div(style="display:inline-block;vertical-align:top",
                 uiele_dw_elements_button,
                 uiele_code_button,
                 uiele_clip_button,
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
            tags$br(),
            verbatimTextOutput(NS(id, "ui_dw_msg")),
            tags$br(),
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
          htmlOutput(NS(id, "ui_dw_new_element_row"))
        )
      }

      uiele
    })
    #------------------------------------
    toNotify <- reactive({
      list(input$button_dw_add_element,
           input$button_dw_add_merge_relationship,
           input$button_dw_save,
           input$hot_dw_elements,
           react_state[[id_UD]],
           react_state[[id_DM]])
    })
    observeEvent(toNotify(), {
      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      # Triggering optional notifications
      notify_res =
      FM_notify(state = state,
       session     = session)
    })
    # This will be used to trigger a response when the dependent modules have
    # changed
    force_mod_update = reactiveValues()
    if(!is.null(react_state)){
      observe({
        react_state[[id_UD]]
        react_state[[id_DM]]
        react_state[[id_ASM]]

        state = DW_fetch_state(id             = id,
                               input          = input,
                               session        = session,
                               FM_yaml_file   = FM_yaml_file,
                               MOD_yaml_file  = MOD_yaml_file,
                               react_state    = react_state)

        FM_le(state, "upstream modules forcing update")
        force_mod_update[["triggered"]] = format(Sys.time(), "%Y-%m-%d %H:%M:%OS3")
      }, priority = 100)

      # Here we list the ui inputs that will result in a state change:
      toListen <- reactive({
        list(input$button_dw_add_element,
             input$select_dw_views,
             input$button_dw_new,
             input$button_dw_del,
             input$button_dw_copy,
             input$button_dw_save,
             input$hot_dw_elements,
             force_mod_update[["triggered"]])
      })
      # This updates the reaction state:
      observeEvent(toListen(), {
        state = DW_fetch_state(id              = id,
                               input           = input,
                               session         = session,
                               FM_yaml_file    = FM_yaml_file,
                               MOD_yaml_file   = MOD_yaml_file,
                               react_state     = react_state)

        FM_le(state, "reaction state updated")

        # Checking if there are exportable datasets:
        react_state[[id]][["DW"]][["hasds"]]    = DW_hasds(state)

        # Module checksum
        react_state[[id]][["DW"]][["checksum"]] = state[["DW"]][["checksum"]]
      }, priority=-101)
    }

    # Removing holds
    remove_hold_listen  <- reactive({
      list(
           force_mod_update[["triggered"]],
           input$button_dw_add_merge_relationship,
           input$select_fds_merge_relationships,
           input$select_fds_sort_column, 
           input$button_dw_save,
           input$select_dw_views)
    })
    observeEvent(remove_hold_listen(), {
      # Once the UI has been regenerated we
      # remove any holds for this module
      state = DW_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      FM_le(state, "removing holds")
      # Removing all holds
      for(hname in names(state[["DW"]][["ui_hold"]])){
        remove_hold(state, session, hname)
      }
    }, priority = -100)

  })
}                                                     # nocov end


#'@export
#'@title Fetch Data Wrangling State
#'@description Merges default app options with the changes made in the UI
#'@param id Shiny module ID
#'@param input Shiny input variable
#'@param session Shiny session variable
#'@param FM_yaml_file App configuration file with FM as main section.
#'@param MOD_yaml_file  Module configuration file with MC as main section.
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
#'    \item{DSV:}              Result of \code{FM_fetch_ds()}.
#'    \item{ui:}               Current value of form elements in the UI
#'    \item{ui_hold:}          List of hold elements to disable updates before a full ui referesh is complete.
#'    \item{view_cntr:}        Counter for tracking view ids, value contains the id of the last view created.
#'    \item{views:}            List of data wrangling views. Each view has the following structure:
#'      \itemize{
#'        \item{checksum:            MD5 sum of WDS}
#'        \item{code:                Code to generate WDS from start to finish}
#'        \item{code_dw_only:        Code for just the wrangling portion.}
#'        \item{code_previous:       Code to load data and assign to view object.}
#'        \item{elements_table:      Table of data wrangling elements.}
#'        \item{id:                  Character id (\code{view_idx})}
#'        \item{idx:                 Numeric id (\code{1})}
#'        \item{isgood:}             Boolean status of the data view. False if evaluation fails
#'        \item{key:                 User key (short description)}
#'        \item{view_ds_object_name: Object name for this data view}
#'        \item{WDS:                 Current value of the data view with all of the successful commands in elements_table evaluated.}
#'      }
#'  }
#'  \item{MOD_TYPE:} Character data containing the type of module \code{"DW"}
#'  \item{id:} Character data containing the module id
#'  \item{FM_yaml_file:} App configuration file with FM as main section.
#'  \item{MOD_yaml_file:}  Module configuration file with MC as main section.
#'  module in the session variable.
#'}
#'@examples
#' # Within shiny both session and input variables will exist,
#' # this creates examples here for testing purposes:
#' sess_res = DW_test_mksession()
#' session = sess_res$session
#' input   = sess_res$input
#'
#' # Configuration files
#' FM_yaml_file  = system.file(package = "formods", "templates", "formods.yaml")
#' MOD_yaml_file = system.file(package = "formods", "templates", "DW.yaml")
#'
#' # We need to specify both the DW module id as well as the
#' # id of the UD module that feeds into it.
#' id    = "DW"
#'
#' # Creating an empty state object
#' state = DW_fetch_state(id              = id,
#'                        input           = input,
#'                        session         = session,
#'                        FM_yaml_file    = FM_yaml_file,
#'                        MOD_yaml_file   = MOD_yaml_file,
#'                        react_state     = NULL)
#'
DW_fetch_state = function(id,                    input,     session,
                          FM_yaml_file,  MOD_yaml_file,
                          react_state){

  #---------------------------------------------
  # Getting the current state
  state = FM_fetch_mod_state(session, id)
  # If the state has not yet been defined then we
  # initialize it
  if(is.null(state)){
    # General state information
    state = DW_init_state(FM_yaml_file, MOD_yaml_file, id, session)
  }

  id_UD  = state[["MC"]][["module"]][["depends"]][["id_UD"]]
  id_DM  = state[["MC"]][["module"]][["depends"]][["id_DM"]]

  # detecting changes in the datasets
  UPDATE_DS =
    FM_has_ds_changed(state = state,
      ids         = c(id_UD, id_DM),
      fdres       =  state[["DW"]][["DSV"]] ,
      react_state = react_state)

  if(UPDATE_DS){
    FM_le(state, FM_build_comment(level = 1, comment_str="updating data sources"))
    if(state[["DW"]][["isgood"]]){
      state[["DW"]][["DSV"]] = FM_fetch_ds(state, session, c(id_UD, id_DM))
    } else {
      # If there is no dataset loaded the figure generation state will be bad
      # (isgood is FALSE). Then we need to reinitialize the module:
      state = DW_init_state(FM_yaml_file    = FM_yaml_file,
                            MOD_yaml_file   = MOD_yaml_file,
                            id              = id,
                            session         = session)
    }

    # Walking through each view and detecting changes in data sources
    state = DW_rectify(state=state, session=session, id=id, id_UD=id_UD, id_DM=id_DM)
    FM_le(state, FM_build_comment(level = 1, comment_str="updating data sources: done"))
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
    msgs = c()
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
            del_key = NULL
            if(nrow(OLD_ET) == nrow(hot_df)){
              for(eridx in 1:nrow(OLD_ET)){
                if(hot_df[eridx, ]$Delete == FALSE){
                  NEW_ET = rbind(NEW_ET,
                                 OLD_ET[eridx,])
                } else {
                  del_key = OLD_ET[eridx,]$Key
                  # When we find the row being deleted we add a notification
                  del_row = OLD_ET[eridx, ]
                  notify_text = state[["MC"]][["notifications"]][["del_dw_element"]]
                  notify_text = stringr::str_replace_all(notify_text, "===ACTION===", del_row$Action)
                  notify_text = stringr::str_replace_all(notify_text, "===DESC===",   del_row$Description)
                  state = FM_set_notification(state, notify_text, "DW element deleted", "warning")
                }
              }
              # Removing the key from the list as well
              if(!is.null(del_key)){
                current_view[["elements_list"]][[del_key]]  = NULL
              }


              # Now we save the view with the elements table
              # updated and force the rebuild:
              current_view[["elements_table"]]  = NEW_ET
              state = DW_set_current_view(state=state, session=session, dw_view=current_view)
              #browser()
              state = DW_rectify(state=state, session=session, view_ids = current_view[["id"]], 
                                 id=id, id_UD=id_UD, id_DM=id_DM, 
                                 force=TRUE)
              msgs = c(msgs, state[["DW"]][["res"]][["rebuild_current_view"]][["msgs"]])
              state = DW_update_checksum(state)
              FM_le(state, "wrangling element deleted")
            }
          }
        }
      }
    }
  }

  # Detecting view selection changes
  if(has_updated(ui_val   = state[["DW"]][["ui"]][["select_dw_views"]],
                 old_val  = state[["DW"]][["current_view"]],
                 init_val = "") &
      (!fetch_hold(state,"select_dw_views"))){

    # Changing the current view to the one selected in the UI
    state[["DW"]][["current_view"]] =  state[["DW"]][["ui"]][["select_dw_views"]]
    state = set_hold(state, inputId = "select_dw_views")
    FM_le(state, "updated: select_dw_views")
  }

  # Detecting sort changes
  if(has_updated(ui_val   = state[["DW"]][["ui"]][["select_fds_sort_column"]],
                 old_val  = state[["DW"]][["sort_cols"]])&
      (!fetch_hold(state,"select_fds_sort_column"))){

    state[["DW"]][["sort_cols"]] = state[["DW"]][["ui"]][["select_fds_sort_column"]]

    #state[["DW"]][["sort_cols"]] =  state[["DW"]][["ui"]][["select_fds_sort"]]
    state = set_hold(state, inputId = "select_fds_sort_column")
    FM_le(state, "updated: select_fds_sort_column")
    FM_le(state, paste0("sort columns: ", paste0(state[["DW"]][["sort_cols"]], collapse =", ")))
  }



  # Detecting add_element clicks
  if(has_updated(ui_val   = state[["DW"]][["ui"]][["button_dw_add_element"]],
                 old_val  = state[["DW"]][["button_counters"]][["button_dw_add_element"]],
                 init_val = c("", 0))){
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
      dwb_res = dwrs_builder(state=state, session=session)

      # saving the messages
      msgs = c(msgs, dwb_res[["msgs"]])

      # If the data wrangling command was successfully built we evaluate
      # the chain to make sure the new element runs correctly:
      if(dwb_res[["isgood"]]){
        # Evaluating the element
        dwee_res = dw_eval_element(state=state, session=session, cmd=dwb_res[["cmd"]], res_obj = dwb_res[["res_obj"]])

        # Appending any messages
        msgs = c(msgs, dwee_res[["msgs"]])

        # If that was successful we append the element to the elements
        # table
        if(dwee_res[["isgood"]]){
          state    = DW_add_wrangling_element(
                       state=state, 
                       session=session, 
                       dwb_res=dwb_res, 
                       dwee_res=dwee_res)
          
          notify_text = state[["MC"]][["notifications"]][["new_dw_element"]]
          notify_text = stringr::str_replace_all(notify_text, "===ACTION===", dwb_res$action)
          notify_text = stringr::str_replace_all(notify_text, "===DESC===", dwb_res$desc)
          state = FM_set_notification(state, notify_text, "DW element added")

          # Triggering an update of any dependent data views 
          view_deps        = DW_fetch_view_deps(
            state    = state,  
            session  = session, 
            view_ids = current_view[["id"]])

          # If there are any views that depend on the current view we rectify those
          if(length(view_deps[["dep_catalog_ex"]][["view_id"]])>0){
            FM_le(state, paste0("triggering update of dependent data views: ", paste0(view_deps[["dep_catalog_ex"]][["view_id"]], collapse=", ")))
            state = DW_rectify(state=state, session=session, view_ids = view_deps[["dep_catalog_ex"]][["view_id"]], id=id, id_UD=id_UD, id_DM=id_DM)
          }
        }
      } else {
        notify_text = state[["MC"]][["notifications"]][["bad_dw_element"]]
        state = FM_set_notification(state, notify_text, "DW element not added", type="failure")
      }
    }

    # Passing any messages back to the user
    state = FM_set_ui_msg(state, msgs)

    # Lastly we save the button value from the UI to the state:
    state[["DW"]][["button_counters"]][["button_dw_add_element"]] =state[["DW"]][["ui"]][["button_dw_add_element"]]

    # updating the state checksum
    state = DW_update_checksum(state)
  }

  # Detecting add merge relationships clicks
  if(has_updated(ui_val   = state[["DW"]][["ui"]][["button_dw_add_merge_relationship"]],
                 old_val  = state[["DW"]][["button_counters"]][["button_dw_add_merge_relationship"]],
                 init_val = c("", 0))){

    merge_condition = state[["DW"]][["ui"]][["select_fds_merge_condition"]]
    FM_le(state, paste0("adding merge relationship: ", merge_condition))

    tmp_merge_rel = list(
      condition = merge_condition)

    # Merge description with placeholders
    merge_desc = state[["MC"]][["formatting"]][["select_fds_merge_condition"]][["conditions"]][[merge_condition]][["desc"]]

    if(merge_condition %in% c("equality", "inequality_closest", "inequality")) {
      tmp_merge_rel[["dv_cols"]] = state[["DW"]][["ui"]][["select_fds_merge_condition_dv_cols"]]
      tmp_merge_rel[["ns_cols"]] = state[["DW"]][["ui"]][["select_fds_merge_condition_ns_cols"]]
      merge_desc = stringr::str_replace_all(string=merge_desc, pattern="===DV===", replacement= tmp_merge_rel[["dv_cols"]])
      merge_desc = stringr::str_replace_all(string=merge_desc, pattern="===NS===", replacement= tmp_merge_rel[["ns_cols"]])
    }

    if(merge_condition %in% c("inequality", "inequality_closest")) {

      tmp_merge_rel[["operator"]] = state[["DW"]][["ui"]][["select_fds_merge_condition_operator"]]

      tmp_op_index =
        which(tmp_merge_rel[["operator"]] ==
              state[["MC"]][["formatting"]][["select_fds_merge_condition"]][["relationships"]][["eq_ineq"]][["operators"]][["choices"]])

      tmp_merge_op_desc =    state[["MC"]][["formatting"]][["select_fds_merge_condition"]][["relationships"]][["eq_ineq"]][["operators"]][["dnames"]][[tmp_op_index]]


      merge_desc = stringr::str_replace_all(
        string      = merge_desc,
        pattern     = "===OP===",
        replacement = tmp_merge_op_desc)

    } else if(merge_condition %in% c("between", "within", "overlaps")) {
      tmp_merge_rel[["dv_lb"]] = state[["DW"]][["ui"]][["select_fds_merge_dv_lb"]]
      tmp_merge_rel[["dv_ub"]] = state[["DW"]][["ui"]][["select_fds_merge_dv_ub"]]
      tmp_merge_rel[["ns_lb"]] = state[["DW"]][["ui"]][["select_fds_merge_ns_lb"]]
      tmp_merge_rel[["ns_ub"]] = state[["DW"]][["ui"]][["select_fds_merge_ns_ub"]]

      merge_desc = stringr::str_replace_all(string=merge_desc, pattern="===DV_LB===", replacement= tmp_merge_rel[["dv_lb"]])
      merge_desc = stringr::str_replace_all(string=merge_desc, pattern="===DV_UB===", replacement= tmp_merge_rel[["dv_ub"]])
      merge_desc = stringr::str_replace_all(string=merge_desc, pattern="===NS_LB===", replacement= tmp_merge_rel[["ns_lb"]])
      merge_desc = stringr::str_replace_all(string=merge_desc, pattern="===NS_UB===", replacement= tmp_merge_rel[["ns_ub"]])
    }

    if(!(merge_desc %in% state[["DW"]][["merge_rels"]])){
      notify_text = state[["MC"]][["notifications"]][["new_merge_rel"]]
      notify_text = stringr::str_replace_all(notify_text, "===MRDESC===", merge_desc)
    }

    state[["DW"]][["merge_rels"]][[merge_desc]] = tmp_merge_rel

    state = FM_set_notification(state, notify_text, "Merge Relationship Added")

    state = set_hold(state, inputId = "select_fds_merge_relationships")

    # Updating the "old" values so they match the current relationships
    state[["DW"]][["ui_old"]][["select_fds_merge_relationships"]] = names( state[["DW"]][["merge_rels"]])


    state[["DW"]][["ui_old"]][["select_fds_merge_source"]]  =
      state[["DW"]][["ui"]][["select_fds_merge_source"]]

    # Lastly we save the button value from the UI to the state:
    state[["DW"]][["button_counters"]][["button_dw_add_merge_relationship"]] =state[["DW"]][["ui"]][["button_dw_add_merge_relationship"]]
  }


  # Removing merge relationships
  if((has_updated(ui_val   = state[["DW"]][["ui"]][["select_fds_merge_relationships"]],              # Detecting changes
                  old_val  = state[["DW"]][["ui_old"]][["select_fds_merge_relationships"]],
                  init_val = c("", 0)) |
                 (length(state[["DW"]][["ui"]][["select_fds_merge_relationships"]])     == 0 |       # Catching the case where the ui value is null
                  length(state[["DW"]][["ui_old"]][["select_fds_merge_relationships"]]) > 0))
                  &
                 !fetch_hold(state,"select_fds_merge_relationships")){

    if(length(state[["DW"]][["merge_rels"]]) > 0){

      ui_rels = state[["DW"]][["ui"]][["select_fds_merge_relationships"]]
      curr_rels = names(state[["DW"]][["merge_rels"]])

      if(!all(curr_rels %in% ui_rels)){
        rm_rels  = curr_rels[!(curr_rels %in% ui_rels)]
        for(rm_rel in rm_rels){
          FM_le(state, paste0("removing relationship: ", rm_rel))
          state[["DW"]][["merge_rels"]][[rm_rel]] = NULL
        }
      }

      state[["DW"]][["ui_old"]][["select_fds_merge_relationships"]] =
      state[["DW"]][["ui"]][["select_fds_merge_relationships"]]
    }
  }
  #------------------------------------
  if(has_updated(ui_val   = state[["DW"]][["ui"]][["button_dw_new"]],
                 old_val  = state[["DW"]][["button_counters"]][["button_dw_new"]],
                 init_val = c("", 0))){

    FM_le(state, "creating new wrangling view")
    # Empty messages:
    msgs = c()

    # Creatign a new view
    state = DW_new_view(state=state, session=session)

    # Setting hold for views select
    state = set_hold(state, inputId = "select_dw_views")

    # Lastly we save the button value from the UI to the state:
    state[["DW"]][["button_counters"]][["button_dw_new"]] =state[["DW"]][["ui"]][["button_dw_new"]]

    # updating the state checksum
    state = DW_update_checksum(state)
  }

  #------------------------------------
  if(has_updated(ui_val   = state[["DW"]][["ui"]][["button_dw_del"]],
                 old_val  = state[["DW"]][["button_counters"]][["button_dw_del"]],
                 init_val = c("", 0))){

    FM_le(state, "deleting wrangling view")
    # Empty messages:
    msgs = c()

    # Fetchign the current view to get the id
    current_view = DW_fetch_current_view(state)

    # Pulling out information on views that depend on the current view
    view_deps        = DW_fetch_view_deps(
      state    = state,  
      session  = session, 
      view_ids = current_view[["id"]])

    # Deleting the current view
    state[["DW"]][["views"]][[current_view[["id"]]]] = NULL

    # We ahve to save the state here because the DW_fetch_ds function will read
    # the state directly from the session object and if we don't save the state
    # the fetch_ds function will return datasets that don't exist.
    FM_set_mod_state(session, id, state)


    # If there are no views then we create an empty one
    if(length(state[["DW"]][["views"]]) == 0){
      state = DW_new_view(state=state, session=session)
    } else {
    # If there are views then we set the first one as active
      state[["DW"]][["current_view"]] = names(state[["DW"]][["views"]])[1]
    }

    # If there are any views that depend on the view we just deleted, we rectify those
    if(length(view_deps[["dep_catalog_ex"]][["view_id"]])>0){
      FM_le(state, paste0("triggering update of dependent data views: ", paste0(view_deps[["dep_catalog_ex"]][["view_id"]], collapse=", ")))
      state = DW_rectify(state=state, session=session, view_ids = view_deps[["dep_catalog_ex"]][["view_id"]], id=id, id_UD=id_UD, id_DM=id_DM)
    }

    # Setting hold for views select
    state = set_hold(state, inputId = "select_dw_views")

    # Lastly we save the button value from the UI to the state:
    state[["DW"]][["button_counters"]][["button_dw_del"]] =state[["DW"]][["ui"]][["button_dw_del"]]

    # updating the state checksum
    state = DW_update_checksum(state)
  }

  #------------------------------------
  if(has_updated(ui_val   = state[["DW"]][["ui"]][["button_dw_copy"]],
                 old_val  = state[["DW"]][["button_counters"]][["button_dw_copy"]],
                 init_val = c("", 0))){

    FM_le(state, "copying data view")

    # Empty messages:
    msgs = c()

    # Original is the view being copied
    original_view = DW_fetch_current_view(state)

    # This creates the new view and makes it active:
    state = DW_new_view(state=state, session=session)

    # Now we pull out the new view:
    new_view = DW_fetch_current_view(state)

    # Setting the data source to be the same:
    new_view = DW_attach_ds(state, new_view, original_view[["ds_source_id"]])

    # Setting the list from the original view to the new view
    new_view[["elements_list"]] =  original_view[["elements_list"]]

    # Saving the new view with the data source
    state = DW_set_current_view(state=state, session=session, dw_view=new_view)

    # Forcing a rebuild with the elements from the old view:
    state = DW_rebuild_current_view(state=state, session=session)

    # Setting holds
    state = set_hold(state)

    # Lastly we save the button value from the UI to the state:
    state[["DW"]][["button_counters"]][["button_dw_copy"]] =state[["DW"]][["ui"]][["button_dw_copy"]]

    # updating the state checksum
    state = DW_update_checksum(state)
  }
  #------------------------------------
  # Resetting merge relationships. This is done in response to changes in the
  # merge source, data view selection
  if(state[["DW"]][["ui"]][["select_dw_element"]] == "merge"){
    if((# has_updated(ui_val   = state[["DW"]][["ui"]][["button_dw_save"]],
        #             old_val  = state[["DW"]][["button_counters"]][["button_dw_save"]],
        #             init_val = c("", 0)) |
       (has_updated(ui_val   = state[["DW"]][["ui"]][["select_dw_views"]],                     # Data view chanted
                   old_val  = state[["DW"]][["current_view"]],
                   init_val = "") &
        (!fetch_hold(state,"select_dw_views"))) |
        (has_updated(ui_val   = state[["DW"]][["ui"]][["select_fds_merge_source"]],            # Merge source changed
                    old_val  = state[["DW"]][["ui_old"]][["select_fds_merge_source"]],
                    init_val = c(""))) &
        !fetch_hold(state,"select_fds_merge_relationships"))){

      if(length( state[["DW"]][["merge_rels"]])>0){
        state[["DW"]][["merge_rels"]] = list()
        FM_le(state, "resetting merge relationships")
        state = set_hold(state, inputId = "select_fds_merge_relationships")
      }
    }
  }
  #------------------------------------
  # Saving data view
  if(has_updated(ui_val   = state[["DW"]][["ui"]][["button_dw_save"]],
                 old_val  = state[["DW"]][["button_counters"]][["button_dw_save"]],
                 init_val = c("", 0))){
    FM_le(state, "saving changes to current wrangling view")

    if(state[["DW"]][["ui"]][["current_key"]] != ""){
      REBUILD_VIEW = FALSE
      # Resetting the key
      current_view = DW_fetch_current_view(state)
      current_view[["key"]] = state[["DW"]][["ui"]][["current_key"]]
      if(has_updated(ui_val   = state[["DW"]][["ui"]][["select_current_source"]],
                     old_val  = current_view[["ds_source_id"]])){
        FM_le(state, "changing data source:")
        FM_le(state, paste0(" - old source: ", current_view[["ds_source_id"]]))
        FM_le(state, paste0(" - new source: ", state[["DW"]][["ui"]][["select_current_source"]]))

        current_source_id = state[["DW"]][["ui"]][["select_current_source"]]
        current_view = DW_attach_ds(state, current_view, current_source_id)
        REBUILD_VIEW = TRUE
      }
      state = DW_set_current_view(state=state, session=session, dw_view=current_view)
      if(REBUILD_VIEW){
        state = DW_rebuild_current_view(state=state, session=session)
        msgs = c(msgs, state[["DW"]][["res"]][["rebuild_current_view"]][["msgs"]])
      }
    } else {
      # returning an error
      msgs = c(msgs,
          tags$em(state[["MC"]][["errors"]][["current_key_empty"]]))
    }

    # setting holds
    state = set_hold(state)

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
#'the name of the list element in react_state where the data set is stored.
#'@param session Shiny session variable
#'module (\code{NULL})
#'@return list containing an empty DW state
#'@examples
#' # Within shiny both session and input variables will exist,
#' # this creates examples here for testing purposes:
#' sess_res = DW_test_mksession()
#' session = sess_res$session
#' input   = sess_res$input
#'
#' state = DW_init_state(
#'    FM_yaml_file  = system.file(package = "formods",
#'                                "templates",
#'                                "formods.yaml"),
#'    MOD_yaml_file = system.file(package = "formods",
#'                                "templates",
#'                                "DW.yaml"),
#'    id              = "DW",
#'    session         = session)
#'
#' state
DW_init_state = function(FM_yaml_file, MOD_yaml_file, id, session){

  MOD_yaml_cont = FM_read_yaml(MOD_yaml_file)
  id_ASM = MOD_yaml_cont[["MC"]][["module"]][["depends"]][["id_ASM"]]
  id_UD  = MOD_yaml_cont[["MC"]][["module"]][["depends"]][["id_UD"]]
  id_DM  = MOD_yaml_cont[["MC"]][["module"]][["depends"]][["id_DM"]]

  # initializing the state with the required formods elements:
  button_counters = c(
    "button_dw_add_element"              , # Element: Adding a new element
    "button_dw_add_merge_relationship"   , # View:    Adding merge relationships
    "button_dw_new"           ,            # View:    New blank view
    "button_dw_del"           ,            # View:    Delete the current view
    "button_dw_save"          ,            # View:    Save the current view
    "button_dw_copy"                       # View:    Copy the current view
    )
  ui_ids          = c(
    "hot_dw_elements"                      ,
    "button_dw_add_element"                ,
    "button_dw_add_merge_relationship"     ,
    "button_dw_new"                        ,
    "button_dw_del"                        ,
    "button_dw_copy"                       ,
    "button_dw_save"                       ,
    "select_fds_filter_column"             ,
    "select_fds_sort_column"               ,
    "select_fds_filter_operator"           ,
    "fds_filter_rhs"                       ,
    "select_fds_mutate_column"             ,
    "select_fds_mutate_rhs"                ,
    "select_fds_rename_column"             ,
    "select_fds_merge_condition_dv_cols"   ,
    "select_fds_merge_condition_ns_cols"   ,
    "select_fds_merge_condition_operator"  ,
    "select_fds_merge_dv_lb"               ,
    "select_fds_merge_dv_ub"               ,
    "select_fds_merge_ns_lb"               ,
    "select_fds_merge_ns_ub"               ,
    "select_fds_merge_relationships"       ,
    "fds_rename_rhs"                       ,
    "current_key"                          ,
    "select_fds_group_column"              ,
    "select_fds_select_column"             ,
    "select_fds_longer_column"             ,
    "select_fds_longer_values"             ,
    "select_fds_longer_names"              ,
    "select_fds_wider_values"              ,
    "select_fds_wider_names"               ,
    "select_fds_merge_source"              ,
    "select_fds_merge_method"              ,
    "select_fds_merge_condition"           ,
  # "select_fds_merge_helper"              ,
    "select_current_source"                ,
    "select_dw_views"                      ,
    "select_dw_element"
    )

  ui_hold         = c(
    "select_fds_merge_relationships",
    "select_current_source"      ,
    "hot_dw_elements",
    "current_key"    ,
    "select_fds_sort_column",
    "select_dw_views"
    )

  state = FM_init_state(
    FM_yaml_file    = FM_yaml_file,
    MOD_yaml_file   = MOD_yaml_file,
    id              = id,
    MT              = "DW",
    dep_mod_ids     = c(id_UD),
    button_counters = button_counters,
    ui_ids          = ui_ids,
    ui_hold         = ui_hold,
    session         = session)

  #---------------------------------------------
  # Fetching datasets
  DSV = FM_fetch_ds(state, session, c(id_UD, id_DM))

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
  state[["DW"]][["sort_cols"]]            = ""
  state[["DW"]][["view_cntr"]]            = 0
  state[["DW"]][["DSV"]]                  = DSV

  state[["DW"]][["ui_old"]][["select_fds_merge_relationships"]] = ""
  state[["DW"]][["ui_old"]][["select_fds_merge_source"]]        = ""

  # By default the state is bad
  state[["DW"]][["isgood"]]         = DSV[["hasds"]]

  if(state[["DW"]][["isgood"]]){
    # Initializing an empty figure
    state = DW_new_view(state=state, session=session)
  }

  # Storing merge relationships
  state[["DW"]][["merge_rels"]]         = list()

  FM_le(state, "State initialized")

  # initializing the module checksum:
  state = DW_update_checksum(state)


state }


#'@export
#'@title Builds a Data Wrangling R Statement From ui Elements:
#'@description Takes the current ui elements and constructs the appropriate
#'data wrangling command from the user input.
#'@param state DW state from \code{DW_fetch_state()}
#'@param session Shiny session variable
#'@return list containing the following elements
#'\itemize{
#'  \item{isgood:} Return status of the function
#'  \item{cmd:}    Data wrangling R command
#'  \item{action:} The action being performed
#'  \item{pll:}    Preload list (pll) containing components to save with
#'  mk_preload.
#'  \item{res_obj:} Resource objects beyond data view used by this wrangling
#'  statement (e.g. data source used when merging) 
#'   or \code{NULL} if no resource is used.
#'  \item{res_obj_DSchecksum:} Checksum of the res_obj
#'   or \code{NULL} if no resource is used.
#'  \item{res_obj_mod_id:} Module ID of the res obj 
#'   or \code{NULL} if no resource is used.
#'  \item{desc:}   Verbose description of the action
#'  \item{msgs:}   Messages to be passed back to the user
#'}
#'@example inst/test_apps/DW_funcs.R
dwrs_builder = function(state, session){

  isgood = TRUE
  msgs   = c()
  cmd    = ""
  desc   = ""
  res_obj = NULL
  res_obj_DSchecksum = NULL
  res_obj_mod_id   = NULL
  action = ""

  # Preload list
  pll    = list()

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
  } else if(action %in%  c("onerow", "ungroup")){
    # Nothing needs to be done here
  } else if(action == "sort"){
    if(length(ui[["select_fds_sort_column"]])==1){
      if(ui[["select_fds_sort_column"]]==""){
        isgood = FALSE
        msgs = c(msgs, state[["MC"]][["errors"]][["fds_sort_column"]])
      }
    }
  } else if(action == "merge"){
    valid_methods = state[["MC"]][["formatting"]][["select_fds_merge_condition"]][["valid_methods"]]
    merge_method  = state[["DW"]][["ui"]][["select_fds_merge_method"]]
    merge_source  = state[["DW"]][["ui"]][["select_fds_merge_source"]]

    if(merge_source == ""){
      isgood = FALSE
      msgs = c(msgs, state[["MC"]][["errors"]][["fds_merge_source"]])
    }
    if(merge_method == ""){
      isgood = FALSE
      msgs = c(msgs, state[["MC"]][["errors"]][["fds_merge_method"]])
    }
    if(merge_method %in% valid_methods){
      if(length(state[["DW"]][["merge_rels"]]) == 0){
        isgood = FALSE
        msgs = c(msgs, state[["MC"]][["errors"]][["fds_merge_rels"]])
      }
    }
  } else {
    isgood = FALSE
    msgs = c(msgs, state[["MC"]][["errors"]][["unknown_action"]])
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
      cmd = paste0(view_ds_object_name,  " <- dplyr::filter(", view_ds_object_name, ",", cond_str, ")")

      # Packing up the preload list:
      pll[["action"]]     = action
      pll[["column"]]     = ui[["select_fds_filter_column"]]
      pll[["operator"]]   = ui[["select_fds_filter_operator"]]
      pll[["rhs"]]        = ui[["fds_filter_rhs"]]

    } else if(action == "mutate"){
      rhs_str = ui[["select_fds_mutate_rhs"]]
      cmd = paste0(view_ds_object_name, " <- dplyr::mutate(", view_ds_object_name,",",
                  ui[["select_fds_mutate_column"]],
                  " = ",
                  rhs_str, ")")
      desc = paste( ui[["select_fds_mutate_column"]],
                    "=", rhs_str)

      # Packing up the preload list:
      pll[["action"]]     = action
      pll[["column"]]     = ui[["select_fds_mutate_column"]]
      pll[["rhs"]]        = ui[["select_fds_mutate_rhs"]]

    } else if(action == "rename"){
      new_name =  ui[["fds_rename_rhs"]]
      cmd = paste0(view_ds_object_name, " <- dplyr::rename(", view_ds_object_name,",",
                  new_name,
                  " = ",
                  ui[["select_fds_rename_column"]],
                  ")")
      desc = paste(ui[["select_fds_rename_column"]], " to ", new_name)

      # Packing up the preload list:
      pll[["action"]]     = action
      pll[["column"]]     = ui[["select_fds_rename_column"]]
      pll[["rhs"]]        = ui[["fds_rename_rhs"]]

    } else if(action == "group"){
      group_cols_str   = paste(ui[["select_fds_group_column"]], collapse=', ')
      cmd = paste0(view_ds_object_name, " <- dplyr::group_by(", view_ds_object_name,",",
                  group_cols_str,
                  ")")
      desc = paste(group_cols_str)

      # Packing up the preload list:
      pll[["action"]]     = action
      pll[["column"]]     = ui[["select_fds_group_column"]]

    } else if(action == "select"){
      select_cols_str   = paste(ui[["select_fds_select_column"]], collapse=', ')
      cmd = paste0(view_ds_object_name, " <- dplyr::select(", view_ds_object_name,",",
                  select_cols_str,
                  ")")
      desc = paste(select_cols_str)

      # Packing up the preload list:
      pll[["action"]]     = action
      pll[["column"]]     = ui[["select_fds_select_column"]]

    } else if(action == "longer"){
      select_cols_str         = paste(ui[["select_fds_longer_column"]], collapse=', ')
      select_cols_str_quote   = paste(ui[["select_fds_longer_column"]], collapse='", "')
      select_cols_str_quote   = paste0('c("', select_cols_str_quote, '")')
      names_to          = ui[["select_fds_longer_names"]]
      values_to         = ui[["select_fds_longer_values"]]

      cmd = paste0(view_ds_object_name, " <- tidyr::pivot_longer(", view_ds_object_name,
                  ', cols      = ', select_cols_str_quote,
                  ', names_to  = "',  names_to,  '"',
                  ', values_to = "', values_to, '"',
                  ")")
      desc = paste(select_cols_str, "-->", names_to, ", ", values_to)

      # Packing up the preload list:
      pll[["action"]]     = action
      pll[["column"]]     = ui[["select_fds_longer_column"]]
      pll[["names"]]      = ui[["select_fds_longer_names"]]
      pll[["values"]]     = ui[["select_fds_longer_values"]]

    } else if(action == "wider"){
      names_from        = ui[["select_fds_wider_names"]]
      values_from       = ui[["select_fds_wider_values"]]

      cmd = paste0(view_ds_object_name, " <- tidyr::pivot_wider(", view_ds_object_name,
                  ', names_from  = "',  names_from,  '"',
                  ', values_from = "', values_from, '"',
                  ")")
      desc = paste("names_from: ", names_from, ", values_from: ", values_from)

      # Packing up the preload list:
      pll[["action"]]     = action
      pll[["names"]]      = ui[["select_fds_wider_names"]]
      pll[["values"]]     = ui[["select_fds_wider_values"]]

    } else if(action == "ungroup"){
      cmd = paste0(view_ds_object_name, " <- dplyr::ungroup(",view_ds_object_name,")")
      desc = state[["MC"]][["labels"]][["ungroup_data"]]

      # Packing up the preload list:
      pll[["action"]]     = action
    } else if(action == "onerow"){
      cmd = paste0(view_ds_object_name, " <- dplyr::filter(", view_ds_object_name, ",row_number()==1)")
      desc = state[["MC"]][["labels"]][["keep_onerow"]]

      # Packing up the preload list:
      pll[["action"]]     = action
    } else if(action == "sort"){
      sort_cols     = c()
      sort_cols_txt = c()
      for(sort_str in ui[["select_fds_sort_column"]]){
        sort_comps = unlist(stringr::str_split(string=sort_str, pattern=":"))
        if(sort_comps[2] == "ascending"){
          sort_cols = c(sort_cols, sort_comps[1])
          sort_cols_txt = c(sort_cols_txt, sort_comps[1])
        } else if(sort_comps[2] == "descending"){
          sort_cols     = c(sort_cols,     paste0("dplyr::desc(", sort_comps[1], ")"))
          sort_cols_txt = c(sort_cols_txt, paste0(sort_comps[1], "(desc)"))
        }
      }
      cmd = paste0(view_ds_object_name, " <- dplyr::arrange(", view_ds_object_name, ",",paste0(sort_cols, collapse=", "),")")
      desc = state[["MC"]][["labels"]][["sorting"]]
      desc = stringr::str_replace(
               string      =desc,
               pattern     ="===COLS===",
               replacement = paste0(sort_cols_txt, collapse=", "))

      # Packing up the preload list:
      pll[["action"]]     = action
      pll[["column"]]     = ui[["select_fds_sort_column"]]
    } else if(action == "merge"){
      valid_methods    = state[["MC"]][["formatting"]][["select_fds_merge_condition"]][["valid_methods"]]
      merge_method     = state[["DW"]][["ui"]][["select_fds_merge_method"]]
      merge_rels_ui    = state[["DW"]][["merge_rels"]]
      res_obj          = state[["DW"]][["ui"]][["select_fds_merge_source"]]

      ds_meta = DW_fetch_obj_ds_meta(state   = state, 
                                     session = session, 
                                     dw_view = current_view,
                                     ds_obj  = res_obj)
      res_obj_DSchecksum = ds_meta[["res_row"]][["DSchecksum"]]
      res_obj_mod_id   = ds_meta[["res_row"]][["id"]]

      tmp_merge_rels = NULL

      # Constructing the merge relationships
      for(mrname in names(merge_rels_ui)){
         if(merge_rels_ui[[mrname]][["condition"]]        == "equality"){
           tmp_merge_rels = c(tmp_merge_rels,
             paste0(merge_rels_ui[[mrname]][["dv_cols"]],
                    " == ",
                    merge_rels_ui[[mrname]][["ns_cols"]]))
         } else if(merge_rels_ui[[mrname]][["condition"]] == "inequality"){
           tmp_merge_rels = c(tmp_merge_rels,
             paste0(merge_rels_ui[[mrname]][["dv_cols"]], " ",
                    merge_rels_ui[[mrname]][["operator"]], " ",
                    merge_rels_ui[[mrname]][["ns_cols"]]))
         } else if(merge_rels_ui[[mrname]][["condition"]] == "inequality_closest"){
           tmp_merge_rels = c(tmp_merge_rels,
             paste0("dplyr::closest(", merge_rels_ui[[mrname]][["dv_cols"]], " ",
                    merge_rels_ui[[mrname]][["operator"]], " ",
                    merge_rels_ui[[mrname]][["ns_cols"]],")"))
         } else if(merge_rels_ui[[mrname]][["condition"]] == "between"){
           tmp_merge_rels = c(tmp_merge_rels,
             paste0("dplyr::between(",
                    merge_rels_ui[[mrname]][["dv_lb"]], ", ",
                    merge_rels_ui[[mrname]][["ns_lb"]], ", ",
                    merge_rels_ui[[mrname]][["ns_ub"]],")"))
         } else if(merge_rels_ui[[mrname]][["condition"]] == "within"){
           tmp_merge_rels = c(tmp_merge_rels,
             paste0("dplyr::within(",
                    merge_rels_ui[[mrname]][["dv_lb"]], ", ",
                    merge_rels_ui[[mrname]][["dv_ub"]], ", ",
                    merge_rels_ui[[mrname]][["ns_lb"]], ", ",
                    merge_rels_ui[[mrname]][["ns_ub"]],")"))
         } else if(merge_rels_ui[[mrname]][["condition"]] == "overlaps"){
           tmp_merge_rels = c(tmp_merge_rels,
             paste0("dplyr::overlaps(",
                    merge_rels_ui[[mrname]][["dv_lb"]], ", ",
                    merge_rels_ui[[mrname]][["dv_ub"]], ", ",
                    merge_rels_ui[[mrname]][["ns_lb"]], ", ",
                    merge_rels_ui[[mrname]][["ns_ub"]],")"))
         } else {
          isgood = FALSE
          msgs = c(msgs, paste("Merge relationship condition not found: ",merge_rels_ui[[mrname]][["condition"]]))
         }
      }

      if(isgood){
        desc = paste0(state[["MC"]][["formatting"]][["select_fds_merge_method"]][["methods"]][[merge_method]][["cname"]],
                      ": ", res_obj)
        # Applying the methods
        if(merge_method == "rbind"){
          cmd = paste0(view_ds_object_name, " <- rbind(", view_ds_object_name, ",",res_obj,")")
        } else if(merge_method == "cbind"){
          cmd = paste0(view_ds_object_name, " <- cbind(", view_ds_object_name, ",",res_obj,")")
        } else if(merge_method %in%  c("left_join", "right_join", "full_join", "inner_join")){

          # This creates a join_by command to be passed to the relevant merge
          if(is.null(tmp_merge_rels)){
            tmp_join_by_cmd = "tmp_by = NULL"
          } else {
            tmp_join_by_cmd = paste0("tmp_by <- dplyr::join_by(", paste0(tmp_merge_rels, collapse=", "), ")")
          }
          cmd = c(tmp_join_by_cmd,
                  paste0(view_ds_object_name, " <- dplyr::", merge_method,"(x=", view_ds_object_name, ",y=",res_obj,", by=tmp_by)"))
        } else {
          isgood = FALSE
          msgs = c(msgs, paste("Merge method not found:", merge_method))
        }
      }
      # Packing up the preload list:
      pll[["action"]]                      = action
      pll[["method"]]                      = merge_method
      pll[["merge_rels_ui"]]               = merge_rels_ui

      # Attaching the data source for the dataset to be merged:
      ds_meta = DW_fetch_obj_ds_meta(state   = state, 
                                     session = session, 
                                     dw_view = current_view,
                                     ds_obj  = res_obj)
      if(ds_meta[["isgood"]]){
        pll[["data_source"]] = list(
          id        = ds_meta[["res_row"]][["id"]],
          idx       = ds_meta[["res_row"]][["idx"]],
          res_label = ds_meta[["res_row"]][["res_label"]])
      } else {
        isgood = FALSE
        msgs = c(msgs, ds_meta[["msgs"]])
      }

      # JMH add pll components for the dataset to be merged
    } else {
      isgood = FALSE
      msgs = c(msgs, paste("Action not found:", action))
    }
  }

  res = list(isgood           = isgood,
             cmd              = cmd,
             action           = action,
             res_obj          = res_obj,
             res_obj_DSchecksum = res_obj_DSchecksum,
             res_obj_mod_id   = res_obj_mod_id,
             desc             = desc,
             pll              = pll,
             msgs             = msgs)

  res
}


#'@export
#'@title Evaluates Data Wrangling Generated Code
#'@description Takes the current state and a string containing a data
#'wranlging command and evaluates it.
#'@param state DW state from \code{DW_fetch_state()}
#'@param session Shiny session variable
#'@param cmd string containing the data wrangling command
#'@param res_obj resource objects needed to run the code such as merge
#'datasets
#'@return list with the following elements
#'\itemize{
#'  \item{isgood:} Return status of the function.
#'  \item{msgs:}   Messages to be passed back to the user.
#'  \item{DS:}     Wrangled dataset.
#'}
#'@example inst/test_apps/DW_funcs.R
dw_eval_element = function(state, session, cmd, res_obj=NULL){

  msgs = c()
  current_view        = DW_fetch_current_view(state)
  DS                  = current_view[["WDS"]]
  view_ds_object_name = current_view[["view_ds_object_name"]]
  isgood              = TRUE

  # Creating the name of the dataset with the correct
  # object name
  assign(view_ds_object_name,
         DS)

  # Creating any resources needed
  if(!is.null(res_obj)){
    for(tmp_oname in res_obj){
      ds_meta = DW_fetch_obj_ds_meta(state   = state, 
                                     session = session, 
                                     dw_view = current_view,
                                     ds_obj  = tmp_oname)
      if(ds_meta[["isgood"]]){
        assign(tmp_oname, ds_meta[["DS"]])
      } else {
        isgood = FALSE
        msgs = c(msgs, ds_meta[["msgs"]])
      }
    }
  }

  # Trying to evaluate the generated command against DS
  # to see if any errors are generated:
  if(isgood){
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
    isgood = tcres[["isgood"]]
  }

  res = list(isgood = isgood,
             msgs   = msgs,
             DS     = DS)

res}

#'@export
#'@title New Data Wrangling View
#'@description Appends a new empty data wrangling view to the DW state object
#'and makes this new view the active view.
#'@param state DW state from \code{DW_fetch_state()}
#'@param session Shiny session variable
#'@return DW state object containing a new data view and that view set as the
#'current active view. See the help for \code{DW_fetch_state()} for view
#'format.
#'@example inst/test_apps/DW_funcs.R
DW_new_view = function(state, session){

  # Incrementing the view   counter
  state[["DW"]][["view_cntr"]] = state[["DW"]][["view_cntr"]] + 1

  # Creating a default View ID
  view_id = paste0("view_", state[["DW"]][["view_cntr"]])

  # Creating the object name for this view
  view_ds_object_name = paste0(state[["MC"]][["ds_object_name"]],
                          "_", state[["DW"]][["view_cntr"]])

  if(is.null(state[["DW"]][["DSV"]][["catalog"]][["object"]][1])){
    current_source_id = NULL
  } else {
    current_source_id = state[["DW"]][["DSV"]][["catalog"]][["object"]][1]
  }

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
         WDS                 = NULL,
         ds_source_id        = NULL,
         ds_source_checksum  = NULL,
         elements_table      = NULL,
         elements_list       = list(),
         dwe_cntr            = 1,
         # Generated on save
         checksum            = NULL,
         code                = NULL,
         code_dw_only        = NULL)

  view_def = DW_attach_ds(
    state        = state,
    dw_view      = view_def,
    ds_source_id = current_source_id)

  # Setting the new view as current
  state[["DW"]][["current_view"]]     = view_id

  # Dropping the new view into the state
  state = DW_set_current_view(state=state, session=session, dw_view=view_def)

state}

#'@export
#'@title Fetches Current Data View
#'@description Takes a DW state and returns the current active view
#'@param state DW state from \code{DW_fetch_state()}
#'@return List containing the details of the active data view. The structure
#'of this list is the same as the structure of \code{state$DW$views} in the output of
#'\code{DW_fetch_state()}.
#'@example inst/test_apps/DW_funcs.R
DW_fetch_current_view    = function(state){

  view_id = state[["DW"]][["current_view"]]

  current_view = state[["DW"]][["views"]][[view_id]]

current_view}

#'@export
#'@title Sets Current Data View
#'@description Takes a DW state and an updated view and sets that view to the
#'current view_id
#'@param state DW state from \code{DW_fetch_state()}
#'@param session Shiny session variable
#'@param dw_view Data view list of the format returned from \code{DW_fetch_current_view()}
#'(see the structure of \code{state$DW$views} in the output of \code{DW_fetch_state()}).
#'@return DW state object with the value of \code{dw_view} set to the current view id.
#'@example inst/test_apps/DW_funcs.R
DW_set_current_view    = function(state, session, dw_view){

  view_id = state[["DW"]][["current_view"]]


  # Saving the checksum
  chk_before = dw_view[["checksum"]]
  dw_view[["checksum"]]  = digest::digest(dw_view[["WDS"]], algo=c("md5"))
  chk_after  =  dw_view[["checksum"]]

  if(toString(chk_before) != toString(chk_after)){
    FM_le(state, paste0("data view checksum updated: "))
    FM_le(state, paste0("  old: ", chk_before))
    FM_le(state, paste0("  new: ", chk_after))
  }

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

  # Saving the dataview
  state[["DW"]][["views"]][[view_id]] = dw_view

  # Generating code for the data view:
  codeall      = c()
  code_sources = c()
  code_view    = c()
  if(length(good_elements[["cmd"]])> 0){
    DW_obj_map <- setNames(as.list(names(state[["DW"]][["views"]])), 
                           sapply(state[["DW"]][["views"]], 
                                  function(x) x[["view_ds_object_name"]]))

    avail_ds  = DW_fetch_available_sources(state = state, session=session, dw_view = dw_view)


    # DW objects to check
    DW_ds_objs_check   = c(dw_view[["view_ds_object_name"]])

    # Mapping  between the DW data view object 
    # and the initial value from the FM objects
    DW_FM_source = list()

    # Formods objects to include in "loading data"
    FM_ds_objs = c()

    while(length(DW_ds_objs_check) > 0){

      # Pulling out the object name for the first DW view
      tmp_DW_obj       = DW_ds_objs_check[1]
      DW_ds_objs_check =  DW_ds_objs_check[-1] 

      # If it hasn't been processed yet we add the DW view. If it's already
      # processed then we skip it. 
      if(!(tmp_DW_obj %in% names(DW_FM_source))){
        # Tracking the formods source data object
        FM_ds_objs = c(FM_ds_objs, state[["DW"]][["views"]][[ DW_obj_map[[tmp_DW_obj]] ]][["ds_source_id"]])

        # Marking the DW view as done
        DW_FM_source = c(
          as.list(setNames(state[["DW"]][["views"]][[ DW_obj_map[[tmp_DW_obj]] ]][["ds_source_id"]], 
                  tmp_DW_obj)),
          DW_FM_source)

        # Appending the code for the current data view:
        code_view = 
          c(paste0("# ", state[["DW"]][["views"]][[ DW_obj_map[[tmp_DW_obj]] ]][["key"]]),
            paste0(tmp_DW_obj, "<-", state[["DW"]][["views"]][[ DW_obj_map[[tmp_DW_obj]] ]][["ds_source_id"]]),
            state[["DW"]][["views"]][[ DW_obj_map[[tmp_DW_obj]] ]][["elements_table"]][["cmd"]],
            "", code_view)

        # Looking for any resource objects in the current data view:
        tmp_res_objs = state[["DW"]][["views"]][[ DW_obj_map[[tmp_DW_obj]] ]][["elements_table"]][["res_obj"]]
        tmp_res_objs = tmp_res_objs[tmp_res_objs != ""]

        if(length(tmp_res_objs) > 0){
          # If res_obj is a dataview and hasn't been added already then add it
          # to: DW_ds_objs_check
          if(any(tmp_res_objs %in% names(DW_obj_map))){
            # Resource objects that are in the DW module:
            tmp_res_objs_DW = tmp_res_objs[tmp_res_objs %in% names(DW_obj_map)]

            # The subset of those that have not been processed yet
            tmp_res_objs_DW = tmp_res_objs_DW[!(tmp_res_objs_DW %in% names(DW_FM_source))]

            # If there is anything left we add that to be checked:
            if( length(tmp_res_objs_DW) > 0){
              DW_ds_objs_check = c( DW_ds_objs_check, tmp_res_objs_DW)
            }
          }

          # If res_obj is a data source we add it to FM_ds_objs
          if(any(tmp_res_objs %in% avail_ds[["FM_objs"]])){
            tmp_res_objs_FM = tmp_res_objs[ tmp_res_objs %in% avail_ds[["FM_objs"]] ]
            if( length(tmp_res_objs_FM) > 0){
              FM_ds_objs = c(FM_ds_objs, tmp_res_objs_FM)
            }
          }
        }
      }
    }


    # Pulling the code for the data sources
    code_sources = avail_ds[["catalog"]][avail_ds[["catalog"]][["object"]] %in% unique(FM_ds_objs) , ][["code"]]


    #DW_FM_source()
    # updating the all of the code:
    codeall = c("# Loading source data",
                code_sources,
                "",
                "# Data wrangling",
                code_view)
    

  } else {
    codeall = c("")
  }

  # Saving the dataview
  state[["DW"]][["views"]][[view_id]][["code"]] = paste(codeall, collapse="\n")

  # Forcing an checksum update
  state = DW_update_checksum(state)

state}


#'@export
#'@title Updates DW Module Checksum
#'@description Takes a DW state and updates the checksum used to trigger
#'downstream updates
#'@param state DW state from \code{DW_fetch_state()}
#'@return DW state object with the checksum updated
#'@examples
#' # Within shiny both session and input variables will exist,
#' # this creates examples here for testing purposes:
#' sess_res = DW_test_mksession()
#' session = sess_res$session
#' input   = sess_res$input
#'
#' # We also need a state variable
#' state = sess_res$state
#'
#' state = DW_update_checksum(state)
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

  new_checksum = digest::digest(chk_str, algo=c("md5"))

  if(has_updated(ui_val = new_checksum, old_val=state[["DW"]][["checksum"]])){
    state[["DW"]][["checksum"]] = new_checksum
    FM_le(state, paste0("module checksum updated:", state[["DW"]][["checksum"]]))
  }


state}


#'@export
#'@title Attach Data Set to DW View
#'@description Attaches a dataset to the DW state supplied.
#'@param state DW state from \code{DW_fetch_state()}
#'@param dw_view      Data view list of the format returned from \code{DW_fetch_current_view()}
#'@param ds_source_id Source id from \code{DSV[["catalog"]][["object"]]} for
#'the dataset.
#'@return state with data set attached
#'@examples
#' # Within shiny both session and input variables will exist,
#' # this creates examples here for testing purposes:
#' sess_res = DW_test_mksession()
#' session = sess_res$session
#' input   = sess_res$input
#'
#' # We also need a state variable
#' state = sess_res$state
#'
#' # We need to identify the UD module with the data
#' # JMH TODO: update example
#' dw_view = DW_fetch_current_view(state=state)
#' 
#' ds_source_id = names(state[["DW"]][["DSV"]][["ds"]])[1]
#' 
#' dw_view = DW_attach_ds(state=state, dw_view=dw_view, ds_source_id=ds_source_id)
DW_attach_ds = function(state, dw_view, ds_source_id){

  if(ds_source_id %in% names(state[["DW"]][["DSV"]][["ds"]])){
    # Setting the dataset details
    dw_view[["ds_source_id"]]       = ds_source_id
    dw_view[["WDS"]]                = state[["DW"]][["DSV"]][["ds"]][[ds_source_id]][["DS"]]
    dw_view[["ds_source_checksum"]] = state[["DW"]][["DSV"]][["ds"]][[ds_source_id]][["DSchecksum"]]
    dw_view[["code_previous"]]      = state[["DW"]][["DSV"]][["ds"]][[ds_source_id]][["code"]]
  } else {
    FM_le(state, paste0("data view id: ", dw_view[["id"]], ", key: ", dw_view[["key"]]), entry_type="danger")
    FM_le(state, paste0("undefined data source: ", ds_source_id), entry_type="danger")
    dw_view[["isgood"]] = FALSE
  }

dw_view}

#'@export
#'@title Adding Wrangling Element to Current Data View
#'@description Adds the wrangling element to the current data view.
#'@param state DW state from \code{DW_fetch_state()}
#'@param session Shiny session variable
#'@param dwb_res  Output from \code{dwrs_builder()}
#'@param dwee_res Output from \code{dw_eval_element()}
#'@param status   Wrangling element status
#'returned by \code{UD_fetch_state()}.
#'@return state with data set attached
#'@example inst/test_apps/DW_funcs.R
DW_add_wrangling_element = function(state, session, dwb_res, dwee_res, status="Success"){

  current_view = DW_fetch_current_view(state)
  # - append the cmd and description to the DW table
  dwe_key = paste0("DWE ", current_view[["dwe_cntr"]])

  # Tracking resources
  res_obj          = ""
  res_obj_DSchecksum = ""
  res_obj_mod_id   = ""
  if(!is.null(dwb_res[["res_obj"]])){
    res_obj          = dwb_res[["res_obj"]] }
  if(!is.null(dwb_res[["res_obj_DSchecksum"]])){
    res_obj_DSchecksum = dwb_res[["res_obj_DSchecksum"]]}
  if(!is.null(dwb_res[["res_obj_mod_id"]])){
    res_obj_mod_id = dwb_res[["res_obj_mod_id"]]}



  # Adding to the table
  current_view[["elements_table"]] =
    rbind(current_view[["elements_table"]],
      data.frame(
      Key                = dwe_key,
      Action             = dwb_res[["action"]],
      Description        = dwb_res[["desc"]],
      cmd                = paste0(dwb_res[["cmd"]], collapse="\n"),
      res_obj            = res_obj,
      res_obj_DSchecksum = res_obj_DSchecksum,
      res_obj_mod_id     = res_obj_mod_id,
      Status             = status,
      Delete             = FALSE))

  # Setting the view to good:
  current_view[["isgood"]] = TRUE

  # Adding to the preload list
  current_view[["elements_list"]][[dwe_key]][["pll"]]     = dwb_res[["pll"]]
  
  # Incrementing the counter
  current_view[["dwe_cntr"]] = current_view[["dwe_cntr"]] + 1

  paste0("old: ", digest::digest(current_view[["WDS"]], algo=c("md5")))
  paste0("new: ", digest::digest(dwee_res[["DS"]],      algo=c("md5")))

  current_view[["WDS"]]  = dwee_res[["DS"]]
  state = DW_set_current_view(state=state, session=session, dw_view=current_view)

state}

#'@title Adding Preload List  Component
#'@description Adds the wrangling component specified in the preload list
#'@param state DW state from \code{DW_fetch_state()}
#'@param session Shiny session variable
#'@param pll Preload list either from a preload file or
#'@return state with the preload list added. See the field
#'  \code{state[["DW"]][["res"]][["proc_pll"]]} for the exit status and any
#'  messages.
DW_proc_pll  = function(state, session=NULL, pll){

  msgs         = c()
  isgood       = TRUE
  current_view = DW_fetch_current_view(state)
  view_id      = current_view[["id"]]
  res_obj      = NULL

  add_component = TRUE

  # Here we construct the input based on the type of action selected
  state[["DW"]][["ui"]][["select_dw_element"]] = pll[["action"]]


  # If you start deleteing random stuff it can really break a data view. 
  # This will catch that so there are no errors in the UI and the user can 
  # see what happend and try to fix it. This mainly happens if you are merging a
  # dataset and the dataset has been deleted. 
  if(is.null(pll[["action"]])){
    pll[["action"]] = "empty action"
  }

  # pll_fields = c("column", "operator", "rhs", "names", "values", "method", "merge_rels_ui", "merge_rels")

  if(pll[["action"]] == "filter"){
    state[["DW"]][["ui"]][["select_fds_filter_column"]]   = pll[["column"]]
    state[["DW"]][["ui"]][["select_fds_filter_operator"]] = pll[["operator"]]
    state[["DW"]][["ui"]][["fds_filter_rhs"]]             = pll[["rhs"]]
  }else if(pll[["action"]] == "mutate"){
    state[["DW"]][["ui"]][["select_fds_mutate_column"]]   = pll[["column"]]
    state[["DW"]][["ui"]][["select_fds_mutate_rhs"]]      = pll[["rhs"]]
  }else if(pll[["action"]] == "rename"){
    state[["DW"]][["ui"]][["select_fds_rename_column"]]   = pll[["column"]]
    state[["DW"]][["ui"]][["fds_rename_rhs"]]             = pll[["rhs"]]
  }else if(pll[["action"]] == "group"){
    state[["DW"]][["ui"]][["select_fds_group_column"]]    = pll[["column"]]
  }else if(pll[["action"]] == "longer"){
    state[["DW"]][["ui"]][["select_fds_longer_column"]]    = pll[["column"]]
    state[["DW"]][["ui"]][["select_fds_longer_names"]]     = pll[["names"]]
    state[["DW"]][["ui"]][["select_fds_longer_values"]]    = pll[["values"]]
  }else if(pll[["action"]] == "wider"){
    state[["DW"]][["ui"]][["select_fds_wider_names"]]      = pll[["names"]]
    state[["DW"]][["ui"]][["select_fds_wider_values"]]     = pll[["values"]]
  }else if(pll[["action"]] == "select"){
    state[["DW"]][["ui"]][["select_fds_select_column"]]    = pll[["column"]]
  }else if(pll[["action"]] == "sort"){
    state[["DW"]][["ui"]][["select_fds_sort_column"]]      = pll[["column"]]
  }else if(pll[["action"]] == "merge"){
    # The merge source is an R object. This will fetch that:
    avail_ds  = DW_fetch_available_sources(state = state, session=session, dw_view = current_view)

    # Extracting the object for the data source being merged:
    fr_res = fetch_resource(
      catalog = avail_ds[["catalog"]],
      id        = pll[["data_source"]][["id"]],
      idx       = pll[["data_source"]][["idx"]],
      res_label = pll[["data_source"]][["res_label"]])

    # Making sure all of the merge components are defined.
    state[["DW"]][["ui"]][["select_fds_merge_method"]]     = ""
    if(!is.null(pll[["method"]])){
      state[["DW"]][["ui"]][["select_fds_merge_method"]]     = pll[["method"]] }

    if(!is.null(pll[["merge_rels_ui"]])){
    state[["DW"]][["merge_rels"]]                          = pll[["merge_rels_ui"]] }


    state[["DW"]][["ui"]][["select_fds_merge_source"]]     = ""
    res_obj                                                = ""
    if(!is.null(fr_res[["res_row"]][["object"]])){
      state[["DW"]][["ui"]][["select_fds_merge_source"]]     = fr_res[["res_row"]][["object"]]
      res_obj                                                = fr_res[["res_row"]][["object"]]
    }

    if(!fr_res[["isgood"]]){
      isgood        = FALSE
      msgs = c(msgs,
        paste0("view_id:        ",view_id),
        "failed to find data source",
        fr_res[["msgs"]]
      )
    }
  }else if(pll[["action"]] == "ungroup"){
  }else if(pll[["action"]] == "onerow"){
  }else{
    isgood        = FALSE
    add_component = FALSE
    msgs = c(msgs,
             paste0("view_id:        ",view_id),
             paste0("Unknown action: ",pll[["action"]])
             )
  }

  if(add_component){

    if("Failure" %in% current_view[["elements_table"]][["Status"]]){
      previous_failure = TRUE
    } else {
      previous_failure = FALSE
    }


    dwb_res  = dwrs_builder(state=state, session=session)
    dwee_res = dw_eval_element(state=state, session=session, cmd = dwb_res[["cmd"]], res_obj=res_obj)
    if(!isgood){
      dwb_res[["desc"]] = paste0(pll[["action"]], " failed")
      dwb_res[["code"]] = paste0("# ", pll[["action"]], " failed")
    }

    if(!previous_failure){
      # Capturing any failures:
      if(!dwb_res[["isgood"]]){
        isgood = FALSE
        msgs = c(msgs, paste0(view_id, ": dwrs_builder() failed"))
        msgs = c(msgs, dwb_res[["msgs"]])
      }
      if(!dwee_res[["isgood"]]){
        isgood = FALSE
        msgs = c(msgs, paste0(view_id, ": dw_eval_element() failed"))
        msgs = c(msgs, dwee_res[["msgs"]])
      }

      if(isgood){
        status = "Success"
      } else {
        status = "Failure"
      }
    } else {
      status = "Not Run"
    }

    #message(paste0("action: ", pll[["action"]], " status: ", status, " previous_failure:", previous_failure))

    state    = DW_add_wrangling_element(
                 state=state, 
                 session=session, 
                 dwb_res=dwb_res, 
                 dwee_res=dwee_res, 
                 status=status)
  }

  # Saving the exit status of the function
  state[["DW"]][["res"]][["proc_pll"]] = list(
    isgood = isgood,
    msgs   = msgs)
state}



#'@export
#'@title Fetch Module Code
#'@description Fetches the code to generate results seen in the app
#'@param state DW state from \code{DW_fetch_state()}
#'@return Character object vector with the lines of code
#'and isgood)
#'@examples
#' # This will create a formods DW state object for the example
#' sess_res = DW_test_mksession()
#' state   = sess_res$state
#' code = DW_fetch_code(state)
#' cat(code)
DW_fetch_code = function(state){

  # If there are no datasets we will return NULL otherwise we return the code
  code = NULL
  if(state[["DW"]][["DSV"]][["hasds"]]){
    codes = c()
    for(view_id in names(state[["DW"]][["views"]])){
      codes=c(codes,
              FM_build_comment(level=2, comment_str=state[["DW"]][["views"]][[view_id]][["key"]]),
              paste0(
                state[["DW"]][["views"]][[view_id]][["view_ds_object_name"]],
                " <- ", 
                state[["DW"]][["views"]][[view_id]][["ds_source_id"]]),
              state[["DW"]][["views"]][[view_id]][["code_dw_only"]],
              "")
    }
    code = paste(codes, collapse="\n")
  }
code}

#'@export
#'@title Append Report Elements
#'@description Takes the current state of the app and appends data views to an
#'xlsx report object.
#'@param state DW state from \code{DW_fetch_state()}
#'@param rpt Report with the current content of the report which will be appended to in
#'this function. For details on the structure see the documentation for \code{\link{FM_generate_report}}.
#'@param rpttype Type of report to generate (supported "xlsx").
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
#'
#'@examples
#' # We need a state object to use below
#' sess_res = DW_test_mksession()
#' state = sess_res$state
#'
#' rpt = list(summary = list(), sheets=list())
#'
#' rpt_res = DW_append_report(state,
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
#'@seealso \code{\link{FM_generate_report}}
DW_append_report = function(state, rpt, rpttype, gen_code_only=FALSE){

  isgood    = TRUE
  hasrptele = FALSE
  code      = c()
  msgs      = c()

  # The DW module only supports the xlsx report type
  supported_rpttypes = c("xlsx")

  if(rpttype %in% supported_rpttypes){
    # Walking through each data view
    for(view_id in names(state[["DW"]][["views"]])){
      # If the working dataset is a data frame we append it to the report
      if(is.data.frame(state[["DW"]][["views"]][[view_id]][["WDS"]])){
        hasrptele = TRUE
        # Storing the data frame in the object name used in the code
        assign(state[["DW"]][["views"]][[view_id]][["view_ds_object_name"]],
               state[["DW"]][["views"]][[view_id]][["WDS"]])

        # This appends the data frame to the report list
        code_chunk = c(paste0("# ", state[["DW"]][["views"]][[view_id]][["key"]] ),
                       paste0(
                            'rpt[["sheets"]][["',
                            state[["DW"]][["views"]][[view_id]][["view_ds_object_name"]],
                            '"]] <- ',
                            state[["DW"]][["views"]][[view_id]][["view_ds_object_name"]] ))
        # Evaluating the code
        if(!gen_code_only){
          eval(parse(text=code_chunk))}
        # Appending to returned code
        code = c(code, code_chunk)

        # Appends the mapping between sheet name and description:
        code_chunk = c('rpt[["summary"]] <- rbind(rpt[["summary"]],',
                       "  data.frame(",
                paste0('    Sheet_Name  = "',  state[["DW"]][["views"]][[view_id]][["view_ds_object_name"]], '",'),
                paste0('    Description = "', state[["DW"]][["views"]][[view_id]][["key"]], '"'),
                       "  )",
                       ')', "")
        # Evaluating the code
        if(!gen_code_only){
          eval(parse(text=code_chunk))}
        # Appending to returned code
        code = c(code, code_chunk)
      }
    }
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
#'@title Fetch Module Datasets
#'@description Fetches the datasets contained in the module.
#'@param state UD state from \code{UD_fetch_state()}
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
#'    \item{id: module ID}
#'    \item{idx: unique numerical ID to identify this dataset in the module.}
#'    \item{res_label: optional label that can be defined by a user and used in
#'    workflows. Must be unique to the module.}
#'    \item{DS: Dataframe containing the actual dataset.}
#'    \item{DSMETA: Metadata describing DS, see \code{FM_fetch_ds()} for
#'    details on the format.}
#'    \item{code: Complete code to build dataset.}
#'    \item{checksum: Module checksum.}
#'    \item{DSchecksum: Dataset checksum.}
#'  }
#'}
#'@examples
#' # We need a state variable
#' sess_res = DW_test_mksession()
#' state = sess_res$state
#'
#' ds = DW_fetch_ds(state)
DW_fetch_ds = function(state, meta_only=FALSE){

  hasds  = FALSE
  isgood = TRUE
  msgs   = c()
  ds     = list()

  # Empty list for new datasets
  NEWDS = list(label      = NULL,
               MOD_TYPE   = NULL,
               id         = NULL,
               idx        = NULL,
               res_label  = "",
               DS         = NULL,
               DSMETA     = NULL,
               code       = NULL,
               checksum   = NULL,
               DSchecksum = NULL)

  DW_checksum = state[["DW"]][["checksum"]]
  dw_views    = names(state[["DW"]][["views"]])

  # Walking through each view:
  for(dw_view in dw_views){
    tmp_checksum      = state[["DW"]][["views"]][[dw_view]][["checksum"]]
    tmp_object_name   = state[["DW"]][["views"]][[dw_view]][["view_ds_object_name"]]
    tmp_code          = state[["DW"]][["views"]][[dw_view]][["code"]]
    tmp_key           = state[["DW"]][["views"]][[dw_view]][["key"]]
    tmp_code_previous = state[["DW"]][["views"]][[dw_view]][["code_previous"]]

    tmp_et            = state[["DW"]][["views"]][[dw_view]][["elements_table"]]
    tmp_idx           = state[["DW"]][["views"]][[dw_view]][["idx"]]

    # The module code is the two chuncks pasted together
    modcode = paste(c(tmp_code), collapse="\n")
    if(is.null(modcode)){
      modcode = ""
    }
    if(is.null(tmp_key)){
      tmp_key = dw_view
    }

    # If the view is complete we append it to the ds list
    if(!is.null(tmp_checksum)    &
       !is.null(tmp_object_name) &
       !is.null(tmp_et)          &
       !is.null( state[["DW"]][["views"]][[dw_view]][["WDS"]])){

      TMPDS = NEWDS

      if(meta_only){
        TMPDS[["DS"]]         = NULL
      } else {
        TMPDS[["DS"]]         = state[["DW"]][["views"]][[dw_view]][["WDS"]]
      }

      TMPDS[["label"]]      = tmp_key
      TMPDS[["idx"]]        = tmp_idx
      TMPDS[["checksum"]]   = DW_checksum
      TMPDS[["DSchecksum"]] = tmp_checksum
      TMPDS[["code"]]       = modcode
      TMPDS[["MOD_TYPE"]]   = "DW"
      TMPDS[["id"]]         = state[["id"]]
      object_name           = tmp_object_name
      hasds                 = TRUE

      ds[[object_name]] = TMPDS
    }
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
#'@return The DW portion of the `all_sess_res` returned from \code{\link{FM_app_preload}}
#'@examples
#' session = shiny::MockShinySession$new()
#' sess_res = DW_test_mksession(session=session)
#'@seealso \code{\link{FM_app_preload}}
DW_test_mksession = function(session=list()){

# sources = c(system.file(package="formods", "preload", "ASM_preload.yaml"),
#             system.file(package="formods", "preload", "UD_preload.yaml"),
#             system.file(package="formods", "preload", "DM_preload.yaml"),
#             system.file(package="formods", "preload", "DW_preload.yaml"))
# res = FM_app_preload(session=session, sources=sources)
# res = res[["all_sess_res"]][["DW"]]

  pldir = tempfile(pattern="preload_")
  mpd_res = mk_preload_dir(
    directory = pldir,
    preload   = c(system.file(package="formods", "preload", "ASM_preload.yaml"),
                  system.file(package="formods", "preload", "DW_preload.yaml"),
                  system.file(package="formods", "preload", "DM_preload.yaml"),
                  system.file(package="formods", "preload", "UD_preload.yaml")),
    mod_yaml  = c( 
      system.file(package="formods",  "templates", "formods.yaml"),
      system.file(package="formods",  "templates", "ASM.yaml"),
      system.file(package="formods",  "templates", "DW.yaml"),
      system.file(package="formods",  "templates", "DM.yaml"),
      system.file(package="formods",  "templates", "UD.yaml")),
    include = list(
      UD = list(
        from = system.file(package="formods", "test_data", "TEST_DATA.xlsx"),
        to   = "TEST_DATA.xlsx" ),
      DM = list(
        path = file.path("data", "DM"),
        from = system.file(package="formods", "test_data", "TEST_DATA.xlsx"),
        to   = "TEST_DATA.xlsx" )
    )
  )
  
  old_dir = getwd()
  setwd(pldir)
  on.exit(setwd(old_dir))
  res = FM_app_preload(session=list(), sources="preload.yaml")
  res = res[["all_sess_res"]][["DW"]]

  setwd(old_dir)
  unlink(pldir, recursive = TRUE)

res}


#'@export
#'@title Check DW State For Datasets
#'@description Walks through the DW state object to see if there are any
#'datasets available
#'@param state DW state from \code{DW_fetch_state()}
#'@return Logical TRUE if there is a dataset or FALSE otherwise.
#'@examples
#' sess_res = DW_test_mksession()
#' state = sess_res[["state"]]
#' DW_hasds(state)
DW_hasds = function(state){
  hasds = FALSE
  dw_views    = names(state[["DW"]][["views"]])
  for(dw_view in dw_views){
    tmp_checksum      = state[["DW"]][["views"]][[dw_view]][["checksum"]]
    tmp_object_name   = state[["DW"]][["views"]][[dw_view]][["view_ds_object_name"]]
    tmp_contents      = state[["DW"]][["views"]][[dw_view]][["WDS"]]
    tmp_et            = state[["DW"]][["views"]][[dw_view]][["elements_table"]]
    if(!is.null(tmp_checksum)    &
       !is.null(tmp_object_name) &
       !is.null(tmp_et)          &
       !is.null(tmp_contents)){
       hasds = TRUE
    }
  }
hasds}

#'@export
#'@title Preload Data for DW Module
#'@description Populates the supplied session variable with information from
#'list of sources.
#'@param session     Shiny session variable (in app) or a list (outside of app)
#'@param src_list    List of preload data (all read together with module IDs at the top level)
#'@param yaml_res    List data from module yaml config
#'@param mod_ID      Module ID of the module being loaded
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
DW_preload  = function(session, src_list, yaml_res, mod_ID=NULL, react_state = list(), quickload=FALSE){
  isgood = TRUE
  input  = list()
  msgs   = c()

  FM_yaml_file  = render_str(src_list[[mod_ID]][["fm_yaml"]])
  MOD_yaml_file = render_str(src_list[[mod_ID]][["mod_yaml"]])
  id_UD         = yaml_res[[mod_ID]][["mod_cfg"]][["MC"]][["module"]][["depends"]][["id_UD"]]


  state = DW_fetch_state(id              = mod_ID,
                         input           = input,
                         session         = session,
                         FM_yaml_file    = FM_yaml_file,
                         MOD_yaml_file   = MOD_yaml_file,
                         react_state     = react_state)

  if(!formods::is_shiny(session)){
    session = FM_set_mod_state(session, mod_ID, state)
  }

  elements = src_list[[mod_ID]][["elements"]]

  if(!is.null(elements)){
    if(length(elements) > 0){
      # All of the numeric IDs in the preload
      enumeric    = c()

      # Map between list index and internal figure ID
      element_map = list()

      for(ele_idx in 1:length(elements)){
        enumeric = c(enumeric, elements[[ele_idx]][["element"]][["idx"]])
        element_map[[ paste0("view_",elements[[ele_idx]][["element"]][["idx"]] )]] = ele_idx
      }
      # Creating empty view placeholders
      while(state[["DW"]][["view_cntr"]] < max(enumeric)){
        state =DW_new_view(state=state, session=session)
      }
      # culling any unneeded views
      for(view_id in names(state[["DW"]][["views"]])){
        # This is a view that doesn't exist in elements so
        # we need to cull it
        if(!(view_id %in% names(element_map))){
          # Setting the view to be deleted as the current view
          state[["DW"]][["views"]][[ view_id ]] = NULL
        }
      }

      # Pulling out all the available datasets
      DSV = state[["DW"]][["DSV"]]

      # Now we have empty data views for the needed elements
      for(view_id in names(element_map)){
        state[["DW"]][["current_view"]] = view_id

        # Getting the numeric position in the list corresponding to the current
        # view id
        ele_idx = element_map[[view_id]]

        # first we set the name
        FM_le(state, paste0("loading data view idx: ", ele_idx))

        # Attaching dataset to the current view
        fr_res =
          fetch_resource(
            catalog   = DSV[["catalog"]],
            id        = elements[[ele_idx]][["element"]][["data_source"]][["id"]],
            idx       = elements[[ele_idx]][["element"]][["data_source"]][["idx"]],
            res_label = elements[[ele_idx]][["element"]][["data_source"]][["res_label"]])


        #----------
        if(fr_res[["isgood"]]){
          FM_le(state, paste0("  -> setting data source: ", fr_res[["res_obj"]]) )
          current_view = DW_fetch_current_view(state)
          #current_view[["ds_source_id"]] = fr_res[["res_obj"]]
          #current_view[["WDS"]]          = DSV[["ds"]][[ fr_res[["res_obj"]] ]][["DS"]]

          current_view = DW_attach_ds(state, current_view, fr_res[["res_obj"]])

          state = DW_set_current_view(state=state, session=session, dw_view=current_view)

          if(!is.null(elements[[ele_idx]][["element"]][["name"]])){
            FM_le(state, paste0("setting name: ", elements[[ele_idx]][["element"]][["name"]]))
            current_view = DW_fetch_current_view(state)
            current_view[["key"]] = elements[[ele_idx]][["element"]][["name"]]
            state = DW_set_current_view(state=state, session=session, dw_view=current_view)
          }

          # Now we walk through any components
          if(length(elements[[ele_idx]][["element"]][["components"]]) > 0){
            #-------------------------
            for(comp_idx in 1:length(elements[[ele_idx]][["element"]][["components"]])){
              tmp_pll       = elements[[ele_idx]][["element"]][["components"]][[comp_idx]][["component"]]
              FM_le(state, paste0("  -> ", tmp_pll[["action"]]))

              # If any failed before this, this will prevent more components
              # from being processed
              if(isgood){
                state = DW_proc_pll(state=state, session=session, pll=tmp_pll)
              }
              if(!state[["DW"]][["res"]][["proc_pll"]][["isgood"]]){
                 isgood = FALSE
                 msgs = c(msgs, state[["DW"]][["res"]][["proc_pll"]][["msgs"]])
              }
            }
            #-------------------------
          }
        } else {
          isgood = FALSE
          msgs = c(msgs, fr_res[["msgs"]])
        }
        #----------
        # We need to save the state so data views created here can be sources
        # for subsequent views that are created
        if(is_shiny(session)){
          FM_set_mod_state(session, mod_ID, state)
        } else {
          session = FM_set_mod_state(session, mod_ID, state)
        }
      }
      # Setting holds
      # Defaulting to the last view
      state[["DW"]][["current_view"]] = names(state[["DW"]][["views"]])[ length(names(state[["DW"]][["views"]])) ]
    }
  }

  # Setting holds:
  state = set_hold(state)

  # Required for proper reaction:
  react_state[[mod_ID]]  = list(DW  =
          list(checksum = state[["DW"]][["checksum"]],
               hasds    = DW_hasds(state)))

  formods::FM_le(state,paste0("module isgood: ",isgood))

  # Saving the state
  if(is_shiny(session)){
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
#'@title Make List of Current DW State
#'@description Converts the current DW state into a preload list.
#'@param state DW state object
#'@return list with the following elements
#' \itemize{
#'   \item{isgood:}       Boolean indicating the exit status of the function.
#'   \item{msgs:}         Messages to be passed back to the user.
#'   \item{yaml_list:}    Lists with preload components.
#'}
#'@examples
#' sess_res = DW_test_mksession()
#' state = sess_res$state
#' res = DW_mk_preload(state)
DW_mk_preload     = function(state){
  isgood    = TRUE
  msgs      = c()
  err_msg   = c()

  ylist     = list(
      fm_yaml  = file.path("config", basename(state[["FM_yaml_file"]])),
      mod_yaml = file.path("config", basename(state[["MOD_yaml_file"]])),
      res_deps = list(ds=list()),
      elements = list()
  )

  ele_idx = 1
  DSV = state[["DW"]][["DSV"]]

  # Walking through each element:
  for(element_id in names(state[["DW"]][["views"]])){
    tmp_source_ele = state[["DW"]][["views"]][[element_id]]

    # Finding the data source:
    dsv_row =
      DSV[["catalog"]][
        DSV[["catalog"]][["object"]] == tmp_source_ele[["ds_source_id"]],
        ]

    # Creates the empty element:
    tmp_element = list(
      idx  = tmp_source_ele[["idx"]],
      name = tmp_source_ele[["key"]],
      data_source = list(
        id        = dsv_row[["id"]],
        idx       = dsv_row[["idx"]],
        res_label = dsv_row[["res_label"]]),
      components = list())

    # Storing any resource labels used
    if(dsv_row[["res_label"]] != ""){
      ylist[["res_deps"]][["ds"]][[ dsv_row[["id"]] ]] = c(
        ylist[["res_deps"]][["ds"]][[ dsv_row[["id"]] ]], 
        dsv_row[["res_label"]]
      )
    }

    FM_le(state, paste0("saving element (", tmp_source_ele[["idx"]], ") ", tmp_source_ele[["key"]]))

    # Adding components:
    if(is.data.frame(tmp_source_ele[["elements_table"]])){
      comp_idx = 1
      for(tmp_key in tmp_source_ele[["elements_table"]][["Key"]]){
        if(tmp_key %in% names(tmp_source_ele[["elements_list"]])){
          if("pll" %in% names(tmp_source_ele[["elements_list"]][[tmp_key]])){

            # If the current component is a merge component we want to check and
            # see if the data source has a resource label. If it does then we
            # need to ad it to the res_deps for the module
            if(tmp_source_ele[["elements_list"]][[tmp_key]][["pll"]][["action"]] == "merge"){
              tmp_ds = tmp_source_ele[["elements_list"]][[tmp_key]][["pll"]][["data_source"]]
              if(tmp_ds[["res_label"]] != ""){
               ylist[["res_deps"]][["ds"]][[ tmp_ds[["id"]] ]] = c(
                 ylist[["res_deps"]][["ds"]][[ tmp_ds[["id"]] ]],
                 tmp_ds[["res_label"]])
              }
            }

            tmp_element[["components"]][[comp_idx]] = list(component=
              tmp_source_ele[["elements_list"]][[tmp_key]][["pll"]])
            FM_le(state, paste0("  -> ",tmp_key, ": ", tmp_source_ele[["elements_list"]][[tmp_key]][["pll"]][["action"]]) )
          } else {
            tmp_err_msg = paste0("  -> missing preload list (pll) for key: ", tmp_key)
            FM_le(state,  tmp_err_msg, entry_type="danger")
            err_msg     = c(err_msg, tmp_err_msg)
            isgood = FALSE
          }
        } else {
          tmp_err_msg = paste0("  -> missing key: ", tmp_key)
          FM_le(state,  tmp_err_msg, entry_type="danger")
          err_msg = c(err_msg, tmp_err_msg)
          isgood = FALSE
        }
        comp_idx = comp_idx + 1
      }
    }

    # Appending element
    ylist[["elements"]][[ele_idx]] = list(element = tmp_element)
    ele_idx = ele_idx + 1
  }


  formods::FM_le(state,paste0("mk_preload isgood: ",isgood))

  # Making sure the resource dependencies that were found are unique
  for(tmp_id in names( ylist[["res_deps"]][["ds"]] )){
    ylist[["res_deps"]][["ds"]][[tmp_id]] = 
      unique(ylist[["res_deps"]][["ds"]][[tmp_id]])
  }

  yaml_list = list()
  yaml_list[[ state[["id"]] ]]  = ylist

  if(!isgood && !is.null(err_msg)){
    msgs = c(msgs, err_msg)
  }

  res = list(
    isgood    = isgood,
    msgs      = msgs,
    yaml_list = yaml_list)
res}

#'@export
#'@title Rebuilds Data Views After Source Changes
#'@description Will attempt to rebuild data views when changes in the source datasets have been detected
#'@param state DW state object
#'@param session Shiny session variable
#'@param view_ids List of view ids to mend or NULL to attempt to mend all
#'@param id Shiny id of the DW module
#'@param id_UD Shiny id of the UD module
#'@param id_DM Shiny id of the DM module
#'@param force Boolean to force rebuild
#'@return State with dataviews rebuilt if dataset changes were detected
#'@examples
#' sess_res = DW_test_mksession()
#' state    = sess_res$state
#' session  = sess_res$session
#' view_ids = names(state[["DW"]][["views"]])[1]
#' state = DW_rectify(
#'   state    = state, 
#'   session  = session, 
#'   view_ids = view_ids,
#'   id = "DW", id_UD="UD", id_DM="DM")
DW_rectify = function(state, session, view_ids = NULL, id=NULL, id_UD = NULL, id_DM=NULL, force=FALSE){

  # View IDs to rectify:
  if(is.null(view_ids)){
    view_ids =  names(state[["DW"]][["views"]])
  }
  # Saving the old current view:
  old_current_view = state[["DW"]][["current_view"]]

  # JMH TODO: also add code here to check other dependencies such as on datasets
  # being merged:
  view_deps        = DW_fetch_view_deps(state, session, view_ids)
  view_ids_rectify = view_deps[["dep_catalog"]][["view_id"]]

  for(tmp_view_id in view_ids_rectify){
     # Updating the DSV because an upstream data view can break one downstream of it
     state[["DW"]][["DSV"]] = FM_fetch_ds(state, session, c(id_UD, id_DM))

     # Saving the state so that it will be in sync with the session
     FM_set_mod_state(session, id, state)


    # Flag to indicate if we need to rebuild the
    REBUILD_VIEW = FALSE
    RBLD_MSG = c()

    if(force){
      REBUILD_VIEW = TRUE
      RBLD_MSG = c(RBLD_MSG, "forcing rebuild")
    }


    # Setting the current view and pulling it out:
    state[["DW"]][["current_view"]] = tmp_view_id
    tmp_cv = DW_fetch_current_view(state)

    if( tmp_cv[["ds_source_id"]] %in% names(state[["DW"]][["DSV"]][["ds"]])){
      if(tmp_cv[["ds_source_checksum"]] != state[["DW"]][["DSV"]][["ds"]][[  tmp_cv[["ds_source_id"]] ]][["DSchecksum"]]){
        RBLD_MSG = c(RBLD_MSG, paste0("initial data source changed: ", tmp_cv[["ds_source_id"]]))
        REBUILD_VIEW = TRUE
      }
    } else {
      # This happens when a data source has been deleted:
      RBLD_MSG = c(RBLD_MSG, paste0("initial data source removed: ", tmp_cv[["ds_source_id"]]))
      REBUILD_VIEW = TRUE
    }

    # Checking for data sources other than the initial (e.g. merged data sources)
    if(any(tmp_cv[["elements_table"]][["res_obj_mod_id"]] != "")){
      avail_ds = DW_fetch_available_sources (state, session, dw_view=tmp_cv)
      # JMH check all of the sources in res_obj_DSchecksum to see if any have changed
      # if they have then we trigger a rebuild

      # This is all of the resources used in the current data view. So all of
      # the data sources both external (from UD or DM) and internal (from DW)
      tmp_res_all = tmp_cv[["elements_table"]][ tmp_cv[["elements_table"]][["res_obj"]]!="", ]
      for(tmp_res_obj in tmp_res_all[["res_obj"]]){
        if( tmp_res_obj %in% avail_ds[["catalog"]][["object"]]){

          # compareing the current checksum in the available dataset to the checksum in the source when the data view was last built
          avail_ds_checksum = avail_ds[["catalog"]][avail_ds[["catalog"]][["object"]] == tmp_res_obj, ][["DSchecksum"]]
          dv_ds_checksum = tmp_res_all[tmp_res_all[["res_obj"]] == tmp_res_obj, ][["res_obj_DSchecksum"]]

          # If these two are different then we need to trigger a rebuild
          if(avail_ds_checksum != dv_ds_checksum){
            RBLD_MSG = c(RBLD_MSG, paste0("element data source changed: ", tmp_cv[["ds_source_id"]]))
            REBUILD_VIEW = TRUE
          }
        }else{
          # This happens when a data source used in the element has been removed
          RBLD_MSG = c(RBLD_MSG, paste0("element data source removed: ", tmp_cv[["ds_source_id"]]))
          REBUILD_VIEW = TRUE
        }
      }
    }
 
    if(REBUILD_VIEW){
      FM_le(state, RBLD_MSG)
      state = DW_rebuild_current_view(state=state, session=session)
    }
  }
  # Restting the current view back to the old view:
  state[["DW"]][["current_view"]] = old_current_view

  state = DW_update_checksum(state)

state}

#'@export
#'@title Recursively Finds View Dependencies
#'@description For the view ids specified in view_ids this will walk through and find any data views that
#' depend on those view IDS and rebuild them if necessary.
#'@param state DW state object
#'@param session Shiny session variable
#'@param view_ids List of view ids to mend or NULL to attempt to mend all
#'@return list with the following elements
#'\itemize{
#'  \item{isgood:}       Return status of the function.
#'  \item{msgs:}         Messages to be passed back to the user.
#'  \item{dep_catalog:}  Dataframe of dependencies (including specified
#                        view_ids) sorted by idx with the following columns
#'  \itemize{
#'    \item{view_id:}      Data view ID (e.g., view_3)
#'    \item{idx:}          Numeric data view id (e.g. 3) 
#'    \item{checksum:}     Checksum of the data view 
#'    \item{object_name:}  R object name used (e.g., DW_myDS_2)
#'   }
#'  \item{dep_catalog_ex:} Same as deps_catalog but exculding  view_ids
#' }
#'@examples
#' sess_res = DW_test_mksession()
#' state    = sess_res$state
#' session  = sess_res$session
#' view_ids = names(state[["DW"]][["views"]])[1]
#' view_deps = DW_fetch_view_deps(
#'   state    = state, 
#'   session  = session, 
#'   view_ids = view_ids)
DW_fetch_view_deps = function(state, session, view_ids = NULL){
  isgood = TRUE
  msgs   = c()
  dep_catalog    = NULL
  dep_catalog_ex = NULL

  # All view IDs in the app
  all_view_ids = names(state[["DW"]][["views"]])

  if(!is.null(view_ids)){
    view_ids_to_proc =view_ids
    while(length(view_ids_to_proc) > 0){
      # Pulling off the first view id
      tmp_view_id = view_ids_to_proc[1]
      # Removing from the list of view ids to process
      view_ids_to_proc = view_ids_to_proc[-1]

      if(length(which(all_view_ids == tmp_view_id))>0){
       
        # This is the current view being processed:
        tmp_view = state[["DW"]][["views"]][[tmp_view_id]]
       
        # Adding the view to the depencency catalog:
        dep_catalog = rbind(dep_catalog,
          data.frame(
            view_id     = tmp_view[["id"]],
            idx         = tmp_view[["idx"]],
            checksum    = tmp_view[["checksum"]],
            object_name = tmp_view[["view_ds_object_name"]]
          )
        )
       
        # Checking for any ids that depend on the current view.
        # First we get all of the view ids afer this one 
        if(length(all_view_ids) > which(all_view_ids == tmp_view_id)){
          # If there are view ids after this one we walk through them to see if
          # the current view (tmp_view) is used in them
          for(chk_dep_view_id in all_view_ids[(which(all_view_ids == tmp_view_id)+1):length(all_view_ids)]){
       
            # Make sure the hk_dep_view_id is not in the to be added (view_ids_to_proc) or 
            # already added list (dep_catalog[["view_id"]]). Otherwise we skip them
            if(!(chk_dep_view_id %in%  view_ids_to_proc) & !(chk_dep_view_id %in% dep_catalog[["view_id"]])){
              if(length(state[["DW"]][["views"]][[chk_dep_view_id]][["elements_table"]][["res_obj"]])> 0){
       
                # If any of the object names in the catalog of dependencies are found in the current view ID being checked
                # then we add that view id to the list to be processed. 
                if(any(dep_catalog[["object_name"]] %in% state[["DW"]][["views"]][[chk_dep_view_id]][["elements_table"]][["res_obj"]])){
                 # Now we check if any of the object names are in the elements table of the view id to be checked. 
                  view_ids_to_proc = c(view_ids_to_proc, chk_dep_view_id)
                }
              }
            }
          }
        }
      } else {
        isgood = FALSE
        msgs = c(msgs, paste0("the specified view_id was not found: ", paste0(view_ids, collapse=", ")))
      }
    }
    #sorting dep_catalog by idx so it will be in an order to process
    if(!is.null(dep_catalog)){
      dep_catalog    = dep_catalog[order(dep_catalog[["idx"]]), ]
      dep_catalog_ex = dep_catalog[!(dep_catalog[["view_id"]] %in% view_ids), ]
    }
  }

  res = list(
    isgood         = isgood,
    msgs           = msgs,
    dep_catalog    = dep_catalog,
    dep_catalog_ex = dep_catalog_ex
  )

res}




#'@title Forces Rebuild of Current Data View
#'@description Will attempt to rebuild data views when changes in the source
#' datasets have been detected.
#'@param state DW state object
#'@param session Shiny session variable
#'@return State with current data view rebuilt. Any messages will be stored in
#'\code{state[["DW"]][["res"]][["rebuild_current_view"]][["msgs"]]}
#'@examples
#' # JMH add examples
DW_rebuild_current_view = function(state, session){
  # First we pull out the current view and
  current_view = DW_fetch_current_view(state)

  # This is the id of the data source:
  current_source_id     = current_view[["ds_source_id"]]

  # tracking view status
  view_isgood = TRUE

  # Messages to pass back to the user
  msgs = c()
  FM_le(state, FM_build_comment(level = 1, comment_str=paste0('rebuilding "', current_view[["key"]], '" (',current_view[["id"]], ')')))

  # Pulling the current elements list:
  NEW_EL = current_view[["elements_list"]]


  # Making sure the data source still exists:
  if(current_source_id %in% names(state[["DW"]][["DSV"]][["ds"]])){

    # Resetting the element aspects of the data view. These should essentially
    # match the values in DW_new_view. We're resetting these:
    current_view[["dwe_cntr"]]       = 1
    current_view[["elements_list"]]  = list()
    current_view[["elements_table"]] = NULL
    current_view[["WDS"]]            = NULL
    current_view[["checksum"]]       = NULL
    current_view[["code"]]           = NULL
    current_view[["code_dw_only"]]   = NULL


    # Now we reattach the data source so we start adding components to the
    # original data view:
    current_view = DW_attach_ds(state, current_view,  current_view[["ds_source_id"]])

    # Now we save these as the current view to be populated below:
    state = DW_set_current_view(state=state, session=session, dw_view=current_view)

    # We use this so we can report only the first error
    ERROR_FOUND = FALSE

    if(length(NEW_EL) > 0){
      for(ename in names(NEW_EL)){
        state = DW_proc_pll(state=state, pll=NEW_EL[[ename]][["pll"]], session=session)
        # Detecting any issues with adding the element
        if(!state[["DW"]][["res"]][["proc_pll"]][["isgood"]] & !ERROR_FOUND){
          msgs = c(msgs, state[["DW"]][["res"]][["proc_pll"]][["msgs"]])
          ERROR_FOUND = TRUE
          view_isgood = FALSE
          notify_text = state[["MC"]][["notifications"]][["rebuild_failed"]]
          state = FM_set_notification(state, notify_text, "Rebuild failed", "failure")
        }
      }
    }
  } else {
    current_view[["WDS"]]            = NULL
    current_view[["code"]]           = NULL
    current_view[["code_dw_only"]]   = NULL
    if(length(current_view[["elements_table"]][["Status"]])>0){
      current_view[["elements_table"]][["Status"]][1] = "Failure"
      if(length(current_view[["elements_table"]][["Status"]])>1){
        current_view[["elements_table"]][["Status"]][ 
          2:length(current_view[["elements_table"]][["Status"]]) ] = "Not Run"
      }
    }

    ERROR_FOUND = TRUE
    view_isgood = FALSE
    notify_text = state[["MC"]][["notifications"]][["dw_source_missing"]]
    notify_text = stringr::str_replace_all(notify_text, "===DSID===",   current_source_id)
    notify_text = stringr::str_replace_all(notify_text, "===DVID===",   current_view[["id"]])
    notify_text = stringr::str_replace_all(notify_text, "===VIEW===",   current_view[["key"]])
    state = FM_set_notification(state, notify_text, "DW data source missing", "failure")

    # Now we save these as the current view as initialized above
    state = DW_set_current_view(state=state, session=session, dw_view=current_view)
  }

  # Pulling out the current view again
  current_view = DW_fetch_current_view(state)
  FM_le(state, paste0("rebuild successful:  ", !ERROR_FOUND))
  FM_le(state, paste0("data view isgood:   ", view_isgood))
  if(!is.null(msgs)){
    FM_le(state, msgs)
  }
  state[["DW"]][["res"]][["rebuild_current_view"]][["msgs"]] = msgs

  # Passing any messages back to the user
  state = FM_set_ui_msg(state, msgs)

  current_view[["isgood"]] = view_isgood
  state = DW_set_current_view(state=state, session=session, dw_view=current_view)
  state = DW_update_checksum(state)
  state}

#'@export
#'@title Fetch Available Source
#'@description 
#' Returns a list of avaliable data sources for a given data wrangling
#' element. This includes any modules the DW module can pull from (e.g. UD and
#' DM) as well as any data views defined before the provided \code{dw_view}.
#' If \code{dw_view} is NULL then only the sources from the modules DW is
#' dependent on will be returned. 
#'@param state DW state object
#'@param session Shiny session variable
#'@param dw_view Optoinal data view to use as a reference point for DW data sources
#'@return list with the following elements
#'\itemize{
#'   \item{choices:} List of values to use with selectInput()/pickerInput()
#'   \itemize{
#'     \item{plain:}  Raw vector of choices
#'     \item{labeled:} Named vector of choices
#'     \item{grouped:} Choices grouped by module
#'   }
#'   \item{catalog:} Dataframe containing the a tabular catalog of the
#'   datasets found.
#'   \itemize{
#'     \item{label:} Text label
#'     \item{object:} Name of the R Object containing the data frame
#'     \item{MOD_TYPE:} Short name of the type of module
#'     \item{id:} Module ID
#'     \item{idx:} Numerical identifier within the module
#'     \item{res_label: optional label that can be defined by a user and used in
#'     workflows. Must be unique to the module.}
#'     \item{checksum:} Module checksum
#'     \item{DSchecksum:} Checksum of the dataset
#'     \item{code:} Code to generate the dataset
#'   }
#'   \item{FM_objs:} List of object names that come from other formods modules
#'   \item{DW_objs:} List of object names that come from the current DW module 
#'}
#'@example inst/test_apps/DW_funcs.R
DW_fetch_available_sources = function(state, session, dw_view=NULL){

  isgood = TRUE
  msgs   = c()
  res    = list()
  FM_objs= c()
  DW_objs= c()

  res[["choices"]] = state[["DW"]][["DSV"]][["choices"]]
  res[["catalog"]] = state[["DW"]][["DSV"]][["catalog"]]
  FM_objs= state[["DW"]][["DSV"]][["catalog"]][["object"]]

  if(!is.null(dw_view)){
    # This pulls all available data sources from the DW module:
    DW_sources = FM_fetch_ds(state=state, session=session, ids = state[["id"]], meta_only=TRUE )

    if(DW_sources[["hasds"]] & DW_sources[["isgood"]]){
      # Now we need to trim it down to just those defined 
      # _before_ the current one:
      current_view = DW_fetch_current_view(state)
      catalog_keep = dplyr::filter(DW_sources[["catalog"]], .data[["idx"]] < current_view[["idx"]])

      if(nrow(catalog_keep) > 0){
        # This will keep only the data views created before the current data
        # view:
        choices_plain_keep    = catalog_keep[["object"]] 
        choices_labeled_keep  = DW_sources[["choices"]][["labeled"]][DW_sources[["choices"]][["labeled"]] %in% catalog_keep[["object"]] ]
  
        tmp_grp_name = names(DW_sources[["choices"]][["grouped"]])[1]
        choices_grouped_keep  = list()
        choices_grouped_keep[[tmp_grp_name]] = DW_sources[["choices"]][["grouped"]][[tmp_grp_name]][
           DW_sources[["choices"]][["grouped"]][[tmp_grp_name]] %in% catalog_keep[["object"]] ]

        # Tracking the objects provided by the current module separately
        # fromn the other formods modules
        DW_objs= c(DW_objs, catalog_keep[["object"]])
  
        # Appending the choices:
        res[["catalog"]]              = rbind(res[["catalog"]], catalog_keep)
        res[["choices"]][["plain"]]   = c( res[["choices"]][["plain"]],   choices_plain_keep)
        res[["choices"]][["labeled"]] = c( res[["choices"]][["labeled"]], choices_labeled_keep)
        res[["choices"]][["grouped"]] = c( res[["choices"]][["grouped"]], choices_grouped_keep)

      } else {
        isgood = FALSE
        msgs = "unable to find current data view"
      }
    }
  }
                   
  res[["isgood"]]   = isgood
  res[["msgs"]]     = msgs
  res[["FM_objs"]]  = FM_objs
  res[["DW_objs"]]  = DW_objs

res}

#'@export
#'@title Fetches Metadata for a Data Source Based on R Object Name
#'@description  Data sources can be either resources from other modules (e.g.
#' UD and DM) or previously defined dataviews. This function is a simple
#' interface to retrieve a data source based on the source ID or label. 
#'@description  Resources such as datasets can be specified using the module
#' ID and either identifier index or the resource label. 
#' This function will attempt to rectify those and give the "best" answer. If a
#' resource label is supplied and exists uniquely for that module ID, that
#' object will be returned. If not then an error will be returned. If the
#' resource label is not specified and the index is specified and that exists
#' uniquely, that object will be returned. Otherwise an error will be returned.
#'@param state DW state from \code{DW_fetch_state()}
#'@param session Shiny session variable
#'@param dw_view Optoinal data view to use as a reference point for DW data sources set to NULL if only formods sources are to be used.
#'@param ds_obj  Name of object that contains data source 
#'@return list containing the current dataset with the following format:
#' \itemize{
#'   \item{isgood:}  Boolean indicating the whether a resource object was found.
#'   \item{msgs:}    Any messages generated
#'   \item{res_row:} Row of catalog associated with resource or \code{NULL} if not found.
#'   \item{DS:}      Data frame of the current resource
#' }
#'@example inst/test_apps/DW_funcs.R
DW_fetch_obj_ds_meta = function(state, session, dw_view=NULL, ds_obj=NULL){

  isgood  = TRUE
  msgs    = c()
  DS      = NULL
  res_row = NULL

  avail_ds  = DW_fetch_available_sources(state = state, session=session, dw_view = dw_view)

  if(avail_ds[["isgood"]]){
    ds_source = NULL
    
    if( ds_obj %in% avail_ds[["FM_objs"]]){
      ds_source = "FM"
    } else if( ds_obj %in% avail_ds[["DW_objs"]]){
      ds_source = "DW"
    }
    
    if(!is.null(ds_source)){
       res_row = avail_ds[["catalog"]][avail_ds[["catalog"]][["object"]] == ds_obj, ]

       # Pulling out the dataset 
       if(ds_source == "FM"){


         # If its from formods we can use the value from FM_fetch_ds stored
         # in the state. This shoudl be updated any time there is a chang
         # upstream of the current DW module:
         DS = state[["DW"]][["DSV"]][["ds"]][[ds_obj]][["DS"]]
       } else if(ds_source == "DW"){
         # Mapping the object names in the data wranling module to the index used
         # for storage in the state object
         DW_obj_map <- setNames(as.list(names(state[["DW"]][["views"]])), 
                                sapply(state[["DW"]][["views"]], 
                                       function(x) x[["view_ds_object_name"]]))

         # If it's from the current DW module we map the object name to the
         # view id and then pull out the working dataset for that data view:
         DS = state[["DW"]][["views"]][[ DW_obj_map[[ds_obj]] ]][["WDS"]]
       }
    } else {
      isgood = FALSE
      msgs = c(msgs, paste0("unable to find source details for: ",ds_obj))
    }
  } else {
    isgood = FALSE
    msgs = c(msgs, "no data sources available, see below for details.")
    msgs = c(msgs, avail_ds[["msgs"]])
  }

  res = list(
    isgood  = isgood,
    DS      = DS,
    res_row = res_row,
    msgs    = msgs)

res}
