#'@import rhandsontable
#'@import shiny
#'@import formods
#'@importFrom digest digest
#'@importFrom shinyAce aceEditor updateAceEditor

#'@export
#'@title Data Management Server
#'@description Server function for the Data Management  Shiny Module
#'@param id An ID string that corresponds with the ID used to call the modules UI elements
#'@param FM_yaml_file App configuration file with FM as main section.
#'@param MOD_yaml_file  Module configuration file with MC as main section.
#'@param deployed Boolean variable indicating whether the app is deployed or not.
#'@param react_state Variable passed to server to allow reaction outside of module (\code{NULL})
#'@return UD Server object
#'@example inst/test_apps/FM_compact.R
DM_Server <- function(id,
               FM_yaml_file  = system.file(package = "formods", "templates", "formods.yaml"),
               MOD_yaml_file = system.file(package = "formods",  "templates", "DM.yaml"),
               deployed      = FALSE,
               react_state   = NULL) {
  moduleServer(id, function(input, output, session) {

    MOD_yaml_cont = FM_read_yaml(MOD_yaml_file)
    id_ASM = MOD_yaml_cont[["MC"]][["module"]][["depends"]][["id_ASM"]]

    #------------------------------------
    # Select the active ds
    output$DM_ui_select_element = renderUI({
      input$button_clk_save
      input$button_clk_del
      input$button_clk_copy
      input$button_clk_new
      input$DM_hot_resources
      input$DM_hot_ds_preview

      state = DM_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)
      choices = list()
      content = c()
      HL_COLOR = state[["yaml"]][["FM"]][["ui"]][["color_orange"]]
      for(element_id in names(state[["DM"]][["elements"]])){


        choices[[ state[["DM"]][["elements"]][[element_id]][["ui"]][["element_name"]] ]] = element_id

        if(state[["DM"]][["elements"]][[element_id]][["isgood"]]){
          content = c(content,
              paste0(
               "<div>",
                state[["DM"]][["elements"]][[element_id]][["ui"]][["element_name"]],
                "</div>"))
        } else {
          content = c(content,
              paste0(
               "<div style='font-style:bold; background: ", HL_COLOR ,"; color: black;'>",
                state[["DM"]][["elements"]][[element_id]][["ui"]][["element_name"]],
                "</div>"))
        }

      }

      choicesOpt = list( content = content)

      uiele =
      shinyWidgets::pickerInput(
        selected   = state[["DM"]][["current_element"]],
        inputId    = NS(id, "element_selection"),
        label      = state[["MC"]][["labels"]][["current_element"]],
        choicesOpt = choicesOpt,
        choices    = choices,
        width      = state[["MC"]][["formatting"]][["current_element"]][["width"]])

      uiele = formods::FM_add_ui_tooltip(state, uiele,
         tooltip     = state[["MC"]][["formatting"]][["current_element"]][["tooltip"]],
         position    = state[["MC"]][["formatting"]][["current_element"]][["tooltip_position"]])

      uiele})
    #------------------------------------
    # Table with list of files
    output$DM_hot_resources    = rhandsontable::renderRHandsontable({
      input$button_file_upload
      input$button_clk_get_url
      input$DM_hot_resources

      state = DM_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      files_df = state[["DM"]][["defined_sources"]]
      uiele = NULL
      if(nrow(files_df) == 0){
        files_df = data.frame(files=state[["MC"]][["errors"]][["no_files"]])
        uiele    = rhandsontable::rhandsontable(files_df,
                     stretchH   = "all",
                     width      = state[["MC"]][["formatting"]][["preview"]][["width"]],
                     height     = state[["MC"]][["formatting"]][["preview"]][["height"]],
                     rowHeaders = NULL) |>
          hot_col("files", valign='htCenter')
      } else {
        # Removing unnecessary columns
        files_df[["checksum"]]    = NULL
        files_df[["full_path"]]   = NULL
        files_df[["file_name"]]   = NULL
        files_df[["url"]]         = NULL
        files_df[["excel_file"]]  = NULL
        files_df[["rel_path"]]    = NULL
        files_df[["size_num"]]    = NULL

        names(files_df)[names(files_df) == "source_type"] <- "Type"

        # Fixing the File at 50% of the column width and distributing the rest
        # equally among the other columns:
        FW = state[["MC"]][["formatting"]][["preview"]][["width"]]*.5
        OCW = (state[["MC"]][["formatting"]][["preview"]][["width"]]-FW)/(ncol(files_df)-1)
        colWidths = rep(OCW, ncol(files_df))
        colWidths[which(names(files_df) == "Resource")] = FW

        uiele    = rhandsontable::rhandsontable(files_df,
                     width      = state[["MC"]][["formatting"]][["preview"]][["width"]],
                     height     = state[["MC"]][["formatting"]][["preview"]][["height"]],
                     rowHeaders = NULL) |>
          hot_col("ID",     valign='htCenter')   |>
          hot_col("Delete", valign='htCenter', width = 40 )   |>
          hot_col("Type",   valign='htCenter')   |>
          hot_cols(colWidths = colWidths)
      }



    uiele})
    #------------------------------------
    # Table with list of files
    output$DM_hot_ds_preview =  rhandsontable::renderRHandsontable({
      input$ui_dm_code
      state = DM_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      current_ele  = DM_fetch_current_element(state)
      uiele = NULL
      if(current_ele[["isgood"]]){
        if(!is.null(current_ele[["res"]][["run_code"]][["ds"]]) &
           !is.null(current_ele[["res"]][["run_code"]][["isgood"]])){
           if(current_ele[["res"]][["run_code"]][["isgood"]]){
             df = current_ele[["res"]][["run_code"]][["ds"]]
             browser()
             hfmt = FM_fetch_data_format(df, state)
             uiele = rhandsontable::rhandsontable(
               df,
               width      = state[["MC"]][["formatting"]][["preview"]][["width"]],
               height     = state[["MC"]][["formatting"]][["preview"]][["height"]],
               colHeaders = as.vector(unlist(hfmt[["col_heads"]])),
               rowHeaders = NULL
               )
          }
        }
      }


      if(is.null(uiele)){
         tmp_df = data.frame(data_source=state[["MC"]][["errors"]][["no_ds"]])
         uiele    = rhandsontable::rhandsontable(tmp_df,
                      stretchH   = "all",
                      width      = state[["MC"]][["formatting"]][["preview"]][["width"]],
                      height     = state[["MC"]][["formatting"]][["preview"]][["height"]],
                      rowHeaders = NULL) |>
           hot_col("data_source", valign='htCenter')
      }

    uiele})
    #------------------------------------
    # Table with list of files
    output$DM_hot_preview  = rhandsontable::renderRHandsontable({
      input$ds_sheet
      input$source_id
      input$clean_ds
      state = DM_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)
      uiele = NULL
    uiele})
    #------------------------------------
    # File upload form
    output$DM_ui_file_upload    = renderUI({
      state = DM_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)


      uiele =
        tagList(
          fileInput(NS(id, "button_file_upload"),
                    label    = state[["MC"]][["labels"]][["file_upload"]],
                    width    = state[["MC"]][["formatting"]][["file_upload"]][["width"]],
                    multiple = TRUE))
      uiele})
    #------------------------------------
    # Data source file (creating the picker)
    output$DM_ui_ds_sheet   = renderUI({
      input$button_clk_save
      input$button_clk_del
      input$button_clk_copy
      input$button_clk_new
      input$button_clk_get_url
      input$button_file_upload
      input$element_selection
      input$DM_hot_resources
      req(input$source_id)
      state = DM_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      current_ele = DM_fetch_current_element(state)
      srcnfo = DM_fetch_source(state = state, element = current_ele)

      uiele = NULL
      if(srcnfo[["excel_file"]]){
        choices  = c("PH")
        selected = "PH"
        if(!is.null( current_ele[["ui"]][["source_id"]])){
          choices = readxl::excel_sheets(path=srcnfo[["full_path"]])
          selected = choices[1]
          if(!is.null( srcnfo[["sheet"]])){
            if(srcnfo[["sheet"]]  %in% choices){
              # If the current element has a valid value we overwrite the
              # default here:
              selected = srcnfo[["sheet"]]
            }
          }
        }

        uiele =
          shinyWidgets::pickerInput(
            inputId    = NS(id, "ds_sheet"),
            label      = state[["MC"]][["labels"]][["ds_sheet"]],
            selected   = selected,
            choices    = choices,
            width      = state[["MC"]][["formatting"]][["ds_sheet"]][["width"]])
      }


      uiele})
    #------------------------------------
    # Data source file (populating/updating the picker)
    observe({
      input$button_clk_save
      input$button_clk_del
      input$button_clk_copy
      input$button_clk_new
      input$button_clk_get_url
      input$element_selection
      input$button_file_upload
      input$source_id
      input$DM_hot_resources
      req(input$source_id)

      force_mod_update[["triggered"]]

      state = DM_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)


      current_ele = DM_fetch_current_element(state)
      srcnfo = DM_fetch_source(state = state, element = current_ele)

      if(srcnfo[["excel_file"]]){
        choicesOpt = NULL
        choices = readxl::excel_sheets(path=srcnfo[["full_path"]])
        # Defaulting to the first file
        selected = choices[1]
        if(!is.null( srcnfo[["sheet"]])){
          if(srcnfo[["sheet"]]  %in% choices){
            # If the current element has a valid value we overwrite the
            # default here:
            selected = srcnfo[["sheet"]]
          }
        }
        shinyWidgets::updatePickerInput(
          session    = session,
          selected   = selected,
          inputId    = "ds_sheet",
          choices    = choices,
          choicesOpt = choicesOpt)
      }
    })
    #------------------------------------
    # Data source file (creating the picker)
    output$DM_ui_source_id    = renderUI({
      input$button_clk_save
      input$button_clk_del
      input$button_clk_copy
      input$button_clk_new
      input$button_file_upload
      input$button_clk_get_url
      input$element_selection
      input$DM_hot_resources
      force_mod_update[["triggered"]]
      state = DM_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      current_ele = DM_fetch_current_element(state)

      uiele = NULL
      if(nrow(state[["DM"]][["defined_sources"]]) > 0){

        if(!is.null( current_ele[["ui"]][["source_id"]])){
          choices = state[["DM"]][["defined_sources"]][["ID"]]
          names(choices) = state[["DM"]][["defined_sources"]][["Resource"]]
          selected = current_ele[["ui"]][["source_id"]]
        } else {
          choices  = c("PH")
          selected = "PH"
        }

        uiele =
          shinyWidgets::pickerInput(
            inputId    = NS(id, "source_id"),
            label      = state[["MC"]][["labels"]][["source_id"]],
            selected   = selected,
            choices    = choices,
            width      = state[["MC"]][["formatting"]][["source_id"]][["width"]])
      } else {
        uiele = state[["MC"]][["errors"]][["no_files_mkres"]]
      }

      uiele})
    #------------------------------------
    # Data get url button
    output$DM_ui_button_get_url = renderUI({
      #input$button_clk_save
      #input$button_clk_del
      #input$button_clk_copy
      #input$button_clk_new
      input$element_selection
      state = DM_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      current_ele = DM_fetch_current_element(state)
      srcnfo = DM_fetch_source(state = state, element=current_ele)

      uiele =
        shinyWidgets::actionBttn(
          inputId = NS(id, "button_clk_get_url"),
          label   = state[["MC"]][["labels"]][["get_url_btn"]],
          style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
          size    = state[["MC"]][["formatting"]][["button_clk_get_url"]][["size"]],
          block   = state[["MC"]][["formatting"]][["button_clk_get_url"]][["block"]],
          color   = "primary",
          icon    = icon("cloud-arrow-down"))


      uiele = div(style=paste0("width:",
                               state[["MC"]][["formatting"]][["button_clk_get_url"]][["width"]]),
                  uiele)
      # Optinally adding the tooltip:
      uiele =
        formods::FM_add_ui_tooltip(state, uiele,
          tooltip     = state[["MC"]][["formatting"]][["button_clk_get_url"]][["tooltip"]],
          position    = state[["MC"]][["formatting"]][["button_clk_get_url"]][["tooltip_position"]])

      uiele})
    #------------------------------------
    # Data source url
    output$DM_ui_source_url   = renderUI({
      input$element_selection
      state = DM_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      current_ele = DM_fetch_current_element(state)

      # JMH update value below with srcnfo
      srcnfo = DM_fetch_source(state = state, element=current_ele)

      uiele =
      textInput(
        inputId     = NS(id, "source_url"),
        label       = state[["MC"]][["labels"]][["source_url"]],
        width       = state[["MC"]][["formatting"]][["source_url"]][["width"]] ,
        value       = state[["DM"]][["ui"]][["source_url"]],
        placeholder = state[["MC"]][["formatting"]][["source_url"]][["placeholder"]]
      )

      uiele})
    #------------------------------------
    # Data source file (populating/updating the picker)
    observe({
      input$button_clk_save
      input$button_clk_del
      input$button_clk_copy
      input$button_clk_new
      input$button_file_upload
      input$button_clk_get_url
      input$element_selection
      input$DM_hot_resources
      req(input$source_id)

      force_mod_update[["triggered"]]

      state = DM_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      current_ele = DM_fetch_current_element(state)

      # Only updating the selection if there are files:
      if(nrow(state[["DM"]][["defined_sources"]]) > 0){
        choicesOpt = NULL
        choices = state[["DM"]][["defined_sources"]][["ID"]]
        names(choices) = state[["DM"]][["defined_sources"]][["Resource"]]

        # Defaulting to the first file
        selected = choices[1]
        if(!is.null( current_ele[["ui"]][["source_id"]])){
          if(current_ele[["ui"]][["source_id"]]  %in% choices){
            # If the current element has a valid value we overwrite the
            # default here:
            selected = current_ele[["ui"]][["source_id"]]
          }
        }

        shinyWidgets::updatePickerInput(
          session    = session,
          selected   = selected,
          inputId    = "source_id",
          choices    = choices,
          choicesOpt = choicesOpt)
      }
    })
    #------------------------------------
    # Current ds name:
    output$DM_ui_clean_ds = renderUI({
      input$button_clk_save
      input$button_clk_del
      input$button_clk_copy
      input$button_clk_new
      input$button_clk_get_url
      input$element_selection
      input$DM_hot_resources
      input$ds_sheet
      req(input$source_id)

      state = DM_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      uiele = NULL

      if(is_installed("janitor")){

        current_ele = DM_fetch_current_element(state)
        srcnfo = DM_fetch_source(state = state, element = current_ele)

        if(srcnfo[["isgood"]]){
          uiele =
            shinyWidgets::materialSwitch(
               inputId = NS(id, "clean_ds"),
               label   = state[["MC"]][["labels"]][["clean_ds"]],
               value   = as.logical(current_ele[["ui"]][["clean_ds"]]),
               status  = "success"
            )

          uiele = FM_add_ui_tooltip(state, uiele,
                  tooltip     = state[["MC"]][["formatting"]][["clean_ds"]][["tooltip"]],
                  position    = state[["MC"]][["formatting"]][["clean_ds"]][["tooltip_position"]])

        }
      } else {
       uiele = tagList(tags$b(state[["MC"]][["errors"]][["no_janitor"]]), tags$br(), tags$br())
      }

    })
    #------------------------------------
    # Current ds label
    output$DM_ui_res_label = renderUI({
      input$element_selection
      input$DM_hot_ds_preview
      input$DM_hot_resources
      state = DM_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      current_ele = DM_fetch_current_element(state)
      srcnfo = DM_fetch_source(state = state, element = current_ele)
      uiele = NULL

      if(srcnfo[["isgood"]]){
        uiele =
        textInput(
          inputId     = NS(id, "res_label"),
          label       = state[["MC"]][["labels"]][["res_label"]],
          width       = state[["MC"]][["formatting"]][["res_label"]][["width"]] ,
          value       = current_ele[["ui"]][["res_label"]],
          placeholder = state[["MC"]][["formatting"]][["res_label"]][["placeholder"]]
        )

        uiele = formods::FM_add_ui_tooltip(state, uiele,
          tooltip     = state[["MC"]][["formatting"]][["res_label"]][["tooltip"]],
          position    = state[["MC"]][["formatting"]][["res_label"]][["tooltip_position"]])
      }

    uiele})
    #------------------------------------
    # Current ds label
    output$DM_ui_res_label_val = renderUI({
      input$res_label
      input$element_selection
      input$DM_hot_ds_preview
      input$DM_hot_resources
      state = DM_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      current_ele = DM_fetch_current_element(state)
      srcnfo = DM_fetch_source(state = state, element = current_ele)
      uiele = NULL

      if(srcnfo[["isgood"]]){
        is_val = TRUE

        clashes = c()
        #message("fix res_label_val")
        # If the label is undefined then it's fine
        if(current_ele[["ui"]][["res_label"]] != ""){
          # JMH add logic to validate label
          for(element_id in names(state[["DM"]][["elements"]])){
            if(state[["DM"]][["current_element"]] != element_id){
              if(state[["DM"]][["elements"]][[element_id]][["ui"]][["res_label"]] != ""){
                # Both the current element and the element being tested
                # (identified by element_id) have non "" values for res_label,
                # so if they are the same we flag this as a conflict.
                if(state[["DM"]][["elements"]][[element_id]][["ui"]][["res_label"]] ==
                   current_ele[["ui"]][["res_label"]]){
                   clashes = c(clashes,
                     paste0(state[["DM"]][["elements"]][[element_id]][["ui"]][["element_name"]],
                     " (", element_id, ")")
                   )
                  is_val = FALSE
                }
              }
            }
          }
        }


        if(is_val){
           val_str     = state[["MC"]][["formatting"]][["res_label_val"]][["val_good"]]
           tooltip_str = state[["MC"]][["formatting"]][["res_label_val"]][["tooltip_good"]]
        } else {
           val_str = state[["MC"]][["formatting"]][["res_label_val"]][["val_bad"]]
           tooltip_str = state[["MC"]][["formatting"]][["res_label_val"]][["tooltip_bad"]]

           tooltip_str = stringr::str_replace_all(
             string       = tooltip_str,
             pattern      = "===CONFLICTS===",
             replacement  = paste0(clashes, collapse=", "))
        }

        uiele = render_str(val_str)
        uiele = formods::FM_add_ui_tooltip(state, uiele,
          tooltip     = tooltip_str,
          position    = state[["MC"]][["formatting"]][["res_label_val"]][["tooltip_position"]])

        render_str(val_str)
      }

    uiele})
    #------------------------------------
    # Current ds name:
    output$DM_ui_text_element_name = renderUI({
      input$element_selection
      state = DM_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      current_ele = DM_fetch_current_element(state)

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
    output$DM_ui_element = renderUI({
      state = DM_fetch_state(id              = id,
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
      input$button_clk_save
      input$button_clk_del
      input$button_clk_copy
      input$button_clk_new
      input$button_clk_get_url
      input$element_selection
      input$DM_hot_resources
      input$source_id
      input$source_type
      input$source_id
      input$ds_sheet
      input$clean_ds

      # Reacting to file changes
      state = DM_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      current_ele  = DM_fetch_current_element(state)
      if(is.null( current_ele[["code_ele_only"]])){
        uiele = state[["MC"]][["errors"]][["no_code"]]
      } else {
        uiele = current_ele[["code_ele_only"]]
      }

      shinyAce::updateAceEditor(
        session         = session,
        editorId        = "ui_dm_code",
        theme           = state[["yaml"]][["FM"]][["code"]][["theme"]],
        showLineNumbers = state[["yaml"]][["FM"]][["code"]][["showLineNumbers"]],
        readOnly        = state[["MC"]][["code"]][["readOnly"]],
        mode            = state[["MC"]][["code"]][["mode"]],
        value           = uiele)

    })
    #------------------------------------
    # Side buttons:
    # new
    output$ui_dm_new_btn = renderUI({
      state = DM_fetch_state(id              = id,
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
    output$ui_dm_save_btn = renderUI({
      state = DM_fetch_state(id        = id,
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
    output$ui_dm_clip_code = renderUI({
      state = DM_fetch_state(id              = id,
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
    output$ui_dm_del_btn   = renderUI({
      state = DM_fetch_state(id              = id,
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
    output$ui_dm_copy_btn   = renderUI({
      state = DM_fetch_state(id              = id,
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
    output$ui_dm_msg = renderText({
      input$ui_dm_code
      input$button_clk_get_url

      state = DM_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      uiele = state[["DM"]][["ui_msg"]]

      uiele})
    # Creates the ui for the compact view of the module
    #------------------------------------
    # Compact ui
    output$DM_ui_compact  =  renderUI({
      state = DM_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)


      uiele_code_button = NULL
      # Generating code button if enabled
      if( state[["MC"]][["compact"]][["code"]]){
        uiele_code = tagList(shinyAce::aceEditor(
          NS(id, "ui_dm_code"),
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
           status  = "danger btn-custom-dm",
           icon    = icon("code", lib="font-awesome"),
           tooltip = shinyWidgets::tooltipOptions(title = state[["MC"]][["tooltips"]][["show_code"]]))
        )

      }

      # Button with DM elements table
      uiele_dm_elements_button = NULL
     # Uncomment this if your ds has a components table
     #uiele_dm_elements = rhandsontable::rHandsontableOutput(NS(id, "hot_dm_elements"))
     #uiele_dm_elements_button = tagList(
     # shinyWidgets::dropdownButton(
     #   uiele_dm_elements,
     #   inline  = FALSE,
     #   right   = TRUE ,
     #   size    = "sm",
     #   circle  = FALSE,
     #   status  = "primary btn-custom-dm",
     #   icon    = icon("layer-group", lib="font-awesome"),
     #   tooltip = shinyWidgets::tooltipOptions(title = state[["MC"]][["tooltips"]][["elements"]]))
     #)

      uiele = tagList(
        div(style="display:inline-block", htmlOutput(NS("DM", "DM_ui_select_element"))),
        div(style="display:inline-block", htmlOutput(NS("DM", "DM_ui_text_element_name"))),
        tags$br(),
        div(style="display:inline-block", verbatimTextOutput(NS(id, "ui_dm_msg")))
      )

      # We only show the clip button if it's enabled
      uiele_clip_button = NULL
      if(state[["MC"]][["compact"]][["clip"]]){
        uiele_clip_button = htmlOutput(NS(id, "ui_dm_clip_code"))
      }

      uiele_buttons_right = tagList(
               tags$style(".btn-custom-dm {width: 100px;}"),
               div(style="display:inline-block;vertical-align:top;height:100px",
               uiele_dm_elements_button,
               uiele_code_button,
               uiele_clip_button,
               htmlOutput(NS(id, "ui_dm_save_btn")),
               htmlOutput(NS(id, "ui_dm_copy_btn")),
               htmlOutput(NS(id, "ui_dm_del_btn")),
               htmlOutput(NS(id, "ui_dm_new_btn"))
               ))
      # Appending the preview
      div_style = paste0("display:inline-block;vertical-align:top;",
        "width:",   state[["MC"]][["formatting"]][["preview"]][["width"]],  ";",
        "height: ", state[["MC"]][["formatting"]][["preview"]][["height"]])
      uiele_preview = div(style=div_style,
        shinydashboard::tabBox(width=12,
          shiny::tabPanel(
            id = NS(id, "ds_tab"),
            title=tagList(
                          icon(state[["MC"]][["formatting"]][["preview"]][["ds_tab"]][["icon"]]),
                               state[["MC"]][["formatting"]][["preview"]][["ds_tab"]][["label"]]
                           ),
            rhandsontable::rHandsontableOutput(NS(id, "DM_hot_ds_preview"))
          ),
          shiny::tabPanel(
            id = NS(id, "resources_tab"),
            title=tagList(
                          icon(state[["MC"]][["formatting"]][["preview"]][["resources_tab"]][["icon"]]),
                               state[["MC"]][["formatting"]][["preview"]][["resources_tab"]][["label"]]
                           ),
            rhandsontable::rHandsontableOutput(NS(id, "DM_hot_resources"))
          )
        )
      )
      uiele = tagList(
        uiele,
        uiele_preview,
        uiele_buttons_right,
        tags$br()
      )

      uiele = tagList( uiele,
        tags$br(),
        formods::render_str(state[["MC"]][["labels"]][["dataset_header"]]),
        div(style="display:inline-block", htmlOutput(NS(id, "DM_ui_source_id"))),
        div(style="display:inline-block", htmlOutput(NS(id, "DM_ui_res_label"))),
        div(style="display:inline-block", htmlOutput(NS(id, "DM_ui_res_label_val"))),
        tags$br(),
        div(style="display:inline-block", htmlOutput(NS(id, "DM_ui_ds_sheet"))),
        div(style="display:inline-block", htmlOutput(NS(id, "DM_ui_clean_ds"))),
        tags$br(),
        formods::render_str(state[["MC"]][["labels"]][["resources_header"]]),
        htmlOutput(NS(id, "DM_ui_file_upload")),
        div(style="display:inline-block",   htmlOutput(NS(id, "DM_ui_source_url"))),
        div(style="display:inline-block",   htmlOutput(NS(id, "DM_ui_button_get_url")))
      )
      uiele
    })

    #------------------------------------
    # Creating reaction if a variable has been specified
    force_mod_update = reactiveValues()
    if(!is.null(react_state)){
      observe({
        react_state[[id_ASM]]

        state = DM_fetch_state(id             = id,
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
        list(
            #input$button_file_upload,
            #input$button_clk_get_url,
            #input$button_clk_new,
            #input$button_clk_del,
            #input$button_clk_copy,
             input$button_clk_save,
             input$ui_dm_code
             )
      })
      # This updates the reaction state:
      observeEvent(toListen(), {
        state = DM_fetch_state(id        = id,
                               input           = input,
                               session         = session,
                               FM_yaml_file    = FM_yaml_file,
                               MOD_yaml_file   = MOD_yaml_file,
                               react_state     = react_state)

        hasds = DM_hasds(state) 
        FM_le(state, paste0("reaction state updated (hasds: ", hasds, ")"))
        react_state[[id]][["DM"]][["hasds"]]    = hasds
        react_state[[id]][["DM"]][["checksum"]] = state[["DM"]][["checksum"]]
      }, priority=-101)
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
       input$button_clk_new,
       input$button_clk_get_url,
       input$button_file_upload,
       input$DM_hot_resources,
       input$source_id,
       input$ds_sheet,
       input$clean_ds
      )
    })
    observeEvent(toNotify(), {
      state = DM_fetch_state(id              = id,
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
      state = DM_fetch_state(id              = id,
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
          current_ele = DM_fetch_current_element(state)
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
             input$button_clk_new,
             input$button_clk_del,
             input$button_clk_copy,
           # input$button_clk_save,
             input$element_selection
           # input$current_element
           )
      })
    observeEvent(remove_hold_listen(), {
      # Once the UI has been regenerated we
      # remove any holds for this module
      state = DM_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      FM_le(state, "removing holds")
      # Removing all holds
      for(hname in names(state[["DM"]][["ui_hold"]])){
        remove_hold(state, session, hname)
      }
    }, priority = -100)

  })
}

#'@export
#'@title Fetch Data Management State
#'@description Merges default app options with the changes made in the UI
#'@param id Shiny module ID
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
#' \item{DM:}
#' \itemize{
#'   \item{isgood:} Boolean object indicating if the file was successfully loaded.
#'   \item{checksum:} This is an MD5 sum of the contents element and can be
#'   used to detect changes in the state.
#' }
#'  \item{MOD_TYPE:} Character data containing the type of module \code{"DM"}
#'  \item{id:} Character data containing the module id module in the session variable.
#'  \item{FM_yaml_file:} App configuration file with FM as main section.
#'  \item{MOD_yaml_file:}  Module configuration file with MC as main section.
#'}
#'@examples
#' # Within shiny both session and input variables will exist,
#' # this creates examples here for testing purposes:
#' sess_res = DM_test_mksession()
#' session = sess_res$session
#' input   = sess_res$input
#'
#' # Configuration files
#' FM_yaml_file  = system.file(package = "formods", "templates", "formods.yaml")
#' MOD_yaml_file = system.file(package = "formods", "templates", "DM.yaml")
#'
#' # Creating an empty state object
#' state = DM_fetch_state(id              = "DM",
#'                        input           = input,
#'                        session         = session,
#'                        FM_yaml_file    = FM_yaml_file,
#'                        MOD_yaml_file   = MOD_yaml_file,
#'                        react_state     = NULL)
DM_fetch_state = function(id, input, session, FM_yaml_file, MOD_yaml_file, react_state){

  # Template for an empty dataset
  #---------------------------------------------
  # Getting the current state
  state = FM_fetch_mod_state(session, id)
  # If the state has not yet been defined then we
  # initialize it

  if(is.null(state)){
    # General state information
    state = DM_init_state(FM_yaml_file, MOD_yaml_file, id, session)
  }

  #---------------------------------------------
  # Here we update the state based on user input
  for(ui_name in state[["DM"]][["ui_ids"]]){
    if(!is.null(isolate(input[[ui_name]]))){
       state[["DM"]][["ui"]][[ui_name]] = isolate(input[[ui_name]])
     } else {
       if(ui_name %in% names(state[["DM"]][["button_counters"]])){
         state[["DM"]][["ui"]][[ui_name]] = 0
       } else {
         state[["DM"]][["ui"]][[ui_name]] = ""
       }

       # initializing the previous ui values as well:
       if(is.null(state[["DM"]][["ui_old"]][[ui_name]])){
         state[["DM"]][["ui_old"]][[ui_name]] = state[["DM"]][["ui"]][[ui_name]]
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
  current_ele = DM_fetch_current_element(state)
  # There are scenarios where you wouldn't want to do this. Like when
  # switching elements in the ui. You would need to add some logic to
  # only update below conditionally.

  for(ui_name in state[["DM"]][["ui_ids"]]){
    if(!fetch_hold(state, ui_name)){
      if(ui_name %in% names(state[["DM"]][["button_counters"]])){
        # Button changes are compared to the button click tracking values
        change_detected =
          has_updated(ui_val   = state[["DM"]][["ui"]][[ui_name]],
                      old_val  = state[["DM"]][["button_counters"]][[ui_name]],
                      init_val = c("", "0"))

        if(change_detected){
          formods::FM_le(state, paste0("button click: ", ui_name, " = ", state[["DM"]][["ui"]][[ui_name]]))

          # Saving the change:
          state[["DM"]][["button_counters"]][[ui_name]] =
            state[["DM"]][["ui"]][[ui_name]]

          # logging the changed ui name:
          changed_uis = c(changed_uis, ui_name)
        }
      }else{
        change_detected =
          has_updated(ui_val   = state[["DM"]][["ui"]][[ui_name]],
                      old_val  = state[["DM"]][["ui_old"]][[ui_name]],
                      init_val = c(""))
        if(change_detected){
          changed_data_str = paste(state[["DM"]][["ui"]][[ui_name]], collapse=", ")
          changed_data_str = substr(changed_data_str, 1, 70)
          formods::FM_le(state, paste0("setting ds: ", ui_name, " = ", changed_data_str))

          # Saving the change:
          state[["DM"]][["ui_old"]][[ui_name]] = state[["DM"]][["ui"]][[ui_name]]

          # logging the changed ui name:
          changed_uis = c(changed_uis, ui_name)

          # This also updates the current element if that ui_name is part of
          # an element
          if(ui_name %in% state[["DM"]][["ui_ele"]]){
            current_ele[["ui"]][[ui_name]] = state[["DM"]][["ui"]][[ui_name]]
          }
        }
      }
    }
  }
  # Updating the element with any changes:
  state = DM_set_current_element(
    state   = state,
    element = current_ele)

  #---------------------------------------------
  # Here we react to changes between the UI and the current state
  # save ds
  if("button_clk_save" %in% changed_uis){
    FM_le(state, "save ds")
    current_ele = DM_fetch_current_element(state)

    current_ele[["ui"]][["element_name"]] =
      state[["DM"]][["ui"]][["element_name"]]

    state = DM_set_current_element(
      state   = state,
      element = current_ele)
  }
  #---------------------------------------------
  # clip ds
  if("button_clk_clip" %in% changed_uis){
    FM_le(state, "clip ds")
  }
  #---------------------------------------------
  # copy ds
  if("button_clk_copy" %in% changed_uis){
    FM_le(state, "copy ds")

    # First we pull out the current element:
    old_ele = DM_fetch_current_element(state)

    # Now we create a new element and make it the current element
    state   = DM_new_element(state)
    new_ele = DM_fetch_current_element(state)


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

    state = DM_set_current_element(
      state   = state,
      element = new_ele)
  }
  #---------------------------------------------
  # del ds
  if("button_clk_del" %in% changed_uis){
    FM_le(state, "delete ds")
    state = DM_del_current_element(state)
  }
  #---------------------------------------------
  # new ds
  if("button_clk_new" %in% changed_uis){
    FM_le(state, "new ds")
    state = DM_new_element(state)
    #browser()

    if(state[["DM"]][["ui"]][["source_id"]]!= ""){
      current_ele = DM_fetch_current_element(state)
      current_ele[["ui"]][["source_id"]]=state[["DM"]][["ui"]][["source_id"]]
      state = DM_set_current_element(
        state   = state,
        element = current_ele)
    }


    state = set_hold(state)
    state[["DM"]][["button_counters"]][["button_clk_new"]] = 
      state[["DM"]][["ui"]][["button_clk_new"]]
  }
  #---------------------------------------------
  # uploading file
  if("button_file_upload" %in% changed_uis){
    FM_le(state, "processing file uploads")
    upload_isgood = TRUE
    upload_msgs   = c()

    for(ridx in 1:nrow(state[["DM"]][["ui"]][["button_file_upload"]])){
      state = DM_add_file(
        state     = state,
        file_path = state[["DM"]][["ui"]][["button_file_upload"]][ridx, ][["datapath"]],
        file_name = state[["DM"]][["ui"]][["button_file_upload"]][ridx, ][["name"]])

      if(!state[["DM"]][["res"]][["add_file"]][["isgood"]]){
        upload_isgood = FALSE
      }
      upload_msgs = c(upload_msgs, state[["DM"]][["res"]][["add_file"]][["msgs"]])
    }

    if(upload_isgood){
      state = FM_set_notification(state       = state,
                                  type        = "success",
                                  notify_id   = "add_files",
                                  notify_text = paste0(upload_msgs, collapse=", "))
    } else {
      msgs = c(msgs, paste0(upload_msgs, collapse="\n"))
      state = FM_set_notification(state       = state,
                                  type        = "failure",
                                  notify_id   = "add_files",
                                  notify_text = paste0(upload_msgs, collapse=", "))
    }

  }
  #---------------------------------------------
  # uploading file
  if("DM_hot_resources" %in% changed_uis){
    FM_le(state, "changes in DM_hot_resources")
    hfdf = rhandsontable::hot_to_r(state[["DM"]][["ui"]][["DM_hot_resources"]])
    if("Delete" %in% names(hfdf)){
      if(any(hfdf[["Delete"]])){
        FM_le(state, "deleting file")

        # Pulling out the current list of files
        files_df = state[["DM"]][["defined_sources"]]
        # Records to delete
        rec_del = hfdf[which(hfdf[["Delete"]]), ]

        del_isgood = TRUE
        del_msgs   = c()

        for(ridx in 1:nrow(rec_del)){
          state = DM_delete_source(state = state, id = rec_del[ridx, ][["ID"]])
        }
        if(!state[["DM"]][["res"]][["del_file"]][["isgood"]]){
          del_isgood = FALSE
        }
        del_msgs = c(del_msgs, state[["DM"]][["res"]][["del_file"]][["msgs"]])

        if(del_isgood){
          state = FM_set_notification(state       = state,
                                      type        = "success",
                                      notify_id   = "add_files",
                                      notify_text = paste0(del_msgs, collapse=", "))
        } else {
          msgs = c(msgs, paste0(del_msgs, collapse="\n"))
          state = FM_set_notification(state       = state,
                                      type        = "failure",
                                      notify_id   = "add_files",
                                      notify_text = paste0(del_msgs, collapse=", "))
        }
      }
    }
  }
  #---------------------------------------------
  # Getting urls as they are triggered
  if("button_clk_get_url" %in% changed_uis){
    FM_le(state, "fetching url")
    current_ele = DM_fetch_current_element(state)

    #bad_url   = "https://www.healthline.com/health/high-blood-pressure-%hypertension"
    #bad_url   = "https://urldoesnotexist.xyz/"
    #excel_url = "https://github.com/john-harrold/formods/raw/master/inst/test_data/TEST_DATA.xlsx"
    #csv_url   = "https://raw.githubusercontent.com/john-harrold/ruminate/refs/heads/main/inst/test_data/dose_from_cols.csv"
    #state$DM$ui$source_url = excel_url
    ui_url = state[["DM"]][["ui"]][["source_url"]]



    state = DM_add_url( state = state,  url = ui_url)

    if(state[["DM"]][["res"]][["add_url"]][["isgood"]]){
      FM_le(state, "url attached")
      state = FM_set_notification(
        state       = state,
        type        = "success",
        notify_id   = "add_url_success",
        notify_text = state[["MC"]][["errors"]][["add_url_success"]])
    } else {
      FM_le(state, "url add failure", entry_type="danger")
      # Messaging failure to attach url:
      formods::FM_le(
        state,
        state[["DM"]][["res"]][["add_url"]][["msgs"]],
        entry_type="danger")

      msgs = c(msgs, state[["DM"]][["res"]][["add_url"]][["msgs"]])

      state = FM_set_notification(
        state       = state,
        type        = "failure",
        notify_id   = "add_url_failure",
        notify_text = state[["MC"]][["errors"]][["add_url_failed"]])
    }

    state = DM_set_current_element(
      state   = state,
      element = current_ele)
  }
  #---------------------------------------------
  # source (file, url, etc) changed so we update the dataset
  ui_force_update = c("source_id",
                      "button_clk_new",
                      "button_clk_copy",
                      "button_clk_get_url",
                      "DM_hot_resources",
                      "clean_ds",
                      "ds_sheet")
  if(any(ui_force_update %in% changed_uis)){
    current_ele = DM_fetch_current_element(state)

    FM_le(state, "updating code")
    #srcnfo = DM_fetch_source(state, current_ele)

    # Update element code:
    current_ele = DM_update_element_code(
      state   = state,
      element = current_ele,
      session = session)

    # Removing any previous run_code results:
    current_ele[["res"]][["run_code"]][["isgood"]] = FALSE
    current_ele[["res"]][["run_code"]][["msgs"]]   = "Not run yet."
    current_ele[["res"]][["run_code"]][["ds"]]     = NULL

    if(current_ele[["res"]][["code_update"]][["isgood"]]){
      # Running the code
      FM_le(state, "running code")
      current_ele = DM_run_code(
        state   = state,
        element = current_ele)

      # Messaging failure to run code
      if(!current_ele[["res"]][["run_code"]][["isgood"]]){
        formods::FM_le(
          state,
          current_ele[["res"]][["run_code"]][["msgs"]],
          entry_type="danger")
        msgs = c(msgs, current_ele[["res"]][["run_code"]][["msgs"]])
        state = FM_set_notification(
          state       = state,
          type        = "failure",
          notify_id   = "run_code_failure",
          notify_text = state[["MC"]][["errors"]][["run_code_failed"]])
      }
    }

    state = DM_set_current_element(
      state   = state,
      element = current_ele)
  }
  #---------------------------------------------
  # selected ds changed
  if("element_selection" %in% changed_uis){
    state[["DM"]][["current_element"]] =
       state[["DM"]][["ui"]][["element_selection"]]

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
#'@title Initialize DM Module State
#'@description Creates a list of the initialized module state
#'@param FM_yaml_file App configuration file with FM as main section.
#'@param MOD_yaml_file  Module configuration file with MC as main section.
#'@param id ID string for the module.
#'@param session Shiny session variable
#'@return list containing an empty DM state
#'@examples
#' # Within shiny both session and input variables will exist,
#' # this creates examples here for testing purposes:
#' sess_res = DM_test_mksession()
#' session = sess_res$session
#' input   = sess_res$input
#'
#' state = DM_init_state(
#'    FM_yaml_file  = system.file(package = "formods",
#'                                "templates",
#'                                "formods.yaml"),
#'    MOD_yaml_file = system.file(package = "formods",
#'                                "templates",
#'                                "DM.yaml"),
#'    id              = "DM",
#'    session         = session)
#'
#' state
DM_init_state = function(FM_yaml_file, MOD_yaml_file,  id, session){

  MOD_yaml_contents = FM_read_yaml(MOD_yaml_file)
  button_counters = MOD_yaml_contents[["MC"]][["ui_ids"]][["buttons"]]
  ui_module       = MOD_yaml_contents[["MC"]][["ui_ids"]][["module"]]

  # These are the module ui elements that are associated with
  # the current element
  ui_ele          = MOD_yaml_contents[["MC"]][["ui_ids"]][["element"]]
  ui_ids          = c(button_counters, ui_ele, ui_module)

  # Making all the ui_ids holdable
  ui_hold         = ui_ids

  state = FM_init_state(
    FM_yaml_file    = FM_yaml_file,
    MOD_yaml_file   = MOD_yaml_file,
    id              = id,
    # Add module dependencies here
    dep_mod_ids     = NULL,
    MT              = "DM",
    button_counters = button_counters,
    ui_ids          = ui_ids,
    ui_hold         = ui_hold,
    session         = session)

  # Storing the ui_ids for the elements
  state[["DM"]][["ui_ele"]]               = ui_ele

  # This tracks elements for the module
  state[["DM"]][["code_previous"]]        = NULL
  state[["DM"]][["elements"]]             = NULL
  state[["DM"]][["current_element"]]      = NULL
  state[["DM"]][["element_cntr"]]         = 0

  # pulling out the user directory and defining the directory to hold uploaded
  # files:
  user_dir      = FM_fetch_user_files_path(state)
  state[["DM"]][["data_dir_full"]] = file.path(user_dir, "data", id)
  state[["DM"]][["data_dir_rel"]]  = file.path(          "data", id)

  if(!dir.exists( state[["DM"]][["data_dir_full"]] )){
    dir.create(path = state[["DM"]][["data_dir_full"]], recursive=TRUE)
  }

  # Stores details for uploaded files
  state[["DM"]][["defined_sources"]] = data.frame()
  state[["DM"]][["resource_cntr"]]      =  1

  # Creating a default element:
  state = DM_new_element(state)

  # initializing the module checksum:
  state = DM_update_checksum(state)

  FM_le(state, "State initialized")

  # Saving the state (must be done before FM_fetch_deps below)
  FM_set_mod_state(session=session, id=id, state=state)

  # Setting the module dependencies
  state[["DM"]][["mod_deps"]] = FM_fetch_deps(state, session)

state}

#'@export
#'@title Fetch Module Code
#'@description Fetches the code to generate results seen in the app
#'@param state DM state from \code{DM_fetch_state()}
#'@return Character object vector with the lines of code
#'@example inst/test_apps/DM_funcs.R
DM_fetch_code = function(state){

  codes = c()
  for(element_id in names(state[["DM"]][["elements"]])){
    tmp_source_ele = state[["DM"]][["elements"]][[element_id]]
    if(tmp_source_ele[["isgood"]]){
      codes = c(codes,
        FM_build_comment(2, tmp_source_ele[["ui"]][["element_name"]]),
        tmp_source_ele[["code"]])
    }
  }

  if(is.null(codes)){
    code = NULL
  } else {
    code = paste(codes, collapse="\n")
  }

code}

#'@export
#'@title Append Report Elements
#'@description Appends report elements to a formods report.
#'@param state DM state from \code{DM_fetch_state()}
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
#' sess_res = DM_test_mksession()
#' state = sess_res$state
#'
#' rpt = list(summary = list(), sheets=list())
#'
#' rpt_res = DM_append_report(state,
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
DM_append_report = function(state, rpt, rpttype, gen_code_only=FALSE){

  isgood    = TRUE
  hasrptele = FALSE
  code      = c()
  msgs      = c()


  # The DM module only supports the following report types:
  supported_rpttypes = c("xlsx")

  if(rpttype %in% supported_rpttypes){
    if(state[["DM"]][["isgood"]]){
      for(element_id in names(state[["DM"]][["elements"]])){
        tmp_source_ele = state[["DM"]][["elements"]][[element_id]]
        if(tmp_source_ele[["isgood"]]){
          hasrptele = TRUE
          #tmp_source_ele[["ui"]][["element_name"]]
          #tmp_source_ele[["ui"]][["res_label"]]
          #tmp_source_ele[["objs"]][["element_object_name"]]
          #tmp_source_ele[["res"]][["run_code"]][["ds"]]

          #------------------------------------------------
          # This appends the data frame to the report list
          code_chunk = c(paste0('# ', tmp_source_ele[["ui"]][["element_name"]]),
                         paste0(
                              'rpt[["sheets"]][["',
                              tmp_source_ele[["objs"]][["element_object_name"]],
                              '"]] <- ',
                              tmp_source_ele[["objs"]][["element_object_name"]]))
          # Evaluating the code
          if(!gen_code_only){
            assign(tmp_source_ele[["objs"]][["element_object_name"]],
                   tmp_source_ele[["res"]][["run_code"]][["ds"]])
            eval(parse(text=code_chunk))
          }
          # Appending to returned code
          code = c(code, code_chunk)
          #------------------------------------------------
          # Appends the mapping between sheet name and description:
          tmp_desc  = tmp_source_ele[["ui"]][["element_name"]]
          if( tmp_source_ele[["ui"]][["res_label"]] != ""){
            tmp_desc = paste0(tmp_desc, ": ", tmp_source_ele[["ui"]][["res_label"]])
          }

          code_chunk = c('rpt[["summary"]] <- rbind(rpt[["summary"]],',
                         "  data.frame(",
                  paste0('    Sheet_Name="', tmp_source_ele[["objs"]][["element_object_name"]],
                         '",'),
                  paste0('    Description="', tmp_desc, '"'),
                         "  )",
                         ')', "")
          # Evaluating the code
          if(!gen_code_only){
            assign(tmp_source_ele[["objs"]][["element_object_name"]],
                   tmp_source_ele[["res"]][["run_code"]][["ds"]])
            eval(parse(text=code_chunk))
          }
          # Appending to returned code
          code = c(code, code_chunk)
        }
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
#'@title Fetch Data Management Module Datasets
#'@description Fetches the datasets contained in the module.
#'@param state DM state from \code{DM_fetch_state()}
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
#' sess_res = DM_test_mksession()
#' state = sess_res$state
#'
#' ds = DM_fetch_ds(state)
#'
#' ds
DM_fetch_ds = function(state, meta_only=FALSE){
  hasds  = FALSE
  isgood = TRUE
  msgs   = c()
  ds     = list()

  mod_checksum = state[["DM"]][["checksum"]]
  # Empty list for new datasets
  NEWDS = list(label      = NULL,
               MOD_TYPE   = "DM",
               id         = state[["id"]],
               idx        = NULL,
               res_label   = "",
               DS         = NULL,
               DSMETA     = NULL,
               code       = NULL,
               checksum   = mod_checksum,
               DSchecksum = NULL)

  # This prevents returning a dataset if this is triggered before data has
  # been loaded
  if(state[["DM"]][["isgood"]]){
    for(ename in names(state[["DM"]][["elements"]])){

      tmp_ele = state[["DM"]][["elements"]][[ename]]
      if(tmp_ele[["isgood"]]){

        tmp_object_name   = tmp_ele[["objs"]][["element_object_name"]]
        tmp_DSchecksum    = tmp_ele[["checksum"]]
        tmp_code          = tmp_ele[["code"]]
        tmp_label         = tmp_ele[["ui"]][["element_name"]]
        tmp_res_label     = tmp_ele[["ui"]][["res_label"]]
        tmp_idx           = tmp_ele[["idx"]]


        if(!is.null(tmp_object_name) &
           !is.null(tmp_ele[["res"]][["run_code"]][["ds"]])){

          TMPDS = NEWDS

          if(meta_only){
            TMPDS[["DS"]]         = NULL
          } else {
            TMPDS[["DS"]]         = tmp_ele[["res"]][["run_code"]][["ds"]]
          }

          TMPDS[["label"]]      = tmp_label
          TMPDS[["res_label"]]  = tmp_res_label
          TMPDS[["idx"]]        = tmp_idx
          TMPDS[["DSchecksum"]] = tmp_DSchecksum
          TMPDS[["code"]]       = tmp_code
          hasds                 = TRUE

          # Putting it all into the ds object to be returned
          ds[[tmp_object_name]] = TMPDS
        }
      }
    }
  }

  res = list(hasds  = hasds,
             isgood = isgood,
             msgs   = msgs,
             ds     = ds)
res}

#  #'@export
#  #'@title Fetch Data Management Module Models
#  #'@description Fetches the models contained in the module.
#  #'@param state DM state from \code{DM_fetch_state()}
#  #'@return list containing the following elements
#  #'\itemize{
#  #'  \item{isgood:}    Return status of the function.
#  #'  \item{hasmdl:}    Boolean indicator if the module has any models
#  #'  \item{msgs:}      Messages to be passed back to the user.
#  #'  \item{mdl:}       List with models. Each list element has the name of
#  #'  the R-object for that dataset. Each element has the following structure:
#  #'  \itemize{
#  #'    \item{label:}         Text label for the model (e.g. one-compartment model).
#  #'    \item{MOD_TYPE:}      Type of module.
#  #'    \item{id:}            Module ID.
#  #'    \item{idx:}           Numeric ID for the element.
#  #'    \item{rx_obj:}        The rxode2 object.
#  #'    \item{rx_obj_name:}   The rxode2 object name that holds the model.
#  #'    \item{ts_obj:}        List of timescale information for the system and
#  #'                          details of other timescales (\code{list(system="weeks", details = list(days=list(verb="days", conv=86400)))})
#  #'    \item{ts_obj_name:}   The object name that holds the timescale for this  model.
#  #'    \item{fcn_def:}      Text to define the model.
#  #'    \item{MDLMETA:}      Notes about the model.
#  #'    \item{code:}         Code to generate the model.
#  #'    \item{checksum:}     Module checksum.
#  #'    \item{MDLchecksum:} Model checksum.
#  #'  }
#  #'}
#  #'@examples
#  #' # We need a module state:
#  #' sess_res = DM_test_mksession()
#  #' state = sess_res$state
#  #'
#  #' mdls = DM_fetch_mdl(state)
#  #'
#  #' names(mdls)
#  DM_fetch_mdl = function(state){
#
#    hasmdl     = FALSE
#    isgood     = TRUE
#    msgs       = c()
#    mdl        = list()
#
#  # if(state[["DM"]][["isgood"]]){
#  #
#  #   # Checksum for the module
#  #   m_checksum = state[["DM"]][["checksum"]]
#  #   elements = names(state[["DM"]][["elements"]])
#  #   if(!is.null(elements)){
#  #     # We have at least 1 model
#  #     hasmdl = TRUE
#  #     for(element in elements){
#  #       # current element
#  #       ce = state[["DM"]][["elements"]][[element]]
#  #       ce_checksum = ce[["checksum"]]
#  #
#  #
#  #       # NOTE: You need to populate teh NULL pieces below:
#  #       mdl[[ ce[["rx_obj_name"]] ]] =
#  #         list(label       = ce[["ui"]][["element_name"]],
#  #              MOD_TYPE    = "DM",
#  #              id          = state[["id"]],
#  #              idx         = ce[["idx"]],
#  #              rx_obj      = NULL, #
#  #              rx_obj_name = NULL, #
#  #              ts_obj      = NULL, #
#  #              ts_obj_name = NULL, #
#  #              fcn_def     = NULL, #
#  #              MDLMETA     = NULL, #
#  #              code        = NULL, #
#  #              checksum    = m_checksum,
#  #              MDLchecksum = ce_checksum)
#  #     }
#  #   }
#  #
#  # } else {
#  #   isgood = FALSE
#  #   msgs = c(msgs, "Bad DM state")
#  # }
#
#    res = list(hasmdl     = hasmdl,
#               isgood     = isgood,
#               msgs       = msgs,
#               mdl        = mdl)
#    res}


#'@export
#'@title Updates DM Module Checksum
#'@description Takes a DM state and updates the checksum used to trigger
#'downstream updates
#'@param state DM state from \code{DM_fetch_state()}
#'@return DM state object with the checksum updated
#'@example inst/test_apps/DM_funcs.R
DM_update_checksum     = function(state){

  # checksum string
  chk_str = ""

  # We'll concatinate all the individual checksums together
  # and create a checksum of those:
  element_ids = names(state[["DM"]][["elements"]])
  for(element_id in element_ids){
    # We trigger updates when the element changes:
    chk_str = paste0(chk_str, ":", state[["DM"]][["elements"]][[element_id]][["checksum"]])
    chk_str = paste0(chk_str, ":", state[["DM"]][["elements"]][[element_id]][["ui"]][["element_name"]])
    # if(state[["DM"]][["elements"]][[element_id]][["isgood"]]){
    #   browser()
    # }
    #JMH add element_name here?
  }

  # This prevents messaging when no change has been made to the module.
  old_chk = state[["DM"]][["checksum"]]
  new_chk = digest::digest(chk_str, algo=c("md5"))

  if(has_updated(old_chk, new_chk)){
    state[["DM"]][["checksum"]] = new_chk
    FM_le(state, paste0("module checksum updated: ", state[["DM"]][["checksum"]]))
  }


state}


#'@export
#'@title Populate Session Data for Module Testing
#'@description Populates the supplied session variable for testing.
#'@param session Shiny session variable (in app) or a list (outside of app)
#'@return The DM portion of the `all_sess_res` returned from \code{\link{FM_app_preload}}
#'@examples
#' session = shiny::MockShinySession$new()
#' sess_res = DM_test_mksession(session=session)
DM_test_mksession = function(session = list()){

# sources = c(system.file(package="formods", "preload", "ASM_preload.yaml"),
#             system.file(package="formods", "preload", "DM_preload.yaml"))
# res = FM_app_preload(session=session, sources=sources)
# res = res[["all_sess_res"]][["DM"]]

  pldir = tempfile(pattern="preload_")
  mpd_res = mk_preload_dir(
    directory = pldir,
    preload   = c(system.file(package="formods", "preload", "ASM_preload.yaml"),
                  system.file(package="formods", "preload", "DM_preload.yaml")),
    mod_yaml  = c( 
      system.file(package="formods",  "templates", "formods.yaml"),
      system.file(package="formods",  "templates", "ASM.yaml"),
      system.file(package="formods",  "templates", "DM.yaml")),
    include = list(
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
  res = res[["all_sess_res"]][["DM"]]

  setwd(old_dir)
  unlink(pldir, recursive = TRUE)

res}

#'@export
#'@title New Data Management ds
#'@description Appends a new empty ds to the DM state object
#'and makes this new ds the active ds.
#'@param state DM state from \code{DM_fetch_state()}
#'@return DM state object containing a new ds and that
#'ds is set as the current active ds. See the help for
#'\code{DM_fetch_state()} for ds format.
#'@example inst/test_apps/DM_funcs.R
DM_new_element = function(state){

  # Incrementing the element counter
  state[["DM"]][["element_cntr"]] = state[["DM"]][["element_cntr"]] + 1

  # Creating a default element ID
  element_id = paste0("element_", state[["DM"]][["element_cntr"]])

  # Creating the object name for this element
  element_object_name = paste0(state[["MC"]][["element_object_name"]],
                       "_", state[["DM"]][["element_cntr"]])



  def_clean_ds    = as.logical(state[["MC"]][["formatting"]][["clean_ds"]][["default"]])


  # Default for a new element:
  element_def =
    list(
         # internal use only
         isgood                 = FALSE,
         # This will hold object names used in generated code
         objs                   = list(
           element_object_name    = element_object_name
         ),
         # This will hold the ui values for the current element
         ui                     = list(
           element_name  = paste0("Dataset ", state[["DM"]][["element_cntr"]]),
           source_id     = "",
           res_label      = "",
           clean_ds      = def_clean_ds
           ),
         id                     = element_id,
         idx                    = state[["DM"]][["element_cntr"]],
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

  # Creating default results
  #element_def = DM_add_url(state = state)
 #element_def[["res"]][["addattach_ds_url"]] = list(
 #  isgood = FALSE,
 #  msgs   = c(),
 #  url    = "")

  # This contains the code to generate inputs for the current element (e.g.
  # datasets that are needed).
  code_previous = ""
  element_def[["code_previous"]] = code_previous

  # Dropping the new element into the state
  state[["DM"]][["elements"]][[element_id]] = element_def


  # updating the checksum for the current element
  state[["DM"]][["elements"]][[element_id]][["checksum"]] = digest::digest(element_def, algo=c("md5"))

  # Setting the new element as current
  state[["DM"]][["current_element"]]     = element_id




state}


#'@export
#'@title Fetches Current ds
#'@description Takes a DM state and returns the current active
#'ds
#'@param state DM state from \code{DM_fetch_state()}
#'@return List containing the details of the active data view. The structure
#'of this list is the same as the structure of \code{state$DM$elements} in the output of
#'\code{DM_fetch_state()}.
#'@example inst/test_apps/DM_funcs.R
DM_fetch_current_element    = function(state){

  element_id = state[["DM"]][["current_element"]]

  current_element = state[["DM"]][["elements"]][[element_id]]

current_element}


#'@export
#'@title Sets the Value for the  Current ds
#'@description Takes a DM state and returns the current active
#'ds
#'@param state DM state from \code{DM_fetch_state()}
#'@param element Element list from \code{DM_fetch_current_element()}
#'@return DM state object with the current ds set using the
#'supplied value.
#'@example inst/test_apps/DM_funcs.R
DM_set_current_element    = function(state, element){

  element_id = state[["DM"]][["current_element"]]

  # updating the checksum for the current element
  tmp_ele = element
  tmp_ele[["checksum"]]  = ""

  tmp_checksum  = digest::digest(tmp_ele, algo=c("md5"))
  if(has_updated(element[["checksum"]], tmp_checksum)){
    FM_le(state, paste0("ds checksum updated: ", tmp_checksum))
    element[["checksum"]]  = tmp_checksum
  }

  # this updates the current element
  state[["DM"]][["elements"]][[element_id]] = element

  # This will update the checksum for the module
  state = DM_update_checksum(state)

state}

#'@export
#'@title Deletes Current ds
#'@description Takes a DM state and deletes the current ds.
#'If that is the last element, then a new default will be added.
#'@param state DM state from \code{DM_fetch_state()}
#'@return DM state object with the current ds deleted.
#'@example inst/test_apps/DM_funcs.R
DM_del_current_element    = function(state){

  # We need the current element and corresponding ID
  current_element = DM_fetch_current_element(state)
  element_id = current_element[["id"]]

  # This deletes the current element ID
  state[["DM"]][["elements"]][[element_id]] = NULL

  if(length(names(state[["DM"]][["elements"]])) == 0){
    # This is triggered when we've deleted the last element,
    # So now we will create a new one that will be active:
    state = DM_new_element(state)
  } else {
    # If there is at least one left, we pull off the first
    # one and make that active:
    element_id = names(state[["DM"]][["elements"]])[1]
    state[["DM"]][["current_element"]] = element_id
  }

state}


#'@export
#'@title Processes State After Loading
#'@description When loading a saved analysis, this will process the state
#'object to account for differences that may apply between servers.
#'@param state DM state from \code{DM_fetch_state()}
#'@param session Shiny session variable
#'@return List contianing the DM state object and shiny session object
#'@examples
#' sess_res = DM_test_mksession()
#' session = sess_res$session
#' state   = sess_res$state
#' state = DM_onload(state, session)
DM_onload     = function(state, session){

  # Put any post processing you would use after loading here. If you do not
  # have any you can leave this function as a passthrough for the state object
  # or just delete it.


  res =
  list(state   = state,
       session = session)
res}


#'@export
#'@title Preload Data for DM Module
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
DM_preload  = function(session, src_list, yaml_res, mod_ID=NULL, react_state = list(), quickload=FALSE){
  isgood  = TRUE
  input   = list()
  msgs    = c()
  res     = c()
  err_msg = c()


  FM_yaml_file  = render_str(src_list[[mod_ID]][["fm_yaml"]])
  MOD_yaml_file = render_str(src_list[[mod_ID]][["mod_yaml"]])
  id_ASM        = yaml_res[[mod_ID]][["mod_cfg"]][["MC"]][["module"]][["depends"]][["id_ASM"]]

  # Creating an empty state object
  state = DM_fetch_state(id              = mod_ID,
                         input           = input,
                         session         = session,
                         FM_yaml_file    = FM_yaml_file,
                         MOD_yaml_file   = MOD_yaml_file,
                         react_state     = react_state)

  if(!formods::is_shiny(session)){
    session = FM_set_mod_state(session, mod_ID, state)
  }

  # Data sources
  sources  = src_list[[mod_ID]][["sources"]]
  # Elements
  elements = src_list[[mod_ID]][["elements"]]

  if(!is.null(sources)){
    # Adding all the sources
    FM_le(state, "adding preload sources")
    for(src_idx in 1:length(sources)){
      if(sources[[src_idx]][["source"]][["source_type"]] == "file"){
        file_path = render_str(sources[[src_idx]][["source"]][["source_str"]])
        file_name = render_str(sources[[src_idx]][["source"]][["file_name"]])
        source_id = render_str(sources[[src_idx]][["source"]][["source_id"]])
        state = DM_add_file(
          state     = state,
          file_name =  file_name,
          file_path =  file_path,
          load_ID   =  source_id)

        if(state[["DM"]][["res"]][["add_file"]][["isgood"]]){
          FM_le(state, state[["DM"]][["res"]][["add_file"]][["msgs"]])
        } else {
          FM_le(state, state[["DM"]][["res"]][["add_file"]][["msgs"]], entry_type="danger")
        }
      } else if(sources[[src_idx]][["source"]][["source_type"]] == "url"){
        source_url = render_str(sources[[src_idx]][["source"]][["source_str"]])
        source_id = render_str(sources[[src_idx]][["source"]][["source_id"]])
        state = DM_add_url(
          state     = state,
          url       =  source_url,
          load_ID   =  source_id)
        if(state[["DM"]][["res"]][["add_url"]][["isgood"]]){
          FM_le(state, state[["DM"]][["res"]][["add_url"]][["msgs"]])
        } else {
          FM_le(state, state[["DM"]][["res"]][["add_url"]][["msgs"]], entry_type="danger")
        }
      }
    }
    # setting the resource counter to account for sources added
    state[["DM"]][["resource_cntr"]]  =
      max(as.numeric(as.character(state[["DM"]][["defined_sources"]][["ID"]]))) + 1
    if(!is.null(elements)){
      # All of the numeric IDs in the preload
      enumeric    = c()

      # Map between list index and internal figure ID
      element_map = list()
      for(ele_idx in 1:length(elements)){
        enumeric = c(enumeric, elements[[ele_idx]][["element"]][["idx"]])
        element_map[[ paste0("element_",elements[[ele_idx]][["element"]][["idx"]] )]] = ele_idx
      }

      # Creating empty element placeholders
      while(state[["DM"]][["element_cntr"]] < max(enumeric)){
        state = DM_new_element(state)
      }

      # culling any unneeded views
      for(ele_id  in names(state[["DM"]][["elements"]])){
        # This is a view that doesn't exist in elements so
        # we need to cull it
        if(!(ele_id  %in% names(element_map))){
          # Setting the view to be deleted as the current view
          state[["DM"]][["elements"]][[ ele_id  ]] = NULL
        }
      }

      # Adding the main UI components
      for(uiname in names(src_list[["DM"]][["ui"]])){
        state[["DM"]][["ui"]][[uiname]] = src_list[["DM"]][["ui"]][[uiname]]
      }

      # TODO: You need to process the elements and components here
      # Now we have empty elements defined
      for(element_id in names(element_map)){
        # Getting the numeric position in the list corresponding
        # to the current element id
        ele_idx = element_map[[element_id]]
        ele_isgood = TRUE
        ele_err_msg = c()

        # Element data to be preloaded:

        FM_le(state, paste0("loading element idx: ", ele_idx ))
        # Making the current element id active
        state[["DM"]][["current_element"]]  =  element_id
        # Pulling it out so we can populate it below and
        # create datasets
        current_ele  = DM_fetch_current_element(state)


        # populating element data
        current_ele[["ui"]] = elements[[ele_idx]][["element"]][["ui"]]

        # Updating the element code
        current_ele = DM_update_element_code(
          state   = state,
          element = current_ele,
          session = session)

        if(current_ele[["res"]][["code_update"]][["isgood"]]){
          # Running the element code
          current_ele = DM_run_code(
            state   = state,
            element = current_ele)

          if(!current_ele[["res"]][["run_code"]][["isgood"]]){
            ele_err_msg = c( ele_err_msg, current_ele[["res"]][["run_code"]][["msgs"]])
            ele_isgood  = FALSE
          }
        } else {
          ele_err_msg = c( ele_err_msg, current_ele[["res"]][["code_update"]][["msgs"]])
          ele_isgood  = FALSE
        }


        # Saving changes to the element
        state = DM_set_current_element(
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
  }

  # Required for proper reaction:
  react_state[[mod_ID]]  = list(DM  =
          list(checksum = state[["DM"]][["checksum"]],
               hasds    = DM_hasds(state)))


  # Making sure the ui tracked at the state level is consistent with the ui in the current element
  current_ele  = DM_fetch_current_element(state)
  for(ui_name in names(current_ele[["ui"]])){
    if(ui_name %in% names(state[["DM"]][["ui"]])){
      state[["DM"]][["ui"]][[ui_name]] = current_ele[["ui"]][[ui_name]] 
      }
    } 

  # Setting old ui values to current to prevent reactions on load
  for(ui_name in names(state[["DM"]][["ui"]])){
     state[["DM"]][["ui_old"]][[ui_name]] = state[["DM"]][["ui"]][[ui_name]] 
    }

  # holding everything
  state = set_hold(state)


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
#'@title Make List of Current DM State
#'@description Deletes a file based on the specified ID
#'@param state DM state object
#'@param id  Location of file to add
#'@return DM state with file deleted and the `res$del_file` field with the
#'following elements:
#'\itemize{
#'  \item{isgood:}    Return status of the function.
#'  \item{msgs:}      Messages to be passed back to the user.
#'}
#'@example inst/test_apps/DM_funcs_ll.R
DM_delete_source       = function(state, id){

  # List of all the files:
  files_df = state[["DM"]][["defined_sources"]]
  # Deleting the file:
  full_path = files_df[files_df[["ID"]] == id, ][["full_path"]]
  file_name = files_df[files_df[["ID"]] == id, ][["File"]]
  unlink(full_path)

  # Removing the record:
  files_df = files_df[files_df[["ID"]] != id, ]

  # Finding any elements that depend on this resource and flagging those as
  # bad
  #browser()
  # JMH should deletes trigger anything downstream?
  # - check if the id was being used by any elements
  # - if it is, remove the id and set the element isgood to FALSE
  # - perhaps force rebuild with any other assets
  #message("fix delete")
  for(element_id in names(state[["DM"]][["elements"]])){
    tmp_source_ele = state[["DM"]][["elements"]][[element_id]]
    if(tmp_source_ele[["ui"]][["source_id"]] == as.numeric(id)){
      state[["DM"]][["elements"]][[element_id]][["isgood"]] = FALSE
    }
  }

  state[["DM"]][["res"]][["del_file"]] = list(isgood = TRUE, msgs = c())

  state[["DM"]][["res"]][["del_file"]][["msgs"]] =  paste0("file: ", file_name, " deleted")

  state[["DM"]][["defined_sources"]] = files_df


  # Updating the module checksum
  state = DM_update_checksum(state)

state}

#'@export
#'@title Make List of Current DM State
#'@description Adds a file to the DM State
#'@param state DM state object
#'@param file_path  Location of file to add
#'@param file_name  Name of file
#'@param load_ID    When loading from a file the resource ID can be specified,
#'set to \code{NULL} (default) have the ID auto generated.
#'@return DM state with file added and the `res$add_file` field with the
#'following elements:
#'\itemize{
#'  \item{isgood:}    Return status of the function.
#'  \item{msgs:}      Messages to be passed back to the user.
#'}
#'@example inst/test_apps/DM_funcs_ll.R
DM_add_file       = function(state, file_path, file_name, load_ID=NULL){

  tmp_dest= file.path( state[["DM"]][["data_dir_full"]], file_name)

  file_checksum  =  digest::digest(object=file_path, file=TRUE, algo=c("md5"))

  size_num      = file.size(file_path)
  size_str      = size_num
  if(is_installed("utils")){
    #size_str = utils:::format.object_size(size_str, "auto")
    size_str = format(structure(size_str, class="object_size"), units="auto")
  }

  state[["DM"]][["res"]][["add_file"]] = list(isgood = TRUE, msgs = c())

  # JMH should anything else be done or triggered when a file is
  # overwritten?
  if(file.exists(tmp_dest) & any(state[["DM"]][["defined_sources"]][["full_path"]] == tmp_dest)){
    FM_le(state, paste0("overwriting previously uploaded file: ", file_name))
    # If the file exists we just overwrite it with the new file and update the
    # meta data.

    ridxup = which(state[["DM"]][["defined_sources"]][["full_path"]] == tmp_dest)[1]
    state[["DM"]][["defined_sources"]][ridxup, ]

    state[["DM"]][["defined_sources"]][ridxup, ][["Size"]]      = size_str
    state[["DM"]][["defined_sources"]][ridxup, ][["size_num"]]  = size_num
    state[["DM"]][["defined_sources"]][ridxup, ][["Delete"]]    = FALSE
    state[["DM"]][["defined_sources"]][ridxup, ][["checksum"]]  = file_checksum

    state[["DM"]][["res"]][["add_file"]][["msgs"]] = paste0("updated: ", file_name)
    FM_le(state, paste0(" - file updated: ", file_name))

  } else {

    if(is.null(load_ID)){
      resource_ID                       = as.character(state[["DM"]][["resource_cntr"]])
      state[["DM"]][["resource_cntr"]]  = state[["DM"]][["resource_cntr"]] + 1
    } else {
      resource_ID                       = as.character(load_ID)
    }

    # Now we store information about the file so we can track it
    state[["DM"]][["defined_sources"]]  = rbind(
      state[["DM"]][["defined_sources"]],
      data.frame(
        ID          = resource_ID,
        Resource    = file_name,
        file_name   = file_name,
        full_path   = tmp_dest,
        rel_path    = file.path(state[["DM"]][["data_dir_rel"]], file_name),
        Size        = size_str,
        size_num    = size_num,
        url         =  "",
        source_type = "file",
        Delete      = FALSE,
        excel_file  = is_excel(file_name),
        checksum    = file_checksum
      )
    )

    state[["DM"]][["res"]][["add_file"]][["msgs"]] = paste0("added: ", file_name)
    FM_le(state, paste0(" - file added: ", file_name))
  }

  file.copy(from=file_path, to  = tmp_dest, overwrite =TRUE)

  # Updating the module checksum
  state = DM_update_checksum(state)

state}


#'@export
#'@title Updates Element Code
#'@description Updates the code for the current element
#'
#'@param state DM state from \code{DM_fetch_state()}
#'@param element Element list from \code{DM_fetch_current_element()}
#'@param session Shiny session variable
#'@return DM element with code updated
#'@return DM element with code updated and the `res$code_update` field with the
#'following elements:
#'\itemize{
#'  \item{isgood:}    Return status of the function.
#'  \item{msgs:}      Messages to be passed back to the user.
#'}
#'@example inst/test_apps/DM_funcs_ll.R
DM_update_element_code    = function(state, element, session){

  code_export   = c()
  code_run      = c()

  msgs        = c()
  isgood      = TRUE

  srcnfo = DM_fetch_source(state = state, element = element)

  if(!srcnfo[["isgood"]]){
    isgood = FALSE
    msgs = c(msgs, "unable to find source for current element")
  }

  if(srcnfo[["source_type"]] == "file"){
    if(srcnfo[["excel_file"]]){
      if(srcnfo[["excel_good"]]){
        isgood      = TRUE
        code_export = paste0(element[["objs"]][["element_object_name"]], " <- rio::import(file=",deparse(srcnfo[["rel_path"]]),  ", which =", deparse(srcnfo[["sheet"]]), ")")
        code_run    = paste0(element[["objs"]][["element_object_name"]], " <- rio::import(file=",deparse(srcnfo[["full_path"]]), ", which =", deparse(srcnfo[["sheet"]]), ")")
      } else {
        isgood      = FALSE
        msgs = c(msgs, "excel file issue probably no sheet selected")
      }
    }

    if(!srcnfo[["excel_file"]]){
      isgood      = TRUE
      code_export = paste0(element[["objs"]][["element_object_name"]], " <- rio::import(file=",deparse(srcnfo[["rel_path"]]),  ")")
      code_run    = paste0(element[["objs"]][["element_object_name"]], " <- rio::import(file=",deparse(srcnfo[["full_path"]]), ")")
    }
  }

  if(srcnfo[["source_type"]] == "url"){
    if(srcnfo[["excel_file"]]){
      if(srcnfo[["excel_good"]]){
        isgood      = TRUE
        code_export = paste0(element[["objs"]][["element_object_name"]], " <- rio::import(file=",deparse(srcnfo[["url"]]),  ", which =", deparse(srcnfo[["sheet"]]), ")")
        code_run    = paste0(element[["objs"]][["element_object_name"]], " <- rio::import(file=",deparse(srcnfo[["full_path"]]), ", which =", deparse(srcnfo[["sheet"]]), ")")
      } else {
        isgood      = FALSE
        msgs = c(msgs, "excel file issue probably no sheet selected")
      }
    }

    if(!srcnfo[["excel_file"]]){
      isgood      = TRUE
      code_export = paste0(element[["objs"]][["element_object_name"]], " <- rio::import(file=",deparse(srcnfo[["url"]]),  ")")
      code_run    = paste0(element[["objs"]][["element_object_name"]], " <- rio::import(file=",deparse(srcnfo[["full_path"]]), ")")
    }
  }

  # Adding cleaning code if necessary/possible
  if(isgood){
    if(is_installed("janitor")){
      if(is.logical(element[["ui"]][["clean_ds"]])){
        if(element[["ui"]][["clean_ds"]]){
          clean_code = paste0( element[["objs"]][["element_object_name"]],
                              " <- janitor::clean_names(",
                              element[["objs"]][["element_object_name"]],
                              ', case="none")')

          code_export = c(code_export, clean_code)
          code_run    = c(code_run,    clean_code)
        }
      }
    }
  }

  if(!isgood){
    msgs = c(msgs, "DM_update_element_code()")
    FM_le(state, msgs)
  }

  # Passing status back to the user
  element[["res"]][["code_update"]][["isgood"]] = isgood
  element[["res"]][["code_update"]][["msgs"]]   = msgs


  # Adding the package code
  fdres = FM_fetch_deps(state=state, session=session)
  if(is.null(fdres[["package_code"]])){
    code_ele_only = c(code_export)
  } else {
    code_ele_only = c(
      fdres[["package_code"]],
      "",
      code_export)
  }

  # This is the code that is exported in the save script and shown in the app
  element[["code"]]           = paste0(code_export,   collapse="\n")
  # This is the code that is shown in the app
  element[["code_ele_only"]]  = paste0(code_ele_only, collapse="\n")
  # This is the code that is run in the app to load the data
  element[["code_run"]]       = paste0(code_run,      collapse="\n")
element}

#'@export
#'@title Run Element Code
#'@description Runs the element code and updates the dataset
#'
#'@param state DM state from \code{DM_fetch_state()}
#'@param element Element list from \code{DM_fetch_current_element()}
#'@param session Shiny session variable
#'@return DM element with code updated
#'@return DM element with code updated and the `res$run_code` field with the
#'following elements:
#'\itemize{
#'  \item{isgood:}    Return status of the function.
#'  \item{msgs:}      Messages to be passed back to the user.
#'  \item{ds:}        Dataset if reading was successful and NULL if not.
#'}
#'@example inst/test_apps/DM_funcs_ll.R
DM_run_code = function(state, element, session){

  tcres = FM_tc(
    cmd    = element[["code_run"]],
    tc_env = list(),
    capture = element[["objs"]][["element_object_name"]])

  # If everything is good this will capture the result
  if(tcres[["isgood"]]){
    element[["res"]][["run_code"]][["ds"]] =
      tcres[["capture"]][[
        element[["objs"]][["element_object_name"]] ]]
  } else {
    # If we fail we set the dataset to NULL
    element[["res"]][["run_code"]][["ds"]] = NULL
  }

  element[["res"]][["run_code"]][["isgood"]] = tcres[["isgood"]]
  element[["res"]][["run_code"]][["msgs"]]   = tcres[["msgs"]]
  # Flagging the element overall
  element[["isgood"]]                        = tcres[["isgood"]]

element}

#'@export
#'@title Determines if a File is Excel
#'@description Check to see if a file is an excel file
#'@param file_name Name of file to check.
#'@return Logical indicating if a file is an excel file or not.
#'@examples
#'is_excel("file.xls")
#'is_excel("file.xlsx")
#'is_excel("file.csv")
is_excel = function(file_name){
  res = c( stringr::str_detect(string = file_name, pattern="\\.xls") |
           stringr::str_detect(string = file_name, pattern="\\.xlsx"))
res}

#'@export
#'@title Determines if a File is Importable
#'@description Check to see if a file can be imported
#'@param file_name Name of file to check.
#'@return Logical indicating if a file is an excel file or not.
#'@examples
#'is_importable("file.xls")
#'is_importable("file.csv")
#'is_importable("file.bob")
is_importable = function(file_name){
  finfo = rio::get_info(file_name)

  res = TRUE
  if(is.na(finfo[["type"]])){
    res = FALSE
  }
res}

#'@export
#'@title Gets Source File Name for Element
#'@description Gets file information about the current element
#'@param state DM state from \code{DM_fetch_state()}
#'@param element Element list from \code{DM_fetch_current_element()}
#'@return list with the following elements
#' \itemize{
#'   \item{isgood:}       Boolean indicating if the file is good (if it's an  excel file and has a sheet).
#'   \item{msgs:}         Messages to be passed back to the user.
#'   \item{excel_file:}   Logical indicating if the source is an excel file.
#'   \item{excel_good:}   Logical indicating if the source is an excel file and we have a sheet defined in that file.
#'   \item{file_name:}    Base file name (e.g. 'file.csv')
#'   \item{full_path:}    Full path to the file (e.g. '/var/tmp/file.csv'), NULL if not found.
#'   \item{rel_path:}     Path to the file relative to user directory file (e.g. 'data/DM/file.csv'), NULL if not found.
#'   \item{sheet_found:}  Logical indicating if a sheet was found
#'   \item{sheet:}        Excel sheet selected by user
#'   \item{source_type:}  Type of source selected ("file", "url" or "unknown")
#'}
#'@example inst/test_apps/DM_funcs_ll.R
DM_fetch_source = function(state, element){

  isgood      = TRUE
  excel_file  = FALSE
  sheet_found = FALSE
  excel_good  = FALSE
  source_type = "unknown"
  msgs        = c()
  full_path   = NULL
  file_name   = NULL
  rel_path    = NULL
  url         = NULL
  sheet       = NULL

  if(is.null(element[["ui"]][["source_id"]])){
    isgood = FALSE
    msgs   = c(msgs, "no source_id defined")
  } else {
    ridx = which(state[["DM"]][["defined_sources"]][["ID"]] == element[["ui"]][["source_id"]])
    if(length(ridx) > 0){
      files_df    = state[["DM"]][["defined_sources"]]
      full_path   = files_df[files_df[["ID"]] == element[["ui"]][["source_id"]], ][["full_path"  ]]
      file_name   = files_df[files_df[["ID"]] == element[["ui"]][["source_id"]], ][["file_name"  ]]
      excel_file  = files_df[files_df[["ID"]] == element[["ui"]][["source_id"]], ][["excel_file" ]]
      rel_path    = files_df[files_df[["ID"]] == element[["ui"]][["source_id"]], ][["rel_path"   ]]
      source_type = files_df[files_df[["ID"]] == element[["ui"]][["source_id"]], ][["source_type"]]
      url         = files_df[files_df[["ID"]] == element[["ui"]][["source_id"]], ][["url"        ]]

      if(length(ridx) > 1){
        browser()
      }

      # Populating the sheet information
      if(!is.null(element[["ui"]][["ds_sheet"]])){
        if(element[["ui"]][["ds_sheet"]] != ""){
          sheet = element[["ui"]][["ds_sheet"]]
          sheet_found = TRUE
        }
      }

      # If we have an excel file and a sheet we will determine if
      # that sheet is in the current excel file:
      if(excel_file & sheet_found){
        if(sheet %in%  readxl::excel_sheets(full_path)){
          excel_good = TRUE
        }
      }
    } else {
      isgood = FALSE
      msgs = c(msgs, paste0("bad source_id (", element[["ui"]][["source_id"]], ") not found"))
    }
  }

  res = list(
    isgood      = isgood,
    msgs        = msgs,
    excel_file  = excel_file,
    excel_good  = excel_good,
    file_name   = file_name,
    full_path   = full_path,
    rel_path    = rel_path,
    url         = url,
    source_type = source_type,
    sheet_found = sheet_found,
    sheet       = sheet
  )

res}


#'@export
#'@title Attaches Data Source from URL
#'@description Using the user specified url a data source will be downloaded
#'and attached (if possible).
#'@param state DM state from \code{DM_fetch_state()}
#'@param url        URL to load
#'@param load_ID    When loading from a file the resource ID can be specified
#'set to \code{NULL} (default) have the ID auto generated.
#'@return DM state with url attachment results stored in the
#'`res$add_url` field with the following elements:
#'\itemize{
#'  \item{isgood:}    Return status of the function.
#'  \item{msgs:}      Messages to be passed back to the user.
#'}
#' If successful it will be added to the resources table.
#'@example inst/test_apps/DM_funcs_ll.R
DM_add_url = function(state, url = "", load_ID = NULL){

  isgood    = TRUE
  msgs      = c()
  file_name = NULL
  full_path = NULL
  rel_path  = NULL


  if(!is.null(url)){
    if(url == ""){
      isgood = FALSE
      msgs = c(msgs, 'url is empty ("")')
    }
  } else {
    isgood = FALSE
    msgs = c(msgs, "url is NULL")
  }

  # We will attempt do download the source url into a temporary file:
  if(isgood){
    # Creating a temp file to hold the downlaod
    tmp_fext = tools::file_ext(url)
    if(tmp_fext!=""){
      tmp_fext = paste0(".", tmp_fext)
    }
    tmp_file = tempfile(pattern="FM_DM_", fileext=tmp_fext)

    # This will download the file:
    cmd = paste0("curl::curl_download(url=",deparse(url),
                 ",destfile=",deparse(tmp_file), ")")
    tcres = FM_tc(
      cmd      = cmd,
      tc_env   = list(),
      capture  = c())

    if(tcres[["isgood"]]){
      full_path = tmp_file
      file_name = basename(tmp_file)
      rel_path  = tmp_file
    } else {
      isgood = FALSE
      msgs = c(msgs, tcres[["msgs"]])
    }
  }

  if(isgood){
    file_checksum  =  digest::digest(object=full_path, file=TRUE, algo=c("md5"))
    size_num       = file.size(full_path)
    size_str       = size_num
    if(is_installed("utils")){
      #size_str = utils:::format.object_size(size_str, "auto")
      size_str = format(structure(size_str, class="object_size"), units="auto")
    }

    if(any(state[["DM"]][["defined_sources"]][["url"]] == url)){
      # If the file exists we just overwrite it with the new file and update the
      # meta data.
      ridxup = which(state[["DM"]][["defined_sources"]][["url"]] == url)[1]
      state[["DM"]][["defined_sources"]][ridxup, ]

      state[["DM"]][["defined_sources"]][ridxup, ][["Size"]]      = size_str
      state[["DM"]][["defined_sources"]][ridxup, ][["size_num"]]  = size_num
      state[["DM"]][["defined_sources"]][ridxup, ][["Delete"]]    = FALSE
      state[["DM"]][["defined_sources"]][ridxup, ][["checksum"]]  = file_checksum

      state[["DM"]][["defined_sources"]][ridxup, ][["file_name"]] = file_name
      state[["DM"]][["defined_sources"]][ridxup, ][["full_path"]] = full_path
      state[["DM"]][["defined_sources"]][ridxup, ][["rel_path"]]  = rel_path

      msgs = c(msgs, paste0("url updated: ", url))
      formods::FM_le(state, paste0(" - url updated: ", url))
    } else {

      if(is.null(load_ID)){
        resource_ID                       = as.character(state[["DM"]][["resource_cntr"]])
        state[["DM"]][["resource_cntr"]]  = state[["DM"]][["resource_cntr"]] + 1
      } else {
        resource_ID                       = as.character(load_ID)
      }

      # Now we store information about the file so we can track it
      state[["DM"]][["defined_sources"]]  = rbind(
        state[["DM"]][["defined_sources"]],
        data.frame(
          ID          = resource_ID,
          Resource    = url,
          file_name   = file_name,
          full_path   = full_path,
          rel_path    = rel_path,
          Size        = size_str,
          size_num    = size_num,
          url         = url,
          source_type = "url",
          Delete      = FALSE,
          excel_file  = is_excel(file_name),
          checksum    = file_checksum
        )
      )
      msgs = c(msgs, paste0("url added: ", url))
      formods::FM_le(state, paste0(" - url added: ", url))
    }
  }

  state[["DM"]][["res"]][["add_url"]] = list(
    isgood    = isgood,
    msgs      = msgs)

state}

#'@export
#'@title Make List of Current DM State
#'@description Reads in the app state from yaml files.
#'@param state DM state object
#'@return list with the following elements
#' \itemize{
#'   \item{isgood:}       Boolean indicating the exit status of the function.
#'   \item{msgs:}         Messages to be passed back to the user.
#'   \item{yaml_list:}    Lists with preload components.
#'}
#'@examples
#' sess_res = DM_test_mksession()
#' state = sess_res$state
#' res = DM_mk_preload(state)
DM_mk_preload     = function(state){
  isgood    = TRUE
  msgs      = c()
  err_msg   = c()
  yaml_list = list()

  ylist = list(
      fm_yaml  = file.path("config", basename(state[["FM_yaml_file"]])),
      mod_yaml = file.path("config", basename(state[["MOD_yaml_file"]])),
      res_deps = list(ds=list()) ,
      sources  = NULL,
      elements = NULL 
  )

  # Creating the yaml list with the module ID at the top level
  # Adding state level ui values:
  ui_state = c(
    "element_selection",
    "source_url")

  for(uiname in ui_state){
    if(is.null( state[["DM"]][["ui"]][[uiname]])){
      ylist[[ "ui" ]][[uiname]] = ""
    } else {
      ylist[[ "ui" ]][[uiname]] = state[["DM"]][["ui"]][[uiname]]
    }
  }


  if(nrow(state[["DM"]][["defined_sources"]])>0){
    # Walking through each source
    for(src_idx in 1:nrow(state[["DM"]][["defined_sources"]])){
  
      src_row = state[["DM"]][["defined_sources"]][src_idx, ]
      source_type = src_row[["source_type"]]
      source_id   = src_row[["ID"]]
  
      tmp_source = list()
      if(source_type == "file") {
        source_str  = src_row[["rel_path"]]
        tmp_source[["file_name"]]  = src_row[["file_name"]]
      }else if(source_type == "url") {
        source_str  = src_row[["url"]]
      }
      tmp_source[["source_id"]]    = source_id
      tmp_source[["source_type"]]  = source_type
      tmp_source[["source_str"]]   = source_str
  
      ylist[["sources"]][[src_idx]] = list(source  = tmp_source)
      formods::FM_le(state, paste0("saving source (", source_id, ") ", source_str))
  
      src_idx = src_idx + 1
    }
  
    ele_idx = 1
    # Walking through each element:
    for(element_id in names(state[["DM"]][["elements"]])){
      tmp_source_ele = state[["DM"]][["elements"]][[element_id]]
  
      formods::FM_le(state, paste0("saving element (", tmp_source_ele[["idx"]], ") ", tmp_source_ele[["ui"]][["element_name"]]))
  
      # Constructing the source element
      tmp_element = list(
        idx  = tmp_source_ele[["idx"]],
        ui   = tmp_source_ele[["ui"]])
  
      # Appending element
      ylist[["elements"]][[ele_idx]] = list(element = tmp_element)
      ele_idx = ele_idx + 1
    }

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

#'@export
#'@title Check DM State For Datasets
#'@description Walks through the DM state object to see if there are any
#'datasets available
#'@param state DM state from \code{DM_fetch_state()}
#'@return Logical TRUE if there is a dataset or FALSE otherwise.
#'@examples
#' sess_res = DM_test_mksession()
#' state = sess_res[["state"]]
#' DM_hasds(state)
DM_hasds = function(state){
  hasds = FALSE
# dw_views    = names(state[["DM"]][["views"]])
  ds_eles = names(state[["DM"]][["elements"]])
  for(ds_ele  in ds_eles){
    tmp_checksum      = state[["DM"]][["elements"]][[ds_ele]][["checksum"]]
    tmp_object_name   = state[["DM"]][["elements"]][[ds_ele]][["objs"]][["element_object_name"]]
    tmp_ds            = state[["DM"]][["elements"]][[ds_ele]][["res"]][["run_code"]][["ds"]]
    tmp_rc_isgood     = state[["DM"]][["elements"]][[ds_ele]][["res"]][["run_code"]][["isgood"]]
    if(!is.logical(tmp_rc_isgood)){
      tmp_rc_isgood = FALSE }

    if(!is.null(tmp_checksum)    &
       !is.null(tmp_object_name) &
       !is.null(tmp_ds)          &
       tmp_rc_isgood){
       hasds = TRUE
    }
  }
hasds}
