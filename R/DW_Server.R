#'@import dplyr
#'@import rhandsontable
#'@import shiny
#'@import shinyWidgets
#'@importFrom digest digest
#'@importFrom magrittr "%>%"
#'@importFrom readr read_csv
#'@importFrom rlang .data
#'@importFrom shinyAce aceEditor updateAceEditor
#'@importFrom yaml read_yaml

# https://getbootstrap.com/docs/3.3/components/

#'@export
#'@title Data Wrangling Server
#'@description Server function for the data wrangling module
#'@param id An ID string that corresponds with the ID used to call the module's 
#'UI function
#'@param yaml_section  Section of the yaml file with the module 
#'configuration (`"DW"`)
#'@param yaml_file Upload Data cofiguration file
#'@param id_UD  ID string for the upload data module used to handle uploads or 
#'the name of the list element in react_state where the data set is stored.
#'@param react_state Variable passed to server to allow reaction outside of 
#'module (`NULL`)
#'@return return
DW_Server <- function(id,
                      yaml_section = "DW",
                      yaml_file    = system.file(package = "formods", 
                                                 "templates", 
                                                 "config.yaml"),
                      id_UD        = "UD",
                      react_state  = NULL) {
  moduleServer(id, function(input, output, session) {
    
    #------------------------------------
    # Current DW elements
    output$hot_dw_elements = rhandsontable::renderRHandsontable({
      req(input$select_dw_element)
      # Force update on button click
      input$button_dw_add_element
      # Force update on deletion clicks
      input$hot_dw_elements
      
      
      state = DW_fetch_state(id           = id,
                             input        = input,
                             session      = session,
                             yaml_file    = yaml_file,
                             yaml_section = yaml_section,
                             id_UD        = id_UD,
                             react_state  = react_state)
      uiele = NULL
      
      if(state[["DW"]][["DS"]][["isgood"]]){
        if(is.null(state[["DW"]][["elements_table"]])){
          df = data.frame("No_Data"="# No data wragling elements defined yet!")
          
          hot= rhandsontable::rhandsontable(
            df,
            width  = state[["MC"]][["dimensions"]][["dw_elements"]][["width"]],
            height = state[["MC"]][["dimensions"]][["dw_elements"]][["height"]],
            rowHeaders = NULL
          )
          
          uiele =  hot
          
        } else {
          df = state[["DW"]][["elements_table"]]
          df[["cmd"]] = NULL
          
          hot = rhandsontable::rhandsontable(
            df,
            width  = state[["MC"]][["dw_elements"]][["width"]],
            height = state[["MC"]][["dw_elements"]][["height"]],
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
            hot_col("Action" ,      readOnly = TRUE) %>%
            hot_col("Description" , readOnly = TRUE) %>%
            hot_col("Status" ,      readOnly = TRUE)
          
          uiele = hot
        }
      }
      
      uiele
      
    })
    #------------------------------------
    # Generated data wrangling code
    observe({
      # Forcing a reaction to changes from the upload data module
      react_state[[id_UD]]
      # Force update on button click
      input$button_dw_add_element
      # Force update on deletion clicks
      input$hot_dw_elements
      
      state = DW_fetch_state(id           = id,
                             input        = input,
                             session      = session,
                             yaml_file    = yaml_file,
                             yaml_section = yaml_section,
                             id_UD        = id_UD,
                             react_state  = react_state)
      
      uiele = NULL
      
      if(state[["DW"]][["DS"]][["isgood"]]){
        if(is.null(state[["DW"]][["elements_table"]])){
          uiele = "# No data wragling elements defined yet!"
        } else {
          uiele = paste(state[["DW"]][["elements_table"]][["cmd"]], collapse="\n")
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
    #output$ui_dw_code  =  renderText({
    #  #req(input$select_dw_element)
    #  # Force update on button click
    #  input$button_dw_add_element
    #  # Force update on deletion clicks
    #  input$hot_dw_elements
    #
    #  state = DW_fetch_state(id           = id,
    #                         input        = input,
    #                         session      = session,
    #                         yaml_file    = yaml_file,
    #                         yaml_section = yaml_section,
    #                         id_UD        = id_UD,
    #                         react_state  = react_state)
    #
    #  if(is.null(state[["DW"]][["elements_table"]])){
    #    uiele = "# No data wragling elements defined yet!"
    #  } else {
    #    uiele = paste(state[["DW"]][["elements_table"]][["cmd"]], collapse="\n")
    #  }
    #
    #  uiele})
    
    #------------------------------------
    output$ui_dw_new_element_msg = renderText({
      # Force update on button click
      input$button_dw_add_element
      # Update when they delete elements as well
      input$hot_dw_elements       
      #req(input$select_dw_element)
      state = DW_fetch_state(id           = id,
                             input        = input,
                             session      = session,
                             yaml_file    = yaml_file,
                             yaml_section = yaml_section,
                             id_UD        = id_UD,
                             react_state  = react_state)
      
      uiele = NULL
      if(state[["DW"]][["DS"]][["isgood"]]){
        # If the add element message isn't NULL we return that.
        uiele = state[["DW"]][["add_element_msg"]]
      }
      uiele})
    #------------------------------------
    # Row for new DW elements
    output$ui_dw_new_element_row = renderUI({
      #input$button_dw_add_element
      req(input$select_dw_element)
      state = DW_fetch_state(id           = id,
                             input        = input,
                             session      = session,
                             yaml_file    = yaml_file,
                             yaml_section = yaml_section,
                             id_UD        = id_UD,
                             react_state  = react_state)
      
      if(state[["DW"]][["DS"]][["isgood"]]){
        if(state[["DW"]][["isgood"]]){
          # Current final dataset
          WDS = state[["DW"]][["WDS"]]
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
      state = DW_fetch_state(id           = id,
                             input        = input,
                             session      = session,
                             yaml_file    = yaml_file,
                             yaml_section = yaml_section,
                             id_UD        = id_UD,
                             react_state  = react_state)
      req(input$select_dw_element)
      uiele = NULL
      if(state[["DW"]][["isgood"]]){
        # Current final dataset
        WDS = state[["DW"]][["WDS"]]
        # Columns in that dataset
        dscols = names(WDS)
        uiele = tagList(
          div(style = "display: flex;",
              pickerInput(
                inputId  = NS(id, "select_fds_mutate_column"),
                label    = NULL,
                width    = 200,
                choices  = dscols,
                options  = list(
                  title = state[["MC"]][["labels"]][["fds_mutate_column"]])
              ),
              #selectizeInput(
              #   inputId  = NS(id, "select_fds_mutate_column"),
              #   label    = NULL,
              #   width    = 200,
              #   choices  = dscols,
              #   options = list(
              #     placeholder = state[["MC"]][["labels"]][["fds_mutate_column"]],
              #     maxItems = 1)
              #),
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
      state = DW_fetch_state(id           = id,
                             input        = input,
                             session      = session,
                             yaml_file    = yaml_file,
                             yaml_section = yaml_section,
                             id_UD        = id_UD,
                             react_state  = react_state)
      req(input$select_dw_element)
      uiele = NULL
      if(state[["DW"]][["isgood"]]){
        # Current final dataset
        WDS = state[["DW"]][["WDS"]]
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
      state = DW_fetch_state(id           = id,
                             input        = input,
                             session      = session,
                             yaml_file    = yaml_file,
                             yaml_section = yaml_section,
                             id_UD        = id_UD,
                             react_state  = react_state)
      req(input$select_dw_element)
      uiele = NULL
      if(state[["DW"]][["isgood"]]){
        # Current final dataset
        WDS = state[["DW"]][["WDS"]]
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
    #------------------------------------
    output$ui_dw_fds_ungroup             = renderUI({
      req(input$select_dw_element)
      state = DW_fetch_state(id           = id,
                             input        = input,
                             session      = session,
                             yaml_file    = yaml_file,
                             yaml_section = yaml_section,
                             id_UD        = id_UD,
                             react_state  = react_state)
      req(input$select_dw_element)
      uiele = NULL
      if(state[["DW"]][["isgood"]]){
        # Current final dataset
        WDS = state[["DW"]][["WDS"]]
        # Columns in that dataset
        dscols = names(WDS)
        uiele = tagList(
          tags$b("ungroup dataset")
        )
      }
      uiele
    })
    #------------------------------------
    # filter
    output$ui_dw_fds_filter_column_select = renderUI({
      state = DW_fetch_state(id           = id,
                             input        = input,
                             session      = session,
                             yaml_file    = yaml_file,
                             yaml_section = yaml_section,
                             id_UD        = id_UD,
                             react_state  = react_state)
      req(input$select_dw_element)
      uiele = NULL
      if(state[["DW"]][["isgood"]]){
        # Current final dataset
        WDS = state[["DW"]][["WDS"]]
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
      state = DW_fetch_state(id           = id,
                             input        = input,
                             session      = session,
                             yaml_file    = yaml_file,
                             yaml_section = yaml_section,
                             id_UD        = id_UD,
                             react_state  = react_state)
      req(input$select_fds_filter_column)
      uiele = NULL
      if(state[["DW"]][["isgood"]]){
        # Current final dataset
        WDS = state[["DW"]][["WDS"]]
        # Columns in that dataset
        dscols = names(WDS)
        
        filter_col = state[["DW"]][["ui"]][["select_fds_filter_column"]]
        
        if(is.factor(WDS[[filter_col]])){
          choices  = state[["MC"]][["op_choices"]][["factor"]]
        } else {
          choices  = state[["MC"]][["op_choices"]][["not_factor"]]
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
      state = DW_fetch_state(id           = id,
                             input        = input,
                             session      = session,
                             yaml_file    = yaml_file,
                             yaml_section = yaml_section,
                             id_UD        = id_UD,
                             react_state  = react_state)
      req(input$select_fds_filter_column)
      req(input$select_fds_filter_operator)
      uiele = NULL
      if(state[["DW"]][["isgood"]]){
        # Current final dataset
        WDS = state[["DW"]][["WDS"]]
        
        # If the user filters down to zero rows
        if(nrow(WDS) > 0){
          # Columns in that dataset
          dscols = names(WDS)
          
          filter_col = state[["DW"]][["ui"]][["select_fds_filter_column"]]
          
          choices  = sort(unique(unfactor((WDS[[filter_col]]))))
          
          # We process factors different than
          if(is.factor(WDS[[filter_col]])){
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
      state = DW_fetch_state(id           = id,
                             input        = input,
                             session      = session,
                             yaml_file    = yaml_file,
                             yaml_section = yaml_section,
                             id_UD        = id_UD,
                             react_state  = react_state)
      
      if(state[["DW"]][["DS"]][["isgood"]]){
        uiele = tagList()
        choicesOpt = list(
          subtext = c(
            state[["MC"]][["actions"]][["filter"]] [["subtext"]],
            state[["MC"]][["actions"]][["mutate"]] [["subtext"]],
            state[["MC"]][["actions"]][["rename"]] [["subtext"]],
            state[["MC"]][["actions"]][["group"]]  [["subtext"]],
            state[["MC"]][["actions"]][["ungroup"]][["subtext"]]
          ),
          # icon    = c(
          #   "glyphicon-filter" ,
          #   "glyphicon-wrench",
          #   "glyphicon-edit",
          #   "glyphicon-resize-small",
          #   "glyphicon-resize-full"
          # ))
          icon    = c(
            "filter" ,
            "wrench",
            "edit",
            "resize-small",
            "resize-full"
          ),
          lib = rep("glyphicon", length(icon)))
          
        
        cnames = c( state[["MC"]][["actions"]][["filter"]] [["choice"]] ,
                    state[["MC"]][["actions"]][["mutate"]] [["choice"]] ,
                    state[["MC"]][["actions"]][["rename"]] [["choice"]] ,
                    state[["MC"]][["actions"]][["group"]]  [["choice"]] ,
                    state[["MC"]][["actions"]][["ungroup"]][["choice"]]
        )
        choices   = c(
          "filter",
          "mutate",
          "rename",
          "group",
          "ungroup"
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
      # Forcing a reaction to changes from the upload data module
      react_state[[id_UD]]
      state = DW_fetch_state(id           = id,
                             input        = input,
                             session      = session,
                             yaml_file    = yaml_file,
                             yaml_section = yaml_section,
                             id_UD        = id_UD,
                             react_state  = react_state)
      
      if(state[["DW"]][["DS"]][["isgood"]]){
        uiele = tagList(
          actionBttn(
            inputId = NS(id, "button_dw_add_element"),
             label = state[["MC"]][["labels"]][["add_element"]],
             icon  = icon("plus-sign", lib="glyphicon"),
             color = "primary",
             style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]]
             ))
      } else {
        uiele = NULL
      }
      
      uiele
    })
    #------------------------------------
    # table preview of the data
    output$hot_data_preview =  rhandsontable::renderRHandsontable({
      # Forcing a reaction to changes from the upload data module
      react_state[[id_UD]]
      # Triggering rebuilding of the table
      input$button_dw_add_element
      # Force update on deletion clicks
      input$hot_dw_elements
      state = DW_fetch_state(id           = id,
                             input        = input,
                             session      = session,
                             yaml_file    = yaml_file,
                             yaml_section = yaml_section,
                             id_UD        = id_UD,
                             react_state  = react_state)
      #ds = isolate(react_state[[id_UD]])
      #df = ds$DS$contents
      
      if(state[["DW"]][["DS"]][["isgood"]]){
        df = state[["DW"]][["WDS"]]
        ds = state[["DW"]][["DS"]]
        if(ds[["isgood"]]){
          uiele = rhandsontable::rhandsontable(
            df,
            width  = state[["MC"]][["dimensions"]][["preview"]][["width"]],
            height = state[["MC"]][["dimensions"]][["preview"]][["height"]],
            rowHeaders = NULL
          )
        } else {
          uiele = NULL
        }
      } else {
        uiele = NULL
      }
      uiele
    })
    #------------------------------------
    # Creates the ui for the compact view of the module
    output$DW_ui_compact  =  renderUI({
      state = DW_fetch_state(id           = id,
                             input        = input,
                             session      = session,
                             yaml_file    = yaml_file,
                             yaml_section = yaml_section,
                             id_UD        = id_UD,
                             react_state  = react_state)
      
      uiele = NULL
      
      uiele_main = tagList(
        div(style="display:inline-block", 
            htmlOutput(NS(id, "ui_dw_select"))),
        div(style="display:inline-block", 
            htmlOutput(NS(id, "ui_dw_add_element_button"))),
        htmlOutput(NS(id, "ui_dw_new_element_row")),
        verbatimTextOutput(NS(id, "ui_dw_new_element_msg")),
        rhandsontable::rHandsontableOutput(NS(id, "hot_dw_elements")))
      if( state$MC$compact$preview){
        uiele_main = tagList(
          uiele_main, 
          tags$br(),
          rhandsontable::rHandsontableOutput(NS(id, "hot_data_preview")))
      }
      
      if( state$MC$compact$code){
        # uiele_preview = tagList(htmlOutput(NS(id, "UD_ui_data_preview")))
        uiele_code = tagList(shinyAce::aceEditor(NS(id, "ui_dw_code")))
        
        uiele_str ="tabPanel(state$MC$labels$tab_main,   uiele_main)"
        if(state$MC$compact$code){
          uiele_str = paste0(uiele_str, ",tabPanel(state$MC$labels$tab_code, uiele_code)") }
        
        uiele_str = paste0("tabsetPanel(",uiele_str, ")")
        
        uiele = eval(parse(text=uiele_str))
      } else {
        uiele = uiele_main
      }
      
      uiele
    })
    #------------------------------------
    # Creating reaction if a variable has been specified
    if(!is.null(react_state)){
      # Here we list the ui inputs that will result in a state change:
      toListen <- reactive({
        list(input$button_dw_add_element)
      })
      # This updates the reaction state:
      observeEvent(toListen(), {
        react_state[[id]] = DW_fetch_state(
          id           = id,
          input        = input,
          session      = session,
          yaml_file    = yaml_file,
          yaml_section = yaml_section,
          id_UD        = id_UD,
          react_state  = react_state)
      })
    }
  })
}


#'@export
#'@title Fetch Data Wrangling State
#'@description Merges default app options with the changes made in the UI
#'@param id Shiny module ID
#'@param input Shiny input variable
#'@param session Shiny session variable
#'@param yaml_file cofiguration file
#'@param yaml_section  Section of the yaml file with the module configuration
#'@param id_UD  ID string for the upload data module used to handle uploads or 
#'the name of the list element in react_state where the data set is stored.
#'@param react_state Variable passed to server to allow reaction outside of 
#'module (`NULL`)
#'@return list containing the current state of the app including default
#'values from the yaml file as well as any changes made by the user
DW_fetch_state = function(id,           input,           session,
                          yaml_file,    yaml_section,    id_UD,
                          react_state){
  
  # After the app has loaded the state must be initialized
  FM_DW_ID = paste0("FM_DW_", id)
  
  #---------------------------------------------
  # Getting the current state
  if(is.null(session$userData[[FM_DW_ID]])){
    # General state information
    state = DW_init_state(yaml_file, yaml_section)
  } else {
    # If it's not null we just pluck the state
    # from the session variable
    state = session$userData[[FM_DW_ID]]
  }
  
  
  #---------------------------------------------
  # detecting changes in the datasets
  if("checksum" %in% names(react_state[[id_UD]][["DS"]])){
    # Checksum of the uploaded dataset from the UD module
    UD_checksum = isolate(react_state[[id_UD]][["DS"]][["checksum"]])
    # Checksum of the copy in the DW module:
    DW_checksum = isolate(state[["DW"]][["DS"]][["checksum"]])
    
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
      state = DW_init_state(yaml_file, yaml_section)
    }
  }
  
  #---------------------------------------------
  # Here we update the state based on user input
  # Dataset changes
  if(is.null(state[["DW"]][["DS"]])){
    if(!is.null(react_state)){
      # This contains the input dataset:
      state[["DW"]][["DS"]] = isolate(react_state[[id_UD]][["DS"]])
      
      # This contains the output dataset which when initialized will
      # be just the input above:
      state = set_wds(state, state[["DW"]][["DS"]][["contents"]])
    } else {
      state[["DW"]][["DS"]][["isgood"]] = FALSE
      state[["DW"]][["isgood"]]         = FALSE
    }
  }
  
  
  #---------------------------------------------
  # Getting the current ui elements into the state
  ui_elements = list(
    "hot_dw_elements"            = "update_elements",
    "button_dw_add_element"      = "add_element",
    "select_fds_filter_column"   = "filter",
    "select_fds_filter_operator" = "filter",
    "fds_filter_rhs"             = "filter",
    "select_fds_mutate_column"   = "mutate",
    "select_fds_mutate_rhs"      = "mutate",
    "select_fds_rename_column"   = "rename",
    "fds_rename_rhs"             = "rename",
    "select_fds_group_column"    = "group",
    "select_dw_element"          = "element_type"
  )
  for(ui_name in names(ui_elements)){
    if(!is.null(isolate(input[[ui_name]]))){
      state[["DW"]][["ui"]][[ui_name]] = isolate(input[[ui_name]])
    } else {
      state[["DW"]][["ui"]][[ui_name]] = ""
    }
  }
  
  
  # Here we're processing any element delete requests
  # - first we only do this if the hot_dw_elements has been defined
  if(is.list(state[["DW"]][["ui"]][["hot_dw_elements"]])){
    # - If that's the case we get the data frame for it:
    hot_df = rhandsontable::hot_to_r(state[["DW"]][["ui"]][["hot_dw_elements"]])
    # - Because the UI initialzes to a "no elements defined message" we need
    # to make sure there is a delete column
    if("Delete" %in% names(hot_df)){
      # - lastly we check to see if any have been selected for deletion:
      if(any(hot_df$Delete == TRUE)){
        
        # We walk through the current elements table and keep only the
        # rows where Delete is FALSE
        NEW_ET = NULL
        OLD_ET = state[["DW"]][["elements_table"]]
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
            
            # This starts by resetting the working data set (WDS) to the default
            # one:
            state = set_wds(state, state[["DW"]][["DS"]][["contents"]])
            
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
                  dwee_res = eval_element(state,NEW_ET[eridx,][["cmd"]])
                  
                  # Appending any messages
                  msgs = c(msgs, dwee_res[["msgs"]])
                  
                  if(dwee_res[["isgood"]]){
                    # We set the status to success here:
                    NEW_ET[eridx,][["Status"]] = "Success"
                    # Then we reset the WDS to the current value
                    state = set_wds(state, dwee_res[["DS"]])
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
              
              # Passing any messages ack to the user
              if(is.null(msgs)){
                state[["DW"]][["add_element_msg"]] = NULL
              } else {
                state[["DW"]][["add_element_msg"]] = paste(msgs, collapse = "\n")
              }
              
            }
            # Saving the NEW_ET over the elements_table in the state
            state[["DW"]][["elements_table"]]  = NEW_ET
          }
        }
      }
    }
  }
  
  # Detecting add_element clicks
  if(!is.null(state[["DW"]][["ui"]][["button_dw_add_element"]])){
    # Current value of the button in the UI
    button_ui = state[["DW"]][["ui"]][["button_dw_add_element"]]
    
    # If the button in the UI has a value different than the current state
    # Then we trigger addition of the DW element currently there
    if(state[["DW"]][["add_counter"]] != button_ui){
      
      if(button_ui != 0 & button_ui != ""){
        
        msgs = c()
        # Building the data wranglig command
        dwb_res = dwrs_builder(state)
        
        # saving the messages
        msgs = c(msgs, dwb_res[["msgs"]])
        
        # If the dwlyr command was successfully built we evaluate
        #the chain to make sure the new element runs correctly:
        if(dwb_res[["isgood"]]){
          # Pulling out the current wrangled dataset
          dwee_res = eval_element(state, dwb_res[["cmd"]])
          # Appending any messages
          msgs = c(msgs, dwee_res[["msgs"]])
          
          # If that was successful we
          if(dwee_res[["isgood"]]){
            # - save the new DS as the wrangled dataset (WDS)
            state = set_wds(state, dwee_res[["DS"]])
            
            # - append the cmd and description to the DW table
            ET = state[["DW"]][["elements_table"]]
            ET = rbind(ET,
                       data.frame(Action        = dwb_res[["action"]],
                                  Description   = dwb_res[["desc"]],
                                  cmd           = dwb_res[["cmd"]],
                                  Status        = "Success",
                                  Delete        = FALSE))
            state[["DW"]][["elements_table"]]  = ET
          }
        }
        
        # Passing any messages ack to the user
        if(is.null(msgs)){
          state[["DW"]][["add_element_msg"]] = NULL
        } else {
          state[["DW"]][["add_element_msg"]] = paste(msgs, collapse = "\n")
        }
      }
      
      # Lastly we save the button value from the UI to the state:
      state[["DW"]][["add_counter"]] = button_ui
    }
  }
  
  # Saving the state
  session$userData[[FM_DW_ID]] = state
  
  # Returning the state
  
  state
}

#'@export
#'@title Initialize DW Module State
#'@description Creates a list of the initialized module state
#'@param yaml_file App configuration file
#'@param yaml_section  Section of the yaml file with the module configuration
#'@return list containing an empty app state object
DW_init_state = function(yaml_file, yaml_section){
  state = list()
  # Reading in default information from the yaml file
  state[["yaml"]] = yaml::read_yaml(yaml_file)
  
  # This assigns the module config "MC" element to the correct yaml_section.
  state[["MC"]] = state[["yaml"]][[yaml_section]]
  
  # Creating operator table
  opdf = NULL
  for(op_idx in c(1:length(state[["MC"]][["operators"]]))){
    opdf = rbind(opdf,
                 as.data.frame(state[["MC"]][["operators"]][[op_idx]]))
  }
  
  tmpdf = dplyr::filter(opdf, .data[["type"]] == "factor")
  state[["MC"]][["op_choices"]][["factor"]] = stats::setNames(tmpdf$rop, c(tmpdf$text))
  tmpdf = dplyr::filter(opdf, .data[["type"]] == "not_factor")
  state[["MC"]][["op_choices"]][["not_factor"]] = stats::setNames(tmpdf$rop, c(tmpdf$text))
  
  
  # Defaults for the module
  DW_NULL =
    list(isgood           = TRUE,
         add_counter      = 0,         # counter tracking the ds_add_element button
         add_element_msg  = NULL,
         elements_table   = NULL)
  
  state[["DW"]] = DW_NULL
  
  state
}


#'@export
#'@title Builds a Data Wrangling R Statement From ui Elements:
#'@description Takes the current ui elements and constructs the appropriate
#'data wrangling command from the user input.
#'@param state module state with all of the current ui elements populated
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
  
  action = state[["DW"]][["ui"]][["select_dw_element"]]
  ui     = state[["DW"]][["ui"]]
  
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
  } else if(action == "ungroup"){
    # Nothing needs to be done here
  } else {
    isgood = FALSE
    msgs = c(msgs, state[["MC"]][["errors"]][["fds_group_column"]])
  }
  
  # if we made it this far then everything is good and we build the command:)
  if(isgood){
    if(       action == "filter"){
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
          state[["MC"]][["op_choices"]][["factor"]][state[["MC"]][["op_choices"]][["factor"]]
                                                    == ui[["select_fds_filter_operator"]]] )[1]
        desc = paste0(ui[["select_fds_filter_column"]]," ",
                      op_desc, " ",
                      paste(ui[["fds_filter_rhs"]], collapse=','))
      }
      cmd = paste("DS = dplyr::filter(DS,", cond_str, ")")
    } else if(action == "mutate"){
      rhs_str = ui[["select_fds_mutate_rhs"]]
      cmd = paste("DS = dplyr::mutate(DS,",
                  ui[["select_fds_mutate_column"]],
                  " = ",
                  rhs_str, ")")
      desc = paste( ui[["select_fds_mutate_column"]],
                    "=", rhs_str)
    } else if(action == "rename"){
      new_name =  ui[["fds_rename_rhs"]]
      cmd = paste("DS = dplyr::rename(DS,",
                  new_name,
                  " = ",
                  ui[["select_fds_rename_column"]],
                  ")")
      desc = paste(ui[["select_fds_rename_column"]], " to ", new_name)
      
    } else if(action == "group"){
      group_cols_str   = paste(ui[["select_fds_group_column"]], collapse=', ')
      cmd = paste("DS = dplyr::group_by(DS,",
                  group_cols_str,
                  ")")
      desc = paste(group_cols_str)
    } else if(action == "ungroup"){
      cmd = "DS = dplyr::ungroup(DS)"
      desc = "grouping removed"
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
#'@description Creates a list of the initialized app state
#'@param state data wrangling module state
#'@param cmd string containing the data wrangling command
#'@return list containing an empty app state object
eval_element = function(state, cmd){
  
  msgs = c()
  DS = state[["DW"]][["WDS"]]
  ET = NULL
  # Trying to evaluate the generated command against DS
  # to see if any errors are generated:
  tcres = tryCatch({
    eval(parse(text=cmd))
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
  }
  
  res = list(isgood = tcres[["isgood"]],
             msgs   = msgs,
             DS     = DS)
  
  res
}


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
  res
}

#'@export
#'@title Sets/Updates Wrangled Dataset Field (WDS)
#'@description Takes an object that is a factor and returns an unfactored
#'vector with the same type by the value removed
#'@param state module state with all of the current ui elements populated
#'@param WDS new wrangled data set
#'@return object with factors removed
set_wds = function(state, WDS){
  
  # Saving WDS
  state[["DW"]][["WDS"]]       = WDS
  
  # Saving the checksum
  state[["DW"]][["checksum"]]  = digest::digest(WDS, algo=c("md5"))
  
  state
}
