#'@import ggplot2
#'@import rhandsontable
#'@import shiny
#'@import shinyWidgets
#'@importFrom shinyAce aceEditor updateAceEditor
#'@importFrom ggforce facet_grid_paginate facet_wrap_paginate


#'@export
#'@title Figure Generation Server
#'@description Server function for the figure generation module
#'@param id An ID string that corresponds with the ID used to call the module's UI function
#'@param FM_yaml_file App configuration file with FM as main section.
#'@param MOD_yaml_file  Module configuration file with MC as main section.
#'@param id_ASM ID string for the app state management module used to save and load app states
#'@param id_UD  ID string for the upload data module used to handle uploads or the name of the list element in react_state where the data set is stored.
#'@param id_DW  ID string for the data wrangling module to process any uploaded data
#'@param deployed Boolean variable indicating whether the app is deployed or not.
#'@param react_state Variable passed to server to allow reaction outside of module (\code{NULL})
#'@return FG Server object
#'@example inst/test_apps/FM_compact.R
FG_Server <- function(id,
                FM_yaml_file  = system.file(package = "formods", "templates", "formods.yaml"),
                MOD_yaml_file = system.file(package = "formods", "templates", "FG.yaml"),
                id_ASM        = "ASM",
                id_UD         = "UD",
                id_DW         = "DW",
                deployed      = FALSE,
                react_state   = NULL) {
  moduleServer(id, function(input, output, session) {

    #------------------------------------
    output$hot_fg_elements = rhandsontable::renderRHandsontable({
      req(input$select_fg_element)
      input[["button_fig_new"]]
      input[["button_fig_save"]]
      input[["button_fig_del"]]
      input[["button_fig_copy"]]
      input[["button_element_add"]]
      input[["hot_fg_elements"]]
      input[["select_current_fig"]]

      state = FG_fetch_state(id             = id,
                             input          = input,
                             session        = session,
                             FM_yaml_file   = FM_yaml_file,
                             MOD_yaml_file  = MOD_yaml_file,
                             id_ASM         = id_ASM,
                             id_UD          = id_UD,
                             id_DW          = id_DW,
                             react_state    = react_state)
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
          hot_col("Element" ,      readOnly = TRUE) |>
          hot_col("Description" , readOnly = TRUE) |>
          hot_col("Status" ,      readOnly = TRUE)

        uiele = hot
      }


      uiele})
    #------------------------------------
    # Creates the ui for the compact view of the module
    output$FG_ui_compact  =  renderUI({

      # Forcing a reaction to changes in other modules
   # JMH data views fix
   #  react_state[[id_UD]]
   #  react_state[[id_DW]]
      react_state[[id_ASM]]
      state = FG_fetch_state(id             = id,
                             input          = input,
                             session        = session,
                             FM_yaml_file   = FM_yaml_file,
                             MOD_yaml_file  = MOD_yaml_file,
                             id_ASM         = id_ASM,
                             id_UD          = id_UD,
                             id_DW          = id_DW,
                             react_state    = react_state)

      current_fig = FG_fetch_current_fig(state)

      if(is.null(current_fig)){
        uiele = state[["MC"]][["labels"]][["no_figures_defined"]]
      } else {
        uiele_code_button = NULL
        # Generating code button if enabled
        if( state[["MC"]][["compact"]][["code"]]){
          uiele_code = tagList(shinyAce::aceEditor(
            NS(id, "ui_fg_code"),
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
             status  = "danger btn-custom-fg",
             icon    = icon("code", lib="font-awesome"),
             tooltip = shinyWidgets::tooltipOptions(title = state[["MC"]][["tooltips"]][["show_code"]]))
          )

        }


        # Button with FG elements table
        uiele_fg_elements = rhandsontable::rHandsontableOutput(NS(id, "hot_fg_elements"))
        uiele_fg_elements_button = tagList(
         shinyWidgets::dropdownButton(
           uiele_fg_elements,
           inline  = FALSE,
           right   = TRUE ,
           size    = "sm",
           circle  = FALSE,
           status  = "primary btn-custom-fg",
           icon    = icon("layer-group", lib="font-awesome"),
           tooltip = shinyWidgets::tooltipOptions(title = state[["MC"]][["tooltips"]][["fg_elements"]]))
        )

        uiele = tagList(
          div(style="display:inline-block", htmlOutput(NS(id, "ui_fg_curr_figs"))),
          div(style="display:inline-block", htmlOutput(NS(id, "ui_fg_fig_name"))),
          div(style="display:inline-block", htmlOutput(NS(id, "ui_fg_curr_views"))),
          tags$br(),
          div(style="display:inline-block", htmlOutput(NS(id, "ui_fg_fig_notes"))),
          tags$br(),
          verbatimTextOutput(NS(id, "ui_fg_msg"))
        )

        # We only show the clip button if it's enabled
        uiele_clip_button = NULL
        if(state[["MC"]][["compact"]][["clip"]]){
          uiele_clip_button = htmlOutput(NS(id, "ui_fg_clip_code"))
        }


        uiele_buttons_right = tagList(
                 tags$style(".btn-custom-fg {width: 100px;}"),
                 div(style="display:inline-block;vertical-align:top",
                 uiele_fg_elements_button,
                 uiele_code_button,
                 uiele_clip_button,
                 htmlOutput(NS(id, "ui_fg_save_fig")),
                 htmlOutput(NS(id, "ui_fg_copy_fig")),
                 htmlOutput(NS(id, "ui_fg_del_fig")),
                 htmlOutput(NS(id, "ui_fg_new_fig"))
                 ))

        # these are all the save, copy, etc buttons
        uiele_buttons_left = tagList(
       #  div(style="display:inline-block;vertical-align:top",
       #  htmlOutput(NS(id, "ui_fg_save_fig")),
       #  htmlOutput(NS(id, "ui_fg_copy_fig")),
       #  htmlOutput(NS(id, "ui_fg_del_fig")),
       #  htmlOutput(NS(id, "ui_fg_new_fig"))
       #  )
        )

        # Appending the preview
        pvh          = state[["MC"]][["formatting"]][["preview"]][["height"]]
        pvw          = state[["MC"]][["formatting"]][["preview"]][["width"]]
        pv_div_style = paste0("height:",pvh,"px;width:",pvw,"px;display:inline-block;vertical-align:top")



        plot_method = "ggplot"

        if(system.file(package = "plotly") != ""){
          if(state[["MC"]][["compact"]][["preview"]] == "plotly"){
            plot_method = "plotly"
          }

        }


        if(plot_method == "plotly"){
          uiele_preview =
             div(style=pv_div_style,
               plotly::plotlyOutput(NS("FG", "ui_fg_preview_plotly")))
           # div(style=pv_div_style,
           # div(style=pv_div_style,
           #   plotly::plotlyOutput(NS("FG", "ui_fg_preview_plotly"))),
           #   htmlOutput(NS("FG", "ui_fg_slider_page")))

        } else {
          uiele_preview =
             div(style=pv_div_style,
               plotOutput(NS("FG", "ui_fg_preview_ggplot"), width="100%", height="100%"))
        }

        #      htmlOutput(NS("FG", "ui_fg_slider_page"))))

          uiele = tagList(
            uiele,
            uiele_buttons_left,
            uiele_preview,
            uiele_buttons_right,
            tags$br(),
            htmlOutput(NS("FG", "ui_fg_select_page"))
          )

        uiele = tagList( uiele,
          tags$br(),
          div(style="display:inline-block", htmlOutput(NS(id, "ui_fg_add_element_button"))),
          div(style="display:inline-block", htmlOutput(NS(id, "ui_fg_select"))),
          tags$br(),
          htmlOutput(NS(id, "ui_fg_new_element_row")
          )
        )
      }


    uiele})
    #------------------------------------
    output$ui_fg_preview_ggplot   = renderPlot({
      # Forcing a reaction to changes in other modules
      react_state[[id_UD]]
      react_state[[id_DW]]
      react_state[[id_ASM]]
      # Forcing reactions:
      input[["button_fig_new"]]
      input[["button_fig_save"]]
      input[["button_fig_del"]]
      input[["button_fig_copy"]]
      input[["hot_fg_elements"]]
      input[["button_element_add"]]
      input[["select_current_fig"]]

      input[["select_fg_page"]]

      state = FG_fetch_state(id             = id,
                             input          = input,
                             session        = session,
                             FM_yaml_file   = FM_yaml_file,
                             MOD_yaml_file  = MOD_yaml_file,
                             id_ASM         = id_ASM,
                             id_UD          = id_UD,
                             id_DW          = id_DW,
                             react_state    = react_state)

      # Pausing access to the screen
      FM_pause_screen(state   = state,
                      message = state[["MC"]][["labels"]][["busy"]][["fig_update"]],
                      session = session)

      current_fig = FG_fetch_current_fig(state)
      fobj = current_fig[["fobj"]]

      if(current_fig[["num_pages"]] > 1){
        fobj = FG_extract_page(state, current_fig[["page"]])
      }

      # This forces a build of the ggplot to capure errors that only occur
      # when the figure is built.
      tcres =
        FM_tc(tc_env = list(fobj=fobj),
              cmd = "fbuild = ggplot2::ggplot_build(fobj); suppressMessages(ggsave(tempfile(fileext='.png'), fobj))",
              capture = c("fbuild"))

      if(tcres[["isgood"]]){
        fobj = tcres[["capture"]][["fbuild"]]
      } else {
        # capturing the errors and returning those in the figure
        fobj = FM_mk_error_fig(tcres[["msgs"]])

        # Making sure the messages are also returned
        state = FM_set_ui_msg(state, tcres[["msgs"]])
        FM_set_mod_state(session, id, state)
      }


      # Removing the pause
      FM_resume_screen(state   = state,
                       session = session)

      fobj})
    #------------------------------------
    if(system.file(package = "plotly") != ""){
     output$ui_fg_preview_plotly   = plotly::renderPlotly({
       # Forcing a reaction to changes in other modules
       react_state[[id_UD]]
       react_state[[id_DW]]
       react_state[[id_ASM]]
       # Forcing reactions:
       input[["button_fig_new"]]
       input[["button_fig_save"]]
       input[["button_fig_del"]]
       input[["button_fig_copy"]]
       input[["hot_fg_elements"]]
       input[["button_element_add"]]
       input[["select_current_fig"]]

       input[["select_fg_page"]]

       state = FG_fetch_state(id             = id,
                              input          = input,
                              session        = session,
                              FM_yaml_file   = FM_yaml_file,
                              MOD_yaml_file  = MOD_yaml_file,
                              id_ASM         = id_ASM,
                              id_UD          = id_UD,
                              id_DW          = id_DW,
                              react_state    = react_state)

       current_fig = FG_fetch_current_fig(state)

       current_fig = FG_fetch_current_fig(state)
       fobj = current_fig[["fobj"]]

       if(current_fig[["num_pages"]] > 1){
         fobj = FG_extract_page(state, current_fig[["page"]])
       }

       if(is.null(fobj)){
         uiele = NULL
       }else{
         uiele = plotly::ggplotly(fobj,
           width = state[["MC"]][["formatting"]][["preview"]][["width"]],
           height = state[["MC"]][["formatting"]][["preview"]][["height"]]
         )
       }

       uiele})
    }
    #------------------------------------
    output$ui_fg_select    = renderUI({
      #req(input$X)
      state = FG_fetch_state(id             = id,
                             input          = input,
                             session        = session,
                             FM_yaml_file   = FM_yaml_file,
                             MOD_yaml_file  = MOD_yaml_file,
                             id_ASM         = id_ASM,
                             id_UD          = id_UD,
                             id_DW          = id_DW,
                             react_state    = react_state)

      uiele = NULL
      if(state[["FG"]][["isgood"]]){
        uiele = tagList()


        # Creating the choices based on the names in the configuration file.
        subtext = c()
        cnames  = c()
        choices = c()
        for(choice in names(state[["MC"]][["elements"]])){
          choices = c(choices, choice)
          subtext = c(subtext, state[["MC"]][["elements"]][[choice]][["subtext"]])
          cnames  = c(cnames,  state[["MC"]][["elements"]][[choice]][["choice"]])
        }

        choicesOpt = list(
          subtext = subtext )
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
      # Forcing a reaction to changes in other modules
      react_state[[id_UD]]
      react_state[[id_DW]]
      react_state[[id_ASM]]
      #req(input$X)
      input[["button_fig_new"]]
      input[["button_fig_save"]]
      input[["button_fig_del"]]
      input[["button_fig_copy"]]
      input[["select_current_fig"]]
      state = FG_fetch_state(id             = id,
                             input          = input,
                             session        = session,
                             FM_yaml_file   = FM_yaml_file,
                             MOD_yaml_file  = MOD_yaml_file,
                             id_ASM         = id_ASM,
                             id_UD          = id_UD,
                             id_DW          = id_DW,
                             react_state    = react_state)


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
      state = FG_fetch_state(id             = id,
                             input          = input,
                             session        = session,
                             FM_yaml_file   = FM_yaml_file,
                             MOD_yaml_file  = MOD_yaml_file,
                             id_ASM         = id_ASM,
                             id_UD          = id_UD,
                             id_DW          = id_DW,
                             react_state    = react_state)

      uiele = NULL
      if(state[["FG"]][["isgood"]]){
        uiele = shinyWidgets::actionBttn(
                  inputId = NS(id, "button_fig_new"),
                  label   = state[["MC"]][["labels"]][["new_fig"]],
                  style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
                  size    = state[["MC"]][["formatting"]][["button_fig_new"]][["size"]],
                  block   = state[["MC"]][["formatting"]][["button_fig_new"]][["block"]],
                  color   = "success",
                  icon    = icon("plus"))


        # Optinally adding the tooltip:
        uiele = FM_add_ui_tooltip(state, uiele,
                 tooltip             = state[["MC"]][["formatting"]][["button_fig_new"]][["tooltip"]],
                 position    = state[["MC"]][["formatting"]][["button_fig_new"]][["tooltip_position"]])
      }
      uiele})
    #------------------------------------
    output$ui_fg_save_fig   = renderUI({
      #req(input$X)
      state = FG_fetch_state(id             = id,
                             input          = input,
                             session        = session,
                             FM_yaml_file   = FM_yaml_file,
                             MOD_yaml_file  = MOD_yaml_file,
                             id_ASM         = id_ASM,
                             id_UD          = id_UD,
                             id_DW          = id_DW,
                             react_state    = react_state)

      uiele = NULL
      if(state[["FG"]][["isgood"]]){
        uiele = shinyWidgets::actionBttn(
                  inputId = NS(id, "button_fig_save"),
                  label   = state[["MC"]][["labels"]][["save_fig"]],
                  style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
                  size    = state[["MC"]][["formatting"]][["button_fig_save"]][["size"]],
                  block   = state[["MC"]][["formatting"]][["button_fig_save"]][["block"]],
                  color   = "primary",
                  icon    = icon("arrow-down"))


        # Optinally adding the tooltip:
        uiele = FM_add_ui_tooltip(state, uiele,
                 tooltip     = state[["MC"]][["formatting"]][["button_fig_save"]][["tooltip"]],
                 position    = state[["MC"]][["formatting"]][["button_fig_save"]][["tooltip_position"]])

      }
      uiele})
    #------------------------------------
    output$ui_fg_clip_code  = renderUI({
      input$button_fg_clip
      state = FG_fetch_state(id             = id,
                             input          = input,
                             session        = session,
                             FM_yaml_file   = FM_yaml_file,
                             MOD_yaml_file  = MOD_yaml_file,
                             id_ASM         = id_ASM,
                             id_UD          = id_UD,
                             id_DW          = id_DW,
                             react_state    = react_state)

      # This is a suggest, so we only generate this button conditionally
      uiele = NULL
      if((system.file(package="clipr") != "") &
         !deployed){
        uiele = shinyWidgets::actionBttn(
                  inputId = NS(id, "button_fg_clip"),
                  label   = state[["MC"]][["labels"]][["clip_fig"]],
                  style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
                  size    = state[["MC"]][["formatting"]][["button_fig_clip"]][["size"]],
                  block   = state[["MC"]][["formatting"]][["button_fig_clip"]][["block"]],
                  no_outline = FALSE,
                  color   = "royal",
                  icon    = icon("clipboard", lib="font-awesome"))

        # Optinally adding the tooltip:
        uiele = FM_add_ui_tooltip(state, uiele,
                 tooltip     = state[["MC"]][["formatting"]][["button_fig_clip"]][["tooltip"]],
                 position    = state[["MC"]][["formatting"]][["button_fig_clip"]][["tooltip_position"]])

      }

      uiele})
    #------------------------------------
    output$ui_fg_del_fig   = renderUI({
      #req(input$X)
      state = FG_fetch_state(id             = id,
                             input          = input,
                             session        = session,
                             FM_yaml_file   = FM_yaml_file,
                             MOD_yaml_file  = MOD_yaml_file,
                             id_ASM         = id_ASM,
                             id_UD          = id_UD,
                             id_DW          = id_DW,
                             react_state    = react_state)

      uiele = NULL
      if(state[["FG"]][["isgood"]]){
        uiele = shinyWidgets::actionBttn(
                  inputId = NS(id, "button_fig_del"),
                  label   = state[["MC"]][["labels"]][["del_fig"]],
                  style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
                  size    = state[["MC"]][["formatting"]][["button_fig_del"]][["size"]],
                  block   = state[["MC"]][["formatting"]][["button_fig_del"]][["block"]],
                  color   = "danger",
                  icon    = icon("minus"))

        # Optinally adding the tooltip:
        uiele = FM_add_ui_tooltip(state, uiele,
                 tooltip     = state[["MC"]][["formatting"]][["button_fig_del"]][["tooltip"]],
                 position    = state[["MC"]][["formatting"]][["button_fig_del"]][["tooltip_position"]])


      }
      uiele})
    #------------------------------------
    output$ui_fg_copy_fig   = renderUI({
      #req(input$X)
      state = FG_fetch_state(id             = id,
                             input          = input,
                             session        = session,
                             FM_yaml_file   = FM_yaml_file,
                             MOD_yaml_file  = MOD_yaml_file,
                             id_ASM         = id_ASM,
                             id_UD          = id_UD,
                             id_DW          = id_DW,
                             react_state    = react_state)

      uiele = NULL
      if(state[["FG"]][["isgood"]]){
        uiele = shinyWidgets::actionBttn(
                  inputId = NS(id, "button_fig_copy"),
                  label   = state[["MC"]][["labels"]][["copy_fig"]],
                  style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
                  size    = state[["MC"]][["formatting"]][["button_fig_copy"]][["size"]],
                  block   = state[["MC"]][["formatting"]][["button_fig_copy"]][["block"]],
                  color   = "royal",
                  icon    = icon("copy"))

        # Optinally adding the tooltip:
        uiele = FM_add_ui_tooltip(state, uiele,
                 tooltip     = state[["MC"]][["formatting"]][["button_fig_copy"]][["tooltip"]],
                 position    = state[["MC"]][["formatting"]][["button_fig_copy"]][["tooltip_position"]])
      }
      uiele})
    #------------------------------------
    output$ui_fg_fig_notes = renderUI({
      # Forcing a reaction to changes in other modules
      react_state[[id_UD]]
      react_state[[id_DW]]
      react_state[[id_ASM]]
      #req(input$X)
      input[["button_fig_new"]]
      input[["button_fig_save"]]
      input[["button_fig_del"]]
      input[["button_fig_copy"]]
      input[["select_current_fig"]]
      state = FG_fetch_state(id             = id,
                             input          = input,
                             session        = session,
                             FM_yaml_file   = FM_yaml_file,
                             MOD_yaml_file  = MOD_yaml_file,
                             id_ASM         = id_ASM,
                             id_UD          = id_UD,
                             id_DW          = id_DW,
                             react_state    = react_state)

      uiele = NULL
      if(state[["FG"]][["isgood"]]){
        current_fig = FG_fetch_current_fig(state)
        value       = current_fig[["notes"]]
        uiele = textAreaInput(inputId     = NS(id, "text_fig_notes"),
                             width        = state[["MC"]][["formatting"]][["notes"]][["width"]],
                             height       = state[["MC"]][["formatting"]][["notes"]][["height"]],
                             label        = NULL,
                             value        = value,
                             placeholder  = state[["MC"]][["labels"]][["ph"]][["notes"]])
      }
      uiele})
    #------------------------------------
    output$ui_fg_select_page = renderUI({
      react_state[[id_UD]]
      react_state[[id_DW]]
      react_state[[id_ASM]]
      # Forcing reactions:
      input[["button_fig_new"]]
      input[["button_fig_save"]]
      input[["button_fig_del"]]
      input[["button_fig_copy"]]
      input[["hot_fg_elements"]]
      input[["button_element_add"]]
      input[["select_current_fig"]]

      state = FG_fetch_state(id             = id,
                             input          = input,
                             session        = session,
                             FM_yaml_file   = FM_yaml_file,
                             MOD_yaml_file  = MOD_yaml_file,
                             id_ASM         = id_ASM,
                             id_UD          = id_UD,
                             id_DW          = id_DW,
                             react_state    = react_state)

      # Figuring out the pages in the current figure
      current_fig = FG_fetch_current_fig(state)

      uiele =  " "
      if(current_fig[["num_pages"]] > 1){

        # If faceting changes and the selected facet page is > the current
        # number of pages then we set the current page to 1
        if( current_fig[["page"]] <= current_fig[["num_pages"]] ){
          current_page = current_fig[["page"]]
        } else {
          current_page = 1
        }

        pages = c(1:current_fig[["num_pages"]])
        uiele =
           pickerInput(
             inputId  = NS(id, "select_fg_page"),
             width    = state[["MC"]][["formatting"]][["select_fg_page"]][["width"]],
             selected = current_page,
             label    = NULL,
             choices  = pages)
          #sliderTextInput(
          #  inputId  = NS(id, "select_fg_page"),
          #  label    = NULL,
          #  width    = state[["MC"]][["formatting"]][["select_fg_page"]][["width"]],
          #  grid     = TRUE,
          #  selected = current_page,
          #  choices  = pages
          #)

        uiele  = FM_add_ui_tooltip(state, uiele,
                 tooltip     = state[["MC"]][["formatting"]][["select_fg_page"]][["tooltip"]],
                 position    = state[["MC"]][["formatting"]][["select_fg_page"]][["tooltip_position"]])
      }

      uiele})
    #------------------------------------
    output$ui_fg_add_element_button = renderUI({
      #req(input$X)

      state = FG_fetch_state(id             = id,
                             input          = input,
                             session        = session,
                             FM_yaml_file   = FM_yaml_file,
                             MOD_yaml_file  = MOD_yaml_file,
                             id_ASM         = id_ASM,
                             id_UD          = id_UD,
                             id_DW          = id_DW,
                             react_state    = react_state)

      uiele = "ui_fg_add_element_button"
      uiele = NULL
      if(state[["FG"]][["isgood"]]){
        uiele = shinyWidgets::actionBttn(
                  inputId = NS(id, "button_element_add"),
                  label   = state[["MC"]][["labels"]][["add_ele"]],
                  style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
                  size    = state[["MC"]][["formatting"]][["button_fig_add"]][["size"]],
                  block   = state[["MC"]][["formatting"]][["button_fig_add"]][["block"]],
                  color   = "success",
                  icon    = icon("plus-sign", lib="glyphicon"))
      }
      uiele})
    #------------------------------------
    output$ui_fg_msg = renderText({
      req(input$select_fg_element)
      input[["button_fig_new"]]
      input[["button_fig_save"]]
      input[["button_fig_del"]]
      input[["button_fig_copy"]]
      input[["button_element_add"]]
      state = FG_fetch_state(id             = id,
                             input          = input,
                             session        = session,
                             FM_yaml_file   = FM_yaml_file,
                             MOD_yaml_file  = MOD_yaml_file,
                             id_ASM         = id_ASM,
                             id_UD          = id_UD,
                             id_DW          = id_DW,
                             react_state    = react_state)

      uiele = state[["FG"]][["ui_msg"]]

      uiele})
    #------------------------------------
    output$ui_fg_new_element_row = renderUI({
      req(input$select_fg_element)
      input[["button_fig_save"]]
      # force update when a new plot element is selected
      state = FG_fetch_state(id             = id,
                             input          = input,
                             session        = session,
                             FM_yaml_file   = FM_yaml_file,
                             MOD_yaml_file  = MOD_yaml_file,
                             id_ASM         = id_ASM,
                             id_UD          = id_UD,
                             id_DW          = id_DW,
                             react_state    = react_state)

      # Pulling out the current plot element:
      curr_element = state[["FG"]][["ui"]][["select_fg_element"]]

      # These are the elements that are governed by aesthetics
      aes_elements = state[["FG"]][["aes_elements"]]

      # Getting the current figure
      current_fig = FG_fetch_current_fig(state)

      # Getting the columns of the dataset attached to the current figure:
      # JMH check the columns are being pulled
      fig_dscols = names(state[["FG"]][["DSV"]][["ds"]][[current_fig[["fig_dsview"]]]][["DS"]])

      tmp_dsview = state[["FG"]][["DSV"]][["ds"]][[current_fig[["fig_dsview"]]]][["DS"]]

      uiele = NULL
      if(state[["FG"]][["isgood"]]){

        # The UI for plot elements governed by aesthetics are constructed
        # consistently
        if(curr_element %in% aes_elements){
          aes_list = tagList()

          # Pulling out column header formatting information.
          hfmt = FM_fetch_data_format(
           state[["FG"]][["DSV"]][["ds"]][[current_fig[["fig_dsview"]]]][["DS"]],
           state)

          for(aes_idx in 1:length(state[["MC"]][["elements"]][[curr_element]][["ui_aes"]])){
            # Aesthetic name
            ui_aes = state[["MC"]][["elements"]][[curr_element]][["ui_aes"]][aes_idx]
            # these are the IDs for the UI elements to be generated
            # JMH remove this
            #id_select = state[["MC"]][["elements"]][[curr_element]][["ui_aes_select_id"]][aes_idx]
            #id_manual = state[["MC"]][["elements"]][[curr_element]][["ui_aes_manual_id"]][aes_idx]

            # JMH replace with this
            id_select = state[["FG"]][["auto_elements"]][[curr_element]][["ui_aes_select_id"]][aes_idx]
            id_manual = state[["FG"]][["auto_elements"]][[curr_element]][["ui_aes_manual_id"]][aes_idx]


            # Constructing the choices
            sel_names      = c()
            sel_choices    = c()
            sel_style      = c()
            sel_subtext    = c()

            # Making the first option Not Used if the aesthetic isn't
            # required:
            if(!ui_aes %in% state[["MC"]][["elements"]][[curr_element]][["aes_req"]]){
              sel_names      = c(sel_names  , state[["MC"]][["labels"]][["not_used"]])
              sel_choices    = c(sel_choices, "not_used")
              sel_subtext    = c(sel_subtext, "")
              sel_style      = c(sel_style,
                  paste0("background: ", state[["yaml"]][["FM"]][["ui"]][["color_red"]] ,"; color: white;"))
            }

            # Adding the columns
            sel_names   = c(sel_names  , fig_dscols)
            sel_choices = c(sel_choices, fig_dscols)
            sel_style   = c(sel_style  , rep("", length(fig_dscols)))
            sel_subtext = c(sel_subtext, as.vector(unlist( hfmt[["col_subtext"]])))

            # Adding manual option
            sel_names      = c(sel_names  , state[["MC"]][["labels"]][["manual"]])
            sel_choices    = c(sel_choices, 'manual')
            sel_subtext    = c(sel_subtext, "")
            sel_style      = c(sel_style,
                paste0("background: ", state[["yaml"]][["FM"]][["ui"]][["color_blue"]] ,"; color: white;"))

            # For the columns in the dataset the names and the values are the
            # same but for not_used and manual the value displaed in the UI
            # may be changed so we need to make sure that the value returned
            # in the server is consistent. This will map the displayed value
            # to the value returned by the by the UI to the server:
            names(sel_choices) = sel_names

            tmp_tI =
              textInput(
                 inputId     = NS(id, id_manual),
                 label       = NULL,
                 placeholder = state[["MC"]][["labels"]][["ph"]][["manual"]],
                 width       = state[["MC"]][["formatting"]][["components"]][["aes"]][["width"]])

            # Optinally adding the tooltip:
            tmp_tI = FM_add_ui_tooltip(state, tmp_tI,
                     tooltip     = state[["MC"]][["tooltips"]][["components"]][["manual"]][[ui_aes]],
                     position    = "bottom")

            aes_list    = tagList(aes_list,
              div(style="display:inline-block",
                div(
                  # Picker on top
                  pickerInput(
                    inputId    = NS(id, id_select),
                    label      = state[["MC"]][["labels"]][["components"]][[ui_aes]],
                    choices    = sel_choices,
                    width      = state[["MC"]][["formatting"]][["components"]][["aes"]][["width"]],
                    options    = list(size = state[["yaml"]][["FM"]][["ui"]][["select_size"]]),
                    choicesOpt = list( style   = sel_style,
                                       subtext = sel_subtext,
                                      "live-search"=TRUE)),
                  # Manual text input on the bottom
                    tmp_tI
                )
              )
            )
          }
          uiele = aes_list
        } else if(curr_element == "facet") {

          sel_choices = fig_dscols

          #-------------------------------------------
          # Building out the facet variables/columns ui
          uiele_vars  =
            pickerInput(
              inputId    = NS(id, "select_component_facet"),
              label      = state[["MC"]][["labels"]][["select_component_facet"]],
              multiple   = TRUE,
              choices    = sel_choices,
              width      = state[["MC"]][["formatting"]][["components"]][["facet"]][["width"]],
              options    = list(
                            size     = state[["yaml"]][["FM"]][["ui"]][["select_size"]],
                            maxItems = 2))

          uiele_vars   = FM_add_ui_tooltip(state, div(uiele_vars) ,
                   tooltip     = state[["MC"]][["formatting"]][["components"]][["facet"]][["tooltip"]],
                   position    = state[["MC"]][["formatting"]][["components"]][["facet"]][["tooltip_position"]])


          #-------------------------------------------
          # Building out the facet scale ui
          subtext = c()
          cnames  = c()
          choices = c()
          for(choice in names(state[["MC"]][["formatting"]][["components"]][["facet_scales"]][["choices"]])){
            choices = c(choices, choice)
            subtext = c(subtext, state[["MC"]][["formatting"]][["components"]][["facet_scales"]][["choices"]][[choice]][["subtext"]])
            cnames  = c(cnames,  state[["MC"]][["formatting"]][["components"]][["facet_scales"]][["choices"]][[choice]][["choice"]])
          }
          choicesOpt = list(
            subtext = subtext )
          names(choices) = cnames

          uiele_scale =
            pickerInput(
              inputId    = NS(id, "select_component_facet_scales"),
              label      = state[["MC"]][["labels"]][["facet_scales"]],
              multiple   = FALSE,
              selected   = state[["MC"]][["formatting"]][["components"]][["facet_scales"]][["default"]],
              choicesOpt = choicesOpt,
              choices    = choices,
              width      = state[["MC"]][["formatting"]][["components"]][["facet_scales"]][["width"]])
            # options    = list(
            #               size     = state[["yaml"]][["FM"]][["ui"]][["select_size"]],
            #               maxItems = 2))
            #
          uiele_scale  = FM_add_ui_tooltip(state, div(uiele_scale),
                   tooltip     = state[["MC"]][["formatting"]][["components"]][["facet_scales"]][["tooltip"]],
                   position    = state[["MC"]][["formatting"]][["components"]][["facet_scales"]][["tooltip_position"]])


          #-------------------------------------------
          # Numer of rows
          uiele_nrow =
            pickerInput(
              inputId    = NS(id, "select_component_facet_nrow"),
              label      = state[["MC"]][["labels"]][["facet_dims_nrow"]],
              multiple   = FALSE,
              selected   = state[["MC"]][["formatting"]][["components"]][["facet_dims"]][["nrow"]][["default"]],
              choices    = state[["MC"]][["formatting"]][["components"]][["facet_dims"]][["nrow"]][["choices"]],
              width      = state[["MC"]][["formatting"]][["components"]][["facet_dims"]][["nrow"]][["width"]])

          uiele_nrow   = FM_add_ui_tooltip(state, div(uiele_nrow),
                   tooltip     = state[["MC"]][["formatting"]][["components"]][["facet_dims"]][["nrow"]][["tooltip"]],
                   position    = state[["MC"]][["formatting"]][["components"]][["facet_dims"]][["nrow"]][["tooltip_position"]])
          #-------------------------------------------
          # Numer of cols
          uiele_ncol =
            pickerInput(
              inputId    = NS(id, "select_component_facet_ncol"),
              label      = state[["MC"]][["labels"]][["facet_dims_ncol"]],
              multiple   = FALSE,
              selected   = state[["MC"]][["formatting"]][["components"]][["facet_dims"]][["ncol"]][["default"]],
              choices    = state[["MC"]][["formatting"]][["components"]][["facet_dims"]][["ncol"]][["choices"]],
              width      = state[["MC"]][["formatting"]][["components"]][["facet_dims"]][["ncol"]][["width"]])

          uiele_ncol  = FM_add_ui_tooltip(state, div(uiele_ncol),
                   tooltip     = state[["MC"]][["formatting"]][["components"]][["facet_dims"]][["ncol"]][["tooltip"]],
                   position    = state[["MC"]][["formatting"]][["components"]][["facet_dims"]][["ncol"]][["tooltip_position"]])

          # Combining them together
          uiele = tagList( div(style="display:inline-block", uiele_vars),
                           div(style="display:inline-block", uiele_nrow),
                           div(style="display:inline-block", uiele_ncol),
                           div(style="display:inline-block", uiele_scale))

        } else if(curr_element == "scales") {

          scale_choices = c("linear", "log10", "log2")
          yscale =
            pickerInput(
              inputId    = NS(id, "select_component_yscale"),
              label      = state[["MC"]][["labels"]][["components"]][["yscale"]],
              selected   = "log10",
              multiple   = FALSE,
              choices    = scale_choices,
              width      = state[["MC"]][["formatting"]][["components"]][["axscale"]][["width"]])

          ylim =
             textInput(
                inputId     = NS(id, "text_component_ylim"),
                label       = state[["MC"]][["labels"]][["components"]][["ylim"]],
                placeholder = state[["MC"]][["labels"]][["ph"]][["axlim"]],
                width       = state[["MC"]][["formatting"]][["components"]][["axlim"]][["width"]])

          xscale =
            pickerInput(
              inputId    = NS(id, "select_component_xscale"),
              label      = state[["MC"]][["labels"]][["components"]][["xscale"]],
              selected   = "linear",
              multiple   = TRUE,
              choices    = scale_choices,
              width      = state[["MC"]][["formatting"]][["components"]][["axscale"]][["width"]])

          xlim =
             textInput(
                inputId     = NS(id, "text_component_xlim"),
                label       = state[["MC"]][["labels"]][["components"]][["xlim"]],
                placeholder = state[["MC"]][["labels"]][["ph"]][["axlim"]],
                width       = state[["MC"]][["formatting"]][["components"]][["axlim"]][["width"]])


          uiele = tagList(
            div(style="display:inline-block", yscale),
            div(style="display:inline-block", ylim),
            tags$br(),
            div(style="display:inline-block", xscale),
            div(style="display:inline-block", xlim)
          )

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
      # Forcing a reaction to changes in other modules
      react_state[[id_UD]]
      react_state[[id_DW]]
      react_state[[id_ASM]]

      #req(input$X)
      input[["button_fig_new"]]
      input[["button_fig_save"]]
      input[["button_fig_del"]]
      input[["button_fig_copy"]]
      input[["button_element_add"]]
      state = FG_fetch_state(id             = id,
                             input          = input,
                             session        = session,
                             FM_yaml_file   = FM_yaml_file,
                             MOD_yaml_file  = MOD_yaml_file,
                             id_ASM         = id_ASM,
                             id_UD          = id_UD,
                             id_DW          = id_DW,
                             react_state    = react_state)

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
           if(is.null(tmp_fig[["notes"]])){
             subtext = c(subtext, "")
           } else {
             subtext = c(subtext, strtrim(tmp_fig[["notes"]], 20))
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
    # DSV dataset views
    # This creates the selection UI
    output$ui_fg_curr_views = renderUI({
      state = FG_fetch_state(id             = id,
                             input          = input,
                             session        = session,
                             FM_yaml_file   = FM_yaml_file,
                             MOD_yaml_file  = MOD_yaml_file,
                             id_ASM         = id_ASM,
                             id_UD          = id_UD,
                             id_DW          = id_DW,
                             react_state    = react_state)

      choicesOpt = NULL
      uiele =
        shinyWidgets::pickerInput(
          selected   = "PH",
          inputId    = NS(id, "select_current_view"),
          label      = state[["MC"]][["labels"]][["select_current_view"]],
          choices    = c("PH"),
          width      = state[["MC"]][["formatting"]][["select_current_view"]][["width"]],
          choicesOpt = choicesOpt)

      uiele})
    #------------------------------------
    # This forces the dataset view selection to update
    observe({
      input$button_element_add
      input$button_fig_new
      input$button_fig_save
      input$button_fig_copy
      input$button_fig_del
      input$select_current_fig

      react_state[[id_UD]]
      react_state[[id_DW]]
      react_state[[id_ASM]]
      # Forcing a reaction to changes in other modules
      state = FG_fetch_state(id             = id,
                             input          = input,
                             session        = session,
                             FM_yaml_file   = FM_yaml_file,
                             MOD_yaml_file  = MOD_yaml_file,
                             id_ASM         = id_ASM,
                             id_UD          = id_UD,
                             id_DW          = id_DW,
                             react_state    = react_state)

      current_fig = FG_fetch_current_fig(state)

      # If this is triggered before datasets have been loaded the state will
      # be bad:
      # JMH check the dsviews stuff
      if(state[["FG"]][["isgood"]]){

        # Pulling out the data set views catalog
        ds_catalog = state[["FG"]][["DSV"]][["catalog"]]

        if(current_fig[["fig_dsview"]] %in% ds_catalog[["object"]]){
          current_view_id= current_fig[["fig_dsview"]]
        } else {
          current_view_id = ds_catalog[["object"]][1]
          FM_le(state, paste0("ui_fg_curr_views: dataset view missing."   ))
          FM_le(state, paste0("fig_key: ",     current_fig[["key"]]       ))
          FM_le(state, paste0("fig_dsview: ",  current_fig[["fig_dsview"]]))
          FM_le(state, paste0("switching to view:", current_view_id ))
        }

        choices        = ds_catalog[["object"]]
        names(choices) = ds_catalog[["label"]]

        choicesOpt = NULL
        shinyWidgets::updatePickerInput(
          session    = session,
          selected   = current_view_id,
          inputId    = "select_current_view",
          choices    = choices,
          choicesOpt = choicesOpt)
      }
    })
    #------------------------------------
    observeEvent(input$button_fg_clip, {
      state = FG_fetch_state(id             = id,
                             input          = input,
                             session        = session,
                             FM_yaml_file   = FM_yaml_file,
                             MOD_yaml_file  = MOD_yaml_file,
                             id_ASM         = id_ASM,
                             id_UD          = id_UD,
                             id_DW          = id_DW,
                             react_state    = react_state)
      # This is all conditional on the whether clipr is installed $
      # and if the app isn't deployed
      if((system.file(package="clipr") != "") &
         !deployed){

        uiele = NULL
        current_fig = FG_fetch_current_fig(state)

        if(is.null(current_fig[["elements_table"]])){
          uiele = "# No figure elements defined yet!"
        } else {
          uiele = current_fig[["code"]]
        }
        clipr::write_clip(uiele)
      }
    })
    #------------------------------------
    observe({
      # Forcing a reaction to changes in other modules
      react_state[[id_UD]]
      react_state[[id_DW]]
      react_state[[id_ASM]]
      input[["button_fig_new"]]
      input[["button_fig_save"]]
      input[["button_fig_del"]]
      input[["button_fig_copy"]]
      input[["button_element_add"]]
      input[["hot_fg_elements"]]
      input[["select_current_fig"]]

      state = FG_fetch_state(id             = id,
                             input          = input,
                             session        = session,
                             FM_yaml_file   = FM_yaml_file,
                             MOD_yaml_file  = MOD_yaml_file,
                             id_ASM         = id_ASM,
                             id_UD          = id_UD,
                             id_DW          = id_DW,
                             react_state    = react_state)

      uiele = NULL
      current_fig = FG_fetch_current_fig(state)

      if(is.null(current_fig[["elements_table"]])){
        uiele = "# No figure elements defined yet!"
      } else {
        uiele = current_fig[["code"]]
        # Adding the preamble to load necessary packages
        mod_deps = FM_fetch_deps(state = state, session = session)
        if("package_code" %in% names(mod_deps)){
          uiele = paste0(c(mod_deps$package_code, "", uiele), collapse="\n")
        }
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
    # Creating reaction if a variable has been specified
    if(!is.null(react_state)){
      # Here we list the ui inputs that will result in a state change:
      toListen <- reactive({
        list(input$button_element_add,
             input$button_fig_new,
             input$button_fig_save,
             input$button_fig_copy,
             input$button_fig_del,
             react_state[[id_UD]],
             react_state[[id_DW]],
             react_state[[id_ASM]]
            )
      })
      # This updates the reaction state:
      observeEvent(toListen(), {

        state = FG_fetch_state(id             = id,
                               input          = input,
                               session        = session,
                               FM_yaml_file   = FM_yaml_file,
                               MOD_yaml_file  = MOD_yaml_file,
                               id_ASM         = id_ASM,
                               id_UD          = id_UD,
                               id_DW          = id_DW,
                               react_state    = react_state)

        FM_le(state, "reaction state updated")
        #react_state[[id]] = state
        react_state[[id]][["FG"]][["checksum"]] = state[["FG"]][["checksum"]]
      })
    }

    # Removing holds
    remove_hold_listen  <- reactive({
      list(react_state[[id_ASM]],
           input$select_current_fig,
           input$select_current_view)
    })
    observeEvent(remove_hold_listen(), {
      # Once the UI has been regenerated we
      # remove any holds for this module
      state = FG_fetch_state(id             = id,
                             input          = input,
                             session        = session,
                             FM_yaml_file   = FM_yaml_file,
                             MOD_yaml_file  = MOD_yaml_file,
                             id_ASM         = id_ASM,
                             id_UD          = id_UD,
                             id_DW          = id_DW,
                             react_state    = react_state)
      FM_le(state, "removing holds")
      # Removing all holds
      for(hname in names(state[["FG"]][["ui_hold"]])){
        remove_hold(state, session, hname)
      }
    }, priority = -100)


  })
}


#'@export
#'@title Fetch Figure Generation State
#'@description Merges default app options with the changes made in the UI
#'@param id Shiny module ID
#'@param input Shiny input variable
#'@param FM_yaml_file App configuration file with FM as main section.
#'@param MOD_yaml_file  Module configuration file with MC as main section.
#'@param id_ASM ID string for the app state management module used to save and load app states
#'@param id_UD  ID string for the upload data module used to handle uploads or the name of the list element in react_state where the data set is stored.
#'@param id_DW  ID string for the data wrangling module to process any uploaded data
#'@param react_state Variable passed to server to allow reaction outside of module (\code{NULL})
#'@param session Shiny session variable
#'@return list containing the current state of the app including default
#'values from the yaml file as well as any changes made by the user. The
#'structure of the list is defined below:
#'\itemize{
#'  \item{yaml:} Contents of the yaml file.
#'  \item{MC:} Module components of the yaml file.
#'  \item{FG:} Data wrangling state
#'  \itemize{
#'    \item{isgood:} Boolean status of the state. Currently just TRUE
#'    \item{button_counters:}  List of counters to detect button clicks.
#'    \item{ui_msg:}           Message returned when users perform actions.
#'    \item{ui:}               Current value of form elements in the UI.
#'    \item{ui_ids:}           Vector of UI elements for the module.
#'    \item{ui_hold:}          List of hold elements to disable updates before a full ui referesh is complete.
#'    \item{checksum:          checksum of the FG module used to detect changes in the module.}
#'    \item{aes_elements:}     Plot elements defined by aesthetics (i.e. the X in geom_X)
#'    \item{current_fig:}      fig_id of the currently figure.
#'    \item{fig_cntr:}         Counter for figures, incremented each time a new figure is created.
#'    \item{DSV:}              Available data sets from the UD and DW modules.
#'    \item{figs:}             List of figures. Each view has the following structure:
#'      \itemize{
#'        \item{add_isgood:     Boolean indicating the success/failure of adding the last figure elemnt.}
#'        \item{checksum:       Checksum of the figure used to detect changes in the figure.}
#'        \item{code:           Code to generate figure from start to finish.}
#'        \item{code_fg_only:   Code to just generate the figure.}
#'        \item{code_previous:  Code to load and/or wrangle the dataset.}
#'        \item{elements_table: Table of figure generation elements.}
#'        \item{fg_object_name: The object name that contains the figure in the generated code.}
#'        \item{fig_dsview:     Name of the dataset view for the current figure (also the R object name of the dataset view).}
#'        \item{fobj:           The ggplot object of the figure.}
#'        \item{id:             Character id (\code{fig_idx})}
#'        \item{idx:            Numeric id (\code{1})}
#'        \item{isgood:}        Boolean status of the figure. FALSE if evaluation/build fails.
#'        \item{key:            Figure key acts as a title/caption (user editable)}
#'        \item{msgs:           Any messages generated when building the
#'        figure.}
#'        \item{notes:          Figure notes  (user editable)}
#'        \item{num_pages:      Number of pages in the figure.}
#'        \item{page:           The currently selected figure page.}
#'      }
#'  }
#'  \item{MOD_TYPE:} Character data containing the type of module \code{"DW"}
#'  \item{id:} Character data containing the module id module in the session variable.
#'  \item{FM_yaml_file:} App configuration file with FM as main section.
#'  \item{MOD_yaml_file:}  Module configuration file with MC as main section.
#'}
#'@examples
#' # Configuration files
#' FM_yaml_file  = system.file(package = "formods", "templates", "formods.yaml")
#' MOD_yaml_file = system.file(package = "formods", "templates", "FG.yaml")
#'
#' # We need to specify both the FG module id as well as the
#' # id of the UD module that feeds into it.
#' id    = "FG"
#' id_UD = "UD"
#' id_DW = "DW"
#'
#' # These would be the Shiny input and session variables
#' input   = list()
#' session = list()
#'
#' # Creating an empty state object
#' state = FG_fetch_state(id              = id,
#'                        input           = input,
#'                        session         = session,
#'                        FM_yaml_file    = FM_yaml_file,
#'                        MOD_yaml_file   = MOD_yaml_file,
#'                        id_UD           = id_UD,
#'                        id_DW           = id_DW,
#'                        react_state     = NULL)
#'
#' state
FG_fetch_state = function(id,
                          input,
                          session,
                          FM_yaml_file,
                          MOD_yaml_file,
                          id_ASM = NULL,
                          id_UD  = NULL,
                          id_DW  = NULL,
                          react_state){

  # After the app has loaded the state must be initialized

  #---------------------------------------------
  # Getting the current state
  state = FM_fetch_mod_state(session, id)
  # If the state has not yet been defined then we
  # initialize it

  if(is.null(state)){
    # General state information
    state = FG_init_state(FM_yaml_file    = FM_yaml_file,
                          MOD_yaml_file   = MOD_yaml_file,
                          id              = id,
                          id_UD           = id_UD,
                          id_DW           = id_DW,
                          session         = session)
  }

  # detecting changes in the datasets
  UPDATE_DS = FALSE
  # Changes in uploaded dataset
  if("checksum" %in% names(isolate(react_state[[id_UD]][["UD"]]))){
    if(!is.null(isolate(react_state[[id_UD]][["UD"]][["checksum"]]))){
      if(is.null(state[["FG"]][["DSV"]][["modules"]][["UD"]][[id_UD]])){
        # If the UD checksum isn't NULL but the stored value in DSV is then we
        # need to update the dataset
        UPDATE_DS = TRUE
      } else if(isolate(react_state[[id_UD]][["UD"]][["checksum"]]) !=
                state[["FG"]][["DSV"]][["modules"]][["UD"]][[id_UD]]){
        # If the stored checksum in DSV is different than the currently
        # uploaded dataset in UD then we force a reset as well:
        UPDATE_DS = TRUE
      }
    }
  }

  # Changes in data views from data wrangling module
  if("checksum" %in% names(isolate(react_state[[id_DW]][["DW"]]))){
    if(!is.null(isolate(react_state[[id_DW]][["DW"]][["checksum"]]))){
      if(isolate(react_state[[id_DW]][["DW"]][["hasds"]])){
        if(is.null(state[["FG"]][["DSV"]][["modules"]][["DW"]][[id_DW]])){
          # If the DW checksum isn't NULL but the stored value in DSV is then we
          # need to update the dataset
          UPDATE_DS = TRUE
        } else if(isolate(react_state[[id_DW]][["DW"]][["checksum"]]) !=
                  state[["FG"]][["DSV"]][["modules"]][["DW"]][[id_DW]]){
          # If the stored checksum in DSV is different than the currently
          # uploaded dataset in DW then we force a reset as well:
          UPDATE_DS = TRUE
        }
      } else {
        # If there is no dataset but there was one once before we also
        # trigger a dataset update:
        if(!is.null(state[["FG"]][["DSV"]][["modules"]][["DW"]][[id_DW]])){
          UPDATE_DS = TRUE
        }
      }
    }
  }

 #message(" IN FG_fetch_state()")
 #message(paste0("UPDATE_DS: ", UPDATE_DS))
 #message(paste0("UD: ", isolate(react_state[[id_UD]][["UD"]]),                " source" ))
 #message(paste0("UD: ", state[["FG"]][["DSV"]][["modules"]][["UD"]][[id_UD]], " current"))
 #message(paste0("DW: ", isolate(react_state[[id_DW]][["DW"]][["checksum"]]),  " source" ))
 #message(paste0("DW: ", state[["FG"]][["DSV"]][["modules"]][["DW"]][[id_DW]], " current"))

  if(UPDATE_DS){
    FM_le(state, "Updating DS")
    # If the module initializes and there is a dataset then the figure
    # generation state will be good. Then we just need to attach the updated
    # dataset views:
    if(state[["FG"]][["isgood"]]){
      state[["FG"]][["DSV"]] = FM_fetch_ds(state, session, c(id_UD, id_DW))
    } else {
      # If there is no dataset loaded the figure generation state will be bad
      # (isgood is FALSE). Then we need to reinitialize the module:
      state = FG_init_state(FM_yaml_file    = FM_yaml_file,
                            MOD_yaml_file   = MOD_yaml_file,
                            id              = id,
                            id_UD           = id_UD,
                            id_DW           = id_DW,
                            session         = session)
    }


    # Forcing a figure rebuild when a database change has been detected
    # JMH should we loop through all figures and rebuild them all?
    state = FG_build( state=state, del_row = NULL, cmd = NULL, pll=NULL)
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
            state = FG_build( state=state, del_row = del_row, cmd = NULL, pll=NULL)
          }
        }
      }
    }
  }


  # Detecting figure selection changes
  if(has_changed(ui_val   = state[["FG"]][["ui"]][["select_current_fig"]],
                 old_val  = state[["FG"]][["current_fig"]]) &
      (!fetch_hold(state, "select_current_fig"))){

    # Changing the current view to the one selected in the UI
    state[["FG"]][["current_fig"]]  =  state[["FG"]][["ui"]][["select_current_fig"]]
  }

  # Detecting page selects
  if(has_changed(ui_val   = state[["FG"]][["ui"]][["select_fg_page"]],
                 old_val  =  FG_fetch_current_fig(state)[["page"]])
      #(!fetch_hold(state, "select_fg_page"))
      ){

    FM_le(state, "figure page change detected")

    # pulling the current figure
    current_fig = FG_fetch_current_fig(state)

    # updating the view id
    current_fig[["page"]] = state[["FG"]][["ui"]][["select_fg_page"]]

    # saving the updated figure
    state = FG_set_current_fig(state, current_fig)

  }
  # Adding a new element
  if(has_changed(ui_val   = state[["FG"]][["ui"]][["button_element_add"]],
                 old_val  = state[["FG"]][["button_counters"]][["button_element_add"]])){

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
        pll     = fgb_res[["pll"]],
        element = fgb_res[["element"]],
        desc    = fgb_res[["desc"]])
    }

    # Saving the button state to the counter
    state[["FG"]][["button_counters"]][["button_element_add"]] =
      state[["FG"]][["ui"]][["button_element_add"]]

    # pulling out the current figure to extract any messages
    # generated from the build process
    current_fig = FG_fetch_current_fig(state)
    msgs = c(msgs, current_fig[["msgs"]])

    # Updating any messages
    state = FM_set_ui_msg(state, msgs)
  }
  # New figure
  if(has_changed(ui_val   = state[["FG"]][["ui"]][["button_fig_new"]],
                 old_val  = state[["FG"]][["button_counters"]][["button_fig_new"]])){

    FM_le(state, "creating new figure")
    msgs = c()

    # Creating a new figure
    state = FG_new_fig(state)

    # Setting hold for figure select
    state = set_hold(state, inputId = "select_current_fig")
    state = set_hold(state, inputId = "select_current_view")

    # Saving the button state to the counter
    state[["FG"]][["button_counters"]][["button_fig_new"]] =
      state[["FG"]][["ui"]][["button_fig_new"]]

    # Updating any messages
    state = FM_set_ui_msg(state, msgs)
  }
  # Delete figure
  if(has_changed(ui_val   = state[["FG"]][["ui"]][["button_fig_del"]],
                 old_val  = state[["FG"]][["button_counters"]][["button_fig_del"]])){

    FM_le(state, "deleting figure")
    msgs = c()

    # Getting the current figure
    current_fig = FG_fetch_current_fig(state)

    # Deleting the figure
    state[["FG"]][["figs"]][[current_fig[["id"]]]] = NULL

    # If there are no figures left then we create an empty one
    if( length(state[["FG"]][["figs"]])  == 0){
      state = FG_new_fig(state)
    } else {
      # If there are figures then we set the first one as active
      state[["FG"]][["current_fig"]] = names(state[["FG"]][["figs"]])[1]
    }

    # Setting hold for figure select
    state = set_hold(state, inputId = "select_current_fig")
    state = set_hold(state, inputId = "select_current_view")

    # Saving the button state to the counter
    state[["FG"]][["button_counters"]][["button_fig_del"]] =
      state[["FG"]][["ui"]][["button_fig_del"]]

    # Updating any messages
    state = FM_set_ui_msg(state, msgs)
  }
  # Save figure
  if(has_changed(ui_val   = state[["FG"]][["ui"]][["button_fig_save"]],
                 old_val  = state[["FG"]][["button_counters"]][["button_fig_save"]])){

    FM_le(state, "saving changes to current figure")
    msgs = c()

    # Getting the current figure
    current_fig = FG_fetch_current_fig(state)

    if(state[["FG"]][["ui"]][["text_fig_key"]] != ""){
      # Resetting the key
      current_fig[["key"]] = state[["FG"]][["ui"]][["text_fig_key"]]

      # If the key is equal to the current ID we assign it to the name of the
      # source dataset
      if(current_fig[["key"]] == current_fig[["id"]]){

        fig_dsview = state[["FG"]][["ui"]][["select_current_view"]]
        if(!is.null(state[["FG"]][["DSV"]][["ds"]][[ fig_dsview ]][["label"]])){
          if(state[["FG"]][["DSV"]][["ds"]][[ fig_dsview ]][["label"]] != ""){
            current_fig[["key"]] =  state[["FG"]][["DSV"]][["ds"]][[ fig_dsview ]][["label"]]
          }
        }
      }
    } else {
      # returning an error
      msgs = c(msgs,
          state[["MC"]][["errors"]][["current_key_empty"]])
    }

    # Saving the caption as well
    current_fig[["notes"]] = state[["FG"]][["ui"]][["text_fig_notes"]]

    # updating the view id
    current_fig[["fig_dsview"]] = state[["FG"]][["ui"]][["select_current_view"]]

    # Saving changes to the current figure
    state = FG_set_current_fig(state, current_fig)

    # Saving the button state to the counter
    state[["FG"]][["button_counters"]][["button_fig_save"]] =
      state[["FG"]][["ui"]][["button_fig_save"]]

    # Updating any messages
    state = FM_set_ui_msg(state, msgs)

    # Forcing a rebuild of the figure:
    state = FG_build( state=state, del_row = NULL, cmd = NULL, pll = NULL)
  }
  # Copy figure
  if(has_changed(ui_val   = state[["FG"]][["ui"]][["button_fig_copy"]],
                 old_val  = state[["FG"]][["button_counters"]][["button_fig_copy"]])){

    FM_le(state, "copying figure")
    msgs = c()

    # Getting the original figure that is being copied:
    old_fig = FG_fetch_current_fig(state)

    # This creates a new figure and makes it active:
    state = FG_new_fig(state)

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
            .data[["cmd"]],
            paste0("\\b", old_fig[["fg_object_name"]], "\\b"),
            new_fig[["fg_object_name"]]))
    }


    #From the original figure we copy several fields:
    new_fig[["fig_dsview"    ]]  = old_fig[["fig_dsview"    ]]
  # new_fig[["UD_checksum"   ]]  = old_fig[["UD_checksum"   ]]
  # new_fig[["DW_checksum"   ]]  = old_fig[["DW_checksum"   ]]
  # new_fig[["DSV_checksum"  ]]  = old_fig[["DSV_checksum"  ]]
    new_fig[["elements_table"]]  = old_fig[["elements_table"]]
    new_fig[["elements_list" ]]  = old_fig[["elements_list" ]]
    new_fig[["code"          ]]  = old_fig[["code"          ]]
    new_fig[["fobj"          ]]  = old_fig[["fobj"          ]]
    new_fig[["code_previous" ]]  = old_fig[["code_previous" ]]
    new_fig[["code_fg_only"  ]]  = old_fig[["code_fg_only"  ]]
    new_fig[["notes"         ]]  = old_fig[["notes"         ]]
    new_fig[["fge_cntr"      ]]  = old_fig[["fge_cntr"      ]]

    # Now we dump the new figure with the old components
    # copied into it back into the state object:
    state = FG_set_current_fig(state, new_fig)

    # Setting hold for figure select
    state = set_hold(state, inputId = "select_current_fig")
    state = set_hold(state, inputId = "select_current_view")

    # Saving the button state to the counter
    state[["FG"]][["button_counters"]][["button_fig_copy"]] =
      state[["FG"]][["ui"]][["button_fig_copy"]]

    # Updating any messages
    state = FM_set_ui_msg(state, msgs)
  }

  # Saving the state
  FM_set_mod_state(session, id, state)

  # Returning the state

state}


#'@export
#'@title Initialize FG Module State
#'@description Creates a list of the initialized module state
#'@param session Shiny session variable
#'@param FM_yaml_file App configuration file with FM as main section.
#'@param MOD_yaml_file  Module configuration file with MC as main section.
#'@param id Shiny module ID
#'@param id_UD  ID string for the upload data module used to handle uploads or the name of the list element in react_state where the data set is stored.
#'@param id_DW  ID string for the data wrangling module to process any uploaded data
#'@return list containing an empty app state object
#'@examples
#' # These would be the Shiny input and session variables
#' input   = list()
#' session = list()
#'
#' state = FG_init_state(
#'    FM_yaml_file  = system.file(package = "formods",
#'                                "templates",
#'                                "formods.yaml"),
#'    MOD_yaml_file = system.file(package = "formods",
#'                                "templates",
#'                                "FG.yaml"),
#'    id              = "FG",
#'    id_UD           = "UD",
#'    id_DW           = "DW",
#'    session         = session)
#'
#' state
FG_init_state = function(FM_yaml_file, MOD_yaml_file, id, id_UD, id_DW, session){
  state = list()

  # Reading in default information from the yaml file
  state[["yaml"]] = yaml::read_yaml(FM_yaml_file)

  # This assigns the module config "MC" element to the correct
  MOD_CONFIG = yaml::read_yaml(MOD_yaml_file)
  state[["MC"]] = MOD_CONFIG[["MC"]]

  isgood = TRUE

  # Plot elements defined by aesthetics
  aes_elements = c("line", "point", "errorbar", "hguide", "vguide", "smooth", "ribbon", "boxplot")

  # This will hold the ids of the UI elements that need to be collected
  # when module fetch_state function is called. Some of them will be
  # specified explicitly and others will be generated on the fly from the
  # configuration file.
  ui_ids = c()

  # These are automatically generated elements
  auto_elements = list()

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
        # JMH remove this
        #state[["MC"]][["elements"]][[element]][["ui_aes_select_id"]] = select_id
        #state[["MC"]][["elements"]][[element]][["ui_aes_manual_id"]] = manual_id

        # JMH replace with this
        auto_elements[[element]][["ui_aes_select_id"]] = select_id
        auto_elements[[element]][["ui_aes_manual_id"]] = manual_id
      }
    }


    # these are more manually generated and the lists map ui values to names
    # used in export/import yaml files
    manual_elements = list(
    facet = list(
      mapping = list(
        select_component_facet        = "column",
        select_component_facet_scales = "scales",
        select_component_facet_nrow   = "nrow",
        select_component_facet_ncol   = "ncol")),
    label = list(
      mapping = list(
        text_component_xlab           = "xlab",
        text_component_ylab           = "ylab",
        text_component_ggtitle        = "title")),
    scales = list(
      mapping = list(
        select_component_xscale       = "xscale",
        select_component_yscale       = "yscale",
        text_component_xlim           = "xlim",
        text_component_ylim           = "ylim")))

    # Adding other ui_ids here
    ui_ids = c(ui_ids,
      "button_fig_new",
      "button_fig_save",
      "button_fig_del",
      "button_fig_copy",
      "button_element_add",
      "hot_fg_elements",
      "text_fig_key",
      "text_fig_notes",
      "select_fg_page",
      "select_current_fig",
      "select_current_view",
       names(manual_elements[["facet"]][["mapping"]]),
       names(manual_elements[["scales"]][["mapping"]]),
       names(manual_elements[["label"]][["mapping"]]),
      "select_fg_element")

    # Since some IDs can be reused in the elements above we do this to
    # remove any extras:
    ui_ids = unique(ui_ids)
  }

  # Names of button elements:
  button_counters = c(
    "button_element_add",     # Element: Adding a new element
    "button_fig_save"   ,     # Figure:  Saving the current figure
    "button_fig_new"    ,     # Figure:  New blank figure
    "button_fig_del"    ,     # Figure:  Delete the current figure
    "button_fig_copy"         # Figure:  Copy the current figure
    )

  # Hold-able UI elements
  ui_hold = c(
    "hot_fg_elements"    ,
    "text_fig_key"       ,
    "text_fig_notes"     ,
    "select_current_view",
    "select_current_fig"
    )

  # Populating the formods state elements
  state = FM_init_state(
    FM_yaml_file    = FM_yaml_file,
    MOD_yaml_file   = MOD_yaml_file,
    id              = id,
    dep_mod_ids     = c(id_UD, id_DW),
    MT              = "FG",
    button_counters = button_counters,
    ui_ids          = ui_ids,
    ui_hold         = ui_hold,
    session         = session)

  # JMH remove this
  #state[["MC"]] = state_tmp[["MC"]]


  #---------------------------------------------
  # Finding the dataset
  DSV = FM_fetch_ds(state, session, c(id_UD, id_DW))

  # If the dataset isn't good then we need to
  # flag the whole module as not being good
  if(!DSV[["isgood"]]){
    isgood = FALSE
  }

  # Module-specific elements
  state[["FG"]][["isgood"]]        = isgood
  state[["FG"]][["DSV"]]           = DSV
  state[["FG"]][["figs"]]          = NULL
  state[["FG"]][["fig_cntr"]]      = 0
  state[["FG"]][["current_fig"]]   = NULL
  state[["FG"]][["aes_elements"]]  = aes_elements
  # JMH replace with tis
  # This adds the automatically created elements:
  state[["FG"]][["auto_elements"]]   = auto_elements
  state[["FG"]][["manual_elements"]] = manual_elements

  FM_le(state, "State initialized")

  if(isgood){
    # Initializing an empty figure
    state = FG_new_fig(state)
  }

state}


#'@export
#'@title Initialize New Figure
#'@description Creates a new figure in a FG module
#'@param state FG state from \code{FG_fetch_state()}
#'@return FG state object containing a new empty figure  and that figure set as the
#'current active figure
#'@example inst/test_apps/FG_funcs.R
FG_new_fig    = function(state){

  # Incrementing the figure counter
  state[["FG"]][["fig_cntr"]] = state[["FG"]][["fig_cntr"]] + 1

  # Creating a default figure ID
  fig_id = paste0("Fig_", state[["FG"]][["fig_cntr"]])

  # Pulling out the dataset views
  DSV = state[["FG"]][["DSV"]]

  # Using the default dsview for the new figure
  fig_dsview = names(DSV[["ds"]])[1]

  fg_object_name = paste0("FG_", state[["MC"]][["fg_object_name"]], "_", state[["FG"]][["fig_cntr"]])

  # This is the object that contains the different components of
  # the figure list:
  fig_def =
    list(key            = fig_id,
         id             = fig_id,
         idx            = state[["FG"]][["fig_cntr"]],
         page           = 1,
         num_pages      = 1,
         fg_object_name = fg_object_name,
         fobj           = NULL,
         msgs           = c(),
         fig_dsview     = fig_dsview,
         checksum       = digest::digest(NULL, algo=c("md5")),
         code_fg_only   = NULL,
         code_previous  = NULL,
         code           = NULL,
         notes          = "",
         isgood         = TRUE,
         add_isgood     = TRUE,
         fge_cntr       = 1,
         elements_table = NULL,
         elements_list  = list())

  # Setting the new figure id as the current figure
  state[["FG"]][["current_fig"]]    = fig_id

  # Storing the empty figure object in the state
  state = FG_set_current_fig(state, fig_def)

  # Creating the new figure pages
  state = FG_build(state=state, del_row = NULL, cmd = NULL, pll = NULL)

state}

#'@export
#'@title Fetches Current Figure
#'@description Takes a FG state and returns the current active figure
#'@param state FG state from \code{FG_fetch_state()}
#'@return List containing the details of the active figure. The structure
#'of this list is the same as the structure of \code{state$FG$figs} in the output of
#'\code{FG_fetch_state()}.
#'@example inst/test_apps/FG_funcs.R
FG_fetch_current_fig    = function(state){

  # Current figure ID
  fig_id = state[["FG"]][["current_fig"]]

  # Current figure
  fig = state[["FG"]][["figs"]][[fig_id]]

fig}


#'@export
#'@title Sets Current Figure
#'@description Takes a FG state and a figure list and sets that figure list
#'as the value for the active figure
#'@param state FG state from \code{FG_fetch_state()}
#'@param fig Figure list from \code{FG_fetch_current_fig}
#'@return State with the current figure updated
#'@example inst/test_apps/FG_funcs.R
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
#'  \item{pll:}    Preload list (pll) containing components to save with
#'  mk_preload.
#'  \item{desc:}   Verbose description of the element
#'  \item{msgs:}   Messages to be passed back to the user
#'}
#'@examples
#'\donttest{
#' sess_res = FG_test_mksession()
#' state = sess_res$state
#' fb_res = fers_builder(state)
#'}
fers_builder = function(state){

  isgood   = TRUE
  msgs     = c()
  cmd      = ""
  desc     = ""
  descs    = c()
  pll      = list()
  element  = ""

  element        = state[["FG"]][["ui"]][["select_fg_element"]]
  ui             = state[["FG"]][["ui"]]
  aes_elements   = state[["FG"]][["aes_elements"]]

  # Pulling out the current figure to get the object name
  current_fig = FG_fetch_current_fig(state)
  fg_object_name = current_fig[["fg_object_name"]]

  # Pulling out the element components
  element_cfg                       = state[["MC"]][["elements"]][[element]]
  # adding the select and manual id elements
  element_cfg[["ui_aes_select_id"]] = state[["FG"]][["auto_elements"]][[element]][["ui_aes_select_id"]]
  element_cfg[["ui_aes_manual_id"]] = state[["FG"]][["auto_elements"]][[element]][["ui_aes_manual_id"]]


  if(element%in% aes_elements){
    # Defining the preload type
    pll[["type"]] = element

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
      # this means it's either an aesthetic or manual specification or
      # not "", meaning nothing has been specified for it.
      if((ui[[sel_idx]] != "not_used") & (ui[[sel_idx]] != "")){
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

            # The preload list just needs the raw input passed to it
            man_rhs_pll = ui[[man_idx]] 
            man_comp = c(man_comp, paste0(comp_idx, "=", man_rhs))

            # updating description
            descs = c(descs, paste0(comp_idx,":", man_rhs))

            # Adding manual component to the preload list:
            pll[["manual"]][[comp_idx]] = man_rhs_pll 
          }
        } else {
          aes_comp = c(aes_comp, paste0(comp_idx, "=", ui[[sel_idx]]))

          # updating description
          descs = c(descs, paste0(comp_idx,":", ui[[sel_idx]]))

          # Adding manual component to the preload list:
          pll[["aes"]][[comp_idx]] = ui[[sel_idx]]
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
    # Defining the preload type
    pll[["type"]] = element

    # We want to make sure at least one column has been selected
    if(ui[["select_component_facet"]][1] == ""){
      isgood = FALSE
      msgs = c(msgs, state[["MC"]][["labels"]][["msg_bad_facet"]])

    } else {

      # Getting the facet scales
      if(!(ui[["select_component_facet_scales"]] %in% names(state[["MC"]][["formatting"]][["components"]][["facet_scales"]][["choices"]]))){
        ui[["select_component_facet_scales"]] = state[["MC"]][["formatting"]][["components"]][["facet_scales"]][["default"]]
        msgs = c(msgs, "Unable to determine facet scales from ui using default")
      }


      # Number of rows
      if(!(ui[["select_component_facet_nrow"]] %in% state[["MC"]][["formatting"]][["components"]][["facet_dims"]][["nrow"]][["choices"]])){
        ui[["select_component_facet_nrow"]] = state[["MC"]][["formatting"]][["components"]][["facet_dims"]][["nrow"]][["default"]]
      }

      # Number of columns
      if(!(ui[["select_component_facet_ncol"]] %in% state[["MC"]][["formatting"]][["components"]][["facet_dims"]][["ncol"]][["choices"]])){
        ui[["select_component_facet_ncol"]] = state[["MC"]][["formatting"]][["components"]][["facet_dims"]][["ncol"]][["default"]]
      }

      # The faceting command will depend on the number of columns selected
      if(length(ui[["select_component_facet"]]) == 2){

        cmd = paste0(
                     fg_object_name , " = ", fg_object_name, " + ",
                     "ggforce::facet_grid_paginate(",
                     ui[["select_component_facet"]][1],
                     "~",
                     ui[["select_component_facet"]][2],
                     ", scales = ", deparse( ui[["select_component_facet_scales"]] ),
                     ", nrow = ", ui[["select_component_facet_nrow"]],
                     ", ncol = ", ui[["select_component_facet_ncol"]],
                     ", page=1",
                     ")")



      } else{
        cmd = paste0(
                     fg_object_name , " = ", fg_object_name, " + ",
                     "ggforce::facet_wrap_paginate(vars(",
                     paste0(ui[["select_component_facet"]], collapse=", ")
                     , ")",
                     ", scales = ", deparse( ui[["select_component_facet_scales"]] ),
                     ", nrow = ", ui[["select_component_facet_nrow"]],
                     ", ncol = ", ui[["select_component_facet_ncol"]],
                     ", page=1",
                     ")")

      }

      # Defining the preload options for faceting
      for(uiname in names(state[["FG"]][["manual_elements"]][[element]][["mapping"]])){
        pll[["options"]][[
           state[["FG"]][["manual_elements"]][[element]][["mapping"]][[ uiname ]]
         ]] = ui[[uiname]]
      }

      desc = paste0(ui[["select_component_facet"]], collapse= ", ")

    }
  } else if(element == "scales"){
    # Defining the preload type
    pll[["type"]] = element

    cmds      = c() # All the scale commands
    descs     = c() # All the descriptions
    # Walking through x and y here:
    snames = c("x", "y")
    for(sname in snames){

      # Processing the axis limits
      tmp_lim = ui[[paste0("text_component_", sname, "lim")]]
      tmp_lim_str = "NULL"
      if(tmp_lim !=""){
        # First we try to evaluate the limits
        tcres =
          FM_tc(tc_env = NULL,
                cmd = paste0("limval = c(", tmp_lim, ")"),
                capture = c("limval"))

        # This just makes sure it evaluted correctly
        if(tcres[["isgood"]]){
          if(is.numeric(tcres[["capture"]][["limval"]])){
             if(length(tcres[["capture"]][["limval"]]) == 2){
               descs = c(descs, paste0(sname, "lim:", tmp_lim))
               tmp_lim_str = paste0("c(", tmp_lim, ")")
      #        cmds = c(cmds,  paste0( fg_object_name , " = ", fg_object_name,
      #                        " + ",sname,"lim(", tmp_lim, ")"))
             } else {
               isgood = FALSE
               msgs = c(msgs, paste0("Length of ",sname,"-axis limits should be 2, instead ",length(tcres[["capture"]][["limval"]]), " were found."))
             }
          } else {
            isgood = FALSE
            msgs = c(msgs, paste0("Supplied ",sname,"-axis limits were not numeric."))
          }
        } else {
          isgood = FALSE
          msgs = c(msgs, paste0("Unable to process ",sname,"-axis limits."))
          msgs = c(msgs, tcres[["msgs"]])
        }
      }

      # Processing axis scale:
      if(ui[[paste0("select_component_", sname, "scale")]] =="linear"){
        descs = c(descs, paste0(sname, "scale:linear"))
      }else if(ui[[paste0("select_component_", sname, "scale")]] =="log10"){
        descs = c(descs, paste0(sname, "-scale:log10"))
        cmds = c(cmds,  paste0( fg_object_name , " = ", fg_object_name,
                               " + scale_",sname,"_log10(limits=", tmp_lim_str, ")"))
      }

    }

    # Defining the preload options for faceting
    for(uiname in names(state[["FG"]][["manual_elements"]][[element]][["mapping"]])){
      pll[["options"]][[
         state[["FG"]][["manual_elements"]][[element]][["mapping"]][[ uiname ]]
       ]] = ui[[uiname]]
    }

    cmd  = paste0(cmds,  collapse= " \n")
    desc = paste0(descs, collapse= ", ")
  } else if(element == "label"){
    # Defining the preload type
    pll[["type"]] = element

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

    # Defining the preload options for faceting
    for(uiname in names(state[["FG"]][["manual_elements"]][[element]][["mapping"]])){
      pll[["options"]][[
         state[["FG"]][["manual_elements"]][[element]][["mapping"]][[ uiname ]]
       ]] = ui[[uiname]]
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
             pll      = pll,
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
#'@param pll Preload list for the plotting command.  Set to NULL to initialize a
#'new figure or force a rebuild after a dataset update.
#'@param element Short name for the figure element being performed, eg. point
#'@param desc Verbose description for the action being performed
#'@return State with the build result stored in the current figure. Pull out
#'the figure to inspect it:
#'
#' current_fig = FG_fetch_current_fig(state)
#'
#' Look at this logical object to test if the build worked.
#'
#' current_fig[["add_isgood"]]
#'@example inst/test_apps/FG_funcs.R
FG_build = function(state,
                   del_row     = NULL,
                   cmd         = NULL,
                   pll         = NULL,
                   element     = "unknown",
                   desc        = "unknown"){

  # Pulling out the current figure
  current_fig = FG_fetch_current_fig(state)

  if(!is.null(current_fig)){
    msgs        = c()

    # Defining the dataset locally:
    # JMH check assignments below:
    ds_object_name = current_fig[["fig_dsview"]]
    assign(ds_object_name,
           state[["FG"]][["DSV"]][["ds"]][[ds_object_name]][["DS"]])

    # Pulling out the figure object name:
    fg_object_name = current_fig[["fg_object_name"]]

    # Initializing the figure object
    assign(fg_object_name, NULL)

    # These will be used to flag any failures below:
    isgood     = TRUE
    add_isgood = TRUE

    # The figure code is initialized with the code init:
    code_init = paste0(fg_object_name, " = ggplot2::ggplot(data=", ds_object_name,")")

    # The figure code lines start with this:
    code_lines = code_init

    # This is the elements table
    curr_ET = current_fig[["elements_table"]]

    # Here we process row deletions
    if(!is.null(curr_ET) & !is.null(del_row)){
      # Removing the preload component
      current_fig[["elements_list"]][[ curr_ET[del_row, ][["Key"]] ]] = NULL

      # Removing the specified row from the elements table:
      curr_ET = curr_ET[-c(del_row), ]

      # If there was only one row and we deleted it we need
      # to set the elements table to NULL
      if(nrow(curr_ET) ==0){
        curr_ET = NULL
      }
    }


    # Certain elements can only be used once in a figure. When these already
    # exist in a figure and are added again by the user we replace the last
    # instance of the element with the new one. Here the dupe_replace vector
    # lists the element types to force that replacement. All other element types
    # will just be layered on top of the current figure.
    dupe_replace = c("facet", "label", "scales")
    dupe_found   = FALSE

    if(isgood){

      # First we create the code to initialize the
      # figure
      eval(parse(text=code_init))

      # Process current elements
      if(!is.null(curr_ET)){
        for(row_idx in 1:nrow(curr_ET)){

          # This is triggered when the element being added is already present
          # and it is a "duplicate"
          if((element %in% dupe_replace) &
             (element == curr_ET[row_idx, ][["Element"]])){

            # We flag that we found a duplicate:
            dupe_found = TRUE

            # Updating the preload component with the new one:
            current_fig[["elements_list"]][[ curr_ET[row_idx, ][["Key"]] ]][["pll"]] = pll

            # Then we replace the cmd and Description elements of the old row
            # with the new one:
            curr_ET[row_idx, ][["cmd"]]         = cmd
            curr_ET[row_idx, ][["Description"]] = desc

            msgs = c(msgs,
              stringr::str_replace_all(
                state[["MC"]][["errors"]][["only_one_element"]],
                "===ELEMENT===",
                 element))
          }

          # Now we either add the previous element or the new one if a
          # dupliacate was found:
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
          # We only run the new element if a duplicate wasn't found. If it was
          # found it should have been run in line with the elements above.
          if(!dupe_found){
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
              fge_key = paste0("FGE ", current_fig[["fge_cntr"]])
              # Then we add the new row to the event table
              curr_ET = rbind(curr_ET,
                    data.frame(Key         = fge_key,
                               Element     = element,
                               cmd         = cmd,
                               Description = desc,
                               Status      = "Success",
                               Delete      = FALSE))

              # Adding to the list
              current_fig[["elements_list"]][[fge_key]][["pll"]] = pll

              # Incrementing the counter
              current_fig[["fge_cntr"]] = current_fig[["fge_cntr"]] + 1

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
    code_previous   = state[["FG"]][["DSV"]][["ds"]][[current_fig[["fig_dsview"]]]][["code"]]

    # Just the code to build the figure
    code_fg_only    = paste(code_lines, collapse="\n")
    # All the code required to generate this module
    code            = paste(c(code_previous,
                              "",
                              "# Figure Generation",
                              code_fg_only), collapse="\n")

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
    current_fig[["checksum"]]         = digest::digest(get(fg_object_name), algo=c("md5"))

    # updating the current figure with the changes above
    state = FG_set_current_fig(state, current_fig)

    # updating the module checksum
    state = FG_update_checksum(state)
  }

state}

#'@export
#'@title Updates FG Module Checksum
#'@description Called after any changes to figures, this function will update
#'the checksum of the module. This allows other modules to determine if there
#'were any changes to the figures within it.
#'@param state FG state from \code{FG_fetch_state()}
#'@return state with checksum updated.
#'@examples
#'# This will create a populated FG state object:
#'\donttest{
#' sess_res = FG_test_mksession()
#' state   = sess_res$state
#' state = FG_update_checksum(state)
#'}
FG_update_checksum = function(state){

  fig_checksums = c()

  for(fig_id in names(state[["FG"]][["figs"]])){
    fig_checksums = c(fig_checksums,
          state[["FG"]][["figs"]][[fig_id]][["checksum"]])
  }

  # This concatinates all the individual checksums together into a string.
  # This will be used to create a module checksum below:
  all_checksum_string = paste(fig_checksums, collapse=":")

  # updating the checksum
  state[["FG"]][["checksum"]] = digest::digest(all_checksum_string, algo=c("md5"))

  FM_le(state, paste0("module checksum updated:", state[["FG"]][["checksum"]]))

state}


#'@export
#'@title Extracts Specific Page from Paginated Figure
#'@description Used to extract the specified page from the current figure.
#'@param state FG state from \code{FG_fetch_state()}
#'@param page  Page number to extract
#'@return ggplot object with the specified page.
#'@example inst/test_apps/FG_funcs.R
FG_extract_page  = function(state, page){

  # Current figure
  current_fig = FG_fetch_current_fig(state)

  # Current ggplot object
  fobj = current_fig[["fobj"]]

  # Creating the named figure object locally
  assign(current_fig[["fg_object_name"]], fobj)

  facet_row = current_fig[["elements_table"]] |>
    dplyr::filter(.data[["Element"]] =="facet")

  if(nrow(facet_row > 0)){
    # original faceting command
    facet_cmd = facet_row[1,][["cmd"]]

    # replacing the page number
    facet_cmd = stringr::str_replace(facet_cmd, "page=1", paste0("page=", page))

    # Pulling out the faceting command
    eval(parse(text=facet_cmd))

    # replacing the object
    fobj = get(current_fig[["fg_object_name"]])

  } else {
    FM_le(state, "unable to extract figure page, no facet elements found")
    FM_le(state, paste0(  "figure id: ", current_fig[["id"]]))
  }

fobj}

#'@export
#'@title Fetch Module Code
#'@description Fetches the code to generate results seen in the app
#'@param state UD state from \code{FG_fetch_state()}
#'@return Character object vector with the lines of code
#'@examples
#'\donttest{
#'# This will create a populated FG state object:
#'sess_res = FG_test_mksession()
#'state   = sess_res$state
#'code  = FG_fetch_code(state)
#'cat(paste(code, collapse="\n"))
#'}
FG_fetch_code = function(state){
  if(state[["FG"]][["isgood"]]){
    figs_code = c()
    for(fid in names(state[["FG"]][["figs"]])){
      figs_code = c(figs_code,
                    FM_build_comment(2, state[["FG"]][["figs"]][[fid]][["key"]]),
                    state[["FG"]][["figs"]][[fid]][["code_fg_only"]])
    }
    code = paste(figs_code, collapse="\n")
  } else {
    code = NULL
  }
code}

#'@export
#'@title Append Report Elements
#'@description Description
#'@param state FG state from \code{FG_fetch_state()}
#'@param rpt Report with the current content of the report which will be appended to in
#'this function. For details on the structure see the documentation for \code{\link{FM_generate_report}}.
#'@param rpttype Type of report to generate (supported "xlsx", "pptx", "docx").
#'@param gen_code_only Boolean value indicating that only code should be
#'generated (\code{FALSE}).
#'@return list containing the following elements
#'\itemize{
#'  \item{isgood:}    Return status of the function.
#'  \item{hasrptele:} Boolean indicator if the module has any reportable elements.
#'  \item{code:}      Data wrangling R command.
#'  \item{msgs:}      Messages to be passed back to the user.
#'  \item{rpt:}       Report with any additions passed back to the user.
#'}
#'@seealso \code{\link{FM_generate_report}}
#'@examples
#'\donttest{
#' sess_res = FG_test_mksession()
#' state = sess_res$state
#' # This will read in the default PowerPoint report template
#' rpt =
#' onbrand::read_template(
#'  template = system.file(package="onbrand","templates","report.pptx"),
#'  mapping  = system.file(package="onbrand","templates","report.yaml"))
#'
#' rpt_res =
#' FG_append_report(state   = state,    rpt = rpt,
#'                  rpttype = "pptx", gen_code_only=TRUE)
#'
#' # Shows if report elements are present
#' rpt_res$hasrptele
#'
#' # Code chunk to generate report element
#' cat(paste(rpt_res$code, collapse="\n"))
#'}
FG_append_report = function(state, rpt, rpttype, gen_code_only=FALSE){

  isgood    = TRUE
  hasrptele = FALSE
  code      = c()
  msgs      = c()


  # The FG module only supports the following report types:
  supported_rpttypes = c("pptx", "docx")

  if(rpttype %in% supported_rpttypes){
    for(fig_id in names(state[["FG"]][["figs"]])){

      #Making sure we've flipped the has reportable elements bit
      hasrptele      = TRUE

      # Pulling out the figure object
      current_fig    = state[["FG"]][["figs"]][[fig_id]]
      key            = current_fig[["key"]]
      notes          = current_fig[["notes"]]
      fg_object_name = current_fig[["fg_object_name"]]
      fobj           = current_fig[["fobj"]]
      # creating the fg_object_name locally
      if(!is.null(fg_object_name)){
        assign(fg_object_name, fobj)
        if(rpttype %in% c("pptx", "docx")){
          # this is a normal ggplot object/single figure.
          if(is.null(ggforce::n_pages(fobj))){
            if(rpttype == "pptx"){
              # creating the code for the slide:
              code_chunk = c(
              paste0('# Figure ', fig_id, ": ", key, '                                   '),
                     'rpt  = onbrand::report_add_slide(rpt,                              ',
                     '          template = "content_list",                               ',
                     '          elements = list(                                         ',
              paste0('            title        = list( content = "', key,'",             '),
                     '                                 type    = "text"),                ',
              paste0('            content_body = list( content = ', fg_object_name, ',   '),
                     '                                 type    = "ggplot")))               '
              )
            }
            if(rpttype == "docx"){
              notes_str = NULL
              if(notes != ""){
                notes_str = paste0('          notes    = "',notes,'",')
              }
              code_chunk = c(
              paste0('# Inserting figure: ', key),
                     'rpt = onbrand::report_add_doc_content(rpt,',
                     '        type     = "ggplot",',
                     '        content  = list(',
              paste0('          image    =  ', fg_object_name, ','),
              paste0('          key      = "', fig_id,  '",'),
                                notes_str,
              paste0('          caption  = "',key,'"))'),
                     '# adding a page break',
                     'rpt = onbrand::report_add_doc_content(rpt,',
                     '        type    = "break",',
                     '        content = NULL)',
                     ' '
              )
            }

            if(!is.null(code_chunk)){
              # Evaluating the code created above:
              if(!gen_code_only){
                eval(parse(text=paste(code_chunk, collapse="\n"))) }

              # Saving the code for the slide
              code = c(code, code_chunk)
            }
          } else {

            # Add each paginated object
            # First we get the facet row from the elements_table:
            facet_row = current_fig[["elements_table"]] |>
              dplyr::filter(.data[["Element"]] =="facet")

            for(page in 1:ggforce::n_pages(fobj)){

              # original faceting command
              facet_cmd = facet_row[1,][["cmd"]]

              # Stripping off the assignment portion so the command will just
              # return a ggplot object
              facet_cmd = stringr::str_replace(facet_cmd, paste0(fg_object_name, "\\s*="), "")

              #Wrapping the command in parentheses
              facet_cmd = paste0("(", facet_cmd, ")")

              # Assigning the current page
              facet_cmd = stringr::str_replace(facet_cmd, "page=1", paste0("page=", page))

             ## A slide for each picture
              if(rpttype == "pptx"){
                # creating the code for the slide:
                code_chunk = c(
                paste0('# Figure ', fig_id, '(',page,'): ', key, '                         '),
                       'rpt  = onbrand::report_add_slide(rpt,                              ',
                       '          template = "content_list",                               ',
                       '          elements = list(                                         ',
                paste0('            title        = list( content = "', key,'",             '),
                       '                                 type    = "text"),                ',
                paste0('            content_body = list( content = ', facet_cmd, ',        '),
                       '                                 type    = "ggplot")))               '
                     )
              }
              if(rpttype == "docx"){
                code_chunk = c()
                notes_str = NULL
                if(notes != ""){
                  notes_str = paste0('          notes    = "',notes,'",')
                }
                code_chunk = c(
                paste0('# Inserting figure ', fig_id, '(',page,'): ', key),
                       'rpt = onbrand::report_add_doc_content(rpt,',
                       '        type     = "ggplot",',
                       '        content  = list(',
                paste0('          image    =  ', facet_cmd, ','),
                paste0('          key      = "', fig_id,  '",'),
                                  notes_str,
                paste0('          caption  = "',key,'"))'),
                       '# adding a page break',
                       'rpt = onbrand::report_add_doc_content(rpt,',
                       '        type    = "break",',
                       '        content = NULL)',
                       ' '
                )
              }

              if(!is.null(code_chunk)){
                if(!gen_code_only){
                  # Evaluating the code created above:
                  eval(parse(text=paste(code_chunk, collapse="\n")))}

                # Saving the code for the slide
                code = c(code, code_chunk)
              }
            }
          }
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
#'@title Populate Session Data for Module Testing
#'@description Populates the supplied session variable for testing.
#'@param session Shiny session variable (in app) or a list (outside of app)
#' session = shiny::MockShinySession$new().
#'@param full Boolean indicating if the full test session should be created.
#'@return The FG portion of the `all_sess_res` returned from \code{\link{FM_app_preload}}
#'@examples
#' session = shiny::MockShinySession$new()
#' sess_res = FG_test_mksession(session=session)
#'@seealso \code{\link{FM_app_preload}}
FG_test_mksession = function(session=list(), full=FALSE){
  if(full){
    sources = c(system.file(package="formods", "preload", "ASM_preload.yaml"),
                system.file(package="formods", "preload", "UD_preload.yaml"),
                system.file(package="formods", "preload", "DW_preload.yaml"),
                system.file(package="formods", "preload", "FG_preload.yaml"))
  } else {
    sources = c(system.file(package="formods", "preload", "ASM_preload.yaml"),
                system.file(package="formods", "preload", "UD_preload.yaml"),
                system.file(package="formods", "preload", "DW_preload.yaml"),
                system.file(package="formods", "preload", "FG_preload_minimal.yaml"))
  }
  res = FM_app_preload(session=session, sources=sources)
  res = res[["all_sess_res"]][["FG"]]

res}


#'@export
#'@title Preload Data for FG Module
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
FG_preload  = function(session, src_list, yaml_res, mod_ID=NULL, react_state = list(), quickload=FALSE){
  isgood = TRUE
  input  = list()
  msgs   = c()
  res    = c()


  FM_yaml_file  = render_str(src_list[[mod_ID]][["fm_yaml"]])
  MOD_yaml_file = render_str(src_list[[mod_ID]][["mod_yaml"]])
  id_UD         = yaml_res[[mod_ID]][["mod_cfg"]][["MC"]][["module"]][["depends"]][["id_UD"]]
  id_ASM        = yaml_res[[mod_ID]][["mod_cfg"]][["MC"]][["module"]][["depends"]][["id_ASM"]]
  id_DW         = yaml_res[[mod_ID]][["mod_cfg"]][["MC"]][["module"]][["depends"]][["id_DW"]]

  # Creating an empty state object
  state = FG_fetch_state(id              = mod_ID,
                         input           = input,
                         session         = session,
                         FM_yaml_file    = FM_yaml_file,
                         MOD_yaml_file   = MOD_yaml_file,
                         id_ASM          = id_ASM,
                         id_UD           = id_UD,
                         id_DW           = id_DW,
                         react_state     = react_state)

  elements = src_list[[mod_ID]][["elements"]]

  # Checks to see if we can add elements
  ADD_ELEMENTS = TRUE
  if(is.null(elements)){
    ADD_ELEMENTS = FALSE
  }
  if(is.null(state[["FG"]][["DSV"]][["hasds"]])){
    ADD_ELEMENTS = FALSE
  } else if(!state[["FG"]][["DSV"]][["hasds"]]){
    ADD_ELEMENTS = FALSE
  }

  if(ADD_ELEMENTS){
    # All of the numeric IDs in the preload
    enumeric    = c()

    # Map between list index and internal figure ID
    element_map = list()
    for(ele_idx in 1:length(elements)){
      enumeric = c(enumeric, elements[[ele_idx]][["element"]][["idx"]])
      element_map[[ paste0("Fig_",elements[[ele_idx]][["element"]][["idx"]] )]] = ele_idx
    }

    # Creating empty fig placeholders
    while(state[["FG"]][["fig_cntr"]] < max(enumeric)){
      state = FG_new_fig(state)
    }
    # culling any unneeded views
    for(fig_id  in names(state[["FG"]][["figs"]])){
      # This is a view that doesn't exist in elements so
      # we need to cull it
      if(!(fig_id  %in% names(element_map))){
        # Setting the view to be deleted as the current view
        state[["FG"]][["figs"]][[ fig_id  ]] = NULL
      }
    }

    # Creating some local variables to make it easier below
    # This contains all plot types:
    plot_elements = state[["MC"]][["elements"]]

    # these are the elements defined by aesthetics
    aes_elements  = state[["FG"]][["aes_elements"]]

    # ui elements that are automatically generated
    auto_elements = state[["FG"]][["auto_elements"]]

    # ui elements taht are more manually generated
    manual_elements = state[["FG"]][["manual_elements"]]

    # Available data sources
    DSV = state[["FG"]][["DSV"]]



    # Now we have empty elements defined
    for(element_id in names(element_map)){
      # Making the current element element id active
      state[["FG"]][["current_fig"]]  =  element_id

      # Getting the numeric position in the list corresponding
      # to the current element id
      ele_idx = element_map[[element_id]]

      fig_isgood = TRUE
      # first we set the figure name:
      FM_le(state, paste0("loading figure idx: ", ele_idx ))
      if(!is.null(elements[[ele_idx]][["element"]][["name"]])){
        FM_le(state, paste0("setting name: ", elements[[ele_idx]][["element"]][["name"]]))
        current_ele = FG_fetch_current_fig(state)
        current_ele[["key"]] = elements[[ele_idx]][["element"]][["name"]]
        state = FG_set_current_fig(state, current_ele)
      }
      if(!is.null(elements[[ele_idx]][["element"]][["notes"]])){
        FM_le(state, paste0("notes found and set"))
        current_ele = FG_fetch_current_fig(state)
        current_ele[["notes"]] = elements[[ele_idx]][["element"]][["notes"]]
        state = FG_set_current_fig(state, current_ele)
      }

      # Attaching data source
      if(!is.null(elements[[ele_idx]][["element"]][["data_source"]][["id"]]) &
         !is.null(elements[[ele_idx]][["element"]][["data_source"]][["idx"]])){
        tmp_DSV = DSV[["catalog"]][c(DSV[["catalog"]][["id"]]  == elements[[ele_idx]][["element"]][["data_source"]][["id"]] &
                                     DSV[["catalog"]][["idx"]] == elements[[ele_idx]][["element"]][["data_source"]][["idx"]]), ]
        if(nrow(tmp_DSV) == 1){
          FM_le(state, paste0("setting data source: ", tmp_DSV[["object"]][1]) )
          current_ele = FG_fetch_current_fig(state)
          current_ele[["fig_dsview"]] = tmp_DSV[["object"]][1]
          state = FG_set_current_fig(state, current_ele)
        } else {
          FM_le(state, paste0("error locating data source, expecting 1 source found ", nrow(tmp_DSV)), entry_type="danger")
        }
      } else {
        fig_isgood = FALSE
      }

      if(fig_isgood){
      } else {
        isgood = FALSE
        tmp_msgs = c(paste0("fig id: ", element_id, " unable to find data source."))
        FM_le(state, tmp_msgs, entry_type="danger")
        msgs = c(msgs, tmp_msgs)
      }



      # If everything is good up to this point then we process the components
      if(length(elements[[ele_idx]][["element"]][["components"]] > 0) & fig_isgood){

        # This will pull out all of the ui_ids that realte to components.
        # Basically all of the ui_ids that start with either select_component
        # or text_component
        all_comp_ids = sort(names(state[["FG"]][["ui"]])[
                              grep(x=names(state[["FG"]][["ui"]]), pattern="^select_component|^text_component")
                            ])


        for(comp_idx in 1:length(elements[[ele_idx]][["element"]][["components"]])){
          tmp_component = elements[[ele_idx]][["element"]][["components"]][[comp_idx]][["component"]]
          add_component = TRUE
          comp_err_msg = c()


          if(!is.null(tmp_component [["type"]])){

            # Zeroing out the input list for figure components. This prevents
            # residual data from being reused when looping:
            state[["FG"]][["ui"]][all_comp_ids] = ""

            FM_le(state, paste0("  -> ", tmp_component[["type"]]))
            if(tmp_component[["type"]] %in% names(plot_elements)){

              # defining the component type:
              state[["FG"]][["ui"]][["select_fg_element"]] = tmp_component[["type"]]


              if(tmp_component[["type"]] %in% aes_elements){
                # Processing components defined by aesthetics. These
                # are plot components like geom_line, geom_point, etc

                # All of the aesthetics defined (either ass aes or manual) for
                # the current component
                def_aes_all = c(names(tmp_component[["aes"]]), names(tmp_component[["manual"]]))

                # Checking to see if all the required aes have been defined:
                if(!all(plot_elements[[ tmp_component[["type"]] ]][["aes_req"]] %in% def_aes_all)){
                  add_component = FALSE
                  isgood        = FALSE
                  missing_aes = plot_elements[[ tmp_component[["type"]] ]][["aes_req"]][
                      !c(plot_elements[[ tmp_component[["type"]] ]][["aes_req"]] %in% def_aes_all)
                       ]
                  comp_err_msg = c( comp_err_msg,
                              paste0("  -> missing required aesthetic(s): ", paste0(missing_aes, collapse=", ") ) )
                }


                # Making sure the specified ui elements are allowed:
                if(!all(def_aes_all %in% plot_elements[[ tmp_component[["type"]] ]][["ui_aes"]] )){
                  add_component = FALSE
                  isgood        = FALSE
                  missing_aes = def_aes_all[
                      !c( def_aes_all %in% plot_elements[[ tmp_component[["type"]] ]][["ui_aes"]] )
                       ]
                  comp_err_msg = c( comp_err_msg,
                              paste0("  -> extra aesthetic(s) found: ", paste0(missing_aes, collapse=", ")))
                }

                # Making sure that something has been defined
                if(is.null(tmp_component[["aes"]]) && is.null(tmp_component[["manual"]])){
                  add_component = FALSE
                  isgood        = FALSE
                  comp_err_msg = c( comp_err_msg,
                              paste0("  -> both aes and manual fields are NULL"))
                }

                if(add_component){
                  # Adding aesthetics
                  if(!is.null(tmp_component[["aes"]])){
                    for(cname in names( tmp_component[["aes"]])){
                      cname_ui = paste0("select_component_", cname)
                      state[["FG"]][["ui"]][[cname_ui]] = tmp_component[["aes"]][[cname]]
                    }
                  }
                  # Manual
                  if(!is.null(tmp_component[["manual"]])){
                    for(cname in names( tmp_component[["manual"]])){
                      # setting the select component to manual
                      state[["FG"]][["ui"]][[paste0("select_component_", cname)]] = "manual"

                      # now setting the manual value
                      cname_ui = paste0("text_component_", cname, "_manual")
                      state[["FG"]][["ui"]][[cname_ui]] = tmp_component[["manual"]][[cname]]
                    }
                  }
                }
              } else {
                # These are other ggplot components like scales and labels
                # JMH this next line isn't needed?
                #manual_elements[[  tmp_component[["type"]] ]]
                allowed_options = as.vector(unlist(manual_elements[[  tmp_component[["type"]]  ]][["mapping"]]))
                found_options   = names(tmp_component[["options"]])

                # Making sure the found options are good
                if(!all(found_options %in% allowed_options)){
                  add_component = FALSE
                  isgood        = FALSE
                  extra_options = found_options[!(found_options %in% allowed_options)]
                  comp_err_msg = c( comp_err_msg,
                              paste0("  -> extra options found: ", paste0(extra_options, collapse=", ")))
                }

                if(add_component){
                  for(oname_ui in names( manual_elements[[  tmp_component[["type"]]  ]][["mapping"]] )){

                    oname = manual_elements[[  tmp_component[["type"]]  ]][["mapping"]][[oname_ui]]
                    if(oname %in% names(tmp_component[["options"]])){
                      state[["FG"]][["ui"]][[oname_ui]] = tmp_component[["options"]][[oname]]
                    } else {
                      # This clears out any previous definitions
                      state[["FG"]][["ui"]][[oname_ui]] = ""
                    }
                  }
                }
              }
            } else {
              add_component = FALSE
              isgood = FALSE
              tmp_msg = paste0("unknown plot component type: ", tmp_component[["type"]] )
              msgs = c(msgs, tmp_msg)
              FM_le(state,tmp_msg,entry_type="danger")
            }
          } else {
              add_component = FALSE
              isgood = FALSE
              tmp_msg = paste0("plot type not defined")
              msgs = c(msgs, tmp_msg)
              FM_le(state,tmp_msg,entry_type="danger")
          }

          if(add_component){
            fgb_res  = fers_builder(state)
            if(fgb_res[["isgood"]]){
              state = FG_build( state,
                cmd     = fgb_res[["cmd"]],
                pll     = fgb_res[["pll"]],
                element = fgb_res[["element"]],
                desc    = fgb_res[["desc"]])

              # Checking to make sure the component addition worked:
              current_fig = FG_fetch_current_fig(state)
              if(!current_fig[["add_isgood"]]){
                isgood         = FALSE
                add_component = FALSE
                comp_err_msg = c(comp_err_msg, current_fig[["msgs"]])
              }
              #browser()
            } else {
              isgood         = FALSE
              add_component = FALSE
              comp_err_msg = c(comp_err_msg,  paste0(element_id, ": fers_builder() failed"))
              comp_err_msg = c(comp_err_msg, fgb_res[["msgs"]])
            }
          }
          # Flushing all the error messages
          if(!add_component){
            comp_err_msg = c(
              paste0("figure idx: ", ele_idx, ", component idx: ", comp_idx, ", type: ", tmp_component[["type"]] ),
              comp_err_msg)
            msgs = c(msgs, comp_err_msg)
            FM_le(state,comp_err_msg,entry_type="danger")
          }
        }
      }
    }


    # If we failed somewhere up there then we return a bad status
    if(!fig_isgood){
      isgood = FALSE
    }
  }

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
#'@title Make List of Current FG State
#'@description Converts the current FG state into a preload list.
#'@param state FG state object
#'@return list with the following elements
#' \itemize{
#'   \item{isgood:}       Boolean indicating the exit status of the function.
#'   \item{msgs:}         Messages to be passed back to the user.
#'   \item{yaml_list:}    Lists with preload components.
#'}
#'@examples
#' sess_res = FG_test_mksession()
#' state = sess_res$state
#' res = FG_mk_preload(state)
FG_mk_preload     = function(state){
  isgood    = TRUE
  msgs      = c()
  err_msg   = c()

  ylist     = list(
      fm_yaml  = file.path("config", basename(state[["FM_yaml_file"]])),
      mod_yaml = file.path("config", basename(state[["MOD_yaml_file"]])),
      elements = list()
  )


  DSV = state[["FG"]][["DSV"]]

  if(DSV[["hasds"]]){
    ele_idx = 1
    # Walking through each element:
    for(element_id in names(state[["FG"]][["figs"]])){
      tmp_source_ele = state[["FG"]][["figs"]][[element_id]]

      # Finding the data source:
      dsv_row =
      DSV[["catalog"]][
        DSV[["catalog"]][["object"]] == tmp_source_ele[["fig_dsview"]], ]
      ds_id  = dsv_row[["id"]]
      ds_idx = dsv_row[["idx"]]

      # Creates the empty element:
      tmp_element = list(
        idx   = tmp_source_ele[["idx"]],
        name  = tmp_source_ele[["key"]],
        notes = tmp_source_ele[["notes"]],
        data_source = list(
          id  = ds_id,
          idx = ds_idx),
        components = list())

      FM_le(state, paste0("saving element (", tmp_source_ele[["idx"]], ") ", tmp_source_ele[["key"]]))

      # Adding components:
      if(is.data.frame(tmp_source_ele[["elements_table"]])){
        comp_idx = 1
        for(tmp_key in tmp_source_ele[["elements_table"]][["Key"]]){
          if(tmp_key %in% names(tmp_source_ele[["elements_list"]])){
            if("pll" %in% names(tmp_source_ele[["elements_list"]][[tmp_key]])){
              tmp_element[["components"]][[comp_idx]] = list(component=
                tmp_source_ele[["elements_list"]][[tmp_key]][["pll"]])
              FM_le(state, paste0("  -> ",tmp_key, ": ", tmp_source_ele[["elements_list"]][[tmp_key]][["pll"]][["type"]]) )
            } else {
              tmp_err_msg =paste0("missing preload list (pll) for key: ", tmp_key)
              err_msg = c(err_msg, tmp_err_msg)
              FM_le(state,  tmp_err_msg, entry_type="danger")
              isgood = FALSE
            }
          } else {
            tmp_err_msg = paste0("  -> missing key: ",tmp_key)
            err_msg = c(err_msg, tmp_err_msg)
            FM_le(state,  tmp_err_msg, entry_type="danger")
            isgood = FALSE
          }
          comp_idx = comp_idx + 1
        }
      }

      # Appending element
      ylist[["elements"]][[ele_idx]] = list(element = tmp_element)
      ele_idx = ele_idx + 1
    }
  }

  formods::FM_le(state,paste0("mk_preload isgood: ",isgood))

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

