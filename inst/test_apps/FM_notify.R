if(interactive()){
library(formods)
library(shiny)
library(shinydashboard)
#https://fontawesome.com/icons?from=io

ui <- dashboardPage(
  skin="red",
  dashboardHeader(title="Test Notifications"),
  dashboardSidebar(
     sidebarMenu(
       menuItem("Notifications",    tabName="example",  icon=icon("table"))
     )
  ),
  dashboardBody(
    tabItems(
       tabItem(tabName="example",
        fluidRow(
          shiny::actionButton("set_notification", "Set Notification"),
          shiny::textInput("user_text", label="Notify Text Here", value="Notify me"),
          shiny::actionButton("show_notification", "Show Notification")
         )
       )
     )
   )
 )

# Main app server
server <- function(input, output, session) {

  # Need formods state object
  sess_res = UD_test_mksession(session, id="UD")

  # Captures input and sets the notification
  observeEvent(input$set_notification, {

    state = FM_fetch_mod_state(session, id="UD")
    state = FM_set_notification(state,
                                notify_text = isolate(input$user_text),
                                notify_id   = "example")
    FM_set_mod_state(session, id="UD", state)
   })


  # Displays the notification
  observeEvent(input$show_notification, {
    state = FM_fetch_mod_state(session, id="UD")
    FM_notify(state, session)
   })
}

shinyApp(ui, server)
}
