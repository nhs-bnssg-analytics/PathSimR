#Warmup page W1 sidebar ui content


w1_setup_sidebar <- list(

#0. Title - instructions ####  
h3(strong("Instructions")),

#1. Step 1: enter service point names ####
h4("Step 1: Enter names of all Service Points"),
p(
  "'A' is currently listed as an example Service Point.
               Enter names in the 'Service Point' column by selecting an empty cell or editing an existing one.
                           The entry form will automatically grow when the limit is reached.
                           To refresh, click away and then enter new name.",
  #1.1 info button ####
  actionLink(
    inputId = "serv_point_help",
    label = "What is a Service Point?",
    icon = icon("info-circle")
  ),
  style = "color:gray"
),

#1.2 content for infobutton popup
bsModal(
  id = "modal_serv_point",
  title = HTML("<h2><strong>Service Point Help</strong></h2>"),
  trigger = "serv_point_help",
  size = "large",
  ... =
    HTML(
      '
                         <p> A Service Point is a ward, clinic or any treatment/service that occurs on the pathway. This can range from a GP surgery on a set timetable to a bedded ward providing continuous care.
               The key defining feature of a service point is that it has an associated capacity and service time.</p>
                              '
    )
),
br(),


#2. Step 2: enter exit names ####
h4("Step 2: Enter names of all Exits"),
p(
  "'B' is currently listed as an example Exit.
               Enter names in the 'Exit' column by selecting an empty cell or editing an existing one.
                           The entry form will automatically grow when the limit is reached.
                           To refresh, click away and then enter new name.",
  actionLink(
    inputId = "exit_help",
    label = "What is an Exit?",
    icon = icon("info-circle")
  ),
  style = "color:gray"
),

bsModal(
  id = "modal_exit",
  title = HTML("<h2><strong>Exit Help</strong></h2>"),
  trigger = "exit_help",
  size = "large",
  ... =
    HTML(
      "
                         <p> An exit is any location/service where patients are no longer tracked,
               i.e. they have left the pathway of interest. Example exits could be home, care home, mortality, another pathway that isn't being modelled (e.g. 'Further Treatment', 'Out of patch'').
                              These locations have no associated capacity or LoS and are simply end points along the patient pathway.</p>
                              "
    )
),

br(),

#3. Step 3: Check all entries are in tables ####
h4(
  "Step 3: Check the resulting tables and ensure all entries are included"
),
br(),

#4. Step 4: press the next button ####
h4("Step 4: Proceed by pressing the 'Next' button."),
p(
  "If you require to add/remove any names during the wizard process, you can return to this page and edit the inputs
               to restart the wizard.",
  style = "color:gray"
),

br(),

#4.1 Buttons ####
fluidRow(
  #4.1.1 back button ####
  column(
    6,
    align = "center",
    actionButton(
      inputId = "jb2i2",
      label = "Back to Intro",
      icon = icon("arrow-left")
    )
  ),
  #4.1.2 next button ####
  column(
    6,
    align = "center",
    actionButton(
     inputId = "j2de",
     label = c(tagList("Next", icon("arrow-right")))
  ))
)

)