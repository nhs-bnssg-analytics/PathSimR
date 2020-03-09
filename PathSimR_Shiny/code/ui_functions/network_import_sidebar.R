network_import <- list(
  
  # Input: Select a file -
  h5(strong("Instructions")),
  h5(
    "Step 1: Upload csv templates or bring through Wizard results by selecting the checkbox"
  ),
  h5(
    "Step 2: Press the 'Create visualisation' button to visualise the network."
  ),
  p(
    h5(
      "If there is an error in the template, the issues log will appear and highlight the issue.",
      style = "color:gray"
    )
  ),
  p(
    h5(
      "Optional Step: Toggle the checkboxes to see more information and refresh if appropriate",
      style = "color:gray"
    )
  ),
  h5(
    "Step 3: Once the network is created and correct, proceed to tab 2 (Simulation Setup & Run)"
  ),
  
  
  hr(),
  fluidRow(column(
    12,
    align = "center",
    checkboxInput(
      inputId = "w_temp",
      label = "Bring Through Wizard Results",
      value = 0
    )
  ), style = 'font-size:125%'),
  
  #add box to choose time unit ####
  #not used for any calculation, just for labelling of outputs
  selectInput(
    inputId = "time_unit",
    label = "Choose time unit",
    choices = list(
      "seconds",
      "minutes",
      "hours",
      "days",
      "weeks",
      "months",
      "years"
    )
  ),
  
  conditionalPanel(
    condition = "input.w_temp== '0'",
    fileInput(
      inputId = "file1",
      label = "Upload Network CSV",
      buttonLabel = list(icon("project-diagram"), "Browse..."),
      multiple = FALSE,
      accept = c("text/csv",
                 "text/comma-separated-values,text/plain",
                 ".csv")
    ),
    
    # Input: Select a file --
    fileInput(
      inputId = "file2",
      label = "Upload Calendar CSV",
      buttonLabel = list(icon("calendar-alt"), "Browse..."),
      multiple = FALSE,
      accept = c("text/csv",
                 "text/comma-separated-values,text/plain",
                 ".csv")
    )
  ),
  
  
  
  fluidRow(column(
    12,
    align = "center",
    actionButton(
      inputId = "go_viz",
      label = "Create / Refresh Visualisation",
      icon = icon("project-diagram"),
      style = 'padding:10px; font-size:125%'
    )
  )),
  br(),
  
  fluidRow(column(
    12,
    align = "center",
    actionLink(
      inputId = "viz_help",
      label = HTML("Understanding the Network Visualisation"),
      icon = icon("info-circle"),
      style = 'font-size:125%'
    )
  )),
  br(),
  bsModal(
    id = "modal_viz",
    title = HTML("<h2><strong>Network Visualisation Help</strong></h2>"),
    trigger = "viz_help",
    size = "large",
    ... =
      p(
        "The network visualisation summarises all the input information about the pathway in one place.
                         These are its key features:.",
        style = 'font-size:110%'
      ),
    
    br(),
    h4(strong("Tooltips")),
    p(
      "Hovering over service points will display a detailed capacity calendar. Tooltip behaviour may differ depending on the make of browser (e.g. RStudio internal, Chrome, Edge, Firefox, Internet Explorer) being used to view the Shiny app.",
      style = 'font-size:110%'
    ),
    br(),
    
    h4(strong("Acronyms")),
    p("LOS: Average Length of Service", style = 'font-size:110%'),
    p("Av Cap: Average Capacity", style = 'font-size:110%'),
    p("IQC: Internal Queue Capacity", style = 'font-size:110%'),
    p("EQC: External Queue Capacity", style = 'font-size:110%'),
    br(),
    
    h4(strong("Colours & Shapes")),
    p("Service points: Blue Square", style = 'font-size:110%'),
    p("Exits: Green Diamond", style = 'font-size:110%'),
    p("Arrivals: Red Arrow", style = 'font-size:110%'),
    p("Transfers with (prescribed) delay: Brown Arrow", style = 'font-size:110%'),
    p("Transfers without (prescribed) delay: Black Arrow", style = 'font-size:110%')
    
  ),
  
  hr(),
  
  checkboxInput(
    inputId = "disp1",
    label = "Display network input table",
    value = TRUE
  ),
  
  checkboxInput(
    inputId = "disp2",
    label = "Display calendar input table",
    value = TRUE
  ),
  
  checkboxInput(
    inputId = "disp3",
    label = "Display extra Service Point information (Requires refresh)",
    value = TRUE
  ),
  
  
  
  hr(),
  
  fluidRow(
    column(
      6,
      align = "center",
      actionButton(
        inputId = "jb2i",
        label = "Back to Intro",
        icon = icon("arrow-left")
      )
    ),
    uiOutput("next_button")
  )
  
)