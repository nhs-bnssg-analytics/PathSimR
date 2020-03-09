sim_setup <- list(
  
  # Input: Select warm up & simulation period --
  h4(strong("Instructions")),
  br(),
  actionLink(
    inputId = "sim_mode_help",
    label = HTML("<strong>Which Simulation Mode should I use?</strong>"),
    icon = icon("info-circle"),
    style = 'font-size:110%'
  ),
  br(),
  bsModal(
    id = "modal_sim_mode",
    title = HTML("<h2><strong>Simulation Mode Help</strong></h2>"),
    trigger = "sim_mode_help",
    size = "large",
    ... =
      HTML(
        "
                         <p>PathSimR has two Simulation Modes: Trial Simulation, and Full Simulation.</p>

                         <h4>Trial Simulation </h4>
                         <p>Runs a small number of replications (10) to allow users to (i) estimate a warm up period, (ii) get an idea of roughly how long per replication the simulation will take to run, and (iii) sense check the outputs, all before committing potentially substnatial amounts of computer time and resource to a full simulation run with a large number of replications.</p>
                         
                         <p>The only input required is the the simulation period. Outputs will be restricted to the Warm-up Period Assistance and Average Through Time tabs when in Trial Simulation mode.</p>

                         <h4>Full Simulation </h4>
                         <p> Performs the full simulation, with a large number of replications to achieve (relative) statistical accuracy, and calculates a full suite of output measures and visualisations. The simulation is run for the user-defined simulation period plus the user-defined warm-up period, and outputs then calculated based on the post-warm-up period (i.e. starting from 'normal' levels of occupancy rather than from empty).</p>
                         
                         <p>In full simluation mode, all outputs (excpet the Warm-Up Period Assistance tab) will be viewable, and the a selection of tables, dowloads and a summary document will be available to download. </p>
                         
                         <p>The number of computer CPU cores will be automatically maximised in Full Simulation mode - this may reduce the capacity of the computer to perform other tasks while it is running.</p>

                         "
      )
    
  ),
  
  
  br(),
  h4(strong("Mode 1: Trial Simulation")),
  h5("Step 1: Input Simulation period below"),
  actionLink(
    inputId = "wu_help",
    label = HTML("<strong>What are the warm-up and simulation periods?</strong>"),
    icon = icon("info-circle")
  ),
  
  bsModal(
    id = "modal_wu",
    title = HTML("<h2><strong>Warm-up and Simulation Period Help</strong></h2>"),
    trigger = "wu_help",
    size = "large",
    ... =
      HTML(
        "
                         <h3><strong>Warm-up Period</strong></h3>
                         <p>
                         The warm-up period represents the time it takes for a simulation to reach stable running conditions and after which results can be recorded.
                         As each simulation starts from empty, it is important that the warm-up period be long enough so that the results collected are reflective of
                         the modelled system and not an emptier version.</p>
                         
                         <p>To determine a suitable warm-up period, first run the simulation in Trial Simulation Mode and look
                         at the resulting Warm-up Period Assistance and Average Through Time plots. The first two plots below show a stable system with a recognisable warm-up period.
                         The total in system graph stabilises around the value 30 after approximately 25 time units and all 5 metrics in the average through time graph
                         flatten out/stabilise after
                         roughly the same time period, therefore the full simulation should use a warm-up period of 25.</p>
                         <p>The second set of two plots shows an unstable system, characterised
                         by an ever increasing queue length and total number in system. This system can still be simulated but the results may not be sensible given the instability. If it is unclear
                         whether the system has stabilised, run the simulation again with a longer simulation period and update the warm-up period approximation appropriately.

                         </p>"
      ),
    
    fluidRow(column(
      6, img(
        src = 'wu1.png',
        height = "100%",
        width = "100%"
      )
    ),
    column(
      6, img(
        src = 'wu2.png',
        height = "100%",
        width = "100%"
      )
    )),
    
    fluidRow(column(
      6, img(
        src = 'wu3.png',
        height = "100%",
        width = "100%"
      )
    ),
    column(
      6, img(
        src = 'wu4.png',
        height = "100%",
        width = "100%"
      )
    )),
    
    HTML(
      "<h3><strong>Simulation Period</strong></h3>
                         <p>
                         The Simulation Period is the amount of time over which to collect results from the simulation. This will need to be sufficiently long such that
                         a large number of patients can pass through the pathway. For example, if a pathway has an average length of 365 days then simulating it for only 10
                         would not produce complete results, as unique patients would not have completed the pathway. The simulation period should therefore be longer than
                         the average pathway length and long enough to collect data. The warm-up and simulation periods sum to the total simulation length, over which the
                         external arrivals and capacity calendar will operate. Therefore, the simulation may start collecting data part way through a calendar cycle but this
                         will have no affect on the results.
                         </p> "
    )
    
  ),
  
  
  h5("Step 2: Create checklist and ensure all values are correct"),
  h5(
    "Step 3: Press the 'Run Simulation' button to produce trial results based on a small number of replications"
  ),
  h5(
    "Step 4: Check the Simulation Outputs tab to evaluate the results of the trial simulation to help estimate the warm-up period (using the warm-up period assistance and average through time tabs).
                  Proceed to 'Full Simulation' mode if simulation period is suitable or return to 'Trial Simulation' mode, update the inputs and re-run"
  ),
  br(),
  h4(strong("Mode 2: Full Simulation")),
  h5(
    "Step 1: Input warm-up period below based on the output from the warm-up period assistance tab"
  ),
  h5("Step 2: Input number of simulation replications below"),
  h5("Step 3: Refresh checklist and ensure all values are correct"),
  h5(
    "Step 4: Start the simulation by pressing the 'Run Simulation' button"
  ),
  h5(
    "Step 5: Simulation Outputs and Download Outputs tabs are now available"
  ),
  
  
  # Horizontal line -
  
  hr(),
  
  fluidRow(column(
    12,
    align = "center",
    selectInput(
      inputId = "run_type",
      label = "Select Mode",
      choices = c("Trial Simulation", "Full Simulation"),
      selected = "Trial Simulation",
      selectize = F
    )
  )),
  
  
  conditionalPanel(
    condition = "input.run_type=='Full Simulation'",
    hr(),
    numericInput(
      inputId = "wu",
      label = "Length of warm-up period",
      value = 0,
      min = 0
    )
  ),
  
  
  hr(),
  numericInput(
    inputId = "st",
    label = "Length of simulation period",
    value = "",
    min = 1
  ),
  
  
  conditionalPanel(
    condition = "input.run_type=='Full Simulation'",
    hr(),
    numericInput(
      inputId = "reps",
      label = "Number of simulation replications",
      value = 100,
      min = 1
    )
  ),
  
  
  hr(),
  
  fluidRow(column(
    12,
    align = "center",
    actionButton(
      inputId = "checklist",
      label = "Create / Refresh Checklist",
      icon = icon("clipboard"),
      style = 'padding:10px; font-size:125%'
    )
  )),
  
  
  
  br(),
  fluidRow(column(
    12,
    align = "center",
    actionButton(
      inputId = "sim",
      label = "Run Simulation",
      icon = icon("play"),
      style = 'padding:10px; font-size:125%'
    )
  )),
  br(),
  fluidRow(
    column(
      6,
      align = "center",
      actionButton(
        inputId = "jb2niv",
        label = "Previous",
        icon = icon("arrow-left")
      )
    ),
    uiOutput("next_button2")
  )
  
)