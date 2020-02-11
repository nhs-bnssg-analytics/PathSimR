##### SHINY UI CODE #####
ui <- 
    
  navbarPage(
    #style - read in from a bootstrap v3 (n.b. current version is 4, but doesn't work with Shiny) CSS
  theme="mod.cerulean.css",
  
  title = c(tagList(icon("compass"), "Navigation Bar")),
  id = "navbar",
  
  ####INTRODUCTION TAB ####
  
  tabPanel("Introduction",
           sidebarLayout(
             
             introduction_sidebar,
             
             introduction_main
             
             #front page/p1 main panel
                          
#             mainPanel(
#               br(),
#               fluidRow(
#                 column(4, align = "center", img(src = 'THF_logo.png', style = "height:150px;")),
#                 column(4, align = "center", img(src = 'BNSSG_logo.png', style = "height:150px;")),
#                 column(4, align = "center", img(src = 'UoB_logo.png', style = "height:150px;"))
#               ),
#               br(),
#               br(),
#               br(),
#               br(),
#               br(),
#               br(),
#               br(),
#               br(),
#               br(),
#               br(),
#               
#               fluidRow(column(
#                 12, align = "center", img(src = 'Rplot.png', style = "height:400px;")
#               )),
#               #fluidRow(column(12,align="center",img(src='Logo2.jpg'))),
#               
#               br(),
#               br(),
#               br(),
#               br(),
#               br(),
#               h5(strong(
#                 "Example patient pathway (built in PathSimR)"
#               ), align = "right")
#             )
          )),
  
  ####OVERVIEW AND GLOSSARY TAB ####
  
  tabPanel(
    "Overview & Glossary",
    navlistPanel(
      tabPanel("Overview",
               fluidRow(
                 column(
                   width = 1,
                   offset = 0,
                   style = 'padding:0px;'
                 ),
                 column(
                   7,
                   HTML(
                     "
                      <h1>Overview</h1>
                      <p>PathSimR is a Discrete Event Simulation (DES) tool designed exclusively in R. Aimed at use within the healthcare sector, PathSimR uses relevant terminology and constructs healthcare focussed metrics as outputs. For more information about DES, please consult the technical documentation.</p>

                      <h1>Moving Away From Average-based Modelling</h1>
                      <p> Planning by averages involves a simple equation: Capacity= Arrivals x Length of Service.
                      So with an average of 10 arrivals per day and each patient staying an average of 5 days, the capacity should be 50 beds.
                      Real data shows however that most patients don't actually stay for 5 days exactly and instead show large amounts of variation in their length of stay.
                      Averaging data and therefore removing the variation component needs to be done with extreme caution.
                      Often planning service provision based on data that has been averaged will lead to underestimating the amount
                      of capacity that is required and could have serious impacts on waiting times and capacity driven delays.</p>


                      <h1>Key Features and Assumptions of PathSimR</h1>
                      <p>PathSimR is capable of simulating patient pathways consisting of a number of user defined service points. Each service point can take the form of any service/treatment location that has a prescribed capacity and service length. In the case of a bedded ward in a hospital, this would be the number of beds and treatment length whilst in the case of a GP clinic, this would be number of GPs on shift and appointment length.</p>
                      <p>Each service point along the pathway has a number of user defined parameters including:</p>
                      <ul>
                      <li>Calendar-dependent external arrival rates (assumed to be Poisson distributed)</li>
                      <li>Calendar-dependent capacities</li>
                      <li>Service times that can be modelled as any parameter distribution available in R</li>
                      <li>Internal and external queue capacities (including zero and infinite)</li>
                      </ul>
                      <p>Movement between service points and subsequently movement to an exit is managed through a transition rate matrix which describes the proportion of patient who move between two locations on the pathway.</p>
                      <p>PathSimR deals with both blocking after service (due to lack of downstream capacity in the network) and with user-specified delays/transition times between service points/service points and pathway exits. The former arises when there is no available capacity in an onward service point and no queue to join which forces a patient to reside in their current location, stopping a new patient from starting. The latter, transition delays, are user-defined and are implemented in matrices which describe pairwise delay distributions (and their parameters) between pathway locations.</p>
                      <p>The tool makes a handful of assumptions in order to simplify the modelled systems. Firstly, with respect to queues and service points, PathSimR assumes First Come First Served (FCFS) queue discipline. Secondly, PathSimR assumes that once a patient joins a queue, they will not leave it until they are served at the associated service point (i.e. PathSimR does not allow reneging or baulking).</p>"
                   )
                 )
               )),
      
      tabPanel(
        "Wizard & Setup Terms",
        h3("Wizard & Setup"),
        
        
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = 'padding:0px;'
          ),
          column(2, strong(p("Time Units")), align =
                   "center"),
          column(
            4,
            p(
              " PathSimR does not have a prescribed time unit, instead users can use whichever time unit is appropriate. This must however be consistent throughout the tool and all data
                           entered must match the chosen units (e.g. all data included is on the scale of days: Arrivals per day, Length of Service on scale of days, prescribed delays in fractions of days)."
            )
          )
        ),
        br(),
        
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = 'padding:0px;'
          ),
          column(2, strong(p("Service Point")), align =
                   "center"),
          column(
            4,
            p(
              " Service Point is a ward, clinic or any treatment/service that occurs on the pathway. This can range from a GP surgery on a set timetable to a bedded ward providing continuous care.
               The key defining feature of a service point is that it has an associated capacity and service time."
            )
          )
        ),
        br(),
        
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = 'padding:0px;'
          ),
          column(2, strong(p("Exit")), align = "center"),
          column(
            4,
            p(
              " An exit is any location/service where patients are no longer tracked,
               i.e. they have left the pathway of interest. Example exits could be home, care home, death, another pathway that is not being modelled (e.g. 'Further Treatment', 'Out of patch'').
                              These locations have no associated capacity or LoS and are simply end points along the patient pathway."
            )
          )
        ),
        br(),
        
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = 'padding:0px;'
          ),
          column(2, strong(p("Length of Service")), align =
                   "center"),
          column(
            4,
            p(
              "For the purposes of PathSimR, the Length of Service (or Active Service) corresponds to the amount of time a patient is actually receiving treatment or using a service.
                                          It does not include any time that the patient is blocked or delayed due to capacity restraints nor any prescribed delays.
                                          It represents the time between a patient starting to recieve treatment and the earliest time when they could move on, i.e. the required service time.
                                          This is different to the Length of Stay which is a metric calculated as part of the simulation and includes both active treatment time and any delays which may result due to the dynamics of the system."
            )
          )
        ),
        br(),
        
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = 'padding:0px;'
          ),
          column(2, strong(p(
            "Distributions and Parameters"
          )), align = "center"),
          column(
            4,
            p(
              "A probability distribution is a mathematical function that quantifies the likelihood of different possible outcomes. In PathSimR, these outcomes are lengths of time
                               representing service lengths, inter-arrival times and departure delays. Rather than every patient having a length of service equal to the mean, PathSimR relies on probability distributions to help capture the natural variation in the data.
                               There are a large variety of distributions that can be used in modelling, each of which requires different parameters to shape the probabilities."
            )
          )
        ),
        br(),
        
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = 'padding:0px;'
          ),
          column(2, strong(p("Transition Delay")), align =
                   "center"),
          column(
            4,
            p(
              "Transition  delays are included in PathSimR to help simulate an expected amount of time that a patient will remain in a ward, clinic, or other service point after they have completed their treatment before they can move to their next service point or be discharged (to represent, for example, travel time, the completion of administrative tasks, sourcing of social care funding, discussions with families, or delays in discharge to services which are not being explicitly modelled).
                                          Delays can occur between any pair of service points (that have a zero length queue between them) or between a service point and an exit point.
                                          The delay operates by keeping a patient in their current location while holding a space for them in their onward location (when exiting the pathway, there is always space).
                                          This is summarised in the Transition Delay tab in the outputs."
            )
          )
        ),
        br(),
        
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = 'padding:0px;'
          ),
          column(2, strong(p(
            "Capacity Driven Delay"
          )), align = "center"),
          column(
            4,
            p(
              "Capacity delays occur as a result of blocking after service and are not inputs into the model. They are listed here to help distinguish them from the Transition Delays.
                               They are a result of lack of downstream capacity within the network (i.e. at a service point with defined capacity) which forces a patient to stay in their current location until a space in an onward service point or queue is available.
                                          This is summarised in the Capacity Driven Delay tab in the outputs."
            )
          )
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = 'padding:0px;'
          ),
          column(2, strong(p("Warm-up period")), align =
                   "center"),
          column(
            4,
            p(
              "The warm-up period represents the time it takes for a simulation to reach stable running conditions and after which results can be recorded.
                                          As each simulation starts from empty, it is important that the warm-up period be long enough so that the results collected are reflective of
                                          the modelled system and not an emptier version. "
            )
          )
        ),
        br(),
        
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = 'padding:0px;'
          ),
          column(2, strong(p("Simulation period")), align =
                   "center"),
          column(
            4,
            p(
              "The Simulation Period is the amount of time over which to collect results from the simulation.
                                          This will need to be sufficiently long such that a large number of patients can pass through the pathway.
                                          For example, if a pathway has an average length of 365 days then simulating it for only 10 would not produce complete results,
                                          as unique patients would not have completed the pathway. The simulation period should therefore be longer than the average pathway
                                          length and long enough to collect data. "
            )
          )
        ),
        br(),
        
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = 'padding:0px;'
          ),
          column(2, strong(p("Replications")), align =
                   "center"),
          column(
            4,
            p(
              "Number of times to run a particular simulation so as to capture the natural variation in the system.
                                          Results are averaged over all replications to ensure all system eventualities are captured."
            )
          )
        ),
        br(),
        
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = 'padding:0px;'
          ),
          column(2, strong(p(
            "Network Input Template"
          )), align = "center"),
          column(
            4,
            p(
              "A .csv file that specifies transition rates between service points, permitted internal and external queue sizes, and the probabilty distribution names and parameters for both service point Lengths of Service, and prescribed transition delays between pairs of servcie points/service points and exits."
            )
          )
        ),
        br(),
        
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = 'padding:0px;'
          ),
          column(2, strong(p(
            "Calendar Input Template"
          )), align = "center"),
          column(
            4,
            p(
              "A .csv file that includes the calendar of capacity and mean external arrival rates (by time period) for each service point"
            )
          )
        ),
        br()
        
      ),
      
      tabPanel(
        "Output Terms",
        h3("Output Terms"),
        h4("Patient Based Outputs"),
        
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = 'padding:0px;'
          ),
          column(2, strong(p(
            "Total time in system (TTIS)"
          )), align = "center"),
          column(
            4,
            p(
              "Time between external arrival and departure to an exit for each patient."
            )
          )
        ),
        br(),
        
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = 'padding:0px;'
          ),
          column(2, strong(p("Wait")), align = "center"),
          column(4, p(
            "Time between arrival and service start at a service point (time spent in queue)."
          ))
        ),
        br(),
        
        
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = 'padding:0px;'
          ),
          column(2, strong(p("Active Service")), align =
                   "center"),
          column(
            4,
            p(
              "Time between service start and service end at the service point (e.g. treatment on a ward, a clinic appointment etc.)."
            )
          )
        ),
        br(),
        
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = 'padding:0px;'
          ),
          column(2, strong(p(
            "Time Delayed (Capacity Driven)"
          )), align = "center"),
          column(
            4,
            p(
              "Time between service end and start of transition delay (or departure if no transition delay) at the service point (e.g. treatment on a ward, a clinic appointment etc.) - the amount of time a patient spends blocked at a service point after completing their Active Service, waiting for capacity to become free downstream."
            )
          )
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = 'padding:0px;'
          ),
          column(2, strong(p(
            "Time Delayed (Transition)"
          )), align = "center"),
          column(
            4,
            p(
              "Time between capacity driven delay end (or service end if no capacity delay) and departure from the service point (e.g. treatment on a ward, a clinic appointment etc.) - they user defined delay (not depedent on downstream capacity)."
            )
          )
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = 'padding:0px;'
          ),
          column(2, strong(p(
            "Length of Stay (LOS)"
          )), align = "center"),
          column(
            4,
            p(
              "Time between service start and departure (Active Service + Delay to transfer + Transition delay (user-defined delay))"
            )
          )
        ),
        br(),
        
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = 'padding:0px;'
          ),
          column(2, strong(p(
            "Delay To Transfer (DTT)"
          )), align = "center"),
          column(
            4,
            p(
              "Time between service end and departure, i.e. the amount of time the patient is delayed due to a lack of capacity downstream (blocking after service) plus any transition delay."
            )
          )
        ),
        br(),
        
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = 'padding:0px;'
          ),
          column(2, strong(p("Rejection Rate")), align =
                   "center"),
          column(
            4,
            p(
              "Number of patients rejected from full external queues divided by the length of the simulation run."
            )
          )
        ),
        
        
        h4("Service Point Based Outputs"),
        
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = 'padding:0px;'
          ),
          column(2, strong(p("Queue")), align = "center"),
          column(
            4,
            p(
              "Number of concurrent patients who have arrived at a service point and are yet to start the service."
            )
          )
        ),
        br(),
        
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = 'padding:0px;'
          ),
          column(2, strong(p(
            "Occupancy/Patient Occupancy"
          )), align = "center"),
          column(
            4,
            p(
              "Number of patients who are actually receiving or have received service and are occupying a space in the service point."
            )
          )
        ),
        br(),
        
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = 'padding:0px;'
          ),
          column(2, strong(p("Bed Occupancy")), align =
                   "center"),
          column(
            4,
            p(
              "The total number of beds currently not available to new arrivals - includes Occupancy/Patient Occupancy as above and also any 'empty' beds that are currently reserved for patients under Transition Delay upstream."
            )
          )
        ),
        br(),
        
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = 'padding:0px;'
          ),
          column(2, strong(p(
            "Capacity Driven Delay"
          )), align = "center"),
          column(
            4,
            p(
              "Number of patients concurrently delayed due to insufficient capacity downstream (blocking after service). These patients are included in the occupancy and the bed occupancy"
            )
          )
        ),
        br(),
        
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = 'padding:0px;'
          ),
          column(2, strong(p("Transition Delay")), align =
                   "center"),
          column(
            4,
            p(
              "Number of patients concurrently experiencing a prescribed transfer delay.
                             Patients moving to downstream nodes will also be reserving a space in the onward node and thus appear in the bed occupancy metric for that unit.
                             Patients are included in the occupancy and bed occupancy of the current node"
            )
          )
        ),
        br(),
        
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = 'padding:0px;'
          ),
          column(2, strong(p("% time at level")), align =
                   "center"),
          column(
            4,
            p(
              "The percentage of the total simulation time that a service point was at a particular level of the output measure of interest (calculated across all replications) - e.g. if there was a queue of length 5 at service point A was 150 time units out of a total simulation time of 1,500 time units, the '% time at level' for that unit and that queue would be 10%."
            )
          )
        ),
        br(),
        
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = 'padding:0px;'
          ),
          column(2, strong(p("Percentiles")), align =
                   "center"),
          column(
            4,
            p(
              "The percentage of total simulation time that the metric of interest was below (or above) the given level - e.g. if the 95th percentile occupancy for unit A was 6, then it's occupancy was at or below 6 for 95% of the simulation time (and conversely, there it was greater than six for 5% of the simulation time). The 50th percentile is the median."
            )
          )
        ),
        br(),
        
        h4("Connecting Events with Outputs"),
        fluidRow(column(
          width = 1,
          offset = 0,
          style = 'padding:0px;'
        ),
        column(
          6,
          p(
            "The figure below shows how the different events that occur at each Service Point connect to the different outputs
                        listed on this page. The outputs in the coloured boxes represent the time spent in those states, e.g. the time
                        between Arrival and Service Start is defined as the Wait. Both Delay-To-Transfer and Length of Stay are combined metrics
                        that represent the sum of time spent in multiple states."
          )
        )),
        
        fluidRow(column(
          width = 1,
          offset = 0,
          style = 'padding:0px;'
        ),
        column(
          6,
          p(
            "The Service Point Based Outputs refer to the number of patients concurrently existing in the same state/activity.
                         For example, the number of patients concurrently experiencing Capacity Driven Delay are all those that are between
                         Service End and Transition Start simultaneously."
          )
        )),
        
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = 'padding:0px;'
          ),
          column(6, img(src = 'Event_Outputs.png'))
        )
      ),
      
      widths = c(2, 10),
      well = TRUE
      
    )
  ),
  
  ####WIZARD 1 - SETUP TAB ####
  
  tabPanel("W1. Setup",
           
           sidebarLayout(
             sidebarPanel(
               h3(strong("Instructions")),
               h4("Step 1: Enter names of all Service Points"),
               p(
                 "'A' is currently listed as an example Service Point.
               Enter names in the 'Service Point' column by selecting an empty cell or editing an existing one.
                           The entry form will automatically grow when the limit is reached.
                           To refresh, click away and then enter new name.",
                 actionLink(
                   inputId = "serv_point_help",
                   label = "What is a Service Point?",
                   icon = icon("info-circle")
                 ),
                 style = "color:gray"
               ),
               
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
               h4(
                 "Step 3: Check the resulting tables and ensure all entries are included"
               ),
               br(),
               h4("Step 4: Proceed by pressing the 'Next' button."),
               p(
                 "If you require to add/remove any names during the wizard process, you can return to this page and edit the inputs
               to restart the wizard.",
                 style = "color:gray"
               ),
               
               br(),
               fluidRow(
                 column(
                   6,
                   align = "center",
                   actionButton(
                     inputId = "jb2i2",
                     label = "Back to Intro",
                     icon = icon("arrow-left")
                   )
                 ),
                 column(6, align = "center", actionButton(
                   inputId = "j2de", label = c(tagList("Next", icon("arrow-right")))
                 ))
               ),
               
               width = 3
             ),
             
             mainPanel(
               fluidRow(
                 br(),
                 fluidRow(h2(strong(
                   textOutput("duplicate")
                 )), align = "center"),
                 br(),
                 br(),
                 column(
                   width = 1,
                   offset = 0,
                   style = 'padding:0px;'
                 ),
                 column(
                   4,
                   matrixInput(
                     inputId = "sp",
                     value = m1,
                     class = "character",
                     cols = list(
                       names = TRUE,
                       extend = FALSE,
                       editableNames = FALSE
                     ),
                     rows = list(
                       names = TRUE,
                       extend = TRUE,
                       editableNames = FALSE,
                       delta = 1
                     ),
                     copy = FALSE,
                     paste = TRUE
                   )
                 ),
                 column(
                   width = 2,
                   offset = 0,
                   style = 'padding:0px;'
                 ),
                 column(
                   4,
                   matrixInput(
                     inputId = "exit",
                     value = m2,
                     class = "character",
                     cols = list(
                       names = TRUE,
                       extend = FALSE,
                       editableNames = FALSE
                     ),
                     rows = list(
                       names = TRUE,
                       extend = TRUE,
                       editableNames = FALSE,
                       delta = 1
                     ),
                     copy = FALSE,
                     paste = TRUE
                   )
                 ),
                 column(
                   width = 1,
                   offset = 0,
                   style = 'padding:0px;'
                 )
               ),
               br(),
               
               fluidRow(
                 column(
                   width = 1,
                   offset = 0,
                   style = 'padding:0px;'
                 ),
                 column(4, tableOutput("sp_table"), align = "center"),
                 column(
                   width = 2,
                   offset = 0,
                   style = 'padding:0px;'
                 ),
                 column(4, tableOutput("exit_table"), align = "center"),
                 column(
                   width = 1,
                   offset = 0,
                   style = 'padding:0px;'
                 )
               )
               
             )
           )),
  
  ####WIZARD 2 - DATA ENTRY TAB ####
  
  tabPanel("W2. Data Entry",
           
           sidebarLayout(
             sidebarPanel(
               h3(strong("Instructions")),
               br(),
               h4(strong(
                 em("For each named tab on the right, fill out all the information")
               )),
               br(),
               h4(
                 "Step 1: Enter a Length of Service distribution and parameters for the Service Point"
               ),
               p(
                 "Select the distribution from the drop-down below and then enter the parameter values (as numbers) to the right, in the correct named box.
                 If the distribution and parameters for the service point are not know, use the Service Distribution tool (in the navigation bar above)
                 to either fit models to uploaded data or scale against BNSSG data and then enter resulting distributions and parameters.",
                 actionLink(
                   inputId = "serv_help",
                   label = "What is a Length of Service and how does it connect to distributions and parameters?",
                   icon = icon("info-circle")
                 )
                 ,
                 style = "color:gray"
               ),
               
               bsModal(
                 id = "modal_serv",
                 title = HTML(
                   "<h2><strong>Length of Service & Distributions Help</strong></h2>"
                 ),
                 trigger = "serv_help",
                 size = "large",
                 ... =
                   HTML(
                     '<h3><strong>Length of Service</strong></h3>
                         <p>For the purposes of PathSimR, the Length of Service (or Active Service) corresponds to the amount of time a patient is actually receiving treatment or using a service.
                         It does not include any time that the patient is blocked or delayed due to capacity restraints nor any prescribed delays.
                         It represents the time between a patient starting to recieve treatment and the earliest time when they could move on, i.e. the required service time.
                         This is different to the Length of Stay which is a metric calculated as part of the simulation and includes both active treatment time and any delays which
                         may result due to the dynamics of the system.</p>

                              <h3><strong>LoS Distributions and Parameters</strong></h3>
                              <p> Rather than every patient having a length of service equal to the mean, PathSimR uses probability distributions (range of values with different probabilities) to help capture the natural variation in the data.
                              For example, using only the average LoS would ignore instances where patients have substaintially longer service times, which in turn could impact effective capacity.
                              The probability of having a certain length of service is modelled by the user chosen distributions and parameters. PathSimR includes a Service Distribution Tool, which
                              allows users to either fit models to uploaded data or scale against BNSSG data and is accessed from the Navigation Bar.</p>
                              '
                   )
               ),
               
               
               
               
               
               br(),
               h4("Step 2: Enter information about maximum queue lengths"),
               p(
                 "Enter numberic values into both boxes.",
                 actionLink(
                   inputId = "queue_help",
                   label = "What counts as a queue?",
                   icon = icon("info-circle")
                 ),
                 style = "color:gray"
               ),
               
               
               bsModal(
                 id = "modal_queue",
                 title = HTML("<h2><strong>External & Internal Queue Help</strong></h2>"),
                 trigger = "queue_help",
                 size = "large",
                 ... =
                   fluidRow(column(
                     11,
                     HTML(
                       "
                         <p>A queue in PathSimR is simply a part of the pathway in which the patient can wait for the next service point. These queues can be as small or large as required, even
                         effectively infinite (e.g. when patients simply wait at home for the next available appointment and don't wait in a physical queue). In the case where there is effectively
                         unlimited/infinite queueing capacity, the user can enter a large number (e.g. 99999) to represent a queue that will never fill.
                         In PathSimR, queues are defined by the service point in which they enter, therefore if multiple service points (e.g. A, B & C) have patients that move to service point D,
                         the queue capacity would be shared between patients from A, B & C and the capacity would be defined at service point D.
                         There are two different types of queue:External Queues and Internal Queues, which are both described below. A service point can have both types of queue.</p>

                         <h3><strong>External Queues</strong></h3>
                         <p>An external queue is defined as a queue that accepts arrivals from outside of the pathway and therefore only connect to a single service point. A full external queue
                         causes arriving patients to be lost from the system, a metric which is recorded in the outputs.
                         These are the arrivals that determine the external arrival rate, entered in step 4 on this page.
                         In the pathway visualisation later in the tool, these will appear as red arrows.</p>

                          <h3><strong>Internal Queues</strong></h3>
                          <p>An internal queue is one that connects service points within the pathway network.
                          As outlined above, internal queues are defined by the downstream service point so each service point has only 1 internal queue,
                          not a unique queue between each service point. In the case where an internal queue is full, patients will have to wait in the
                          preceeding service point until a space in the queue becomes available.</p>

                          <h3><strong>Why are there two types of queue?</strong></h3>
                          <p>The two types of queues represent different aspects of the system. Setting the external queue length allows the user to manipulate how external arrivals are treated,
                          e.g. whether they can wait until a service space becomes available or, in the case of a zero length external queue, start service if there is a space in the service point or be lost to the system.
                          Internal queues dictate how patients move & wait between service points, with a zero length internal queue representing the requirement for continuous care and will potentially result in capacity
                          driven delays. </p>
                          "
                     )
                   ))
               ),
               
               
               
               
               
               
               br(),
               h4("Step 3: Enter Transition Proportions and Transition Delays"),
               p(
                 strong(
                   "Enter a value between and including 0 and 1 in the proportion box to represent the proportion of patients who move to that service point.",
                   "All proportion values should sum to 1 on each Service Point."
                 ),
                 "If there is a Transition Delay associated with the move, select the describing distribution and enter the neccessary parameters.
                 A fixed Transition Delay can be modelled using the uniform distribution and entering the same value into the min and max boxes.",
                 actionLink(
                   inputId = "delay_help",
                   label = "What is a Delay and how are they important?",
                   icon = icon("info-circle")
                 ),
                 style = "color:gray"
               ),
               
               
               bsModal(
                 id = "modal_delay",
                 title = HTML("<h2><strong>Departure Delays Help</strong></h2>"),
                 trigger = "delay_help",
                 size = "large",
                 ... =
                   fluidRow(column(
                     11,
                     HTML(
                       "
                         <h3>Transition Delays</h3>
                         <p>
                         Transition Delays are included in PathSimR to help simulate the amount of time needed to move from one unit to the next.
                         Delays can occur between any pair of service points (that have a zero length queue between them) or between a service point and an exit point.
                         The delay functions by keeping a patient in their current location while holding a space for them in their onward location (when exiting the pathway,
                         there is always space). The delays are formulated in the same way as the Length of Services, i.e. using probability distributions
                         to model variation. A fixed Transition Delay can be applied to patient using the uniform distribution and entering the same value
                         into the min and max boxes (e.g. If all patients need to have exactly a 2 day delay when exiting to a care home, select the uniform
                         distribution and enter a 2 into both parameter boxes). The Transition Delay tab in the outputs looks at the number of patients who are concurrently
                         experiencing a trasition delay through time.
                         </p>

                         <h3>Capacity Driven Departure Delays</h3>
                         <p> Even if no Transition Delays are included in the pathway, delays due to capacity can still occur. These delays are due to blocking after service
                         and arise when there is no available capacity in an onward service point and no queue to join. This forces the patient to reside in their current
                         location, stopping new patients from starting. The capacity delay ends when a space becomes available for the patient downstream. The Capacity Driven Delay
                         tab in the outputs looks at the number of patients who are concurrently experiencing a capacity driven delay through time.
                         </p>

                         <h3>What is Delay To Transfer?</h3>
                         <p>There is a output metric called 'Delay To Transfer', which looks at the amount of time between a patient finishing service and departing the service point.
                         This time is the sum of time experiencing any Capacity Driven Delay and any transition delay at a service point. This
                         metric can be found on the statistics output tabs in the Outputs section.
                         </p>

                              "
                     )
                   ))
               ),
               
               br(),
               h4("Step 4: Complete the Calendars"),
               p(
                 "The External Arrival Rate & Capacity are able to change at given times throughout the simulation. These changes occur at times set in the respective calendars.
                 Both calendars require at least 1 row to be filled.",
                 
                 actionLink(
                   inputId = "cal_help",
                   label = "How do I fill the calendar?",
                   icon = icon("info-circle")
                 ),
                 style = "color:gray"
               ),
               
               bsModal(
                 id = "modal_calendar",
                 title = HTML("<h2><strong>Calendar Help</strong></h2>"),
                 trigger = "cal_help",
                 size = "large",
                 ... =
                   HTML(
                     '<h3><strong>External Arrival Rate Calendar</strong></h3>
                              <p><b><em>Only include arrivals from outside the pathway i.e. those that would join the external queue
                              and have not moved from a Service Point on the pathway.
                              If there is no change in the external arrival rate through time, enter 0 in
                              the start column and the arrival rate (0 if there are no external arrivals) in the Arrival Rate column.</em></b> If the arrival
                              rate does change through time, fill out a row for each period in sequence, matching
                              the end times with the subsequent start times. The simulation will loop through the
                              calendar (i.e. will reach the max end time and then start again from the first
                              calendar entry). </p>
                                <p>The arrival calendar below follows the following pattern:</p>
                                <ul>
                                  <li>Between time 0 and 100, the arrival rate is an average of 1 patient per time step.</li>
                                  <li>Between time 100 and 150, there are no patients arriving.</li>
                                  <li>Between time 150 and 200, the arrival rate is an average of 2 patients per time step.</li>
                                  <li>The calendar then returns to the first row and starts again
                                  (i.e. between time 200 and 300, average arrival rate is 1 patient per time step).</li>
                                  </ul>'
                   ),
                 br(),
                 fluidRow(column(
                   12, tableOutput("ext_arr_example"), align = "center"
                 )),
                 HTML(
                   '<p>&nbsp;</p>
                              <h3><strong>Capacity Calendar</strong></h3>
                              <p><b><em>If there is no change in the capacity through time, enter 0 in the start
                              column and the capacity in the Capcity column.</b></em> If the capacity does change
                              through time, fill out a row for each period in sequence, matching the end
                              times with the subsequent start times. The simulation will loop through the
                              calendar (i.e. will reach the max end time and then start again from the first calendar entry).
                              If modelling a clinic or a fixed time Service Point, the capacity can be set to 0 for a period
                              of time to represent a closed service.</p>
                              <p>The capacity calendar below follows the following pattern:</p>
                                <ul>
                                  <li>Between time 0 and 30, the capacity is 24 (beds or service spaces).</li>
                                  <li>Between time 30 and 90, the capacity is 48 (beds or service spaces).</li>
                                  <li>Between time 90 and 180, the capacity is 72 (beds or service spaces).</li>
                                  <li>The calendar then returns to the first row and starts again
                                  (i.e. between time 180 and 210, the capacity is 24).</li>
                                  </ul>
                              <p>&nbsp;</p>'
                 ),
                 fluidRow(column(12, tableOutput("cap_example"), align =
                                   "center"))
               ),
               
               br(),
               h4("Step 5: Repeat Steps 1 to 4 for each Service Point Tab"),
               p(" ", style = "color:gray"),
               
               br(),
               h4(
                 "Step 6: Once all Service Point Tabs are complete, proceed by pressing the 'Next' button"
               ),
               p(
                 " A new tab will also appear at the top of the page. If you require to edit any data entered on any tab during the wizard process, you can return to this page and edit the inputs.
                 If you do, then ensure that the subsequent pages are refreshed.",
                 style = "color:gray"
               ),
               
               br(),
               fluidRow(
                 column(
                   6,
                   align = "center",
                   actionButton(
                     inputId = "j2s",
                     label = "Previous",
                     icon = icon("arrow-left")
                   )
                 ),
                 column(6, align = "center", actionButton(
                   inputId = "j2ftd", label = c(tagList("Next", icon("arrow-right")))
                 ))
               ),
               width = 3
             ),
             mainPanel(uiOutput("tabs"))
           )),
  
  ####WIZARD 3 - FINAL WIZARD TABLES & DOWNLOAD TAB ####
  
  tabPanel(
    "W3. Final Wizard Tables & Download",
    
    sidebarLayout(
      sidebarPanel(
        h3(strong("Instructions")),
        h4(
          "Step 1:  Press the 'Create/Refresh tables' button to see a summary of the data entered & Issues Log."
        ),
        p(
          "There are 4 tables: Issues, Mean Length of Service (only appears when no issues), Network Template and Calendar template",
          style = "color:gray"
        ),
        br(),
        h4(
          "Step 2: If there are any issues, return to the previous page and ammend the data inputs."
        ),
        p(
          "The location of the issue is listed along with a brief description.",
          style = "color:gray"
        ),
        br(),
        h4(
          "Step 3: Once there are no issues remaining, the option to download the templates for further use becomes available"
        ),
        p(
          "The templates created in the wizard can be saved down and then directly used in PathSimR at a later date.
                 Both templates are required for use in this way.",
          style = "color:gray"
        ),
        br(),
        h4("Step 4: Proceed by pressing the 'Move to Simulation Tool' button."),
        p(
          "The inputs created in the wizard can be pulled through on the following page",
          style = "color:gray"
        ),
        
        
        br(),
        fluidRow(column(
          12,
          align = "center",
          actionButton(
            inputId = "go",
            label = "Create / Refresh tables",
            style = 'padding:10px; font-size:150%'
          )
        )),
        br(),
        uiOutput("download_buttons"),
        br(),
        br(),
        fluidRow(column(
          6,
          align = "center",
          actionButton(
            inputId = "jb2de",
            label = "Previous",
            icon = icon("arrow-left")
          )
        ),
        uiOutput("j2st")),
        width = 3
      ),
      mainPanel(
        fluidRow(br(),
                 column(12, tableOutput("issues"), align = "center")),
        
        fluidRow(br(),
                 column(12, tableOutput("means"), align = "center")),
        
        fluidRow(br(),
                 column(12, tableOutput("var_view"), align = "center")),
        
        fluidRow(br(),
                 column(12, tableOutput("cal_view"), align = "center"))
      )
    )
    
  ),
  
  ####SERVICE DISTRIBUTION TOOL TAB ####
  
  
  tabPanel(
    title = "Service Distribution Tool",
    icon = icon("chart-area"),
    sidebarLayout(
      sidebarPanel(
        h3(strong("Instructions")),
        
        
        br(),
        actionLink(
          inputId = "model_help",
          label = HTML("Which option do I need?"),
          icon = icon("info-circle"),
          style = ' font-size:150%'
        ),
        br(),
        bsModal(
          id = "modal_model",
          title = HTML("<h2><strong>Service Distribution Tool Help</strong></h2>"),
          trigger = "model_help",
          size = "large",
          ... =
            HTML(
              "
                         <p>
                         PathSimR's Service Distribution Tool contains 2 options depending on how much information is available about a service point. </p>

                         <p>If LoS data is available for the service point in question, then <strong>Option 1</strong> should be used. The data can be uploaded and
                         model fits run within the tool that provides the best fitting distribution and parameters that can be used. The data in question must be a single
                         column of data with no column header, saved as a csv. The graph below shows an example of uploaded data with 5 different best fitting distributions
                         plotted to show how the tool approximates the real data.</p>

                         <p>If only the mean LoS is known, then <strong>Option 2</strong> can be used, provided the service point type exists in the library (found on the Scale data by mean tab).
                         This portion of the tool scales a model distribution provided by BNSSG CCG to match the mean provided by the user, resulting in a model that has the correct shape and mean
                         for the service point type in question.
                         </p>
                              "
            ),
          plotOutput("model_help_figure")
        ),
        
        h4(strong("Option 1: Model fits to user data")),
        h4(
          em("Distribution & Parameters based on User data", style = "color:gray")
        ),
        h5("Step 1: Select the 'Model fits to user data tab"),
        h5(
          "Step 2: Upload a single column csv that only includes LoS data - ",
          em(" No Header required")
        ),
        h5("Step 3: Press the 'Run Distribution Fit Tool' button"),
        h5(
          "Step 4: Inspect the histgram plot and model fit curves, the details of which are displayd in the Ranked Model Table"
        ),
        h5(
          "Step 5: Copy the top ranking model information from the table into the data entry page (i.e. Select the Distribution from the dropdown and enter the parameters listed)"
        ),
        br(),
        h4(strong("Option 2: Scale data by mean")),
        h4(
          em("Distribution & Parameters based on scaled data", style = "color:gray")
        ),
        h5("Step 1: Select the 'Scale data by mean' tab"),
        h5(
          "Step 2: Select a Service Point from the drop-down library that matches the Service Point being modelled"
        ),
        h5(
          "Step 3: Enter the mean LoS associated with the modelled Service Point"
        ),
        h5("Step 4: Press the 'Run/Refresh Scaling Tool' Button"),
        h5(
          "Step 5: Copy the model information from the table into the data entry page (i.e. Select the Distribution from the dropdown and eneter the parameters listed)"
        ),
        h5(
          "Optional Step: Inspect the distribution plot to see a visual version of the Length of Service Distribution"
        ),
        
        
        width = 3
      ),
      mainPanel(tabsetPanel(
        tabPanel(
          title = "Model fits to user data",
          fileInput(
            inputId = "los_dat",
            label = "Upload csv",
            multiple = FALSE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv"),
            width = '25%'
          ),
          actionButton(inputId = "go_distfit", label = "Run Distribution Fit Tool"),
          br(),
          br(),
          fluidRow(
            column(10, plotOutput("los_fit_plot"), align = "center"),
            column(2, br(), br(), br(), tableOutput("mini_summary"), align = "center")
          ),
          
          fluidRow(br(),
                   h3(textOutput("los_text")),
                   p(textOutput("los_text_help"))),
          fluidRow(column(12, tableOutput("los_fit_table"), align =
                            "center")),
          h3(textOutput("fit_error"))
        ),
        
        tabPanel(
          title = "Scale data by mean",
          fluidRow(column(
            8,
            br(),
            p(
              "Distributions and Parameters have been found for a variety of PODs/ Service Points, which are listed in the Service Point Library.
                            These were based on model fits to BNSSG data in order to match the shape of the Service time distribution. The data is rescaled based
                            on the Average Service value entered to create the required distribution."
            )
          )),
          hr(),
          fluidRow(
            column(3, uiOutput("treatment_select_ui")),
            column(
              2,
              numericInput(
                inputId = "treatment_mean",
                label = "Average Length of Service (Mean)",
                min = 0,
                value = 0,
                step = 0.01
              )
            ),
            column(
              1,
              br(),
              actionButton(inputId = "go_scaled_fit", label = "Run/Refresh Scaling Tool")
            )
          ),
          hr(),
          tableOutput("scaled_fit"),
          plotOutput("scaled_fit_plot")
        )
      ))
    )
  ),
  
  ####TOOL 1 - NETWORK IMPORT & VISUALISATION TAB ####
  
  tabPanel(
    "1. Network Import & Visualisation",
    sidebarLayout(
      # Sidebar panel for inputs -
      sidebarPanel(
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
        ),
        
        width = 3
      ),
      
      
      mainPanel(
        # Output: Data file --
        
        grVizOutput("network", height = "450px"),
        tableOutput("file_check_issues"),
        tableOutput("contents1"),
        tableOutput("contents2")
        
      )
      
    )
  ),
  
  ####TOOL 2 - SIMULATION SETUP & RUN TAB ####
  
  
  tabPanel(
    "2. Simulation Setup & Run",
    sidebarLayout(
      # Sidebar panel for inputs --
      sidebarPanel(
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
        ),
        
        
        
        width = 3
      ),
      
      # Main panel for displaying outputs -
      mainPanel(
        useShinyalert(),
        fluidRow(
          column(4, align = "center", tableOutput("checklist_table_render")),
          column(8, grVizOutput("cl_viz"))
        ),
        fluidRow(column(12, align = "center", h1(textOutput(
          "comp"
        )))),
        br()
        #,
        #fluidRow(column(12,align="center",tableOutput("run_time")))
      )
      
    )
  ),
  
  
  ####TOOL 3 - SIMULATION OUTPUTS TAB ####
  
  tabPanel(
    "3. Simulation Outputs",
    navlistPanel(
      id = "Simulation Outputs",
      
      tabPanel(
        title = "Output Interpretation",
        icon = icon("book"),
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = 'padding:0px;'
          ),
          column(
            9,
            h2("Output Interpretation"),
            p(
              "The pages on the left show the key results from the simulation, a description of which can be found below.
                        Depending on the number of replications run, the graphs and tables may take a moment to render (Click through the tabs to ensure the rendering begins).
                        Return to the previous pages using the navigation bar above."
            ),
            hr(),
            h4(
              strong(
                "Warm-Up Period Assistance (Only available in Trial Simulation Mode)"
              )
            ),
            p(
              "The warm-up period represents the time it takes for a simulation to reach stable running conditions and after which results can be recorded.
                         As each simulation starts from empty, it is important that the warm-up period be long enough so that the results collected are reflective of
                         the modelled system and not an emptier version. To determine a suitable warm-up period, the Warm-Up Period Assistance tab shows the total number of people in the
                                            system through time. This metric can be used to work out how long the warm-up period needs to be."
            ),
            
            hr(),
            h4(
              strong(
                "Average Through Time Plot (Available in both Trial and Full Simulation Modes)"
              )
            ),
            p(
              "A summary plot showing how each of the 5 time varying parameters vary through time with mean values plotted with envelopes of 50%, 95% and 99% percentile data.
                        This should allow the overarching trends in these metrics to be understood in a single figure and also how changes and shift in one metrics influence changes in
                        others. Variation in the mean lines could be the result of two different factors: 1) Sample size being too small (increase # of replications) or 2) The system has
                        inherent variation, either driven by random dynamics or the prescribed calendar schedule. This plot allows the user to quickly understand the dynamics of the system
                        e.g. if a unit has reached capcaity and stayed full, if the queue length has stabilised or if it is continuously increasing, whether the number of patients being delayed
                        due to capacity is lower than expected values."
            ),
            
            hr(),
            h4(
              strong(
                "Service Point & Pathway Statistics (Only available in Full Simulation Mode)"
              )
            ),
            p(
              "These pages contains multiple tables looking at 7 key metrics of the patient experience (broken down by service node or summarised over the pathway):"
            ),
            tags$ol(
              tags$li(
                strong("Total time in system"),
                "- Amount of time between arriving at the first node and leaving the last on a patient pathway."
              ),
              tags$li(
                strong("Wait"),
                "- Amount of time between arriving at a queue and starting service."
              ),
              tags$li(
                strong("Time Delayed (Capacity Driven)"),
                "- Amount of time experiencing a capacity driven delay."
              ),
              tags$li(
                strong("Time Delayed (Transition)"),
                "- Amount of time experiencing a transition delay."
              ),
              tags$li(
                strong("Length of Stay"),
                "- Amount of time between starting service and departing to the next service."
              ),
              tags$li(
                strong("Delay to Transfer"),
                "- Amount of time between finishing service and departure (i.e. end of any delays)."
              ),
              tags$li(
                strong("Rejection Rate"),
                "- Number of external arrivals that were rejected due to full queues per time unit."
              )
            ),
            
            
            h4(
              strong(
                "Metrics Through Time Summaries (Only available in Full Simulation Mode)"
              )
            ),
            p(
              "5 values are monitored throughout the simulation so their changes through time can be investigated through time:"
            ),
            strong(tags$ul(
              tags$li("Patient Occupancy"),
              tags$li("Bed Occupancy"),
              tags$li("Capacity Driven Delay"),
              tags$li("Transition Delay"),
              tags$li("Queue")
            )),
            p("Each page on the left contains the same 5 tables/graphs:"),
            tags$ul(
              tags$li(
                strong("Top Left - Percentage time at level plot"),
                " - A graph showing the amount of time each Service Point spent at level of the metric
                                (e.g. Amount of time Service Point A had an occupancy of 5 patients). The distribution of bars informs how the metric has varied throughout the simulation,
                                for example if the bars appear reasonably symetric around a value then the system is showing signs of stability. On the other hand, if one bar dominates then
                                the system is showing signs of underlying system dynamics e.g. constantly at full capacity or following a strict calendar."
              ),
              br(),
              tags$li(
                strong("Top Right - Metric through time plot"),
                " - A graph showing the metric in question through time, split by Service Point and replicate (max 5 random replicatations).
                                These represent actual simulation runs that are then combined (across all replications) to form the summary outputs. These should not be used to infer specific results, but
                                are intended to be illustrative of the variation found within simulation."
              ),
              br(),
              tags$li(
                strong("Bottom Left - Percentiles summary table"),
                " - A table outlining the values associated with different percentile levels e.g. 90% of the time, the Service Point has an occupancy of 5 or less"
              ),
              br(),
              tags$li(
                strong("Bottom Right - Percentage time at level table"),
                " - Raw data used to construct 'Percentage time at level' plot.
                                Can be filtered and sorted by any column and also contains a cumulative sum which can be used to calcuate percentiles."
              ),
              br(),
              tags$li(
                strong("Bottom Centre - Average Over Simulation"),
                " - Average value for the metric in question when using the data from the entire simulation."
              )
            ),
            p(
              "All plots show a maximum of 5 replicates and have the same legends and colour coding for Service Points"
            ),
            hr()
            
            
            
          )
        )
      ),
      
      tabPanel(
        "Warm-Up Period Assistance",
        icon = icon("chart-line"),
        h2(strong("Warm-Up Period Assistance")),
        hr(),
        
        
        conditionalPanel(
          condition = "input.run_type=='Trial Simulation'",
          p(
            "The warm-up period represents the time it takes for a simulation to reach stable running conditions and after which results can be recorded.
                         As each simulation starts from empty, it is important that the warm-up period be long enough so that the results collected are reflective of
                         the modelled system and not an emptier version. To determine a suitable warm-up period, find the time after which the total number of people in system has leveled out/ stabilised.
                                   For highly dynamic systems, you may also need to consult the average through time tab to see how the number of people in each service point and queue is changing. The
                                   warm-up period can be determined in the same way as before but needs to be the time required for all metrics to stabilise."
          ),
          plotOutput("tisp", height =
                       "850px")
          
        ),
        
        conditionalPanel(condition = "input.run_type=='Full Simulation'",
                         h2(strong(
                           "Not Available in Full Simulation"
                         )))
      ),
      
      tabPanel(
        "Average Through Time Plot",
        icon = icon("chart-line"),
        h2(strong("Average Through Time Overview")),
        hr(),
        p(
          "The plot below shows how each of the 5 time varying parameters vary through time with mean values plotted with envelopes of 50%, 95% and 99% percentile data.
                        This should allow the overarching trends in these metrics to be understood in a single figure and also how changes and shift in one metrics influence changes in
                        others. Variation in the mean lines could be the result of two different factors: 1) Sample size being too small (increase # of replications) or 2) The system has
                        iherent variation, either driven by random dynamics or the prescribed calendar schedule. This plot allows the user to quickly understand the dynamics of the system
                        e.g. if a unit has reached capcaity and stayed full, if the queue length has stabilised or if it is continuously increasing, whether the number of patients being delayed
                        due to capacity is lower than expected values."
        ),
        plotOutput("multi_plot", height = "850px")
      ),
      
      
      tabPanel(
        "Service Point Statistics",
        icon = icon("table"),
        h2(strong("Service Point Statistics")),
        hr(),
        conditionalPanel(
          condition = "input.run_type=='Full Simulation'",
          fluidRow(column(
            12, align = "center", grVizOutput("tables_viz1", height = "400px")
          )),
          br(),
          
          fluidRow(
            column(
              4,
              align = "center",
              h4("Wait Time"),
              tableOutput("node_wait_summary"),
              align = "center"
            ),
            column(
              4,
              align = "center",
              h4("Time Delayed (Capacity Driven)"),
              tableOutput("node_capacity_delay_summary"),
              align = "center"
            ),
            column(
              4,
              align = "center",
              h4("Time Delayed (Transition)"),
              tableOutput("node_transition_delay_summary"),
              align = "center"
            )
          ),
          
          fluidRow(
            column(
              4,
              align = "center",
              h4("Length Of Stay"),
              tableOutput("node_loss"),
              align = "center"
            ),
            column(
              4,
              align = "center",
              h4("Delay-To-Transfer"),
              tableOutput("node_dtts"),
              align = "center"
            ),
            column(
              width = 1,
              offset = 0,
              style = 'padding:0px;'
            ),
            column(2, h4("Rejection Rate"), tableOutput("rejs"), align =
                     "center")
          )
        ),
        
        conditionalPanel(condition = "input.run_type=='Trial Simulation'",
                         h2(
                           strong("Not Available in Trial Simulation")
                         ))
      ),
      
      tabPanel(
        "Pathway Statistics",
        icon = icon("table"),
        h2(strong("Pathway Statistics")),
        hr(),
        conditionalPanel(
          condition = "input.run_type=='Full Simulation'",
          fluidRow(column(
            12, align = "center", grVizOutput("tables_viz2", height = "400px")
          )),
          br(),
          fluidRow(
            column(
              4,
              align = "center",
              h4("Wait Time"),
              tableOutput("pat_wait_summary"),
              align = "center"
            ),
            column(
              4,
              align = "center",
              h4("Time Delayed (Capacity Driven)"),
              tableOutput("pat_capacity_delay_summary"),
              align = "center"
            ),
            column(
              4,
              align = "center",
              h4("Time Delayed (Transition)"),
              tableOutput("pat_transition_delay_summary"),
              align = "center"
            )
          ),
          
          fluidRow(
            column(
              4,
              align = "center",
              h4("Length Of Stay"),
              tableOutput("pat_loss"),
              align = "center"
            ),
            column(
              4,
              align = "center",
              h4("Delay-To-Transfer"),
              tableOutput("pat_dtts"),
              align = "center"
            ),
            column(
              4,
              align = "center",
              h4("Total Time in System"),
              tableOutput("ttiss"),
              align = "center"
            )
          )
        ),
        
        conditionalPanel(condition = "input.run_type=='Trial Simulation'",
                         h2(
                           strong("Not Available in Trial Simulation")
                         ))
      ),
      
      
      
      tabPanel(
        "Patient Occupancy Summary",
        icon = icon("user"),
        h2(strong("Patient Occupancy Summary")),
        hr(),
        conditionalPanel(
          condition = "input.run_type=='Full Simulation'",
          
          fluidRow(
            column(
              6,
              align = "center",
              h3("% time at Patient Occupancy level"),
              plotOutput("pto_plot", height = "500px")
            ),
            column(
              6,
              align = "center",
              h3("Patient Occupancy for 5 replicates"),
              plotOutput("o", height = "500px")
            )
          ),
          
          fluidRow(
            column(4, align = "center", dataTableOutput("opercentiles")),
            column(8, align = "center", dataTableOutput("pto_percent", width =
                                                          "70%"))
          ),
          
          fluidRow(
            column(
              width = 4,
              offset = 0,
              style = 'padding:0px;'
            ),
            column(2, align = "center", dataTableOutput("avg_occupancy"))
          )
        ),
        
        conditionalPanel(condition = "input.run_type=='Trial Simulation'",
                         h2(
                           strong("Not Available in Trial Simulation")
                         ))
        
      ),
      
      tabPanel(
        "Bed Occupancy Summary",
        icon = icon("bed"),
        h2(strong("Bed Occupancy Summary")),
        hr(),
        conditionalPanel(
          condition = "input.run_type=='Full Simulation'",
          fluidRow(
            column(
              6,
              align = "center",
              h3("% time at Bed Occupancy level"),
              plotOutput("ptb_plot", height = "500px")
            ),
            column(
              6,
              align = "center",
              h3("Bed Occupancy for 5 replicates"),
              plotOutput("b", height = "500px")
            )
          ),
          fluidRow(
            column(4, align = "center", dataTableOutput("bpercentiles")),
            column(8, align = "center", dataTableOutput("ptb_percent", width =
                                                          "70%"))
          ),
          
          fluidRow(
            column(
              width = 4,
              offset = 0,
              style = 'padding:0px;'
            ),
            column(2, align = "center", dataTableOutput("avg_occ_bed"))
          )
        ),
        
        conditionalPanel(condition = "input.run_type=='Trial Simulation'",
                         h2(
                           strong("Not Available in Trial Simulation")
                         ))
      ),
      
      
      
      
      
      tabPanel(
        "Capacity Driven Delay Summary",
        icon = icon("door-closed"),
        h2(strong("Capacity Driven Delay Summary")),
        hr(),
        conditionalPanel(
          condition = "input.run_type=='Full Simulation'",
          
          fluidRow(
            column(
              6,
              align = "center",
              h3("% time at Capacity Delay level"),
              plotOutput("ptd_plot", height = "500px")
            ),
            column(
              6,
              align = "center",
              h3("Capacity Delay Level for 5 replicates"),
              plotOutput("d", height = "500px")
            )
          ),
          fluidRow(
            column(4, align = "center", dataTableOutput("dpercentiles")),
            column(8, align = "center", dataTableOutput("ptd_percent", width =
                                                          "70%"))
          ),
          
          fluidRow(
            column(
              width = 4,
              offset = 0,
              style = 'padding:0px;'
            ),
            column(2, align = "center", dataTableOutput("avg_delayed"))
          )
        ),
        
        conditionalPanel(condition = "input.run_type=='Trial Simulation'",
                         h2(
                           strong("Not Available in Trial Simulation")
                         ))
        
      ),
      
      
      
      tabPanel(
        "Transition Delay Summary",
        icon = icon("expand-arrows-alt"),
        h2(strong("Transition Delay Summary")),
        hr(),
        
        conditionalPanel(
          condition = "input.run_type=='Full Simulation'",
          
          fluidRow(
            column(
              6,
              align = "center",
              h3("% time at Transition Delay level"),
              plotOutput("ptt_plot", height = "500px")
            ),
            column(
              6,
              align = "center",
              h3("Transition Delay Level for 5 replicates"),
              plotOutput("t", height = "500px")
            )
          ),
          fluidRow(
            column(4, align = "center", dataTableOutput("tpercentiles")),
            column(8, align = "center", dataTableOutput("ptt_percent", width =
                                                          "70%"))
          ),
          
          fluidRow(
            column(
              width = 4,
              offset = 0,
              style = 'padding:0px;'
            ),
            column(2, align = "center", dataTableOutput("avg_transition"))
          )
        ),
        
        conditionalPanel(condition = "input.run_type=='Trial Simulation'",
                         h2(
                           strong("Not Available in Trial Simulation")
                         ))
      ),
      
      tabPanel(
        "Queueing Summary",
        icon = icon("clock"),
        h2(strong("Queueing Summary")),
        hr(),
        
        conditionalPanel(
          condition = "input.run_type=='Full Simulation'",
          
          fluidRow(
            column(
              6,
              align = "center",
              h3("% time at Queue Length"),
              plotOutput("ptq_plot", height = "500px")
            ),
            column(
              6,
              align = "center",
              h3("Queue Length for 5 replicates"),
              plotOutput("q", height = "500px")
            )
          ),
          fluidRow(
            column(4, align = "center", dataTableOutput("qpercentiles")),
            column(8, align = "center", dataTableOutput("ptq_percent", width =
                                                          "70%"))
          ),
          
          fluidRow(
            column(
              width = 4,
              offset = 0,
              style = 'padding:0px;'
            ),
            column(2, align = "center", dataTableOutput("avg_queue"))
          )
        ),
        
        conditionalPanel(condition = "input.run_type=='Trial Simulation'",
                         h2(
                           strong("Not Available in Trial Simulation")
                         ))
        
      ),
      
      widths = c(3, 9),
      well = TRUE
      
    )
  ),
  
  ####TOOL 4 - DOWNLOAD OUTPUTS TAB ####
  
  tabPanel(
    "4. Download Outputs",
    sidebarLayout(
      # Sidebar panel for buttons --
      sidebarPanel(
        # File Downloader --
        h4(strong("Details")),
        h5(
          "A description of each of the files can be found in the",
          strong("Output Library Document.")
        ),
        
        
        hr(),
        h4("Data Tables"),
        p(
          "PathSimR produces an excel workbook which contains all metrics produced within the tool at both replicate level and simulation level. Each tab is clearly labelled,
                 with the first half of the tabs relating to patient level metrics (e.g. wait times, Length of Stays etc) and the second half containing information regarding the through
                 time metrics (e.g. Occupancy, Queues etc). The final tab contains all the data required to recreate the 'Average Through Time' plot."
        ),
        fluidRow(column(
          12,
          align = "center",
          downloadButton(
            outputId = "downloadtables",
            label = "Download Tables",
            icon = icon("download"),
            style = 'padding:10px; font-size:125%'
          )
        )),
        
        hr(),
        h4("Simulation Plots"),
        p(
          "All plots created in PathSimR are saved down in a single PDF that can then be manipulated as needed. All the figures shown in PathSimR can be recreated from the data
                 provided in the Data Tables (download button above)."
        ),
        fluidRow(column(
          12,
          align = "center",
          downloadButton(
            outputId = "downloadplot",
            label = "Download Plots",
            icon = icon("download"),
            style = 'padding:10px; font-size:125%'
          )
        )),
        
        hr(),
        h4("Automated Report"),
        p(
          "An automated word document is produced which includes a collection of figures and data tables from the simulation. These have been sorted into sections and a brief description of
                 the metrics and figures is included. This report is designed to speed up the summary process and provides easy manipulation for the user."
        ),
        fluidRow(column(
          12,
          align = "center",
          downloadButton(
            outputId = "downloadreport",
            label = "Download Report",
            icon = icon("download"),
            style = 'padding:10px; font-size:125%'
          )
        )),
        hr(),
        
        
        width = 3
      ),
      
      # Main panel for displaying outputs -
      mainPanel()
      
    ),
    useShinyalert()
  )
  
  
)



