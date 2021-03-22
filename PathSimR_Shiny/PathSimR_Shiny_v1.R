# install.packages("shiny")
# install.packages("DiagrammeR")
# install.packages("magrittr")
# install.packages("readr")
# install.packages("DT")
# install.packages("openxlsx")
# install.packages("grid")
# install.packages("gridExtra")
# install.packages("parallel")
# install.packages("data.table")
# install.packages("tidyverse")
# install.packages("shinyalert")
# install.packages("shinyMatrix")
# install.packages("fitdistrplus")
# install.packages("shinyBS")
# install.packages("shinyjs")
# install.packages("shinythemes")

library(shiny)
library(DiagrammeR)
library(magrittr)
library(readr)
library(DT)
library(openxlsx)
library(grid)
library(gridExtra)
library(parallel)
library(data.table)
library(tidyverse)
library(shinyalert)
library(shinyMatrix)
library(fitdistrplus)
library(shinyBS)
library(shinyjs)
library(shinythemes)

options(shiny.maxRequestSize = 30 * 1024 ^ 2) # Sets the Shiny file upload size limit to 30MB

#### Creating the starting name matricies ####
m1 <- matrix(nrow = 1,
             ncol = 1,
             data = c("A"))
colnames(m1) <- c("Service Points")
rownames(m1) <- c("Enter Names in Right Column")


m2 = matrix(nrow = 1,
            ncol = 1,
            data = c("B"))
colnames(m2) <- c("Exits")
rownames(m2) <- c("Enter Names in Right Column")


##### SHINY UI CODE #####
ui <- navbarPage(
  theme = shinytheme("cerulean"),
  
  
  
  title = c(tagList(icon("compass"), "Navigation Bar")),
  id = "navbar",
  
  ####INTRODUCTION TAB ####
  
  tabPanel("Introduction",
           sidebarLayout(
             sidebarPanel(
               #Makes some manual changes to the CSS to increase the size of checkboxes and font on tabs
               
               tags$head(
                 tags$style(
                   ".checkbox { /* checkbox is a div class*/line-height: 40px;margin-bottom: 40px; /*set the margin, so boxes don't overlap*/}
                    input[type='checkbox']{ /* style for checkboxes */
                      width: 30px; /*Desired width*/
                      height: 30px; /*Desired height*/
                      line-height: 30px;
                    }
                    span {
                        margin-left: 15px;  /*set the margin, so boxes don't overlap labels*/
                        line-height: 30px;
                    }
                    * { font-family: Helvetica }
                    .nav-tabs {font-size: 30px}
                "
                 ),
                 tags$style(
                   HTML(
                     "
        input[type=number] {
              -moz-appearance:textfield;
        }
        input[type=number]::{
              -moz-appearance:textfield;
        }
        input[type=number]::-webkit-outer-spin-button,
        input[type=number]::-webkit-inner-spin-button {
              -webkit-appearance: none;
              margin: 0;
        }

    "
                   )
                 ),
                 tags$style(type = 'text/css', '.navbar {font-size: 17px;}')
               ),
               
               
               
               h1(strong("PathSimR")),
               h2(em(
                 "A versatile tool for modelling pathway capacity in R"
               )),
               
               hr(),
               fluidRow(column(
                 12,
                 align = "center",
                 actionButton(
                   inputId = "j2w",
                   label = "Start Pathway Wizard",
                   icon = icon("magic"),
                   style = 'padding:16px; font-size:150%'
                 )
               )),
               hr(),
               fluidRow(column(
                 12,
                 align = "center",
                 actionButton(
                   inputId = "j2s1",
                   label = "Start Simulation Tool",
                   icon = icon("project-diagram"),
                   style = 'padding:16px; font-size:150%'
                 )
               )),
               hr(),
               h3(
                 "PathSimR is a simulation tool designed to give insights into how care pathways are performing
                              and enable 'what if' analysis to identify more effective and efficient service configurations."
               ),
               hr(),
               em(strong(
                 p(
                   "An overview of the tool and a glossary of key terms can be found in the Overview & Glossary tab in the Navigation Bar (accessible at all times).
                           Moreover, throughout the tool, there are ",
                   actionLink(inputId = "info_help", label = icon("info-circle")),
                   " symbols which provide additional information on specific topics."
                 )
               )),
               em(strong(
                 p(
                   "New users are advised to read through the overview and glossary when first using the tool to familiarise themselves with the relevant terminology and ideas."
                 )
               )),
               em(strong(
                 p(
                   "All data must be entered in a consistent time unit (e.g. all data uploaded is on the scale of either days or hours, but not a mixture of the two - if using hours, enter a day as 24 time units, a week as 168 etc.). Users can choose a label for their time unit on the Network Import and Visualisation tab - this will not be used in calculations, but will be added to output tables and graphs."
                 )
               )),
               hr(),
               p(
                 "Proceed through the tabs in the navigation bar at the top of the page, completing each tab in sequence.
               Once all inputs have been entered and confirmed on a tab, the subsequent tab will appear.
               The tabs can be navigated either using the 'previous' and 'next' buttons at the bottom of the page or clicking on the tabs themselves at the top of the page.
               Instructions are given on every page regarding how to use the tool. Users may return to any previous tab and update inputs,
                 simply rerun any uploads/processes on subsequent tabs before moving on."
               ),
               hr(),
               p(
                 "The Pathway Wizard has been designed to help users collate the neccessary information required to run a simulation.
                 Like the simulation tool, complete each tab in sequence and then proceed into PathSimR."
               ),
               hr(),
               p(
                 strong("To zoom out, press control+- and to zoom in press control+shift+=")
               ),
               p(
                 strong(
                   "For more information or guidance on how to use the tool, please contact the Modelling and Analytics team at BNSSG CCG."
                 )
               ),
               
               
               width = 3
               
             ),
             
             mainPanel(
               br(),
               fluidRow(
                 column(4, align = "center", img(src = 'THF_logo.png', style = "height:150px;")),
                 column(4, align = "center", img(src = 'BNSSG_logo.png', style = "height:150px;")),
                 column(4, align = "center", img(src = 'UoB_logo.png', style = "height:150px;"))
               ),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               
               fluidRow(column(
                 12, align = "center", img(src = 'Rplot.png', style = "height:400px;")
               )),
               #fluidRow(column(12,align="center",img(src='Logo2.jpg'))),
               
               br(),
               br(),
               br(),
               br(),
               br(),
               h5(strong(
                 "Example patient pathway (built in PathSimR)"
               ), align = "right")
             )
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
       # useShinyalert(),
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
      
    )
    #,
    #useShinyalert()
  )
  
  
)



#### SINHY SERVER CODE (INC SIM CODE) ####
server <- function(input, output, session) {
  ####Figures for Modals####
  
  output$ext_arr_example = renderTable({
    data.frame(
      "Start Time" = c(0, 100, 150),
      "End Time" = c(100, 150, 200),
      "Arrival Rate" = c(1, 0, 2)
    )
  }, caption = "Example External Arrival Calendar",
  caption.placement = getOption("xtable.caption.placement", "top"),
  caption.width = getOption("xtable.caption.width", NULL), striped = TRUE, bordered = TRUE)
  
  output$cap_example = renderTable({
    data.frame(
      "Start Time" = c(0, 30, 90),
      "End Time" = c(30, 90, 180),
      "Capacity" = c(24, 48, 72)
    )
  }, caption = "Example Capacity Calendar",
  caption.placement = getOption("xtable.caption.placement", "top"),
  caption.width = getOption("xtable.caption.width", NULL), striped = TRUE, bordered = TRUE)
  
  output$model_help_figure <- renderPlot({
    x <- c(rexp(10000, 1))
    
    fe <- fitdist(data = x, distr = "exp")
    fl <- fitdist(data = x, distr = "lnorm")
    fu <- fitdist(data = x, distr = "unif")
    fw <- fitdist(data = x, distr = "weibull")
    fg <- fitdist(data = x, distr = "gamma")
    
    p <-
      denscomp(
        ft = list(fe, fl, fu, fw, fg),
        plotstyle = "ggplot",
        breaks = 100,
        #fitcol = c("#009E73","#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
        fitlty = 1
      )
    p <- p + theme_bw()
    p
    
    
    
  }, res = 128)
  
  
  #### Navigation Buttons ####
  
  hideTab(inputId = "navbar", target = "1. Network Import & Visualisation")
  hideTab(inputId = "navbar", target = "2. Simulation Setup & Run")
  hideTab(inputId = "navbar", target = "3. Simulation Outputs")
  hideTab(inputId = "navbar", target = "4. Download Outputs")
  hideTab(inputId = "navbar", target = "W1. Setup")
  hideTab(inputId = "navbar", target = "W2. Data Entry")
  hideTab(inputId = "navbar", target = "W3. Final Wizard Tables & Download")
  hideTab(inputId = "navbar", target = "Service Distribution Tool")
  
  
  
  observeEvent(input$j2w, {
    hideTab(inputId = "navbar", target = "1. Network Import & Visualisation")
    hideTab(inputId = "navbar", target = "2. Simulation Setup & Run")
    hideTab(inputId = "navbar", target = "3. Simulation Outputs")
    hideTab(inputId = "navbar", target = "4. Download Outputs")
    hideTab(inputId = "navbar", target = "W1. Setup")
    hideTab(inputId = "navbar", target = "W2. Data Entry")
    hideTab(inputId = "navbar", target = "W3. Final Wizard Tables & Download")
    hideTab(inputId = "navbar", target = "Service Distribution Tool")
    
    showTab(inputId = "navbar", target = "W1. Setup")
    
    
    updateTabsetPanel(session, "navbar",
                      selected = "W1. Setup")
  })
  
  
  observeEvent(input$j2s1, {
    hideTab(inputId = "navbar", target = "1. Network Import & Visualisation")
    hideTab(inputId = "navbar", target = "2. Simulation Setup & Run")
    hideTab(inputId = "navbar", target = "3. Simulation Outputs")
    hideTab(inputId = "navbar", target = "4. Download Outputs")
    hideTab(inputId = "navbar", target = "W1. Setup")
    hideTab(inputId = "navbar", target = "W2. Data Entry")
    hideTab(inputId = "navbar", target = "W3. Final Wizard Tables & Download")
    hideTab(inputId = "navbar", target = "Service Distribution Tool")
    
    showTab(inputId = "navbar", target = "1. Network Import & Visualisation")
    updateTabsetPanel(session, "navbar",
                      selected = "1. Network Import & Visualisation")
  })
  
  
  
  observeEvent(input$jb2i, {
    updateTabsetPanel(session, "navbar",
                      selected = "Introduction")
  })
  
  
  observeEvent(input$j2PSR2, {
    updateTabsetPanel(session, "navbar",
                      selected = "2. Simulation Setup & Run")
  })
  
  
  observeEvent(input$j2s, {
    updateTabsetPanel(session, "navbar",
                      selected = "W1. Setup")
  })
  
  observeEvent(input$j2de, {
    showTab(inputId = "navbar", target = "W2. Data Entry")
    showTab(inputId = "navbar", target = "Service Distribution Tool")
    updateTabsetPanel(session, "navbar",
                      selected = "W2. Data Entry")
  })
  
  observeEvent(input$j2ftd, {
    showTab(inputId = "navbar", target = "W3. Final Wizard Tables & Download")
    updateTabsetPanel(session, "navbar",
                      selected = "W3. Final Wizard Tables & Download")
  })
  
  observeEvent(input$jb2de, {
    showTab(inputId = "navbar", target = "W2. Data Entry")
    updateTabsetPanel(session, "navbar",
                      selected = "W2. Data Entry")
  })
  
  observeEvent(input$j2PSR, {
    showTab(inputId = "navbar", target = "1. Network Import & Visualisation")
    updateTabsetPanel(session, "navbar",
                      selected = "1. Network Import & Visualisation")
  })
  
  
  
  observeEvent(input$j2PSR3, {
    updateTabsetPanel(session, "navbar",
                      selected = "3. Simulation Outputs")
  })
  
  observeEvent(input$jb2i2, {
    updateTabsetPanel(session, "navbar",
                      selected = "Introduction")
  })
  
  observeEvent(input$jb2niv, {
    updateTabsetPanel(session, "navbar",
                      selected = "1. Network Import & Visualisation")
  })
  
  
  
  ##### START OF DYNAMIC WIZARD SERVER CODE ######
  
  
  #### Name Input tables and checks ####
  ### Creates table of service point names ###
  output$sp_table <- renderTable({
    x <- input$sp
    x <- unique(x)
    rownames(x) <- 1:nrow(x)
    colnames(x) <- "Service Point"
    x <- trimws(x = x, which = "both")
    x <- gsub(x = x, pattern = " ", "_")
    x <- x[which(x != "")]
    x <- data.frame("Service Points" = x)
    colnames(x) <- "Service Points"
    x
    
    
  }, rownames = TRUE, striped = TRUE, bordered = TRUE)
  
  
  ### Creates table of exit names ###
  output$exit_table <- renderTable({
    x <- input$exit
    x <- unique(x)
    rownames(x) <- 1:nrow(x)
    x <- trimws(x = x, which = "both")
    x <- gsub(x = x, pattern = " ", "_")
    x <- x[which(x != "")]
    data.frame("Exits" = x)
  }, rownames = TRUE, striped = TRUE, bordered = TRUE)
  
  
  
  ### Creates text for duplicates ###
  output$duplicate <- renderText({
    x <- input$sp
    x <- unique(x)
    rownames(x) <- 1:nrow(x)
    colnames(x) <- "Service Point"
    x <- trimws(x = x, which = "both")
    x <- gsub(x = x, pattern = " ", "_")
    x <- x[which(x != "")]
    x <- data.frame("Service Points" = x)
    colnames(x) <- "Service Points"
    s <- x
    
    
    
    x <- input$exit
    x <- unique(x)
    rownames(x) <- 1:nrow(x)
    x <- trimws(x = x, which = "both")
    x <- gsub(x = x, pattern = " ", "_")
    x <- x[which(x != "")]
    e <- data.frame("Exits" = x)
    
    if (any(s[, 1] %in% e[, 1]) | any(e[, 1] %in% s[, 1])) {
      "One or more names appear in both the Service Point & Exit lists. \n Please update before proceeding."
      
      
      
    }
    
    
  })
  
  #### Creates the Data Entry Service Point tabs UI ####
  output$tabs = renderUI({
    x <- input$sp
    x <- unique(x)
    rownames(x) <- 1:nrow(x)
    x <- trimws(x = x, which = "both")
    x <- gsub(x = x, pattern = " ", "_")
    sp <- x[which(x != "")]
    
    x <- input$exit
    x <- unique(x)
    rownames(x) <- 1:nrow(x)
    x <- trimws(x = x, which = "both")
    x <- gsub(x = x, pattern = " ", "_")
    exit <- x[which(x != "")]
    
    node_number <- length(sp)
    exit_number <- length(exit)
    tabnames <- sp
    
    node_names <- sp
    
    exit_names <- exit
    
    all_names <- c(node_names, exit_names)
    
    
    
    #### Creates the transition probability inputs & delay departure entry (dynamic based on number of nodes & exits) ####
    for (j in 1:node_number) {
      assign(x = paste0("transition_", j),
             value = lapply(1:length(all_names), function(i) {
               if (j != i) {
                 column(
                   2,
                   fluidRow(
                     column(
                       12,
                       align = "center",
                       h4(all_names[i]),
                       style = 'padding:2px; font-size:150%'
                     )
                   ),
                   
                   numericInput(
                     inputId = paste0("transition_", j, "_", i),
                     label = paste("Proportion from", all_names[j], "to", all_names[i]),
                     value = 0,
                     min = 0,
                     max = 1,
                     step = 0.001
                   ),
                   selectInput(
                     inputId = paste0("delay_dist_", j, "_", i),
                     label = paste(
                       "Distribution for Transition Delay from",
                       all_names[j],
                       "to",
                       all_names[i]
                     ),
                     choices = c(
                       "None",
                       "Exponential",
                       "log-Normal",
                       "Uniform",
                       "Weibull",
                       "Gamma"
                     ),
                     selected = "None",
                     selectize = F
                   ),
                   
                   fluidRow(
                     conditionalPanel(
                       condition = paste0(
                         "input.",
                         paste0("delay_dist_", j, "_", i),
                         " == 'None'"
                       ),
                       disabled(column(
                         12,
                         textInput(
                           inputId = paste0("delay_param_none_1_", i),
                           value = "NA",
                           label = "No Parameters Required"
                         )
                       ))
                     ),
                     
                     
                     conditionalPanel(
                       condition = paste0(
                         "input.",
                         paste0("delay_dist_", j, "_", i),
                         " == 'Exponential'"
                       ),
                       column(
                         6,
                         numericInput(
                           inputId = paste0("delay_param_exp_1_", j, "_", i),
                           label = "Rate",
                           value = "",
                           min = 0,
                           step = 0.0001
                         )
                       ),
                       column(6, br())
                     ),
                     
                     conditionalPanel(
                       condition = paste0(
                         "input.",
                         paste0("delay_dist_", j, "_", i),
                         " == 'log-Normal'"
                       ),
                       column(
                         6,
                         numericInput(
                           inputId = paste0("delay_param_lnorm_1_", j, "_", i),
                           label = "meanlog",
                           value = "",
                           step = 0.0001
                         )
                       ),
                       column(
                         6,
                         numericInput(
                           inputId = paste0("delay_param_lnorm_2_", j, "_", i),
                           label = "sdlog",
                           value = "",
                           step = 0.0001
                         )
                       )
                     ),
                     
                     conditionalPanel(
                       condition = paste0(
                         "input.",
                         paste0("delay_dist_", j, "_", i),
                         " == 'Uniform'"
                       ),
                       column(
                         6,
                         numericInput(
                           inputId = paste0("delay_param_unif_1_", j, "_", i),
                           label = "Min",
                           value = "",
                           min = 0,
                           step = 0.0001
                         )
                       ),
                       column(
                         6,
                         numericInput(
                           inputId = paste0("delay_param_unif_2_", j, "_", i),
                           label = "Max",
                           value = "",
                           min = 0,
                           step = 0.0001
                         )
                       )
                     ),
                     
                     conditionalPanel(
                       condition = paste0(
                         "input.",
                         paste0("delay_dist_", j, "_", i),
                         " == 'Weibull'"
                       ),
                       column(
                         6,
                         numericInput(
                           inputId = paste0("delay_param_weibull_1_", j, "_", i),
                           label = "Shape",
                           value = "",
                           step = 0.0001
                         )
                       ),
                       column(
                         6,
                         numericInput(
                           inputId = paste0("delay_param_weibull_2_", j, "_", i),
                           label = "Scale",
                           value = "",
                           step = 0.0001
                         )
                       )
                     ),
                     
                     conditionalPanel(
                       condition = paste0(
                         "input.",
                         paste0("delay_dist_", j, "_", i),
                         " == 'Gamma'"
                       ),
                       column(
                         6,
                         numericInput(
                           inputId = paste0("delay_param_gamma_1_", j, "_", i),
                           label = "Shape",
                           value = "",
                           step = 0.001
                         )
                       ),
                       column(
                         6,
                         numericInput(
                           inputId = paste0("delay_param_gamma_2_", j, "_", i),
                           label = "Rate",
                           value = "",
                           step = 0.001
                         )
                       )
                     )
                     
                   ),
                   
                   br(),
                   style = 'border:0.5px dashed #e6e6e6;'
                 )
               } else {
                 column(
                   2,
                   fluidRow(
                     column(
                       12,
                       align = "center",
                       h4(all_names[i]),
                       style = 'padding:2px; font-size:150%'
                     )
                   ),
                   
                   disabled(
                     numericInput(
                       inputId = paste0("transition_", j, "_", i),
                       label = paste("Proportion from", all_names[j], "to", all_names[i]),
                       value = 0
                     )
                   ),
                   
                   disabled(
                     textInput(
                       inputId = paste0("delay_dist_", j, "_", i),
                       label = paste(
                         "Distribution for Transition Delay from",
                         all_names[j],
                         "to",
                         all_names[i]
                       ),
                       value = "None"
                     )
                   ),
                   
                   fluidRow(column(12, disabled(
                     textInput(
                       inputId = paste0("delay_param_none_1_", i),
                       value = "NA",
                       label = "No Parameters Required"
                     )
                   ))),
                   
                   
                   
                   br(),
                   style = 'border:0.5px dashed #e6e6e6;'
                 )
                 
                 
                 
               }
               
             }))
    }
    
    
    
    
    
    m3 = matrix(ncol = 3,
                nrow = node_number,
                data = "")
    colnames(m3) <- c("Start Time", "End Time", "Arrival Rate")
    
    m4 = matrix(ncol = 3,
                nrow = node_number,
                data = "")
    colnames(m4) <- c("Start Time", "End Time", "Capacity")
    
    
    
    #### Defines 'tabs' layout (dynamic based on number of nodes & exits) ####
    myTabs = lapply(1:node_number, function(i) {
      tabPanel(
        title = HTML(tabnames[i]),
        useShinyjs(),
        br(),
        h1(paste("Service Point Name:", tabnames[i])),
        
        hr(),
        
        
        h4("Length of Service Information"),
        #p("If distribution and parameters for the service point are not know, use the Service Distribution Tool (in the bar above) to either fit models to uploaded data or scale against BNSSG data and then enter resulting distributions and parameters.", style="color:gray"),
        
        fluidRow(
          column(
            2,
            selectInput(
              inputId = paste0("serv_dist_", i),
              label = "Select a distribution",
              choices = c(
                "Exponential",
                "log-Normal",
                "Uniform",
                "Weibull",
                "Gamma"
              ),
              selected = "Exponential"
            ),
            selectize = F
          ),
          column(
            width = 1,
            offset = 0,
            style = 'padding:0px;'
          ),
          
          
          conditionalPanel(
            condition = paste0("input.", paste0("serv_dist_", i), " == 'Exponential'"),
            
            column(
              2,
              numericInput(
                inputId = paste0("serv_param_exp_1_", i),
                label = "Rate",
                value = "",
                step = 0.001,
                min = 0
              )
            )
            
            
          ),
          
          conditionalPanel(
            condition = paste0("input.", paste0("serv_dist_", i), " == 'log-Normal'"),
            column(
              2,
              numericInput(
                inputId = paste0("serv_param_lnorm_1_", i),
                label = "meanlog",
                value = "",
                step = 0.001
              )
            ),
            column(
              2,
              numericInput(
                inputId = paste0("serv_param_lnorm_2_", i),
                label = "sdlog",
                value = "",
                step = 0.001
              )
            )
            
            
          ),
          
          conditionalPanel(
            condition = paste0("input.", paste0("serv_dist_", i), " == 'Uniform'"),
            column(
              2,
              numericInput(
                inputId = paste0("serv_param_unif_1_", i),
                label = "Min",
                value = "",
                step = 0.001,
                min = 0
              )
            ),
            column(
              2,
              numericInput(
                inputId = paste0("serv_param_unif_2_", i),
                label = "Max",
                value = "",
                step = 0.001,
                min = 0
              )
            )
          ),
          
          conditionalPanel(
            condition = paste0("input.", paste0("serv_dist_", i), " == 'Weibull'"),
            column(
              2,
              numericInput(
                inputId = paste0("serv_param_weibull_1_", i),
                label = "Shape",
                value = "",
                step = 0.001
              )
            ),
            column(
              2,
              numericInput(
                inputId = paste0("serv_param_weibull_2_", i),
                label = "Scale",
                value = "",
                step = 0.001
              )
            )
          ),
          
          conditionalPanel(
            condition = paste0("input.", paste0("serv_dist_", i), " == 'Gamma'"),
            column(
              2,
              numericInput(
                inputId = paste0("serv_param_gamma_1_", i),
                label = "Shape",
                value = "",
                step = 0.001
              )
            ),
            column(
              2,
              numericInput(
                inputId = paste0("serv_param_gamma_2_", i),
                label = "Rate",
                value = "",
                step = 0.001
              )
            )
          ),
          
          style = 'border-bottom:1px dashed silver;'
          
        ),
        
        h4("Queue Information"),
        #p("An external queue is defined as a queue that accepts arrivals from outside of the pathway.
        # An internal queue is one that connects service points within the pathway network.", style="color:gray"),
        fluidRow(
          column(
            2,
            numericInput(
              inputId = paste0("ext_q_", i),
              label = "External Queue Capacity",
              value = 0,
              min = 0
            )
          ),
          column(
            width = 1,
            offset = 0,
            style = 'padding:0px;'
          ),
          column(
            2,
            numericInput(
              inputId = paste0("int_q_", i),
              label = "Internal Queue Capacity",
              value = 0,
              min = 0
            )
          ),
          style = 'border-bottom:1px dashed silver;'
        ),
        
        
        
        fluidRow(column(
          12,
          h4("Transitions & Departure Delays"),
          # p("All proportion values should sum to 1 for each Service Point tab.
          #   A fixed presscribed delay can be modelled using the uniform distribution and entering the same value into the min and max boxes.", style="color:gray"),
          do.call(fluidRow, get(paste0(
            "transition_", i
          )))
        )),
        fluidRow(
          column(
            width = 12,
            offset = 0,
            style = 'padding:10px;'
          ),
          style = 'border-bottom:1px dashed silver;'
        ),
        
        
        
        fluidRow(
          column(
            5,
            h4("External Arrival Rate Calendar"),
            p(
              "For more information, consult the instructions sidebar (Step 4) and 'How do I fill the calendar?' info button",
              style = "color:gray"
            ),
            fluidRow(column(
              12,
              matrixInput(
                inputId = paste0("ext_arr_", i),
                value = m3,
                class = "numeric",
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
            ))
          ),
          
          column(
            width = 1,
            offset = 0,
            style = 'padding:0px;'
          ),
          
          column(
            5,
            h4("Capacity Calendar"),
            p(
              "For more information, consult the instructions sidebar (Step 4) and 'How to fill out the calendar?' info button",
              style = "color:gray"
            ),
            fluidRow(column(
              12,
              matrixInput(
                inputId = paste0("cap_", i),
                value = m4,
                class = "numeric",
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
            ))
          ),
          
          column(
            width = 1,
            offset = 0,
            style = 'padding:0px;'
          )
          
        )
      )
      
      
    })
    do.call(tabsetPanel, myTabs)
  })
  
  
  
  
  #### Creates the trial Var_input ####
  var <- eventReactive(input$go, {
    x <- input$sp
    x <- unique(x)
    rownames(x) <- 1:nrow(x)
    x <- trimws(x = x, which = "both")
    x <- gsub(x = x, pattern = " ", "_")
    sp <- x[which(x != "")]
    
    x <- input$exit
    x <- unique(x)
    rownames(x) <- 1:nrow(x)
    x <- trimws(x = x, which = "both")
    x <- gsub(x = x, pattern = " ", "_")
    exit <- x[which(x != "")]
    
    node_number <- length(sp)
    exit_number <- length(exit)
    tabnames <- sp
    
    node_names <- sp
    
    exit_names <- exit
    
    all_names <- c(node_names, exit_names)
    
    
    
    
    ## Transition Matrix
    t1 = lapply(1:node_number, function(j) {
      t2 = lapply(1:length(all_names), function(i) {
        input[[paste0("transition_", j, "_", i)]]
      })
      t2
    })
    
    dat <- as.numeric(as.vector(unlist(t1)))
    
    tm <- matrix(data = dat,
                 nrow = length(node_names),
                 byrow = T)
    #tm<-t(tm)
    colnames(tm) <- all_names
    rownames(tm) <- node_names
    
    
    ## Queues
    
    iq <- lapply(1:node_number , function(i) {
      input[[paste0("int_q_", i)]]
    })
    iq <- as.numeric(as.vector(unlist(iq)))
    
    
    eq <- lapply(1:node_number , function(i) {
      input[[paste0("ext_q_", i)]]
    })
    eq <- as.numeric(as.vector(unlist(eq)))
    
    ## Service Distributions & Parameters
    
    sd <- lapply(1:node_number , function(i) {
      input[[paste0("serv_dist_", i)]]
    })
    sd <- as.vector(unlist(sd))
    
    
    sdp <- lapply(1:node_number , function(i) {
      if (sd[i] == "Exponential") {
        input[[paste0("serv_param_exp_1_", i)]]
        
      } else if (sd[i] == "log-Normal") {
        paste0(input[[paste0("serv_param_lnorm_1_", i)]], ";", input[[paste0("serv_param_lnorm_2_", i)]])
        
      } else if (sd[i] == "Uniform") {
        paste0(input[[paste0("serv_param_unif_1_", i)]], ";", input[[paste0("serv_param_unif_2_", i)]])
        
      } else if (sd[i] == "Weibull") {
        paste0(input[[paste0("serv_param_weibull_1_", i)]], ";", input[[paste0("serv_param_weibull_2_", i)]])
        
      } else if (sd[i] == "Gamma") {
        paste0(input[[paste0("serv_param_gamma_1_", i)]], ";", input[[paste0("serv_param_gamma_2_", i)]])
        
      }
      
    })
    sdp <- as.vector(unlist(sdp))
    
    
    
    ## Delay Distribution Matrix
    
    dd1 = lapply(1:node_number, function(j) {
      dd2 = lapply(1:length(all_names), function(i) {
        input[[paste0("delay_dist_", j, "_", i)]]
      })
      dd2
    })
    
    
    
    dat <- as.vector(unlist(dd1))
    
    ddm <- matrix(data = dat,
                  nrow = length(node_names),
                  byrow = T)
    
    ddm[which(ddm == "None")] <- NA
    ddm[which(ddm == "Exponential")] <- "exp"
    ddm[which(ddm == "log-Normal")] <- "lnorm"
    ddm[which(ddm == "Uniform")] <- "unif"
    ddm[which(ddm == "Weibull")] <- "weibull"
    ddm[which(ddm == "Gamma")] <- "gamma"
    
    
    
    #tm<-t(tm)
    colnames(ddm) <- all_names
    rownames(ddm) <- paste0(node_names, "_Delay_Dist")
    output$ddm <- renderTable({
      ddm
    })
    
    
    
    
    ## Delay Parameter Matrix
    
    dp1 <- lapply(1:node_number , function(j) {
      dp2 = lapply(1:length(all_names), function(i) {
        test <- input[[paste0("delay_dist_", j, "_", i)]]
        
        
        if (test == "None") {
          NA
          
        } else if (test == "Exponential") {
          input[[paste0("delay_param_exp_1_", j, "_", i)]]
          
        } else if (test == "log-Normal") {
          paste0(input[[paste0("delay_param_lnorm_1_", j, "_", i)]], ";", input[[paste0("delay_param_lnorm_2_", j, "_", i)]])
          
        } else if (test == "Uniform") {
          paste0(input[[paste0("delay_param_unif_1_", j, "_", i)]], ";", input[[paste0("delay_param_unif_2_", j, "_", i)]])
          
        } else if (test == "Weibull") {
          paste0(input[[paste0("delay_param_weibull_1_", j, "_", i)]], ";", input[[paste0("delay_param_weibull_2_", j, "_", i)]])
          
        } else if (test == "Gamma") {
          paste0(input[[paste0("delay_param_gamma_1_", j, "_", i)]], ";", input[[paste0("delay_param_gamma_2_", j, "_", i)]])
          
        }
        
      })
      dp2
    })
    
    ddp <- as.vector(unlist(dp1))
    
    ddp <- matrix(data = ddp,
                  nrow = length(node_names),
                  byrow = T)
    
    
    
    colnames(ddp) <- all_names
    rownames(ddp) <- paste0(node_names, "_Delay_Params")
    
    
    ####
    
    
    var <- cbind(tm, sd, sdp, eq, iq, ddm, ddp)
    var <- rbind(var, matrix(NA, nrow = exit_number, ncol = ncol(var)))
    
    rownames(var) <- all_names
    colnames(var) <-
      c(
        all_names,
        "serv_dist",
        "serv_dist_param",
        "ext_queue",
        "int_queue",
        paste0(all_names, "_delay_dist"),
        paste0(all_names, "_delay_params")
      )
    
    var <- as.data.frame(var)
    
    var$serv_dist <-
      gsub(x = var$serv_dist,
           pattern = "Exponential",
           replacement = "exp")
    var$serv_dist <-
      gsub(x = var$serv_dist,
           pattern = "log-Normal",
           replacement = "lnorm")
    var$serv_dist <-
      gsub(x = var$serv_dist,
           pattern = "Uniform",
           replacement = "unif")
    var$serv_dist <-
      gsub(x = var$serv_dist,
           pattern = "Weibull",
           replacement = "weibull")
    var$serv_dist <-
      gsub(x = var$serv_dist,
           pattern = "Gamma",
           replacement = "gamma")
    
    var
  })
  
  
  #### Creates the trial Cal_input ####
  cal <- eventReactive(input$go, {
    x <- input$sp
    x <- unique(x)
    rownames(x) <- 1:nrow(x)
    x <- trimws(x = x, which = "both")
    x <- gsub(x = x, pattern = " ", "_")
    sp <- x[which(x != "")]
    
    x <- input$exit
    x <- unique(x)
    rownames(x) <- 1:nrow(x)
    x <- trimws(x = x, which = "both")
    x <- gsub(x = x, pattern = " ", "_")
    exit <- x[which(x != "")]
    
    node_number <- length(sp)
    exit_number <- length(exit)
    tabnames <- sp
    
    node_names <- sp
    
    exit_names <- exit
    
    all_names <- c(node_names, exit_names)
    
    ## External Arrival
    ea = lapply(1:node_number, function(i) {
      x <- as.data.frame(input[[paste0("ext_arr_", i)]])
      x <- head(x, -1)
      if (nrow(x) > 0) {
        x <- cbind("ext_arr", paste0(node_names[i]), x)
        colnames(x) <- c("metric", "node", "start", "end", "value")
      }
      x
    })
    
    ea_rows <- lapply(1:node_number, function(i) {
      nrow(ea[[i]])
    })
    
    if (!all(ea_rows == 0)) {
      eam <- rbindlist(ea[c(which(ea_rows > 0))])
    }
    
    
    
    
    ## Capacity
    cap = lapply(1:node_number, function(i) {
      x <- as.data.frame(input[[paste0("cap_", i)]])
      x <- head(x, -1)
      if (nrow(x) > 0) {
        x <- cbind("cap", paste0(node_names[i]), x)
        colnames(x) <- c("metric", "node", "start", "end", "value")
      }
      x
    })
    
    cap_rows <- lapply(1:node_number, function(i) {
      nrow(cap[[i]])
      
      
    })
    
    if (!all(cap_rows == 0)) {
      capm <- rbindlist(cap[c(which(cap_rows > 0))])
    }
    
    
    if (exists("eam") & exists("capm")) {
      cal <- rbind(eam, capm)
      colnames(cal) <- c("metric", "node", "start", "end", "value")
      cal
      
    } else if (exists("eam")) {
      cal <- eam
      colnames(cal) <- c("metric", "node", "start", "end", "value")
      cal
    } else if (exists("capm")) {
      cal <- capm
      colnames(cal) <- c("metric", "node", "start", "end", "value")
      cal
      
    } else{
      cal <-
        data.frame(
          "metric" = "",
          "node" = "",
          "start" = "",
          "end" = "",
          "value" = ""
        )
      cal
      
    }
  })
  
  #### Creates the Var_input visual####
  observeEvent(input$go, {
    output$var_view <- renderTable({
      var()
    }, rownames = TRUE, striped = T, bordered = T, align = "c", caption = "Network Information",
    caption.placement = getOption("xtable.caption.placement", "top"),
    caption.width = getOption("xtable.caption.width", NULL))
  })
  
  
  #### Creates the Cal_input visual####
  observeEvent(input$go, {
    output$cal_view <- renderTable({
      cal()
    }, rownames = FALSE, striped = T, bordered = T, align = "c", caption = "Calendar Information",
    caption.placement = getOption("xtable.caption.placement", "top"),
    caption.width = getOption("xtable.caption.width", NULL), digits = 5)
  })
  
  #### Creates the input checklist####
  
  observeEvent(input$go, {
    issues <- c()
    
    var <- var()
    cal <- cal()
    
    x <- input$sp
    x <- unique(x)
    rownames(x) <- 1:nrow(x)
    x <- trimws(x = x, which = "both")
    x <- gsub(x = x, pattern = " ", "_")
    sp <- x[which(x != "")]
    
    x <- input$exit
    x <- unique(x)
    rownames(x) <- 1:nrow(x)
    x <- trimws(x = x, which = "both")
    x <- gsub(x = x, pattern = " ", "_")
    exit <- x[which(x != "")]
    
    node_number <- length(sp)
    exit_number <- length(exit)
    tabnames <- sp
    
    node_names <- sp
    
    exit_names <- exit
    
    all_names <- c(node_names, exit_names)
    
    
    
    ### Testing if the transition matrix has rowsums of 1###
    
    f <- var[1:node_number, 1:length(all_names)]
    indx <- sapply(f, is.factor)
    f[indx] <-
      lapply(f[indx], function(x)
        as.numeric(as.character(x)))
    transition <- as.data.frame(f)
    
    
    
    if (sum(transition < 0) > 0 | sum(transition > 1) > 0) {
      issues <-
        c(issues, c(
          paste0("Network Input"),
          "All",
          paste(
            "Transition proportions contains value outside required range (replace with value between 0 and 1)",
            sep = ""
          )
        ))
      
    }
    
    
    
    rs <- rowSums(transition)
    
    for (i in 1:node_number) {
      x <- rs[i]
      
      if (is.na(x)) {
        issues <-
          c(issues, c(
            paste0("Network Input"),
            node_names[i],
            paste(
              "Transition row contains NA (replace with 0 or value)",
              sep = ""
            )
          ))
        
      } else if (!isTRUE(near(x, 1))) {
        issues <-
          c(issues, c(
            paste0("Network Input"),
            node_names[i],
            paste(
              "Transition proportion row does not sum to 1 (Currently:",
              x,
              ")",
              sep = ""
            )
          ))
      }
    }
    
    
    ### Testing if the distribution parameter inputs are correct ###
    f <-
      var[1:node_number, (length(all_names) + 1):(length(all_names) + 2)]
    indx <- sapply(f, is.factor)
    f[indx] <- lapply(f[indx], function(x)
      as.character(x))
    serv_dist_param <- as.data.frame(f)
    
    
    
    for (i in 1:node_number) {
      if (serv_dist_param[i, 1] == "exp") {
        x <- serv_dist_param[i, 2]
        
        if (is.na(x)) {
          issues <-
            c(
              issues,
              paste0("Network Input"),
              node_names[i],
              "Missing a service distribution parameter"
            )
          
        }
        
        if ((!is.na(x)) & x <= 0) {
          issues <-
            c(
              issues,
              paste0("Network Input"),
              node_names[i],
              "Service distribution parameter is not greater than 0"
            )
          
        }
        
      } else{
        x <- serv_dist_param[i, 2]
        x <- strsplit(x, ";")[[1]]
        
        if ("NA" %in% x) {
          issues <-
            c(
              issues,
              paste0("Network Input"),
              node_names[i],
              "Missing a service distribution parameter"
            )
          
        }
        
        if ((!("NA" %in% x)) &
            any(x <= 0) & serv_dist_param[i, 1] == "unif") {
          issues <-
            c(
              issues,
              paste0("Network Input"),
              node_names[i],
              "Service distribution parameters are not greater than 0"
            )
          
        }
        
        if ((!("NA" %in% x)) &
            any(x <= 0) & serv_dist_param[i, 1] == "gamma") {
          issues <-
            c(
              issues,
              paste0("Network Input"),
              node_names[i],
              "Service distribution parameters are not greater than 0"
            )
          
        }
        
        if ((!("NA" %in% x)) &
            any(x <= 0) & serv_dist_param[i, 1] == "weibull") {
          issues <-
            c(
              issues,
              paste0("Network Input"),
              node_names[i],
              "Service distribution parameters are not greater than 0"
            )
          
        }
        
        if ((!("NA" %in% x)) & x[2] < 0 &
            serv_dist_param[i, 1] == "lnorm") {
          issues <-
            c(
              issues,
              paste0("Network Input"),
              node_names[i],
              "lnorm service parameter (sdlog) is less than 0"
            )
          
        }
        
        
      }
    }
    
    
    #### Testing if the Queue inputs are correct #
    iq <- lapply(1:node_number , function(i) {
      input[[paste0("int_q_", i)]]
    })
    iq <- as.numeric(as.vector(unlist(iq)))
    
    
    eq <- lapply(1:node_number , function(i) {
      input[[paste0("ext_q_", i)]]
    })
    eq <- as.numeric(as.vector(unlist(eq)))
    
    for (i in 1:node_number) {
      x <- iq[i]
      
      if (is.na(x)) {
        issues <-
          c(issues, c(
            paste0("Network Input"),
            node_names[i],
            paste("Need to enter Internal Queue Value")
          ))
      }
      
      if (x %% 1 != 0) {
        issues <-
          c(issues, c(
            paste0("Network Input"),
            node_names[i],
            paste("Need to enter an integer Internal Queue Value")
          ))
      }
      
      if (x < 0) {
        issues <-
          c(issues, c(
            paste0("Network Input"),
            node_names[i],
            paste("Need to enter a positive Internal Queue Value")
          ))
      }
      
    }
    
    for (i in 1:node_number) {
      x <- eq[i]
      
      if (is.na(x)) {
        issues <-
          c(issues, c(
            paste0("Network Input"),
            node_names[i],
            paste("Need to enter External Queue Value")
          ))
      }
      
      if (x %% 1 != 0) {
        issues <-
          c(issues, c(
            paste0("Network Input"),
            node_names[i],
            paste("Need to enter an integer External Queue Value")
          ))
      }
      
      if (x < 0) {
        issues <-
          c(issues, c(
            paste0("Network Input"),
            node_names[i],
            paste("Need to enter a positive External Queue Value")
          ))
      }
      
    }
    
    
    
    ### Testing if the delay parameter inputs are correct ###
    
    f <-
      var[1:node_number, (length(all_names) + 5):((2 * length(all_names)) + 4)]
    indx <- sapply(f, is.factor)
    f[indx] <- lapply(f[indx], function(x)
      as.character(x))
    delay_dist <- as.data.frame(f)
    
    
    f <- var[1:node_number, (2 * length(all_names) + 5):ncol(var)]
    indx <- sapply(f, is.factor)
    f[indx] <- lapply(f[indx], function(x)
      as.character(x))
    delay_param <- as.data.frame(f)
    
    for (j in 1:length(all_names)) {
      for (i in 1:node_number) {
        if (!is.na(delay_dist[i, j])) {
          if (delay_dist[i, j] == "exp") {
            x <- delay_param[i, j]
            
            if (is.na(x)) {
              issues <-
                c(
                  issues,
                  paste0("Network Input"),
                  node_names[i],
                  "Missing a delay distribution parameter"
                )
              
            }
            
            if ((!is.na(x)) & x <= 0) {
              issues <-
                c(
                  issues,
                  paste0("Network Input"),
                  node_names[i],
                  "Delay parameter is not greater than 0"
                )
              
            }
            
          } else{
            x <- delay_param[i, j]
            x <- strsplit(x, ";")[[1]]
            
            if ("NA" %in% x) {
              issues <-
                c(
                  issues,
                  paste0("Network Input"),
                  node_names[i],
                  "Missing a delay distribution parameter"
                )
              
            }
            if ((!("NA" %in% x)) & any(x <= 0) &
                delay_dist[i, j] == "unif") {
              issues <-
                c(
                  issues,
                  paste0("Network Input"),
                  node_names[i],
                  "Delay distribution parameters are not greater than 0"
                )
              
            }
            
            if ((!("NA" %in% x)) &
                any(x <= 0) & delay_dist[i, j] == "gamma") {
              issues <-
                c(
                  issues,
                  paste0("Network Input"),
                  node_names[i],
                  "Delay distribution parameters are not greater than 0"
                )
              
            }
            
            if ((!("NA" %in% x)) &
                any(x <= 0) & delay_dist[i, j] == "weibull") {
              issues <-
                c(
                  issues,
                  paste0("Network Input"),
                  node_names[i],
                  "Delay distribution parameters are not greater than 0"
                )
              
            }
            
            if ((!("NA" %in% x)) & x[2] < 0 &
                delay_dist[i, j] == "lnorm") {
              issues <-
                c(
                  issues,
                  paste0("Network Input"),
                  node_names[i],
                  "lnorm service parameter (sdlog) is less than 0"
                )
              
            }
            
            
            
          }
        }
      }
    }
    
    
    ### Testing if there is at least 1 row of capacity and ext_arrival rate for each service point###
    
    row_test <- as.data.frame(cal[, 1:2])
    
    for (j in c("cap", "ext_arr")) {
      for (i in 1:node_number) {
        x <- row_test[which(row_test[, 1] == j), ]
        x <- x[which(x[, 2] == node_names[i]), ]
        
        
        if (nrow(x) == 0) {
          issues <-
            c(issues,
              "Calendar",
              node_names[i],
              paste0("Missing ", j, " input rows"))
          
        }
      }
    }
    
    ### Testing that every line in the caledar template has a value entry###
    
    value_test <- as.data.frame(cal)
    
    for (j in c("cap", "ext_arr")) {
      for (i in 1:node_number) {
        x <- value_test[which(value_test[, 1] == j), ]
        x <- x[which(x[, 2] == node_names[i]), 5]
        
        if (length(x) > 0) {
          if (any(is.na(x))) {
            issues <-
              c(
                issues,
                "Calendar",
                node_names[i],
                paste0("Missing ", j, " value entry in calendar")
              )
            
          }
          if (!any(is.na(x))) {
            if (any(x < 0)) {
              issues <-
                c(
                  issues,
                  "Calendar",
                  node_names[i],
                  paste0("Negative ", j, " value entry in calendar")
                )
            }
          }
          if (!any(is.na(x))) {
            if (j == "cap" & all(x == 0)) {
              issues <-
                c(
                  issues,
                  "Calendar",
                  node_names[i],
                  paste0("All zero ", j, " values entered in calendar")
                )
              
            }
          }
          
        }
      }
    }
    
    ### Testing that nodes that have 2+ lines in the calendar have any values in the start and end columns ###
    
    value_test <- as.data.frame(cal)
    
    for (j in c("cap", "ext_arr")) {
      for (i in 1:node_number) {
        x <- value_test[which(value_test[, 1] == j), ]
        x <- x[which(x[, 2] == node_names[i]), ]
        
        
        if (nrow(x) > 1) {
          start <- x[, 3]
          end <- x[, 4]
          
          if (any(is.na(start))) {
            issues <-
              c(
                issues,
                "Calendar",
                node_names[i],
                paste0("Missing start value(s) in ", j, " calendar")
              )
            
          }
          
          if (any(is.na(end))) {
            issues <-
              c(
                issues,
                "Calendar",
                node_names[i],
                paste0("Missing end value(s) in ", j, " calendar")
              )
            
          }
        }
      }
    }
    
    
    ### Testing that nodes that have a zero in the first start line in the calendar ###
    
    value_test <- as.data.frame(cal)
    
    
    for (j in c("cap", "ext_arr")) {
      for (i in 1:node_number) {
        x <- value_test[which(value_test[, 1] == j), ]
        x <- x[which(x[, 2] == node_names[i]), ]
        
        if (nrow(x) != 0) {
          if (!is.na(x[1, 3])) {
            start <- x[1, 3]
            if (start != 0) {
              issues <-
                c(
                  issues,
                  "Calendar",
                  node_names[i],
                  paste0("Non-Zero Initial Start Time in ", j, " calendar")
                )
            }
          }
          if (is.na(x[1, 3])) {
            issues <-
              c(
                issues,
                "Calendar",
                node_names[i],
                paste0("Non-Zero Initial Start Time in ", j, " calendar")
              )
          }
        }
      }
    }
    
    
    
    ### Testing that nodes that have 2+ lines in the calendar have matching values in the start and end columns ###
    
    value_test <- as.data.frame(cal)
    
    for (j in c("cap", "ext_arr")) {
      for (i in 1:node_number) {
        x <- value_test[which(value_test[, 1] == j), ]
        x <- x[which(x[, 2] == node_names[i]), ]
        
        
        if (nrow(x) > 1) {
          start <- x[, 3]
          end <- x[, 4]
          
          start_tail <- tail(start, -1)
          end_head <- head(end, -1)
          
          start_tail[is.na(start_tail)] <- 0
          end_head[is.na(end_head)] <- 0
          
          
          
          if (any(!(start_tail == end_head))) {
            issues <-
              c(
                issues,
                "Calendar",
                node_names[i],
                paste0(
                  "Start & End values don't match up sequentially in ",
                  j,
                  " calendar"
                )
              )
            
          }
        }
      }
    }
    
    ### Testing that nodes that have ascending start and end values ###
    
    value_test <- as.data.frame(cal)
    
    for (j in c("cap", "ext_arr")) {
      for (i in 1:node_number) {
        x <- value_test[which(value_test[, 1] == j), ]
        x <- x[which(x[, 2] == node_names[i]), ]
        
        
        if (nrow(x) > 1) {
          start <- x[, 3]
          end <- x[, 4]
          
          if (!any(is.na(start))) {
            if (any(diff(start) <= 0)) {
              issues <-
                c(
                  issues,
                  "Calendar",
                  node_names[i],
                  paste0(
                    "Start values don't increase sequentially in ",
                    j,
                    " calendar"
                  )
                )
            }
          }
          if (!any(is.na(end))) {
            if (any(diff(end) <= 0)) {
              issues <-
                c(
                  issues,
                  "Calendar",
                  node_names[i],
                  paste0(
                    "End values don't increase sequentially in ",
                    j,
                    " calendar"
                  )
                )
            }
          }
          
          
          
        }
      }
    }
    
    
    ### Testing that there are arrivals to at least one node ###
    
    value_test <- as.data.frame(cal)
    
    x <- value_test[which(value_test[, 1] == "ext_arr"), 5]
    if (!any(is.na(x))) {
      if (all(x == 0)) {
        issues <-
          c(
            issues,
            "Calendar",
            "All",
            paste0(
              "No Arrival rates to any service point in the ext_arr calendar"
            )
          )
        
        
      }
    }
    
    
    
    
    ####
    
    
    
    output$issues <- renderTable({
      if (length(issues) == 0) {
        issues <- c("Complete", "Complete", "Complete")
      }
      issues <- matrix(data = issues,
                       ncol = 3,
                       byrow = T)
      
      colnames(issues) <- c("Location", "Service Point", "Issue")
      
      issues
    }, striped = T, bordered = T, align = "c", caption = '<font size=4 color="red"><strong><p>Issues Log</p></strong></font>',
    caption.placement = getOption("xtable.caption.placement", "top"),
    caption.width = getOption("xtable.caption.width", NULL))
    
    
    
    output$means <- renderTable({
      if (length(issues) == 0) {
        var <- var()
        cal <- cal()
        
        mean_table <- c()
        
        x <- input$sp
        x <- unique(x)
        rownames(x) <- 1:nrow(x)
        x <- trimws(x = x, which = "both")
        x <- gsub(x = x, pattern = " ", "_")
        sp <- x[which(x != "")]
        
        node_number <- length(sp)
        
        node_names <- sp
        
        
        for (i in 1:node_number) {
          pars <-
            as.numeric(unlist(strsplit(
              as.character(var$serv_dist_param[i]), ";"
            )))
          
          if (var$serv_dist[i] == "exp") {
            mean_table <- c(mean_table, 1 / pars)
            
          } else if (var$serv_dist[i] == "unif") {
            mean_table <- c(mean_table, (pars[1] + pars[2]) / 2)
            
          } else if (var$serv_dist[i] == "lnorm") {
            mean_table <- c(mean_table, exp(pars[1] + 0.5 * (pars[2]) ^ 2))
            
          } else if (var$serv_dist[i] == "weibull") {
            mean_table <- c(mean_table, pars[2] * (gamma(1 + 1 / pars[1])))
            
          } else if (var$serv_dist[i] == "gamma") {
            mean_table <- c(mean_table, pars[1] / pars[2])
          } else{
            mean_table <- c(mean_table, c("Error in Mean Calculation"))
          }
        }
        
        mean_table <- as.data.frame(mean_table)
        rownames(mean_table) <- node_names
        colnames(mean_table) <- "Mean Length of Service"
        mean_table
      }
    }, striped = T, bordered = T, align = "c", rownames = T, caption = "LoS Means",
    caption.placement = getOption("xtable.caption.placement", "top"),
    caption.width = getOption("xtable.caption.width", NULL))
    
    
    
    
    output$download_buttons <- renderUI({
      if (length(issues) == 0) {
        fluidRow(
          column(
            6,
            align = "center",
            downloadButton(
              outputId = "var_dl",
              label = "Network Template Download",
              style = 'padding:16px; font-size:110%'
            )
          ),
          column(
            6,
            align = "center",
            downloadButton(
              outputId = "cal_dl",
              label = "Calendar Download",
              style = 'padding:16px; font-size:110%'
            )
          )
        )
      }
    })
    
    output$j2st <- renderUI({
      if (length(issues) == 0) {
        column(6, align = "center", actionButton(inputId = "j2PSR", label = c(
          tagList("Move to Simulation Tool", icon("arrow-right"))
        )))
      }
    })
  })
  
  
  
  
  #### Creates the wizard template downloader####
  
  ### Creates the Var_input downloader###
  output$var_dl <- downloadHandler(
    filename = "var_input.csv",
    content = function(file) {
      write.csv(var(), file, row.names = TRUE)
    }
  )
  ### Creates the cal_input downloader###
  output$cal_dl <- downloadHandler(
    filename = "cal_input.csv",
    content = function(file) {
      write.csv(cal(), file, row.names = FALSE)
    }
  )
  
  
  #### Length of Service Model Fit Tab####
  observeEvent(input$go_distfit, {
    req(input$los_dat)
    df <- read.csv(input$los_dat$datapath,
                   header = F,
                   sep = ",")
    
    
    if (is.numeric(df[, 1])) {
      colnames(df) <- "data"
      
      fe <- fitdist(data = df$data, distr = "exp")
      fl <- fitdist(data = df$data, distr = "lnorm")
      fu <- fitdist(data = df$data, distr = "unif")
      fw <- fitdist(data = df$data, distr = "weibull")
      fg <- fitdist(data = df$data, distr = "gamma")
      
      
      output$los_plot <- renderPlot({
        plotdist(df$data, histo = T, demp = T)
      }, res = 128)
      
      output$los_cf <- renderPlot({
        descdist(df$data, boot = 100)
        
      }, res = 128)
      
      output$los_fit_plot <- renderPlot({
        p <-
          denscomp(
            ft = list(fe, fl, fu, fw, fg),
            plotstyle = "ggplot",
            breaks = 100,
            #fitcol = c("#009E73","#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
            fitlty = 1
          )
        p <- p + theme_bw()
        p
        
      }, res = 128)
      
      
      output$los_text <- renderText({
        c("Ranked Model Table")
        
        
      })
      
      output$los_text_help <- renderText({
        c(
          "The distributions below have been ranked in terms of best fit. The Rank 1 Distribution was found to fit closest to the provided data.
      Simply use the top ranking model and enter the details in the data entry tab.
      If the exponential distribution is the highest ranking, then there is only one parameter to copy across (rate), else there will be two. These are named in the table and should be copied
      to the relevant box on the data entry page. If the histogram appears completely flat, it may be that the uniform distribution is the best fitting model. In this case, ignore the rankings and take the parameters from that row.
      In the case where multiple distributions are found to have the same fit, some model fit lines may be obscured on the plot (i.e. plotting over eachother). These models will still be ranked but should be treated as ranking equally.
          "
        )
        
      })
      
      
      output$los_fit_table <- renderTable({
        fes <- summary(fe)
        fls <- summary(fl)
        fus <- summary(fu)
        fws <- summary(fw)
        fgs <- summary(fg)
        aic = c(fes$aic, fls$aic, fus$aic, fws$aic, fgs$aic)
        del_aic <- aic - min(aic, na.rm = T)
        aic_lik <- exp(-0.5 * del_aic)
        aic_weight <- aic_lik / sum(aic_lik, na.rm = T)
        
        means <-
          c((1 / fes$estimate[1]),
            (exp(fls$estimate[1] + (
              0.5 * (fls$estimate[2]) ^ 2
            ))),
            (0.5 * (fus$estimate[1] + fus$estimate[2])),
            (fws$estimate[2] * gamma(1 + 1 / fws$estimate[1])),
            (fgs$estimate[1] / fgs$estimate[2])
          )
        means <- unname(means)
        mean_dif <- means - mean(df$data)
        
        fit_table <- data.frame(
          "Distribution" = c(
            "exponential",
            "log-normal",
            "uniform",
            "weibull",
            "gamma"
          ),
          
          
          "Parameter 1 Name" = c(
            names(fes$estimate)[1],
            names(fls$estimate)[1],
            names(fus$estimate)[1],
            names(fws$estimate)[1],
            names(fgs$estimate)[1]
          ),
          "Parameter 1 Value" = c(
            fes$estimate[1],
            fls$estimate[1],
            fus$estimate[1],
            fws$estimate[1],
            fgs$estimate[1]
          ),
          "Parameter 2 Name" = c(
            names(fes$estimate)[2],
            names(fls$estimate)[2],
            names(fus$estimate)[2],
            names(fws$estimate)[2],
            names(fgs$estimate)[2]
          ),
          "Parameter 2 Value" = c(
            fes$estimate[2],
            fls$estimate[2],
            fus$estimate[2],
            fws$estimate[2],
            fgs$estimate[2]
          ),
          "AIC Score" = c(ceiling(aic)),
          "AIC Weight" = c(100 * signif(aic_weight, digits = 3)),
          "Mean" = means,
          "Diff from actual mean" = signif(mean_dif, digits = 3),
          row.names = NULL
        )
        
        #rownames(fit_table)<-c()
        fit_table <-
          fit_table[order(fit_table$AIC.Weight,
                          decreasing = T,
                          na.last = T), ]
        fit_table <- cbind("Rank" = 1:5, fit_table)
        fit_table[which(fit_table$Distribution == "uniform"), c(7, 8)] <-
          "Check Graph for fit"
        colnames(fit_table) <-
          c(
            "Rank",
            "Distribution",
            "Parameter 1 Name",
            "Parameter 1 Value",
            "Parameter 2 Name",
            "Parameter 2 Value",
            "AIC Score",
            "AIC Weight (/100)",
            "Estiamted Mean",
            "Diff from data mean"
          )
        fit_table <- fit_table[, -c(7, 8, 9, 10)]
        fit_table
      }, striped = T, bordered = T, align = "c")
      
      output$fit_error <- renderText({
        c("")
        
      })
      
      
      output$mini_summary <- renderTable({
        mini_summary <-
          data.frame(
            "Metric" = c(
              "Mean",
              "Standard Deviation",
              "Inter-quartile range",
              "90th Percentile"
            ),
            "Value" = c(
              mean(df$data),
              sd(df$data),
              IQR(df$data),
              quantile(df$data, probs = c(0.9))
            )
          )
        mini_summary
        
      }, striped = T, bordered = T, align = "c", caption = "Uploaded Data",
      caption.placement = getOption("xtable.caption.placement", "top"),
      caption.width = getOption("xtable.caption.width", NULL))
      
      
    } else{
      output$fit_error <- renderText({
        c(
          "Error: Ensure that the uploaded file is a csv, has only one column of numbers (No Header Required) and that they are located in the leftmost column"
        )
        
      })
      
      output$los_fit_plot <- renderPlot({
      })
      
      output$los_text <- renderText({
      })
      
      output$los_text_help <- renderText({
      })
      
      output$los_fit_table <- renderTable({
      })
      
      output$lmini_summary <- renderTable({
      })
    }
  })
  
  #### Length of Service Scaled Means Tab####
  
  #LOS distriubtion dataframe ####
  #reads in pre-caculated values from csv stored in www folder
  #mostly calcuated by interval-censored maximum likelihood distriubtion fitting on HES
    #data, with candidate distrubtion chosen by AIC
  #But with some fitted to non-interval censored regional data (where HES fits did not
    #coverge or were otherwise unavailable). n.b the HES method is NOT the same as that
    #in the "fit your own" data tab, which assumes uncensored data
  pre_fitted_data <- read.csv("./www/fits_for_pathsimr.csv",
                              check.names = FALSE) %>% 
    arrange(Names)
  
  output$treatment_select_ui <- renderUI({
    
    x <- as.character(pre_fitted_data$Names)

    
    selectInput(
      inputId = "treatment_select",
      label = "Service Point Library",
      choices = x,
      selected = x[1],
      selectize = F,
      width = '150%'
    )

  })
  
  
  
  observeEvent(input$go_scaled_fit, {
    table <- pre_fitted_data
    
    
    
    req(input$treatment_mean)
    
    df <-
      #as.data.frame(subset(table, table$Names == input$treatment_select))
      filter(table,Names == input$treatment_select)
    
    if (df$Distribution == "exponential") {
      df$`Parameter 1 Value` <- 1 / input$treatment_mean
      
      df$`Parameter 1 Value` <-
        as.character(signif(df$`Parameter 1 Value`, digits = 5))
      
      
    } else if (df$Distribution == "log-normal") {
      df$`Parameter 1 Value` <-
        log(input$treatment_mean) - 0.5 * (df$`Parameter 2 Value`) ^ 2
      
      df$`Parameter 1 Value` <-
        as.character(signif(df$`Parameter 1 Value`, digits = 5))
      df$`Parameter 2 Value` <-
        as.character(signif(df$`Parameter 2 Value`, digits = 5))
      
      
      
    } else if (df$Distribution == "gamma") {
      df$`Parameter 2 Value` <- df$`Parameter 1 Value` / input$treatment_mean
      
      df$`Parameter 1 Value` <-
        as.character(signif(df$`Parameter 1 Value`, digits = 5))
      df$`Parameter 2 Value` <-
        as.character(signif(df$`Parameter 2 Value`, digits = 5))
      
      
      
    } else if (df$Distribution == "weibull") {
      df$`Parameter 2 Value` <-
        input$treatment_mean / gamma(1 + (1 / df$`Parameter 1 Value`))
      
      df$`Parameter 1 Value` <-
        as.character(signif(df$`Parameter 1 Value`, digits = 5))
      df$`Parameter 2 Value` <-
        as.character(signif(df$`Parameter 2 Value`, digits = 5))
      
      
      
    }
    
    
    output$scaled_fit <- renderTable({
      df
      
    }, rownames = FALSE, striped = T, bordered = T, align = "c")
    
    
    output$scaled_fit_plot <- renderPlot({
      t_mean <- input$treatment_mean
      
      if (df$Distribution == "exponential") {
        x <- seq(0, (10 * as.numeric(t_mean)), length.out = 1000)
        y <- dexp(x, rate = as.numeric(df$`Parameter 1 Value`))
        dat = data.frame("Time" = x, "Probability" = y)
        
        ggplot(data = dat) + geom_line(aes(x = Time, y = Probability),
                                       size = 1,
                                       col = "blue") + theme_bw()
        
        
        
      } else if (df$Distribution == "log-normal") {
        x <- seq(0, (10 * as.numeric(t_mean)), length.out = 1000)
        y <-
          dlnorm(
            x,
            meanlog = as.numeric(df$`Parameter 1 Value`),
            sdlog = as.numeric(df$`Parameter 2 Value`)
          )
        dat = data.frame("Time" = x, "Probability" = y)
        
        ggplot(data = dat) + geom_line(aes(x = Time, y = Probability),
                                       size = 1,
                                       col = "blue") + theme_bw()
        
        
        
        
        
      } else if (df$Distribution == "gamma") {
        x <- seq(0, (10 * as.numeric(t_mean)), length.out = 1000)
        y <-
          dgamma(
            x,
            shape = as.numeric(df$`Parameter 1 Value`),
            rate = as.numeric(df$`Parameter 2 Value`)
          )
        dat = data.frame("Time" = x, "Probability" = y)
        
        ggplot(data = dat) + geom_line(aes(x = Time, y = Probability),
                                       size = 1,
                                       col = "blue") + theme_bw()
        
        
        
      } else if (df$Distribution == "weibull") {
        x <- seq(0, (10 * as.numeric(t_mean)), length.out = 1000)
        y <-
          dweibull(
            x,
            shape = as.numeric(df$`Parameter 1 Value`),
            scale = as.numeric(df$`Parameter 2 Value`)
          )
        dat = data.frame("Time" = x, "Probability" = y)
        
        ggplot(data = dat) + geom_line(aes(x = Time, y = Probability),
                                       size = 1,
                                       col = "blue") + theme_bw()
        
        
        
      }
      
    }, res = 128)
    
    
    
  })
  
  
  
  
  
  ######END OF WIZARD#######
  ###### START OF SIMULATION TOOL##########
  
  ####Template upload and checks ####
  output$contents1 <- renderTable({
    if (input$disp1 == TRUE) {
      if (input$w_temp == 0) {
        req(input$file1)
        df <- read.csv(input$file1$datapath,
                       header = TRUE,
                       sep = ",")
        rownames(df) <- df[, 1]
        df <- df[, -1]
        colnames(df)[1:(which(colnames(df) == "serv_dist") - 1)] <-
          rownames(df)
        df
        
      } else{
        var()
      }
      
      
      
    }
  }, rownames = TRUE, caption = "Variable Inputs",
  caption.placement = getOption("xtable.caption.placement", "top"),
  caption.width = getOption("xtable.caption.width", NULL))
  
  
  output$contents2 <- renderTable({
    if (input$disp2 == TRUE) {
      if (input$w_temp == 0) {
        req(input$file2)
        df <- read.csv(input$file2$datapath,
                       header = TRUE,
                       sep = ",")
        df
        
      } else{
        cal()
        
      }
    }
  }, caption = "Calendar Inputs",
  caption.placement = getOption("xtable.caption.placement", "top"),
  caption.width = getOption("xtable.caption.width", NULL))
  
  
  
  
  issues <- eventReactive(input$go_viz, {
    req(input$file1)
    req(input$file2)
    
    df <- read.csv(input$file1$datapath,
                   header = TRUE,
                   sep = ",")
    
    rownames(df) <- df[, 1]
    df <- df[, -1]
    colnames(df)[1:(which(colnames(df) == "serv_dist") - 1)] <-
      rownames(df)
    
    
    df2 <- read.csv(input$file2$datapath,
                    header = TRUE,
                    sep = ",")
    
    issues <- c()
    
    var <- df
    cal <- df2
    
    x <- rownames(var[which(!is.na(var[, 1])), ])
    x <- unique(x)
    #rownames(x)<-1:nrow(x)
    x <- trimws(x = x, which = "both")
    x <- gsub(x = x, pattern = " ", "_")
    sp <- x[which(x != "")]
    
    x <- rownames(var[which(is.na(var[, 1])), ])
    x <- unique(x)
    #rownames(x)<-1:nrow(x)
    x <- trimws(x = x, which = "both")
    x <- gsub(x = x, pattern = " ", "_")
    exit <- x[which(x != "")]
    
    node_number <- length(sp)
    exit_number <- length(exit)
    
    node_names <- sp
    
    exit_names <- exit
    
    all_names <- c(node_names, exit_names)
    
    
    
    
    ### Testing if the names match between templates###
    
    cal_names <- unique(cal$node)
    
    
    if (length(node_names) != length(cal_names) |
        any(!(node_names %in% cal_names))) {
      issues <-
        c(issues,
          c(
            paste0("Network & Cal input"),
            "All",
            "Service point names do not match between templates"
          ))
      
    }
    
    
    
    
    
    ### Testing if the transition matrix has rowsums of 1###
    
    f <- var[1:node_number, 1:length(all_names)]
    indx <- sapply(f, is.factor)
    f[indx] <-
      lapply(f[indx], function(x)
        as.numeric(as.character(x)))
    transition <- as.data.frame(f)
    
    
    if (sum(transition < 0) > 0 | sum(transition > 1) > 0) {
      issues <-
        c(issues, c(
          paste0("Network Input"),
          "All",
          paste(
            "Transition matrix contains value outside required range (replace with value between 0 and 1)",
            sep = ""
          )
        ))
      
    }
    
    rs <- rowSums(transition)
    
    for (i in 1:node_number) {
      x <- rs[i]
      
      if (is.na(x)) {
        issues <-
          c(issues, c(
            paste0("Network Input"),
            node_names[i],
            paste(
              "Transition row contains NA (replace with 0 or value)",
              sep = ""
            )
          ))
        
      } else if (!isTRUE(near(x, 1))) {
        issues <-
          c(issues, c(
            paste0("Network Input"),
            node_names[i],
            paste("Row sum does not equal 1 (Currently:", x, ")", sep = "")
          ))
      }
    }
    
    
    
    ### Testing if the distribution parameter inputs are correct ###
    f <-
      var[1:node_number, (length(all_names) + 1):(length(all_names) + 2)]
    indx <- sapply(f, is.factor)
    f[indx] <- lapply(f[indx], function(x)
      as.character(x))
    serv_dist_param <- as.data.frame(f)
    
    
    
    for (i in 1:node_number) {
      x <- serv_dist_param[i, 1]
      
      if (is.na(x)) {
        issues <-
          c(
            issues,
            paste0("Network Input"),
            node_names[i],
            "Missing a service distribution"
          )
        
      }
      
    }
    
    ### Testing if the distribution parameter inputs are correct ###
    f <-
      var[1:node_number, (length(all_names) + 1):(length(all_names) + 2)]
    indx <- sapply(f, is.factor)
    f[indx] <- lapply(f[indx], function(x)
      as.character(x))
    serv_dist_param <- as.data.frame(f)
    
    
    
    for (i in 1:node_number) {
      if (serv_dist_param[i, 1] == "exp") {
        x <- serv_dist_param[i, 2]
        
        if (is.na(x)) {
          issues <-
            c(
              issues,
              paste0("Network Input"),
              node_names[i],
              "Missing a service distribution parameter"
            )
          
        }
        
        if ((!is.na(x)) & x <= 0) {
          issues <-
            c(
              issues,
              paste0("Network Input"),
              node_names[i],
              "Service distribution parameter is not greater than 0"
            )
          
        }
        
      } else{
        x <- serv_dist_param[i, 2]
        x <- strsplit(x, ";")[[1]]
        
        if ("NA" %in% x) {
          issues <-
            c(
              issues,
              paste0("Network Input"),
              node_names[i],
              "Missing a service distribution parameter"
            )
          
        }
        
        if ((!("NA" %in% x)) &
            any(x <= 0) & serv_dist_param[i, 1] == "unif") {
          issues <-
            c(
              issues,
              paste0("Network Input"),
              node_names[i],
              "Service distribution parameters are not greater than 0"
            )
          
        }
        
        if ((!("NA" %in% x)) &
            any(x <= 0) & serv_dist_param[i, 1] == "gamma") {
          issues <-
            c(
              issues,
              paste0("Network Input"),
              node_names[i],
              "Service distribution parameters are not greater than 0"
            )
          
        }
        
        if ((!("NA" %in% x)) &
            any(x <= 0) & serv_dist_param[i, 1] == "weibull") {
          issues <-
            c(
              issues,
              paste0("Network Input"),
              node_names[i],
              "Service distribution parameters are not greater than 0"
            )
          
        }
        
        # if((!("NA" %in% x))&x[2]<0&serv_dist_param[i,1]=="lnorm"){
        #
        #   issues<-c(issues,paste0("Network Input"),node_names[i],"lnorm service parameter (sdlog) is less than 0")
        #
        # }
      }
    }
    
    
    #### Testing if the Queue inputs are correct #
    iq <- var$int_queue
    
    
    eq <- var$ext_queue
    
    for (i in 1:node_number) {
      x <- iq[i]
      
      if (x == Inf) {
        x = 9999
      }
      
      if (is.na(x)) {
        issues <-
          c(issues, c(
            paste0("Network Input"),
            node_names[i],
            paste("Need to enter Internal Queue Value")
          ))
      }
      
      
      if (as.numeric(x) %% 1 != 0) {
        issues <-
          c(issues, c(
            paste0("Network Input"),
            node_names[i],
            paste("Need to enter an integer Internal Queue Value")
          ))
      }
      
      if (as.numeric(x) < 0) {
        issues <-
          c(issues, c(
            paste0("Network Input"),
            node_names[i],
            paste("Need to enter a positive Internal Queue Value")
          ))
      }
      
      
    }
    
    for (i in 1:node_number) {
      x <- eq[i]
      
      if (x == Inf) {
        x = 9999
      }
      
      if (is.na(x)) {
        issues <-
          c(issues, c(
            paste0("Network Input"),
            node_names[i],
            paste("Need to enter External Queue Value")
          ))
      }
      
      if (as.numeric(x) %% 1 != 0) {
        issues <-
          c(issues, c(
            paste0("Network Input"),
            node_names[i],
            paste("Need to enter an integer External Queue Value")
          ))
      }
      
      if (as.numeric(x) < 0) {
        issues <-
          c(issues, c(
            paste0("Network Input"),
            node_names[i],
            paste("Need to enter a positive External Queue Value")
          ))
      }
      
    }
    
    
    
    
    ### Testing if the delay distribution inputs are correct ###
    
    f <-
      var[1:node_number, (length(all_names) + 5):((2 * length(all_names)) + 4)]
    indx <- sapply(f, is.factor)
    f[indx] <- lapply(f[indx], function(x)
      as.character(x))
    delay_dist <- as.data.frame(f)
    
    
    f <- var[1:node_number, (2 * length(all_names) + 5):ncol(var)]
    indx <- sapply(f, is.factor)
    f[indx] <- lapply(f[indx], function(x)
      as.character(x))
    delay_param <- as.data.frame(f)
    
    for (j in 1:length(all_names)) {
      for (i in 1:node_number) {
        if (!is.na(delay_param[i, j])) {
          x <- delay_dist[i, j]
          
          if (is.na(x)) {
            issues <-
              c(
                issues,
                paste0("Network Input"),
                node_names[i],
                "Missing a delay distribution "
              )
          }
        }
      }
    }
    
    
    ### Testing if the delay parameter inputs are correct ###
    
    f <-
      var[1:node_number, (length(all_names) + 5):((2 * length(all_names)) + 4)]
    indx <- sapply(f, is.factor)
    f[indx] <- lapply(f[indx], function(x)
      as.character(x))
    delay_dist <- as.data.frame(f)
    
    
    f <- var[1:node_number, (2 * length(all_names) + 5):ncol(var)]
    indx <- sapply(f, is.factor)
    f[indx] <- lapply(f[indx], function(x)
      as.character(x))
    delay_param <- as.data.frame(f)
    
    for (j in 1:length(all_names)) {
      for (i in 1:node_number) {
        if (!is.na(delay_dist[i, j])) {
          if (delay_dist[i, j] == "exp") {
            x <- delay_param[i, j]
            
            if (is.na(x)) {
              issues <-
                c(
                  issues,
                  paste0("Network Input"),
                  node_names[i],
                  "Missing a delay distribution parameter"
                )
              
            }
            if ((!is.na(x)) & x <= 0) {
              issues <-
                c(
                  issues,
                  paste0("Network Input"),
                  node_names[i],
                  "Delay parameter is not greater than 0"
                )
              
            }
            
          } else{
            x <- delay_param[i, j]
            x <- strsplit(x, ";")[[1]]
            
            if ("NA" %in% x) {
              issues <-
                c(
                  issues,
                  paste0("Network Input"),
                  node_names[i],
                  "Missing a delay distribution parameter"
                )
              
            }
            
            if ((!("NA" %in% x)) & any(x <= 0) &
                delay_dist[i, j] == "unif") {
              issues <-
                c(
                  issues,
                  paste0("Network Input"),
                  node_names[i],
                  "Delay distribution parameters are not greater than 0"
                )
              
            }
            
            if ((!("NA" %in% x)) &
                any(x <= 0) & delay_dist[i, j] == "gamma") {
              issues <-
                c(
                  issues,
                  paste0("Network Input"),
                  node_names[i],
                  "Delay distribution parameters are not greater than 0"
                )
              
            }
            
            if ((!("NA" %in% x)) &
                any(x <= 0) & delay_dist[i, j] == "weibull") {
              issues <-
                c(
                  issues,
                  paste0("Network Input"),
                  node_names[i],
                  "Delay distribution parameters are not greater than 0"
                )
              
            }
            
            # if((!("NA" %in% x))&x[2]<0&delay_dist[i,j]=="lnorm"){
            #
            #   issues<-c(issues,paste0("Network Input"),node_names[i],"lnorm service parameter (sdlog) is less than 0")
            #
            # }
            
            
          }
        }
      }
    }
    
    
    ### Testing if there is at least 1 row of capacity and ext_arrival rate for each service point###
    
    row_test <- as.data.frame(cal[, 1:2])
    
    for (j in c("cap", "ext_arr")) {
      for (i in 1:node_number) {
        x <- row_test[which(row_test[, 1] == j), ]
        x <- x[which(x[, 2] == node_names[i]), ]
        
        
        if (nrow(x) == 0) {
          issues <-
            c(issues,
              "Calendar",
              node_names[i],
              paste0("Missing ", j, " input rows"))
          
        }
      }
    }
    
    ### Testing that every line in the calendar template has a value entry###
    
    value_test <- as.data.frame(cal)
    
    for (j in c("cap", "ext_arr")) {
      for (i in 1:node_number) {
        x <- value_test[which(value_test[, 1] == j), ]
        x <- x[which(x[, 2] == node_names[i]), 5]
        
        if (length(x) > 0) {
          if (any(is.na(x))) {
            issues <-
              c(
                issues,
                "Calendar",
                node_names[i],
                paste0("Missing ", j, " value entry in calendar")
              )
            
          }
          if (!any(is.na(x))) {
            if (any(x < 0)) {
              issues <-
                c(
                  issues,
                  "Calendar",
                  node_names[i],
                  paste0("Negative ", j, " value entry in calendar")
                )
            }
          }
          if (!any(is.na(x))) {
            if (j == "cap" & all(x == 0)) {
              issues <-
                c(
                  issues,
                  "Calendar",
                  node_names[i],
                  paste0("All zero ", j, " values entered in calendar")
                )
              
            }
          }
          
        }
      }
    }
    
    ### Testing that nodes that have 2+ lines in the calendar have any values in the start and end columns ###
    
    value_test <- as.data.frame(cal)
    
    for (j in c("cap", "ext_arr")) {
      for (i in 1:node_number) {
        x <- value_test[which(value_test[, 1] == j), ]
        x <- x[which(x[, 2] == node_names[i]), ]
        
        
        if (nrow(x) > 1) {
          start <- x[, 3]
          end <- x[, 4]
          
          if (any(is.na(start))) {
            issues <-
              c(
                issues,
                "Calendar",
                node_names[i],
                paste0("Missing start value(s) in ", j, " calendar")
              )
            
          }
          
          if (any(is.na(end))) {
            issues <-
              c(
                issues,
                "Calendar",
                node_names[i],
                paste0("Missing end value(s) in ", j, " calendar")
              )
            
          }
        }
      }
    }
    
    ### Testing that nodes that have a zero in the first start line in the calendar ###
    
    value_test <- as.data.frame(cal)
    
    
    for (j in c("cap", "ext_arr")) {
      for (i in 1:node_number) {
        x <- value_test[which(value_test[, 1] == j), ]
        x <- x[which(x[, 2] == node_names[i]), ]
        
        if (nrow(x) != 0) {
          if (!is.na(x[1, 3])) {
            start <- x[1, 3]
            if (start != 0) {
              issues <-
                c(
                  issues,
                  "Calendar",
                  node_names[i],
                  paste0("Non-Zero Initial Start Time in ", j, " calendar")
                )
            }
          }
          if (is.na(x[1, 3])) {
            issues <-
              c(
                issues,
                "Calendar",
                node_names[i],
                paste0("Non-Zero Initial Start Time in ", j, " calendar")
              )
          }
        }
      }
    }
    
    
    ### Testing that nodes that have 2+ lines in the calendar have matching values in the start and end columns ###
    
    value_test <- as.data.frame(cal)
    
    for (j in c("cap", "ext_arr")) {
      for (i in 1:node_number) {
        x <- value_test[which(value_test[, 1] == j), ]
        x <- x[which(x[, 2] == node_names[i]), ]
        
        
        if (nrow(x) > 1) {
          start <- x[, 3]
          end <- x[, 4]
          
          start_tail <- tail(start, -1)
          end_head <- head(end, -1)
          
          start_tail[is.na(start_tail)] <- 0
          end_head[is.na(end_head)] <- 0
          
          
          
          if (any(!(start_tail == end_head))) {
            issues <-
              c(
                issues,
                "Calendar",
                node_names[i],
                paste0(
                  "Start & End values don't match up sequentially in ",
                  j,
                  " calendar"
                )
              )
            
          }
        }
      }
    }
    
    
    
    ### Testing that nodes that have ascending start and end values ###
    
    value_test <- as.data.frame(cal)
    
    for (j in c("cap", "ext_arr")) {
      for (i in 1:node_number) {
        x <- value_test[which(value_test[, 1] == j), ]
        x <- x[which(x[, 2] == node_names[i]), ]
        
        
        if (nrow(x) > 1) {
          start <- x[, 3]
          end <- x[, 4]
          
          if (!any(is.na(start))) {
            if (any(diff(start) <= 0)) {
              issues <-
                c(
                  issues,
                  "Calendar",
                  node_names[i],
                  paste0(
                    "Start values don't increase sequentially in ",
                    j,
                    " calendar"
                  )
                )
            }
          }
          if (!any(is.na(end))) {
            if (any(diff(end) <= 0)) {
              issues <-
                c(
                  issues,
                  "Calendar",
                  node_names[i],
                  paste0(
                    "End values don't increase sequentially in ",
                    j,
                    " calendar"
                  )
                )
            }
          }
          
          
          
        }
      }
    }
    
    ### Testing that there are arrivals to at least one node ###
    
    value_test <- as.data.frame(cal)
    
    x <- value_test[which(value_test[, 1] == "ext_arr"), 5]
    if (!any(is.na(x))) {
      if (all(x == 0)) {
        issues <-
          c(
            issues,
            "Calendar",
            "All",
            paste0(
              "No Arrival rates to any service point in the ext_arr calendar"
            )
          )
        
        
      }
    }
    
    
    
    if (length(issues) == 0) {
      issues <- c("Complete", "Complete", "Complete")
    }
    
    issues <- matrix(data = issues,
                     ncol = 3,
                     byrow = T)
    
    colnames(issues) <- c("Location", "Service Point", "Issue")
    
    issues
    
  })
  
  output$file_check_issues <- renderTable({
    issues <- issues()
    issues
    
  }, striped = T, bordered = T, align = "c", caption = "Issues Log",
  caption.placement = getOption("xtable.caption.placement", "top"),
  caption.width = getOption("xtable.caption.width", NULL))
  
  
  
  
  #### NETWORK VISUALISATION ####
  viz <- eventReactive(input$go_viz, {
    if (input$w_temp == 0) {
      req(input$file1)
      req(input$file2)
      
      var_input <-
        read.csv(input$file1$datapath,
                 header = TRUE,
                 sep = ",")
      rownames(var_input) <- var_input[, 1]
      var_input <- var_input[, -1]
      
      cal_input <-
        read.csv(input$file2$datapath,
                 header = TRUE,
                 sep = ",")
      
      issues <- issues()
      
      req(issues[1, 1] == "Complete")
      
    } else{
      var_input <- var()
      
      var_input <- as.data.frame(var_input)
      
      f <- var_input
      indx <- sapply(f, is.factor)
      f[indx] <- lapply(f[indx], function(x)
        as.character(x))
      var_input <- as.data.frame(f)
      
      f <- var_input[, 1:nrow(var_input)]
      indx <- 1:nrow(var_input)
      f[, indx] <- lapply(f[indx], function(x)
        as.numeric(x))
      var_input[, 1:nrow(var_input)] <- f
      
      
      
      var_input$ext_queue <- as.numeric(var_input$ext_queue)
      
      var_input$int_queue <- as.numeric(var_input$int_queue)
      
      
      
      cal_input <- cal()
      cal_input <- as.data.frame(cal_input)
      cal_input$metric <- as.character(cal_input$metric)
      cal_input$node <- as.character(cal_input$node)
      cal_input$start <- as.numeric(as.character(cal_input$start))
      cal_input$end <- as.numeric(as.character(cal_input$end))
      cal_input$value <- as.numeric(as.character(cal_input$value))
      
      
      
      
    }
    
    nodes <-
      rownames(var_input[which(rowSums(var_input[, 1:which(colnames(var_input) ==
                                                             "serv_dist") - 1], na.rm = T) != 0), ])
    exits <-
      rownames(var_input[which(rowSums(var_input[, 1:which(colnames(var_input) ==
                                                             "serv_dist") - 1], na.rm = T) == 0), ])
    #ext_arr<-rownames(var_input[which(var_input$ext_arr>0),])
    
    ext_arr <-
      unique(cal_input$node[which(cal_input$metric == "ext_arr" &
                                    cal_input$value > 0)])
    
    
    delay_dist <-
      var_input[, (nrow(var_input) + 5):(nrow(var_input) + nrow(var_input) + 4)] ## Import the template in csv
    rownames(delay_dist) <- rownames(var_input)
    colnames(delay_dist) <- rownames(var_input)
    delay_dist[which(delay_dist == "", arr.ind = T)] <- NA
    
    delay_param <-
      var_input[, (nrow(var_input) + nrow(var_input) + 5):(ncol(var_input))] ## Import the template in csv
    rownames(delay_param) <- rownames(var_input)
    colnames(delay_param)[1:nrow(delay_param)] <- rownames(var_input)
    delay_param[which(delay_param == "", arr.ind = T)] <- NA
    
    
    from <- c(0)
    to <- c(0)
    
    
    for (i in 1:nrow(delay_dist)) {
      for (j in 1:nrow(delay_dist)) {
        if (!is.na(delay_dist[i, j])) {
          from <- c(from, i)
          to <- c(to, j)
        }
      }
    }
    
    delay_list <- cbind(from, to)
    
    
    tmp <- rownames(delay_dist)
    delay_exits <-
      tmp[c(delay_list[, 2])][!tmp[c(delay_list[, 2])] %in% nodes]
    
    
    var_input$serv_dist[which(rownames(var_input) %in% exits[!(exits %in% delay_exits)])] <-
      NA
    var_input$serv_dist_param[which(rownames(var_input) %in% exits[!(exits %in% delay_exits)])] <-
      NA
    
    cap_min <- vector()
    for (i in nodes) {
      cap_min <-
        c(cap_min, min(cal_input$value[which(cal_input$node == i &
                                               cal_input$metric == "cap")]))
    }
    
    
    cap_max <- vector()
    for (i in nodes) {
      cap_max <-
        c(cap_max, max(cal_input$value[which(cal_input$node == i &
                                               cal_input$metric == "cap")]))
    }
    
    
    cal_tooltip <- vector()
    
    for (i in nodes) {
      tmp <- cal_input[which(cal_input$node == i &
                               cal_input$metric == "cap"), ]
      tmp2 <- vector()
      
      for (j in 1:nrow(tmp)) {
        tmp3 <-
          paste("\n",
                "Start:",
                tmp[j, 3],
                "End:",
                tmp[j, 4],
                "Capacity:",
                tmp[j, 5],
                "//")
        
        
        tmp2 <- c(tmp2, tmp3)
      }
      tmp2 <- paste(tmp2, collapse = "")
      tmp2 <- paste("Capacity Calendar //", tmp2)
      cal_tooltip <- c(cal_tooltip, tmp2)
    }
    
    
    
    # Create a node data frame (ndf)
    
    ndf1 <- create_node_df(
      n = length(nodes),
      type = "lower",
      label = c(nodes),
      fillcolor = "deepskyblue1",
      color = "black",
      fontcolor = "black",
      shape = "square",
      tooltip = cal_tooltip,
      fixedsize = FALSE
    )
    
    
    
    ndf2 <- create_node_df(
      n = length(exits),
      type = "lower",
      label = c(exits),
      fillcolor = "green",
      color = "black",
      fontcolor = "black",
      shape = "diamond",
      tooltip = "Exit",
      fixedsize = FALSE
    )
    
    
    
    
    ndf3 <- create_node_df(
      n = length(ext_arr),
      type = "lower",
      label = as.numeric(c(length(c(
        nodes, exits
      )) + 1):(length(c(
        nodes, exits
      )) + length(ext_arr))) ,
      fillcolor = "white",
      fontcolor = "white",
      shape = "square",
      color = "white"
    )
    
    ndf <- combine_ndfs(ndf1, ndf2, ndf3)
    
    # Create an edge data frame (edf)
    f <- vector()
    t <- vector()
    l <- vector()
    edge_col <- vector()
    edge_tip <- vector()
    
    for (i in 1:length(nodes)) {
      for (j in 1:length(c(nodes, exits))) {
        if (var_input[i, j] > 0) {
          f <- c(f, i)
          t <- c(t, j)
          
          if (!is.na(delay_dist[i, j])) {
            l <- c(l, paste0(round(var_input[i, j] * 100, digits = 2), "%"))
            edge_col <- c(edge_col, "sienna2")
            
            if (delay_dist[i, j] == "exp") {
              pars <-
                as.numeric(unlist(strsplit(
                  x = as.character(delay_param[i, j]), split = ";"
                )))
              delay_mean <- 1 / pars[1]
              
            } else if (delay_dist[i, j] == "unif") {
              pars <-
                as.numeric(unlist(strsplit(
                  x = as.character(delay_param[i, j]), split = ";"
                )))
              delay_mean <- (pars[1] + pars[2]) / 2
              
            } else if (delay_dist[i, j] == "lnorm") {
              pars <-
                as.numeric(unlist(strsplit(
                  x = as.character(delay_param[i, j]), split = ";"
                )))
              delay_mean <- exp(pars[1] + 0.5 * (pars[2]) ^ 2)
              
            } else if (delay_dist[i, j] == "weibull") {
              pars <-
                as.numeric(unlist(strsplit(
                  x = as.character(delay_param[i, j]), split = ";"
                )))
              delay_mean <- pars[2] * (gamma(1 + (1 / pars[1])))
              
            } else if (delay_dist[i, j] == "gamma") {
              pars <-
                as.numeric(unlist(strsplit(
                  x = as.character(delay_param[i, j]), split = ";"
                )))
              delay_mean <- pars[1] / pars[2]
              
            } else{
              pars <-
                as.numeric(unlist(strsplit(
                  x = as.character(delay_param[i, j]), split = ";"
                )))
              tmp2 <-
                do.call(get(paste0("r", delay_dist[i, j])), as.list(c(10 ^ 7, pars)))  #Creates a service time
              delay_mean <- mean(tmp2)
              
            }
            
            edge_tip <-
              c(
                edge_tip,
                paste0(
                  "Mean Delay: ",
                  delay_mean,
                  " (Delay Dist: ",
                  delay_dist[i, j],
                  ")"
                )
              )
          }
          else{
            l <- c(l, paste0(round(var_input[i, j] * 100, digits = 2), "%"))
            edge_col <- c(edge_col, "black")
            edge_tip <- c(edge_tip, paste0("No Delay"))
          }
          
        }
        
      }
    }
    
    
    edf1 <- create_edge_df(
      from = f,
      to = t,
      #rel = c("leading_to"),
      label = l,
      color = edge_col,
      fontcolor = edge_col,
      tooltip = edge_tip
    )
    
    
    edf2 <-
      create_edge_df(
        from = c(length(c(nodes, exits)) + 1):(length(c(nodes, exits)) + length(ext_arr)),
        to = as.numeric(which(rownames(var_input) %in% ext_arr)),
        #rel = c("leading_to"),
        label = as.character("Arrivals"),
        color = "red",
        fontcolor = "red",
        tooltip = "Arrival"
      )
    
    edf <- combine_edfs(edf1, edf2)
    
    
    
    
    #Create a list of average LOS
    LOS <- vector()
    
    
    for (i in nodes) {
      arr.dist <- var_input$serv_dist[which(rownames(var_input) == i)]
      pars <-
        as.numeric(unlist(strsplit(
          as.character(var_input$serv_dist_param[which(rownames(var_input) == i)]), ";"
        )))
      
      
      if (arr.dist == "exp") {
        tmp3 <- 1 / pars
        LOS <- c(LOS, tmp3)
        
      } else if (arr.dist == "unif") {
        tmp3 <- (pars[1] + pars[2]) / 2
        LOS <- c(LOS, tmp3)
        
      } else if (arr.dist == "lnorm") {
        tmp3 <- exp(pars[1] + 0.5 * (pars[2]) ^ 2)
        LOS <- c(LOS, tmp3)
        
      } else if (arr.dist == "weibull") {
        tmp3 <- pars[2] * (gamma(1 + (1 / pars[1])))
        LOS <- c(LOS, tmp3)
        
        
      } else if (arr.dist == "gamma") {
        tmp3 <- pars[1] / pars[2]
        LOS <- c(LOS, tmp3)
        
        
        
      } else{
        tmp2 <-
          do.call(get(paste0("r", arr.dist)), as.list(c(10 ^ 7, pars)))  #Creates a service time
        tmp3 <- mean(tmp2)
        
        LOS <- c(LOS, tmp3)
        
        
      }
    }
    LOS <- round(LOS, digits = 2)
    
    
    TAC <- vector()
    
    for (i in nodes) {
      tmp <- cal_input[which(cal_input$node == i &
                               cal_input$metric == "cap"), ]
      
      if (nrow(tmp) == 1) {
        TAC <- c(TAC, tmp$value)
      }
      if (nrow(tmp) > 1) {
        tmp2 <- sum(tmp$value * (tmp$end - tmp$start)) / max(tmp$end)
        TAC <- c(TAC, tmp2)
      }
    }
    
    TAC <- ceiling(TAC)
    
    
    node_labels <- vector()
    
    for (i in 1:length(nodes)) {
      tmp1 <-
        paste0(
          nodes[i],
          "\n",
          " LOS: ",
          LOS[i],
          "\n",
          "Av Cap: ",
          TAC[i],
          "\n",
          "IQC: ",
          var_input$int_queue[i],
          "\n",
          "EQC: ",
          var_input$ext_queue[i]
        )
      
      node_labels <- c(node_labels, tmp1)
      
    }
    
    if (input$disp3 == TRUE) {
      ndf$label[1:length(nodes)] <- node_labels
    }
    
    
    # Create a graph with the ndf and edf
    graph <-
      create_graph(nodes_df = ndf,
                   edges_df = edf)
    
    graph$global_attrs[1, "value"] <- "dot"
    graph$global_attrs[4, "value"] <- 20
    graph$global_attrs[6, "value"] <- "false"
    graph$global_attrs[14, "value"] <- 20
    graph$global_attrs[17, "value"] <- 1
    
    graph$global_attrs <-
      rbind(graph$global_attrs, c("rankdir", "LR", "graph"))
    graph$global_attrs <-
      rbind(graph$global_attrs, c("splines", "true", "graph"))
    
    showTab(inputId = "navbar", target = "2. Simulation Setup & Run")
    
    output$next_button <- renderUI({
      column(6, align = "center", actionButton(inputId = "j2PSR2", label = c(tagList(
        "Next", icon("arrow-right")
      ))))
      
    })
    
    
    
    
    
    render_graph(graph)
    
  })
  
  output$network <- renderGrViz({
    viz()
  })
  
  checklist_viz <- eventReactive(input$checklist, {
    viz()
  })
  
  output$cl_viz <- renderGrViz({
    checklist_viz()
  })
  
  
  
  
  checklist_table <- eventReactive(input$checklist, {
    #req(input$reps)
    
    req(input$st)
    
    if (input$run_type == "Full Simulation") {
      req(input$wu)
      warm_up <- input$wu
    }
    
    if (input$run_type == "Trial Simulation") {
      warm_up <- 0
    }
    
    
    
    
    
    if (input$w_temp == 0) {
      req(input$file1)
      
      df <- read.csv(input$file1$datapath,
                     header = TRUE,
                     sep = ",")
      rownames(df) <- df[, 1]
      df <- df[, -1]
      colnames(df)[1:(which(colnames(df) == "serv_dist") - 1)] <-
        rownames(df)
    } else{
      var_input <- var()
      
      var_input <- as.data.frame(var_input)
      
      f <- var_input
      indx <- sapply(f, is.factor)
      f[indx] <- lapply(f[indx], function(x)
        as.character(x))
      var_input <- as.data.frame(f)
      
      f <- var_input[, 1:nrow(var_input)]
      indx <- 1:nrow(var_input)
      f[, indx] <- lapply(f[indx], function(x)
        as.numeric(x))
      var_input[, 1:nrow(var_input)] <- f
      
      
      
      var_input$ext_queue <- as.numeric(var_input$ext_queue)
      
      var_input$int_queue <- as.numeric(var_input$int_queue)
      
      df <- var_input
      
    }
    nodes <-
      length(rownames(df[which(rowSums(df[, 1:which(colnames(df) == "serv_dist") -
                                            1], na.rm = T) != 0), ]))
    #exits<-length(rownames(df[which(rowSums(df[,1:which(colnames(df)=="serv_dist")-1],na.rm = T)==0),]))
    #delay_exits<-length(rownames(df[which(rowSums(df[,1:which(colnames(df)=="serv_dist")-1],na.rm = T)==0&as.character(df$serv_dist_param)!=""),]))
    
    x <-
      matrix(
        data = c(
          "Simulation Replications",
          "Warm-up Period",
          "Simulation Period",
          "Total Simulation length",
          "Number of Service points",
          ifelse(
            input$run_type == c("Full Simulation"),
            ceiling(input$reps),
            "NA"
          ),
          warm_up,
          input$st,
          warm_up + input$st,
          nodes
        ),
        ncol = 2
      )
    colnames(x) <- c("Metric", "Value")
    x
    
  })
  
  output$checklist_table_render <-
    renderTable({
      checklist_table()
    }, caption = "Checklist",
    caption.placement = getOption("xtable.caption.placement", "top"),
    caption.width = getOption("xtable.caption.width", NULL))
  
  
  
  
  
  observeEvent(input$sim, {
    #req(input$file1)
    #req(input$file2)
    
    if (input$run_type == "Full Simulation") {
      req(input$wu)
    }
    
    req(input$st)
    req(input$st > 0)
    if (input$run_type == c("Full Simulation")) {
      req(input$reps > 0)
    }
    
    showModal(modalDialog(
      title = div(paste0("Simulation Running \n (Started at : ", format(Sys.time()), ")"),style="font-size:200%"), 
      div("The simulation is now running. If there is an error, a new message box will appear with advice. Once the simulation is complete, a completion message will appear.", style="font-size:200%"),
      easyClose = FALSE,
      footer = NULL,
      size="l"
    ))
    # shinyalert(
    #   title = paste0("Simulation Running \n (Started at : ", format(Sys.time()), ")"),
    #   text = "The simulation is now running. If there is an error, a new message box will appear with advice.",
    #   closeOnEsc = FALSE,
    #   closeOnClickOutside = FALSE,
    #   html = FALSE,
    #   type = "info",
    #   showConfirmButton = FALSE,
    #   showCancelButton = FALSE,
    #   confirmButtonText = "OK",
    #   confirmButtonCol = "#87D9FF",
    #   timer = 0,
    #   imageUrl = "",
    #   animation = TRUE
    # )
  }, priority = 2)
  
  
  
  #### SIMULATION ####
  #sim_out is the object that will contain all the outputs from the simulation and is therefore important in rendering all of the outputs
  # The tryCatch is a error capture system that results in a pop-up for the user if there are any errors within the system. The exact pop-up can be found at the bottom of the simulation section.
  
  sim_out <- eventReactive(input$sim, {
    tryCatch({
      ### Inputs and Initilisation ##################################################################
      
      #req(input$file1)
      #req(input$file2)
      #req(checklist_table())
      req(input$st > 0)
      
      #if(input$run_type==c("Trial Simulation")){updateRadioButtons(session = session,inputId = "run_type",label = "Select Mode",choices = c("Trial Simulation","Full Simulation"),selected = "Full Simulation")}
      
      if (input$run_type == c("Full Simulation")) {
        req(input$reps > 0)
      }
      
      if (input$run_type == c("Trial Simulation")) {
        reps <- 10
      }
      if (input$run_type == c("Full Simulation")) {
        reps <- ceiling(input$reps)
      }
      
      if (input$run_type == c("Trial Simulation")) {
        warm_up <- 0
      }
      if (input$run_type == c("Full Simulation")) {
        warm_up <- input$wu
      }
      
      
      if (input$run_type == c("Trial Simulation")) {
        
        #restrict to 2 (or 1) cores
        #cl <- makeCluster(min(c(2, max(
        #  detectCores() - 1, 1
        #))))
        
        #set to use of n-1 cores
        cl <- makeCluster(min(max(reps - 1, 1), detectCores() - 1))
        
      }
      if (input$run_type == c("Full Simulation")) {
        cl <- makeCluster(min(max(reps - 1, 1), detectCores() - 1))
      }
      
      #ceiling(detectCores()/2)
      
      
      
      hideTab(inputId = "navbar", target = "3. Simulation Outputs")
      showTab(inputId = "navbar", target = "3. Simulation Outputs")
      hideTab(inputId = "navbar", target = "4. Download Outputs")
      
      hideTab(inputId = "3. Simulation Outputs", target = "Output Interpretation")
      # hideTab(inputId = "Simulation Outputs",target = "Service Point Statistics")
      # hideTab(inputId = "Simulation Outputs",target = "Pathway Statistics")
      # hideTab(inputId = "Simulation Outputs",target = "Patient Occupancy Summary")
      # hideTab(inputId = "Simulation Outputs",target = "Bed Occupancy Summary")
      # hideTab(inputId = "Simulation Outputs",target = "Capacity Driven Delay Summary")
      # hideTab(inputId = "Simulation Outputs",target = "Transition Delay Summary")
      # hideTab(inputId = "Simulation Outputs",target = "Queueing Summary")
      
      
      
      if (input$run_type == c("Full Simulation")) {
        showTab(inputId = "navbar", target = "4. Download Outputs")
      }
      
      output$next_button2 <- renderUI({
        column(6, align = "center", actionButton(inputId = "j2PSR3", label = c(tagList(
          "Next", icon("arrow-right")
        ))))
        
      })
      
      
      
      library(shiny)
      library(DiagrammeR)
      library(magrittr)
      library(readr)
      library(DT)
      library(openxlsx)
      library(grid)
      library(gridExtra)
      #library(plotly)
      library(parallel)
      library(data.table)
      library(tidyverse)
      
      ptm <- proc.time()
      
      ##### Simulation Inputs ##############################################################
      
      if (input$w_temp == 0) {
        req(input$file1)
        req(input$file2)
        
        var_input <-
          read.csv(input$file1$datapath,
                   header = TRUE,
                   sep = ",")
        
        syst_names <-
          cbind(as.numeric(c(1:nrow(var_input))), as.character(var_input[, 1]))
        syst_names_single <- syst_names[, 2]
        
        var_input <- var_input[, -1]
        rownames(var_input) <- 1:nrow(var_input)
        colnames(var_input)[1:nrow(var_input)] <- c(1:nrow(var_input))
        
        cal_input <-
          read.csv(input$file2$datapath,
                   header = TRUE,
                   sep = ",")
        cal_input$node <- as.character(cal_input$node)
        
      } else{
        var_input <- var()
        
        var_input <- as.data.frame(var_input)
        
        f <- var_input
        indx <- sapply(f, is.factor)
        f[indx] <- lapply(f[indx], function(x)
          as.character(x))
        var_input <- as.data.frame(f)
        
        f <- var_input[, 1:nrow(var_input)]
        indx <- 1:nrow(var_input)
        f[, indx] <- lapply(f[indx], function(x)
          as.numeric(x))
        var_input[, 1:nrow(var_input)] <- f
        
        
        
        var_input$ext_queue <- as.numeric(var_input$ext_queue)
        
        var_input$int_queue <- as.numeric(var_input$int_queue)
        
        syst_names <-
          cbind(as.numeric(c(1:nrow(var_input))), as.character(rownames(var_input)))
        syst_names_single <- syst_names[, 2]
        
        rownames(var_input) <- 1:nrow(var_input)
        colnames(var_input)[1:nrow(var_input)] <- c(1:nrow(var_input))
        
        cal_input <- cal()
        cal_input <- as.data.frame(cal_input)
        cal_input$metric <- as.character(cal_input$metric)
        cal_input$node <- as.character(cal_input$node)
        cal_input$start <- as.numeric(as.character(cal_input$start))
        cal_input$end <- as.numeric(as.character(cal_input$end))
        cal_input$value <- as.numeric(as.character(cal_input$value))
        
        
      }
      
      
      # var_input<-read.csv(input$file1$datapath,header = TRUE,sep = ",") ## Import the template in csv
      #
      # syst_names<-cbind(as.numeric(c(1:nrow(var_input))),as.character(var_input[,1]))
      # syst_names_single<-syst_names[,2]
      #
      # var_input<-var_input[,-1]
      # rownames(var_input)<-1:nrow(var_input)
      # colnames(var_input)[1:nrow(var_input)]<-c(1:nrow(var_input))
      
      nodes <-
        as.numeric(rownames(var_input[which(rowSums(var_input[, 1:which(colnames(var_input) ==
                                                                          "serv_dist") - 1], na.rm = T) != 0), ])) ##create a list of the service nodes
      node_names <- syst_names[nodes, ]
      node_names <- rbind(node_names, c(NA, NA))
      rownames(node_names) <- c()
      
      
      delay_dist <-
        var_input[, (nrow(var_input) + 5):(nrow(var_input) + nrow(var_input) + 4)] ## Import the template in csv
      rownames(delay_dist) <- 1:nrow(delay_dist)
      colnames(delay_dist)[1:nrow(delay_dist)] <- c(1:nrow(delay_dist))
      delay_dist[which(delay_dist == "", arr.ind = T)] <- NA
      
      delay_param <-
        var_input[, (nrow(var_input) + nrow(var_input) + 5):(ncol(var_input))] ## Import the template in csv
      rownames(delay_param) <- 1:nrow(delay_param)
      colnames(delay_param)[1:nrow(delay_param)] <-
        c(1:nrow(delay_param))
      delay_param[which(delay_param == "", arr.ind = T)] <- NA
      
      
      rep_bed <- list()
      
      from <- c(0)
      to <- c(0)
      
      
      for (i in 1:nrow(delay_dist)) {
        for (j in 1:nrow(delay_dist)) {
          if (!is.na(delay_dist[i, j])) {
            from <- c(from, i)
            to <- c(to, j)
          }
        }
      }
      
      delay_list <- cbind(from, to)
      
      #
      # cal_input<-read.csv(input$file2$datapath,header = TRUE,sep = ",") ## Import the template in csv
      # cal_input$node<-as.character(cal_input$node)
      #
      
      
      if (!is.null(nrow(node_names))) {
        for (i in 1:nrow(node_names)) {
          cal_input$node[as.character(cal_input$node) == node_names[i, 2]] <-
            as.numeric(i)
        }
      }
      
      if (is.null(nrow(node_names))) {
        cal_input$node[as.character(cal_input$node) == node_names[2]] <- 1
        
      }
      
      cap_cal_input <- cal_input[which(cal_input$metric == "cap"), ]
      cap_cal_input <- as.data.frame(cap_cal_input)
      
      arr_cal_input <- cal_input[which(cal_input$metric == "ext_arr"), ]
      arr_cal_input <- as.data.frame(arr_cal_input)
      cap_cal_input_original <- cap_cal_input
      arr_cal_input_original <- arr_cal_input
      
      
      
      ### Shifting Calendars so that the start of the sim_time is the equivalent of 0 on the calendar
      
      if (warm_up != 0) {
        cap_cal_input_new <- cap_cal_input[0, ]
        
        for (cc in nodes) {
          cap_cal_shift <- cap_cal_input[which(cap_cal_input$node == cc), ]
          
          if (nrow(cap_cal_shift) > 1) {
            cap_cal_max = max(cap_cal_shift$end)
            warm_up_modulo = warm_up %% cap_cal_max
            
            if (warm_up_modulo != 0) {
              cap_cal_shift$start <- cap_cal_shift$start + warm_up_modulo
              cap_cal_shift$end <- cap_cal_shift$end + warm_up_modulo
              
              cap_cal_stable <-
                cap_cal_shift[1:min(which(cap_cal_shift$end >= cap_cal_max)), ]
              cap_cal_stable$end[nrow(cap_cal_stable)] <- cap_cal_max
              
              cap_cal_switch <-
                cap_cal_shift[min(which(cap_cal_shift$end > cap_cal_max)):nrow(cap_cal_shift), ]
              cap_cal_switch$start[1] <- cap_cal_max
              cap_cal_switch$start <- cap_cal_switch$start - cap_cal_max
              cap_cal_switch$end <- cap_cal_switch$end - cap_cal_max
              
              
              cap_cal_shift <- rbind(cap_cal_stable, cap_cal_switch)
              cap_cal_shift <-
                cap_cal_shift[order(cap_cal_shift$start), ]
              
              cap_cal_input_new <-
                rbind(cap_cal_input_new, cap_cal_shift)
              
            } else{
              cap_cal_input_new <- rbind(cap_cal_input_new, cap_cal_shift)
            }
            
          } else{
            cap_cal_input_new <- rbind(cap_cal_input_new, cap_cal_shift)
          }
        }
        
        
        arr_cal_input_new <- arr_cal_input[0, ]
        
        for (ac in nodes) {
          arr_cal_shift <- arr_cal_input[which(arr_cal_input$node == ac), ]
          
          if (nrow(arr_cal_shift) > 1) {
            arr_cal_max = max(arr_cal_shift$end)
            warm_up_modulo = warm_up %% arr_cal_max
            
            if (warm_up_modulo != 0) {
              arr_cal_shift$start <- arr_cal_shift$start + warm_up_modulo
              arr_cal_shift$end <- arr_cal_shift$end + warm_up_modulo
              
              arr_cal_stable <-
                arr_cal_shift[1:min(which(arr_cal_shift$end > arr_cal_max)), ]
              arr_cal_stable$end[nrow(arr_cal_stable)] <- arr_cal_max
              
              arr_cal_switch <-
                arr_cal_shift[min(which(arr_cal_shift$end > arr_cal_max)):nrow(arr_cal_shift), ]
              arr_cal_switch$start[1] <- arr_cal_max
              arr_cal_switch$start <- arr_cal_switch$start - arr_cal_max
              arr_cal_switch$end <- arr_cal_switch$end - arr_cal_max
              
              
              arr_cal_shift <- rbind(arr_cal_stable, arr_cal_switch)
              arr_cal_shift <-
                arr_cal_shift[order(arr_cal_shift$start), ]
              
              arr_cal_input_new <-
                rbind(arr_cal_input_new, arr_cal_shift)
            } else{
              arr_cal_input_new <- rbind(arr_cal_input_new, arr_cal_shift)
            }
            
          } else{
            arr_cal_input_new <- rbind(arr_cal_input_new, arr_cal_shift)
          }
        }
        
        
        cap_cal_input <- cap_cal_input_new
        arr_cal_input <- arr_cal_input_new
        
      }
      
      # Sets the timer
      
      
      
      record_scale <- 0.8
      na_lim <- 100
      rpi <- 0.1
      
      
      sim_time <- input$st
      
      
      t.period <- warm_up + sim_time #Sets simulation period
      
      
      
      
      
      
      clusterExport(
        cl = cl,
        varlist = c(
          "cl",
          "var_input",
          "syst_names",
          "syst_names_single",
          "nodes",
          "delay_dist",
          "delay_param",
          "delay_list",
          "cap_cal_input",
          "arr_cal_input",
          "cal_input",
          "record_scale",
          "na_lim",
          "rpi",
          "warm_up",
          "sim_time",
          "t.period",
          "node_names",
          "reps"
        ),
        envir = environment()
      )
      
      clusterSetRNGStream(cl)
      
      clusterEvalQ(
        cl = cl,
        c(
          library(shiny),
          library(DiagrammeR),
          library(magrittr),
          library(readr),
          library(DT),
          library(openxlsx),
          library(grid),
          library(gridExtra),
          #library(plotly),
          library(tidyverse)
        )
      )
      
      
      ####### SIMULATION CODE ##################################################################
      outputs <- parLapply(
        cl = cl,
        X = 1:reps,
        fun = function(j) {
          #print(paste("replicate",j))
          
          
          req(var_input)
          req(cal_input)
          
          
          time <- 0     #Sets time start
          patient <- 0  #Sets initial patient label
          
          
          #nodes<-as.numeric(rownames(var_input)) ##create a list of the service nodes
          
          colnames(var_input)[1:(which(colnames(var_input) == "serv_dist") -
                                   1)] <- c(1:(which(
                                     colnames(var_input) == "serv_dist"
                                   ) - 1))
          
          exits <-
            as.numeric(rownames(var_input[which(rowSums(var_input[, 1:which(colnames(var_input) ==
                                                                              "serv_dist") - 1], na.rm = T) == 0), ])) #Finds all the exit pathway nodes
          exit_names <- syst_names[exits, ]
          
          sch <-
            matrix(NA, ncol = 6, nrow = max(3 * sum(
              t.period * arr_cal_input$value, na.rm = T
            )), 25)
          colnames(sch) <-
            c("time",
              "event",
              "patient",
              "current_node",
              "next_node",
              "previous_node")
          #sch<-data.frame(time=numeric(0),event=character(0),patient=numeric(0),current_node=numeric(0),next_node=numeric(0),previous_node=numeric(0))    #Creates a data frame for the event schedule
          
          #record<-matrix(NA,ncol=6,nrow=23000)
          #colnames(record)<-c("time","event","patient","current_node","next_node","previous_node")
          #record<-data.frame(time=numeric(0),event=character(0),patient=numeric(0),current_node=numeric(0),next_node=numeric(0),previous_node=numeric(0)) #Creates a data frame to collect all records
          
          blocked_mat <-
            matrix(NA,
                   ncol = 6,
                   nrow = sum(cap_cal_input$value, na.rm = T))
          colnames(blocked_mat) <-
            c("time",
              "event",
              "patient",
              "current_node",
              "next_node",
              "previous_node")
          #blocked_mat<-data.frame(time=numeric(0),event=character(0),patient=numeric(0),current_node=numeric(0),next_node=numeric(0),previous_node=numeric(0)) #Creates a data frame to manage blocked patients
          
          bed <-
            data.frame(
              time = as.numeric(0),
              bed = as.numeric(0),
              node = as.numeric(0),
              rep = as.numeric(0)
            )
          
          
          ## Create syst_ variables and assign initial zero value and exits to Inf####
          
          for (i in nodes) {
            tmp <- paste("syst_", i, sep = "")
            assign(tmp, 0)
          }
          
          for (i in exits) {
            tmp <- paste("syst_", i, sep = "")
            assign(tmp, Inf)
          }
          
          
          ##Create n_serv_ variables and assign capacity from input for cal time 0 and set exits to Inf####
          
          initial_cap <- rep(x = 0, times = length(nodes))
          for (i in nodes) {
            initial_cap[i] <-
              cap_cal_input$value[which(cap_cal_input$node == i &
                                          cap_cal_input$start == 0)]
          }
          
          
          for (i in 1:length(nodes)) {
            tmp <- paste("n_serv_", nodes[i], sep = "")
            tmp2 <- initial_cap[i]
            assign(tmp, tmp2)
          }
          
          for (i in exits) {
            tmp <- paste("n_serv_", i, sep = "")
            assign(tmp, Inf)
          }
          
          
          ##Creates inward & outward nodes and probabilities####
          
          for (i in 1:length(nodes)) {
            tmp <- paste("inward_nodes_", nodes[i], sep = "")
            tmp2 <- rownames(var_input[var_input[, i] > 0, ])
            assign(tmp, tmp2)
          }
          
          
          onward_nodes <-
            as.numeric(colnames(var_input[, 1:(which(colnames(var_input) == "serv_dist") -
                                                 1)]))
          
          
          for (i in 1:length(nodes)) {
            tmp <- paste("onward_nodes_prob_", nodes[i], sep = "")
            tmp2 <-
              as.vector(var_input[i, 1:(which(colnames(var_input) == "serv_dist") - 1)])
            
            assign(tmp, tmp2)
          }
          
          
          # Creates the service distribution and parameters lists####
          
          
          serv_dist <- var_input$serv_dist
          serv_dist_param <- var_input$serv_dist_param
          
          
          
          ## Creates the arrivals schedules per node and combines them to form sch ####
          
          
          #sch[1,]<-c(0,1,1,nodes[1],sample(x=onward_nodes,size = 1,prob = get(paste("onward_nodes_prob_",nodes[1],sep=""))),0)   #Adds a patient to the schedule to enter service node 1 at time 0
          
          
          for (i in 1:length(nodes)) {
            arr_cal_input_temp <-
              arr_cal_input[which(arr_cal_input$node == nodes[i]), ]
            
            if (nrow(arr_cal_input_temp) == 1) {
              arr_cal_input_temp$end <- t.period
              
            }
            
            for (w in 1:nrow(arr_cal_input_temp)) {
              cycle_max <- max(arr_cal_input_temp$end)
              
              start <- arr_cal_input_temp$start[w]
              end <- arr_cal_input_temp$end[w]
              if (end > t.period) {
                end <- t.period
              }
              time = start
              
              while (time < t.period) {
                if (arr_cal_input_temp[w, ]$value != 0) {
                  while (time < end) {
                    tmp1 <- rexp(1, rate = arr_cal_input_temp$value[w])
                    time <- time + tmp1
                    #print(time)
                    if (time < end) {
                      patient <- patient + 1
                      sch[match(NA, sch[, "time"]), ] <-
                        c(
                          time,
                          1,
                          patient,
                          nodes[i],
                          sample(
                            x = onward_nodes,
                            size = 1,
                            prob = get(
                              paste("onward_nodes_prob_", nodes[i], sep = "")
                            )
                          ),
                          0
                        )
                      
                    }
                    #sch<-rbind(sch,data.frame(time=time,event="arrival",patient=patient,current_node=nodes[i],next_node=sample(x=onward_nodes,size = 1,prob = get(paste("onward_nodes_prob_",nodes[i],sep = ""))),previous_node="external"))
                    
                  }
                }
                start <- start + cycle_max
                time <- start
                end <- end + cycle_max
                
                
              }
            }
            
          }
          
          
          sch <- sch[1:(match(NA, sch[, "time"]) + 5), ]
          
          #loss_potential<-sum(sch[,"current_node"] %in% nodes[which(var_input$ext_arr>0&var_input$ext_queue!=Inf)])
          
          record <- matrix(NA,
                           ncol = 6,
                           nrow = round(record_scale * nrow(sch), 0))
          colnames(record) <-
            c("time",
              "event",
              "patient",
              "current_node",
              "next_node",
              "previous_node")
          
          
          
          # Creates the service change schedules per node and combines them to form sch ####
          
          cap_sch <-
            data.frame(
              time = numeric(),
              event = numeric(),
              patient = numeric(),
              current_node = numeric(),
              next_node = numeric(),
              previous_node = numeric()
            )
          
          for (i in as.numeric(cap_cal_input$node[duplicated(cap_cal_input$node)])) {
            cap_cal_input_temp <- cap_cal_input[which(cap_cal_input$node == i), ]
            for (l in 1:sum(cap_cal_input_temp$node == i)) {
              time = cap_cal_input_temp$start[l]
              while (time < t.period) {
                cap_sch <- rbind(cap_sch,
                                 c(time, 7, 0, i, cap_cal_input_temp$value[l], 0))
                time <- time + max(cap_cal_input_temp$end)
                
              }
            }
            
          }
          colnames(cap_sch) <-
            c("time",
              "event",
              "patient",
              "current_node",
              "next_node",
              "previous_node")
          cap_sch <- cap_sch[order(cap_sch$time), ]
          cap_sch <- cap_sch[-which(cap_sch$time == 0), ]
          #write.csv(cap_sch,"cap_sch.csv")
          
          sch <- rbind(sch, as.matrix(cap_sch))
          rownames(sch) <- c()
          
          
          
          ##Creates the external queue list and sets queue max ####
          
          for (i in 1:length(nodes)) {
            tmp <- paste("ext_queue_", nodes[i], sep = "")
            q_max <- var_input$ext_queue[i]
            if (q_max == Inf) {
              q_max = max(nrow(sch[which(sch[, "current_node"] == nodes[i]), ]), 1)
            }
            mat <- matrix(NA, ncol = 6, nrow = q_max)
            colnames(mat) <-
              c("time",
                "event",
                "patient",
                "current_node",
                "next_node",
                "previous_node")
            assign(tmp, mat)
            
            tmp <- paste("ext_queue_max_", nodes[i], sep = "")
            tmp2 <- var_input$ext_queue[i]
            assign(tmp, tmp2)
          }
          
          
          ##Creates the internal queue list and sets queue max ####
          
          for (i in 1:length(nodes)) {
            tmp <- paste("int_queue_", nodes[i], sep = "")
            q_max <- var_input$int_queue[i]
            if (q_max == Inf) {
              q_max = nrow(sch)
            }
            mat <- matrix(NA, ncol = 6, nrow = q_max)
            colnames(mat) <-
              c("time",
                "event",
                "patient",
                "current_node",
                "next_node",
                "previous_node")
            assign(tmp, mat)
            
            
            tmp <- paste("int_queue_max_", nodes[i], sep = "")
            tmp2 <- var_input$int_queue[i]
            assign(tmp, tmp2)
          }
          
          
          
          
          ####SIMULATION CYCLE ######################################################################################
          
          ###START - Simulation Cycle###
          while (min(sch[, "time"], na.rm = T) < t.period) {
            #while(min(sch[,"time"],na.rm = T)<21.2) {
            #print(min(sch[,"time"],na.rm = T))
            
            time_test <- min(sch[, "time"], na.rm = T)
            
            
            
            if (sum(is.na(record[, "time"])) <= na_lim) {
              mat <- matrix(NA,
                            ncol = 6,
                            nrow = round(rpi * nrow(record), 0))
              colnames(mat) <-
                c(
                  "time",
                  "event",
                  "patient",
                  "current_node",
                  "next_node",
                  "previous_node"
                )
              record <- rbind(record, mat)
            }
            
            
            
            roi_test <- 7 %in% sch[which(sch[, "time"] == time_test), "event"]
            
            if (roi_test == FALSE) {
              roi <- which.min(sch[, "time"])
            } else{
              roi <- which(sch[, "time"] == time_test & sch[, "event"] == 7)
              roi <- roi[1]
            }
            
            
            ### EXTERNAL ARRIVALS###
            if (sch[roi, "event"] == 1) {
              #Checks if the event at the top of the sch is an arrival
              
              ###EXTERNAL ARRIVAL SCENARIOS -####
              ###
              ###1. SPACE IN THE NODE
              ###
              ###2.   ELSE SPACE IN THE QUEUE
              ###
              ###3.     ELSE QUEUE IS FULL ~ LOST
              ###
              
              
              if (sch[roi, "event"] != 1) {
                print("line199-Non_arrival_event_in_arrival_section")
              }
              
              ### EXTERNAL ARRIVALS 1 -  SPACE IN THE NODE #############################################################
              
              if (get(paste("syst_", sch[roi, "current_node"], sep = "")) <
                  get(paste("n_serv_", sch[roi, "current_node"], sep = ""))) {
                #Checks if there is space at the node for an arrival
                
                
                record[match(NA, record[, "time"]), ] <-
                  sch[roi, ] #Adds the event to the record
                
                arr.dist <-
                  serv_dist[which(nodes == (sch[roi, "current_node"]))]
                pars <-
                  as.numeric(unlist(strsplit(
                    as.character(serv_dist_param[which(nodes == (sch[roi, "current_node"]))]), ";"
                  )))
                
                tmp2 <-
                  do.call(get(paste0("r", arr.dist)), as.list(c(1, pars)))  #Creates a service time
                
                #tmp2<-do.call(paste("serv_dist_",sch$current_node[roi],sep=""),args = list())
                
                record[match(NA, record[, "time"]), ] <-
                  c(min(sch[, "time"], na.rm = T), 2, patient = sch[roi, "patient"], sch[roi, "current_node"], sch[roi, "next_node"], 0)    #Adds a service start event to the record
                sch[match(NA, sch[, "time"]), ] <-
                  c(min(sch[, "time"], na.rm = T) + tmp2,
                    3,
                    patient = sch[roi, "patient"],
                    sch[roi, "current_node"],
                    sch[roi, "next_node"],
                    0)       #Adds a service end event to the schedule
                
                tmp3 <-
                  get(paste("syst_", sch[roi, "current_node"], sep = "")) + 1    #Adds 1 to the relevant node system
                assign(paste("syst_", sch[roi, "current_node"], sep = ""), tmp3)    #Assigns the increased node system value to the correct system variable
                
                bed <-
                  rbind(bed, c(
                    time = sch[roi, "time"],
                    bed = get(paste("syst_", sch[roi, "current_node"], sep = "")),
                    node = sch[roi, "current_node"],
                    rep = j
                  ))
                
                if (get(paste("syst_", sch[roi, "current_node"], sep = "")) >
                    paste("n_serv_", sch[roi, "current_node"], sep = "")) {
                  print("line221- Added a patient to a node where there is no capacity")
                }
                
                
                if (time_test < min(sch[, "time"], na.rm = T)) {
                  print(
                    "line224- Event has been addded to the schedule that occurs before current event"
                  )
                }
                sch[roi, ] <-
                  c(rep(NA, 6)) #Removes the event from the schedule list
              }
              
              ### EXTERNAL ARRIVALS 2 -  SPACE IN THE QUEUE #############################################################
              
              
              else
                #If there is not space at the node then the patient is either added to the queue or if the queue is full then the patient is lost
                if (sum(!is.na(get(
                  paste("ext_queue_", sch[roi, "current_node"], sep = "")
                ))) / 6 < get(paste("ext_queue_max_", sch[roi, "current_node"], sep = ""))) {
                  #If there is space in the queue then the patient is added to a queue
                  
                  if (get(paste("syst_", sch[roi, "current_node"], sep = "")) <
                      get(paste("n_serv_", sch[roi, "current_node"], sep = ""))) {
                    print("line232- Added a patient to a queue where there is capacity")
                  }
                  
                  record[match(NA, record[, "time"]), ] <-
                    sch[roi, ]       #Adds the arrival to the record
                  
                  tmp4 <-
                    paste("ext_queue_", sch[roi, "current_node"], sep = "") #Finds relevant queue
                  inter <-
                    get(tmp4)                                                            #Creates copy of queue to ammend
                  inter[match(NA, inter[, "time"]), ] <-
                    sch[roi, ]             #Changes the correct row in the copy
                  assign(tmp4, inter)                                                          #Ressigns the correct queue list
                  
                  if (sum(!is.na(get(
                    paste("ext_queue_", sch[roi, "current_node"], sep = "")
                  ))) / 6 > get(paste("ext_queue_max_", sch[roi, "current_node"], sep = ""))) {
                    print(("line235-Exceeded external queue capactity"))
                  }
                  
                  if (time_test < min(sch[, "time"], na.rm = T)) {
                    print(
                      "line240- Event has been addded to the schedule that occurs before current event"
                    )
                  }
                  sch[roi, ] <-
                    c(rep(NA, 6))   #Removes arrival event from the schedule
                }
              
              ### EXTERNAL ARRIVALS 3 -  NO SPACE IN NODE OR QUEUE THEREFORE LOST #####
              
              else{
                #If there isn't space in the queue then the patient is lost
                record[match(NA, record[, "time"]), ] <-
                  sch[roi, ]      #Adds the arrival to the record
                record[match(NA, record[, "time"]), ] <-
                  c(min(sch[, "time"], na.rm = T), 5, sch[roi, "patient"], sch[roi, "current_node"], sch[roi, "next_node"], 0) #Adds the loss to the record
                
                if (sum(!is.na(get(
                  paste("ext_queue_", sch[roi, "current_node"], sep = "")
                ))) / 6 < get(paste("ext_queue_max_", sch[roi, "current_node"], sep = ""))) {
                  print((
                    "line245-Lost patient even though there is capacity"
                  ))
                }
                
                if (time_test < min(sch[, "time"], na.rm = T)) {
                  print(
                    "line251- Event has been addded to the schedule that occurs before current event"
                  )
                }
                sch[roi, ] <- c(rep(NA, 6))
              }
              
              
              
            }
            
            
            
            
            ###SERVICE END###
            
            else if (sch[roi, "event"] == 3) {
              #Checks if the event at the top of the sch is an arrival
              if (sch[roi, "event"] != 3) {
                print("line265-Non service_end event in service_end section")
              }
              
              ### SERVICE END SCENARIOS################################################################
              ###
              ###1. NO SPACE AT ONWARD NODE OR ONWARD QUEUE & NOT AN EXIT ~~ BLOCKED
              ###
              ###2. SPACE IN ONWARD NODE OR ONWARD QUEUE (OR EXIT)
              ###
              ###     a. PATIENT ADDED TO ONWARD NODE QUEUE
              ###
              ###         1. IF NO PATIENT WAITING UPSTREAM ~~ Add current patient departure and arrival to the record. Decrease the number in node system by 1 to allow new arrivals later
              ###
              ###         2. IF PATIENT WAITING UPSTREAM ~~
              ###               Add current patient details (departure & arrival) to the record for their new node queue
              ###               Add new patient details to the record for the waiting patient and schedule the service_end
              ###               Shift node system values to reflect moving capacity
              ###
              ###                   a. NO MORE WAITING PATIENTS
              ###
              ###                   b. while(MORE WAITING PATIENTS UP THE CHAIN)
              ###                     {Add departure, arrival and service_start time to the record for the waiting patient and schedule the service_end AND Shift node system values to reflect moving capacity}
              ###                     {Includes filling empty capacity in fixed capacity queues}
              ###                       1.service_end backfill
              ###                       2.int queue backfill
              ###                       3.ext_queue backfill
              ###
              ###
              ###
              ###
              ###     b. NEXT NODE/EXIT IS PRESCRIBED TRANSITION DELAY ~~ Check if the patient is moving to a node or exit with a prescribed transition delay
              ###
              ###
              ###
              ###     c. PATIENT ADDED TO ONWARD NODE SERVICE OR EXITS ~~ Check if there is a patient waiting in any of the inward service nodes or the queue. If multiple, then take patient with earliest service_end time
              ###
              ###         1. IF NO PATIENT WAITING ~~ Add current patient details to the record and new service_end time to the schedule. Decrease the number in node system by 1 to allow new arrivals later
              ###
              ###         2. IF PATIENT WAITING ~~
              ###               Add current patient details (departure & arrival) to the record for their new node queue
              ###               Add new patient details to the record for the waiting patient and schedule the service_end
              ###               Shift node system values to reflect moving capacity
              ###
              ###                   a. NO MORE WAITING PATIENTS
              ###
              ###                   b. while(MORE WAITING PATIENTS UP THE CHAIN)
              ###                     {Add departure, arrival and service_start time to the record for the waiting patient and schedule the service_end AND Shift node system values to reflect moving capacity}
              ###                     {Includes filling empty capacity in fixed capacity queues}
              ###                       1.service_end backfill
              ###                       2.int queue backfill
              ###                       3.ext_queue backfill
              ###
              
              
              record[match(NA, record[, "time"]), ] <-
                sch[roi, ]       #Adds the service_end to the record
              
              ### SERVICE END 1 -  THERE IS NO SPACE IN THE QUEUE OR THE SERVICE NODE SO THE PATIENT IS BLOCKED #############################################################
              if (sch[roi, "next_node"] %in% nodes &&
                  get(paste("syst_", sch[roi, "next_node"], sep = "")) >= get(paste("n_serv_", sch[roi, "next_node"], sep =
                                                                                    "")) &&
                  sum(!is.na(get(
                    paste("int_queue_", sch[roi, "next_node"], sep = "")
                  ))) / 6 >= get(paste("int_queue_max_", sch[roi, "next_node"], sep = ""))) {
                ## If the next node is not an exit and there is no space in the onward queue or node (i.e. no space anywhere)
                
                
                if (get(paste("syst_", sch[roi, "next_node"], sep = "")) < get(paste("n_serv_", sch[roi, "next_node"], sep =
                                                                                     ""))) {
                  print("line314-Blocked patient even though there is space in the node")
                }
                if (sum(!is.na(get(
                  paste("int_queue_", sch[roi, "next_node"], sep = "")
                ))) / 6 < get(paste("int_queue_max_", sch[roi, "next_node"], sep = ""))) {
                  print("line315-Blocked patient even though there is space in the queue")
                }
                
                
                blocked_mat[match(NA, blocked_mat[, "time"]), ] <-
                  sch[roi, ]
                
                if (time_test < min(sch[, "time"], na.rm = T)) {
                  print(
                    "line320- Event has been addded to the schedule that occurs before current event"
                  )
                }
                sch[roi, ] <- c(rep(NA, 6))
              }
              
              
              
              else{
                #### SERVICE END 2a -  THERE IS NO SPACE IN THE NODE AND IT ISNT AN EXIT SO THE PATIENT IS ADDED TO THE QUEUE#########################################################
                
                if (get(paste("syst_", sch[roi, "next_node"], sep = "")) >= get(paste("n_serv_", sch[roi, "next_node"], sep =
                                                                                      "")) &&
                    sch[roi, "next_node"] %in% nodes) {
                  ## If the next node is not an exit and there is no space in the onward node (i.e. there is space in the queue, not the node)
                  
                  
                  record[match(NA, record[, "time"]), ] <-
                    c(min(sch[, "time"], na.rm = T), 8, sch[roi, "patient"], sch[roi, "current_node"], sch[roi, "next_node"], sch[roi, "previous_node"])  #Adds the transition start to the record
                  
                  record[match(NA, record[, "time"]), ] <-
                    c(min(sch[, "time"], na.rm = T), 4, sch[roi, "patient"], sch[roi, "current_node"], sch[roi, "next_node"], sch[roi, "previous_node"])  #Adds the departure to the record
                  
                  tmp99 <-
                    sample(x = onward_nodes,
                           size = 1,
                           prob = get(paste(
                             "onward_nodes_prob_", sch[roi, "next_node"], sep = ""
                           )))
                  
                  record[match(NA, record[, "time"]), ] <-
                    c(min(sch[, "time"], na.rm = T),
                      event = 1,
                      sch[roi, "patient"],
                      sch[roi, "next_node"],
                      tmp99,
                      sch[roi, "current_node"])  #Adds the arrival to the record
                  
                  
                  tmp4 <-
                    paste("int_queue_", sch[roi, "next_node"], sep = "") #Finds relevant queue
                  inter <- get(tmp4)
                  inter[match(NA, inter[, "time"]), ] <-
                    c(min(sch[, "time"], na.rm = T), 1, sch[roi, "patient"], sch[roi, "next_node"], tmp99, sch[roi, "current_node"])
                  assign(tmp4, inter) #Adds the patient arrival record to the correct queue
                  
                  
                  
                  if (sum(!is.na(get(
                    paste("int_queue_", sch[roi, "next_node"], sep = "")
                  ))) / 6 > get(paste("int_queue_max_", sch[roi, "next_node"], sep = ""))) {
                    print(("line347-Exceed Internal queue capactity"))
                  }
                  
                  
                  
                  
                  tmp5 <-
                    get(paste("syst_", sch[roi, "current_node"], sep = "")) - 1  #Takes 1 from the relevant node system
                  assign(paste("syst_", sch[roi, "current_node"], sep = ""), tmp5)  #Assigns the decreased node system value to the correct system variable
                  
                  bed <-
                    rbind(bed, c(
                      time = sch[roi, "time"],
                      bed = get(paste("syst_", sch[roi, "current_node"], sep = "")),
                      node = sch[roi, "current_node"],
                      rep = j
                    ))
                  
                  if (get(paste("syst_", sch[roi, "current_node"], sep = "")) <
                      0) {
                    print("line355- Lowered syst value to below zero which is impossible")
                  }
                  
                  
                  
                  
                  backfill_loop <- "TRUE"
                  backfill <-
                    rbind(get(paste("int_queue_", sch[roi, "current_node"], sep = "")), get(paste("ext_queue_", sch[roi, "current_node"], sep =
                                                                                                    "")), blocked_mat[c(which(blocked_mat[, "next_node"] == sch[roi, "current_node"])), ]) #Finds everyone who is either blocked or in a queue for the newly undercapacity node
                  
                  if (sum(!is.na(backfill[, "patient"])) > 0 &
                      get(paste("n_serv_", sch[roi, "current_node"], sep = "")) > get(paste("syst_", sch[roi, "current_node"], sep =
                                                                                            ""))) {
                    while (backfill_loop == "TRUE") {
                      #Finds the next available person from the queue or blocked node
                      if (sch[roi, "event"] != 3) {
                        print("line367-Non service_end event triggering backfill loop")
                      }
                      
                      if (backfill[which.min(backfill[, "time"]), "event"] ==
                          3) {
                        if (!sum(delay_list[, 1] == backfill[which.min(backfill[, "time"]), "current_node"] &
                                 delay_list[, 2] == backfill[which.min(backfill[, "time"]), "next_node"]) >
                            0) {
                          record[match(NA, record[, "time"]), ] <-
                            c(sch[roi, "time"],
                              8,
                              backfill[which.min(backfill[, "time"]), "patient"],
                              backfill[which.min(backfill[, "time"]), "current_node"],
                              backfill[which.min(backfill[, "time"]), "next_node"],
                              backfill[which.min(backfill[, "time"]), "previous_node"]) #Adds a departure event to the record for the blocked patient
                          
                          record[match(NA, record[, "time"]), ] <-
                            c(sch[roi, "time"],
                              4,
                              backfill[which.min(backfill[, "time"]), "patient"],
                              backfill[which.min(backfill[, "time"]), "current_node"],
                              backfill[which.min(backfill[, "time"]), "next_node"],
                              backfill[which.min(backfill[, "time"]), "previous_node"]) #Adds a departure event to the record for the blocked patient
                          
                          tmp99 <-
                            sample(
                              x = onward_nodes,
                              size = 1,
                              prob = get(
                                paste(
                                  "onward_nodes_prob_",
                                  backfill[which.min(backfill[, "time"]), "next_node"],
                                  sep = ""
                                )
                              )
                            )
                          
                          record[match(NA, record[, "time"]), ] <-
                            c(sch[roi, "time"],
                              1,
                              backfill[which.min(backfill[, "time"]), "patient"],
                              backfill[which.min(backfill[, "time"]), "next_node"],
                              tmp99,
                              backfill[which.min(backfill[, "time"]), "current_node"]) #Adds an arrival event to the record for the blocked patient
                          
                          record[match(NA, record[, "time"]), ] <-
                            c(sch[roi, "time"],
                              2,
                              backfill[which.min(backfill[, "time"]), "patient"],
                              backfill[which.min(backfill[, "time"]), "next_node"],
                              tmp99,
                              backfill[which.min(backfill[, "time"]), "current_node"]) #Adds a service start event to the record for blocked patient
                          
                          
                          arr.dist <-
                            serv_dist[which(nodes == backfill[which.min(backfill[, "time"]), "next_node"])]
                          pars <-
                            as.numeric(unlist(strsplit(
                              as.character(serv_dist_param[which(nodes == backfill[which.min(backfill[, "time"]), "next_node"])]), ";"
                            )))
                          
                          tmp7 <-
                            do.call(get(paste0("r", arr.dist)), as.list(c(1, pars)))  #Creates a service time
                          
                          #tmp7<-do.call(paste("serv_dist_",backfill$next_node[which.min(backfill[,"time"])],sep=""),args = list())
                          
                          sch[match(NA, sch[, "time"]), ] <-
                            c(
                              min(sch[, "time"], na.rm = T) + tmp7,
                              3,
                              backfill[which.min(backfill[, "time"]), "patient"],
                              backfill[which.min(backfill[, "time"]), "next_node"],
                              tmp99,
                              backfill[which.min(backfill[, "time"]), "current_node"]
                            )
                          
                          tmp97 <-
                            get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                        "")) - 1  #Takes 1 from the relevant node system
                          assign(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                         ""),
                                 tmp97)  #Assigns the decreased node system value to the correct system variable
                          
                          bed <-
                            rbind(bed,
                                  c(
                                    time = sch[roi, "time"],
                                    bed = get(paste(
                                      "syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                        ""
                                    )),
                                    node = backfill[which.min(backfill[, "time"]), "current_node"],
                                    rep = j
                                  ))
                          
                          
                          if (get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                        "")) < 0) {
                            print(
                              "line398- Lowered syst value within backfill loop to below zero which is impossible"
                            )
                          }
                          
                          
                          
                          tmp_unblocked_node <-
                            backfill[which.min(backfill[, "time"]), "current_node"]
                          tmp_filled_node <-
                            backfill[which.min(backfill[, "time"]), "next_node"]
                          
                          tmp_blocked_remove <-
                            which(
                              blocked_mat[, "current_node"] == tmp_unblocked_node &
                                blocked_mat[, "next_node"] == tmp_filled_node
                            )
                          
                          blocked_mat[tmp_blocked_remove[which.min(blocked_mat[tmp_blocked_remove, "time"])], ] <-
                            c(rep(NA, 6))
                          
                          tmp9 <-
                            get(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                        "")) + 1    #Adds 1 to the relevant node system
                          assign(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                         ""),
                                 tmp9)    #Assigns the increased node system value to the correct system variable
                          
                          
                          bed <-
                            rbind(bed,
                                  c(
                                    time = sch[roi, "time"],
                                    bed = get(paste(
                                      "syst_", backfill[which.min(backfill[, "time"]), "next_node"], sep = ""
                                    )),
                                    node = backfill[which.min(backfill[, "time"]), "next_node"],
                                    rep = j
                                  ))
                          
                          
                          
                          if (get(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                        "")) > get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                                         ""))) {
                            print(
                              "line413- Increased syst value within backfill loop to above capacity"
                            )
                          }
                          
                          
                          
                          backfill <-
                            rbind(get(
                              paste(
                                "int_queue_",
                                tmp_unblocked_node,
                                sep = ""
                              )
                            ),
                            get(
                              paste(
                                "ext_queue_",
                                tmp_unblocked_node,
                                sep = ""
                              )
                            ),
                            blocked_mat[c(which(blocked_mat[, "next_node"] == tmp_unblocked_node)), ]) #Finds everyone who is either blocked or in a queue for the newly undercapacity node
                          
                          if (sum(!is.na(backfill[, "patient"])) > 0) {
                            if (backfill[which.min(backfill[, "time"]), "event"] == 3) {
                              if (get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                            "")) <= get(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                                              ""))) {
                                backfill_loop = "FALSE"
                              }
                            }
                            
                            else{
                              if (get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                            "")) <= get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                                              ""))) {
                                backfill_loop = "FALSE"
                              }
                            }
                          }
                          
                          if (sum(!is.na(backfill[, "patient"])) == 0) {
                            backfill_loop = "FALSE"
                          }
                        }
                        
                        
                        
                        else if (sum(delay_list[, 1] == backfill[which.min(backfill[, "time"]), "current_node"] &
                                     delay_list[, 2] == backfill[which.min(backfill[, "time"]), "next_node"]) >
                                 0) {
                          record[match(NA, record[, "time"]), ] <-
                            c(sch[roi, "time"],
                              8,
                              backfill[which.min(backfill[, "time"]), "patient"],
                              backfill[which.min(backfill[, "time"]), "current_node"],
                              backfill[which.min(backfill[, "time"]), "next_node"],
                              backfill[which.min(backfill[, "time"]), "previous_node"]) #Adds a transfer_delay_start event to the record for the blocked patient
                          
                          
                          arr.dist <-
                            delay_dist[backfill[which.min(backfill[, "time"]), "current_node"], backfill[which.min(backfill[, "time"]), "next_node"]]
                          pars <-
                            as.numeric(unlist(strsplit(
                              as.character(delay_param[backfill[which.min(backfill[, "time"]), "current_node"], backfill[which.min(backfill[, "time"]), "next_node"]]), ";"
                            )))
                          
                          tmp2 <-
                            do.call(get(paste0("r", arr.dist)), as.list(c(1, pars)))  #Creates a service time
                          
                          sch[match(NA, sch[, "time"]), ] <-
                            c(
                              min(sch[, "time"], na.rm = T) + tmp2,
                              6,
                              patient = backfill[which.min(backfill[, "time"]), "patient"],
                              backfill[which.min(backfill[, "time"]), "current_node"],
                              backfill[which.min(backfill[, "time"]), "next_node"],
                              backfill[which.min(backfill[, "time"]), "previous_node"]
                            )
                          
                          
                          if (backfill[which.min(backfill[, "time"]), "next_node"] %in% nodes) {
                            tmp5 <-
                              get(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                          "")) + 1  #Adds 1 from the relevant node system
                            assign(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                           ""),
                                   tmp5)  #Assigns the increased node system value to the correct system variable
                            
                            bed <-
                              rbind(bed,
                                    c(
                                      time = sch[roi, "time"],
                                      bed = get(paste(
                                        "syst_", backfill[which.min(backfill[, "time"]), "next_node"], sep = ""
                                      )),
                                      node = backfill[which.min(backfill[, "time"]), "next_node"],
                                      rep = j
                                    ))
                            
                            
                          }
                          
                          
                          
                          tmp_unblocked_node <-
                            backfill[which.min(backfill[, "time"]), "current_node"]
                          tmp_filled_node <-
                            backfill[which.min(backfill[, "time"]), "next_node"]
                          
                          tmp_blocked_remove <-
                            which(
                              blocked_mat[, "current_node"] == tmp_unblocked_node &
                                blocked_mat[, "next_node"] == tmp_filled_node
                            )
                          
                          blocked_mat[tmp_blocked_remove[which.min(blocked_mat[tmp_blocked_remove, "time"])], ] <-
                            c(rep(NA, 6))
                          
                          backfill_loop = "FALSE"
                          
                          
                        }
                        
                      }## END OF SERVICE END PART OF BACKFILL LOOP
                      
                      else if (backfill[which.min(backfill[, "time"]), "event"] ==
                               1 & backfill[which.min(backfill[, "time"]), "previous_node"] != 0) {
                        record[match(NA, record[, "time"]), ] <-
                          c(sch[roi, "time"],
                            2,
                            backfill[which.min(backfill[, "time"]), "patient"],
                            backfill[which.min(backfill[, "time"]), "current_node"],
                            backfill[which.min(backfill[, "time"]), "next_node"],
                            backfill[which.min(backfill[, "time"]), "previous_node"]) #Adds a service start event to the record for the next person in the queue
                        
                        
                        arr.dist <-
                          serv_dist[which(nodes == backfill[which.min(backfill[, "time"]), "current_node"])]
                        pars <-
                          as.numeric(unlist(strsplit(
                            as.character(serv_dist_param[which(nodes == backfill[which.min(backfill[, "time"]), "current_node"])]), ";"
                          )))
                        
                        tmp7 <-
                          do.call(get(paste0("r", arr.dist)), as.list(c(1, pars)))  #Creates a service time
                        
                        
                        #tmp7<-do.call(paste("serv_dist_",backfill$current_node[which.min(backfill[,"time"])],sep=""),args = list()) #Draws a random service time from the distribution
                        
                        sch[match(NA, sch[, "time"]), ] <-
                          c(
                            min(sch[, "time"], na.rm = T) + tmp7,
                            3,
                            backfill[which.min(backfill[, "time"]), "patient"],
                            backfill[which.min(backfill[, "time"]), "current_node"],
                            backfill[which.min(backfill[, "time"]), "next_node"],
                            backfill[which.min(backfill[, "time"]), "previous_node"]
                          )       #Adds a service end event to schedule for the next person in the queue
                        
                        queue_find <- "int"
                        
                        tmp8 <-
                          get(paste(queue_find, "_queue_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                      ""))     #Find the queue in question
                        tmp8[which.min(tmp8[, "time"]), ] <-
                          c(rep(NA, 6))                                  #Remove the patient from the queue
                        assign(paste(queue_find, "_queue_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                       ""),
                               tmp8)   #Reassign the queue to the correct variable name
                        
                        tmp9 <-
                          get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                      "")) + 1    #Adds 1 to the relevant node system
                        assign(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                       ""), tmp9)    #Assigns the increased node system value to the correct system variable
                        
                        bed <-
                          rbind(bed,
                                c(
                                  time = sch[roi, "time"],
                                  bed = get(paste(
                                    "syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                      ""
                                  )),
                                  node = backfill[which.min(backfill[, "time"]), "current_node"],
                                  rep = j
                                ))
                        
                        
                        
                        if (get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                      "")) > get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                                       ""))) {
                          print(
                            "line455- Increased syst value within backfill loop to above capacity"
                          )
                        }
                        
                        
                        
                        
                        backfill <-
                          rbind(get(paste(
                            "int_queue_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                              ""
                          )), get(paste(
                            "ext_queue_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                              ""
                          )), blocked_mat[c(which(as.vector(blocked_mat[, "next_node"]) == backfill[which.min(backfill[, "time"]), "current_node"])), ]) #Finds everyone who is blocked for the newly undercapacity queue
                        
                        if (length(backfill[which(backfill[, "event"] == 3), "event"]) !=
                            0) {
                          backfill <- rbind(backfill[which(backfill[, "event"] == 3), ], rep(NA, 6))
                          
                          record[match(NA, record[, "time"]), ] <-
                            c(sch[roi, "time"],
                              8,
                              backfill[which.min(backfill[, "time"]), "patient"],
                              backfill[which.min(backfill[, "time"]), "current_node"],
                              backfill[which.min(backfill[, "time"]), "next_node"],
                              backfill[which.min(backfill[, "time"]), "previous_node"]) #Adds a departure event to the record for the blocked patient
                          
                          
                          record[match(NA, record[, "time"]), ] <-
                            c(sch[roi, "time"],
                              4,
                              backfill[which.min(backfill[, "time"]), "patient"],
                              backfill[which.min(backfill[, "time"]), "current_node"],
                              backfill[which.min(backfill[, "time"]), "next_node"],
                              backfill[which.min(backfill[, "time"]), "previous_node"]) #Adds a departure event to the record for the blocked patient
                          
                          
                          tmp99 <-
                            sample(
                              x = onward_nodes,
                              size = 1,
                              prob = get(
                                paste(
                                  "onward_nodes_prob_",
                                  backfill[which.min(backfill[, "time"]), "next_node"],
                                  sep = ""
                                )
                              )
                            )
                          
                          record[match(NA, record[, "time"]), ] <-
                            c(sch[roi, "time"],
                              1,
                              backfill[which.min(backfill[, "time"]), "patient"],
                              backfill[which.min(backfill[, "time"]), "next_node"],
                              tmp99,
                              backfill[which.min(backfill[, "time"]), "current_node"]) #Adds an arrival event to the record for the blocked patient
                          
                          tmp4 <-
                            paste("int_queue_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                    "")          #Finds the correct queue for the patient to enter
                          inter <-
                            get(tmp4)                                                            #Creates copy of queue to ammend
                          
                          inter[match(NA, inter[, "time"]), ] <-
                            c(backfill[which.min(backfill[, "time"]), "time"],
                              1,
                              backfill[which.min(backfill[, "time"]), "patient"],
                              backfill[which.min(backfill[, "time"]), "next_node"],
                              tmp99,
                              backfill[which.min(backfill[, "time"]), "current_node"])
                          assign(tmp4, inter) #Adds the patient arrival record to the correct queue
                          
                          if (sum(!is.na(get(
                            paste("int_queue_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                  "")
                          ))) / 6 > get(paste("int_queue_max_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                              ""))) {
                            print((
                              "line480-Internal queue capactity exceeded"
                            ))
                          }
                          
                          
                          tmp9 <-
                            get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                        "")) - 1    #Subtracts 1 to the relevant node system
                          assign(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                         ""),
                                 tmp9)
                          
                          bed <-
                            rbind(bed,
                                  c(
                                    time = sch[roi, "time"],
                                    bed = get(paste(
                                      "syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                        ""
                                    )),
                                    node = backfill[which.min(backfill[, "time"]), "current_node"],
                                    rep = j
                                  ))
                          
                          
                          if (get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                        "")) < 0) {
                            print(
                              "line464- Lowered syst value within backfill loop to below zero"
                            )
                          }
                          
                          
                          
                          tmp_unblocked_node <-
                            backfill[which.min(backfill[, "time"]), "current_node"]
                          tmp_filled_node <-
                            backfill[which.min(backfill[, "time"]), "next_node"]
                          
                          tmp_blocked_remove <-
                            which(
                              blocked_mat[, "current_node"] == tmp_unblocked_node &
                                blocked_mat[, "next_node"] == tmp_filled_node
                            )
                          
                          blocked_mat[tmp_blocked_remove[which.min(blocked_mat[tmp_blocked_remove, "time"])], ] <-
                            c(rep(NA, 6))
                          
                          backfill <-
                            rbind(get(
                              paste("int_queue_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                      "")
                            ),
                            get(
                              paste("ext_queue_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                      "")
                            ),
                            blocked_mat[c(which(as.vector(blocked_mat[, "next_node"]) == backfill[which.min(backfill[, "time"]), "current_node"])), ]) #Finds everyone who is either blocked for the newly undercapacity queue
                          
                          if (sum(!is.na(backfill[, "patient"])) > 0) {
                            if (backfill[which.min(backfill[, "time"]), "event"] == 3) {
                              if (get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                            "")) <= get(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                                              ""))) {
                                backfill_loop = "FALSE"
                              }
                            }
                            
                            else{
                              if (get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                            "")) <= get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                                              ""))) {
                                backfill_loop = "FALSE"
                              }
                            }
                          }
                        }
                        else{
                          backfill_loop = "FALSE"
                        }
                        
                      }## END OF ARRIVAL (Internal) PART OF BACKFILL LOOP
                      
                      else if (backfill[which.min(backfill[, "time"]), "event"] ==
                               1 & backfill[which.min(backfill[, "time"]), "previous_node"] == 0) {
                        record[match(NA, record[, "time"]), ] <-
                          c(sch[roi, "time"],
                            2,
                            backfill[which.min(backfill[, "time"]), "patient"],
                            backfill[which.min(backfill[, "time"]), "current_node"],
                            backfill[which.min(backfill[, "time"]), "next_node"],
                            backfill[which.min(backfill[, "time"]), "previous_node"]) #Adds a service start event to the record for the next person in the queue
                        
                        
                        arr.dist <-
                          serv_dist[which(nodes == backfill[which.min(backfill[, "time"]), "current_node"])]
                        pars <-
                          as.numeric(unlist(strsplit(
                            as.character(serv_dist_param[which(nodes == backfill[which.min(backfill[, "time"]), "current_node"])]), ";"
                          )))
                        
                        tmp7 <-
                          do.call(get(paste0("r", arr.dist)), as.list(c(1, pars)))  #Creates a service time
                        
                        
                        #tmp7<-do.call(paste("serv_dist_",backfill$current_node[which.min(backfill[,"time"])],sep=""),args = list()) #Draws a random service time from the distribution
                        
                        sch[match(NA, sch[, "time"]), ] <-
                          c(
                            min(sch[, "time"], na.rm = T) + tmp7,
                            3,
                            backfill[which.min(backfill[, "time"]), "patient"],
                            backfill[which.min(backfill[, "time"]), "current_node"],
                            backfill[which.min(backfill[, "time"]), "next_node"],
                            backfill[which.min(backfill[, "time"]), "previous_node"]
                          )       #Adds a service end event to schedule for the next person in the queue
                        
                        queue_find <- "ext"
                        
                        tmp8 <-
                          get(paste(queue_find, "_queue_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                      ""))     #Find the queue in question
                        tmp8[which.min(tmp8[, "time"]), ] <-
                          c(rep(NA, 6))                                  #Remove the patient from the queue
                        assign(paste(queue_find, "_queue_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                       ""),
                               tmp8)   #Reassign the queue to the correct variable name
                        
                        tmp9 <-
                          get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                      "")) + 1    #Adds 1 to the relevant node system
                        assign(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                       ""), tmp9)    #Assigns the increased node system value to the correct system variable
                        
                        bed <-
                          rbind(bed,
                                c(
                                  time = sch[roi, "time"],
                                  bed = get(paste(
                                    "syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                      ""
                                  )),
                                  node = backfill[which.min(backfill[, "time"]), "current_node"],
                                  rep = j
                                ))
                        
                        
                        if (get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                      "")) > get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                                       ""))) {
                          print(
                            "line535- Increased syst value within backfill loop to above capacity"
                          )
                        }
                        
                        
                        
                        backfill_loop = "FALSE"
                      }## END OF ARRIVAL (External) PART OF BACKFILL LOOP
                      
                      else{
                        backfill_loop = "FALSE"
                      }
                      if (sum(!is.na(backfill[, "patient"])) == 0) {
                        backfill_loop = "FALSE"
                      }
                      
                    }
                  }
                  if (time_test < min(sch[, "time"], na.rm = T)) {
                    print(
                      "line546- Event has been addded to the schedule that occurs before current event"
                    )
                  }
                  sch[roi, ] <-
                    c(rep(NA, 6))
                }                                          #Removes the original service end event from the schedule
                
                
                
                #### SERVICE END 2b - NEXT NODE/EXIT IS PRESCRIBED TRANSITION DELAY###########################################################
                
                
                else if (sum(delay_list[, 1] == sch[roi, "current_node"] &
                             delay_list[, 2] == sch[roi, "next_node"]) > 0) {
                  ##Need new test for delay between service points or service point and exit
                  
                  
                  record[match(NA, record[, "time"]), ] <-
                    c(min(sch[, "time"], na.rm = T), 8, sch[roi, "patient"], sch[roi, "current_node"], sch[roi, "next_node"], sch[roi, "previous_node"])  #Adds the transfer_delay_start to the record
                  
                  
                  arr.dist <-
                    delay_dist[sch[roi, "current_node"], sch[roi, "next_node"]]
                  pars <-
                    as.numeric(unlist(strsplit(
                      as.character(delay_param[sch[roi, "current_node"], sch[roi, "next_node"]]), ";"
                    )))
                  
                  tmp2 <-
                    do.call(get(paste0("r", arr.dist)), as.list(c(1, pars)))  #Creates a service time
                  
                  sch[match(NA, sch[, "time"]), ] <-
                    c(min(sch[, "time"], na.rm = T) + tmp2,
                      6,
                      patient = sch[roi, "patient"],
                      sch[roi, "current_node"],
                      sch[roi, "next_node"],
                      sch[roi, "previous_node"])
                  
                  
                  if (sch[roi, "next_node"] %in% nodes) {
                    tmp5 <-
                      get(paste("syst_", sch[roi, "next_node"], sep = "")) + 1  #Adds 1 from the relevant node system
                    assign(paste("syst_", sch[roi, "next_node"], sep = ""), tmp5)  #Assigns the increased node system value to the correct system variable
                    
                    bed <-
                      rbind(bed,
                            c(
                              time = sch[roi, "time"],
                              bed = get(paste("syst_", sch[roi, "next_node"], sep = "")),
                              node = sch[roi, "next_node"],
                              rep = j
                            ))
                  }
                  
                  sch[roi, ] <- c(rep(NA, 6))
                  
                  
                }
                
                
                #### SERVICE END 2c - THERE IS SPACE IN THE ONWARD NODE OR NO DELAY EXIT #####################################################
                else{
                  #There is an empty space in the onward node or this is an no delay exit from the system
                  
                  record[match(NA, record[, "time"]), ] <-
                    c(min(sch[, "time"], na.rm = T), 8, sch[roi, "patient"], sch[roi, "current_node"], sch[roi, "next_node"], sch[roi, "previous_node"])  #Adds the transfer_delay_start to the record
                  
                  record[match(NA, record[, "time"]), ] <-
                    c(min(sch[, "time"], na.rm = T), 4, sch[roi, "patient"], sch[roi, "current_node"], sch[roi, "next_node"], sch[roi, "previous_node"])  #Adds the departure to the record
                  
                  if (sch[roi, "next_node"] %in% nodes) {
                    #If the patient is exiting the system then they will not need an arrival, service_start or service_end to the record or sch
                    
                    tmp99 <-
                      sample(
                        x = onward_nodes,
                        size = 1,
                        prob = get(paste(
                          "onward_nodes_prob_", sch[roi, "next_node"], sep = ""
                        ))
                      ) #Finds the next node destination after moving node
                    
                    record[match(NA, record[, "time"]), ] <-
                      c(min(sch[, "time"], na.rm = T), 1, sch[roi, "patient"], sch[roi, "next_node"], tmp99, sch[roi, "current_node"]) #Adds arrival to the record
                    
                    record[match(NA, record[, "time"]), ] <-
                      c(min(sch[, "time"], na.rm = T), 2, sch[roi, "patient"], sch[roi, "next_node"], tmp99, sch[roi, "current_node"]) #Adds service_start to the record
                    
                    arr.dist <-
                      serv_dist[which(nodes == sch[roi, "next_node"])]
                    pars <-
                      as.numeric(unlist(strsplit(
                        as.character(serv_dist_param[which(nodes == sch[roi, "next_node"])]), ";"
                      )))
                    
                    tmp2 <-
                      do.call(get(paste0("r", arr.dist)), as.list(c(1, pars)))  #Creates a service time
                    
                    #tmp2<-do.call(paste("serv_dist_",sch$next_node[roi],sep=""),args = list())
                    
                    sch[match(NA, sch[, "time"]), ] <-
                      c(min(sch[, "time"], na.rm = T) + tmp2,
                        3,
                        sch[roi, "patient"],
                        sch[roi, "next_node"],
                        tmp99,
                        sch[roi, "current_node"]) #Adds service_end to the sch
                    
                    tmp5 <-
                      get(paste("syst_", sch[roi, "next_node"], sep = "")) + 1  #Adds 1 from the relevant node system
                    assign(paste("syst_", sch[roi, "next_node"], sep = ""), tmp5)  #Assigns the decreased node system value to the correct system variable
                    
                    bed <-
                      rbind(bed,
                            c(
                              time = sch[roi, "time"],
                              bed = get(paste("syst_", sch[roi, "next_node"], sep = "")),
                              node = sch[roi, "next_node"],
                              rep = j
                            ))
                    
                    
                    if (get(paste("syst_", sch[roi, "next_node"], sep = "")) >
                        get(paste("n_serv_", sch[roi, "next_node"], sep = ""))) {
                      print("line577- Increased syst value to above capacity")
                    }
                    
                  }
                  
                  
                  tmp5 <-
                    get(paste("syst_", sch[roi, "current_node"], sep = "")) - 1  #Takes 1 from the relevant node system
                  assign(paste("syst_", sch[roi, "current_node"], sep = ""), tmp5)  #Assigns the decreased node system value to the correct system variable
                  
                  bed <-
                    rbind(bed, c(
                      time = sch[roi, "time"],
                      bed = get(paste("syst_", sch[roi, "current_node"], sep = "")),
                      node = sch[roi, "current_node"],
                      rep = j
                    ))
                  
                  if (get(paste("syst_", sch[roi, "current_node"], sep = "")) <
                      0) {
                    print("line585- Decreased syst value below 0")
                  }
                  
                  
                  
                  backfill_loop <- "TRUE"
                  backfill <-
                    rbind(get(paste("int_queue_", sch[roi, "current_node"], sep = "")), get(paste("ext_queue_", sch[roi, "current_node"], sep =
                                                                                                    "")), blocked_mat[c(which(blocked_mat[, "next_node"] == sch[roi, "current_node"])), ]) #Finds everyone who is either blocked or in a queue for the newly undercapacity node
                  
                  if (sum(!is.na(backfill[, "patient"])) > 0 &
                      get(paste("n_serv_", sch[roi, "current_node"], sep = "")) > get(paste("syst_", sch[roi, "current_node"], sep =
                                                                                            ""))) {
                    while (backfill_loop == "TRUE") {
                      #Finds the next available person from the queue or blocked node
                      if (sch[roi, "event"] != 3) {
                        print("line367-Non service_end event triggering backfill loop")
                      }
                      
                      if (backfill[which.min(backfill[, "time"]), "event"] ==
                          3) {
                        if (!sum(delay_list[, 1] == backfill[which.min(backfill[, "time"]), "current_node"] &
                                 delay_list[, 2] == backfill[which.min(backfill[, "time"]), "next_node"]) >
                            0) {
                          record[match(NA, record[, "time"]), ] <-
                            c(sch[roi, "time"],
                              8,
                              backfill[which.min(backfill[, "time"]), "patient"],
                              backfill[which.min(backfill[, "time"]), "current_node"],
                              backfill[which.min(backfill[, "time"]), "next_node"],
                              backfill[which.min(backfill[, "time"]), "previous_node"]) #Adds a transfer delay start event to the record for the blocked patient
                          
                          record[match(NA, record[, "time"]), ] <-
                            c(sch[roi, "time"],
                              4,
                              backfill[which.min(backfill[, "time"]), "patient"],
                              backfill[which.min(backfill[, "time"]), "current_node"],
                              backfill[which.min(backfill[, "time"]), "next_node"],
                              backfill[which.min(backfill[, "time"]), "previous_node"]) #Adds a departure event to the record for the blocked patient
                          
                          tmp99 <-
                            sample(
                              x = onward_nodes,
                              size = 1,
                              prob = get(
                                paste(
                                  "onward_nodes_prob_",
                                  backfill[which.min(backfill[, "time"]), "next_node"],
                                  sep = ""
                                )
                              )
                            )
                          
                          record[match(NA, record[, "time"]), ] <-
                            c(sch[roi, "time"],
                              1,
                              backfill[which.min(backfill[, "time"]), "patient"],
                              backfill[which.min(backfill[, "time"]), "next_node"],
                              tmp99,
                              backfill[which.min(backfill[, "time"]), "current_node"]) #Adds an arrival event to the record for the blocked patient
                          
                          record[match(NA, record[, "time"]), ] <-
                            c(sch[roi, "time"],
                              2,
                              backfill[which.min(backfill[, "time"]), "patient"],
                              backfill[which.min(backfill[, "time"]), "next_node"],
                              tmp99,
                              backfill[which.min(backfill[, "time"]), "current_node"]) #Adds a service start event to the record for blocked patient
                          
                          
                          arr.dist <-
                            serv_dist[which(nodes == backfill[which.min(backfill[, "time"]), "next_node"])]
                          pars <-
                            as.numeric(unlist(strsplit(
                              as.character(serv_dist_param[which(nodes == backfill[which.min(backfill[, "time"]), "next_node"])]), ";"
                            )))
                          
                          tmp7 <-
                            do.call(get(paste0("r", arr.dist)), as.list(c(1, pars)))  #Creates a service time
                          
                          #tmp7<-do.call(paste("serv_dist_",backfill$next_node[which.min(backfill[,"time"])],sep=""),args = list())
                          
                          sch[match(NA, sch[, "time"]), ] <-
                            c(
                              min(sch[, "time"], na.rm = T) + tmp7,
                              3,
                              backfill[which.min(backfill[, "time"]), "patient"],
                              backfill[which.min(backfill[, "time"]), "next_node"],
                              tmp99,
                              backfill[which.min(backfill[, "time"]), "current_node"]
                            )
                          
                          tmp97 <-
                            get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                        "")) - 1  #Takes 1 from the relevant node system
                          assign(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                         ""),
                                 tmp97)  #Assigns the decreased node system value to the correct system variable
                          
                          bed <-
                            rbind(bed,
                                  c(
                                    time = sch[roi, "time"],
                                    bed = get(paste(
                                      "syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                        ""
                                    )),
                                    node = backfill[which.min(backfill[, "time"]), "current_node"],
                                    rep = j
                                  ))
                          
                          
                          if (get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                        "")) < 0) {
                            print(
                              "line398- Lowered syst value within backfill loop to below zero which is impossible"
                            )
                          }
                          
                          
                          
                          tmp_unblocked_node <-
                            backfill[which.min(backfill[, "time"]), "current_node"]
                          tmp_filled_node <-
                            backfill[which.min(backfill[, "time"]), "next_node"]
                          
                          tmp_blocked_remove <-
                            which(
                              blocked_mat[, "current_node"] == tmp_unblocked_node &
                                blocked_mat[, "next_node"] == tmp_filled_node
                            )
                          
                          blocked_mat[tmp_blocked_remove[which.min(blocked_mat[tmp_blocked_remove, "time"])], ] <-
                            c(rep(NA, 6))
                          
                          tmp9 <-
                            get(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                        "")) + 1    #Adds 1 to the relevant node system
                          assign(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                         ""),
                                 tmp9)    #Assigns the increased node system value to the correct system variable
                          
                          bed <-
                            rbind(bed,
                                  c(
                                    time = sch[roi, "time"],
                                    bed = get(paste(
                                      "syst_", backfill[which.min(backfill[, "time"]), "next_node"], sep = ""
                                    )),
                                    node = backfill[which.min(backfill[, "time"]), "next_node"],
                                    rep = j
                                  ))
                          
                          
                          
                          if (get(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                        "")) > get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                                         ""))) {
                            print(
                              "line413- Increased syst value within backfill loop to above capacity"
                            )
                          }
                          
                          
                          
                          backfill <-
                            rbind(get(
                              paste(
                                "int_queue_",
                                tmp_unblocked_node,
                                sep = ""
                              )
                            ),
                            get(
                              paste(
                                "ext_queue_",
                                tmp_unblocked_node,
                                sep = ""
                              )
                            ),
                            blocked_mat[c(which(blocked_mat[, "next_node"] == tmp_unblocked_node)), ]) #Finds everyone who is either blocked or in a queue for the newly undercapacity node
                          
                          if (sum(!is.na(backfill[, "patient"])) > 0) {
                            if (backfill[which.min(backfill[, "time"]), "event"] == 3) {
                              if (get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                            "")) <= get(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                                              ""))) {
                                backfill_loop = "FALSE"
                              }
                            }
                            
                            else{
                              if (get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                            "")) <= get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                                              ""))) {
                                backfill_loop = "FALSE"
                              }
                            }
                          }
                          
                          if (sum(!is.na(backfill[, "patient"])) == 0) {
                            backfill_loop = "FALSE"
                          }
                        }
                        
                        
                        
                        else if (sum(delay_list[, 1] == backfill[which.min(backfill[, "time"]), "current_node"] &
                                     delay_list[, 2] == backfill[which.min(backfill[, "time"]), "next_node"]) >
                                 0) {
                          record[match(NA, record[, "time"]), ] <-
                            c(sch[roi, "time"],
                              8,
                              backfill[which.min(backfill[, "time"]), "patient"],
                              backfill[which.min(backfill[, "time"]), "current_node"],
                              backfill[which.min(backfill[, "time"]), "next_node"],
                              backfill[which.min(backfill[, "time"]), "previous_node"]) #Adds a transfer delay start event to the record for the blocked patient
                          
                          
                          arr.dist <-
                            delay_dist[backfill[which.min(backfill[, "time"]), "current_node"], backfill[which.min(backfill[, "time"]), "next_node"]]
                          pars <-
                            as.numeric(unlist(strsplit(
                              as.character(delay_param[backfill[which.min(backfill[, "time"]), "current_node"], backfill[which.min(backfill[, "time"]), "next_node"]]), ";"
                            )))
                          
                          tmp2 <-
                            do.call(get(paste0("r", arr.dist)), as.list(c(1, pars)))  #Creates a service time
                          
                          sch[match(NA, sch[, "time"]), ] <-
                            c(
                              min(sch[, "time"], na.rm = T) + tmp2,
                              6,
                              patient = backfill[which.min(backfill[, "time"]), "patient"],
                              backfill[which.min(backfill[, "time"]), "current_node"],
                              backfill[which.min(backfill[, "time"]), "next_node"],
                              backfill[which.min(backfill[, "time"]), "previous_node"]
                            )
                          
                          
                          if (backfill[which.min(backfill[, "time"]), "next_node"] %in% nodes) {
                            tmp5 <-
                              get(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                          "")) + 1  #Adds 1 from the relevant node system
                            assign(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                           ""),
                                   tmp5)  #Assigns the increased node system value to the correct system variable
                            
                            bed <-
                              rbind(bed,
                                    c(
                                      time = sch[roi, "time"],
                                      bed = get(paste(
                                        "syst_", backfill[which.min(backfill[, "time"]), "next_node"], sep = ""
                                      )),
                                      node = backfill[which.min(backfill[, "time"]), "next_node"],
                                      rep = j
                                    ))
                            
                          }
                          
                          
                          
                          tmp_unblocked_node <-
                            backfill[which.min(backfill[, "time"]), "current_node"]
                          tmp_filled_node <-
                            backfill[which.min(backfill[, "time"]), "next_node"]
                          
                          tmp_blocked_remove <-
                            which(
                              blocked_mat[, "current_node"] == tmp_unblocked_node &
                                blocked_mat[, "next_node"] == tmp_filled_node
                            )
                          
                          blocked_mat[tmp_blocked_remove[which.min(blocked_mat[tmp_blocked_remove, "time"])], ] <-
                            c(rep(NA, 6))
                          
                          backfill_loop = "FALSE"
                          
                          
                        }
                        
                      }## END OF SERVICE END PART OF BACKFILL LOOP
                      
                      else if (backfill[which.min(backfill[, "time"]), "event"] ==
                               1 & backfill[which.min(backfill[, "time"]), "previous_node"] != 0) {
                        record[match(NA, record[, "time"]), ] <-
                          c(sch[roi, "time"],
                            2,
                            backfill[which.min(backfill[, "time"]), "patient"],
                            backfill[which.min(backfill[, "time"]), "current_node"],
                            backfill[which.min(backfill[, "time"]), "next_node"],
                            backfill[which.min(backfill[, "time"]), "previous_node"]) #Adds a service start event to the record for the next person in the queue
                        
                        
                        arr.dist <-
                          serv_dist[which(nodes == backfill[which.min(backfill[, "time"]), "current_node"])]
                        pars <-
                          as.numeric(unlist(strsplit(
                            as.character(serv_dist_param[which(nodes == backfill[which.min(backfill[, "time"]), "current_node"])]), ";"
                          )))
                        
                        tmp7 <-
                          do.call(get(paste0("r", arr.dist)), as.list(c(1, pars)))  #Creates a service time
                        
                        
                        #tmp7<-do.call(paste("serv_dist_",backfill$current_node[which.min(backfill[,"time"])],sep=""),args = list()) #Draws a random service time from the distribution
                        
                        sch[match(NA, sch[, "time"]), ] <-
                          c(
                            min(sch[, "time"], na.rm = T) + tmp7,
                            3,
                            backfill[which.min(backfill[, "time"]), "patient"],
                            backfill[which.min(backfill[, "time"]), "current_node"],
                            backfill[which.min(backfill[, "time"]), "next_node"],
                            backfill[which.min(backfill[, "time"]), "previous_node"]
                          )       #Adds a service end event to schedule for the next person in the queue
                        
                        queue_find <- "int"
                        
                        tmp8 <-
                          get(paste(queue_find, "_queue_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                      ""))     #Find the queue in question
                        tmp8[which.min(tmp8[, "time"]), ] <-
                          c(rep(NA, 6))                                  #Remove the patient from the queue
                        assign(paste(queue_find, "_queue_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                       ""),
                               tmp8)   #Reassign the queue to the correct variable name
                        
                        tmp9 <-
                          get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                      "")) + 1    #Adds 1 to the relevant node system
                        assign(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                       ""), tmp9)    #Assigns the increased node system value to the correct system variable
                        
                        bed <-
                          rbind(bed,
                                c(
                                  time = sch[roi, "time"],
                                  bed = get(paste(
                                    "syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                      ""
                                  )),
                                  node = backfill[which.min(backfill[, "time"]), "current_node"],
                                  rep = j
                                ))
                        
                        
                        if (get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                      "")) > get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                                       ""))) {
                          print(
                            "line455- Increased syst value within backfill loop to above capacity"
                          )
                        }
                        
                        
                        
                        
                        backfill <-
                          rbind(get(paste(
                            "int_queue_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                              ""
                          )), get(paste(
                            "ext_queue_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                              ""
                          )), blocked_mat[c(which(as.vector(blocked_mat[, "next_node"]) == backfill[which.min(backfill[, "time"]), "current_node"])), ]) #Finds everyone who is either blocked for the newly undercapacity queue
                        
                        if (length(backfill[which(backfill[, "event"] == 3), "event"]) !=
                            0) {
                          backfill <- rbind(backfill[which(backfill[, "event"] == 3), ], rep(NA, 6))
                          
                          record[match(NA, record[, "time"]), ] <-
                            c(sch[roi, "time"],
                              8,
                              backfill[which.min(backfill[, "time"]), "patient"],
                              backfill[which.min(backfill[, "time"]), "current_node"],
                              backfill[which.min(backfill[, "time"]), "next_node"],
                              backfill[which.min(backfill[, "time"]), "previous_node"]) #Adds a departure event to the record for the blocked patient
                          
                          
                          record[match(NA, record[, "time"]), ] <-
                            c(sch[roi, "time"],
                              4,
                              backfill[which.min(backfill[, "time"]), "patient"],
                              backfill[which.min(backfill[, "time"]), "current_node"],
                              backfill[which.min(backfill[, "time"]), "next_node"],
                              backfill[which.min(backfill[, "time"]), "previous_node"]) #Adds a departure event to the record for the blocked patient
                          
                          
                          tmp99 <-
                            sample(
                              x = onward_nodes,
                              size = 1,
                              prob = get(
                                paste(
                                  "onward_nodes_prob_",
                                  backfill[which.min(backfill[, "time"]), "next_node"],
                                  sep = ""
                                )
                              )
                            )
                          
                          record[match(NA, record[, "time"]), ] <-
                            c(sch[roi, "time"],
                              1,
                              backfill[which.min(backfill[, "time"]), "patient"],
                              backfill[which.min(backfill[, "time"]), "next_node"],
                              tmp99,
                              backfill[which.min(backfill[, "time"]), "current_node"]) #Adds an arrival event to the record for the blocked patient
                          
                          tmp4 <-
                            paste("int_queue_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                    "")          #Finds the correct queue for the patient to enter
                          inter <-
                            get(tmp4)                                                            #Creates copy of queue to ammend
                          
                          inter[match(NA, inter[, "time"]), ] <-
                            c(backfill[which.min(backfill[, "time"]), "time"],
                              1,
                              backfill[which.min(backfill[, "time"]), "patient"],
                              backfill[which.min(backfill[, "time"]), "next_node"],
                              tmp99,
                              backfill[which.min(backfill[, "time"]), "current_node"])
                          assign(tmp4, inter) #Adds the patient arrival record to the correct queue
                          
                          if (sum(!is.na(get(
                            paste("int_queue_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                  "")
                          ))) / 6 > get(paste("int_queue_max_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                              ""))) {
                            print((
                              "line480-Internal queue capactity exceeded"
                            ))
                          }
                          
                          
                          tmp9 <-
                            get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                        "")) - 1    #Subtracts 1 to the relevant node system
                          assign(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                         ""),
                                 tmp9)
                          
                          bed <-
                            rbind(bed,
                                  c(
                                    time = sch[roi, "time"],
                                    bed = get(paste(
                                      "syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                        ""
                                    )),
                                    node = backfill[which.min(backfill[, "time"]), "current_node"],
                                    rep = j
                                  ))
                          
                          if (get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                        "")) < 0) {
                            print(
                              "line464- Lowered syst value within backfill loop to below zero"
                            )
                          }
                          
                          
                          
                          tmp_unblocked_node <-
                            backfill[which.min(backfill[, "time"]), "current_node"]
                          tmp_filled_node <-
                            backfill[which.min(backfill[, "time"]), "next_node"]
                          
                          tmp_blocked_remove <-
                            which(
                              blocked_mat[, "current_node"] == tmp_unblocked_node &
                                blocked_mat[, "next_node"] == tmp_filled_node
                            )
                          
                          blocked_mat[tmp_blocked_remove[which.min(blocked_mat[tmp_blocked_remove, "time"])], ] <-
                            c(rep(NA, 6))
                          
                          backfill <-
                            rbind(get(
                              paste("int_queue_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                      "")
                            ),
                            get(
                              paste("ext_queue_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                      "")
                            ),
                            blocked_mat[c(which(as.vector(blocked_mat[, "next_node"]) == backfill[which.min(backfill[, "time"]), "current_node"])), ]) #Finds everyone who is either blocked for the newly undercapacity queue
                          
                          if (sum(!is.na(backfill[, "patient"])) > 0) {
                            if (backfill[which.min(backfill[, "time"]), "event"] == 3) {
                              if (get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                            "")) <= get(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                                              ""))) {
                                backfill_loop = "FALSE"
                              }
                            }
                            
                            else{
                              if (get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                            "")) <= get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                                              ""))) {
                                backfill_loop = "FALSE"
                              }
                            }
                          }
                        }
                        else{
                          backfill_loop = "FALSE"
                        }
                        
                      }## END OF ARRIVAL (Internal) PART OF BACKFILL LOOP
                      
                      else if (backfill[which.min(backfill[, "time"]), "event"] ==
                               1 & backfill[which.min(backfill[, "time"]), "previous_node"] == 0) {
                        record[match(NA, record[, "time"]), ] <-
                          c(sch[roi, "time"],
                            2,
                            backfill[which.min(backfill[, "time"]), "patient"],
                            backfill[which.min(backfill[, "time"]), "current_node"],
                            backfill[which.min(backfill[, "time"]), "next_node"],
                            backfill[which.min(backfill[, "time"]), "previous_node"]) #Adds a service start event to the record for the next person in the queue
                        
                        
                        arr.dist <-
                          serv_dist[which(nodes == backfill[which.min(backfill[, "time"]), "current_node"])]
                        pars <-
                          as.numeric(unlist(strsplit(
                            as.character(serv_dist_param[which(nodes == backfill[which.min(backfill[, "time"]), "current_node"])]), ";"
                          )))
                        
                        tmp7 <-
                          do.call(get(paste0("r", arr.dist)), as.list(c(1, pars)))  #Creates a service time
                        
                        
                        #tmp7<-do.call(paste("serv_dist_",backfill$current_node[which.min(backfill[,"time"])],sep=""),args = list()) #Draws a random service time from the distribution
                        
                        sch[match(NA, sch[, "time"]), ] <-
                          c(
                            min(sch[, "time"], na.rm = T) + tmp7,
                            3,
                            backfill[which.min(backfill[, "time"]), "patient"],
                            backfill[which.min(backfill[, "time"]), "current_node"],
                            backfill[which.min(backfill[, "time"]), "next_node"],
                            backfill[which.min(backfill[, "time"]), "previous_node"]
                          )       #Adds a service end event to schedule for the next person in the queue
                        
                        queue_find <- "ext"
                        
                        tmp8 <-
                          get(paste(queue_find, "_queue_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                      ""))     #Find the queue in question
                        tmp8[which.min(tmp8[, "time"]), ] <-
                          c(rep(NA, 6))                                  #Remove the patient from the queue
                        assign(paste(queue_find, "_queue_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                       ""),
                               tmp8)   #Reassign the queue to the correct variable name
                        
                        tmp9 <-
                          get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                      "")) + 1    #Adds 1 to the relevant node system
                        assign(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                       ""), tmp9)    #Assigns the increased node system value to the correct system variable
                        
                        bed <-
                          rbind(bed,
                                c(
                                  time = sch[roi, "time"],
                                  bed = get(paste(
                                    "syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                      ""
                                  )),
                                  node = backfill[which.min(backfill[, "time"]), "current_node"],
                                  rep = j
                                ))
                        
                        
                        if (get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                      "")) > get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                                       ""))) {
                          print(
                            "line535- Increased syst value within backfill loop to above capacity"
                          )
                        }
                        
                        
                        
                        backfill_loop = "FALSE"
                      }## END OF ARRIVAL (External) PART OF BACKFILL LOOP
                      
                      else{
                        backfill_loop = "FALSE"
                      }
                      if (sum(!is.na(backfill[, "patient"])) == 0) {
                        backfill_loop = "FALSE"
                      }
                      
                    }
                  }
                  
                  if (time_test < min(sch[, "time"], na.rm = T)) {
                    print(
                      "line776- Event has been addded to the schedule that occurs before current event"
                    )
                  }
                  sch[roi, ] <- c(rep(NA, 6))
                }
                
                
              }
            }
            
            ###Delayed Departure/transfer###
            ###DELAYED DEPARTURE SCENARIOS ################################################################################
            ###
            ###1. Patient finishes transition delay and moves onto next node or exit
            ###         1.service_end backfill
            ###         2.int queue backfill
            ###         3.ext_queue backfill
            ###
            
            
            else if (sch[roi, "event"] == 6) {
              record[match(NA, record[, "time"]), ] <-
                c(min(sch[, "time"], na.rm = T), 4, sch[roi, "patient"], sch[roi, "current_node"], sch[roi, "next_node"], sch[roi, "previous_node"])  #Adds the delayed departure to the record
              
              tmp5 <-
                get(paste("syst_", sch[roi, "current_node"], sep = "")) - 1  #Takes 1 from the relevant node system
              assign(paste("syst_", sch[roi, "current_node"], sep = ""), tmp5)  #Assigns the decreased node system value to the correct system variable
              
              bed <-
                rbind(bed, c(
                  time = sch[roi, "time"],
                  bed = get(paste("syst_", sch[roi, "current_node"], sep = "")),
                  node = sch[roi, "current_node"],
                  rep = j
                ))
              
              if (sch[roi, "next_node"] %in% nodes) {
                tmp99 <-
                  sample(x = onward_nodes,
                         size = 1,
                         prob = get(paste(
                           "onward_nodes_prob_", sch[roi, "next_node"], sep = ""
                         ))) #Finds the next node destination after moving node
                
                record[match(NA, record[, "time"]), ] <-
                  c(min(sch[, "time"], na.rm = T), 1, sch[roi, "patient"], sch[roi, "next_node"], tmp99, sch[roi, "current_node"]) #Adds arrival to the record
                
                record[match(NA, record[, "time"]), ] <-
                  c(min(sch[, "time"], na.rm = T), 2, sch[roi, "patient"], sch[roi, "next_node"], tmp99, sch[roi, "current_node"]) #Adds service_start to the record
                
                arr.dist <- serv_dist[which(nodes == sch[roi, "next_node"])]
                pars <-
                  as.numeric(unlist(strsplit(
                    as.character(serv_dist_param[which(nodes == sch[roi, "next_node"])]), ";"
                  )))
                
                tmp2 <-
                  do.call(get(paste0("r", arr.dist)), as.list(c(1, pars)))  #Creates a service time
                
                #tmp2<-do.call(paste("serv_dist_",sch$next_node[roi],sep=""),args = list())
                
                sch[match(NA, sch[, "time"]), ] <-
                  c(min(sch[, "time"], na.rm = T) + tmp2,
                    3,
                    sch[roi, "patient"],
                    sch[roi, "next_node"],
                    tmp99,
                    sch[roi, "current_node"]) #Adds service_end to the sch
              }
              
              
              
              if (get(paste("syst_", sch[roi, "current_node"], sep = "")) <
                  0) {
                print("line585- Decreased syst value below 0")
              }
              
              
              
              backfill_loop <- "TRUE"
              backfill <-
                rbind(get(paste("int_queue_", sch[roi, "current_node"], sep = "")), get(paste("ext_queue_", sch[roi, "current_node"], sep =
                                                                                                "")), blocked_mat[c(which(blocked_mat[, "next_node"] == sch[roi, "current_node"])), ]) #Finds everyone who is either blocked or in a queue for the newly undercapacity node
              
              if (sum(!is.na(backfill[, "patient"])) > 0 &
                  get(paste("n_serv_", sch[roi, "current_node"], sep = "")) > get(paste("syst_", sch[roi, "current_node"], sep =
                                                                                        ""))) {
                while (backfill_loop == "TRUE") {
                  #Finds the next available person from the queue or blocked node
                  if (sch[roi, "event"] != 6) {
                    print("line367-Non service_end event triggering backfill loop")
                  }
                  
                  if (backfill[which.min(backfill[, "time"]), "event"] == 3) {
                    if (!sum(delay_list[, 1] == backfill[which.min(backfill[, "time"]), "current_node"] &
                             delay_list[, 2] == backfill[which.min(backfill[, "time"]), "next_node"]) >
                        0) {
                      record[match(NA, record[, "time"]), ] <-
                        c(sch[roi, "time"],
                          8,
                          backfill[which.min(backfill[, "time"]), "patient"],
                          backfill[which.min(backfill[, "time"]), "current_node"],
                          backfill[which.min(backfill[, "time"]), "next_node"],
                          backfill[which.min(backfill[, "time"]), "previous_node"]) #Adds a transfer delay start event to the record for the blocked patient
                      
                      record[match(NA, record[, "time"]), ] <-
                        c(sch[roi, "time"],
                          4,
                          backfill[which.min(backfill[, "time"]), "patient"],
                          backfill[which.min(backfill[, "time"]), "current_node"],
                          backfill[which.min(backfill[, "time"]), "next_node"],
                          backfill[which.min(backfill[, "time"]), "previous_node"]) #Adds a departure event to the record for the blocked patient
                      
                      tmp99 <-
                        sample(
                          x = onward_nodes,
                          size = 1,
                          prob = get(
                            paste("onward_nodes_prob_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                    "")
                          )
                        )
                      
                      record[match(NA, record[, "time"]), ] <-
                        c(sch[roi, "time"],
                          1,
                          backfill[which.min(backfill[, "time"]), "patient"],
                          backfill[which.min(backfill[, "time"]), "next_node"],
                          tmp99,
                          backfill[which.min(backfill[, "time"]), "current_node"]) #Adds an arrival event to the record for the blocked patient
                      
                      record[match(NA, record[, "time"]), ] <-
                        c(sch[roi, "time"],
                          2,
                          backfill[which.min(backfill[, "time"]), "patient"],
                          backfill[which.min(backfill[, "time"]), "next_node"],
                          tmp99,
                          backfill[which.min(backfill[, "time"]), "current_node"]) #Adds a service start event to the record for blocked patient
                      
                      
                      arr.dist <-
                        serv_dist[which(nodes == backfill[which.min(backfill[, "time"]), "next_node"])]
                      pars <-
                        as.numeric(unlist(strsplit(
                          as.character(serv_dist_param[which(nodes == backfill[which.min(backfill[, "time"]), "next_node"])]), ";"
                        )))
                      
                      tmp7 <-
                        do.call(get(paste0("r", arr.dist)), as.list(c(1, pars)))  #Creates a service time
                      
                      #tmp7<-do.call(paste("serv_dist_",backfill$next_node[which.min(backfill[,"time"])],sep=""),args = list())
                      
                      sch[match(NA, sch[, "time"]), ] <-
                        c(
                          min(sch[, "time"], na.rm = T) + tmp7,
                          3,
                          backfill[which.min(backfill[, "time"]), "patient"],
                          backfill[which.min(backfill[, "time"]), "next_node"],
                          tmp99,
                          backfill[which.min(backfill[, "time"]), "current_node"]
                        )
                      
                      tmp97 <-
                        get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                    "")) - 1  #Takes 1 from the relevant node system
                      assign(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                     ""), tmp97)  #Assigns the decreased node system value to the correct system variable
                      
                      bed <-
                        rbind(bed,
                              c(
                                time = sch[roi, "time"],
                                bed = get(paste(
                                  "syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                    ""
                                )),
                                node = backfill[which.min(backfill[, "time"]), "current_node"],
                                rep = j
                              ))
                      
                      
                      if (get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                    "")) < 0) {
                        print(
                          "line398- Lowered syst value within backfill loop to below zero which is impossible"
                        )
                      }
                      
                      
                      
                      tmp_unblocked_node <-
                        backfill[which.min(backfill[, "time"]), "current_node"]
                      tmp_filled_node <-
                        backfill[which.min(backfill[, "time"]), "next_node"]
                      
                      tmp_blocked_remove <-
                        which(
                          blocked_mat[, "current_node"] == tmp_unblocked_node &
                            blocked_mat[, "next_node"] == tmp_filled_node
                        )
                      
                      blocked_mat[tmp_blocked_remove[which.min(blocked_mat[tmp_blocked_remove, "time"])], ] <-
                        c(rep(NA, 6))
                      
                      tmp9 <-
                        get(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                    "")) + 1    #Adds 1 to the relevant node system
                      assign(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                     ""), tmp9)    #Assigns the increased node system value to the correct system variable
                      
                      bed <-
                        rbind(bed,
                              c(
                                time = sch[roi, "time"],
                                bed = get(paste(
                                  "syst_", backfill[which.min(backfill[, "time"]), "next_node"], sep = ""
                                )),
                                node = backfill[which.min(backfill[, "time"]), "next_node"],
                                rep = j
                              ))
                      
                      
                      if (get(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                    "")) > get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                                     ""))) {
                        print(
                          "line413- Increased syst value within backfill loop to above capacity"
                        )
                      }
                      
                      
                      
                      backfill <-
                        rbind(get(
                          paste("int_queue_", tmp_unblocked_node, sep = "")
                        ), get(
                          paste("ext_queue_", tmp_unblocked_node, sep = "")
                        ), blocked_mat[c(which(blocked_mat[, "next_node"] == tmp_unblocked_node)), ]) #Finds everyone who is either blocked or in a queue for the newly undercapacity node
                      
                      if (sum(!is.na(backfill[, "patient"])) > 0) {
                        if (backfill[which.min(backfill[, "time"]), "event"] == 3) {
                          if (get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                        "")) <= get(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                                          ""))) {
                            backfill_loop = "FALSE"
                          }
                        }
                        
                        else{
                          if (get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                        "")) <= get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                                          ""))) {
                            backfill_loop = "FALSE"
                          }
                        }
                      }
                      
                      if (sum(!is.na(backfill[, "patient"])) == 0) {
                        backfill_loop = "FALSE"
                      }
                    }
                    
                    
                    
                    else if (sum(delay_list[, 1] == backfill[which.min(backfill[, "time"]), "current_node"] &
                                 delay_list[, 2] == backfill[which.min(backfill[, "time"]), "next_node"]) >
                             0) {
                      record[match(NA, record[, "time"]), ] <-
                        c(sch[roi, "time"],
                          8,
                          backfill[which.min(backfill[, "time"]), "patient"],
                          backfill[which.min(backfill[, "time"]), "current_node"],
                          backfill[which.min(backfill[, "time"]), "next_node"],
                          backfill[which.min(backfill[, "time"]), "previous_node"]) #Adds a transfer delay start event to the record for the blocked patient
                      
                      arr.dist <-
                        delay_dist[backfill[which.min(backfill[, "time"]), "current_node"], backfill[which.min(backfill[, "time"]), "next_node"]]
                      pars <-
                        as.numeric(unlist(strsplit(
                          as.character(delay_param[backfill[which.min(backfill[, "time"]), "current_node"], backfill[which.min(backfill[, "time"]), "next_node"]]), ";"
                        )))
                      
                      tmp2 <-
                        do.call(get(paste0("r", arr.dist)), as.list(c(1, pars)))  #Creates a service time
                      
                      sch[match(NA, sch[, "time"]), ] <-
                        c(
                          min(sch[, "time"], na.rm = T) + tmp2,
                          6,
                          patient = backfill[which.min(backfill[, "time"]), "patient"],
                          backfill[which.min(backfill[, "time"]), "current_node"],
                          backfill[which.min(backfill[, "time"]), "next_node"],
                          backfill[which.min(backfill[, "time"]), "previous_node"]
                        )
                      
                      
                      if (backfill[which.min(backfill[, "time"]), "next_node"] %in% nodes) {
                        tmp5 <-
                          get(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                      "")) + 1  #Adds 1 from the relevant node system
                        assign(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                       ""), tmp5)  #Assigns the increased node system value to the correct system variable
                        
                        bed <-
                          rbind(bed,
                                c(
                                  time = sch[roi, "time"],
                                  bed = get(paste(
                                    "syst_", backfill[which.min(backfill[, "time"]), "next_node"], sep = ""
                                  )),
                                  node = backfill[which.min(backfill[, "time"]), "next_node"],
                                  rep = j
                                ))
                        
                      }
                      
                      tmp_unblocked_node <-
                        backfill[which.min(backfill[, "time"]), "current_node"]
                      tmp_filled_node <-
                        backfill[which.min(backfill[, "time"]), "next_node"]
                      
                      tmp_blocked_remove <-
                        which(
                          blocked_mat[, "current_node"] == tmp_unblocked_node &
                            blocked_mat[, "next_node"] == tmp_filled_node
                        )
                      
                      blocked_mat[tmp_blocked_remove[which.min(blocked_mat[tmp_blocked_remove, "time"])], ] <-
                        c(rep(NA, 6))
                      
                      backfill_loop = "FALSE"
                      
                      
                    }
                    
                  }## END OF SERVICE END PART OF BACKFILL LOOP
                  
                  else if (backfill[which.min(backfill[, "time"]), "event"] ==
                           1 & backfill[which.min(backfill[, "time"]), "previous_node"] != 0) {
                    record[match(NA, record[, "time"]), ] <-
                      c(sch[roi, "time"],
                        2,
                        backfill[which.min(backfill[, "time"]), "patient"],
                        backfill[which.min(backfill[, "time"]), "current_node"],
                        backfill[which.min(backfill[, "time"]), "next_node"],
                        backfill[which.min(backfill[, "time"]), "previous_node"]) #Adds a service start event to the record for the next person in the queue
                    
                    
                    arr.dist <-
                      serv_dist[which(nodes == backfill[which.min(backfill[, "time"]), "current_node"])]
                    pars <-
                      as.numeric(unlist(strsplit(
                        as.character(serv_dist_param[which(nodes == backfill[which.min(backfill[, "time"]), "current_node"])]), ";"
                      )))
                    
                    tmp7 <-
                      do.call(get(paste0("r", arr.dist)), as.list(c(1, pars)))  #Creates a service time
                    
                    
                    #tmp7<-do.call(paste("serv_dist_",backfill$current_node[which.min(backfill[,"time"])],sep=""),args = list()) #Draws a random service time from the distribution
                    
                    sch[match(NA, sch[, "time"]), ] <-
                      c(
                        min(sch[, "time"], na.rm = T) + tmp7,
                        3,
                        backfill[which.min(backfill[, "time"]), "patient"],
                        backfill[which.min(backfill[, "time"]), "current_node"],
                        backfill[which.min(backfill[, "time"]), "next_node"],
                        backfill[which.min(backfill[, "time"]), "previous_node"]
                      )       #Adds a service end event to schedule for the next person in the queue
                    
                    queue_find <- "int"
                    
                    tmp8 <-
                      get(paste(queue_find, "_queue_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                  ""))     #Find the queue in question
                    tmp8[which.min(tmp8[, "time"]), ] <-
                      c(rep(NA, 6))                                  #Remove the patient from the queue
                    assign(paste(queue_find, "_queue_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                   ""),
                           tmp8)   #Reassign the queue to the correct variable name
                    
                    tmp9 <-
                      get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                  "")) + 1    #Adds 1 to the relevant node system
                    assign(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                   ""), tmp9)    #Assigns the increased node system value to the correct system variable
                    
                    bed <-
                      rbind(bed,
                            c(
                              time = sch[roi, "time"],
                              bed = get(paste(
                                "syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                  ""
                              )),
                              node = backfill[which.min(backfill[, "time"]), "current_node"],
                              rep = j
                            ))
                    
                    
                    if (get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                  "")) > get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                                   ""))) {
                      print(
                        "line455- Increased syst value within backfill loop to above capacity"
                      )
                    }
                    
                    
                    
                    
                    backfill <-
                      rbind(get(paste(
                        "int_queue_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                          ""
                      )), get(paste(
                        "ext_queue_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                          ""
                      )), blocked_mat[c(which(as.vector(blocked_mat[, "next_node"]) == backfill[which.min(backfill[, "time"]), "current_node"])), ]) #Finds everyone who is either blocked for the newly undercapacity queue
                    
                    if (length(backfill[which(backfill[, "event"] == 3), "event"]) !=
                        0) {
                      backfill <- rbind(backfill[which(backfill[, "event"] == 3), ], rep(NA, 6))
                      
                      record[match(NA, record[, "time"]), ] <-
                        c(sch[roi, "time"],
                          8,
                          backfill[which.min(backfill[, "time"]), "patient"],
                          backfill[which.min(backfill[, "time"]), "current_node"],
                          backfill[which.min(backfill[, "time"]), "next_node"],
                          backfill[which.min(backfill[, "time"]), "previous_node"]) #Adds a departure event to the record for the blocked patient
                      
                      
                      record[match(NA, record[, "time"]), ] <-
                        c(sch[roi, "time"],
                          4,
                          backfill[which.min(backfill[, "time"]), "patient"],
                          backfill[which.min(backfill[, "time"]), "current_node"],
                          backfill[which.min(backfill[, "time"]), "next_node"],
                          backfill[which.min(backfill[, "time"]), "previous_node"]) #Adds a departure event to the record for the blocked patient
                      
                      
                      tmp99 <-
                        sample(
                          x = onward_nodes,
                          size = 1,
                          prob = get(
                            paste("onward_nodes_prob_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                    "")
                          )
                        )
                      
                      record[match(NA, record[, "time"]), ] <-
                        c(sch[roi, "time"],
                          1,
                          backfill[which.min(backfill[, "time"]), "patient"],
                          backfill[which.min(backfill[, "time"]), "next_node"],
                          tmp99,
                          backfill[which.min(backfill[, "time"]), "current_node"]) #Adds an arrival event to the record for the blocked patient
                      
                      tmp4 <-
                        paste("int_queue_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                "")          #Finds the correct queue for the patient to enter
                      inter <-
                        get(tmp4)                                                            #Creates copy of queue to ammend
                      
                      inter[match(NA, inter[, "time"]), ] <-
                        c(backfill[which.min(backfill[, "time"]), "time"],
                          1,
                          backfill[which.min(backfill[, "time"]), "patient"],
                          backfill[which.min(backfill[, "time"]), "next_node"],
                          tmp99,
                          backfill[which.min(backfill[, "time"]), "current_node"])
                      assign(tmp4, inter) #Adds the patient arrival record to the correct queue
                      
                      if (sum(!is.na(get(
                        paste("int_queue_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                              "")
                      ))) / 6 > get(paste("int_queue_max_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                          ""))) {
                        print((
                          "line480-Internal queue capactity exceeded"
                        ))
                      }
                      
                      
                      tmp9 <-
                        get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                    "")) - 1    #Subtracts 1 to the relevant node system
                      assign(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                     ""), tmp9)
                      
                      bed <-
                        rbind(bed,
                              c(
                                time = sch[roi, "time"],
                                bed = get(paste(
                                  "syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                    ""
                                )),
                                node = backfill[which.min(backfill[, "time"]), "current_node"],
                                rep = j
                              ))
                      
                      if (get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                    "")) < 0) {
                        print("line464- Lowered syst value within backfill loop to below zero")
                      }
                      
                      
                      
                      tmp_unblocked_node <-
                        backfill[which.min(backfill[, "time"]), "current_node"]
                      tmp_filled_node <-
                        backfill[which.min(backfill[, "time"]), "next_node"]
                      
                      tmp_blocked_remove <-
                        which(
                          blocked_mat[, "current_node"] == tmp_unblocked_node &
                            blocked_mat[, "next_node"] == tmp_filled_node
                        )
                      
                      blocked_mat[tmp_blocked_remove[which.min(blocked_mat[tmp_blocked_remove, "time"])], ] <-
                        c(rep(NA, 6))
                      
                      backfill <-
                        rbind(get(paste(
                          "int_queue_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                            ""
                        )), get(paste(
                          "ext_queue_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                            ""
                        )), blocked_mat[c(which(as.vector(blocked_mat[, "next_node"]) == backfill[which.min(backfill[, "time"]), "current_node"])), ]) #Finds everyone who is either blocked for the newly undercapacity queue
                      
                      if (sum(!is.na(backfill[, "patient"])) > 0) {
                        if (backfill[which.min(backfill[, "time"]), "event"] == 3) {
                          if (get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                        "")) <= get(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                                          ""))) {
                            backfill_loop = "FALSE"
                          }
                        }
                        
                        else{
                          if (get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                        "")) <= get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                                          ""))) {
                            backfill_loop = "FALSE"
                          }
                        }
                      }
                    }
                    else{
                      backfill_loop = "FALSE"
                    }
                    
                  }## END OF ARRIVAL (Internal) PART OF BACKFILL LOOP
                  
                  else if (backfill[which.min(backfill[, "time"]), "event"] ==
                           1 & backfill[which.min(backfill[, "time"]), "previous_node"] == 0) {
                    record[match(NA, record[, "time"]), ] <-
                      c(sch[roi, "time"],
                        2,
                        backfill[which.min(backfill[, "time"]), "patient"],
                        backfill[which.min(backfill[, "time"]), "current_node"],
                        backfill[which.min(backfill[, "time"]), "next_node"],
                        backfill[which.min(backfill[, "time"]), "previous_node"]) #Adds a service start event to the record for the next person in the queue
                    
                    
                    arr.dist <-
                      serv_dist[which(nodes == backfill[which.min(backfill[, "time"]), "current_node"])]
                    pars <-
                      as.numeric(unlist(strsplit(
                        as.character(serv_dist_param[which(nodes == backfill[which.min(backfill[, "time"]), "current_node"])]), ";"
                      )))
                    
                    tmp7 <-
                      do.call(get(paste0("r", arr.dist)), as.list(c(1, pars)))  #Creates a service time
                    
                    
                    #tmp7<-do.call(paste("serv_dist_",backfill$current_node[which.min(backfill[,"time"])],sep=""),args = list()) #Draws a random service time from the distribution
                    
                    sch[match(NA, sch[, "time"]), ] <-
                      c(
                        min(sch[, "time"], na.rm = T) + tmp7,
                        3,
                        backfill[which.min(backfill[, "time"]), "patient"],
                        backfill[which.min(backfill[, "time"]), "current_node"],
                        backfill[which.min(backfill[, "time"]), "next_node"],
                        backfill[which.min(backfill[, "time"]), "previous_node"]
                      )       #Adds a service end event to schedule for the next person in the queue
                    
                    queue_find <- "ext"
                    
                    tmp8 <-
                      get(paste(queue_find, "_queue_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                  ""))     #Find the queue in question
                    tmp8[which.min(tmp8[, "time"]), ] <-
                      c(rep(NA, 6))                                  #Remove the patient from the queue
                    assign(paste(queue_find, "_queue_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                   ""),
                           tmp8)   #Reassign the queue to the correct variable name
                    
                    tmp9 <-
                      get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                  "")) + 1    #Adds 1 to the relevant node system
                    assign(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                   ""), tmp9)    #Assigns the increased node system value to the correct system variable
                    
                    bed <-
                      rbind(bed,
                            c(
                              time = sch[roi, "time"],
                              bed = get(paste(
                                "syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                  ""
                              )),
                              node = backfill[which.min(backfill[, "time"]), "current_node"],
                              rep = j
                            ))
                    
                    
                    if (get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                  "")) > get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                                   ""))) {
                      print(
                        "line535- Increased syst value within backfill loop to above capacity"
                      )
                    }
                    
                    
                    
                    backfill_loop = "FALSE"
                  }## END OF ARRIVAL (External) PART OF BACKFILL LOOP
                  
                  else{
                    backfill_loop = "FALSE"
                  }
                  if (sum(!is.na(backfill[, "patient"])) == 0) {
                    backfill_loop = "FALSE"
                  }
                  
                }
              }
              
              if (time_test < min(sch[, "time"], na.rm = T)) {
                print(
                  "line776- Event has been addded to the schedule that occurs before current event"
                )
              }
              sch[roi, ] <- c(rep(NA, 6))
            }
            
            
            
            ### CAPACITY CHANGE###
            ###CAPACITY CHANGE SCENARIOS ##########################################
            ### 1. If the capacity has increased, find all patients who can batch join the new node and then cycle through the backfills repeatedly after each batched patient has been processed.
            ###
            ### 2. If the capacity has decreased, change the capacity value of the node so no new patients can start until the occupancy decreases below the new capacity. The occupancy should fall away until the new capacity is met.
            ###
            
            ### CAPACITY CHANGE 1 -  CAPACITY INCREASED INVOKING BATCH ARRIVALS AND CYCLIC BACKFILL #############################################################
            
            else if (sch[roi, "event"] == 7) {
              tmp1 <- paste("n_serv_", sch[roi, "current_node"], sep = "")
              assign(tmp1, sch[roi, "next_node"])
              
              cap_node <- sch[roi, "current_node"]
              
              if (get(paste("n_serv_", sch[roi, "current_node"], sep = "")) >
                  get(paste("syst_", sch[roi, "current_node"], sep = ""))) {
                x <-
                  get(paste("n_serv_", sch[roi, "current_node"], sep = "")) - get(paste("syst_", sch[roi, "current_node"], sep =
                                                                                          ""))
                
                backfill <-
                  rbind(get(paste("int_queue_", sch[roi, "current_node"], sep = "")), get(paste("ext_queue_", sch[roi, "current_node"], sep =
                                                                                                  "")), blocked_mat[c(which(blocked_mat[, "next_node"] == sch[roi, "current_node"])), ]) #Finds everyone who is either blocked or in a queue for the newly undercapacity node
                backfill <-
                  rbind(backfill, rep(x = NA, times = 6), rep(x = NA, times = 6))
                backfill <- backfill[order(backfill[, "time"]), ]
                y <- sum(!is.na(backfill[, "time"]))
                xy <- min(x, y)
                
                if (xy > 0) {
                  backfill <- backfill[c(1:xy), ]
                  backfill <-
                    rbind(backfill,
                          rep(x = NA, times = 6),
                          rep(x = NA, times = 6))
                  backfill_loop <- "TRUE"
                  
                  while (backfill_loop == "TRUE") {
                    #Finds the next available person from the queue or blocked node
                    
                    if (backfill[which.min(backfill[, "time"]), "event"] == 3) {
                      if (!sum(delay_list[, 1] == backfill[which.min(backfill[, "time"]), "current_node"] &
                               delay_list[, 2] == backfill[which.min(backfill[, "time"]), "next_node"]) >
                          0) {
                        record[match(NA, record[, "time"]), ] <-
                          c(sch[roi, "time"],
                            8,
                            backfill[which.min(backfill[, "time"]), "patient"],
                            backfill[which.min(backfill[, "time"]), "current_node"],
                            backfill[which.min(backfill[, "time"]), "next_node"],
                            backfill[which.min(backfill[, "time"]), "previous_node"]) #Adds a transfer delay start event to the record for the blocked patient
                        
                        record[match(NA, record[, "time"]), ] <-
                          c(sch[roi, "time"],
                            4,
                            backfill[which.min(backfill[, "time"]), "patient"],
                            backfill[which.min(backfill[, "time"]), "current_node"],
                            backfill[which.min(backfill[, "time"]), "next_node"],
                            backfill[which.min(backfill[, "time"]), "previous_node"]) #Adds a departure event to the record for the blocked patient
                        
                        tmp99 <-
                          sample(
                            x = onward_nodes,
                            size = 1,
                            prob = get(
                              paste("onward_nodes_prob_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                      "")
                            )
                          )
                        
                        record[match(NA, record[, "time"]), ] <-
                          c(sch[roi, "time"],
                            1,
                            backfill[which.min(backfill[, "time"]), "patient"],
                            backfill[which.min(backfill[, "time"]), "next_node"],
                            tmp99,
                            backfill[which.min(backfill[, "time"]), "current_node"]) #Adds an arrival event to the record for the blocked patient
                        
                        record[match(NA, record[, "time"]), ] <-
                          c(sch[roi, "time"],
                            2,
                            backfill[which.min(backfill[, "time"]), "patient"],
                            backfill[which.min(backfill[, "time"]), "next_node"],
                            tmp99,
                            backfill[which.min(backfill[, "time"]), "current_node"]) #Adds a service start event to the record for blocked patient
                        
                        
                        arr.dist <-
                          serv_dist[which(nodes == backfill[which.min(backfill[, "time"]), "next_node"])]
                        pars <-
                          as.numeric(unlist(strsplit(
                            as.character(serv_dist_param[which(nodes == backfill[which.min(backfill[, "time"]), "next_node"])]), ";"
                          )))
                        
                        tmp7 <-
                          do.call(get(paste0("r", arr.dist)), as.list(c(1, pars)))  #Creates a service time
                        
                        #tmp7<-do.call(paste("serv_dist_",backfill$next_node[which.min(backfill[,"time"])],sep=""),args = list())
                        
                        sch[match(NA, sch[, "time"]), ] <-
                          c(
                            min(sch[, "time"], na.rm = T) + tmp7,
                            3,
                            backfill[which.min(backfill[, "time"]), "patient"],
                            backfill[which.min(backfill[, "time"]), "next_node"],
                            tmp99,
                            backfill[which.min(backfill[, "time"]), "current_node"]
                          )
                        
                        tmp97 <-
                          get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                      "")) - 1  #Takes 1 from the relevant node system
                        assign(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                       ""),
                               tmp97)  #Assigns the decreased node system value to the correct system variable
                        
                        bed <-
                          rbind(bed,
                                c(
                                  time = sch[roi, "time"],
                                  bed = get(paste(
                                    "syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                      ""
                                  )),
                                  node = backfill[which.min(backfill[, "time"]), "current_node"],
                                  rep = j
                                ))
                        
                        
                        if (get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                      "")) < 0) {
                          print(
                            "line398- Lowered syst value within backfill loop to below zero which is impossible"
                          )
                        }
                        
                        
                        
                        tmp_unblocked_node <-
                          backfill[which.min(backfill[, "time"]), "current_node"]
                        tmp_filled_node <-
                          backfill[which.min(backfill[, "time"]), "next_node"]
                        
                        tmp_blocked_remove <-
                          which(
                            blocked_mat[, "current_node"] == tmp_unblocked_node &
                              blocked_mat[, "next_node"] == tmp_filled_node
                          )
                        
                        blocked_mat[tmp_blocked_remove[which.min(blocked_mat[tmp_blocked_remove, "time"])], ] <-
                          c(rep(NA, 6))
                        
                        tmp9 <-
                          get(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                      "")) + 1    #Adds 1 to the relevant node system
                        assign(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                       ""), tmp9)    #Assigns the increased node system value to the correct system variable
                        
                        bed <-
                          rbind(bed,
                                c(
                                  time = sch[roi, "time"],
                                  bed = get(paste(
                                    "syst_", backfill[which.min(backfill[, "time"]), "next_node"], sep = ""
                                  )),
                                  node = backfill[which.min(backfill[, "time"]), "next_node"],
                                  rep = j
                                ))
                        
                        
                        if (get(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                      "")) > get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                                       ""))) {
                          print(
                            "line413- Increased syst value within backfill loop to above capacity"
                          )
                        }
                        
                        
                        
                        backfill <-
                          rbind(get(
                            paste("int_queue_", tmp_unblocked_node, sep = "")
                          ), get(
                            paste("ext_queue_", tmp_unblocked_node, sep = "")
                          ), blocked_mat[c(which(blocked_mat[, "next_node"] == tmp_unblocked_node)), ]) #Finds everyone who is either blocked or in a queue for the newly undercapacity node
                        
                        if (sum(!is.na(backfill[, "patient"])) > 0) {
                          if (backfill[which.min(backfill[, "time"]), "event"] == 3) {
                            if (get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                          "")) <= get(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                                            ""))) {
                              backfill_loop = "FALSE"
                            }
                          }
                          
                          else{
                            if (get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                          "")) <= get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                                            ""))) {
                              backfill_loop = "FALSE"
                            }
                          }
                        }
                        
                        if (sum(!is.na(backfill[, "patient"])) == 0) {
                          backfill_loop = "FALSE"
                        }
                      }
                      
                      
                      
                      else if (sum(delay_list[, 1] == backfill[which.min(backfill[, "time"]), "current_node"] &
                                   delay_list[, 2] == backfill[which.min(backfill[, "time"]), "next_node"]) >
                               0) {
                        record[match(NA, record[, "time"]), ] <-
                          c(sch[roi, "time"],
                            8,
                            backfill[which.min(backfill[, "time"]), "patient"],
                            backfill[which.min(backfill[, "time"]), "current_node"],
                            backfill[which.min(backfill[, "time"]), "next_node"],
                            backfill[which.min(backfill[, "time"]), "previous_node"]) #Adds a transfer delay start event to the record for the blocked patient
                        
                        arr.dist <-
                          delay_dist[backfill[which.min(backfill[, "time"]), "current_node"], backfill[which.min(backfill[, "time"]), "next_node"]]
                        pars <-
                          as.numeric(unlist(strsplit(
                            as.character(delay_param[backfill[which.min(backfill[, "time"]), "current_node"], backfill[which.min(backfill[, "time"]), "next_node"]]), ";"
                          )))
                        
                        tmp2 <-
                          do.call(get(paste0("r", arr.dist)), as.list(c(1, pars)))  #Creates a service time
                        
                        sch[match(NA, sch[, "time"]), ] <-
                          c(
                            min(sch[, "time"], na.rm = T) + tmp2,
                            6,
                            patient = backfill[which.min(backfill[, "time"]), "patient"],
                            backfill[which.min(backfill[, "time"]), "current_node"],
                            backfill[which.min(backfill[, "time"]), "next_node"],
                            backfill[which.min(backfill[, "time"]), "previous_node"]
                          )
                        
                        
                        if (backfill[which.min(backfill[, "time"]), "next_node"] %in% nodes) {
                          tmp5 <-
                            get(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                        "")) + 1  #Adds 1 from the relevant node system
                          assign(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                         ""),
                                 tmp5)  #Assigns the increased node system value to the correct system variable
                          
                          bed <-
                            rbind(bed,
                                  c(
                                    time = sch[roi, "time"],
                                    bed = get(paste(
                                      "syst_", backfill[which.min(backfill[, "time"]), "next_node"], sep = ""
                                    )),
                                    node = backfill[which.min(backfill[, "time"]), "next_node"],
                                    rep = j
                                  ))
                          
                        }
                        
                        
                        
                        tmp_unblocked_node <-
                          backfill[which.min(backfill[, "time"]), "current_node"]
                        tmp_filled_node <-
                          backfill[which.min(backfill[, "time"]), "next_node"]
                        
                        tmp_blocked_remove <-
                          which(
                            blocked_mat[, "current_node"] == tmp_unblocked_node &
                              blocked_mat[, "next_node"] == tmp_filled_node
                          )
                        
                        blocked_mat[tmp_blocked_remove[which.min(blocked_mat[tmp_blocked_remove, "time"])], ] <-
                          c(rep(NA, 6))
                        
                        backfill_loop = "FALSE"
                        
                        
                      }
                      
                    }## END OF SERVICE END PART OF BACKFILL LOOP
                    
                    else if (backfill[which.min(backfill[, "time"]), "event"] ==
                             1 & backfill[which.min(backfill[, "time"]), "previous_node"] != 0) {
                      record[match(NA, record[, "time"]), ] <-
                        c(sch[roi, "time"],
                          2,
                          backfill[which.min(backfill[, "time"]), "patient"],
                          backfill[which.min(backfill[, "time"]), "current_node"],
                          backfill[which.min(backfill[, "time"]), "next_node"],
                          backfill[which.min(backfill[, "time"]), "previous_node"]) #Adds a service start event to the record for the next person in the queue
                      
                      
                      arr.dist <-
                        serv_dist[which(nodes == backfill[which.min(backfill[, "time"]), "current_node"])]
                      pars <-
                        as.numeric(unlist(strsplit(
                          as.character(serv_dist_param[which(nodes == backfill[which.min(backfill[, "time"]), "current_node"])]), ";"
                        )))
                      
                      tmp7 <-
                        do.call(get(paste0("r", arr.dist)), as.list(c(1, pars)))  #Creates a service time
                      
                      
                      #tmp7<-do.call(paste("serv_dist_",backfill$current_node[which.min(backfill[,"time"])],sep=""),args = list()) #Draws a random service time from the distribution
                      
                      sch[match(NA, sch[, "time"]), ] <-
                        c(
                          min(sch[, "time"], na.rm = T) + tmp7,
                          3,
                          backfill[which.min(backfill[, "time"]), "patient"],
                          backfill[which.min(backfill[, "time"]), "current_node"],
                          backfill[which.min(backfill[, "time"]), "next_node"],
                          backfill[which.min(backfill[, "time"]), "previous_node"]
                        )       #Adds a service end event to schedule for the next person in the queue
                      
                      queue_find <- "int"
                      
                      tmp8 <-
                        get(paste(queue_find, "_queue_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                    ""))     #Find the queue in question
                      tmp8[which.min(tmp8[, "time"]), ] <-
                        c(rep(NA, 6))                                  #Remove the patient from the queue
                      assign(paste(queue_find, "_queue_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                     ""),
                             tmp8)   #Reassign the queue to the correct variable name
                      
                      tmp9 <-
                        get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                    "")) + 1    #Adds 1 to the relevant node system
                      assign(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                     ""), tmp9)    #Assigns the increased node system value to the correct system variable
                      
                      bed <-
                        rbind(bed,
                              c(
                                time = sch[roi, "time"],
                                bed = get(paste(
                                  "syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                    ""
                                )),
                                node = backfill[which.min(backfill[, "time"]), "current_node"],
                                rep = j
                              ))
                      
                      
                      if (get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                    "")) > get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                                     ""))) {
                        print(
                          "line455- Increased syst value within backfill loop to above capacity"
                        )
                      }
                      
                      
                      
                      
                      backfill <-
                        rbind(get(paste(
                          "int_queue_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                            ""
                        )), get(paste(
                          "ext_queue_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                            ""
                        )), blocked_mat[c(which(as.vector(blocked_mat[, "next_node"]) == backfill[which.min(backfill[, "time"]), "current_node"])), ]) #Finds everyone who is either blocked for the newly undercapacity queue
                      
                      if (length(backfill[which(backfill[, "event"] == 3), "event"]) !=
                          0) {
                        backfill <- rbind(backfill[which(backfill[, "event"] == 3), ], rep(NA, 6))
                        
                        record[match(NA, record[, "time"]), ] <-
                          c(sch[roi, "time"],
                            8,
                            backfill[which.min(backfill[, "time"]), "patient"],
                            backfill[which.min(backfill[, "time"]), "current_node"],
                            backfill[which.min(backfill[, "time"]), "next_node"],
                            backfill[which.min(backfill[, "time"]), "previous_node"]) #Adds a departure event to the record for the blocked patient
                        
                        
                        record[match(NA, record[, "time"]), ] <-
                          c(sch[roi, "time"],
                            4,
                            backfill[which.min(backfill[, "time"]), "patient"],
                            backfill[which.min(backfill[, "time"]), "current_node"],
                            backfill[which.min(backfill[, "time"]), "next_node"],
                            backfill[which.min(backfill[, "time"]), "previous_node"]) #Adds a departure event to the record for the blocked patient
                        
                        
                        tmp99 <-
                          sample(
                            x = onward_nodes,
                            size = 1,
                            prob = get(
                              paste("onward_nodes_prob_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                      "")
                            )
                          )
                        
                        record[match(NA, record[, "time"]), ] <-
                          c(sch[roi, "time"],
                            1,
                            backfill[which.min(backfill[, "time"]), "patient"],
                            backfill[which.min(backfill[, "time"]), "next_node"],
                            tmp99,
                            backfill[which.min(backfill[, "time"]), "current_node"]) #Adds an arrival event to the record for the blocked patient
                        
                        tmp4 <-
                          paste("int_queue_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                  "")          #Finds the correct queue for the patient to enter
                        inter <-
                          get(tmp4)                                                            #Creates copy of queue to ammend
                        
                        inter[match(NA, inter[, "time"]), ] <-
                          c(backfill[which.min(backfill[, "time"]), "time"],
                            1,
                            backfill[which.min(backfill[, "time"]), "patient"],
                            backfill[which.min(backfill[, "time"]), "next_node"],
                            tmp99,
                            backfill[which.min(backfill[, "time"]), "current_node"])
                        assign(tmp4, inter) #Adds the patient arrival record to the correct queue
                        
                        if (sum(!is.na(get(
                          paste("int_queue_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                "")
                        ))) / 6 > get(paste("int_queue_max_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                            ""))) {
                          print((
                            "line480-Internal queue capactity exceeded"
                          ))
                        }
                        
                        
                        tmp9 <-
                          get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                      "")) - 1    #Subtracts 1 to the relevant node system
                        assign(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                       ""), tmp9)
                        
                        bed <-
                          rbind(bed,
                                c(
                                  time = sch[roi, "time"],
                                  bed = get(paste(
                                    "syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                      ""
                                  )),
                                  node = backfill[which.min(backfill[, "time"]), "current_node"],
                                  rep = j
                                ))
                        
                        
                        if (get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                      "")) < 0) {
                          print(
                            "line464- Lowered syst value within backfill loop to below zero"
                          )
                        }
                        
                        
                        
                        tmp_unblocked_node <-
                          backfill[which.min(backfill[, "time"]), "current_node"]
                        tmp_filled_node <-
                          backfill[which.min(backfill[, "time"]), "next_node"]
                        
                        tmp_blocked_remove <-
                          which(
                            blocked_mat[, "current_node"] == tmp_unblocked_node &
                              blocked_mat[, "next_node"] == tmp_filled_node
                          )
                        
                        blocked_mat[tmp_blocked_remove[which.min(blocked_mat[tmp_blocked_remove, "time"])], ] <-
                          c(rep(NA, 6))
                        
                        backfill <-
                          rbind(get(paste(
                            "int_queue_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                              ""
                          )), get(paste(
                            "ext_queue_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                              ""
                          )), blocked_mat[c(which(as.vector(blocked_mat[, "next_node"]) == backfill[which.min(backfill[, "time"]), "current_node"])), ]) #Finds everyone who is either blocked for the newly undercapacity queue
                        
                        if (sum(!is.na(backfill[, "patient"])) > 0) {
                          if (backfill[which.min(backfill[, "time"]), "event"] == 3) {
                            if (get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                          "")) <= get(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"], sep =
                                                            ""))) {
                              backfill_loop = "FALSE"
                            }
                          }
                          
                          else{
                            if (get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                          "")) <= get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                                            ""))) {
                              backfill_loop = "FALSE"
                            }
                          }
                        }
                      }
                      else{
                        backfill_loop = "FALSE"
                      }
                      
                    }## END OF ARRIVAL (Internal) PART OF BACKFILL LOOP
                    
                    else if (backfill[which.min(backfill[, "time"]), "event"] ==
                             1 & backfill[which.min(backfill[, "time"]), "previous_node"] == 0) {
                      record[match(NA, record[, "time"]), ] <-
                        c(sch[roi, "time"],
                          2,
                          backfill[which.min(backfill[, "time"]), "patient"],
                          backfill[which.min(backfill[, "time"]), "current_node"],
                          backfill[which.min(backfill[, "time"]), "next_node"],
                          backfill[which.min(backfill[, "time"]), "previous_node"]) #Adds a service start event to the record for the next person in the queue
                      
                      
                      arr.dist <-
                        serv_dist[which(nodes == backfill[which.min(backfill[, "time"]), "current_node"])]
                      pars <-
                        as.numeric(unlist(strsplit(
                          as.character(serv_dist_param[which(nodes == backfill[which.min(backfill[, "time"]), "current_node"])]), ";"
                        )))
                      
                      tmp7 <-
                        do.call(get(paste0("r", arr.dist)), as.list(c(1, pars)))  #Creates a service time
                      
                      
                      #tmp7<-do.call(paste("serv_dist_",backfill$current_node[which.min(backfill[,"time"])],sep=""),args = list()) #Draws a random service time from the distribution
                      
                      sch[match(NA, sch[, "time"]), ] <-
                        c(
                          min(sch[, "time"], na.rm = T) + tmp7,
                          3,
                          backfill[which.min(backfill[, "time"]), "patient"],
                          backfill[which.min(backfill[, "time"]), "current_node"],
                          backfill[which.min(backfill[, "time"]), "next_node"],
                          backfill[which.min(backfill[, "time"]), "previous_node"]
                        )       #Adds a service end event to schedule for the next person in the queue
                      
                      queue_find <- "ext"
                      
                      tmp8 <-
                        get(paste(queue_find, "_queue_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                    ""))     #Find the queue in question
                      tmp8[which.min(tmp8[, "time"]), ] <-
                        c(rep(NA, 6))                                  #Remove the patient from the queue
                      assign(paste(queue_find, "_queue_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                     ""),
                             tmp8)   #Reassign the queue to the correct variable name
                      
                      tmp9 <-
                        get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                    "")) + 1    #Adds 1 to the relevant node system
                      assign(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                     ""), tmp9)    #Assigns the increased node system value to the correct system variable
                      
                      bed <-
                        rbind(bed,
                              c(
                                time = sch[roi, "time"],
                                bed = get(paste(
                                  "syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                    ""
                                )),
                                node = backfill[which.min(backfill[, "time"]), "current_node"],
                                rep = j
                              ))
                      
                      
                      if (get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                    "")) > get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "current_node"], sep =
                                                     ""))) {
                        print(
                          "line535- Increased syst value within backfill loop to above capacity"
                        )
                      }
                      
                      
                      
                      backfill_loop = "FALSE"
                    }## END OF ARRIVAL (External) PART OF BACKFILL LOOP
                    
                    else{
                      backfill_loop = "FALSE"
                    }
                    if (sum(!is.na(backfill[, "patient"])) == 0) {
                      backfill_loop = "FALSE"
                    }
                    
                  }
                }
                
                
              }
              
              
              
              sch[roi, ] <- c(rep(NA, 6))
            }
            
            
            else{
              print("Major Error")
            }
            
          }
          ###END - Simulation Cycle###
          
          ####OUTPUTS--------------------------------------------------------------
          
          ###Create standard record###
          record <- as.data.frame(record[which(!is.na(record[, "time"])), ])
          record$event[which(record$event == 1)] <- "arrival"
          record$event[which(record$event == 2)] <- "service_start"
          record$event[which(record$event == 3)] <- "service_end"
          record$event[which(record$event == 4)] <- "departure"
          record$event[which(record$event == 5)] <- "loss"
          record$event[which(record$event == 6)] <- "delayed_departure"
          record$event[which(record$event == 7)] <- "capacity_change"
          record$event[which(record$event == 8)] <- "transition_start"
          record$previous_node[which(record$previous_node == 0)] <-
            "external"
          
          #record<-record[which(!is.na(record[,"time"])),]
          
          
          bed <- bed[-1, ]
          
          
          ### Create the patient & node metrics ######################################################
          
          all_data <-
            data.frame(
              rep = as.numeric(),
              patient = as.numeric(),
              node = as.numeric(),
              arr = as.numeric(),
              wait = as.numeric(),
              ss = as.numeric(),
              service = as.numeric(),
              se = as.numeric(),
              delayed = as.numeric(),
              tds = as.numeric(),
              transition = as.numeric(),
              dep = as.numeric()
            )
          
          
          library(tidyverse)
          library(data.table)
          for (i in 1:length(nodes)) {
            arr_node <-
              record[which(record$event == "arrival" &
                             record$current_node == nodes[i]), c(3, 1)]
            ss_node <-
              record[which(record$event == "service_start" &
                             record$current_node == nodes[i]), c(3, 1)]
            se_node <-
              record[which(record$event == "service_end" &
                             record$current_node == nodes[i]), c(3, 1)]
            tds_node <-
              record[which(record$event == "transition_start" &
                             record$current_node == nodes[i]), c(3, 1)]
            dep_node <-
              record[which(record$event == "departure" &
                             record$current_node == nodes[i]), c(3, 1)]
            
            
            arr_node <- arr_node[which(arr_node$time > warm_up), ]
            colnames(arr_node)[2] <- "arr"
            
            ss_node <- ss_node[which(ss_node$time > warm_up), ]
            colnames(ss_node)[2] <- "ss"
            
            se_node <- se_node[which(se_node$time > warm_up), ]
            colnames(se_node)[2] <- "se"
            
            tds_node <- tds_node[which(tds_node$time > warm_up), ]
            colnames(tds_node)[2] <- "tds"
            
            dep_node <- dep_node[which(dep_node$time > warm_up), ]
            colnames(dep_node)[2] <- "dep"
            
            
            tmp1 <- merge(arr_node, ss_node, by = "patient", all = TRUE)
            tmp2 <- merge(tmp1, se_node, by = "patient", all = TRUE)
            tmp3 <- merge(tmp2, tds_node, by = "patient", all = TRUE)
            tmp4 <- merge(tmp3, dep_node, by = "patient", all = TRUE)
            
            tmp4 <- cbind(0, i, tmp4)
            colnames(tmp4)[1] <- "rep"
            colnames(tmp4)[2] <- "node"
            
            tmp4 <- mutate(tmp4, wait = ss - arr)
            tmp4 <- mutate(tmp4, service = se - ss)
            tmp4 <- mutate(tmp4, delayed = tds - se)
            tmp4 <- mutate(tmp4, transition = dep - tds)
            
            tmp4 <-
              tmp4[, c(
                "rep",
                "patient",
                "node",
                "arr",
                "wait",
                "ss",
                "service",
                "se",
                "delayed",
                "tds",
                "transition",
                "dep"
              )]
            tmp4[, "node"] <- node_names[c(tmp4[, "node"]), 2]
            
            
            all_data <- rbindlist(list(all_data, tmp4))
            
          }
          
          
          #all_data<-rbindlist(all_data)
          
          rep_node_dat <- all_data %>% group_by(rep, node)
          
          pat_dat <- all_data %>%
            group_by(patient, rep) %>%
            transmute(
              wait = sum(wait),
              service = sum(service),
              delayed = sum(delayed),
              transition = sum(transition)
            ) %>%
            ungroup() %>%
            group_by(rep)
          
          #change all of the below to include time units? #####
          
          node_wait <-
            as.data.frame(
              summarise(
                rep_node_dat,
                metric = "wait",
                mean = mean(wait, na.rm = T),
                sd = sd(wait, na.rm = T),
                iqr = IQR(wait, na.rm = T),
                percentile_95 = quantile(wait, 0.95, na.rm = T)
              )
            )
          
          node_active_service <-
            as.data.frame(
              summarise(
                rep_node_dat,
                metric = "active_service",
                mean = mean(service, na.rm = T),
                sd = sd(service, na.rm = T),
                iqr = IQR(service, na.rm = T),
                percentile_95 = quantile(
                  x = service,
                  probs = 0.95,
                  na.rm = TRUE
                )
              )
            )
          
          node_capacity_delay <-
            as.data.frame(
              summarise(
                rep_node_dat,
                metric = "capacity_delay",
                mean = mean(delayed, na.rm = T),
                sd = sd(delayed, na.rm = T),
                iqr = IQR(delayed, na.rm = T),
                percentile_95 = quantile(
                  x = delayed,
                  probs = 0.95,
                  na.rm = TRUE
                )
              )
            )
          
          node_transition_delay <-
            as.data.frame(
              summarise(
                rep_node_dat,
                metric = "transition_delay",
                mean = mean(transition, na.rm = T),
                sd = sd(transition, na.rm = T),
                iqr = IQR(transition, na.rm = T),
                percentile_95 = quantile(
                  x = transition,
                  probs = 0.95,
                  na.rm = TRUE
                )
              )
            )
          
          node_length_of_stay <-
            as.data.frame(
              summarise(
                rep_node_dat,
                metric = "length_of_stay",
                mean = mean(service + delayed + transition, na.rm = T),
                sd = sd(service + delayed + transition, na.rm = T),
                iqr = IQR(service + delayed + transition, na.rm = T),
                percentile_95 = quantile(
                  x = service + delayed + transition,
                  probs = 0.95,
                  na.rm = TRUE
                )
              )
            )
          
          node_delay_to_transfer <-
            as.data.frame(
              summarise(
                rep_node_dat,
                metric = "delay_to_transfer",
                mean = mean(delayed + transition, na.rm = T),
                sd = sd(delayed + transition, na.rm = T),
                iqr = IQR(delayed + transition, na.rm = T),
                percentile_95 = quantile(
                  x = delayed + transition,
                  probs = 0.95,
                  na.rm = TRUE
                )
              )
            )
          
          
          pat_wait <-
            as.data.frame(
              summarise(
                pat_dat,
                metric = "wait",
                mean = mean(wait, na.rm = T),
                sd = sd(wait, na.rm = T),
                iqr = IQR(wait, na.rm = T),
                percentile_95 = quantile(wait, 0.95, na.rm = T)
              )
            )
          
          pat_active_service <-
            as.data.frame(
              summarise(
                pat_dat,
                metric = "service",
                mean = mean(service, na.rm = T),
                sd = sd(service, na.rm = T),
                iqr = IQR(service, na.rm = T),
                percentile_95 = quantile(service, 0.95, na.rm = T)
              )
            )
          
          pat_capacity_delay <-
            as.data.frame(
              summarise(
                pat_dat,
                metric = "capacity_delay",
                mean = mean(delayed, na.rm = T),
                sd = sd(delayed, na.rm = T),
                iqr = IQR(delayed, na.rm = T),
                percentile_95 = quantile(delayed, 0.95, na.rm = T)
              )
            )
          
          pat_transition_delay <-
            as.data.frame(
              summarise(
                pat_dat,
                metric = "transition_delay",
                mean = mean(transition, na.rm = T),
                sd = sd(transition, na.rm = T),
                iqr = IQR(transition, na.rm = T),
                percentile_95 = quantile(transition, 0.95, na.rm = T)
              )
            )
          
          pat_length_of_stay <-
            as.data.frame(
              summarise(
                pat_dat,
                metric = "length_of_stay",
                mean = mean(service + delayed + transition, na.rm = T),
                sd = sd(service + delayed + transition, na.rm = T),
                iqr = IQR(service + delayed + transition, na.rm = T),
                percentile_95 = quantile(service + delayed + transition, 0.95, na.rm = T)
              )
            )
          
          pat_delay_to_transfer <-
            as.data.frame(
              summarise(
                pat_dat,
                metric = "delay_to_transfer",
                mean = mean(delayed + transition, na.rm = T),
                sd = sd(delayed + transition, na.rm = T),
                iqr = IQR(delayed + transition, na.rm = T),
                percentile_95 = quantile(delayed + transition, 0.95, na.rm = T)
              )
            )
          
          
          
          ttis_dat <- all_data %>%
            group_by(patient, rep) %>%
            transmute(ttis = max(dep) - min(arr))
          
          total_time_in_system <- ttis_dat %>%
            group_by(rep) %>% summarise(
              node = "ALL",
              metric = "total_time_in_system",
              mean = mean(ttis, na.rm = T),
              sd = sd(ttis, na.rm = T),
              iqr = IQR(ttis, na.rm = T),
              percentile_95 = quantile(ttis, 0.95, na.rm = T)
            )
          
          #all_metrics<-rbind(total_time_in_system,wait,active_service,length_of_stay,delay_to_transfer)
          
          
          
          rm(rep_node_dat, node_dat, pat_dat, all_data)
          
          ### Create the rejected rate metrics #########################################################
          
          rejected <-
            data.frame(node = numeric(),
                       metric = character(),
                       mean = numeric())
          
          for (i in 1:length(nodes)) {
            rej_node <-
              record[which(record$event == "loss" &
                             record$current_node == nodes[i]), c(3, 1)]
            rej_node <- rej_node[which(rej_node$time > warm_up), ]
            colnames(rej_node) <- c("patient", "rejected")
            
            rejected <-
              rbind(
                rejected,
                data.frame(
                  node = syst_names[i, 2],
                  metric = "rejected",
                  mean = nrow(rej_node) / sim_time
                )
              )
            
          }
          
          
          ### Create the delayed metrics ######################################################
          
          delayed <-
            data.frame(
              time = numeric(0),
              event = numeric(0),
              delayed = numeric(0),
              node = numeric(0)
            )
          #print(j)
          for (i in 1:length(nodes)) {
            rec_temp_total <-
              record[which(
                record$current_node == nodes[i] &
                  (
                    record$event == "service_end" | record$event == "transition_start"
                  )
              ), ]
            delayed_change <- as.vector(rec_temp_total$event)
            delayed_change <-
              replace(delayed_change, delayed_change == "service_end", 1)
            delayed_change <-
              replace(delayed_change,
                      delayed_change == "transition_start",
                      -1)
            delayed_change <- as.numeric(delayed_change)
            delayed_change <- cumsum(delayed_change)
            
            delayed <-
              rbind(
                delayed,
                data.frame(
                  time = rec_temp_total$time,
                  event = rec_temp_total$event,
                  delayed = delayed_change,
                  node = nodes[i]
                )
              )
            
          }
          delayed <-
            cbind(delayed, c(diff(delayed$time), delayed$time[nrow(delayed)]))
          colnames(delayed) <-
            c("time",
              "event",
              "delayed",
              "node",
              "time_at_delayed_level")
          if (warm_up > 0) {
            delayed <- delayed[-which(delayed$time < warm_up), ]
          }
          
          
          #Calculating the time at each delayed length##
          
          ptd <-
            data.frame(
              node = numeric(0),
              delayed = numeric(),
              time_at_delayed_level = numeric(),
              percent_time_at_delayed_level = numeric()
            )
          
          
          for (i in 1:length(nodes)) {
            node_delayed <- delayed[which(delayed$node == nodes[i]), ]
            node_delayed <- node_delayed[-nrow(node_delayed), ]
            tmp <-
              data.frame(
                node = numeric(0),
                delayed = numeric(),
                time_at_delayed_level = numeric()
              )
            
            for (k in unique(node_delayed$delayed)) {
              time_at_k <-
                sum(node_delayed$time_at_delayed_level[which(node_delayed$delayed == k)])
              
              tmp <-
                rbind(
                  tmp,
                  data.frame(
                    node = nodes[i],
                    delayed = k,
                    time_at_delayed_level = time_at_k
                  )
                )
            }
            tmp2 <-
              cbind(tmp, (100 * tmp$time_at_delayed_level / (
                sum(tmp$time_at_delayed_level)
              )))
            colnames(tmp2) <-
              c(
                "node",
                "delayed",
                "time_at_delayed_level",
                "percent_time_at_delayed_level"
              )
            
            ptd <- rbind(ptd, tmp2)
          }
          
          
          
          
          #rm(tmp_b_length,tmp_b_time,results,avg_b)
          
          ### Create the delayed through time data ######################################################
          
          if (nrow(delayed) != 0) {
            datd <-
              data.frame(
                time = delayed$time[which(delayed$time_at_delayed_level != 0)],
                delayed = delayed$delayed[which(delayed$time_at_delayed_level != 0)],
                node = as.character(delayed$node[which(delayed$time_at_delayed_level != 0)]),
                rep = paste("rep", 0)
              )
            
            datd$node <- as.numeric(as.character(datd$node))
            datd <- datd[order(datd$node), ]
            datd$node <- syst_names_single[as.numeric(datd$node)]
            datd$node <- as.factor(datd$node)
          } else{
            datd <- data.frame(
              time = 0,
              delayed = 0,
              node = 0,
              rep = paste("rep", 0)
            )
            datd <- datd[0, ]
          }
          
          
          
          
          ### Create the queue metrics ######################################################
          
          #Creating the queue tables###
          
          
          queue <-
            data.frame(
              time = numeric(0),
              event = numeric(0),
              queue_length = numeric(0),
              node = numeric(0)
            )
          
          for (i in 1:length(nodes)) {
            rec_temp_total <-
              record[which(
                record$current_node == nodes[i] &
                  (
                    record$event == "arrival" |
                      record$event == "service_start" | record$event == "loss"
                  )
              ), ]
            queue_change <- as.vector(rec_temp_total$event)
            queue_change <-
              replace(queue_change, queue_change == "arrival", 1)
            queue_change <-
              replace(queue_change, queue_change == "service_start", -1)
            queue_change <- replace(queue_change, queue_change == "loss", -1)
            queue_change <- as.numeric(queue_change)
            queue_change <- cumsum(queue_change)
            
            queue <-
              rbind(
                queue,
                data.frame(
                  time = rec_temp_total$time,
                  event = rec_temp_total$event,
                  queue_length = queue_change,
                  node = nodes[i]
                )
              )
            
          }
          queue <-
            cbind(queue, c(diff(queue$time), queue$time[nrow(queue)]))
          colnames(queue) <-
            c("time",
              "event",
              "queue_length",
              "node",
              "time_at_queue_length")
          if (warm_up > 0) {
            queue <- queue[-which(queue$time < warm_up), ]
          }
          
          
          
          #Calculating the time at each queue length##
          
          ptq <-
            data.frame(
              node = numeric(0),
              queue = numeric(),
              time_at_queue_length = numeric(),
              percent_time_at_queue_length = numeric()
            )
          
          
          for (i in 1:length(nodes)) {
            node_queue <- queue[which(queue$node == nodes[i]), ]
            node_queue <- node_queue[-nrow(node_queue), ]
            tmp <-
              data.frame(
                node = numeric(0),
                queue = numeric(),
                time_at_queue_length = numeric()
              )
            
            for (k in unique(node_queue$queue)) {
              time_at_k <-
                sum(node_queue$time_at_queue_length[which(node_queue$queue == k)])
              
              tmp <-
                rbind(tmp,
                      data.frame(
                        node = nodes[i],
                        queue = k,
                        time_at_queue_length = time_at_k
                      ))
            }
            tmp2 <-
              cbind(tmp, (100 * tmp$time_at_queue_length / (
                sum(tmp$time_at_queue_length)
              )))
            colnames(tmp2) <-
              c("node",
                "queue",
                "time_at_queue_length",
                "percent_time_at_queue_length")
            ptq <- rbind(ptq, tmp2)
          }
          
          
          ### Create the queue through time data ######################################################
          
          if (nrow(queue) != 0) {
            datq <-
              data.frame(
                time = queue$time[which(queue$time_at_queue_length != 0)],
                queue_length = queue$queue_length[which(queue$time_at_queue_length != 0)],
                node = as.character(queue$node[which(queue$time_at_queue_length != 0)]),
                rep = paste("rep", 0)
              )
            
            datq$node <- as.numeric(as.character(datq$node))
            datq <- datq[order(datq$node), ]
            datq$node <- syst_names_single[as.numeric(datq$node)]
            datq$node <- as.factor(datq$node)
          } else{
            datq <- data.frame(
              time = 0,
              queue_length = 0,
              node = 0,
              rep = paste("rep", 0)
            )
            datq <- datq[0, ]
          }
          
          
          
          ### Create the occupancy metrics ######################################################
          
          occupancy <-
            data.frame(
              time = numeric(0),
              event = numeric(0),
              occupancy = numeric(0),
              occupancy_prop = numeric(0),
              capacity = numeric(),
              remainder_time = numeric(0),
              node = numeric(0)
            )
          
          
          for (i in 1:length(nodes)) {
            rec_temp_total <-
              record[which(
                record$current_node == nodes[i] &
                  (
                    record$event == "service_start" | record$event == "departure"
                  )
              ), ]
            cap_cal_input_temp <-
              cap_cal_input[which(cap_cal_input$node == i), ]
            if (nrow(cap_cal_input_temp) == 1) {
              cap_cal_input_temp$end <- sim_time
            }
            
            occupancy_change <- as.vector(rec_temp_total$event)
            occupancy_change <-
              replace(occupancy_change,
                      occupancy_change == "service_start",
                      1)
            occupancy_change <-
              replace(occupancy_change,
                      occupancy_change == "departure",
                      -1)
            occupancy_change <- as.numeric(occupancy_change)
            occupancy_change <- cumsum(occupancy_change)
            
            rt <-
              rec_temp_total$time %% max(cap_cal_input_temp$end[which(cap_cal_input_temp$node ==
                                                                        i)])
            
            tmp <-
              data.frame(
                time = rec_temp_total$time,
                event = rec_temp_total$event,
                occupancy = occupancy_change,
                occupancy_prop = NA,
                capacity = NA,
                remainder_time = rt,
                node = nodes[i]
              )
            
            for (time_gap in 1:nrow(cap_cal_input_temp)) {
              tmp$capacity[which(
                tmp$remainder_time >= cap_cal_input_temp$start[time_gap] &
                  tmp$remainder_time < cap_cal_input_temp$end[time_gap]
              )] <- cap_cal_input_temp$value[time_gap]
              
            }
            
            tmp$occupancy_prop = tmp$occupancy / tmp$capacity
            tmp$occupancy_prop[which(tmp$occupancy_prop == Inf |
                                       tmp$occupancy_prop >= 1)] <- 1
            tmp$occupancy_prop <- tmp$occupancy_prop * 100
            
            occupancy <- rbind(occupancy, tmp)
            
          }
          occupancy <-
            cbind(occupancy, c(diff(occupancy$time), occupancy$time[nrow(occupancy)]))
          colnames(occupancy) <-
            c(
              "time",
              "event",
              "occupancy",
              "occupancy_prop",
              "capacity",
              "remainder_time",
              "node",
              "time_at_occupancy"
            )
          if (warm_up > 0) {
            occupancy <- occupancy[-which(occupancy$time < warm_up), ]
          }
          
          
          
          
          #Calculating the time at each occupancy##
          
          
          pto <-
            data.frame(
              node = numeric(0),
              occupancy = numeric(),
              time_at_occupancy = numeric(),
              percent_time_at_occupancy = numeric()
            )
          
          
          for (i in 1:length(nodes)) {
            node_occupancy <- occupancy[which(occupancy$node == nodes[i]), ]
            node_occupancy <- node_occupancy[-nrow(node_occupancy), ]
            tmp <-
              data.frame(
                node = numeric(0),
                occupancy = numeric(),
                time_at_occupancy = numeric()
              )
            
            for (k in unique(node_occupancy$occupancy)) {
              time_at_k <-
                sum(node_occupancy$time_at_occupancy[which(node_occupancy$occupancy == k)])
              
              tmp <-
                rbind(
                  tmp,
                  data.frame(
                    node = nodes[i],
                    occupancy = k,
                    time_at_occupancy = time_at_k
                  )
                )
            }
            tmp2 <-
              cbind(tmp, (100 * tmp$time_at_occupancy / (sum(
                tmp$time_at_occupancy
              ))))
            colnames(tmp2) <-
              c("node",
                "occupancy",
                "time_at_occupancy",
                "percent_time_at_occupancy")
            pto <- rbind(pto, tmp2)
          }
          
          
          #rm(results,tmp,node_occupancy,time_at_k,tmp2)
          
          
          
          ### Create the occupancy through time data ######################################################
          if (nrow(occupancy) != 0) {
            dato <-
              data.frame(
                time = occupancy$time[which(occupancy$time_at_occupancy != 0)],
                occupancy = occupancy$occupancy[which(occupancy$time_at_occupancy != 0)],
                node = occupancy$node[which(occupancy$time_at_occupancy != 0)],
                rep = paste("rep", 0)
              )
            
            dato$node <- as.numeric(as.character(dato$node))
            dato <- dato[order(dato$node), ]
            dato$node <- syst_names_single[as.numeric(dato$node)]
            dato$node <- as.factor(dato$node)
          } else{
            dato <- data.frame(
              time = 0,
              occupancy = 0,
              node = 0,
              rep = paste("rep", 0)
            )
            dato <- dato[0, ]
            
            
          }
          
          
          ### Create the transition metrics ######################################################
          
          
          transition <-
            data.frame(
              time = numeric(0),
              event = numeric(0),
              transition = numeric(0),
              node = numeric(0)
            )
          
          for (i in 1:length(nodes)) {
            rec_temp_total <-
              record[which(
                record$current_node == nodes[i] &
                  (
                    record$event == "transition_start" | record$event == "departure"
                  )
              ), ]
            transition_change <- as.vector(rec_temp_total$event)
            transition_change <-
              replace(transition_change,
                      transition_change == "transition_start",
                      1)
            transition_change <-
              replace(transition_change,
                      transition_change == "departure",
                      -1)
            transition_change <- as.numeric(transition_change)
            transition_change <- cumsum(transition_change)
            
            transition <-
              rbind(
                transition,
                data.frame(
                  time = rec_temp_total$time,
                  event = rec_temp_total$event,
                  transition = transition_change,
                  node = nodes[i]
                )
              )
            
          }
          transition <-
            cbind(transition, c(diff(transition$time), transition$time[nrow(transition)]))
          colnames(transition) <-
            c("time",
              "event",
              "transition",
              "node",
              "time_at_transition_level")
          if (warm_up > 0) {
            transition <- transition[-which(transition$time < warm_up), ]
          }
          
          
          #rm(rec_temp_total,results,transition_change)
          
          
          #Calculating the time at each transition length##
          
          
          
          ptt <-
            data.frame(
              node = numeric(0),
              transition = numeric(),
              time_at_transition_level = numeric(),
              percent_time_at_transition_level = numeric()
            )
          
          
          for (i in 1:length(nodes)) {
            node_transition <- transition[which(transition$node == nodes[i]), ]
            node_transition <- node_transition[-nrow(node_transition), ]
            tmp <-
              data.frame(
                node = numeric(0),
                transition = numeric(),
                time_at_transition_level = numeric()
              )
            
            for (k in unique(node_transition$transition)) {
              time_at_k <-
                sum(node_transition$time_at_transition_level[which(node_transition$transition ==
                                                                     k)])
              
              tmp <-
                rbind(
                  tmp,
                  data.frame(
                    node = nodes[i],
                    transition = k,
                    time_at_transition_level = time_at_k
                  )
                )
            }
            tmp2 <-
              cbind(tmp, (100 * tmp$time_at_transition_level / (
                sum(tmp$time_at_transition_level)
              )))
            colnames(tmp2) <-
              c(
                "node",
                "transition",
                "time_at_transition_level",
                "percent_time_at_transition_level"
              )
            
            ptt <- rbind(ptt, tmp2)
          }
          
          #rm(results,tmp,node_transition,time_at_k,tmp2)
          
          
          
          ### Create the transition through time data ######################################################
          
          if (nrow(transition) != 0) {
            datt <-
              data.frame(
                time = transition$time[which(transition$time_at_transition_level != 0)],
                transition = transition$transition[which(transition$time_at_transition_level !=
                                                           0)],
                node = as.character(transition$node[which(transition$time_at_transition_level !=
                                                            0)]),
                rep = paste("rep", 0)
              )
            
            datt$node <- as.numeric(as.character(datt$node))
            datt <- datt[order(datt$node), ]
            datt$node <- syst_names_single[as.numeric(datt$node)]
            datt$node <- as.factor(datt$node)
          } else{
            datt <- data.frame(
              time = 0,
              transition = 0,
              node = 0,
              rep = paste("rep", 0)
            )
            datt <- datt[0, ]
            
          }
          
          ### Create the occ_bed metrics ######################################################
          
          
          occ_bed <-
            data.frame(time = numeric(0),
                       occ_bed = numeric(0),
                       node = numeric(0))
          
          for (i in 1:length(nodes)) {
            rec_temp_total <- bed[which(bed$node == nodes[i]), ]
            
            occ_bed <-
              rbind(
                occ_bed,
                data.frame(
                  time = rec_temp_total$time,
                  occ_bed = rec_temp_total$bed,
                  node = nodes[i]
                )
              )
            
          }
          occ_bed <-
            cbind(occ_bed, c(diff(occ_bed$time), occ_bed$time[nrow(occ_bed)]))
          colnames(occ_bed) <-
            c("time", "occ_bed", "node", "time_at_occ_bed_level")
          if (warm_up > 0) {
            occ_bed <- occ_bed[-which(occ_bed$time < warm_up), ]
          }
          occ_bed
          
          
          #### % time at bed occupancy level#
          
          
          
          ptb <-
            data.frame(
              node = numeric(0),
              occ_bed = numeric(),
              time_at_occ_bed_level = numeric(),
              percent_time_at_occ_bed_level = numeric()
            )
          
          
          for (i in 1:length(nodes)) {
            node_occ_bed <- occ_bed[which(occ_bed$node == nodes[i]), ]
            node_occ_bed <- node_occ_bed[-nrow(node_occ_bed), ]
            tmp <-
              data.frame(
                node = numeric(0),
                occ_bed = numeric(),
                time_at_occ_bed_level = numeric()
              )
            
            for (k in unique(node_occ_bed$occ_bed)) {
              time_at_k <-
                sum(node_occ_bed$time_at_occ_bed_level[which(node_occ_bed$occ_bed == k)])
              
              tmp <-
                rbind(
                  tmp,
                  data.frame(
                    node = nodes[i],
                    occ_bed = k,
                    time_at_occ_bed_level = time_at_k
                  )
                )
            }
            tmp2 <-
              cbind(tmp, (100 * tmp$time_at_occ_bed_level / (
                sum(tmp$time_at_occ_bed_level)
              )))
            colnames(tmp2) <-
              c(
                "node",
                "occ_bed",
                "time_at_occ_bed_level",
                "percent_time_at_occ_bed_level"
              )
            ptb <- rbind(ptb, tmp2)
          }
          
          
          
          ### Create the occ_bed through time data ######################################################
          
          
          if (nrow(occ_bed) != 0) {
            datb <-
              data.frame(
                time = occ_bed$time[which(occ_bed$time_at_occ_bed_level != 0)],
                occ_bed = occ_bed$occ_bed[which(occ_bed$time_at_occ_bed_level != 0)],
                node = occ_bed$node[which(occ_bed$time_at_occ_bed_level != 0)],
                rep = paste("rep", 0)
              )
            
            datb$node <- as.numeric(as.character(datb$node))
            datb <- datb[order(datb$node), ]
            datb$node <- syst_names_single[as.numeric(datb$node)]
            datb$node <- as.factor(datb$node)
          } else{
            datb <- data.frame(
              time = 0,
              occ_bed = 0,
              node = 0,
              rep = paste("rep", 0)
            )
            datb <- datb[0, ]
            
          }
          
          ### Create the multi data & through time uniform ######################################################
          
          dato_multi <- cbind(dato, rep(x = "occupancy", nrow(dato)))
          colnames(dato_multi) <- c("time", "value", "node", "rep", "metric")
          
          datd_multi <- cbind(datd, rep(x = "delayed", nrow(datd)))
          colnames(datd_multi) <- c("time", "value", "node", "rep", "metric")
          
          datb_multi <- cbind(datb, rep(x = "occ_bed", nrow(datb)))
          colnames(datb_multi) <- c("time", "value", "node", "rep", "metric")
          
          datt_multi <- cbind(datt, rep(x = "transition", nrow(datt)))
          colnames(datt_multi) <- c("time", "value", "node", "rep", "metric")
          
          datq_multi <- cbind(datq, rep(x = "queue", nrow(datq)))
          colnames(datq_multi) <- c("time", "value", "node", "rep", "metric")
          
          
          library(data.table)
          library(tidyverse)
          multi <-
            rbindlist(list(
              datb_multi,
              datd_multi,
              dato_multi,
              datt_multi,
              datq_multi
            ))
          
          
          
          multi_spread <- spread(data = multi,
                                 key = metric,
                                 value = value)
          
          
          multi_spread_uniform <-
            data.frame(
              time = numeric(),
              node = numeric(),
              rep = numeric(),
              occ_bed = numeric(),
              delayed = numeric(),
              occupancy = numeric(),
              transition = numeric(),
              queue = numeric()
            )
          
          uniform_time <- seq(from = warm_up,
                              to = t.period,
                              by = 0.5)
          
          
          
          for (i in nodes) {
            base <-
              multi_spread[which(as.character(multi_spread$node) == node_names[i, 2]), ] ## Reassigns names
            
            uniform_ts <-
              data.frame(
                time = uniform_time,
                node = node_names[i, 2],
                rep = NA,
                occ_bed = NA,
                delayed = NA,
                occupancy = NA,
                transition = NA,
                queue = NA
              )
            
            uniform_ts <-
              rbindlist(list(base, uniform_ts),
                        fill = T,
                        use.names = T)
            
            uniform_ts <- uniform_ts[order(uniform_ts$time), ]
            
            uniform_ts <-
              uniform_ts %>% fill(rep, occ_bed, delayed, occupancy, transition, queue) ## tidyr::fill function changes the NA values to the previous value down the df
            
            uniform_ts <-
              uniform_ts %>% fill(rep,
                                  occ_bed,
                                  delayed,
                                  occupancy,
                                  transition,
                                  queue,
                                  .direction = "up") ## tidyr::fill function changes the NA values to the pervious value up the df
            
            multi_spread_uniform <-
              rbindlist(list(multi_spread_uniform, uniform_ts), use.names = T)
            
          }
          
          multi_spread_uniform <-
            multi_spread_uniform[which(multi_spread_uniform$time %in% uniform_time), ]
          
          
          
          
          
          
          x <-
            list(
              nodes,
              warm_up,
              sim_time,
              reps,
              exits,
              syst_names,
              node_wait,
              node_active_service,
              node_length_of_stay,
              node_delay_to_transfer,
              pat_wait,
              pat_active_service,
              pat_length_of_stay,
              pat_delay_to_transfer,
              total_time_in_system,
              rejected,
              ptd,
              ptq,
              pto,
              ptt,
              ptb,
              multi_spread_uniform,
              delay_list,
              cap_cal_input,
              arr_cal_input,
              node_capacity_delay,
              node_transition_delay,
              pat_capacity_delay,
              pat_transition_delay
            )
          
          names(x) <-
            c(
              "nodes",
              "warm_up",
              "sim_time",
              "reps",
              "exits",
              "syst_names",
              "node_wait",
              "node_active_service",
              "node_length_of_stay",
              "node_delay_to_transfer",
              "pat_wait",
              "pat_active_service",
              "pat_length_of_stay",
              "pat_delay_to_transfer",
              "total_time_in_system",
              "rejected",
              "ptd",
              "ptq",
              "pto",
              "ptt",
              "ptb",
              "multi_spread_uniform",
              "delay_list",
              "node_capacity_delay",
              "node_transition_delay",
              "pat_capacity_delay",
              "pat_transition_delay"
            )
          
          
          rm(
            record,
            datd,
            datq,
            dato,
            datt,
            datb,
            datq_multi,
            datd_multi,
            dato_multi,
            datt_multi,
            datb_multi,
            multi,
            multi_spread
          )
          #gc()
          
          return(x)
          
          
        }
      )
      #stopCluster(cl)
      
      
      #### PLOTS AND SIMULATION LEVEL METRICS #########
      nodes <- outputs[[1]][[1]]
      warm_up <- outputs[[1]][[2]]
      sim_time <- outputs[[1]][[3]]
      reps <- outputs[[1]][[4]]
      exits <- outputs[[1]][[5]]
      syst_names <- outputs[[1]][[6]]
      delay_list <- outputs[[1]][[23]]
      cap_cal_input <- outputs[[1]][[24]]
      arr_cal_input <- outputs[[1]][[25]]
      
      node_wait <- sapply(outputs, function(x)
        x[7])
      node_active_service <- sapply(outputs, function(x)
        x[8])
      node_length_of_stay <- sapply(outputs, function(x)
        x[9])
      node_delay_to_transfer <- sapply(outputs, function(x)
        x[10])
      pat_wait <- sapply(outputs, function(x)
        x[11])
      pat_active_service <- sapply(outputs, function(x)
        x[12])
      pat_length_of_stay <- sapply(outputs, function(x)
        x[13])
      pat_delay_to_transfer <- sapply(outputs, function(x)
        x[14])
      total_time_in_system <- sapply(outputs, function(x)
        x[15])
      rejected <- sapply(outputs, function(x)
        x[16])
      ptd <- sapply(outputs, function(x)
        x[17])
      ptq <- sapply(outputs, function(x)
        x[18])
      pto <- sapply(outputs, function(x)
        x[19])
      ptt <- sapply(outputs, function(x)
        x[20])
      ptb <- sapply(outputs, function(x)
        x[21])
      multi_spread_uniform <- sapply(outputs, function(x)
        x[22])
      
      node_capacity_delay <- sapply(outputs, function(x)
        x[26])
      node_transition_delay <- sapply(outputs, function(x)
        x[27])
      
      pat_capacity_delay <- sapply(outputs, function(x)
        x[28])
      pat_transition_delay <- sapply(outputs, function(x)
        x[29])
      
      rm(outputs)
      
      
      ### Create the Simulation Summary Metrics ######################################################
      
      for (rep_fill in 1:reps) {
        if (!is.na(node_wait[[rep_fill]]$rep[1])) {
          node_wait[[rep_fill]]$rep <- rep_fill
        }
        if (!is.na(node_active_service[[rep_fill]]$rep[1])) {
          node_active_service[[rep_fill]]$rep <- rep_fill
        }
        if (!is.na(node_capacity_delay[[rep_fill]]$rep[1])) {
          node_capacity_delay[[rep_fill]]$rep <- rep_fill
        }
        if (!is.na(node_transition_delay[[rep_fill]]$rep[1])) {
          node_transition_delay[[rep_fill]]$rep <- rep_fill
        }
        if (!is.na(node_length_of_stay[[rep_fill]]$rep[1])) {
          node_length_of_stay[[rep_fill]]$rep <- rep_fill
        }
        if (!is.na(node_delay_to_transfer[[rep_fill]]$rep[1])) {
          node_delay_to_transfer[[rep_fill]]$rep <- rep_fill
        }
        
        if (!is.na(pat_wait[[rep_fill]]$rep[1])) {
          pat_wait[[rep_fill]]$rep <- rep_fill
        }
        if (!is.na(pat_active_service[[rep_fill]]$rep[1])) {
          pat_active_service[[rep_fill]]$rep <- rep_fill
        }
        if (!is.na(pat_capacity_delay[[rep_fill]]$rep[1])) {
          pat_capacity_delay[[rep_fill]]$rep <- rep_fill
        }
        if (!is.na(pat_transition_delay[[rep_fill]]$rep[1])) {
          pat_transition_delay[[rep_fill]]$rep <- rep_fill
        }
        if (!is.na(pat_length_of_stay[[rep_fill]]$rep[1])) {
          pat_length_of_stay[[rep_fill]]$rep <- rep_fill
        }
        if (!is.na(pat_delay_to_transfer[[rep_fill]]$rep[1])) {
          pat_delay_to_transfer[[rep_fill]]$rep <- rep_fill
        }
        
        if (!is.na(total_time_in_system[[rep_fill]]$rep[1])) {
          total_time_in_system[[rep_fill]]$rep <- rep_fill
        }
        
        if (!is.na(rejected[[rep_fill]][1, 1])) {
          rejected[[rep_fill]]$rep <- rep_fill
        }
        
        if (!is.na(multi_spread_uniform[[rep_fill]][1, 1])) {
          multi_spread_uniform[[rep_fill]]$rep <- paste0("rep ", rep_fill)
        }
        
      }
      
      
      
      node_wait <- rbindlist(node_wait)
      node_active_service <- rbindlist(node_active_service)
      node_capacity_delay <- rbindlist(node_capacity_delay)
      node_transition_delay <- rbindlist(node_transition_delay)
      node_length_of_stay <- rbindlist(node_length_of_stay)
      node_delay_to_transfer <- rbindlist(node_delay_to_transfer)
      
      pat_wait <- rbindlist(pat_wait)
      pat_active_service <- rbindlist(pat_active_service)
      pat_capacity_delay <- rbindlist(pat_capacity_delay)
      pat_transition_delay <- rbindlist(pat_transition_delay)
      pat_length_of_stay <- rbindlist(pat_length_of_stay)
      pat_delay_to_transfer <- rbindlist(pat_delay_to_transfer)
      
      total_time_in_system <- rbindlist(total_time_in_system)
      rejected <- rbindlist(rejected)
      
      node_wait_summary <-
        node_wait %>% group_by(node) %>% summarise(
          metric = "wait",
          mean = mean(mean, na.rm = T),
          sd = mean(sd, na.rm = T),
          iqr = mean(iqr, na.rm = T),
          percentile_95 = mean(percentile_95, na.rm = T)
        ) %>% as.data.frame()
      node_active_service_summary <-
        node_active_service %>% group_by(node) %>% summarise(
          metric = "active_service",
          mean = mean(mean, na.rm = T),
          sd = mean(sd, na.rm = T),
          iqr = mean(iqr, na.rm = T),
          percentile_95 = mean(percentile_95, na.rm = T)
        ) %>% as.data.frame()
      node_capacity_delay_summary <-
        node_capacity_delay %>% group_by(node) %>% summarise(
          metric = "capacity_delay",
          mean = mean(mean, na.rm = T),
          sd = mean(sd, na.rm = T),
          iqr = mean(iqr, na.rm = T),
          percentile_95 = mean(percentile_95, na.rm = T)
        ) %>% as.data.frame()
      node_transition_delay_summary <-
        node_transition_delay %>% group_by(node) %>% summarise(
          metric = "transition_delay",
          mean = mean(mean, na.rm = T),
          sd = mean(sd, na.rm = T),
          iqr = mean(iqr, na.rm = T),
          percentile_95 = mean(percentile_95, na.rm = T)
        ) %>% as.data.frame()
      node_length_of_stay_summary <-
        node_length_of_stay %>% group_by(node) %>% summarise(
          metric = "length_of_stay",
          mean = mean(mean, na.rm = T),
          sd = mean(sd, na.rm = T),
          iqr = mean(iqr, na.rm = T),
          percentile_95 = mean(percentile_95, na.rm = T)
        ) %>% as.data.frame()
      node_delay_to_transfer_summary <-
        node_delay_to_transfer %>% group_by(node) %>% summarise(
          metric = "delay_to_transfer",
          mean = mean(mean, na.rm = T),
          sd = mean(sd, na.rm = T),
          iqr = mean(iqr, na.rm = T),
          percentile_95 = mean(percentile_95, na.rm = T)
        ) %>% as.data.frame()
      
      pat_wait_summary <-
        pat_wait %>% summarise(
          metric = "wait",
          mean = mean(mean, na.rm = T),
          sd = mean(sd, na.rm = T),
          iqr = mean(iqr, na.rm = T),
          percentile_95 = mean(percentile_95, na.rm = T)
        ) %>% as.data.frame()
      pat_active_service_summary <-
        pat_active_service  %>% summarise(
          metric = "active_service",
          mean = mean(mean, na.rm = T),
          sd = mean(sd, na.rm = T),
          iqr = mean(iqr, na.rm = T),
          percentile_95 = mean(percentile_95, na.rm = T)
        ) %>% as.data.frame()
      pat_capacity_delay_summary <-
        pat_capacity_delay  %>% summarise(
          metric = "capacity_delay",
          mean = mean(mean, na.rm = T),
          sd = mean(sd, na.rm = T),
          iqr = mean(iqr, na.rm = T),
          percentile_95 = mean(percentile_95, na.rm = T)
        ) %>% as.data.frame()
      pat_transition_delay_summary <-
        pat_transition_delay  %>% summarise(
          metric = "transition_delay",
          mean = mean(mean, na.rm = T),
          sd = mean(sd, na.rm = T),
          iqr = mean(iqr, na.rm = T),
          percentile_95 = mean(percentile_95, na.rm = T)
        ) %>% as.data.frame()
      pat_length_of_stay_summary <-
        pat_length_of_stay  %>% summarise(
          metric = "length_of_stay",
          mean = mean(mean, na.rm = T),
          sd = mean(sd, na.rm = T),
          iqr = mean(iqr, na.rm = T),
          percentile_95 = mean(percentile_95, na.rm = T)
        ) %>% as.data.frame()
      pat_delay_to_transfer_summary <-
        pat_delay_to_transfer %>% summarise(
          metric = "delay_to_transfer",
          mean = mean(mean, na.rm = T),
          sd = mean(sd, na.rm = T),
          iqr = mean(iqr, na.rm = T),
          percentile_95 = mean(percentile_95, na.rm = T)
        ) %>% as.data.frame()
      
      total_time_in_system_summary <-
        total_time_in_system %>% summarise(
          metric = "total_time_in_system",
          mean = mean(mean, na.rm = T),
          sd = mean(sd, na.rm = T),
          iqr = mean(iqr, na.rm = T),
          percentile_95 = mean(percentile_95, na.rm = T)
        ) %>% as.data.frame()
      
      pat_rep_summary <-
        rbind(
          pat_wait,
          pat_active_service,
          pat_capacity_delay,
          pat_transition_delay,
          pat_length_of_stay,
          pat_delay_to_transfer
        )
      pat_total_summary <-
        rbind(
          pat_wait_summary,
          pat_active_service_summary,
          pat_capacity_delay_summary,
          pat_transition_delay_summary,
          pat_length_of_stay_summary,
          pat_delay_to_transfer_summary
        )
      
      
      rejected_summary <-
        rejected %>% group_by(node) %>% summarise(mean = mean(mean)) %>% as.data.frame()
      
      ### Create the delayed metrics ######################################################
      
      
      #Calculating the time at each delayed length##
      
      
      ptd_total <- as.data.frame(rbindlist(ptd))
      rownames(ptd_total) <- c()
      ptd_total$delayed <- as.numeric(as.character(ptd_total$delayed))
      ptd_total$time_at_delayed_level <-
        as.numeric(as.character(ptd_total$time_at_delayed_level))
      ptd_total <- ptd_total[, -4]
      
      ptd_time <-
        ptd_total %>% group_by(node, delayed) %>% mutate(time_at_delayed_level =
                                                           sum(time_at_delayed_level) / reps)
      ptd_time <- as.data.frame(ptd_time)
      ptd_time <- unique(ptd_time)
      ptd_time$node <- as.numeric(as.character(ptd_time$node))
      ptd_time <- ptd_time[order(ptd_time$node, ptd_time$delayed), ]
      
      ptd_percent <-
        ptd_time %>% group_by(node) %>% transmute(
          delayed,
          percent_time_at_delayed_level = 100 * time_at_delayed_level / sum(time_at_delayed_level)
        )
      ptd_percent <-
        ptd_percent %>% group_by(node) %>% transmute(
          delayed,
          percent_time_at_delayed_level,
          cumulative_percent_time_at_delayed_level = cumsum(percent_time_at_delayed_level)
        )
      ptd_percent <- as.data.frame(ptd_percent)
      ptd_percent <- unique(ptd_percent)
      ptd_percent$node <- as.numeric(as.character(ptd_percent$node))
      ptd_percent <-
        ptd_percent[order(ptd_percent$node, ptd_percent$delayed), ]
      ptd_percent$node <- as.factor(ptd_percent$node)
      ptd_percent$node <-
        syst_names_single[as.numeric(as.character(ptd_percent$node))]
      ptd_percent$node <- as.factor(ptd_percent$node)
      
      
      ptd_percent$node <-
        factor(x = ptd_percent$node, levels = syst_names_single)
      
      ptd_plot <-
        ggplot(data = ptd_percent %>% mutate(node=str_replace_all(node,pattern="_",replacement=" ")),
               aes(x = delayed, y = percent_time_at_delayed_level, fill = node)) +
        geom_bar(stat = "identity", position = position_dodge()) + 
        facet_grid(node ~ ., labeller=label_wrap_gen(15)) + 
        theme_bw() +
        #geom_text(aes(label=ifelse(signif(x = ptd_percent$percent_time_at_delayed_level,digits = 3)<100,signif(x = ptd_percent$percent_time_at_delayed_level,digits = 2),"")),vjust=-0.5,position = position_dodge(width=0.9),  size=3)+ coord_cartesian(ylim = c(0,100))+
        xlab("# concurrently delayed") + 
        ylab("% time at delayed level") +
        theme(legend.position="none")
      
      if (max(ptd_percent$delayed) == 1) {
        ptd_plot <-
          ptd_plot + scale_x_discrete(limits = c(min(ptd_percent$delayed), max(ptd_percent$delayed)))
      }
      
      #ptd_plot
      
      #Delay Percentiles##
      
      dpercentiles <- matrix(nrow = length(nodes), ncol = 8)
      
      for (i in as.numeric(nodes)) {
        if (length(unique(
          ptd_percent$cumulative_percent_time_at_delayed_level[which(ptd_percent$node ==
                                                                     syst_names_single[i])]
        )) >= 2) {
          tmp <-
            approx(
              x = ptd_percent$cumulative_percent_time_at_delayed_level[which(ptd_percent$node ==
                                                                               syst_names_single[i])],
              y = ptd_percent$delayed[which(ptd_percent$node == syst_names_single[i])],
              xout = c(50, 80, 85, 90, 95, 99, 100),
              ties = min,
              rule = 2
            )
          tmp$y <- round(tmp$y, digits = 2)
          dpercentiles[as.numeric(i), ] <-
            c(syst_names_single[i], as.numeric(tmp$y))
        }
        else if (length(unique(
          ptd_percent$cumulative_percent_time_at_delayed_level[which(ptd_percent$node ==
                                                                     syst_names_single[i])]
        )) == 0) {
          dpercentiles[as.numeric(i), ] <-
            c(syst_names_single[i], rep(x = NA, times = 7))
        }
        else{
          dpercentiles[as.numeric(i), ] <-
            c(syst_names_single[i], rep(x = 0, times = 7))
        }
        
      }
      colnames(dpercentiles) <-
        c("node",
          "50th",
          "80th",
          "85th",
          "90th",
          "95th",
          "99th",
          "100th")
      
      #Calculating the average delayed per node per replicate & then over the simulation per node##
      #
      
      #cl<-makeCluster(17)
      #clusterExport(cl = cl,varlist = c("ptd","nodes","node_names"))
      
      avg_delayed <- lapply(
        X = ptd,
        FUN = function(ptd) {
          tmp <-
            ptd %>% group_by(node) %>% summarise(avg_delayed = sum(delayed * time_at_delayed_level) /
                                                   sum(time_at_delayed_level)) %>% as.data.frame()
          tmp$node <- node_names[tmp$node, 2]
          tmp
        }
      )
      #stopCluster(cl)
      
      
      avg_delayed_summary <-
        ptd_time %>% group_by(node) %>% summarise(avg_delayed = sum(delayed * time_at_delayed_level) /
                                                    sum(time_at_delayed_level)) %>% as.data.frame()
      avg_delayed_summary$node <- node_names[avg_delayed_summary$node, 2]
      
      
      # avg_delayed<-data.frame(abind(avg_delayed,along = 1))
      # avg_delayed$avg_delayed<-as.numeric(as.character(avg_delayed$avg_delayed))
      
      #rm(tmp_b_length,tmp_b_time,results,avg_b)
      
      
      
      
      ### Create the queue metrics ######################################################
      
      
      #Calculating the time at each queue length##
      
      
      
      ptq_total <- as.data.frame(rbindlist(ptq))
      rownames(ptq_total) <- c()
      ptq_total$queue <- as.numeric(as.character(ptq_total$queue))
      ptq_total$time_at_queue_length <-
        as.numeric(as.character(ptq_total$time_at_queue_length))
      ptq_total <- ptq_total[, -4]
      
      ptq_time <-
        ptq_total %>% group_by(node, queue) %>% mutate(time_at_queue_length = sum(time_at_queue_length) /
                                                         reps)
      ptq_time <- as.data.frame(ptq_time)
      ptq_time <- unique(ptq_time)
      ptq_time$node <- as.numeric(as.character(ptq_time$node))
      ptq_time <- ptq_time[order(ptq_time$node, ptq_time$queue), ]
      
      ptq_percent <-
        ptq_time %>% group_by(node) %>% transmute(
          queue,
          percent_time_at_queue_length = 100 * time_at_queue_length / sum(time_at_queue_length)
        )
      ptq_percent <-
        ptq_percent %>% group_by(node) %>% transmute(
          queue,
          percent_time_at_queue_length,
          cumulative_percent_time_at_queue_length = cumsum(percent_time_at_queue_length)
        )
      ptq_percent <- as.data.frame(ptq_percent)
      ptq_percent <- unique(ptq_percent)
      ptq_percent$node <- as.numeric(as.character(ptq_percent$node))
      ptq_percent <-
        ptq_percent[order(ptq_percent$node, ptq_percent$queue), ]
      ptq_percent$node <- as.factor(ptq_percent$node)
      ptq_percent$node <-
        syst_names_single[as.numeric(as.character(ptq_percent$node))]
      ptq_percent$node <- as.factor(ptq_percent$node)
      
      ptq_percent$node <-
        factor(x = ptq_percent$node, levels = syst_names_single)
      
      ptq_plot <-
        ggplot(data = ptq_percent %>% mutate(node=str_replace_all(node,pattern="_",replacement=" ")),
               aes(x = queue, y = percent_time_at_queue_length, fill = node)) +
        geom_bar(stat = "identity", position = position_dodge()) + 
        facet_grid(node ~ ., scales = "free", labeller=label_wrap_gen(15)) +
        theme_bw() +
        xlab("# in queue") +
        ylab("% time at queue level") +
        theme(legend.position="none")
      
      if (max(ptq_percent$queue) == 1) {
        ptq_plot <-
          ptq_plot + scale_x_discrete(limits = c(min(ptq_percent$queue), max(ptq_percent$queue)))
      }
      
      #ptq_plot
      
      
      
      
      #Queue Percentiles##
      
      qpercentiles <- matrix(nrow = length(nodes), ncol = 8)
      
      for (i in as.numeric(nodes)) {
        if (length(unique(
          ptq_percent$cumulative_percent_time_at_queue_length[which(ptq_percent$node ==
                                                                    syst_names_single[i])]
        )) >= 2) {
          tmp <-
            approx(
              x = ptq_percent$cumulative_percent_time_at_queue_length[which(ptq_percent$node ==
                                                                              syst_names_single[i])],
              y = ptq_percent$queue[which(ptq_percent$node == syst_names_single[i])],
              xout = c(50, 80, 85, 90, 95, 99, 100),
              ties = min,
              rule = 2
            )
          
          tmp$y <- round(tmp$y, digits = 2)
          
          qpercentiles[as.numeric(i), ] <- c(syst_names_single[i], tmp$y)
        }
        else if (length(unique(
          ptq_percent$cumulative_percent_time_at_queue_length[which(ptq_percent$node ==
                                                                    syst_names_single[i])]
        )) == 0) {
          qpercentiles[as.numeric(i), ] <-
            c(syst_names_single[i], rep(x = NA, times = 7))
        }
        else {
          qpercentiles[as.numeric(i), ] <-
            c(syst_names_single[i], rep(x = 0, times = 7))
        }
      }
      colnames(qpercentiles) <-
        c("node",
          "50th",
          "80th",
          "85th",
          "90th",
          "95th",
          "99th",
          "100th")
      
      
      
      #Calculating the average queue per node per replicate & then over the simulation per node##
      #
      
      #cl<-makeCluster(17)
      #clusterExport(cl = cl,varlist = c("ptq","nodes","node_names"))
      
      avg_queue <- lapply(
        X = ptq,
        FUN = function(ptq) {
          #library(tidyverse)
          tmp <-
            ptq %>% group_by(node) %>% summarise(avg_queue = sum(queue * time_at_queue_length) /
                                                   sum(time_at_queue_length)) %>% as.data.frame()
          tmp$node <- node_names[tmp$node, 2]
          tmp
        }
      )
      #stopCluster(cl)
      
      
      avg_queue_summary <-
        ptq_time %>% group_by(node) %>% summarise(avg_queue = sum(queue * time_at_queue_length) /
                                                    sum(time_at_queue_length)) %>% as.data.frame()
      avg_queue_summary$node <- node_names[avg_queue_summary$node, 2]
      
      
      
      ### Create the occupancy metrics ###############################################
      
      
      #Calculating the time at each occupancy##
      
      
      pto_total <- as.data.frame(rbindlist(pto))
      rownames(pto_total) <- c()
      pto_total$occupancy <-
        as.numeric(as.character(pto_total$occupancy))
      pto_total$time_at_occupancy <-
        as.numeric(as.character(pto_total$time_at_occupancy))
      pto_total <- pto_total[, -4]
      
      pto_time <-
        pto_total %>% group_by(node, occupancy) %>% mutate(time_at_occupancy = sum(time_at_occupancy) /
                                                             reps)
      pto_time <- as.data.frame(pto_time)
      pto_time <- unique(pto_time)
      pto_time$node <- as.numeric(as.character(pto_time$node))
      pto_time <- pto_time[order(pto_time$node, pto_time$occupancy), ]
      
      pto_percent <-
        pto_time %>% group_by(node) %>% transmute(
          occupancy,
          percent_time_at_occupancy = 100 * time_at_occupancy / sum(time_at_occupancy)
        )
      pto_percent <-
        pto_percent %>% group_by(node) %>% transmute(
          occupancy,
          percent_time_at_occupancy,
          cumulative_percent_time_at_occupancy = cumsum(percent_time_at_occupancy)
        )
      pto_percent <- as.data.frame(pto_percent)
      pto_percent <- unique(pto_percent)
      pto_percent$node <- as.numeric(as.character(pto_percent$node))
      pto_percent <-
        pto_percent[order(pto_percent$node, pto_percent$occupancy), ]
      pto_percent$node <- as.factor(pto_percent$node)
      pto_percent$node <-
        syst_names_single[as.numeric(as.character(pto_percent$node))]
      pto_percent$node <- as.factor(pto_percent$node)
      
      pto_percent$node <-
        factor(x = pto_percent$node, levels = syst_names_single)
      
      pto_plot <-
        ggplot(data = pto_percent %>% mutate(node=str_replace_all(node,pattern="_",replacement=" ")),
               aes(x = occupancy, y = percent_time_at_occupancy, fill = node)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        facet_grid(node ~ .,labeller=label_wrap_gen(15)) + 
        theme_bw() +
        #geom_text(aes(label=ifelse(signif(x = pto_percent$percent_time_at_occupancy,digits = 3)<100,signif(x = pto_percent$percent_time_at_occupancy,digits = 2),"")),vjust=-0.5,position = position_dodge(width=0.9),  size=3)+ coord_cartesian(ylim = c(0,100))+
        xlab("Patient Occupancy") +
        ylab("% time at patient occupancy level") +
        theme(legend.position="none")
      
      if (max(pto_percent$occupancy) == 1) {
        pto_plot <-
          pto_plot + scale_x_discrete(limits = c(
            min(pto_percent$occupancy),
            max(pto_percent$occupancy)
          ))
      }
      
      #pto_plot
      
      
      
      
      #Occupancy Percentiles##
      
      opercentiles <- matrix(nrow = length(nodes), ncol = 8)
      
      for (i in as.numeric(nodes)) {
        if (length(unique(pto_percent$cumulative_percent_time_at_occupancy[which(pto_percent$node ==
                                                                                 syst_names_single[i])])) >= 2) {
          tmp <-
            approx(
              x = pto_percent$cumulative_percent_time_at_occupancy[which(pto_percent$node ==
                                                                           syst_names_single[i])],
              y = pto_percent$occupancy[which(pto_percent$node == syst_names_single[i])],
              xout = c(50, 80, 85, 90, 95, 99, 100),
              ties = min,
              rule = 2
            )
          tmp$y <- round(tmp$y, digits = 2)
          opercentiles[as.numeric(i), ] <- c(syst_names_single[i], tmp$y)
        }
        else if (length(unique(pto_percent$cumulative_percent_time_at_occupancy[which(pto_percent$node ==
                                                                                      syst_names_single[i])])) == 2) {
          opercentiles[as.numeric(i), ] <-
            c(syst_names_single[i], rep(x = NA, times = 7))
        }
        else{
          opercentiles[as.numeric(i), ] <-
            c(syst_names_single[i], rep(x = 0, times = 7))
        }
        
      }
      colnames(opercentiles) <-
        c("node",
          "50th",
          "80th",
          "85th",
          "90th",
          "95th",
          "99th",
          "100th")
      
      
      #Calculating the average occupancy per node per replicate##
      
      #Calculating the average delayed per node per replicate & then over the simulation per node##
      
      
      #cl<-makeCluster(17)
      #clusterExport(cl = cl,varlist = c("pto","nodes","node_names"))
      
      avg_occupancy <- lapply(
        X = pto,
        FUN = function(pto) {
          #library(tidyverse)
          tmp <-
            pto %>% group_by(node) %>% summarise(avg_occupancy = sum(occupancy * time_at_occupancy) /
                                                   sum(time_at_occupancy)) %>% as.data.frame()
          tmp$node <- node_names[tmp$node, 2]
          tmp
        }
      )
      #stopCluster(cl)
      
      
      avg_occupancy_summary <-
        pto_time %>% group_by(node) %>% summarise(avg_occupancy = sum(occupancy *
                                                                        time_at_occupancy) / sum(time_at_occupancy)) %>% as.data.frame()
      avg_occupancy_summary$node <-
        node_names[avg_occupancy_summary$node, 2]
      
      
      #rm(tmp_occupancy,tmp_o_time,results,avg_o)
      
      
      ### Create the transition metrics ######################################################
      
      
      #Calculating the time at each transition length##
      
      
      ptt_total <- as.data.frame(rbindlist(ptt))
      rownames(ptt_total) <- c()
      ptt_total$transition <-
        as.numeric(as.character(ptt_total$transition))
      ptt_total$time_at_transition_level <-
        as.numeric(as.character(ptt_total$time_at_transition_level))
      ptt_total <- ptt_total[, -4]
      
      ptt_time <-
        ptt_total %>% group_by(node, transition) %>% mutate(time_at_transition_level =
                                                              sum(time_at_transition_level) / reps)
      ptt_time <- as.data.frame(ptt_time)
      ptt_time <- unique(ptt_time)
      ptt_time$node <- as.numeric(as.character(ptt_time$node))
      ptt_time <- ptt_time[order(ptt_time$node, ptt_time$transition), ]
      
      ptt_percent <-
        ptt_time %>% group_by(node) %>% transmute(
          transition,
          percent_time_at_transition_level = 100 * time_at_transition_level / sum(time_at_transition_level)
        )
      ptt_percent <-
        ptt_percent %>% group_by(node) %>% transmute(
          transition,
          percent_time_at_transition_level,
          cumulative_percent_time_at_transition_level = cumsum(percent_time_at_transition_level)
        )
      ptt_percent <- as.data.frame(ptt_percent)
      ptt_percent <- unique(ptt_percent)
      ptt_percent$node <- as.numeric(as.character(ptt_percent$node))
      ptt_percent <-
        ptt_percent[order(ptt_percent$node, ptt_percent$transition), ]
      ptt_percent$node <- as.factor(ptt_percent$node)
      ptt_percent$node <-
        syst_names_single[as.numeric(as.character(ptt_percent$node))]
      ptt_percent$node <- as.factor(ptt_percent$node)
      
      ptt_percent$node <-
        factor(x = ptt_percent$node, levels = syst_names_single)
      
      ptt_plot <-
        ggplot(data = ptt_percent %>% mutate(node=str_replace_all(node,pattern="_",replacement=" ")),
               aes(x = transition, y = percent_time_at_transition_level, fill = node)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        facet_grid(node ~ .,labeller=label_wrap_gen(15)) + 
        theme_bw() +
        #geom_text(aes(label=ifelse(signif(x = ptt_percent$percent_time_at_transition_level,digits = 3)<100,signif(x = ptt_percent$percent_time_at_transition_level,digits = 2),"")),vjust=-0.5,position = position_dodge(width=0.9),  size=3)+ coord_cartesian(ylim = c(0,100))+
        xlab("# concurrently in transition") + 
        ylab("% time at transition level") +
        theme(legend.position="none")
      
      if (max(ptt_percent$transition) == 1) {
        ptt_plot <-
          ptt_plot + scale_x_discrete(limits = c(
            min(ptt_percent$transition),
            max(ptt_percent$transition)
          ))
      }
      
      #ptt_plot
      
      #transition Percentiles##
      
      tpercentiles <- matrix(nrow = length(nodes), ncol = 8)
      
      for (i in as.numeric(nodes)) {
        if (length(unique(
          ptt_percent$cumulative_percent_time_at_transition_level[which(ptt_percent$node ==
                                                                        syst_names_single[i])]
        )) >= 2) {
          tmp <-
            approx(
              x = ptt_percent$cumulative_percent_time_at_transition_level[which(ptt_percent$node ==
                                                                                  syst_names_single[i])],
              y = ptt_percent$transition[which(ptt_percent$node == syst_names_single[i])],
              xout = c(50, 80, 85, 90, 95, 99, 100),
              ties = min,
              rule = 2
            )
          tmp$y <- round(tmp$y, digits = 2)
          tpercentiles[as.numeric(i), ] <-
            c(syst_names_single[i], as.numeric(tmp$y))
        }
        else if (length(unique(
          ptt_percent$cumulative_percent_time_at_transition_level[which(ptt_percent$node ==
                                                                        syst_names_single[i])]
        )) == 0) {
          tpercentiles[as.numeric(i), ] <-
            c(syst_names_single[i], rep(x = NA, times = 7))
        }
        else{
          tpercentiles[as.numeric(i), ] <-
            c(syst_names_single[i], rep(x = 0, times = 7))
        }
        
      }
      colnames(tpercentiles) <-
        c("node",
          "50th",
          "80th",
          "85th",
          "90th",
          "95th",
          "99th",
          "100th")
      
      #Calculating the average transition per node per replicate##
      
      #cl<-makeCluster(17)
      #clusterExport(cl = cl,varlist = c("ptt","nodes","node_names"))
      
      avg_transition <- lapply(
        X = ptt,
        FUN = function(ptt) {
          #library(tidyverse)
          tmp <-
            ptt %>% group_by(node) %>% summarise(
              avg_transition = sum(transition * time_at_transition_level) / sum(time_at_transition_level)
            ) %>% as.data.frame()
          tmp$node <- node_names[tmp$node, 2]
          tmp
        }
      )
      #stopCluster(cl)
      
      
      avg_transition_summary <-
        ptt_time %>% group_by(node) %>% summarise(
          avg_transition = sum(transition * time_at_transition_level) / sum(time_at_transition_level)
        ) %>% as.data.frame()
      avg_transition_summary$node <-
        node_names[avg_transition_summary$node, 2]
      
      
      
      
      #
      # #rm(tmp_t_length,tmp_t_time,results,avg_t)
      #
      
      
      ### Create the Bed Occupancy metrics #######################################################
      #
      
      #### % time at bed occupancy level###
      
      ptb_total <- as.data.frame(rbindlist(ptb))
      rownames(ptb_total) <- c()
      ptb_total$occ_bed <- as.numeric(as.character(ptb_total$occ_bed))
      ptb_total$time_at_occ_bed_level <-
        as.numeric(as.character(ptb_total$time_at_occ_bed_level))
      ptb_total <- ptb_total[, -4]
      
      ptb_time <-
        ptb_total %>% group_by(node, occ_bed) %>% mutate(time_at_occ_bed_level =
                                                           sum(time_at_occ_bed_level) / reps)
      ptb_time <- as.data.frame(ptb_time)
      ptb_time <- unique(ptb_time)
      ptb_time$node <- as.numeric(as.character(ptb_time$node))
      ptb_time <- ptb_time[order(ptb_time$node, ptb_time$occ_bed), ]
      
      ptb_percent <-
        ptb_time %>% group_by(node) %>% transmute(
          occ_bed,
          percent_time_at_occ_bed_level = 100 * time_at_occ_bed_level / sum(time_at_occ_bed_level)
        )
      ptb_percent <-
        ptb_percent %>% group_by(node) %>% transmute(
          occ_bed,
          percent_time_at_occ_bed_level,
          cumulative_percent_time_at_occ_bed_level = cumsum(percent_time_at_occ_bed_level)
        )
      ptb_percent <- as.data.frame(ptb_percent)
      ptb_percent <- unique(ptb_percent)
      ptb_percent$node <- as.numeric(as.character(ptb_percent$node))
      ptb_percent <-
        ptb_percent[order(ptb_percent$node, ptb_percent$occ_bed), ]
      ptb_percent$node <- as.factor(ptb_percent$node)
      ptb_percent$node <-
        syst_names_single[as.numeric(as.character(ptb_percent$node))]
      ptb_percent$node <- as.factor(ptb_percent$node)
      
      ptb_percent$node <-
        factor(x = ptb_percent$node, levels = syst_names_single)
      
      ptb_plot <-
        ggplot(data = ptb_percent %>% mutate(node=str_replace_all(node,pattern="_",replacement=" ")),
               aes(x = occ_bed, y = percent_time_at_occ_bed_level, fill = node)) +
        geom_bar(stat = "identity", position = position_dodge()) + 
        facet_grid(node ~ .,labeller=label_wrap_gen(15)) + 
        theme_bw() +
        #geom_text(aes(label=ifelse(signif(x = ptb_percent$percent_time_at_occ_bed_level,digits = 3)<100,signif(x = ptb_percent$percent_time_at_occ_bed_level,digits = 2),"")),vjust=-0.5,position = position_dodge(width=0.9),  size=3)+ coord_cartesian(ylim = c(0,100))+
        xlab("Bed Occupancy") + 
        ylab("% time at bed occupancy level") +
        theme(legend.position="none")
      
      if (max(ptb_percent$occ_bed) == 1) {
        ptb_plot <-
          ptb_plot + scale_x_discrete(limits = c(min(ptb_percent$occ_bed), max(ptb_percent$occ_bed)))
      }
      
      #ptq_plot
      
      
      
      
      #Occ_Bed Percentiles##
      
      bpercentiles <- matrix(nrow = length(nodes), ncol = 8)
      
      for (i in as.numeric(nodes)) {
        if (length(unique(
          ptb_percent$cumulative_percent_time_at_occ_bed_level[which(ptb_percent$node ==
                                                                     syst_names_single[i])]
        )) >= 2) {
          tmp <-
            approx(
              x = ptb_percent$cumulative_percent_time_at_occ_bed_level[which(ptb_percent$node ==
                                                                               syst_names_single[i])],
              y = ptb_percent$occ_bed[which(ptb_percent$node == syst_names_single[i])],
              xout = c(50, 80, 85, 90, 95, 99, 100),
              ties = min,
              rule = 2
            )
          
          tmp$y <- round(tmp$y, digits = 2)
          
          bpercentiles[as.numeric(i), ] <- c(syst_names_single[i], tmp$y)
          #bpercentiles[as.numeric(i),]<-as.numeric(bpercentiles[as.numeric(i),])
        }
        else if (length(unique(
          ptb_percent$cumulative_percent_time_at_occ_bed_level[which(ptb_percent$node ==
                                                                     syst_names_single[i])]
        )) == 0) {
          bpercentiles[as.numeric(i), ] <-
            c(syst_names_single[i], rep(x = NA, times = 7))
        }
        else {
          bpercentiles[as.numeric(i), ] <-
            c(syst_names_single[i], rep(x = 0, times = 7))
        }
        
      }
      colnames(bpercentiles) <-
        c("node",
          "50th",
          "80th",
          "85th",
          "90th",
          "95th",
          "99th",
          "100th")
      
      
      
      
      #Calculating the average occ_bed per node per replicate##
      #
      #cl<-makeCluster(17)
      #clusterExport(cl = cl,varlist = c("ptb","nodes","node_names"))
      
      avg_occ_bed <- lapply(
        X = ptb,
        FUN = function(ptb) {
          #library(tidyverse)
          tmp <-
            ptb %>% group_by(node) %>% summarise(avg_occ_bed = sum(occ_bed * time_at_occ_bed_level) /
                                                   sum(time_at_occ_bed_level)) %>% as.data.frame()
          tmp$node <- node_names[tmp$node, 2]
          tmp
        }
      )
      #stopCluster(cl)
      
      
      avg_occ_bed_summary <-
        ptb_time %>% group_by(node) %>% summarise(avg_occ_bed = sum(occ_bed * time_at_occ_bed_level) /
                                                    sum(time_at_occ_bed_level)) %>% as.data.frame()
      avg_occ_bed_summary$node <- node_names[avg_occ_bed_summary$node, 2]
      
      
      
      ######  MULTI DATA TABLE ########################################################################
      
      multi_spread_uniform <- rbindlist(multi_spread_uniform)
      
      through_time_uniform <- multi_spread_uniform
      through_time_uniform$time <- through_time_uniform$time - warm_up
      
      rm(multi_spread_uniform)
      
      through_time_uniform_gather <-
        gather(
          through_time_uniform,
          key = "metric",
          value = "value",
          occ_bed,
          delayed,
          occupancy,
          transition,
          queue
        )
      
      
      avg_through_time <- through_time_uniform_gather %>%
        group_by(time, node, metric) %>%
        summarise(
          mean = mean(value, na.rm = T),
          L99 = quantile(value, 0.005, na.rm = T),
          U99 = quantile(value, 0.995, na.rm = T),
          L95 = quantile(value, 0.025, na.rm = T),
          U95 = quantile(value, 0.975, na.rm = T),
          L50 = quantile(value, 0.25, na.rm = T),
          U50 = quantile(value, 0.75, na.rm = T)
        ) %>%
        as.data.frame()
      avg_through_time$metric <-
        factor(
          avg_through_time$metric,
          levels = c('queue', 'occupancy', 'occ_bed', 'delayed', 'transition')
        )
      
      
      avg_through_time$node <-
        factor(x = avg_through_time$node, levels = syst_names_single)
      
      avg_through_time_plot <- ggplot(avg_through_time %>% mutate(node=str_replace_all(node,pattern="_",replacement=" "))) +
        geom_ribbon(aes(
          x = time,
          ymin = L99,
          ymax = U99,
          fill = "99%"
        ), alpha = 0.25) +
        geom_ribbon(aes(
          x = time,
          ymin = L95,
          ymax = U95,
          fill = "95%"
        ), alpha = 0.25) +
        geom_ribbon(aes(
          x = time,
          ymin = L50,
          ymax = U50,
          fill = "50%"
        ), alpha = 0.25) +
        scale_fill_manual(
          name = "Percentiles",
          values = c(
            "99%" = "grey75",
            "95%" = "grey60",
            "50%" = "grey45"
          ),
          breaks = c("99%", "95%", "50%")
        ) +
        geom_line(aes(
          x = time,
          y = mean,
          colour = metric
        ), size = 1.1) +
        facet_grid(metric ~ node, scales = "free",labeller=label_wrap_gen(15)) +
        ylab("Mean # of patients") +
      xlab(paste0("Time (", input$time_unit, ")")) +
        
        theme_bw() + 
        theme(panel.spacing = unit(1, "lines"),
              axis.text.x = element_text(size = 7),
              legend.position="none") + 
        expand_limits(y = 0)
      
      
      through_time_mini <-
        through_time_uniform_gather[which(
          through_time_uniform_gather$rep == "rep 1" |
            through_time_uniform_gather$rep == "rep 2" |
            through_time_uniform_gather$rep == "rep 3" |
            through_time_uniform_gather$rep == "rep 4" |
            through_time_uniform_gather$rep == "rep 5"
        ), ]
      through_time_mini$node <-
        factor(x = through_time_mini$node, levels = syst_names_single)
      
      total_in_system <-
        through_time_uniform_gather[which(
          through_time_uniform_gather$rep == "rep 1" |
            through_time_uniform_gather$rep == "rep 2" |
            through_time_uniform_gather$rep == "rep 3" |
            through_time_uniform_gather$rep == "rep 4" |
            through_time_uniform_gather$rep == "rep 5" |
            through_time_uniform_gather$rep ==
            "rep 6" |
            through_time_uniform_gather$rep == "rep 7" |
            through_time_uniform_gather$rep == "rep 8" |
            through_time_uniform_gather$rep == "rep 9" |
            through_time_uniform_gather$rep == "rep 10"
        ), ]
      total_in_system$node <-
        factor(x = total_in_system$node, levels = syst_names_single)
      total_in_system <-
        total_in_system[which(total_in_system$metric == "occupancy" |
                                total_in_system$metric == "queue"), ]
      total_in_system <- total_in_system[, c(1, 3, 5)]
      
      total_in_system_dat <-
        total_in_system %>% group_by(time, rep) %>% summarise("value" = sum(value)) %>% as.data.frame()
      
      
      
      rm(through_time_uniform_gather)
      
      #the plot "o" ####
      o <-
        ggplot(data = through_time_mini[which(through_time_mini$metric == "occupancy"), ] %>% 
                 mutate(node=str_replace_all(node,pattern="_",replacement=" "))) +
        geom_step(aes(x = time, y = value, col = node)) + 
        facet_grid(node ~ rep,labeller=label_wrap_gen(15)) +
        theme_bw() + 
        ylab("Occupancy") + 
        theme(panel.spacing.x = unit(1, "lines"),
              axis.text.x = element_text(size = 7),
              legend.position="none") +
      xlab(paste0("Time (", input$time_unit, ")"))
      
      #the plot "q" ####
      q <-
        ggplot(data = through_time_mini[which(through_time_mini$metric == "queue"), ] %>% 
                 mutate(node=str_replace_all(node,pattern="_",replacement=" "))) +
        geom_step(aes(x = time, y = value, col = node)) + 
        facet_grid(node ~ rep, labeller=label_wrap_gen(15)) +
        theme_bw() + 
        ylab("Queue") +
      xlab(paste0("Time (", input$time_unit, ")")) +
        theme(panel.spacing.x = unit(1, "lines"),
              axis.text.x = element_text(size =7),
              legend.position="none")
      
      #the plot "d" ####
      d <-
        ggplot(data = through_time_mini[which(through_time_mini$metric == "delayed"), ] %>% 
                 mutate(node=str_replace_all(node,pattern="_",replacement=" "))) +
        geom_step(aes(x = time, y = value, col = node)) +
        facet_grid(node ~ rep, labeller=label_wrap_gen(15)) +
        theme_bw() + ylab("Delayed") +
      xlab(paste0("Time (", input$time_unit, ")")) +
        theme(panel.spacing.x = unit(1, "lines"),
              axis.text.x = element_text(size =7),
              legend.position="none")
      
      #the plot "t" ####
      t <-
        ggplot(data = through_time_mini[which(through_time_mini$metric == "transition"), ] %>% 
                 mutate(node=str_replace_all(node,pattern="_",replacement=" "))) +
        geom_step(aes(x = time, y = value, col = node)) + 
        facet_grid(node ~ rep, labeller=label_wrap_gen(15)) +
        theme_bw() +
        ylab("Transition") +
      xlab(paste0("Time (", input$time_unit, ")")) +
        theme(panel.spacing.x = unit(1, "lines"),
              axis.text.x = element_text(size =7),
              legend.position="none")
      
      #the plot "b" ####
      b <-
        ggplot(data = through_time_mini[which(through_time_mini$metric == "occ_bed"), ] %>% 
                 mutate(node=str_replace_all(node,pattern="_",replacement=" "))) +
        geom_step(aes(x = time, y = value, col = node)) + 
        facet_grid(node ~ rep, labeller=label_wrap_gen(15)) +
        theme_bw() + ylab("Bed Occupancy") +
      xlab(paste0("Time (", input$time_unit, ")")) +
        theme(panel.spacing.x = unit(1, "lines"),
              axis.text.x = element_text(size = 7),
              legend.position="none")
      
      #the plot "tisp" ####
      tisp <-
        ggplot(data = total_in_system_dat) + geom_line(aes(x = time, y = value, group = rep),
                                                       col = "black",
                                                       alpha = 0.4) + 
        theme_bw() + 
        ylab("Total in System")
      
      
      
      #[which(avg_through_time$metric=="occupancy"),]
      
      
      
    
      #SIMULATION OUTPUT OBJECT LIST "combo" ####
      #time units added to selected metric names in tables below ####
      #these will be overwritten directly to remove underscores for the on-screen shiny outputs in some cases
      #but will still pull through to the excel file download in the format below
      
      combo <- list(
        total_time_in_system = total_time_in_system %>% mutate(metric = paste0(metric, " (", input$time_unit, ")")),
        total_time_in_system_summary = total_time_in_system_summary %>% mutate(metric =
                                                                                 paste0(metric, " (", input$time_unit, ")")),
        node_wait = node_wait %>% mutate(metric = paste0(metric, " (", input$time_unit, ")")),
        node_wait_summary = node_wait_summary %>% mutate(metric = paste0(metric, " (", input$time_unit, ")")),
        pat_wait = pat_wait %>% mutate(metric = paste0(metric, " (", input$time_unit, ")")),
        pat_wait_summary = pat_wait_summary %>% mutate(metric = paste0(metric, " (", input$time_unit, ")")),
        node_active_service = node_active_service %>% mutate(metric = paste0(metric, " (", input$time_unit, ")")),
        node_active_service_summary = node_active_service_summary %>% mutate(metric =
                                                                               paste0(metric, " (", input$time_unit, ")")),
        pat_active_service = pat_active_service %>% mutate(metric = paste0(metric, " (", input$time_unit, ")")),
        pat_active_service_summary = pat_active_service_summary %>% mutate(metric =
                                                                             paste0(metric, " (", input$time_unit, ")")),
        node_length_of_stay = node_length_of_stay %>% mutate(metric = paste0(metric, " (", input$time_unit, ")")),
        node_length_of_stay_summary = node_length_of_stay_summary %>% mutate(metric =
                                                                               paste0(metric, " (", input$time_unit, ")")),
        pat_length_of_stay = pat_length_of_stay %>% mutate(metric = paste0(metric, " (", input$time_unit, ")")),
        pat_length_of_stay_summary = pat_length_of_stay_summary %>% mutate(metric =
                                                                             paste0(metric, " (", input$time_unit, ")")),
        node_delay_to_transfer = node_delay_to_transfer %>% mutate(metric =
                                                                     paste0(metric, " (", input$time_unit, ")")),
        node_delay_to_transfer_summary = node_delay_to_transfer_summary %>% mutate(metric =
                                                                                     paste0(metric, " (", input$time_unit, ")")),
        pat_delay_to_transfer = pat_delay_to_transfer %>% mutate(metric =
                                                                   paste0(metric, " (", input$time_unit, ")")),
        pat_delay_to_transfer_summary = pat_delay_to_transfer_summary %>% mutate(metric =
                                                                                   paste0(metric, " (", input$time_unit, ")")),
        pat_rep_summary = pat_rep_summary %>% mutate(metric = paste0(metric, " (", input$time_unit, ")")),
        pat_total_summary = pat_total_summary %>% mutate(metric = paste0(metric, " (", input$time_unit, ")")),
        ptd_percent = ptd_percent,
        ptd_plot = ptd_plot,
        avg_delayed = avg_delayed,
        avg_delayed_summary = avg_delayed_summary,
        d = d,
        ptq_percent = ptq_percent,
        ptq_plot = ptq_plot,
        avg_queue = avg_queue,
        avg_queue_summary = avg_queue_summary,
        q = q,
        pto_percent = pto_percent,
        pto_plot = pto_plot,
        avg_occupancy = avg_occupancy,
        avg_occupancy_summary = avg_occupancy_summary,
        o = o,
        ptb_percent = ptb_percent,
        ptb_plot = ptb_plot,
        avg_occ_bed = avg_occ_bed,
        avg_occ_bed_summary = avg_occ_bed_summary,
        b = b,
        ptt_percent = ptt_percent,
        ptt_plot = ptt_plot,
        avg_transition = avg_transition,
        avg_transition_summary = avg_transition_summary,
        t = t,
        dpercentiles = dpercentiles,
        qpercentiles = qpercentiles,
        opercentiles = opercentiles,
        bpercentiles = bpercentiles,
        tpercentiles = tpercentiles,
        rejected_summary = rejected_summary,
        avg_through_time_plot = avg_through_time_plot,
        reps = reps,
        ptm = ptm,
        avg_through_time = avg_through_time,
        nodes = nodes,
        warm_up = warm_up,
        sim_time = sim_time,
        exits = exits,
        syst_names = syst_names,
        delay_list = delay_list,
        cap_cal_input = cap_cal_input_original,
        arr_cal_input = arr_cal_input_original,
        node_capacity_delay = node_capacity_delay,
        node_capacity_delay_summary = node_capacity_delay_summary,
        node_transition_delay = node_transition_delay,
        node_transition_delay_summary = node_transition_delay_summary,
        pat_capacity_delay = pat_capacity_delay,
        pat_capacity_delay_summary = pat_capacity_delay_summary,
        pat_transition_delay = pat_transition_delay,
        pat_transition_delay_summary = pat_transition_delay_summary,
        tisp = tisp,
        
        #change - add simulation time unit for use in markdown report ####
        #needs to be saved here so it can be added to parameters list
        time_unit = input$time_unit #save character string time unit description for use as param in markdown
      )
      
      
      stopCluster(cl)
      
      
      #change to check on number of simulation outputs ####
      #this is a manual check to see if every item in "combo", the list of items that the sim_out function
      #will return, has been created. Orignally, this list had 72 items and there was a hard-coded check
      #to see if the length of the list was 72. A further item (the time unit) has now been added, and so
      #the number being checked for is now 73
      #The reason for performing this check, and what it is intended to achieve, needs to be clarified
      if (length(combo) == 73) {
        
        showModal(modalDialog(
          title = div(paste0("Simulation Complete \n(", format(Sys.time()), ")"), style="font-size:200%"),
          div("Click anywhere on screen to continue", style="font-size:200%"),
          easyClose = TRUE,
          footer = NULL,
          size="l"
        ))
        
        # shinyalert(
        #   title = paste0("Simulation Complete \n(", format(Sys.time()), ")"),
        #   text = "",
        #   closeOnEsc = TRUE,
        #   closeOnClickOutside = TRUE,
        #   html = FALSE,
        #   type = "info",
        #   showConfirmButton = TRUE,
        #   showCancelButton = FALSE,
        #   confirmButtonText = "OK",
        #   confirmButtonCol = "#87D9FF",
        #   timer = 0,
        #   imageUrl = "",
        #   animation = TRUE
        # )
      } else{
        
        
        showModal(modalDialog(
          title = "Simulation Error",
          "",
          easyClose = TRUE,
          footer = NULL,
          size="l"
        ))
        
        
        # shinyalert(
        #   title = "Simulation Error",
        #   text = "",
        #   closeOnEsc = TRUE,
        #   closeOnClickOutside = TRUE,
        #   html = FALSE,
        #   type = "info",
        #   showConfirmButton = TRUE,
        #   showCancelButton = FALSE,
        #   confirmButtonText = "OK",
        #   confirmButtonCol = "E60000",
        #   timer = 0,
        #   imageUrl = "",
        #   animation = TRUE
        # )
        
        hideTab(inputId = "navbar", target = "3. Simulation Outputs")
        hideTab(inputId = "navbar", target = "4. Download Outputs")
      }
      
      
      
      return(combo)
    }, error = function(e) {
      
      showModal(modalDialog(
        title = div("Simulation Error",style="font-size:200%"),
        div("Try running the simulation for longer (increase simulation period length).
      \n If the error persists, return to the data input pages and check that data has been entered correctly. Click anywhere on screen to continue.", style="font-size:200%"),
        easyClose = TRUE,
        footer = NULL,
        size="l"
      ))
      
      
      
      # shinyalert(
      #   title = HTML(
      #     'Simulation Error \n Try running the simulation for longer (increase simulation period length).
      # \n If the error persists, return to the data input pages and check that data has been entered correctly.'
      #   ),
      #   text = "",
      #   closeOnEsc = FALSE,
      #   closeOnClickOutside = FALSE,
      #   html = FALSE,
      #   type = "info",
      #   showConfirmButton = TRUE,
      #   showCancelButton = FALSE,
      #   confirmButtonText = "OK",
      #   confirmButtonCol = "#FF0000",
      #   timer = 0,
      #   imageUrl = "",
      #   animation = FALSE
      # )
      
      hideTab(inputId = "navbar", target = "3. Simulation Outputs")
      hideTab(inputId = "navbar", target = "4. Download Outputs")
      
      return(NULL)
      
      
    })
  }) # END OF sim_out() FUNCTION ####
  
  
  
  
  ### OUTPUT RENDER TEXT ####
  output$comp <- renderText({
    req(sim_out())
    x <- sim_out()
    y <- x$reps
    time <- proc.time() - x$ptm
    p <-
      c("**Simulation completed in ",
        round(time[3], digits = 1),
        " seconds**")
    p
    
  })
  
  
  # output$run_time<-renderTable({
  #   req(sim_out())
  #   out<-sim_out()
  #   x<-out$reps
  #   time<-proc.time()-out$ptm
  #   rep_run<-time[3]/x
  #
  #   y<-matrix(data = c("10 runs","100 runs","500 runs","1,000 runs","10,000 runs",
  #                      round(10*rep_run/60,digits=2),round(100*rep_run/60,digits=2),round(500*rep_run/60,digits=2),round(1000*rep_run/60,digits=2),round(10000*rep_run/60,digits=2),
  #                      round(10*rep_run/3600,digits=2),round(100*rep_run/3600,digits=2),round(500*rep_run/3600,digits=2),round(1000*rep_run/3600,digits=2),round(10000*rep_run/3600,digits=2)),
  #             ncol=3)
  #
  #   colnames(y)<-c("# of replicates","Run time (in minutes)","Run time (in hours)")
  #   y
  #
  #
  # },caption = "Run Time Estimates",
  # caption.placement = getOption("xtable.caption.placement", "top"),
  # caption.width = getOption("xtable.caption.width", NULL))
  #
  
  ###RENDER TOTAL TIME IN SYSTEM #####
  
  
  output$ttis <- renderDataTable({
    req(sim_out())
    x <- sim_out()
    tmp <- x$total_time_in_system
    #tmp<-format(tmp,digits=5)
    
  }, caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center;',
                                       htmltools::h4(strong(
                                         'Total time in system '
                                       ))), rownames = FALSE, filter = 'top', options = list(pageLength = 10, dom =
                                                                                               'tlp'))
  
  output$ttiss <- renderTable({
    req(sim_out())
    x <- sim_out()
    tmp <- x$total_time_in_system_summary
    
    tmp$metric <- paste0("Total Time In System (", x$time_unit, ")")
    colnames(tmp) <-
      c("Metric",
        "Mean",
        "Standard Deviation",
        "IQR",
        "95th Percentile")
    tmp <- format(tmp, digits = 4, scientific = F)
    tmp
    
    
  }, rownames = FALSE)
  
  ###RENDER WAITS #####
  
  output$node_wait <- renderDataTable({
    req(sim_out())
    x <- sim_out()
    tmp <- x$node_wait
    tmp <- rbindlist(tmp)
    #tmp<-format(tmp,digits=5)
    
  }, caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center;',
                                       
                                       htmltools::h4(strong('Wait'))), rownames = FALSE, filter = 'top', options = list(pageLength = 10, dom =
                                                                                                                          'tlp'))
  
  
  output$node_wait_summary <- renderTable({
    req(sim_out())
    x <- sim_out()
    tmp <- x$node_wait_summary
    tmp <- tmp[order(factor(x = tmp$node, levels = x$syst_names[, 2])), ]
    
    tmp$metric <- paste0("Wait (", x$time_unit, ")")
    tmp$node <- str_replace_all(tmp$node,pattern="_",replacement=" ")
    colnames(tmp) <-
      c("Service Point",
        "Metric",
        "Mean",
        "Standard Deviation",
        "IQR",
        "95th Percentile")
    tmp <- format(tmp, digits = 4, scientific = F)
    
  }, rownames = FALSE)
  
  
  output$pat_wait <- renderDataTable({
    req(sim_out())
    x <- sim_out()
    tmp <- x$pat_wait
    tmp <- rbindlist(tmp)
    #tmp<-format(tmp,digits=5)
    
  }, caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center;',
                                       
                                       htmltools::h4(strong('Wait'))), rownames = FALSE, filter = 'top', options = list(pageLength = 10, dom =
                                                                                                                          'tlp'))
  
  
  output$pat_wait_summary <- renderTable({
    req(sim_out())
    x <- sim_out()
    tmp <- x$pat_wait_summary
    
    tmp$metric <- paste0("Wait (", x$time_unit, ")")
    colnames(tmp) <-
      c("Metric",
        "Mean",
        "Standard Deviation",
        "IQR",
        "95th Percentile")
    tmp <- format(tmp, digits = 4, scientific = F)
    
  }, rownames = FALSE)
  
  
  ###RENDER ACTIVE SERVICE #####
  
  output$node_active_service <- renderDataTable({
    req(sim_out())
    x <- sim_out()
    tmp <- x$node_active_service
    tmp <- rbindlist(tmp)
    #tmp<-format(tmp,digits=5)
    
  }, caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center;',
                                       htmltools::h4(strong('Active Service'))), rownames = FALSE, filter =
    'top', options = list(pageLength = 10, dom = 'tlp'))
  
  
  output$node_active_service_summary <- renderTable({
    req(sim_out())
    x <- sim_out()
    tmp <- x$node_active_service_summary
    tmp <- tmp[order(factor(x = tmp$node, levels = x$syst_names[, 2])), ]
    
    tmp$metric <- paste0("Active Service (", x$time_unit, ")")
    tmp$node <- str_replace_all(tmp$node,pattern="_",replacement=" ")
    colnames(tmp) <-
      c("Service Point",
        "Metric",
        "Mean",
        "Standard Deviation",
        "IQR",
        "95th Percentile")
    tmp <- format(tmp, digits = 4, scientific = F)
    
  }, rownames = FALSE)
  
  
  
  output$pat_active_service <- renderDataTable({
    req(sim_out())
    x <- sim_out()
    tmp <- x$pat_active_service
    tmp <- rbindlist(tmp)
    #tmp<-format(tmp,digits=5)
    
  }, caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center;',
                                       
                                       
                                       htmltools::h4(strong('Active Service'))), rownames = FALSE, filter =
    'top', options = list(pageLength = 10, dom = 'tlp'))
  
  
  output$pat_active_service_summary <- renderTable({
    req(sim_out())
    x <- sim_out()
    tmp <- x$pat_active_service_summary
    colnames(tmp) <-
      c("Metric",
        "Mean",
        "Standard Deviation",
        "IQR",
        "95th Percentile")
    tmp <- format(tmp, digits = 4, scientific = F)
    
  }, rownames = FALSE)
  
  
  
  ###RENDER CAPACITY DELAYS #####
  
  output$node_capacity_delay <- renderDataTable({
    req(sim_out())
    x <- sim_out()
    tmp <- x$node_capacity_delay
    tmp <- rbindlist(tmp)
    #tmp<-format(tmp,digits=5)
    
  }, caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center;',
                                       
                                       htmltools::h4(
                                         strong('Time Delayed (Capacity Driven)')
                                       )), rownames = FALSE, filter = 'top', options = list(pageLength = 10, dom =
                                                                                              'tlp'))
  
  
  output$node_capacity_delay_summary <- renderTable({
    req(sim_out())
    x <- sim_out()
    tmp <- x$node_capacity_delay_summary
    tmp <- tmp[order(factor(x = tmp$node, levels = x$syst_names[, 2])), ]
    
    tmp$metric <- paste0("Capacity Delay (", x$time_unit, ")")
    tmp$node <- str_replace_all(tmp$node,pattern="_",replacement=" ")
    colnames(tmp) <-
      c("Service Point",
        "Metric",
        "Mean",
        "Standard Deviation",
        "IQR",
        "95th Percentile")
    tmp <- format(tmp, digits = 4, scientific = F)
    
  },
  
  
  rownames = FALSE)
  
  
  output$pat_capacity_delay <- renderDataTable({
    req(sim_out())
    x <- sim_out()
    tmp <- x$pat_capacity_delay
    tmp <- rbindlist(tmp)
    #tmp<-format(tmp,digits=5)
    
  }, caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center;',
                                       
                                       htmltools::h4(
                                         strong('Time Delayed (Capacity Driven)')
                                       )), rownames = FALSE, filter = 'top', options = list(pageLength = 10, dom =
                                                                                              'tlp'))
  
  
  output$pat_capacity_delay_summary <- renderTable({
    req(sim_out())
    x <- sim_out()
    tmp <- x$pat_capacity_delay_summary
    
    
    tmp$metric <- paste0("Capacity Delay (", x$time_unit, ")")
    colnames(tmp) <-
      c("Metric",
        "Mean",
        "Standard Deviation",
        "IQR",
        "95th Percentile")
    tmp <- format(tmp, digits = 4, scientific = F)
    
  }, rownames = FALSE)
  
  
  ###RENDER TRANSITION DELAYS #####
  
  output$node_transition_delay <- renderDataTable({
    req(sim_out())
    x <- sim_out()
    tmp <- x$node_transition_delay
    tmp <- tmp[order(factor(x = tmp$node, levels = x$syst_names[, 2])), ]
    tmp <- rbindlist(tmp)
    #tmp<-format(tmp,digits=5)
    
  }, caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center;',
                                       
                                       htmltools::h4(strong(
                                         'Time Delayed (Transition)'
                                       ))), rownames = FALSE, filter = 'top', options = list(pageLength = 10, dom =
                                                                                               'tlp'))
  
  
  output$node_transition_delay_summary <- renderTable({
    req(sim_out())
    x <- sim_out()
    tmp <- x$node_transition_delay_summary
    
    tmp$metric <- paste0("Transition Delay (", x$time_unit, ")")
    tmp$node <- str_replace_all(tmp$node,pattern="_",replacement=" ")
    colnames(tmp) <-
      c("Service Point",
        "Metric",
        "Mean",
        "Standard Deviation",
        "IQR",
        "95th Percentile")
    tmp <- format(tmp, digits = 4, scientific = F)
    
  }, rownames = FALSE)
  
  
  output$pat_transition_delay <- renderDataTable({
    req(sim_out())
    x <- sim_out()
    tmp <- x$pat_transition_delay
    tmp <- rbindlist(tmp)
    #tmp<-format(tmp,digits=5)
    
  }, caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center;',
                                       
                                       htmltools::h4(strong(
                                         'Time Delayed (Transition)'
                                       ))), rownames = FALSE, filter = 'top', options = list(pageLength = 10, dom =
                                                                                               'tlp'))
  
  
  output$pat_transition_delay_summary <- renderTable({
    req(sim_out())
    x <- sim_out()
    tmp <- x$pat_capacity_delay_summary
    
    tmp$metric <- paste0("Transition Delay (", x$time_unit, ")")
    colnames(tmp) <-
      c("Metric",
        "Mean",
        "Standard Deviation",
        "IQR",
        "95th Percentile")
    tmp <- format(tmp, digits = 4, scientific = F)
    
  }, rownames = FALSE)
  
  
  
  ###RENDER LENGTH OF STAY #####
  
  output$node_los <- renderDataTable({
    req(sim_out())
    x <- sim_out()
    tmp <- x$node_length_of_stay
    tmp <- rbindlist(tmp)
    #tmp<-format(tmp,digits=5)
    
  }, caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center;',
                                       
                                       htmltools::h4(strong('Length of Stay'))), rownames = FALSE, filter =
    'top', options = list(pageLength = 10, dom = 'tlp'))
  
  
  output$node_loss <- renderTable({
    req(sim_out())
    x <- sim_out()
    tmp <- x$node_length_of_stay_summary
    tmp <- tmp[order(factor(x = tmp$node, levels = x$syst_names[, 2])), ]
    
    tmp$metric <- paste0("Length Of Stay (", x$time_unit, ")")
    tmp$node <- str_replace_all(tmp$node,pattern="_",replacement=" ")
    colnames(tmp) <-
      c("Service Point",
        "Metric",
        "Mean",
        "Standard Deviation",
        "IQR",
        "95th Percentile")
    tmp <- format(tmp, digits = 4, scientific = F)
    
  }, rownames = FALSE)
  
  
  output$pat_los <- renderDataTable({
    req(sim_out())
    x <- sim_out()
    tmp <- x$pat_length_of_stay
    tmp <- rbindlist(tmp)
    #tmp<-format(tmp,digits=5)
    
  }, caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center;',
                                       
                                       htmltools::h4(strong('Length of Stay'))), rownames = FALSE, filter =
    'top', options = list(pageLength = 10, dom = 'tlp'))
  
  
  output$pat_loss <- renderTable({
    req(sim_out())
    x <- sim_out()
    tmp <- x$pat_length_of_stay_summary
    
    tmp$metric <- paste0("Length Of Stay (", x$time_unit, ")")
    colnames(tmp) <-
      c("Metric",
        "Mean",
        "Standard Deviation",
        "IQR",
        "95th Percentile")
    tmp <- format(tmp, digits = 4, scientific = F)
    
  }, rownames = FALSE)
  
  
  
  ###RENDER DELAY TO TRANSFER #####
  
  output$node_dtt <- renderDataTable({
    req(sim_out())
    x <- sim_out()
    tmp <- x$node_delay_to_transfer
    tmp <- rbindlist(tmp)
    #tmp<-format(tmp,digits=5)
    
  }, caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center;',
                                       
                                       htmltools::h4(strong(
                                         'Delay to Transfer'
                                       ))), rownames = FALSE, filter = 'top', options = list(pageLength = 10, dom =
                                                                                               'tlp'))
  
  
  output$node_dtts <- renderTable({
    req(sim_out())
    x <- sim_out()
    tmp <- x$node_delay_to_transfer_summary
    tmp <- tmp[order(factor(x = tmp$node, levels = x$syst_names[, 2])), ]
    
    tmp$metric <- paste0("Delay To Transfer (", x$time_unit, ")")
    tmp$node <- str_replace_all(tmp$node,pattern="_",replacement=" ")
    colnames(tmp) <-
      c("Service Point",
        "Metric",
        "Mean",
        "Standard Deviation",
        "IQR",
        "95th Percentile")
    tmp <- format(tmp, digits = 4, scientific = F)
    
  }, rownames = FALSE)
  
  
  
  output$pat_dtt <- renderDataTable({
    req(sim_out())
    x <- sim_out()
    tmp <- x$pat_delay_to_transfer
    tmp <- rbindlist(tmp)
    #tmp<-format(tmp,digits=5)
    
  }, caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center;',
                                       
                                       htmltools::h4(strong(
                                         'Delay to Transfer'
                                       ))), rownames = FALSE, filter = 'top', options = list(pageLength = 10, dom =
                                                                                               'tlp'))
  
  
  output$pat_dtts <- renderTable({
    req(sim_out())
    x <- sim_out()
    tmp <- x$pat_delay_to_transfer_summary
    
    tmp$metric <- paste0("Delay To Transfer (", x$time_unit, ")")
    colnames(tmp) <-
      c("Metric",
        "Mean",
        "Standard Deviation",
        "IQR",
        "95th Percentile")
    tmp <- format(tmp, digits = 4, scientific = F)
    
  }, rownames = FALSE)
  
  
  ###RENDER REJECTION RATE #####
  
  
  output$rejs <- renderTable({
    req(sim_out())
    x <- sim_out()
    tmp <- x$rejected_summary
    tmp <- tmp[order(factor(x = tmp$node, levels = x$syst_names[, 2])), ]
    tmp$node <- str_replace_all(tmp$node,pattern="_",replacement=" ")
    colnames(tmp) <- c("Service Point", "Mean")
    tmp <- format(tmp, digits = 4, scientific = F)
    
  }, rownames = FALSE)
  
  
  ###RENDER DELAY METRICS #####
  
  
  output$ptd_percent <- renderDataTable({
    req(sim_out())
    x <- sim_out()
    tmp <- x$ptd_percent
    tmp <- format(tmp, digits = 4, scientific = F)
    tmp$node <- str_replace_all(tmp$node,pattern="_",replacement=" ")
    colnames(tmp) <-
      c(
        "Service Point",
        "Delayed Level",
        "% time at Delayed Level",
        "Cumulative % time at or below Delayed Level"
      )
    tmp
    
  }, caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center;',
                                       htmltools::h4(
                                         strong('Percentage time at delayed level')
                                       )), rownames = FALSE, filter = 'top', options = list(pageLength = 10, dom =
                                                                                              'tlp'))
  
  
  output$ptd_plot <- renderPlot({
    req(sim_out())
    x <- sim_out()
    tmp <- x$ptd_plot
    tmp
    
  }, res = 128)
  
  
  output$avg_delayed <- renderDataTable({
    req(sim_out())
    x <- sim_out()
    tmp <- x$avg_delayed_summary
    tmp[,1] <-  str_replace_all(tmp[,1],pattern="_",replacement=" ")
    #tmp<-rbindlist(tmp)
    tmp[, 2] <- format(tmp[, 2], digits = 5)
    tmp
    
  }, caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center;',
                                       htmltools::h4(strong(
                                         'Average # Delayed'
                                       ))), rownames = FALSE, options = list(pageLength = 10, dom = 'tlp'))
  
  
  output$d <- renderPlot({
    req(sim_out())
    x <- sim_out()
    tmp <- x$d
    tmp
    
  }, res = 128)
  
  ###RENDER QUEUE METRICS #####
  
  output$ptq_percent <- renderDataTable({
    req(sim_out())
    x <- sim_out()
    tmp <- x$ptq_percent
    tmp <- format(tmp, digits = 4, scientific = F)
    tmp$node <- str_replace_all(tmp$node,pattern="_",replacement=" ")
    colnames(tmp) <-
      c(
        "Service Point",
        "Queue Length",
        "% time at Queue Length",
        "Cumulative % time at or below Queue Length"
      )
    tmp
    
  }, caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center;',
                                       htmltools::h4(
                                         strong('Percentage time at queue length')
                                       )), rownames = FALSE, filter = 'top', options = list(pageLength = 10, dom =
                                                                                              'tlp'))
  
  
  output$ptq_plot <- renderPlot({
    req(sim_out())
    x <- sim_out()
    tmp <- x$ptq_plot
    tmp
    
  }, res = 128)
  
  
  output$avg_queue <- renderDataTable({
    req(sim_out())
    x <- sim_out()
    tmp <- x$avg_queue_summary
    tmp[,1] <-  str_replace_all(tmp[,1],pattern="_",replacement=" ")
    #tmp<-rbindlist(tmp)
    tmp[, 2] <- format(tmp[, 2], digits = 5)
    tmp
    
  }, caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center;',
                                       htmltools::h4(strong(
                                         'Average queue length'
                                       ))), rownames = FALSE, options = list(pageLength = 10, dom = 'tlp'))
  
  
  output$q <- renderPlot({
    req(sim_out())
    x <- sim_out()
    tmp <- x$q
    tmp
    
  }, res = 128)
  
  ###RENDER OCCUPANCY METRICS #####
  
  output$pto_percent <- renderDataTable({
    req(sim_out())
    x <- sim_out()
    tmp <- x$pto_percent
    tmp <- format(tmp, digits = 4, scientific = F)
    tmp$node <- str_replace_all(tmp$node,pattern="_",replacement=" ")
    colnames(tmp) <-
      c(
        "Service Point",
        "Patient Occupancy Level",
        "% time at Patient Occupancy Level",
        "Cumulative % time at or below Patient Occupancy Level"
      )
    tmp
    
    
  }, caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center;',
                                       htmltools::h4(
                                         strong('Percentage time at occupancy level')
                                       )), rownames = FALSE, filter = 'top', options = list(pageLength = 10, dom =
                                                                                              'tlp'))
  
  
  output$pto_plot <- renderPlot({
    req(sim_out())
    x <- sim_out()
    tmp <- x$pto_plot
    tmp
    
  }, res = 128)
  
  
  output$avg_occupancy <- renderDataTable({
    req(sim_out())
    x <- sim_out()
    tmp <- x$avg_occupancy_summary
    tmp[,1] <-  str_replace_all(tmp[,1],pattern="_",replacement=" ")
    #tmp<-rbindlist(tmp)
    tmp[, 2] <- format(tmp[, 2], digits = 5)
    tmp
    
  }, caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center;',
                                       htmltools::h4(strong(
                                         'Average Occupancy'
                                       ))), rownames = FALSE, options = list(pageLength = 10, dom = 'tlp'))
  
  
  output$o <- renderPlot({
    req(sim_out())
    x <- sim_out()
    tmp <- x$o
    tmp
    
  }, res = 128)
  
  ###RENDER TRANSITION METRICS #####
  
  output$ptt_percent <- renderDataTable({
    req(sim_out())
    x <- sim_out()
    tmp <- x$ptt_percent
    tmp <- format(tmp, digits = 4, scientific = F)
    tmp$node <- str_replace_all(tmp$node,pattern="_",replacement=" ")
    colnames(tmp) <-
      c(
        "Service Point",
        "Transition Level",
        "% time at Transition Level",
        "Cumulative % time at or below Transition Level"
      )
    tmp
    
    
  }, caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center;',
                                       htmltools::h4(
                                         strong('Percentage time at transition level')
                                       )), rownames = FALSE, filter = 'top', options = list(pageLength = 10, dom =
                                                                                              'tlp'))
  
  
  output$ptt_plot <- renderPlot({
    req(sim_out())
    x <- sim_out()
    tmp <- x$ptt_plot
    tmp
    
  }, res = 128)
  
  
  output$avg_transition <- renderDataTable({
    req(sim_out())
    x <- sim_out()
    tmp <- x$avg_transition_summary
    tmp[,1] <-  str_replace_all(tmp[,1],pattern="_",replacement=" ")
    #tmp<-rbindlist(tmp)
    tmp[, 2] <- format(tmp[, 2], digits = 5)
    tmp
    
  }, caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center;',
                                       htmltools::h4(strong(
                                         'Average Transition'
                                       ))), rownames = FALSE, options = list(pageLength = 10, dom = 'tlp'))
  
  
  output$t <- renderPlot({
    req(sim_out())
    x <- sim_out()
    tmp <- x$t
    tmp
    
  }, res = 128)
  
  ###RENDER BED OCCUPANCY METRICS #####
  
  output$ptb_percent <- renderDataTable({
    req(sim_out())
    x <- sim_out()
    tmp <- x$ptb_percent
    tmp <- format(tmp, digits = 4, scientific = F)
    tmp$node <- str_replace_all(tmp$node,pattern="_",replacement=" ")
    colnames(tmp) <-
      c(
        "Service Point",
        "Bed Occupancy Level",
        "% time at Bed Occupancy Level",
        "Cumulative % time at or below Bed Occupancy Level"
      )
    tmp
    
    
  }, caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center;',
                                       htmltools::h4(
                                         strong('Percentage time at occ_bed level')
                                       )), rownames = FALSE, filter = 'top', options = list(pageLength = 10, dom =
                                                                                              'tlp'))
  
  
  output$ptb_plot <- renderPlot({
    req(sim_out())
    x <- sim_out()
    tmp <- x$ptb_plot
    tmp
    
  }, res = 128)
  
  
  output$avg_occ_bed <- renderDataTable({
    req(sim_out())
    x <- sim_out()
    tmp <- x$avg_occ_bed_summary
    tmp[,1] <-  str_replace_all(tmp[,1],pattern="_",replacement=" ")
    #tmp<-rbindlist(tmp)
    tmp[, 2] <- format(tmp[, 2], digits = 5)
    tmp
    
  }, caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center;',
                                       htmltools::h4(strong(
                                         'Average Bed Occupanncy'
                                       ))), rownames = FALSE, options = list(pageLength = 10, dom = 'tlp'))
  
  
  output$b <- renderPlot({
    req(sim_out())
    x <- sim_out()
    tmp <- x$b
    tmp
    
  }, res = 128)
  
  
  ###RENDER MULTIPLOT #####
  
  output$multi_plot <- renderPlot({
    req(sim_out())
    x <- sim_out()
    tmp <- x$avg_through_time_plot
    tmp
    
  }, res = 175)
  
  ###RENDER Warm-Up Assistance Plot #####
  
  output$tisp <- renderPlot({
    req(sim_out())
    x <- sim_out()
    tmp <- x$tisp
    tmp
    
  }, res = 175)
  
  
  ###RENDER PERCENTILE TABLES #####
  
  output$dpercentiles <- renderDataTable({
    req(sim_out())
    
    sketch = htmltools::withTags(table(class = 'display',
                                       thead(tr(
                                         th(rowspan = 2, 'Service Point'),
                                         th(colspan = 7, 'Percentiles')
                                       ),
                                       tr(lapply(
                                         c("50th", "80th", "85th", "90th", "95th", "99th", "100th"),
                                         th
                                       )))))
    
    
    x <- sim_out()
    tmp <- x$dpercentiles
    tmp[,1] <-  str_replace_all(tmp[,1],pattern="_",replacement=" ")
    #tmp<-ceiling(tmp)
    #tmp<-type.convert(tmp)
    #tmp<-format(tmp,digits=5)
    datatable(
      tmp,
      container = sketch,
      options = list(dom = 't', ordering = F),
      caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center;',
                                        htmltools::h4(strong(
                                          "Delay Percentiles"
                                        )))
    )
    
  })
  
  
  output$qpercentiles <- renderDataTable({
    req(sim_out())
    
    sketch = htmltools::withTags(table(class = 'display',
                                       thead(tr(
                                         th(rowspan = 2, 'Service Point'),
                                         th(colspan = 7, 'Percentiles')
                                       ),
                                       tr(lapply(
                                         c("50th", "80th", "85th", "90th", "95th", "99th", "100th"),
                                         th
                                       )))))
    
    
    x <- sim_out()
    tmp <- x$qpercentiles
    tmp[,1] <-  str_replace_all(tmp[,1],pattern="_",replacement=" ")
    # tmp<-ceiling(tmp)
    # tmp<-type.convert(tmp)
    #tmp<-format(tmp,digits=5)
    datatable(
      tmp,
      container = sketch,
      options = list(dom = 't', ordering = F),
      caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center;',
                                        htmltools::h4(strong(
                                          "Queue Percentiles"
                                        )))
    )
    
  })
  
  
  output$opercentiles <- renderDataTable({
    req(sim_out())
    
    sketch = htmltools::withTags(table(class = 'display',
                                       thead(tr(
                                         th(rowspan = 2, 'Service Point'),
                                         th(colspan = 7, 'Percentiles')
                                       ),
                                       tr(lapply(
                                         c("50th", "80th", "85th", "90th", "95th", "99th", "100th"),
                                         th
                                       )))))
    
    
    x <- sim_out()
    tmp <- x$opercentiles
    #format means you need to use matrix referencing without names here, rather than dollar sign/name referencing
    tmp[,1] <-  str_replace_all(tmp[,1],pattern="_",replacement=" ")
    
    # tmp<-ceiling(tmp)
    # tmp<-type.convert(tmp)
    #tmp<-format(tmp,digits=5)
    datatable(
      tmp,
      container = sketch,
      options = list(dom = 't', ordering = F),
      caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center;',
                                        htmltools::h4(strong(
                                          "Occupancy Percentiles"
                                        )))
    )
    
  })
  
  output$bpercentiles <- renderDataTable({
    req(sim_out())
    
    sketch = htmltools::withTags(table(class = 'display',
                                       thead(tr(
                                         th(rowspan = 2, 'Service Point'),
                                         th(colspan = 7, 'Percentiles')
                                       ),
                                       tr(lapply(
                                         c("50th", "80th", "85th", "90th", "95th", "99th", "100th"),
                                         th
                                       )))))
    
    
    x <- sim_out()
    tmp <- x$bpercentiles
    tmp[,1] <-  str_replace_all(tmp[,1],pattern="_",replacement=" ")
    # tmp<-ceiling(tmp)
    # tmp<-type.convert(tmp)
    #tmp<-format(tmp,digits=5)
    datatable(
      tmp,
      container = sketch,
      options = list(dom = 't', ordering = F),
      caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center;',
                                        htmltools::h4(strong(
                                          "Bed Occupancy Percentiles"
                                        )))
    )
    
  })
  
  
  
  output$tpercentiles <- renderDataTable({
    req(sim_out())
    
    sketch = htmltools::withTags(table(class = 'display',
                                       thead(tr(
                                         th(rowspan = 2, 'Service Point'),
                                         th(colspan = 7, 'Percentiles')
                                       ),
                                       tr(lapply(
                                         c("50th", "80th", "85th", "90th", "95th", "99th", "100th"),
                                         th
                                       )))))
    
    
    x <- sim_out()
    tmp <- x$tpercentiles
    tmp[,1] <-  str_replace_all(tmp[,1],pattern="_",replacement=" ")
    # tmp<-ceiling(tmp)
    # tmp<-type.convert(tmp)
    #tmp<-format(tmp,digits=5)
    datatable(
      tmp,
      container = sketch,
      options = list(dom = 't', ordering = F),
      caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center;',
                                        htmltools::h4(strong(
                                          "Transition Percentiles"
                                        )))
    )
    
  })
  
  
  
  
  output$tables_viz1 <- renderGrViz({
    viz()
  })
  
  output$tables_viz2 <- renderGrViz({
    viz()
  })
  
  ### XLSX DOWNLOAD HANDLER #####
  
  
  
  output$downloadtables <- downloadHandler(
    filename = function() {
      paste0("Simulation Tables.xlsx")
    },
    content = function(filename) {
      req(sim_out())
      x <- sim_out()
      
      showModal(modalDialog(
        title = div("Tables Rendering", style="font-size:200%"),
        div("On completion, click anywhere to continue", style="font-size:200%"),
        easyClose = FALSE,
        footer = NULL,
        size="l"
      ))
      
      # shinyalert(
      #   title = "Tables Rendering",
      #   text = "",
      #   closeOnEsc = FALSE,
      #   closeOnClickOutside = FALSE,
      #   html = FALSE,
      #   type = "info",
      #   showConfirmButton = FALSE,
      #   showCancelButton = FALSE,
      #   confirmButtonText = "OK",
      #   confirmButtonCol = "#87D9FF",
      #   timer = 0,
      #   imageUrl = "",
      #   animation = TRUE
      # )
      
      
      
      list_of_datasets <-
        list(
          "total_time_in_system" = x$total_time_in_system,
          
          "total_time_in_system_summary" = x$total_time_in_system_summary,
          
          "pat_rep_summary" = x$pat_rep_summary,
          
          "pat_total_summary" = x$pat_total_summary,
          
          
          "node_wait" = x$node_wait,
          
          "node_wait_summary" = x$node_wait_summary,
          
          
          
          "node_active_service" = x$node_active_service,
          
          "node_active_service_summary" = x$node_active_service_summary,
          
          
          
          "node_capacity_delay" = x$node_capacity_delay,
          
          "node_capacity_delay_summary" = x$node_capacity_delay_summary,
          
          
          "node_transition_delay" = x$node_transition_delay,
          
          "node_transition_delay_summary" = x$node_transition_delay_summary,
          
          
          
          "node_length_of_stay" = x$node_length_of_stay,
          
          "node_length_of_stay_summary" = x$node_length_of_stay_summary,
          
          
          
          "node_delay_to_transfer" = x$node_delay_to_transfer,
          
          "node_delay_to_transfer_summary" = x$node_delay_to_transfer_summary,
          
          
          
          "rejected_summary" = x$rejected_summary,
          
          
          "ptd_percent" = x$ptd_percent,
          
          "dpercentiles" = x$dpercentiles,
          
          "avg_delayed_summary" = x$avg_delayed_summary,
          
          
          
          "ptq_percent" = x$ptq_percent,
          
          "qpercentiles" = x$qpercentiles,
          
          "avg_queue_summary" = x$avg_queue_summary,
          
          
          
          "pto_percent" = x$pto_percent,
          
          "opercentiles" = x$opercentiles,
          
          "avg_occupancy_summary" = x$avg_occupancy_summary,
          
          
          
          "ptb_percent" = x$ptb_percent,
          
          "bpercentiles" = x$bpercentiles,
          
          "avg_occ_bed_summary" = x$avg_occ_bed_summary,
          
          
          
          "ptt_percent" = x$ptt_percent,
          
          "tpercentiles" = x$tpercentiles,
          
          "avg_transition_summary" = x$avg_transition_summary,
          
          "avg_through_time_uniform" = x$avg_through_time
        )
      
      
      
      write.xlsx(x = list_of_datasets, file = filename)
      
      
      showModal(modalDialog(
        title = div("Tables Download Complete", style="font-size:200%"),
        div("Click anywhere to continue", style="font-size:200%"),
        easyClose = TRUE,
        footer = NULL,
        size="l"
      ))
      
      # shinyalert(
      #   title = "Tables Download Complete",
      #   text = "",
      #   closeOnEsc = TRUE,
      #   closeOnClickOutside = TRUE,
      #   html = FALSE,
      #   type = "info",
      #   showConfirmButton = TRUE,
      #   showCancelButton = FALSE,
      #   confirmButtonText = "OK",
      #   confirmButtonCol = "#87D9FF",
      #   timer = 0,
      #   imageUrl = "",
      #   animation = TRUE
      # )
    }
  )
  
  ### PLOT DOWNLOAD HANDLER #####
  
  output$downloadplot <- downloadHandler(
    filename = "Plots.pdf",
    
    content = function(file) {
      req(sim_out())
      
      
      showModal(modalDialog(
        title = div("Plots Rendering", style="font-size:200%"),
        div("Plots will open in the default PDF reader from which they will need to be saved directly", style="font-size:200%"),
        easyClose = FALSE,
        footer = NULL,
        size="l"
      ))
      
      
      # shinyalert(
      #   title = "Plots Rendering",
      #   text = "",
      #   closeOnEsc = FALSE,
      #   closeOnClickOutside = FALSE,
      #   html = FALSE,
      #   type = "info",
      #   showConfirmButton = FALSE,
      #   showCancelButton = FALSE,
      #   confirmButtonText = "OK",
      #   confirmButtonCol = "#87D9FF",
      #   timer = 0,
      #   imageUrl = "",
      #   animation = TRUE
      # )
      
      x <- sim_out()
      
      pdf(file = file,
          width = 14,
          height = 7)
      print(x$pto_plot)
      print(x$ptb_plot)
      print(x$ptd_plot)
      print(x$ptt_plot)
      print(x$ptq_plot)
      print(x$avg_through_time_plot)
      print(x$o)
      print(x$b)
      print(x$d)
      print(x$t)
      print(x$q)
      
      dev.off()
      
      
      showModal(modalDialog(
        title = div("Plot Download Complete", style="font-size:200%"),
        div("Click anywhere to continue", style="font-size:200%"),
        easyClose = TRUE,
        footer = NULL,
        size="l"
      ))
      
      # shinyalert(
      #   title = "Plot Download Complete",
      #   text = "",
      #   closeOnEsc = TRUE,
      #   closeOnClickOutside = TRUE,
      #   html = FALSE,
      #   type = "info",
      #   showConfirmButton = TRUE,
      #   showCancelButton = FALSE,
      #   confirmButtonText = "OK",
      #   confirmButtonCol = "#87D9FF",
      #   timer = 0,
      #   imageUrl = "",
      #   animation = TRUE
      # )
      
    }
  )
  
  
  
  
  ### RMARKDOWN DOWNLOAD HANDLER #####
  
  output$downloadreport <- downloadHandler(
    filename = paste0("PathSimR_Report.docx"),
    
    content = function(file) {
      
      showModal(modalDialog(
        title = div("Report Compiling", style="font-size:200%"),
        div("On completion, click anywhere to continue", style="font-size:200%"),
        easyClose = FALSE,
        footer = NULL,
        size="l"
      ))
      
      
      
      # shinyalert(
      #   title = "Report Compiling",
      #   text = "",
      #   closeOnEsc = FALSE,
      #   closeOnClickOutside = FALSE,
      #   html = FALSE,
      #   type = "info",
      #   showConfirmButton = FALSE,
      #   showCancelButton = FALSE,
      #   confirmButtonText = "OK",
      #   confirmButtonCol = "#87D9FF",
      #   timer = 0,
      #   imageUrl = "",
      #   animation = TRUE
      # )
      
      
      
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport1 <- file.path(tempdir(), "PathSimR_Report.Rmd")
      tempReport2 <- file.path(tempdir(), "template.docx")
      
      file.copy("PathSimR_Report.Rmd", tempReport1, overwrite = TRUE)
      file.copy("template.docx", tempReport2, overwrite = TRUE)
      x <- sim_out()
      
      # Set up parameters to pass to Rmd document
      params <- list(
        total_time_in_system = x$total_time_in_system,
        total_time_in_system_summary = x$total_time_in_system_summary,
        node_wait = x$node_wait,
        node_wait_summary = x$node_wait_summary,
        pat_wait = x$pat_wait,
        pat_wait_summary = x$pat_wait_summary,
        node_active_service = x$node_active_service,
        node_active_service_summary = x$node_active_service_summary,
        pat_active_service = x$pat_active_service,
        pat_active_service_summary = x$pat_active_service_summary,
        node_length_of_stay = x$node_length_of_stay,
        node_length_of_stay_summary = x$node_length_of_stay_summary,
        pat_length_of_stay = x$pat_length_of_stay,
        pat_length_of_stay_summary = x$pat_length_of_stay_summary,
        node_delay_to_transfer = x$node_delay_to_transfer,
        node_delay_to_transfer_summary = x$node_delay_to_transfer_summary,
        pat_delay_to_transfer = x$pat_delay_to_transfer,
        pat_delay_to_transfer_summary = x$pat_delay_to_transfer_summary,
        pat_rep_summary = x$pat_rep_summary,
        pat_total_summary = x$pat_total_summary,
        ptd_percent = x$ptd_percent,
        ptd_plot = x$ptd_plot,
        avg_delayed = x$avg_delayed,
        avg_delayed_summary = x$avg_delayed_summary,
        d = x$d,
        ptq_percent = x$ptq_percent,
        ptq_plot = x$ptq_plot,
        avg_queue = x$avg_queue,
        avg_queue_summary = x$avg_queue_summary,
        q = x$q,
        pto_percent = x$pto_percent,
        pto_plot = x$pto_plot,
        avg_occupancy = x$avg_occupancy,
        avg_occupancy_summary = x$avg_occupancy_summary,
        o = x$o,
        ptb_percent = x$ptb_percent,
        ptb_plot = x$ptb_plot,
        avg_occ_bed = x$avg_occ_bed,
        avg_occ_bed_summary = x$avg_occ_bed_summary,
        b = x$b,
        ptt_percent = x$ptt_percent,
        ptt_plot = x$ptt_plot,
        avg_transition = x$avg_transition,
        avg_transition_summary = x$avg_transition_summary,
        t = x$t,
        dpercentiles = x$dpercentiles,
        qpercentiles = x$qpercentiles,
        opercentiles = x$opercentiles,
        bpercentiles = x$bpercentiles,
        tpercentiles = x$tpercentiles,
        rejected_summary = x$rejected_summary,
        avg_through_time_plot = x$avg_through_time_plot,
        reps = x$reps,
        ptm = x$ptm,
        avg_through_time = x$avg_through_time,
        nodes = x$nodes,
        warm_up = x$warm_up,
        sim_time = x$sim_time,
        exits = x$exits,
        syst_names = x$syst_names,
        delay_list = x$delay_list,
        cap_cal_input = x$cap_cal_input,
        arr_cal_input = x$arr_cal_input,
        
        node_capacity_delay = x$node_capacity_delay,
        node_capacity_delay_summary = x$node_capacity_delay_summary,
        pat_capacity_delay = x$pat_capacity_delay,
        pat_capacity_delay_summary = x$pat_capacity_delay_summary,
        node_transition_delay = x$node_transition_delay,
        node_transition_delay_summary = x$node_transition_delay_summary,
        pat_transition_delay = x$pat_transition_delay,
        pat_transition_delay_summary = x$pat_transition_delay_summary,
        
        #add the time unit as a parameter ####
        #need to ensure that it exists in the object x<-sim_out() first
        time_unit = x$time_unit
      )
      
      
      
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(
        tempReport1,
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )
      
      
      showModal(modalDialog(
        title = div("Report Download Complete", style="font-size:200%"),
        div("Click anywhere to continue", style="font-size:200%"),
        easyClose = TRUE,
        footer = NULL,
        size="l"
      ))
      
      
      
      
      # shinyalert(
      #   title = "Report Download Complete",
      #   text = "",
      #   closeOnEsc = TRUE,
      #   closeOnClickOutside = TRUE,
      #   html = FALSE,
      #   type = "info",
      #   showConfirmButton = TRUE,
      #   showCancelButton = FALSE,
      #   confirmButtonText = "OK",
      #   confirmButtonCol = "#87D9FF",
      #   timer = 0,
      #   imageUrl = "",
      #   animation = TRUE
      # )
      
    }
  )
  
  
  
  
  
  
  
  
}

shinyApp(ui = ui, server = server)
