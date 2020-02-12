ov_overview_tab <-         fluidRow(
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
                               )
