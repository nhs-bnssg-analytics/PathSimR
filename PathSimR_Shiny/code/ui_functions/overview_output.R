#This is the (html formatted) text for the "Output Terms" tab
#on the "Overview & Glossary" page

#ov_format function is in the script overview_item_format.R,
#which is loaded in PathSmR_Shiny_v1.R

ov_output_tab <- list(
  
  #0. Title ####
  h3("Output Terms"),
  
  #1. Patient Based Outputs ####
  h4("Patient Based Outputs"),
  
  #1.1 Total time in system ####
  ov_format(
    text_name = "Total time in system (TTIS)",
    text_description = "Time between external arrival and departure to an exit for each patient."
    ),
 
  #1.2 Wait ####
  ov_format(
    text_name = "Wait",
    text_description = "Time between arrival and service start at a service point (time spent in queue)."
    ),
  
  #1.3 Active service ####
  ov_format(
    text_name = "Active Service",
    text_description = "Time between service start and service end at the service point (e.g. treatment on a ward, a clinic appointment etc.)."
    ),
  
  #1.4 Time delayed ####
  ov_format(
    text_name = "Time Delayed (Capacity Driven)",
    text_description = "Time between service end and start of transition delay (or departure if no transition delay) at the service point (e.g. treatment on a ward, a clinic appointment etc.) - the amount of time a patient spends blocked at a service point after completing their Active Service, waiting for capacity to become free downstream."
    ),
  
  #1.5 Time delayed (transition) ####
  ov_format(
    text_name = "Time Delayed (Transition)",
    text_description = "Time between capacity driven delay end (or service end if no capacity delay) and departure from the service point (e.g. treatment on a ward, a clinic appointment etc.) - they user defined delay (not depedent on downstream capacity)."
    ),
  
  #1.6 Length of Stay (LOS) ####
  ov_format(
    text_name = "Length of Stay (LOS)",
    text_description = "Time between service start and departure (Active Service + Delay to transfer + Transition delay (user-defined delay))"
    ),
  
  #1.7 Delay to Transfer (DTT) ####
  ov_format(
    text_name = "Delay To Transfer (DTT)",
    text_description = "Time between service end and departure, i.e. the amount of time the patient is delayed due to a lack of capacity downstream (blocking after service) plus any transition delay."
    ),
  
  #1.8 Rejection rate ####
  ov_format(
    text_name = "Rejection Rate",
    text_description = "Number of patients rejected from full external queues divided by the length of the simulation run."
    ),
  
  #2. Service Point Based Outputs ####
  h4("Service Point Based Outputs"),
  
  #2.1 Queue ####
  ov_format(
    text_name = "Queue",
    text_description = "Number of concurrent patients who have arrived at a service point and are yet to start the service."
    ),
  
  #2.2 Occupancy/Patient Occupancy ####
  ov_format(
    text_name = "Occupancy/Patient Occupancy",
    text_description = "Number of patients who are actually receiving or have received service and are occupying a space in the service point."
    ),
  
  #2.3 Bed Occupancy ####
  ov_format(
    text_name = "Bed Occupancy",
    text_description = "The total number of beds currently not available to new arrivals - includes Occupancy/Patient Occupancy as above and also any 'empty' beds that are currently reserved for patients under Transition Delay upstream."
    ),

  #2.4 Capacity Driven Delay ####
  ov_format(
    text_name = "Capacity Driven Delay",
    text_description = "Number of patients concurrently delayed due to insufficient capacity downstream (blocking after service). These patients are included in the occupancy and the bed occupancy."
    ),
  
  #2.5 Transition Delay ####
  ov_format(
    text_name = "Transition Delay",
    text_description = "Number of patients concurrently experiencing a prescribed transfer delay. Patients moving to downstream nodes will also be reserving a space in the onward node and thus appear in the bed occupancy metric for that unit. Patients are included in the occupancy and bed occupancy of the current node"
    ),
  
  #2.6 % time at level ####
  ov_format(
    text_name = "% time at level",
    text_description = "The percentage of the total simulation time that a service point was at a particular level of the output measure of interest (calculated across all replications) - e.g. if there was a queue of length 5 at service point A was 150 time units out of a total simulation time of 1,500 time units, the '% time at level' for that unit and that queue would be 10%."
    ),
 
  #2.7 Percentiles
  ov_format(
    text_name = "Percentiles",
    text_description = "The percentage of total simulation time that the metric of interest was below (or above) the given level - e.g. if the 95th percentile occupancy for unit A was 6, then it's occupancy was at or below 6 for 95% of the simulation time (and conversely, there it was greater than six for 5% of the simulation time). The 50th percentile is the median."
    ),
  
  #3. Connecting events with outputs (diagram) ####
  h4("Connecting Events with Outputs"),

  #3.1 Description text first par ####

  ov_format(
    include_col_2=FALSE,
    width3=6,
    text_description = "The figure below shows how the different events that occur at each Service Point connect to the different outputs
                        listed on this page. The outputs in the coloured boxes represent the time spent in those states, e.g. the time
                        between Arrival and Service Start is defined as the Wait. Both Delay-To-Transfer and Length of Stay are combined metrics
                        that represent the sum of time spent in multiple states."
  ),

  #3.2 Description text second par ####
  ov_format(
    include_col_2=FALSE,
    width3=6,
    text_description = "The Service Point Based Outputs refer to the number of patients concurrently existing in the same state/activity. For example, the number of patients concurrently experiencing Capacity Driven Delay are all those that are between Service End and Transition Start simultaneously."
  ),

  #3.3 Image/diagram (from png) ####
  fluidRow(
    column(
      width = 1,
      offset = 0,
      style = 'padding:0px;'
    ),
    column(6, img(src = 'Event_Outputs.png'))
  )

 )
