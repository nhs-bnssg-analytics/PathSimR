#This is the (html formatted) text for the "Wizard & Setup Terms" tab
#on the "Overview & Glossary" page

#ov_format function is in the script overview_item_format.R,
#which is loaded in PathSmR_Shiny_v1.R

ov_wizard_tab <- list(
  
h3("Wizard & Setup"),

#1. time units ####
ov_format(
  text_name = "Time Units",
  text_description = "PathSimR does not have a prescribed time unit, instead users can use whichever time unit is appropriate. This must, however, be consistent throughout the tool and all data entered must match the chosen units (e.g. all data included is on the scale of days: Arrivals per day, Length of Service on scale of days, prescribed delays in fractions of days)."
          ),

#2. service point ####
ov_format(
  text_name = "Service Point",
  text_description = "Service Point is a ward, clinic or any treatment/service that occurs on the pathway. This can range from a GP surgery on a set timetable to a bedded ward providing continuous care. The key defining feature of a service point is that it has an associated capacity and service time."
          ),

#3. exit ####
ov_format(
  text_name = "Exit",
  text_description = "An exit is any location/service where patients are no longer tracked, i.e. they have left the pathway of interest. Example exits could be home, care home, death, another pathway that is not being modelled (e.g. 'Further Treatment', 'Out of patch''). These locations have no associated capacity or LoS and are simply end points along the patient pathway."
  ),

#4. length of service ####
ov_format(
  text_name = "Length of Service",
  text_description = "For the purposes of PathSimR, the Length of Service (or Active Service) corresponds to the amount of time a patient is actually receiving treatment or using a service. It does not include any time that the patient is blocked or delayed due to capacity restraints nor any prescribed delays. It represents the time between a patient starting to recieve treatment and the earliest time when they could move on, i.e. the required service time. This is different to the Length of Stay which is a metric calculated as part of the simulation and includes both active treatment time and any delays which may result due to the dynamics of the system."
    ),

#5. distributions and parameters ####
ov_format(
  text_name = "Distributions and Parameters",
  text_description = "A probability distribution is a mathematical function that quantifies the likelihood of different possible outcomes. In PathSimR, these outcomes are lengths of time representing service lengths, inter-arrival times and departure delays. Rather than every patient having a length of service equal to the mean, PathSimR relies on probability distributions to help capture the natural variation in the data. There are a large variety of distributions that can be used in modelling, each of which requires different parameters to shape the probabilities."
    ),

#6. transition delay ####
ov_format(
  text_name = "Transition Delay",
  text_description = "Transition  delays are included in PathSimR to help simulate an expected amount of time that a patient will remain in a ward, clinic, or other service point after they have completed their treatment before they can move to their next service point or be discharged (to represent, for example, travel time, the completion of administrative tasks, sourcing of social care funding, discussions with families, or delays in discharge to services which are not being explicitly modelled). Delays can occur between any pair of service points (that have a zero length queue between them) or between a service point and an exit point. The delay operates by keeping a patient in their current location while holding a space for them in their onward location (when exiting the pathway, there is always space). This is summarised in the Transition Delay tab in the outputs."
    ),

#7. capacity driven delay ####
ov_format(
  text_name = "Capacity Driven Delay",
  text_description = "Capacity delays occur as a result of blocking after service and are not inputs into the model. They are listed here to help distinguish them from the Transition Delays. They are a result of lack of downstream capacity within the network (i.e. at a service point with defined capacity) which forces a patient to stay in their current location until a space in an onward service point or queue is available. This is summarised in the Capacity Driven Delay tab in the outputs."
    ),

#8. warm-up period ####
ov_format(
  text_name = "Warm-up period",
  text_description = "The warm-up period represents the time it takes for a simulation to reach stable running conditions and after which results can be recorded. As each simulation starts from empty, it is important that the warm-up period be long enough so that the results collected are reflective of the modelled system and not an emptier version."
    ),

#9. simulation period ####
ov_format(
  text_name = "Simulation period",
  text_description = "The Simulation Period is the amount of time over which to collect results from the simulation. This will need to be sufficiently long such that a large number of patients can pass through the pathway. For example, if a pathway has an average length of 365 days then simulating it for only 10 would not produce complete results, as unique patients would not have completed the pathway. The simulation period should therefore be longer than the average pathway length and long enough to collect data."
    ),

#10. replications ####
ov_format(
  text_name = "Replications",
  text_description = "Number of times to run a particular simulation so as to capture the natural variation in the system. Results are averaged over all replications to ensure all system eventualities are captured."
    ),

#11. network input template ####
ov_format(
  text_name = "Network Input Template",
  text_description = "A .csv file that specifies transition rates between service points, permitted internal and external queue sizes, and the probabilty distribution names and parameters for both service point Lengths of Service, and prescribed transition delays between pairs of servcie points/service points and exits."
    ),

#12. calendar input template ####
ov_format(
  text_name = "Calendar Input Template",
  text_description = "A .csv file that includes the calendar of capacity and mean external arrival rates (by time period) for each service point"
    )

)