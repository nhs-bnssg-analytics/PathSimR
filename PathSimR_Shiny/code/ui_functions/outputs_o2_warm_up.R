o2_warm_up_advice <- list(
  
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
  
)