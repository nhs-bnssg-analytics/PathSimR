o1_interpretation <- list(
  
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
  
)