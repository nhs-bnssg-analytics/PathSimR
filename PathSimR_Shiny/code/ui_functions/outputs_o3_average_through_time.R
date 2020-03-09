o3_through_time <- list(
  
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
  
)