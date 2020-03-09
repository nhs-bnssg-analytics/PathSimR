o10_queueing <- list(
  
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
  
)