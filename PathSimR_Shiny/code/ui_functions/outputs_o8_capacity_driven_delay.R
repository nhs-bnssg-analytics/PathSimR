cap_delay <- list(
  
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
  
)