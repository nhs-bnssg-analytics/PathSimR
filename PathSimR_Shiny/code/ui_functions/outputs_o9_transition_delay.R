transition_delay <- list(
  
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
  
)