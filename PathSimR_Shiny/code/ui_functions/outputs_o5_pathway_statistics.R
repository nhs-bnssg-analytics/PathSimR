o5_pathway <- list(
  
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
  
)