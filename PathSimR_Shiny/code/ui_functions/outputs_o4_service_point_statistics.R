o4_service_point <- list(
  
  h2(strong("Service Point Statistics")),
  hr(),
  conditionalPanel(
    condition = "input.run_type=='Full Simulation'",
    fluidRow(column(
      12, align = "center", grVizOutput("tables_viz1", height = "400px")
    )),
    br(),
    
    fluidRow(
      column(
        4,
        align = "center",
        h4("Wait Time"),
        tableOutput("node_wait_summary"),
        align = "center"
      ),
      column(
        4,
        align = "center",
        h4("Time Delayed (Capacity Driven)"),
        tableOutput("node_capacity_delay_summary"),
        align = "center"
      ),
      column(
        4,
        align = "center",
        h4("Time Delayed (Transition)"),
        tableOutput("node_transition_delay_summary"),
        align = "center"
      )
    ),
    
    fluidRow(
      column(
        4,
        align = "center",
        h4("Length Of Stay"),
        tableOutput("node_loss"),
        align = "center"
      ),
      column(
        4,
        align = "center",
        h4("Delay-To-Transfer"),
        tableOutput("node_dtts"),
        align = "center"
      ),
      column(
        width = 1,
        offset = 0,
        style = 'padding:0px;'
      ),
      column(2, h4("Rejection Rate"), tableOutput("rejs"), align =
               "center")
    )
  ),
  
  conditionalPanel(condition = "input.run_type=='Trial Simulation'",
                   h2(
                     strong("Not Available in Trial Simulation")
                   ))
  
)