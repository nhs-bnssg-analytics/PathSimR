o6_pat_occ <- list(
  
  h2(strong("Patient Occupancy Summary")),
  hr(),
  conditionalPanel(
    condition = "input.run_type=='Full Simulation'",
    
    fluidRow(
      column(
        6,
        align = "center",
        h3("% time at Patient Occupancy level"),
        plotOutput("pto_plot", height = "500px")
      ),
      column(
        6,
        align = "center",
        h3("Patient Occupancy for 5 replicates"),
        plotOutput("o", height = "500px")
      )
    ),
    
    fluidRow(
      column(4, align = "center", dataTableOutput("opercentiles")),
      column(8, align = "center", dataTableOutput("pto_percent", width =
                                                    "70%"))
    ),
    
    fluidRow(
      column(
        width = 4,
        offset = 0,
        style = 'padding:0px;'
      ),
      column(2, align = "center", dataTableOutput("avg_occupancy"))
    )
  ),
  
  conditionalPanel(condition = "input.run_type=='Trial Simulation'",
                   h2(
                     strong("Not Available in Trial Simulation")
                   ))
  
)