o7_bed_occ <- list(
  
  h2(strong("Bed Occupancy Summary")),
  hr(),
  conditionalPanel(
    condition = "input.run_type=='Full Simulation'",
    fluidRow(
      column(
        6,
        align = "center",
        h3("% time at Bed Occupancy level"),
        plotOutput("ptb_plot", height = "500px")
      ),
      column(
        6,
        align = "center",
        h3("Bed Occupancy for 5 replicates"),
        plotOutput("b", height = "500px")
      )
    ),
    fluidRow(
      column(4, align = "center", dataTableOutput("bpercentiles")),
      column(8, align = "center", dataTableOutput("ptb_percent", width =
                                                    "70%"))
    ),
    
    fluidRow(
      column(
        width = 4,
        offset = 0,
        style = 'padding:0px;'
      ),
      column(2, align = "center", dataTableOutput("avg_occ_bed"))
    )
  ),
  
  conditionalPanel(condition = "input.run_type=='Trial Simulation'",
                   h2(
                     strong("Not Available in Trial Simulation")
                   ))
  
)