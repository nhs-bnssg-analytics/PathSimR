sim_setup_main <- list(
  
  useShinyalert(),
  fluidRow(
    column(4, align = "center", tableOutput("checklist_table_render")),
    column(8, grVizOutput("cl_viz"))
  ),
  fluidRow(column(12, align = "center", h1(textOutput(
    "comp"
  )))),
  br()
  
)