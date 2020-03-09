w3_tables_download <- list(
  
fluidRow(br(),
         column(12, tableOutput("issues"), align = "center")),

fluidRow(br(),
         column(12, tableOutput("means"), align = "center")),

fluidRow(br(),
         column(12, tableOutput("var_view"), align = "center")),

fluidRow(br(),
         column(12, tableOutput("cal_view"), align = "center"))

)