distribution_main <- list(
  
  tabsetPanel(
    tabPanel(
      title = "Model fits to user data",
      fileInput(
        inputId = "los_dat",
        label = "Upload csv",
        multiple = FALSE,
        accept = c("text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv"),
        width = '25%'
      ),
      actionButton(inputId = "go_distfit", label = "Run Distribution Fit Tool"),
      br(),
      br(),
      fluidRow(
        column(10, plotOutput("los_fit_plot"), align = "center"),
        column(2, br(), br(), br(), tableOutput("mini_summary"), align = "center")
      ),
      
      fluidRow(br(),
               h3(textOutput("los_text")),
               p(textOutput("los_text_help"))),
      fluidRow(column(12, tableOutput("los_fit_table"), align =
                        "center")),
      h3(textOutput("fit_error"))
      
      #end of first tabpanel serivce distribution tool
    ),
    
    tabPanel(
      title = "Scale data by mean",
      fluidRow(column(
        8,
        br(),
        p(
          "Distributions and Parameters have been found for a variety of PODs/ Service Points, which are listed in the Service Point Library.
                            These were based on model fits to BNSSG data in order to match the shape of the Service time distribution. The data is rescaled based
                            on the Average Service value entered to create the required distribution."
        )
      )),
      hr(),
      fluidRow(
        column(3, uiOutput("treatment_select_ui")),
        column(
          2,
          numericInput(
            inputId = "treatment_mean",
            label = "Average Length of Service (Mean)",
            min = 0,
            value = 0,
            step = 0.01
          )
        ),
        column(
          1,
          br(),
          actionButton(inputId = "go_scaled_fit", label = "Run/Refresh Scaling Tool")
        )
      ),
      hr(),
      tableOutput("scaled_fit"),
      plotOutput("scaled_fit_plot")
      
      #end of second tabpanel serivce distribution tool  
    )
    
    #end of tabset panel for service distribution tool  
  )
  
)