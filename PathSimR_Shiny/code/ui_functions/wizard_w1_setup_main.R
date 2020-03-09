w1_setup_main <- list(
  
  fluidRow(
  br(),
  fluidRow(h2(strong(
    textOutput("duplicate")
  )), align = "center"),
  br(),
  br(),
  column(
    width = 1,
    offset = 0,
    style = 'padding:0px;'
  ),
  column(
    4,
    matrixInput(
      inputId = "sp",
      value = m1,
      class = "character",
      cols = list(
        names = TRUE,
        extend = FALSE,
        editableNames = FALSE
      ),
      rows = list(
        names = TRUE,
        extend = TRUE,
        editableNames = FALSE,
        delta = 1
      ),
      copy = FALSE,
      paste = TRUE
    )
  ),
  column(
    width = 2,
    offset = 0,
    style = 'padding:0px;'
  ),
  column(
    4,
    matrixInput(
      inputId = "exit",
      value = m2,
      class = "character",
      cols = list(
        names = TRUE,
        extend = FALSE,
        editableNames = FALSE
      ),
      rows = list(
        names = TRUE,
        extend = TRUE,
        editableNames = FALSE,
        delta = 1
      ),
      copy = FALSE,
      paste = TRUE
    )
  ),
  column(
    width = 1,
    offset = 0,
    style = 'padding:0px;'
  )
),
br(),

fluidRow(
  column(
    width = 1,
    offset = 0,
    style = 'padding:0px;'
  ),
  column(4, tableOutput("sp_table"), align = "center"),
  column(
    width = 2,
    offset = 0,
    style = 'padding:0px;'
  ),
  column(4, tableOutput("exit_table"), align = "center"),
  column(
    width = 1,
    offset = 0,
    style = 'padding:0px;'
  )
)

)