intro_main <- mainPanel(
  br(),
  fluidRow(
    column(4, align = "center", img(src = 'THF_logo.png', style = "height:150px;")),
    column(4, align = "center", img(src = 'BNSSG_logo.png', style = "height:150px;")),
    column(4, align = "center", img(src = 'UoB_logo.png', style = "height:150px;"))
  ),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  
  fluidRow(column(
    12, align = "center", img(src = 'Rplot.png', style = "height:400px;")
  )),
  #fluidRow(column(12,align="center",img(src='Logo2.jpg'))),
  
  br(),
  br(),
  br(),
  br(),
  br(),
  h5(strong(
    "Example patient pathway (built in PathSimR)"
  ), align = "right")
)