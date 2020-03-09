w3_tables_and_download_sidebar <- list(
  
  h3(strong("Instructions")),
h4(
  "Step 1:  Press the 'Create/Refresh tables' button to see a summary of the data entered & Issues Log."
),
p(
  "There are 4 tables: Issues, Mean Length of Service (only appears when no issues), Network Template and Calendar template",
  style = "color:gray"
),
br(),
h4(
  "Step 2: If there are any issues, return to the previous page and ammend the data inputs."
),
p(
  "The location of the issue is listed along with a brief description.",
  style = "color:gray"
),
br(),
h4(
  "Step 3: Once there are no issues remaining, the option to download the templates for further use becomes available"
),
p(
  "The templates created in the wizard can be saved down and then directly used in PathSimR at a later date.
                 Both templates are required for use in this way.",
  style = "color:gray"
),
br(),
h4("Step 4: Proceed by pressing the 'Move to Simulation Tool' button."),
p(
  "The inputs created in the wizard can be pulled through on the following page",
  style = "color:gray"
),


br(),
fluidRow(column(
  12,
  align = "center",
  actionButton(
    inputId = "go",
    label = "Create / Refresh tables",
    style = 'padding:10px; font-size:150%'
  )
)),
br(),
uiOutput("download_buttons"),
br(),
br(),
fluidRow(column(
  6,
  align = "center",
  actionButton(
    inputId = "jb2de",
    label = "Previous",
    icon = icon("arrow-left")
  )
),
uiOutput("j2st"))

)