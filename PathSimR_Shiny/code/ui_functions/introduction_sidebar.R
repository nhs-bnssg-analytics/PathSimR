intro_side <- sidebarPanel(
  #Makes some manual changes to the CSS to increase the size of checkboxes and font on tabs
  
  tags$head(
    tags$style(
      ".checkbox { /* checkbox is a div class*/line-height: 40px;margin-bottom: 40px; /*set the margin, so boxes don't overlap*/}
                    input[type='checkbox']{ /* style for checkboxes */
                      width: 30px; /*Desired width*/
                      height: 30px; /*Desired height*/
                      line-height: 30px;
                    }
                    span {
                        margin-left: 15px;  /*set the margin, so boxes don't overlap labels*/
                        line-height: 30px;
                    }
                    * { font-family: Helvetica }
                    .nav-tabs {font-size: 30px}
                "
    ),
    tags$style(
      HTML(
        "
        input[type=number] {
              -moz-appearance:textfield;
        }
        input[type=number]::{
              -moz-appearance:textfield;
        }
        input[type=number]::-webkit-outer-spin-button,
        input[type=number]::-webkit-inner-spin-button {
              -webkit-appearance: none;
              margin: 0;
        }

    "
      )
    ),
    tags$style(type = 'text/css', '.navbar {font-size: 17px;}')
  ),
  
  
  #text in front page sidebar
  
  h1(strong("PathSimR")),
  h2(em(
    "A versatile tool for modelling pathway capacity in R"
  )),
  hr(), #horizontal line ("horizontal rule")
  
  
  #(two) buttons in front page side bar
  
  fluidRow(column(
    12,
    align = "center",
    actionButton(
      inputId = "j2w",
      label = "Start Pathway Wizard",
      icon = icon("magic"),
      style = 'padding:16px; font-size:150%'
    )
  )),
  hr(),
  fluidRow(column(
    12,
    align = "center",
    actionButton(
      inputId = "j2s1",
      label = "Start Simulation Tool",
      icon = icon("project-diagram"),
      style = 'padding:16px; font-size:150%'
    )
  )),
  
  #text in front page sidebar
  
  hr(),
  h3(
    "PathSimR is a simulation tool designed to give insights into how care pathways are performing
                              and enable 'what if' analysis to identify more effective and efficient service configurations."
  ),
  hr(),
  em(strong(
    p(
      "An overview of the tool and a glossary of key terms can be found in the Overview & Glossary tab in the Navigation Bar (accessible at all times).
                           Moreover, throughout the tool, there are ",
      actionLink(inputId = "info_help", label = icon("info-circle")),
      " symbols which provide additional information on specific topics."
    )
  )),
  em(strong(
    p(
      "New users are advised to read through the overview and glossary when first using the tool to familiarise themselves with the relevant terminology and ideas."
    )
  )),
  em(strong(
    p(
      "All data must be entered in a consistent time unit (e.g. all data uploaded is on the scale of either days or hours, but not a mixture of the two - if using hours, enter a day as 24 time units, a week as 168 etc.). Users can choose a label for their time unit on the Network Import and Visualisation tab - this will not be used in calculations, but will be added to output tables and graphs."
    )
  )),
  hr(),
  p(
    "Proceed through the tabs in the navigation bar at the top of the page, completing each tab in sequence.
               Once all inputs have been entered and confirmed on a tab, the subsequent tab will appear.
               The tabs can be navigated either using the 'previous' and 'next' buttons at the bottom of the page or clicking on the tabs themselves at the top of the page.
               Instructions are given on every page regarding how to use the tool. Users may return to any previous tab and update inputs,
                 simply rerun any uploads/processes on subsequent tabs before moving on."
  ),
  hr(),
  p(
    "The Pathway Wizard has been designed to help users collate the neccessary information required to run a simulation.
                 Like the simulation tool, complete each tab in sequence and then proceed into PathSimR."
  ),
  hr(),
  p(
    strong("To zoom out, press control+- and to zoom in press control+shift+=")
  ),
  p(
    strong(
      "For more information or guidance on how to use the tool, please contact the Modelling and Analytics team at BNSSG CCG."
    )
  ),
  
  
  width = 3
  
  
  #end of front page sidebar
  
)
