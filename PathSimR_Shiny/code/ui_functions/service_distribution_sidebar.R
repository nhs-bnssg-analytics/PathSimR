distribution_fit <- list(

h3(strong("Instructions")),


br(),
actionLink(
  inputId = "model_help",
  label = HTML("Which option do I need?"),
  icon = icon("info-circle"),
  style = ' font-size:150%'
),
br(),
bsModal(
  id = "modal_model",
  title = HTML("<h2><strong>Service Distribution Tool Help</strong></h2>"),
  trigger = "model_help",
  size = "large",
  ... =
    HTML(
      "
                         <p>
                         PathSimR's Service Distribution Tool contains 2 options depending on how much information is available about a service point. </p>

                         <p>If LoS data is available for the service point in question, then <strong>Option 1</strong> should be used. The data can be uploaded and
                         model fits run within the tool that provides the best fitting distribution and parameters that can be used. The data in question must be a single
                         column of data with no column header, saved as a csv. The graph below shows an example of uploaded data with 5 different best fitting distributions
                         plotted to show how the tool approximates the real data.</p>

                         <p>If only the mean LoS is known, then <strong>Option 2</strong> can be used, provided the service point type exists in the library (found on the Scale data by mean tab).
                         This portion of the tool scales a model distribution provided by BNSSG CCG to match the mean provided by the user, resulting in a model that has the correct shape and mean
                         for the service point type in question.
                         </p>
                              "
    ),
  plotOutput("model_help_figure")
),

h4(strong("Option 1: Model fits to user data")),
h4(
  em("Distribution & Parameters based on User data", style = "color:gray")
),
h5("Step 1: Select the 'Model fits to user data tab"),
h5(
  "Step 2: Upload a single column csv that only includes LoS data - ",
  em(" No Header required")
),
h5("Step 3: Press the 'Run Distribution Fit Tool' button"),
h5(
  "Step 4: Inspect the histgram plot and model fit curves, the details of which are displayd in the Ranked Model Table"
),
h5(
  "Step 5: Copy the top ranking model information from the table into the data entry page (i.e. Select the Distribution from the dropdown and enter the parameters listed)"
),
br(),
h4(strong("Option 2: Scale data by mean")),
h4(
  em("Distribution & Parameters based on scaled data", style = "color:gray")
),
h5("Step 1: Select the 'Scale data by mean' tab"),
h5(
  "Step 2: Select a Service Point from the drop-down library that matches the Service Point being modelled"
),
h5(
  "Step 3: Enter the mean LoS associated with the modelled Service Point"
),
h5("Step 4: Press the 'Run/Refresh Scaling Tool' Button"),
h5(
  "Step 5: Copy the model information from the table into the data entry page (i.e. Select the Distribution from the dropdown and eneter the parameters listed)"
),
h5(
  "Optional Step: Inspect the distribution plot to see a visual version of the Length of Service Distribution"
)

)