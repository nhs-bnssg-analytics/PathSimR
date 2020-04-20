# PathSimR
Discrete event simulation of healthcare pathways in R.

To use PathSimR, download the entire **PathSimR_Shiny** folder, open the script **PathSimR_Shiny_v1.R** from that location and click the RunApp button. This will launch a Shiny app.

If when clicking the RunApp button you get the error message ***Error in shinyAppDir(x) : No Shiny applciation exists at the path ""***, (which may happen depending on your local network settings), then run the folowing line in the console to launch the app: ***shiny::runApp("PathSimR_Shiny_v1.R")***.

***Temporary issue (February 2020)*** PathSimR will not work with ***DiagrammeR v.1.0.5*** (a recent update on CRAN). If you already have an older version of the package installed, then you can ignore this note. However, if you are installing it for the first time, then please install the developer version ***DiagrammeR v.1.0.5.9000*** directly from github by running `devtools::install_github("rich-iannone/DiagrammeR")`.

To use the tool, follow the instructions on the screen.

The *PathSimR_Report.Rmd* and the *template.docx* files are called from within the app to create a downloadable Rmarkdown report and do not need to be opened or run separately unless you wish to alter the report format (e.g. replace the Word template with one containing your own organisation's branding).

The www folder contains images used by the app and should not be altered.

The remaining folders contain supplementary material to help with the use of PathSimR:

1. *documentation* contains the technical documentation for the app and a sample use case template to capture modelling requirements

2. *Use Case Library* contains expamples of requirements, inputs, and outputs for four case studies, as an illustration of how the tool can be used

3. *network_templates* contains sample input templates which can be loaded into the tool (as an alternative to using the inbuilt input wizard). They are intended to demonstrate features of the input format and not to represent real pathways. Files in this format can be downloaded, modified, and re-uploaded to PathSimR to save time, after an initial pathway has been created using the input wizard. A simple example of pair of input templates which will create an error are also included in this folder, as an aid to identifying problem inputs.
