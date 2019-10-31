# PathSimR
Discrete event simulation of healthcare pathways in R.

To use PathSimR, download the entire **PathSimR_Shiny** folder and run the script **PathSimR_Shiny_v1.R** from that location. This will launch a Shiny app. To use the tool, follow the instructions on the screen.

The *PathSimR_Report.Rmd* and the *template.docx* files are called from within the app to create a downloadable Rmarkdown report and do not need to be opened or run separately unless you wish to alter the report format (e.g. replace the Word template with one containing your own organisation's branding).

The www folder contains images used by the app and should not be altered.

The remaining folders contain supplementary material to help with the use of PathSimR:

1. *documentation* contains the technical documentation for the app and a sample use case template to capture modelling requirements

2. *Use Case Library* contains expamples of requirements, inputs, and outputs for four case studies, as an illustration of how the tool can be used

3. *network_templates* contains sample input templates which can be loaded into the tool (as an alternative to using the inbuilt input wizard). They are intended to demonstrate features of the input format and not to represent real pathways. Files in this format can be downloaded, modified, and re-uploaded to PathSimR to save time, after an initial pathway has been created using the input wizard. A simple example of pair of input templates which will create an error are also included in this folder, as an aid to identifying problem inputs.
